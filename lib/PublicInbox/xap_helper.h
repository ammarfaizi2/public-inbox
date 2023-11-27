/*
 * Copyright (C) all contributors <meta@public-inbox.org>
 * License: GPL-2.0+ <https://www.gnu.org/licenses/gpl-2.0.txt>
 * Note: GPL-2+ since it'll incorporate approxidate from git someday
 *
 * Standalone helper process using C and minimal C++ for Xapian,
 * this is not linked to Perl in any way.
 * C (not C++) is used as much as possible to lower the contribution
 * barrier for hackers who mainly know C (this includes the maintainer).
 * Yes, that means we use C stdlib stuff like hsearch and open_memstream
 * instead their equivalents in the C++ stdlib :P
 * Everything here is an unstable internal API of public-inbox and
 * NOT intended for ordinary users; only public-inbox hackers
 */
#ifndef _ALL_SOURCE
#	define _ALL_SOURCE
#endif
#if defined(__NetBSD__) && !defined(_OPENBSD_SOURCE) // for reallocarray(3)
#	define _OPENBSD_SOURCE
#endif
#include <sys/file.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/wait.h>

#include <assert.h>
#include <err.h> // BSD, glibc, and musl all have this
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <search.h>
#include <signal.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sysexits.h>
#include <unistd.h>
#include <xapian.h> // our only reason for using C++

#define MY_VER(maj,min,rev) ((maj) << 16 | (min) << 8 | (rev))
#define XAP_VER \
	MY_VER(XAPIAN_MAJOR_VERSION,XAPIAN_MINOR_VERSION,XAPIAN_REVISION)

#if XAP_VER >= MY_VER(1,3,6)
#	define NRP Xapian::NumberRangeProcessor
#	define ADD_RP add_rangeprocessor
#	define SET_MAX_EXPANSION set_max_expansion // technically 1.3.3
#else
#	define NRP Xapian::NumberValueRangeProcessor
#	define ADD_RP add_valuerangeprocessor
#	define SET_MAX_EXPANSION set_max_wildcard_expansion
#endif

#if defined(__GLIBC__)
#	define MY_DO_OPTRESET() do { optind = 0; } while (0)
#else /* FreeBSD, musl, dfly, NetBSD, OpenBSD */
#	define MY_DO_OPTRESET() do { optind = optreset = 1; } while (0)
#endif

#if defined(__FreeBSD__) || defined(__GLIBC__)
#	define STDERR_ASSIGNABLE (1)
#else
#	define STDERR_ASSIGNABLE (0)
#endif

// assert functions are used correctly (e.g. ensure hackers don't
// cause EINVAL/EFAULT).  Not for stuff that can fail due to HW
// failures.
# define CHECK(type, expect, expr) do { \
	type ckvar______ = (expr); \
	assert(ckvar______ == (expect) && "BUG" && __FILE__ && __LINE__); \
} while (0)

// coredump on most usage errors since our only users are internal
#define ABORT(...) do { warnx(__VA_ARGS__); abort(); } while (0)
#define EABORT(...) do { warn(__VA_ARGS__); abort(); } while (0)

// sock_fd is modified in signal handler, yes, it's SOCK_SEQPACKET
static volatile int sock_fd = STDIN_FILENO;
static sigset_t fullset, workerset;
static bool alive = true;
#if STDERR_ASSIGNABLE
static FILE *orig_err = stderr;
#endif
static int orig_err_fd = -1;
static void *srch_tree; // tsearch + tdelete + twalk
static pid_t *worker_pids; // nr => pid
#define WORKER_MAX USHRT_MAX
static unsigned long nworker, nworker_hwm;
static int pipefds[2];

// PublicInbox::Search and PublicInbox::CodeSearch generate these:
static void mail_nrp_init(void);
static void code_nrp_init(void);
static void qp_init_mail_search(Xapian::QueryParser *);
static void qp_init_code_search(Xapian::QueryParser *);

enum exc_iter {
	ITER_OK = 0,
	ITER_RETRY,
	ITER_ABORT
};

struct srch {
	int paths_len; // int for comparisons
	unsigned qp_flags;
	Xapian::Database *db;
	Xapian::QueryParser *qp;
	char paths[]; // $shard_path0\0$shard_path1\0...
};

#define MY_ARG_MAX 256
typedef bool (*cmd)(struct req *);

// only one request per-process since we have RLIMIT_CPU timeout
struct req { // argv and pfxv point into global rbuf
	char *argv[MY_ARG_MAX];
	char *pfxv[MY_ARG_MAX]; // -A <prefix>
	struct srch *srch;
	char *Oeidx_key;
	cmd fn;
	unsigned long long max;
	unsigned long long off;
	unsigned long timeout_sec;
	size_t nr_out;
	long sort_col; // value column, negative means BoolWeight
	int argc;
	int pfxc;
	FILE *fp[2]; // [0] response pipe or sock, [1] status/errors (optional)
	bool has_input; // fp[0] is bidirectional
	bool collapse_threads;
	bool code_search;
	bool relevance; // sort by relevance before column
	bool asc; // ascending sort
};

struct worker {
	pid_t pid;
	unsigned nr;
};

#define SPLIT2ARGV(dst,buf,len) split2argv(dst,buf,len,MY_ARRAY_SIZE(dst))
static size_t split2argv(char **dst, char *buf, size_t len, size_t limit)
{
	if (buf[0] == 0 || len == 0 || buf[len - 1] != 0)
		ABORT("bogus argument given");
	size_t nr = 0;
	char *c = buf;
	for (size_t i = 1; i < len; i++) {
		if (!buf[i]) {
			dst[nr++] = c;
			c = buf + i + 1;
		}
		if (nr == limit)
			ABORT("too many args: %zu == %zu", nr, limit);
	}
	if (nr == 0) ABORT("no argument given");
	if ((long)nr < 0) ABORT("too many args: %zu", nr);
	return (long)nr;
}

static bool has_threadid(const struct srch *srch)
{
	return srch->db->get_metadata("has_threadid") == "1";
}

static Xapian::Enquire prep_enquire(const struct req *req)
{
	Xapian::Enquire enq(*req->srch->db);
	if (req->sort_col < 0) {
		enq.set_weighting_scheme(Xapian::BoolWeight());
		enq.set_docid_order(req->asc ? Xapian::Enquire::ASCENDING
					: Xapian::Enquire::DESCENDING);
	} else if (req->relevance) {
		enq.set_sort_by_relevance_then_value(req->sort_col, !req->asc);
	} else {
		enq.set_sort_by_value_then_relevance(req->sort_col, !req->asc);
	}
	return enq;
}

static Xapian::MSet enquire_mset(struct req *req, Xapian::Enquire *enq)
{
	if (!req->max) {
		switch (sizeof(Xapian::doccount)) {
		case 4: req->max = UINT_MAX; break;
		default: req->max = ULLONG_MAX;
		}
	}
	for (int i = 0; i < 9; i++) {
		try {
			Xapian::MSet mset = enq->get_mset(req->off, req->max);
			return mset;
		} catch (const Xapian::DatabaseModifiedError & e) {
			req->srch->db->reopen();
		}
	}
	return enq->get_mset(req->off, req->max);
}

// for v1, v2, and extindex
static Xapian::MSet mail_mset(struct req *req, const char *qry_str)
{
	struct srch *srch = req->srch;
	Xapian::Query qry = srch->qp->parse_query(qry_str, srch->qp_flags);
	if (req->Oeidx_key) {
		req->Oeidx_key[0] = 'O'; // modifies static rbuf
		qry = Xapian::Query(Xapian::Query::OP_FILTER, qry,
					Xapian::Query(req->Oeidx_key));
	}
	Xapian::Enquire enq = prep_enquire(req);
	enq.set_query(qry);
	// THREADID is a CPP macro defined on CLI (see) XapHelperCxx.pm
	if (req->collapse_threads && has_threadid(srch))
		enq.set_collapse_key(THREADID);

	return enquire_mset(req, &enq);
}

// for cindex
static Xapian::MSet commit_mset(struct req *req, const char *qry_str)
{
	struct srch *srch = req->srch;
	Xapian::Query qry = srch->qp->parse_query(qry_str, srch->qp_flags);
	// TODO: git_dir + roots_filter

	// we only want commits:
	qry = Xapian::Query(Xapian::Query::OP_FILTER, qry,
				Xapian::Query("T" "c"));
	Xapian::Enquire enq = prep_enquire(req);
	enq.set_query(qry);
	return enquire_mset(req, &enq);
}

static void emit_mset_stats(struct req *req, const Xapian::MSet *mset)
{
	if (req->fp[1])
		fprintf(req->fp[1], "mset.size=%llu nr_out=%zu\n",
			(unsigned long long)mset->size(), req->nr_out);
	else
		ABORT("BUG: %s caller only passed 1 FD", req->argv[0]);
}

static bool starts_with(const std::string *s, const char *pfx, size_t pfx_len)
{
	return s->size() >= pfx_len && !memcmp(pfx, s->c_str(), pfx_len);
}

static void dump_ibx_term(struct req *req, const char *pfx,
			Xapian::Document *doc, const char *ibx_id)
{
	Xapian::TermIterator cur = doc->termlist_begin();
	Xapian::TermIterator end = doc->termlist_end();
	size_t pfx_len = strlen(pfx);

	for (cur.skip_to(pfx); cur != end; cur++) {
		std::string tn = *cur;

		if (starts_with(&tn, pfx, pfx_len)) {
			fprintf(req->fp[0], "%s %s\n",
				tn.c_str() + pfx_len, ibx_id);
			++req->nr_out;
		}
	}
}

static int my_setlinebuf(FILE *fp) // glibc setlinebuf(3) can't report errors
{
	return setvbuf(fp, NULL, _IOLBF, 0);
}

static enum exc_iter dump_ibx_iter(struct req *req, const char *ibx_id,
				Xapian::MSetIterator *i)
{
	try {
		Xapian::Document doc = i->get_document();
		for (int p = 0; p < req->pfxc; p++)
			dump_ibx_term(req, req->pfxv[p], &doc, ibx_id);
	} catch (const Xapian::DatabaseModifiedError & e) {
		req->srch->db->reopen();
		return ITER_RETRY;
	} catch (const Xapian::DocNotFoundError & e) { // oh well...
		warnx("doc not found: %s", e.get_description().c_str());
	}
	return ITER_OK;
}

static bool cmd_dump_ibx(struct req *req)
{
	if ((optind + 1) >= req->argc)
		ABORT("usage: dump_ibx [OPTIONS] IBX_ID QRY_STR");
	if (!req->pfxc)
		ABORT("dump_ibx requires -A PREFIX");

	const char *ibx_id = req->argv[optind];
	if (my_setlinebuf(req->fp[0])) // for sort(1) pipe
		EABORT("setlinebuf(fp[0])"); // WTF?
	req->asc = true;
	req->sort_col = -1;
	Xapian::MSet mset = mail_mset(req, req->argv[optind + 1]);

	// @UNIQ_FOLD in CodeSearchIdx.pm can handle duplicate lines fine
	// in case we need to retry on DB reopens
	for (Xapian::MSetIterator i = mset.begin(); i != mset.end(); i++) {
		for (int t = 10; t > 0; --t)
			switch (dump_ibx_iter(req, ibx_id, &i)) {
			case ITER_OK: t = 0; break; // leave inner loop
			case ITER_RETRY: break; // continue for-loop
			case ITER_ABORT: return false; // error
			}
	}
	emit_mset_stats(req, &mset);
	return true;
}

struct fbuf {
	FILE *fp;
	char *ptr;
	size_t len;
};

struct dump_roots_tmp {
	struct stat sb;
	void *mm_ptr;
	char **entries;
	struct fbuf wbuf;
	int root2off_fd;
};

// n.b. __cleanup__ works fine with C++ exceptions, but not longjmp
// Only clang and g++ are supported, as AFAIK there's no other
// relevant Free(-as-in-speech) C++ compilers.
#define CLEANUP_FBUF __attribute__((__cleanup__(fbuf_ensure)))
static void fbuf_ensure(void *ptr)
{
	struct fbuf *fbuf = (struct fbuf *)ptr;
	if (fbuf->fp && fclose(fbuf->fp))
		err(EXIT_FAILURE, "fclose(fbuf->fp)"); // ENOMEM?
	fbuf->fp = NULL;
	free(fbuf->ptr);
}

static void fbuf_init(struct fbuf *fbuf)
{
	assert(!fbuf->ptr);
	fbuf->fp = open_memstream(&fbuf->ptr, &fbuf->len);
	if (!fbuf->fp) err(EXIT_FAILURE, "open_memstream(fbuf)");
}

static void xclose(int fd)
{
	if (close(fd) < 0 && errno != EINTR)
		EABORT("BUG: close");
}

static size_t off2size(off_t n)
{
	if (n < 0 || (uintmax_t)n > SIZE_MAX)
		ABORT("off_t out of size_t range: %lld\n", (long long)n);
	return (size_t)n;
}

#define CLEANUP_DUMP_ROOTS __attribute__((__cleanup__(dump_roots_ensure)))
static void dump_roots_ensure(void *ptr)
{
	struct dump_roots_tmp *drt = (struct dump_roots_tmp *)ptr;
	if (drt->root2off_fd >= 0)
		xclose(drt->root2off_fd);
	hdestroy(); // idempotent
	size_t size = off2size(drt->sb.st_size);
	if (drt->mm_ptr && munmap(drt->mm_ptr, size))
		EABORT("BUG: munmap(%p, %zu)", drt->mm_ptr, size);
	free(drt->entries);
	fbuf_ensure(&drt->wbuf);
}

static bool root2offs_str(struct fbuf *root_offs, Xapian::Document *doc)
{
	Xapian::TermIterator cur = doc->termlist_begin();
	Xapian::TermIterator end = doc->termlist_end();
	ENTRY e, *ep;
	fbuf_init(root_offs);
	for (cur.skip_to("G"); cur != end; cur++) {
		std::string tn = *cur;
		if (!starts_with(&tn, "G", 1))
			continue;
		union { const char *in; char *out; } u;
		u.in = tn.c_str() + 1;
		e.key = u.out;
		ep = hsearch(e, FIND);
		if (!ep) ABORT("hsearch miss `%s'", e.key);
		// ep->data is a NUL-terminated string matching /[0-9]+/
		fputc(' ', root_offs->fp);
		fputs((const char *)ep->data, root_offs->fp);
	}
	fputc('\n', root_offs->fp);
	if (ferror(root_offs->fp) | fclose(root_offs->fp))
		err(EXIT_FAILURE, "ferror|fclose(root_offs)"); // ENOMEM
	root_offs->fp = NULL;
	return true;
}

// writes term values matching @pfx for a given @doc, ending the line
// with the contents of @root_offs
static void dump_roots_term(struct req *req, const char *pfx,
				struct dump_roots_tmp *drt,
				struct fbuf *root_offs,
				Xapian::Document *doc)
{
	Xapian::TermIterator cur = doc->termlist_begin();
	Xapian::TermIterator end = doc->termlist_end();
	size_t pfx_len = strlen(pfx);

	for (cur.skip_to(pfx); cur != end; cur++) {
		std::string tn = *cur;
		if (!starts_with(&tn, pfx, pfx_len))
			continue;
		fputs(tn.c_str() + pfx_len, drt->wbuf.fp);
		fwrite(root_offs->ptr, root_offs->len, 1, drt->wbuf.fp);
		++req->nr_out;
	}
}

// we may have lines which exceed PIPE_BUF, so we do our own
// buffering and rely on flock(2), here
static bool dump_roots_flush(struct req *req, struct dump_roots_tmp *drt)
{
	char *p;
	int fd = fileno(req->fp[0]);
	bool ok = true;

	if (!drt->wbuf.fp) return true;
	if (fd < 0) EABORT("BUG: fileno");
	if (ferror(drt->wbuf.fp) | fclose(drt->wbuf.fp)) // ENOMEM?
		err(EXIT_FAILURE, "ferror|fclose(drt->wbuf.fp)");
	drt->wbuf.fp = NULL;
	if (!drt->wbuf.len) goto done_free;
	while (flock(drt->root2off_fd, LOCK_EX)) {
		if (errno == EINTR) continue;
		err(EXIT_FAILURE, "LOCK_EX"); // ENOLCK?
	}
	p = drt->wbuf.ptr;
	do { // write to client FD
		ssize_t n = write(fd, p, drt->wbuf.len);
		if (n > 0) {
			drt->wbuf.len -= n;
			p += n;
		} else {
			perror(n ? "write" : "write (zero bytes)");
			return false;
		}
	} while (drt->wbuf.len);
	while (flock(drt->root2off_fd, LOCK_UN)) {
		if (errno == EINTR) continue;
		err(EXIT_FAILURE, "LOCK_UN"); // ENOLCK?
	}
done_free: // OK to skip on errors, dump_roots_ensure calls fbuf_ensure
	free(drt->wbuf.ptr);
	drt->wbuf.ptr = NULL;
	return ok;
}

static enum exc_iter dump_roots_iter(struct req *req,
				struct dump_roots_tmp *drt,
				Xapian::MSetIterator *i)
{
	CLEANUP_FBUF struct fbuf root_offs = {}; // " $ID0 $ID1 $IDx..\n"
	try {
		Xapian::Document doc = i->get_document();
		if (!root2offs_str(&root_offs, &doc))
			return ITER_ABORT; // bad request, abort
		for (int p = 0; p < req->pfxc; p++)
			dump_roots_term(req, req->pfxv[p], drt,
					&root_offs, &doc);
	} catch (const Xapian::DatabaseModifiedError & e) {
		req->srch->db->reopen();
		return ITER_RETRY;
	} catch (const Xapian::DocNotFoundError & e) { // oh well...
		warnx("doc not found: %s", e.get_description().c_str());
	}
	return ITER_OK;
}

static char *hsearch_enter_key(char *s)
{
#if defined(__OpenBSD__) || defined(__DragonFly__)
	// hdestroy frees each key on some platforms,
	// so give it something to free:
	char *ret = strdup(s);
	if (!ret) err(EXIT_FAILURE, "strdup");
	return ret;
// AFAIK there's no way to detect musl, assume non-glibc Linux is musl:
#elif defined(__GLIBC__) || defined(__linux__) || \
	defined(__FreeBSD__) || defined(__NetBSD__)
	// do nothing on these platforms
#else
#warning untested platform detected, unsure if hdestroy(3) frees keys
#warning contact us at meta@public-inbox.org if you get segfaults
#endif
	return s;
}

static bool cmd_dump_roots(struct req *req)
{
	CLEANUP_DUMP_ROOTS struct dump_roots_tmp drt = {};
	drt.root2off_fd = -1;
	if ((optind + 1) >= req->argc)
		ABORT("usage: dump_roots [OPTIONS] ROOT2ID_FILE QRY_STR");
	if (!req->pfxc)
		ABORT("dump_roots requires -A PREFIX");
	const char *root2off_file = req->argv[optind];
	drt.root2off_fd = open(root2off_file, O_RDONLY);
	if (drt.root2off_fd < 0)
		EABORT("open(%s)", root2off_file);
	if (fstat(drt.root2off_fd, &drt.sb)) // ENOMEM?
		err(EXIT_FAILURE, "fstat(%s)", root2off_file);
	// each entry is at least 43 bytes ({OIDHEX}\0{INT}\0),
	// so /32 overestimates the number of expected entries by
	// ~%25 (as recommended by Linux hcreate(3) manpage)
	size_t size = off2size(drt.sb.st_size);
	size_t est = (size / 32) + 1; //+1 for "\0" termination
	drt.mm_ptr = mmap(NULL, size, PROT_READ,
				MAP_PRIVATE, drt.root2off_fd, 0);
	if (drt.mm_ptr == MAP_FAILED)
		err(EXIT_FAILURE, "mmap(%zu, %s)", size, root2off_file);
	size_t asize = est * 2;
	if (asize < est) ABORT("too many entries: %zu", est);
	drt.entries = (char **)calloc(asize, sizeof(char *));
	if (!drt.entries)
		err(EXIT_FAILURE, "calloc(%zu * 2, %zu)", est, sizeof(char *));
	size_t tot = split2argv(drt.entries, (char *)drt.mm_ptr, size, asize);
	if (tot <= 0) return false; // split2argv already warned on error
	if (!hcreate(est))
		err(EXIT_FAILURE, "hcreate(%zu)", est);
	for (size_t i = 0; i < tot; ) {
		ENTRY e;
		e.key = hsearch_enter_key(drt.entries[i++]); // dies on ENOMEM
		e.data = drt.entries[i++];
		if (!hsearch(e, ENTER))
			err(EXIT_FAILURE, "hsearch(%s => %s, ENTER)", e.key,
					(const char *)e.data);
	}
	req->asc = true;
	req->sort_col = -1;
	Xapian::MSet mset = commit_mset(req, req->argv[optind + 1]);

	// @UNIQ_FOLD in CodeSearchIdx.pm can handle duplicate lines fine
	// in case we need to retry on DB reopens
	for (Xapian::MSetIterator i = mset.begin(); i != mset.end(); i++) {
		if (!drt.wbuf.fp)
			fbuf_init(&drt.wbuf);
		for (int t = 10; t > 0; --t)
			switch (dump_roots_iter(req, &drt, &i)) {
			case ITER_OK: t = 0; break; // leave inner loop
			case ITER_RETRY: break; // continue for-loop
			case ITER_ABORT: return false; // error
			}
		if (!(req->nr_out & 0x3fff) && !dump_roots_flush(req, &drt))
			return false;
	}
	if (!dump_roots_flush(req, &drt))
		return false;
	emit_mset_stats(req, &mset);
	return true;
}

// for test usage only, we need to ensure the compiler supports
// __cleanup__ when exceptions are thrown
struct inspect { struct req *req; };

static void inspect_ensure(struct inspect *x)
{
	fprintf(x->req->fp[0], "pid=%d has_threadid=%d",
		(int)getpid(), has_threadid(x->req->srch) ? 1 : 0);
}

static bool cmd_test_inspect(struct req *req)
{
	__attribute__((__cleanup__(inspect_ensure))) struct inspect x;
	x.req = req;
	try {
		throw Xapian::InvalidArgumentError("test");
	} catch (Xapian::InvalidArgumentError) {
		return true;
	}
	fputs("this should not be printed", req->fp[0]);
	return false;
}

#define CMD(n) { .fn_len = sizeof(#n) - 1, .fn_name = #n, .fn = cmd_##n }
static const struct cmd_entry {
	size_t fn_len;
	const char *fn_name;
	cmd fn;
} cmds[] = { // should be small enough to not need bsearch || gperf
	// most common commands first
	CMD(dump_ibx), // many inboxes
	CMD(dump_roots), // per-cidx shard
	CMD(test_inspect), // least common commands last
};

#define MY_ARRAY_SIZE(x)	(sizeof(x)/sizeof((x)[0]))
#define RECV_FD_CAPA 2
#define RECV_FD_SPACE	(RECV_FD_CAPA * sizeof(int))
union my_cmsg {
	struct cmsghdr hdr;
	char pad[sizeof(struct cmsghdr) + 16 + RECV_FD_SPACE];
};

static bool recv_req(struct req *req, char *rbuf, size_t *len)
{
	union my_cmsg cmsg = {};
	struct msghdr msg = {};
	struct iovec iov;
	ssize_t r;
	iov.iov_base = rbuf;
	iov.iov_len = *len;
	msg.msg_iov = &iov;
	msg.msg_iovlen = 1;
	msg.msg_control = &cmsg.hdr;
	msg.msg_controllen = CMSG_SPACE(RECV_FD_SPACE);

	// allow SIGTERM to hit
	CHECK(int, 0, sigprocmask(SIG_SETMASK, &workerset, NULL));

again:
	r = recvmsg(sock_fd, &msg, 0);
	if (r == 0) {
		exit(EX_NOINPUT); /* grandparent went away */
	} else if (r < 0) {
		switch (errno) {
		case EINTR: goto again;
		case EBADF: if (sock_fd < 0) exit(0);
			// fall-through
		default: err(EXIT_FAILURE, "recvmsg");
		}
	}

	// success! no signals for the rest of the request/response cycle
	CHECK(int, 0, sigprocmask(SIG_SETMASK, &fullset, NULL));
	if (r > 0 && msg.msg_flags)
		ABORT("unexpected msg_flags");

	*len = r;
	if (cmsg.hdr.cmsg_level == SOL_SOCKET &&
			cmsg.hdr.cmsg_type == SCM_RIGHTS) {
		size_t clen = cmsg.hdr.cmsg_len;
		int *fdp = (int *)CMSG_DATA(&cmsg.hdr);
		size_t i;
		for (i = 0; CMSG_LEN((i + 1) * sizeof(int)) <= clen; i++) {
			int fd = *fdp++;
			const char *mode = NULL;
			int fl = fcntl(fd, F_GETFL);
			if (fl == -1) {
				errx(EXIT_FAILURE, "invalid fd=%d", fd);
			} else if (fl & O_WRONLY) {
				mode = "w";
			} else if (fl & O_RDWR) {
				mode = "r+";
				if (i == 0) req->has_input = true;
			} else {
				errx(EXIT_FAILURE,
					"invalid mode from F_GETFL: 0x%x", fl);
			}
			req->fp[i] = fdopen(fd, mode);
			if (!req->fp[i])
				err(EXIT_FAILURE, "fdopen(fd=%d)", fd);
		}
		return true;
	}
	errx(EXIT_FAILURE, "no FD received in %zd-byte request", r);
	return false;
}

static int srch_cmp(const void *pa, const void *pb) // for tfind|tsearch
{
	const struct srch *a = (const struct srch *)pa;
	const struct srch *b = (const struct srch *)pb;
	int diff = a->paths_len - b->paths_len;

	return diff ? diff : memcmp(a->paths, b->paths, (size_t)a->paths_len);
}

static bool is_chert(const char *dir)
{
	char iamchert[PATH_MAX];
	struct stat sb;
	int rc = snprintf(iamchert, sizeof(iamchert), "%s/iamchert", dir);

	if (rc <= 0 || rc >= (int)sizeof(iamchert))
		err(EXIT_FAILURE, "BUG: snprintf(%s/iamchert)", dir);
	if (stat(iamchert, &sb) == 0 && S_ISREG(sb.st_mode))
		return true;
	return false;
}

static bool srch_init(struct req *req)
{
	char *dirv[MY_ARG_MAX];
	int i;
	struct srch *srch = req->srch;
	int dirc = (int)SPLIT2ARGV(dirv, srch->paths, (size_t)srch->paths_len);
	const unsigned FLAG_PHRASE = Xapian::QueryParser::FLAG_PHRASE;
	srch->qp_flags = FLAG_PHRASE |
			Xapian::QueryParser::FLAG_BOOLEAN |
			Xapian::QueryParser::FLAG_LOVEHATE |
			Xapian::QueryParser::FLAG_WILDCARD;
	if (is_chert(dirv[0]))
		srch->qp_flags &= ~FLAG_PHRASE;
	try {
		srch->db = new Xapian::Database(dirv[0]);
	} catch (...) {
		warn("E: Xapian::Database(%s)", dirv[0]);
		return false;
	}
	try {
		for (i = 1; i < dirc; i++) {
			if (srch->qp_flags & FLAG_PHRASE && is_chert(dirv[i]))
				srch->qp_flags &= ~FLAG_PHRASE;
			srch->db->add_database(Xapian::Database(dirv[i]));
		}
	} catch (...) {
		warn("E: add_database(%s)", dirv[i]);
		return false;
	}
	try {
		srch->qp = new Xapian::QueryParser;
	} catch (...) {
		perror("E: Xapian::QueryParser");
		return false;
	}
	srch->qp->set_default_op(Xapian::Query::OP_AND);
	srch->qp->set_database(*srch->db);
	try {
		srch->qp->set_stemmer(Xapian::Stem("english"));
	} catch (...) {
		perror("E: Xapian::Stem");
		return false;
	}
	srch->qp->set_stemming_strategy(Xapian::QueryParser::STEM_SOME);
	srch->qp->SET_MAX_EXPANSION(100);

	if (req->code_search)
		qp_init_code_search(srch->qp); // CodeSearch.pm
	else
		qp_init_mail_search(srch->qp); // Search.pm
	return true;
}

static void free_srch(void *p) // tdestroy
{
	struct srch *srch = (struct srch *)p;
	delete srch->qp;
	delete srch->db;
	free(srch);
}

static void dispatch(struct req *req)
{
	int c;
	size_t size = strlen(req->argv[0]);
	union {
		struct srch *srch;
		char *ptr;
	} kbuf;
	char *end;
	FILE *kfp;
	struct srch **s;
	req->fn = NULL;
	for (c = 0; c < (int)MY_ARRAY_SIZE(cmds); c++) {
		if (cmds[c].fn_len == size &&
			!memcmp(cmds[c].fn_name, req->argv[0], size)) {
			req->fn = cmds[c].fn;
			break;
		}
	}
	if (!req->fn) ABORT("not handled: `%s'", req->argv[0]);

	kfp = open_memstream(&kbuf.ptr, &size);
	if (!kfp) err(EXIT_FAILURE, "open_memstream(kbuf)");
	// write padding, first (contents don't matter)
	fwrite(&req->argv[0], offsetof(struct srch, paths), 1, kfp);

	// global getopt variables:
	optopt = 0;
	optarg = NULL;
	MY_DO_OPTRESET();

	// keep sync with @PublicInbox::XapHelper::SPEC
	while ((c = getopt(req->argc, req->argv, "acd:k:m:o:rtA:O:T:")) != -1) {
		switch (c) {
		case 'a': req->asc = true; break;
		case 'c': req->code_search = true; break;
		case 'd': fwrite(optarg, strlen(optarg) + 1, 1, kfp); break;
		case 'k':
			req->sort_col = strtol(optarg, &end, 10);
			if (*end) ABORT("-k %s", optarg);
			switch (req->sort_col) {
			case LONG_MAX: case LONG_MIN: ABORT("-k %s", optarg);
			}
			break;
		case 'm':
			req->max = strtoull(optarg, &end, 10);
			if (*end || req->max == ULLONG_MAX)
				ABORT("-m %s", optarg);
			break;
		case 'o':
			req->off = strtoull(optarg, &end, 10);
			if (*end || req->off == ULLONG_MAX)
				ABORT("-o %s", optarg);
			break;
		case 'r': req->relevance = true; break;
		case 't': req->collapse_threads = true; break;
		case 'A':
			req->pfxv[req->pfxc++] = optarg;
			if (MY_ARG_MAX == req->pfxc)
				ABORT("too many -A");
			break;
		case 'O': req->Oeidx_key = optarg - 1; break; // pad "O" prefix
		case 'T':
			req->timeout_sec = strtoul(optarg, &end, 10);
			if (*end || req->timeout_sec == ULONG_MAX)
				ABORT("-T %s", optarg);
			break;
		default: ABORT("bad switch `-%c'", c);
		}
	}
	if (ferror(kfp) | fclose(kfp)) /* sets kbuf.srch */
		err(EXIT_FAILURE, "ferror|fclose"); // likely ENOMEM
	kbuf.srch->db = NULL;
	kbuf.srch->qp = NULL;
	kbuf.srch->paths_len = size - offsetof(struct srch, paths);
	if (kbuf.srch->paths_len <= 0)
		ABORT("no -d args");
	s = (struct srch **)tsearch(kbuf.srch, &srch_tree, srch_cmp);
	if (!s) err(EXIT_FAILURE, "tsearch"); // likely ENOMEM
	req->srch = *s;
	if (req->srch != kbuf.srch) { // reuse existing
		free_srch(kbuf.srch);
	} else if (!srch_init(req)) {
		assert(kbuf.srch == *((struct srch **)tfind(
					kbuf.srch, &srch_tree, srch_cmp)));
		void *del = tdelete(kbuf.srch, &srch_tree, srch_cmp);
		assert(del);
		free_srch(kbuf.srch);
		goto cmd_err; // srch_init already warned
	}
	try {
		if (!req->fn(req))
			warnx("`%s' failed", req->argv[0]);
	} catch (const Xapian::Error & e) {
		warnx("Xapian::Error: %s", e.get_description().c_str());
	} catch (...) {
		warn("unhandled exception");
	}
cmd_err:
	return; // just be silent on errors, for now
}

static void cleanup_pids(void)
{
	free(worker_pids);
	worker_pids = NULL;
}

static void stderr_set(FILE *tmp_err)
{
#if STDERR_ASSIGNABLE
	if (my_setlinebuf(tmp_err))
		perror("W: setlinebuf(tmp_err)");
	stderr = tmp_err;
	return;
#endif
	int fd = fileno(tmp_err);
	if (fd < 0) err(EXIT_FAILURE, "BUG: fileno(tmp_err)");
	while (dup2(fd, STDERR_FILENO) < 0) {
		if (errno != EINTR)
			err(EXIT_FAILURE, "dup2(%d => 2)", fd);
	}
}

static void stderr_restore(FILE *tmp_err)
{
#if STDERR_ASSIGNABLE
	stderr = orig_err;
	return;
#endif
	if (ferror(stderr) | fflush(stderr))
		err(EXIT_FAILURE, "ferror|fflush stderr");
	while (dup2(orig_err_fd, STDERR_FILENO) < 0) {
		if (errno != EINTR)
			err(EXIT_FAILURE, "dup2(%d => 2)", orig_err_fd);
	}
	clearerr(stderr);
}

static void sigw(int sig) // SIGTERM handler for worker
{
	sock_fd = -1; // break out of recv_loop
}

static void recv_loop(void) // worker process loop
{
	static char rbuf[4096 * 33]; // per-process
	struct sigaction sa = {};
	sa.sa_handler = sigw;

	CHECK(int, 0, sigaction(SIGTERM, &sa, NULL));

	while (sock_fd == 0) {
		size_t len = sizeof(rbuf);
		struct req req = {};
		if (!recv_req(&req, rbuf, &len))
			continue;
		if (req.fp[1])
			stderr_set(req.fp[1]);
		req.argc = (int)SPLIT2ARGV(req.argv, rbuf, len);
		dispatch(&req);
		if (ferror(req.fp[0]) | fclose(req.fp[0]))
			perror("ferror|fclose fp[0]");
		if (req.fp[1]) {
			stderr_restore(req.fp[1]);
			if (ferror(req.fp[1]) | fclose(req.fp[1]))
				perror("ferror|fclose fp[1]");
		}
	}
}

static void insert_pid(pid_t pid, unsigned nr)
{
	assert(!worker_pids[nr]);
	worker_pids[nr] = pid;
}

static void start_worker(unsigned nr)
{
	pid_t pid = fork();
	if (pid < 0) {
		warn("E: fork(worker=%u)", nr);
	} else if (pid > 0) {
		insert_pid(pid, nr);
	} else {
		cleanup_pids();
		xclose(pipefds[0]);
		xclose(pipefds[1]);
		if (signal(SIGCHLD, SIG_DFL) == SIG_ERR)
			err(EXIT_FAILURE, "signal CHLD");
		if (signal(SIGTTIN, SIG_IGN) == SIG_ERR)
			err(EXIT_FAILURE, "signal TTIN");
		if (signal(SIGTTOU, SIG_IGN) == SIG_ERR)
			err(EXIT_FAILURE, "signal TTIN");
		recv_loop();
		exit(0);
	}
}

static void start_workers(void)
{
	sigset_t old;

	CHECK(int, 0, sigprocmask(SIG_SETMASK, &fullset, &old));
	for (unsigned long nr = 0; nr < nworker; nr++) {
		if (!worker_pids[nr])
			start_worker(nr);
	}
	CHECK(int, 0, sigprocmask(SIG_SETMASK, &old, NULL));
}

static void cleanup_all(void)
{
	cleanup_pids();
#ifdef __GLIBC__
	tdestroy(srch_tree, free_srch);
	srch_tree = NULL;
#endif
}

static void sigp(int sig) // parent signal handler
{
	static const char eagain[] = "signals coming in too fast";
	static const char bad_sig[] = "BUG: bad sig\n";
	static const char write_errno[] = "BUG: sigp write (errno)";
	static const char write_zero[] = "BUG: sigp write wrote zero bytes";
	char c = 0;

	switch (sig) {
	case SIGCHLD: c = '.'; break;
	case SIGTTOU: c = '-'; break;
	case SIGTTIN: c = '+'; break;
	default:
		write(STDERR_FILENO, bad_sig, sizeof(bad_sig) - 1);
		_exit(EXIT_FAILURE);
	}
	ssize_t w = write(pipefds[1], &c, 1);
	if (w > 0) return;
	if (w < 0 && errno == EAGAIN) {
		write(STDERR_FILENO, eagain, sizeof(eagain) - 1);
		return;
	} else if (w == 0) {
		write(STDERR_FILENO, write_zero, sizeof(write_zero) - 1);
	} else {
		// strerror isn't technically async-signal-safe, and
		// strerrordesc_np+strerrorname_np isn't portable
		write(STDERR_FILENO, write_errno, sizeof(write_errno) - 1);
	}
	_exit(EXIT_FAILURE);
}

static void reaped_worker(pid_t pid, int st)
{
	unsigned long nr = 0;
	for (; nr < nworker_hwm; nr++) {
		if (worker_pids[nr] == pid) {
			worker_pids[nr] = 0;
			break;
		}
	}
	if (nr >= nworker_hwm) {
		warnx("W: unknown pid=%d reaped $?=%d", (int)pid, st);
		return;
	}
	if (WIFEXITED(st) && WEXITSTATUS(st) == EX_NOINPUT)
		alive = false;
	else if (st)
		warnx("worker[%lu] died $?=%d alive=%d", nr, st, (int)alive);
	if (alive)
		start_workers();
}

static void do_sigchld(void)
{
	while (1) {
		int st;
		pid_t pid = waitpid(-1, &st, WNOHANG);
		if (pid > 0) {
			reaped_worker(pid, st);
		} else if (pid == 0) {
			return;
		} else {
			switch (errno) {
			case ECHILD: return;
			case EINTR: break; // can it happen w/ WNOHANG?
			default: err(EXIT_FAILURE, "BUG: waitpid");
			}
		}
	}
}

static void do_sigttin(void)
{
	if (!alive) return;
	if (nworker >= WORKER_MAX) {
		warnx("workers cannot exceed %zu", (size_t)WORKER_MAX);
		return;
	}
	void *p = realloc(worker_pids, (nworker + 1) * sizeof(pid_t));
	if (!p) {
		warn("realloc worker_pids");
	} else {
		worker_pids = (pid_t *)p;
		worker_pids[nworker++] = 0;
		if (nworker_hwm < nworker)
			nworker_hwm = nworker;
		start_workers();
	}
}

static void do_sigttou(void)
{
	if (!alive || nworker <= 1) return;

	// worker_pids array does not shrink
	--nworker;
	for (unsigned long nr = nworker; nr < nworker_hwm; nr++) {
		pid_t pid = worker_pids[nr];
		if (pid != 0 && kill(pid, SIGTERM))
			warn("BUG?: kill(%d, SIGTERM)", (int)pid);
	}
}

static size_t living_workers(void)
{
	size_t ret = 0;

	for (unsigned long nr = 0; nr < nworker_hwm; nr++) {
		if (worker_pids[nr])
			ret++;
	}
	return ret;
}

int main(int argc, char *argv[])
{
	int c;
	socklen_t slen = (socklen_t)sizeof(c);

	if (getsockopt(sock_fd, SOL_SOCKET, SO_TYPE, &c, &slen))
		err(EXIT_FAILURE, "getsockopt");
	if (c != SOCK_SEQPACKET)
		errx(EXIT_FAILURE, "stdin is not SOCK_SEQPACKET");

	mail_nrp_init();
	code_nrp_init();
	atexit(cleanup_all);

	if (!STDERR_ASSIGNABLE) {
		orig_err_fd = dup(STDERR_FILENO);
		if (orig_err_fd < 0)
			err(EXIT_FAILURE, "dup(2)");
	}

	nworker = 1;
#ifdef _SC_NPROCESSORS_ONLN
	long j = sysconf(_SC_NPROCESSORS_ONLN);
	if (j > 0)
		nworker = j > WORKER_MAX ? WORKER_MAX : j;
#endif // _SC_NPROCESSORS_ONLN

	// make warn/warnx/err multi-process friendly:
	if (my_setlinebuf(stderr))
		err(EXIT_FAILURE, "setlinebuf(stderr)");
	// not using -W<workers> like Daemon.pm, since -W is reserved (glibc)
	while ((c = getopt(argc, argv, "j:")) != -1) {
		char *end;

		switch (c) {
		case 'j':
			nworker = strtoul(optarg, &end, 10);
			if (*end != 0 || nworker > WORKER_MAX)
				errx(EXIT_FAILURE, "-j %s invalid", optarg);
			break;
		case ':':
			errx(EXIT_FAILURE, "missing argument: `-%c'", optopt);
		case '?':
			errx(EXIT_FAILURE, "unrecognized: `-%c'", optopt);
		default:
			errx(EXIT_FAILURE, "BUG: `-%c'", c);
		}
	}
	sigset_t pset; // parent-only
	CHECK(int, 0, sigfillset(&pset));

	// global sigsets:
	CHECK(int, 0, sigfillset(&fullset));
	CHECK(int, 0, sigfillset(&workerset));

#define DELSET(sig) do { \
	CHECK(int, 0, sigdelset(&fullset, sig)); \
	CHECK(int, 0, sigdelset(&workerset, sig)); \
	CHECK(int, 0, sigdelset(&pset, sig)); \
} while (0)
	DELSET(SIGABRT);
	DELSET(SIGBUS);
	DELSET(SIGFPE);
	DELSET(SIGILL);
	DELSET(SIGSEGV);
	DELSET(SIGXCPU);
	DELSET(SIGXFSZ);
#undef DELSET

	if (nworker == 0) { // no SIGTERM handling w/o workers
		recv_loop();
		return 0;
	}
	CHECK(int, 0, sigdelset(&workerset, SIGTERM));
	CHECK(int, 0, sigdelset(&workerset, SIGCHLD));
	nworker_hwm = nworker;
	worker_pids = (pid_t *)calloc(nworker, sizeof(pid_t));
	if (!worker_pids) err(EXIT_FAILURE, "calloc");

	if (pipe(pipefds)) err(EXIT_FAILURE, "pipe");
	int fl = fcntl(pipefds[1], F_GETFL);
	if (fl == -1) err(EXIT_FAILURE, "F_GETFL");
	if (fcntl(pipefds[1], F_SETFL, fl | O_NONBLOCK))
		err(EXIT_FAILURE, "F_SETFL");

	CHECK(int, 0, sigdelset(&pset, SIGCHLD));
	CHECK(int, 0, sigdelset(&pset, SIGTTIN));
	CHECK(int, 0, sigdelset(&pset, SIGTTOU));

	struct sigaction sa = {};
	sa.sa_handler = sigp;

	CHECK(int, 0, sigaction(SIGTTIN, &sa, NULL));
	CHECK(int, 0, sigaction(SIGTTOU, &sa, NULL));
	sa.sa_flags = SA_NOCLDSTOP;
	CHECK(int, 0, sigaction(SIGCHLD, &sa, NULL));

	CHECK(int, 0, sigprocmask(SIG_SETMASK, &pset, NULL));

	start_workers();

	char sbuf[64];
	while (alive || living_workers()) {
		ssize_t n = read(pipefds[0], &sbuf, sizeof(sbuf));
		if (n < 0) {
			if (errno == EINTR) continue;
			err(EXIT_FAILURE, "read");
		} else if (n == 0) {
			errx(EXIT_FAILURE, "read EOF");
		}
		do_sigchld();
		for (ssize_t i = 0; i < n; i++) {
			switch (sbuf[i]) {
			case '.': break; // do_sigchld already called
			case '-': do_sigttou(); break;
			case '+': do_sigttin(); break;
			default: errx(EXIT_FAILURE, "BUG: c=%c", sbuf[i]);
			}
		}
	}

	return 0;
}
