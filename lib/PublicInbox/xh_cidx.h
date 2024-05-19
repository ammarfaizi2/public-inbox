// Copyright (C) all contributors <meta@public-inbox.org>
// License: GPL-2.0+ <https://www.gnu.org/licenses/gpl-2.0.txt>
// This file is only intended to be included by xap_helper.h
// it implements pieces used by CodeSearchIdx.pm

// TODO: consider making PublicInbox::CodeSearchIdx emit binary
// (20 or 32-bit) OIDs instead of ASCII hex.  It would require
// more code in both Perl and C++, though...

// assumes trusted data from same host
static inline unsigned int hex2uint(char c)
{
	switch (c) {
	case '0' ... '9': return c - '0';
	case 'a' ... 'f': return c - 'a' + 10;
	default: return 0xff; // oh well...
	}
}

// assumes trusted data from same host
static kh_inline khint_t sha_hex_hash(const char *hex)
{
	khint_t ret = 0;

	for (size_t shift = 32; shift; )
		ret |= hex2uint(*hex++) << (shift -= 4);

	return ret;
}

KHASHL_CMAP_INIT(KH_LOCAL, root2id_map, root2id,
		const char *, const char *,
		sha_hex_hash, kh_eq_str)

static void term_length_extract(struct req *req)
{
	req->lenv = (size_t *)xcalloc(req->pfxc, sizeof(size_t));
	for (int i = 0; i < req->pfxc; i++) {
		char *pfx = req->pfxv[i];
		// extract trailing digits as length:
		// $len = s/([0-9]+)\z// ? ($1+0) : 0
		for (size_t j = 0; pfx[j]; j++) {
			if (pfx[j] < '0' || pfx[j] > '9')
				continue;
			if (j == 0) {
				warnx("W: `%s' not a valid prefix", pfx);
				continue;
			}
			char *end;
			unsigned long long tmp = strtoull(pfx + j, &end, 10);
			if (*end || tmp >= (unsigned long long)SIZE_MAX) {
				warnx("W: `%s' not recognized", pfx);
			} else {
				req->lenv[i] = (size_t)tmp;
				pfx[j] = 0;
				break;
			}
		}
	}
}

static void dump_ibx_term(struct req *req, int p,
			Xapian::Document *doc, const char *ibx_id)
{
	Xapian::TermIterator cur = doc->termlist_begin();
	Xapian::TermIterator end = doc->termlist_end();
	const char *pfx = req->pfxv[p];
	size_t pfx_len = strlen(pfx);
	size_t term_len = req->lenv[p];

	for (cur.skip_to(pfx); cur != end; cur++) {
		std::string tn = *cur;
		if (!starts_with(&tn, pfx, pfx_len)) break;
		if (term_len > 0 && (tn.length() - pfx_len) != term_len)
			continue;
		fprintf(req->fp[0], "%s %s\n", tn.c_str() + pfx_len, ibx_id);
		++req->nr_out;
	}
}

static enum exc_iter dump_ibx_iter(struct req *req, const char *ibx_id,
				Xapian::MSetIterator *i)
{
	try {
		Xapian::Document doc = i->get_document();
		for (int p = 0; p < req->pfxc; p++)
			dump_ibx_term(req, p, &doc, ibx_id);
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
	term_length_extract(req);
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

struct dump_roots_tmp {
	struct stat sb;
	void *mm_ptr;
	char **entries;
	struct fbuf wbuf;
	root2id_map *root2id;
	int root2off_fd;
};

#define CLEANUP_DUMP_ROOTS __attribute__((__cleanup__(dump_roots_ensure)))
static void dump_roots_ensure(void *ptr)
{
	struct dump_roots_tmp *drt = (struct dump_roots_tmp *)ptr;
	if (drt->root2off_fd >= 0)
		xclose(drt->root2off_fd);
	if (drt->root2id)
		root2id_cm_destroy(drt->root2id);
	size_t size = off2size(drt->sb.st_size);
	if (drt->mm_ptr && munmap(drt->mm_ptr, size))
		EABORT("BUG: munmap(%p, %zu)", drt->mm_ptr, size);
	free(drt->entries);
	fbuf_ensure(&drt->wbuf);
}

static bool root2offs_str(struct dump_roots_tmp *drt,
			struct fbuf *root_offs, Xapian::Document *doc)
{
	Xapian::TermIterator cur = doc->termlist_begin();
	Xapian::TermIterator end = doc->termlist_end();
	fbuf_init(root_offs);
	for (cur.skip_to("G"); cur != end; cur++) {
		std::string tn = *cur;
		if (!starts_with(&tn, "G", 1)) break;
		khint_t i = root2id_get(drt->root2id, tn.c_str() + 1);
		if (i >= kh_end(drt->root2id))
			ABORT("kh get miss `%s'", tn.c_str() + 1);
		fputc(' ', root_offs->fp);
		// kh_val(...) is a NUL-terminated string matching /[0-9]+/
		fputs(kh_val(drt->root2id, i), root_offs->fp);
	}
	fputc('\n', root_offs->fp);
	ERR_CLOSE(root_offs->fp, EXIT_FAILURE); // ENOMEM
	root_offs->fp = NULL;
	return true;
}

// writes term values matching @pfx for a given @doc, ending the line
// with the contents of @root_offs
static void dump_roots_term(struct req *req, int p,
				struct dump_roots_tmp *drt,
				struct fbuf *root_offs,
				Xapian::Document *doc)
{
	Xapian::TermIterator cur = doc->termlist_begin();
	Xapian::TermIterator end = doc->termlist_end();
	const char *pfx = req->pfxv[p];
	size_t pfx_len = strlen(pfx);
	size_t term_len = req->lenv[p];

	for (cur.skip_to(pfx); cur != end; cur++) {
		std::string tn = *cur;
		if (!starts_with(&tn, pfx, pfx_len)) break;
		if (term_len > 0 && (tn.length() - pfx_len) != term_len)
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
	bool ok = true;
	off_t off = ftello(drt->wbuf.fp);
	if (off < 0) EABORT("ftello");
	if (!off) return ok;

	ERR_FLUSH(drt->wbuf.fp); // ENOMEM
	int fd = fileno(req->fp[0]);

	while (flock(drt->root2off_fd, LOCK_EX)) {
		if (errno == EINTR) continue;
		err(EXIT_FAILURE, "LOCK_EX"); // ENOLCK?
	}
	ok = write_all(fd, &drt->wbuf, (size_t)off);
	while (flock(drt->root2off_fd, LOCK_UN)) {
		if (errno == EINTR) continue;
		err(EXIT_FAILURE, "LOCK_UN"); // ENOLCK?
	}
	if (fseeko(drt->wbuf.fp, 0, SEEK_SET)) EABORT("fseeko");
	return ok;
}

static enum exc_iter dump_roots_iter(struct req *req,
				struct dump_roots_tmp *drt,
				Xapian::MSetIterator *i)
{
	CLEANUP_FBUF struct fbuf root_offs = {}; // " $ID0 $ID1 $IDx..\n"
	try {
		Xapian::Document doc = i->get_document();
		if (!root2offs_str(drt, &root_offs, &doc))
			return ITER_ABORT; // bad request, abort
		for (int p = 0; p < req->pfxc; p++)
			dump_roots_term(req, p, drt, &root_offs, &doc);
	} catch (const Xapian::DatabaseModifiedError & e) {
		req->srch->db->reopen();
		return ITER_RETRY;
	} catch (const Xapian::DocNotFoundError & e) { // oh well...
		warnx("doc not found: %s", e.get_description().c_str());
	}
	return ITER_OK;
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
	// so /32 overestimates the number of expected entries
	size_t size = off2size(drt.sb.st_size);
	size_t est = (size / 32) + 1; //+1 for "\0" termination
	drt.mm_ptr = mmap(NULL, size, PROT_READ,
				MAP_PRIVATE, drt.root2off_fd, 0);
	if (drt.mm_ptr == MAP_FAILED)
		err(EXIT_FAILURE, "mmap(%zu, %s)", size, root2off_file);
	size_t asize = est * 2;
	if (asize < est) ABORT("too many entries: %zu", est);
	drt.entries = (char **)xcalloc(asize, sizeof(char *));
	size_t tot = split2argv(drt.entries, (char *)drt.mm_ptr, size, asize);
	if (tot <= 0) return false; // split2argv already warned on error
	drt.root2id = root2id_init();
	root2id_cm_resize(drt.root2id, est);
	for (size_t i = 0; i < tot; ) {
		int absent;
		const char *key = drt.entries[i++];
		khint_t k = root2id_put(drt.root2id, key, &absent);
		if (!absent)
			err(EXIT_FAILURE, "put(%s => %s, ENTER)",
				key, drt.entries[i]);
		kh_val(drt.root2id, k) = drt.entries[i++];
	}
	req->asc = true;
	req->sort_col = -1;
	Xapian::MSet mset = commit_mset(req, req->argv[optind + 1]);
	term_length_extract(req);

	fbuf_init(&drt.wbuf);

	// @UNIQ_FOLD in CodeSearchIdx.pm can handle duplicate lines fine
	// in case we need to retry on DB reopens
	for (Xapian::MSetIterator i = mset.begin(); i != mset.end(); i++) {
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
