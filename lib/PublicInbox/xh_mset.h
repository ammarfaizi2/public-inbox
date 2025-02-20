// Copyright (C) all contributors <meta@public-inbox.org>
// License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
// This file is only intended to be included by xap_helper.h
// it implements pieces used by WWW, IMAP and lei

#ifndef WBUF_FLUSH_THRESHOLD
#	define WBUF_FLUSH_THRESHOLD (BUFSIZ - 1000)
#endif
#if WBUF_FLUSH_THRESHOLD < 0
#	undef WBUF_FLUSH_THRESHOLD
#	define WBUF_FLUSH_THRESHOLD BUFSIZ
#endif

static bool cmd_mset(struct req *req)
{
	if (optind >= req->argc) ABORT("usage: mset [OPTIONS] WANT QRY_STR");
	if (req->fp[1]) ABORT("mset only accepts 1 FD");
	const char *qry_str = req->argv[optind];
	CLEANUP_FBUF struct fbuf wbuf = {};
	Xapian::MSet mset = req->code_search ? commit_mset(req, qry_str) :
						mail_mset(req, qry_str);
	fbuf_init(&wbuf);
	fprintf(wbuf.fp, "mset.size=%llu .get_matches_estimated=%llu\n",
		(unsigned long long)mset.size(),
		(unsigned long long)mset.get_matches_estimated());
	int fd = fileno(req->fp[0]);
	for (Xapian::MSetIterator i = mset.begin(); i != mset.end(); i++) {
		off_t off = ftello(wbuf.fp);
		if (off < 0) EABORT("ftello");
		/*
		 * TODO verify our fflush + fseeko use isn't affected by a
		 * glibc <2.25 bug:
		 * https://sourceware.org/bugzilla/show_bug.cgi?id=20181
		 * CentOS 7.x only has glibc 2.17.  In any case, bug #20181
		 * shouldn't affect us since our use of fseeko is used to
		 * effectively discard data.
		 */
		if (off > WBUF_FLUSH_THRESHOLD) {
			ERR_FLUSH(wbuf.fp);
			if (!write_all(fd, &wbuf, (size_t)off)) return false;
			if (fseeko(wbuf.fp, 0, SEEK_SET)) EABORT("fseeko");
			off = 0;
		}
		fprintf(wbuf.fp, "%llu" "%c" "%d" "%c" "%llu\n",
			(unsigned long long)(*i), // get_docid
			0, i.get_percent(),
			0, (unsigned long long)i.get_rank());
	}
	off_t off = ftello(wbuf.fp);
	if (off < 0) EABORT("ftello");
	ERR_FLUSH(wbuf.fp);
	return off > 0 ? write_all(fd, &wbuf, (size_t)off) : true;
}
