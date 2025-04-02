/*
 * Copyright (C) all contributors <meta@public-inbox.org>
 * License: GPL-3.0+ <https://www.gnu.org/licenses/gpl-3.0.txt>
 *
 * Trailer uploader using libcurl since we could've been reading RFCs
 * wrong the whole time and no other client seems to support sending
 * Trailers.
 *
 * Built and used by t/httpd-corner.t
 *
 * .h suffix (not .c) to avoid MakeMaker trying to build this for XS
 */
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

#define MY_SETOPT(hnd, opt, param) do { \
	CURLcode ret = curl_easy_setopt(hnd, opt, param); \
	if (ret != CURLE_OK) { \
		fprintf(stderr, "curl_easy_setopt: %s\n", \
			curl_easy_strerror(ret)); \
		abort(); \
	} \
} while (0)

static int trailer_cb(struct curl_slist **tr, void *data)
{
	/* libcurl frees the list */
	*tr = curl_slist_append(*tr, "a: b");
	return CURL_TRAILERFUNC_OK;
}

int main(int argc, char *argv[])
{
	CURLcode ret;
	CURL *hnd;
	char ebuf[CURL_ERROR_SIZE];
	struct curl_slist *hdr = NULL;

	if (argc < 2) {
		fprintf(stderr, "%s URL\n", argv[0]);
		return 1;
	}
	hnd = curl_easy_init();
	if (!hnd)
		abort();
	if (!(hdr = curl_slist_append(hdr, "Expect:"))) // clobber
		abort();
	if (!(hdr = curl_slist_append(hdr, "Trailer: a")))
		abort();
	MY_SETOPT(hnd, CURLOPT_URL, argv[1]);
	MY_SETOPT(hnd, CURLOPT_HTTPHEADER, hdr);
	MY_SETOPT(hnd, CURLOPT_NOPROGRESS, 1L);
	MY_SETOPT(hnd, CURLOPT_FAILONERROR, 1L);
	MY_SETOPT(hnd, CURLOPT_UPLOAD, 1L);
	MY_SETOPT(hnd, CURLOPT_HTTP_VERSION,
			(long)CURL_HTTP_VERSION_1_1);
	MY_SETOPT(hnd, CURLOPT_TRAILERFUNCTION, trailer_cb);
	MY_SETOPT(hnd, CURLOPT_ERRORBUFFER, ebuf);
	MY_SETOPT(hnd, CURLOPT_VERBOSE, 1L);

	MY_SETOPT(hnd, CURLOPT_READFUNCTION, fread);
	MY_SETOPT(hnd, CURLOPT_READDATA, stdin);

	MY_SETOPT(hnd, CURLOPT_WRITEFUNCTION, fwrite);
	MY_SETOPT(hnd, CURLOPT_WRITEDATA, stdout);

	ret = curl_easy_perform(hnd);
	if (ret != CURLE_OK)
		fprintf(stderr, "curl_easy_perform: %s\n",
			curl_easy_strerror(ret));

	curl_slist_free_all(hdr);
	curl_easy_cleanup(hnd);

	return (int)ret;
}
