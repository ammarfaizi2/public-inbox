/*
 * Copyright (C) all contributors <meta@public-inbox.org>
 * License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>
 *
 * multiline configs broken before 1.8, I only noticed the former myself:
 * dff05bc30 (Multiline config values not preserved on saving, 2021-11-25)
 * de9a76b92 (config: properly handle multiline quotes, 2023-12-14)
 */
#define CFG_OP_EQ(op, cmd0, dlen) \
	(dlen == (sizeof(op) - 1) && !memcmp(op, cmd0, sizeof(op) - 1))

/* leaks on internal bugs: */
#define FETCH_CSTR(var, ary, i) do { \
	SV **s = av_fetch(ary, i, 0); \
	if (!s) croak("BUG: " #var " = ary[%d] is NULL", i); \
	var = SvPV_nolen(*s); \
} while (0)

void cfgwr_commit(const char *f, AV *todo)
{
	I32 i, todo_max = av_len(todo);
	git_config *cfg;
	const git_error *e;
	const char *cmd0, *name, *val, *re;
	int rc = git_config_open_ondisk(&cfg, f);
	STRLEN dlen;

	for (i = 0; rc == GIT_OK && i <= todo_max; i++) {
		SV **sv = av_fetch(todo, i, 0);
		AV *cmd;

		/* leaks on internal bugs: */
		if (!SvROK(*sv)) croak("BUG: not a ref");
		cmd = (AV *)SvRV(*sv);
		sv = av_fetch(cmd, 0, 0);
		if (!sv) croak("BUG: cmd0 = $todo->[%d]->[0] is NULL", i);
		cmd0 = SvPV(*sv, dlen);
		if (CFG_OP_EQ("--add", cmd0, dlen)) {
			FETCH_CSTR(name, cmd, 1);
			FETCH_CSTR(val, cmd, 2);
			rc = git_config_set_multivar(cfg, name, "$^", val);
		} else if (CFG_OP_EQ("--unset-all", cmd0, dlen)) {
			FETCH_CSTR(name, cmd, 1);
			rc = git_config_delete_multivar(cfg, name, ".*");
			if (rc == GIT_ENOTFOUND)
				rc = GIT_OK;
		} else if (CFG_OP_EQ("--replace-all", cmd0, dlen)) {
			FETCH_CSTR(name, cmd, 1);
			FETCH_CSTR(val, cmd, 2);
			FETCH_CSTR(re, cmd, 3);
			rc = git_config_set_multivar(cfg, name, re, val);
		} else {
			name = cmd0;
			FETCH_CSTR(val, cmd, 1);
			rc = git_config_set_string(cfg, name, val);
		}
	}
	e = rc == GIT_OK ? NULL : giterr_last();
	git_config_free(cfg);
	if (rc != GIT_OK)
		croak("config -f %s (%d %s)",
			f, rc, e ? e->message : "unknown");
}
