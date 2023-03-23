# Copyright (C) all contributors <meta@public-inbox.org>
# License: AGPL-3.0+ <https://www.gnu.org/licenses/agpl-3.0.txt>

# preliminary bash completion support for lei (Local Email Interface)
# Needs a lot of work, see `lei__complete' in lib/PublicInbox::LEI.pm
_lei() {
	local wordlist="$(lei _complete ${COMP_WORDS[@]})"
	wordlist="${wordlist//;/\\\\;}" # escape ';' for ';UIDVALIDITY' and such

	local word="${COMP_WORDS[COMP_CWORD]}"
	if test "$word" = ':' && test $COMP_CWORD -ge 1
	then
		COMPREPLY=($(compgen -W "$wordlist" --))
	else
		COMPREPLY=($(compgen -W "$wordlist" -- "$word"))
	fi
	return 0
}
complete -o default -o bashdefault -F _lei lei
