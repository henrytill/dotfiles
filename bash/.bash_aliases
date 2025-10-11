if test "${TERM}" = "dumb"
then
	alias l="ls -lh"
	alias la="ls -lah"
else
	alias l="clear && ls -lh"
	alias la="clear && ls -lah"
fi

alias llt="ls -lat"
alias lt="ls -lt"
alias u="cd .. && l"

if test -n "$(command -v editor)"
then
	alias e="editor"
fi

if test -n "$(command -v wl-copy)"
then
	alias clc="wl-copy --clear"
fi

if test -n "$(command -v lsblk)"
then
	alias lsblk="lsblk -o +kname"
fi
