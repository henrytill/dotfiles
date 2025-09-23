if test "${TERM}" = "dumb"
then
	alias less="cat"
	alias more="cat"
	alias l="ls -lh"
	alias la="ls -lah"
else
	alias l="clear && ls -lh"
	alias la="clear && ls -lah"
fi

alias lc="ls -C"
alias lf="ls -aF"
alias ll="ls -la"
alias llt="ls -lat"
alias lt="ls -lt"
alias u="cd .. && l"

if test -n "$(command -v emacsclient)"
then
	alias e="emacsclient -t"
	alias ec="emacsclient -cn"
	alias et="emacsclient -t"
fi

if test -n "$(command -v wl-copy)"
then
	alias clc="wl-copy --clear"
fi

if test -n "$(command -v lsblk)"
then
	alias lsblk="lsblk -o +kname"
fi

# Local Variables:
# mode: sh
# sh-basic-offset: 8
# indent-tabs-mode: t
# End:
