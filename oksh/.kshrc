set -o emacs

HISTFILE=~/.ksh_history
HISTSIZE=5000

BOLD=$(tput bold)
NORM=$(tput sgr0)

case "$TERM" in
xterm*|rxvt*|screen*)
    PS1="\n${BOLD}\u@\h:\w${NORM}\n[\$?]> "
    PS2="> "
    ;;
dumb)
    PS1="\n[\$?]> "
    PS2="> "
    ;;
esac

if [ -x /usr/bin/dircolors ]
then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    # alias grep='grep --color=auto'
    # alias fgrep='fgrep --color=auto'
    # alias egrep='egrep --color=auto'
fi

if [ -f ~/.ksh_aliases ]
then
    . ~/.ksh_aliases
fi

if [ -f ~/.ksh_functions ]
then
    . ~/.ksh_functions
fi

if [ -z "$SSH_AGENT_PID" -a -z "$SSH_AUTH_SOCK" ]
then
    if ! [ -e /tmp/ssh-agent-$USER ]
    then
        ssh-agent 2>/dev/null >/tmp/ssh-agent-$USER
    fi
    . /tmp/ssh-agent-$USER >/dev/null
fi

if [ -n "$(command -v emacsclient)" ]
then
    export EDITOR="emacsclient -t -a="
elif [ -n "$(command -v mg)" ]
then
    export EDITOR="mg"
fi

export GPG_TTY="$(tty)"
export LIBVIRT_DEFAULT_URI="qemu:///system"

# Local Variables:
# mode: sh
# End:
#
# vim: set filetype=sh:
