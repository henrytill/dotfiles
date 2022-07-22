set -o emacs

HISTFILE=~/.ksh_history

case "$TERM" in
xterm*|rxvt*)
    PS1="\n\u@\h:\w\n[\$?]> "
    PS2="> "
    ;;
dumb)
    PS1="[\$?]> "
    PS2="> "
    ;;
esac

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

export GPG_TTY="$(tty)"
export LIBVIRT_DEFAULT_URI="qemu:///system"

# Local Variables:
# mode: sh
# End:
#
# vim: set filetype=sh:
