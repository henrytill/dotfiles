set -o emacs

HISTFILE=~/.ksh_history

case "$TERM" in
xterm*|rxvt*)
    PS1="\n\u@\h:\w:\j\n[\$?]> "
    PS2="> "
    ;;
dumb)
    PS1="[\$?]> "
    PS2="> "
    ;;
esac

if [ -f ~/.ksh_aliases ]; then
    . ~/.ksh_aliases
fi

if [ -f ~/.ksh_functions ]; then
    . ~/.ksh_functions
fi


# Local Variables:
# mode: sh
# End:
#
# vim: set filetype=sh:
