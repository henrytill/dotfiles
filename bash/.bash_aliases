alias e="$EDITOR"
alias ec="emacsclient -cn"
alias et="emacsclient -t"
alias lc="ls -C"
alias lf="ls -aF"
alias ll="ls -la"
alias llt="ls -lat"
alias lt="ls -lt"
alias u="cd .. && l"
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"

if [ -n "$(command -v view)" ]; then
    alias v="view"
fi

if [ $TERM = "dumb" ]; then
    alias less="cat"
    alias more="cat"
    alias l="ls -lh"
    alias la="ls -lah"
    export PAGER="cat"
else
    alias l="clear && ls -lh"
    alias la="clear && ls -lah"
fi

# Local Variables:
# mode: sh
# End:
#
# vim: set filetype=sh:
