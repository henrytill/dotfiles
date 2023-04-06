if [ $TERM = "dumb" ]; then
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

if [ -n "$(command -v emacsclient)" ]; then
    alias e="emacsclient -t"
    alias ec="emacsclient -cn"
    alias et="emacsclient -t"
fi

if [ -n "$(command -v wl-copy)" ]; then
    alias clear-clipboard="wl-copy --clear"
fi

alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"

# Local Variables:
# mode: sh
# End:
#
# vim: set filetype=sh:
