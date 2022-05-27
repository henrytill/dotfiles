# .zshrc

setopt autocd
setopt beep
setopt extendedglob
setopt histfcntllock
setopt histignoredups
setopt sharehistory

export HISTFILE=~/.histfile
export HISTSIZE=100000
export SAVEHIST=100000

bindkey -e

# check if in docker container
if [[ -f /.dockerenv ]]
then
    IN_DOCKER_CONTAINER=1
fi

# prompt
create_prompt () {
    local inDocker=${IN_DOCKER_CONTAINER/1/"[docker]"}
    local inNixShell=${IN_NIX_SHELL/1/"[$name]"}

    if [[ $TERM == dumb ]]
    then
        unsetopt zle
        local retStatus='[%?]'
        PROMPT=$'\n'$retStatus$inDocker$inNixShell'> '
        PROMPT2=$inDocker$inNixShell'> '
        RPROMPT=''
    else
        local firstLine='%B%n@%m:%~%b'
        local retStatus='%(?.[%?].%F{1}[%?]%f)'
        PROMPT=$'\n'$firstLine$'\n'$retStatus$inDocker$inNixShell'> '
        PROMPT2=$inDocker$inNixShell'> '
        RPROMPT=''
    fi
}

create_prompt

# completion
zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit
compinit

autoload -Uz bashcompinit
bashcompinit

zstyle ':completion:*' menu select

# window titles
if [[ $TERM == xterm* ]]
then
    function set-title() {
        echo -en "\e]2;$USER@$HOST: $2\a"
    }
    autoload -Uz add-zsh-hook
    add-zsh-hook preexec set-title
fi

# Linux-specific config
if [[ "$(uname -s)" == "Linux" ]]
then
    alias ls="ls --color"
fi

# aliases
alias e="$EDITOR"
alias ec="emacsclient -c -a="
alias et="emacsclient -t -a="
alias kill-emacs-server="emacsclient -e '(kill-emacs)'"
alias lf="ls -aF"
alias ll="ls -la"
alias llt="ls -lat"
alias lt="ls -lt"
alias u="cd .. && l"
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
alias cls="tput reset"

if [[ -n "$(command -v view)" ]]
then
    alias v="view"
fi

if [[ $TERM == "dumb" ]]
then
    alias less="cat"
    alias more="cat"
    alias l="ls -lh"
    alias la="ls -lah"
    export PAGER="cat"
else
    alias l="clear && ls -lh"
    alias la="clear && ls -lah"
fi

if [[ -n "$(command -v nix-shell)" ]]
then
    alias nix-zshell="nix-shell --command zsh"
fi

if [[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ]]
then
    source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"
fi

if [[ -n "$(command -v opam)" && -d "$HOME/.opam" ]]
then
    source $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
fi

if [[ -n "$(command -v hecate)" && -d "$HOME/.hecate" ]]
then
    source <(hecate --bash-completion-script `which hecate`)
fi

if [[ -n "$(command -v direnv)" ]]
then
    eval "$(direnv hook zsh)"
fi

export GPG_TTY=$(tty)
