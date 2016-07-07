#! /usr/bin/env zsh

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

# prompt
() {
    local nixShellMode=${IN_NIX_SHELL/1/"[$name]"}

    if [[ $TERM == dumb ]]; then
        unsetopt zle

        local retStatus='[%?]'

        PROMPT=$'\n'$retStatus$nixShellMode'> '
        PROMPT2=$nixshellMode'> '
        RPROMPT=''
    else
        local firstLine='%F{6}%B%n@%m:%~%f%b'
        local retStatus='%(?.[%?].%F{1}[%?]%f)'

        PROMPT=$'\n'$firstLine$'\n'$retStatus$nixShellMode'> '
        PROMPT2=$nixshellMode'> '
        RPROMPT=''
    fi
}

# completion
zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit
compinit

zstyle ':completion:*' menu select

# window titles
if [[ $TERM == rxvt* ]]; then
    function set-title() {
        echo -en "\e]2;$USER@$HOST: $2\a"
    }
    autoload -Uz add-zsh-hook
    add-zsh-hook preexec set-title
fi

# Darwin-specific config
if [[ $(uname) == Darwin ]]; then
    alias ls="ls -G"

    if [[  -d /Applications/Emacs.app/ ]]; then
        alias Emacs.app="open -n -a /Applications/Emacs.app"
    fi

    export EDITOR="emacsclient -t"
fi

# Linux-specific config
if [[ $(uname) == Linux ]]; then
    alias ls="ls --color"

    export EDITOR="emacsclient -t --alternate-editor="
fi

# aliases
alias e="$EDITOR"
alias v="vim"
alias lf="ls -aF"
alias ll="ls -la"
alias llt="ls -lat"
alias lt="ls -lt"
alias u="cd .. && l"

if [[ $TERM == dumb ]]; then
    alias less="cat"
    alias more="cat"
    alias l="ls -lh"
    alias la="ls -lah"
    export PAGER="cat"
else
    alias l="clear && ls -lh"
    alias la="clear && ls -lah"
fi

if [[ -n $(command -v gpg2) ]]; then
    alias gpg="gpg2"
fi

if [[ -n $(command -v htop) ]]; then
    alias htop="TERM=xterm htop"
fi

if [[ -n $(command -v nix-shell) ]]; then
    alias nix-zshell="nix-shell --command zsh"
fi

if [[ -n $(command -v npm) ]]; then
    npm-exec () { PATH=$(npm bin):$PATH $* }
fi
