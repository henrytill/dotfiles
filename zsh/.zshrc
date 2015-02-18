#! /usr/bin/env zsh

autoload -U promptinit
promptinit
prompt walters
PROMPT=$'\n'$PROMPT

setopt autocd
setopt beep
setopt extendedglob
setopt histfcntllock
setopt histignoredups
setopt sharehistory

HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000

bindkey -e

# completion
zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit
compinit

zstyle ':completion:*' menu select

# window titles
if [[ $TERM = xterm* ]]; then
    function set-title() {
        echo -en "\e]2;$USER@$HOST: $2\a"
    }
    autoload -Uz add-zsh-hook
    add-zsh-hook preexec set-title
fi

# aliases
alias e="$EDITOR"
alias l="clear && pwd && ls -lh"
alias la="clear && pwd && ls -lah"
alias lf="ls -aF"
alias ll="ls -la"
alias llt="ls -lat"
alias lt="ls -lt"
alias u="cd .. && l"

if [[ -e $(which htop) ]]; then
    alias htop="TERM=xterm htop"
fi

if [[ -e $HOME/.nix-profile && -n $(type -p nix-env) ]]; then
    p() {
        clear
        echo 'Current Profile: ' && readlink $HOME/.nix-profile
        echo && echo 'Installed:' && nix-env -q
    }
fi

# Darwin-specific config
if [[ $(uname) == Darwin ]]; then
    alias ls="ls -G"

    if [[  -d $HOME/.nix-profile/Applications/Emacs.app/ ]]; then
        alias Emacs.app=$HOME/.nix-profile/Applications/Emacs.app/Contents/MacOS/Emacs
    fi

    export GPG_TTY=$(tty)
fi
