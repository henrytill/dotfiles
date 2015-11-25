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
    if [[ $TERM == dumb ]]; then
        unsetopt zle

        local retStatus='[%?]'
        local nixShellMode=${IN_NIX_SHELL/1/'[nix-shell]'}

        PROMPT=$'\n'$retStatus$nixShellMode'> '
        PROMPT2=$nixshellMode'> '
        RPROMPT=''
    else
        local firstLine='%F{2}%B%n@%m:%~%f%b'
        local retStatus='%(?.[%?].%F{1}[%?]%f)'
        local nixShellMode=${IN_NIX_SHELL/1/'%F{4}[nix-shell]%f'}

        PROMPT=$'\n'$firstLine$'\n'$retStatus$nixShellMode'> '
        PROMPT2=$nixshellMode'> '
        RPROMPT=''
    fi
}

# completion
zstyle :compinstall filename '$HOME/.zshrc'

if [[ -d $HOME/src/other/nix-zsh-completions ]]; then
    source $HOME/src/other/nix-zsh-completions/nix.plugin.zsh
    fpath=($HOME/src/other/nix-zsh-completions $fpath)
fi

autoload -Uz compinit
compinit

zstyle ':completion:*' menu select

# window titles
if [[ $TERM == xterm* ]]; then
    function set-title() {
        echo -en "\e]2;$USER@$HOST: $2\a"
    }
    autoload -Uz add-zsh-hook
    add-zsh-hook preexec set-title
fi

# env
export EDITOR='emacsclient -t --alternate-editor='

# aliases
alias e="$EDITOR"
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

if [[ -n $(command -v ed) && -n $(command -v rlwrap) ]]; then
    alias ed="rlwrap ed"
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
    npm-exec () {
        PATH=$(npm bin):$PATH $*
    }
fi

if [[ -e $HOME/.nix-profile && -n $(command -v nix-env) ]]; then
    p() {
        clear
        echo 'Current Profile: ' && readlink $HOME/.nix-profile
        echo && echo 'Installed:' && nix-env -q
    }
fi

if [[ -n $TMUX ]]; then
    alias emacsclient="TERM=xterm-256color emacsclient"
    alias emacs="TERM=xterm-256color emacs"
fi

# Darwin-specific config
if [[ $(uname) == Darwin ]]; then
    alias ls="ls -G"

    if [[  -d $HOME/.nix-profile/Applications/Emacs.app/ ]]; then
        alias Emacs.app="open -n -a $HOME/.nix-profile/Applications/Emacs.app"
    fi
fi

# Linux-specific config
if [[ $(uname) == Linux ]]; then
    alias ls="ls --color"
fi
