# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000
HISTFILESIZE=100000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -n "$(command -v lesspipe)" ] && eval "$(SHELL=/bin/sh lesspipe)"

case "$TERM" in
xterm*|rxvt*|foot*|screen*)
    PS1="\n${debian_chroot:+($debian_chroot)}\[\e[1m\]\u@\h\[\e[0m\] \w\\$ "
    ;;
eterm*|dumb)
    PS1="\n${debian_chroot:+($debian_chroot)}\u@\h \w\\$ "
    ;;
esac

# Set the title
case "$TERM" in
xterm*|rxvt*|foot*|screen*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h \w\a\]$PS1"
    ;;
*)
    ;;
esac

if [ -n "$(command -v dircolors)" ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    # alias grep='grep --color=auto'
    # alias fgrep='fgrep --color=auto'
    # alias egrep='egrep --color=auto'
fi

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f ~/.bash_functions ]; then
    . ~/.bash_functions
fi

if [ $TERM = "dumb" ]; then
    export PAGER=cat
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

OPAM_COMPLETE_SH="$HOME/.opam/opam-init/complete.sh"
OPAM_ENV_HOOK_SH="$HOME/.opam/opam-init/env_hook.sh"
if [ -r "$OPAM_COMPLETE_SH" ] && [ -r "$OPAM_ENV_HOOK_SH" ]; then
    . "$OPAM_COMPLETE_SH" >/dev/null 2>/dev/null || true
    . "$OPAM_ENV_HOOK_SH" >/dev/null 2>/dev/null || true
fi

if [ -n "$SSH_CLIENT" ]; then
    if [[ $PS1 =~ (.*)"\\$" ]]; then
        PS1="${BASH_REMATCH[1]} [ssh]\\\$ "
    fi
fi

# Automatically added by the Guix install script.
if [ -n "$GUIX_ENVIRONMENT" ]; then
    if [[ $PS1 =~ (.*)"\\$" ]]; then
        PS1="${BASH_REMATCH[1]} [env]\\\$ "
    fi
fi

if [ -z "$SSH_AUTH_SOCK" ]; then
    if [ -e $XDG_RUNTIME_DIR/openssh_agent ]; then
        export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/openssh_agent"
    elif ! [ -e /tmp/ssh-agent-$USER ]; then
        ssh-agent 2>/dev/null >/tmp/ssh-agent-$USER
    else
        . /tmp/ssh-agent-$USER >/dev/null
    fi
fi

if [ -n "$(command -v tty)" ]; then
    export GPG_TTY="$(tty)"
fi
