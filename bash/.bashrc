# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

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
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

case "$TERM" in
xterm*|rxvt*|foot*)
    PS1="\n\[\e[1m\]\u@\h:\w\[\e[0m\]\n[\$?]> "
    PS2="> "
    ;;
eterm*|dumb)
    PS1="\n[\$?]> "
    PS2="> "
    ;;
esac

if [ -z "$SSH_AUTH_SOCK" ]; then
    if [ -e $XDG_RUNTIME_DIR/openssh_agent ]; then
        export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/openssh_agent"
    elif ! [ -e /tmp/ssh-agent-$USER ]; then
        ssh-agent 2>/dev/null >/tmp/ssh-agent-$USER
    else
        . /tmp/ssh-agent-$USER >/dev/null
    fi
fi

if [ -n "$(command -v emacsclient)" ]; then
    export EDITOR="emacsclient -t"
    export ALTERNATE_EDITOR=""
elif [ -n "$(command -v mg)" ]; then
    export EDITOR="mg"
fi

export GPG_TTY="$(tty)"
export LIBVIRT_DEFAULT_URI="qemu:///system"
export _JAVA_AWT_WM_NONREPARENTING=1

# https://github.com/swaywm/sway/issues/5759
# https://github.com/swaywm/sway/issues/5008
if [ "$(hostname)" = "thalassa" ]
then
    export WLR_DRM_NO_MODIFIERS=1
fi

if [ -x /usr/bin/dircolors ]; then
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
