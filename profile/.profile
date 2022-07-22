# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

if [ -n "$KSH_VERSION" ]; then
    if [ -z "$ENV" ]; then
        export ENV=$HOME/.kshrc
    fi
fi

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.cargo/bin" ]; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

if [ -d "$HOME/.cabal/bin" ]; then
    PATH="$HOME/.cabal/bin:$PATH"
fi

if [ -n "$(command -v opam)" -a -d "$HOME/.opam" ]; then
    . $HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
fi

PLAN9=/usr/local/plan9
if [ -d "$PLAN9" ]; then
    export PLAN9
    PATH="$PATH:$PLAN9/bin"
fi

if [ -d "/usr/local/go" ]; then
    PATH="$PATH:/usr/local/go/bin"
fi

if [ -n "$(command -v vim)" ]; then
    export EDITOR="vim"
elif [ -n "$(command -v emacsclient)" ]; then
    export EDITOR="emacsclient -t -a="
fi
