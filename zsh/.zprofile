#! /usr/bin/env zsh

if [[ $(uname) == Darwin ]]; then
    if [[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]]; then
        source $HOME/.nix-profile/etc/profile.d/nix.sh
        export NIX_PATH=nixpkgs=$HOME/src/nixpkgs
    fi

    if [[ -n $NIX_LINK && -d $NIX_LINK/lib/aspell ]]; then
        export ASPELL_CONF='dict-dir $NIX_LINK/lib/aspell'
    fi

    if [[ -d /opt/apache-maven-3.3.3 ]]; then
        export PATH=/opt/apache-maven-3.3.3/bin:$PATH
    fi

    if [[ -d /opt/gradle-2.4 ]]; then
        export GRADLE_HOME=/opt/gradle-2.4
        export PATH=$GRADLE_HOME/bin:$PATH
    fi

    if [[ -d $HOME/bin ]]; then
        export PATH=$HOME/bin:$PATH
    fi

    export LANG=en_US.UTF-8
fi

if [[ $(uname) == Linux && ! -d /etc/nixos ]]; then
    if [[ -d $HOME/bin ]]; then
        export PATH=$HOME/bin:$PATH
    fi
fi

if [[ -n $(command -v lein) ]]; then
    export LEIN_FAST_TRAMPOLINE=y
fi

export EDITOR='emacsclient -t --alternate-editor='
