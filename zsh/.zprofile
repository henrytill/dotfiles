#! /usr/bin/env zsh

if [[ $(uname) == Darwin ]]; then

    if [[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]]; then
        source $HOME/.nix-profile/etc/profile.d/nix.sh
        export NIX_PATH=$HOME/src/nixpkgs:nixpkgs=$HOME/src/nixpkgs
    fi

    if [[ -d $HOME/bin ]]; then
        export PATH=$HOME/bin:$PATH
    fi

    export EDITOR=emacsclient
    export LANG=en_US.UTF-8

fi
