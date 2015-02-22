#! /usr/bin/env zsh

if [[ $(uname) == Darwin ]]; then

    if [[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]]; then
        source $HOME/.nix-profile/etc/profile.d/nix.sh
        export NIX_PATH=$HOME/src/nixpkgs:nixpkgs=$HOME/src/nixpkgs
    fi

    if [[ -n $NIX_LINK && -f $NIX_LINK/etc/X11/fonts.conf ]]; then
        export FONTCONFIG_FILE=$NIX_LINK/etc/X11/fonts.conf
    fi

    if [[ -d $HOME/bin ]]; then
        export PATH=$HOME/bin:$PATH
    fi

    export EDITOR='emacsclient -t --alternate-editor='
    export LANG=en_US.UTF-8

fi

if [[ -f /etc/gentoo-release ]]; then

    if [[ -d $HOME/bin ]]; then
        export PATH=$HOME/bin:$PATH
    fi

    export EDITOR='emacsclient -t --alternate-editor='

fi
