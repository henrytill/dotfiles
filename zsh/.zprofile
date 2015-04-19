#! /usr/bin/env zsh

if [[ $(uname) == Darwin ]]; then

    if [[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]]; then
        source $HOME/.nix-profile/etc/profile.d/nix.sh
        export NIX_PATH=$HOME/src/nixpkgs:nixpkgs=$HOME/src/nixpkgs
    fi

    if [[ -n $NIX_LINK && -d $NIX_LINK/lib/aspell ]]; then
        export ASPELL_CONF="dict-dir $NIX_LINK/lib/aspell"
    fi

    if [[ -n $NIX_LINK && -f $NIX_LINK/etc/X11/fonts.conf ]]; then
        export FONTCONFIG_FILE=$NIX_LINK/etc/X11/fonts.conf
    fi

    if [[ -n $NIX_LINK && -e $NIX_LINK/bin/ghc ]]; then
        NIX_GHC_VERSION=$(ghc --numeric-version)
        export NIX_GHC="$NIX_LINK/bin/ghc"
        export NIX_GHCPKG="$NIX_LINK/bin/ghc-pkg"
        export NIX_GHC_DOCDIR="$NIX_LINK/share/doc/ghc/html"
        export NIX_GHC_LIBDIR="$NIX_LINK/lib/ghc-${NIX_GHC_VERSION}"
    fi

    if [[ -d $HOME/bin ]]; then
        export PATH=$HOME/bin:$PATH
    fi

    export EDITOR='emacsclient -t --alternate-editor='
    export LANG=en_US.UTF-8

fi

if [[ -d /etc/nixos ]]; then

    if [[ -e /run/current-system/sw/bin/ghc ]]; then
        NIX_GHC_VERSION=$(ghc --numeric-version)
        export NIX_GHC="/run/current-system/sw/bin/ghc"
        export NIX_GHCPKG="/run/current-system/sw/bin/ghc-pkg"
        export NIX_GHC_DOCDIR="/run/current-system/sw/share/doc/ghc/html"
        export NIX_GHC_LIBDIR="/run/current-system/sw/lib/ghc-${NIX_GHC_VERSION}"
    fi

fi

if [[ $(uname) == Linux && ! -d /etc/nixos ]]; then

    if [[ -d $HOME/.cabal/bin/ ]]; then
        export PATH=$HOME/.cabal/bin:$PATH
    fi

    if [[ -d $HOME/bin ]]; then
        export PATH=$HOME/bin:$PATH
    fi

    export EDITOR='emacsclient -t --alternate-editor='

fi
