#! /usr/bin/env zsh

if [[ $(uname) == Darwin ]]; then

    if [[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]]; then
        source $HOME/.nix-profile/etc/profile.d/nix.sh
        export NIX_PATH=nixpkgs=$HOME/src/nixpkgs
    fi

    if [[ -n $(command -v opam) && -d $HOME/.opam ]]; then
      . $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
    fi
fi
