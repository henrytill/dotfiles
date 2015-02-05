# .profile

if [ "$(uname -s)" = "Darwin" ]; then

    if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
        . "$HOME/.nix-profile/etc/profile.d/nix.sh"
        export NIX_PATH="$HOME/src/nixpkgs:nixpkgs=$HOME/src/nixpkgs"
    fi

    if [ -d "$HOME/bin" ]; then
        export PATH="$HOME/bin:$PATH"
    fi

    export EDITOR="emacsclient"
    export LANG="en_US.UTF-8"

    if [ -n "$BASH_VERSION" -a -f "$HOME/.bashrc" ]; then
        source "$HOME/.bashrc"
    fi

fi
