# .profile

if [ "$(uname -s)" = "Darwin" ]; then

    if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
        . "$HOME/.nix-profile/etc/profile.d/nix.sh"
        export NIX_PATH="$HOME/src/nixpkgs:nixpkgs=$HOME/src/nixpkgs"

        if [ -e "$HOME/.nix-profile/bin/git" \
                -a -e "/etc/ssl/certs/certificates.pem" ]; then
            export GIT_SSL_CAINFO="/etc/ssl/certs/certificates.pem"
        fi
    fi

    if [ -d "$HOME/bin" ]; then
        export PATH="$HOME/bin:$PATH"
    fi

    export LANG="en_US.UTF-8"

    export EDITOR="emacsclient -t --alternate-editor="

    if [ -n "$BASH_VERSION" -a -f "$HOME/.bashrc" ]; then
        source "$HOME/.bashrc"
    fi
fi
