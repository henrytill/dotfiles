# .profile

if [ "$(uname -s)" = "Darwin" ]; then
    [ -d "$HOME/bin" ] && export PATH="$HOME/bin:$PATH"

    if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
        . "$HOME/.nix-profile/etc/profile.d/nix.sh"
        export NIX_PATH="$HOME/src/nixpkgs:nixpkgs=$HOME/src/nixpkgs"

        if [ -e "$HOME/.nix-profile/bin/git" \
                -a -e "/etc/ssl/certs/certificates.pem" ]; then
            export GIT_SSL_CAINFO="/etc/ssl/certs/certificates.pem"
        fi
    fi

    if [ -d "/Applications/Racket v6.1.1/bin" ]; then
        export PATH="/Applications/Racket v6.1.1/bin:$PATH"
    fi

    export LANG="en_US.UTF-8"

    export EDITOR="emacsclient -t --alternate-editor="

    [ -n "$BASH_VERSION" -a -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
fi
