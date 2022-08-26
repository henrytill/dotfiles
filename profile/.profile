if [ -n "$KSH_VERSION" -a -z "$ENV" ]
then
    export ENV="$HOME/.kshrc"
fi

if [ -n "$BASH_VERSION" -a -f "$HOME/.bashrc" ]
then
    . "$HOME/.bashrc"
fi

if [ -d "$HOME/.cargo/bin" ]
then
    PATH="$HOME/.cargo/bin:$PATH"
fi

if [ -d "$HOME/.cabal/bin" ]
then
    PATH="$HOME/.cabal/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ]
then
    PATH="$HOME/.local/bin:$PATH"
fi

PLAN9="/usr/local/plan9"
if [ -d "$PLAN9" ]
then
    export PLAN9
    PATH="$PATH:$PLAN9/bin"
fi

SMLNJ="/usr/local/smlnj-110.99.3"
if [ -d "$SMLNJ" ]
then
    PATH="$SMLNJ/bin:$PATH"
fi

OPAMSH="$HOME/.opam/opam-init/init.sh"
if [ -e "$OPAMSH" ]
then
    . "$OPAMSH" > /dev/null 2> /dev/null || true
fi

NIXSH="$HOME/.nix-profile/etc/profile.d/nix.sh"
if [ -e "$NIXSH" ]
then
    . "$NIXSH"
fi

# $HOME/bin should always come first
if [ -d "$HOME/bin" ]
then
    PATH="$HOME/bin:$PATH"
fi
