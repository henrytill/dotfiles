if [ -n "$KSH_VERSION" -a -z "$ENV" ]
then
    export ENV="$HOME/.kshrc"
fi

if [ -n "$BASH_VERSION" -a -f "$HOME/.bashrc" ]
then
    . "$HOME/.bashrc"
fi

if [ -d "$HOME/bin" ]
then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ]
then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.cargo/bin" ]
then
    PATH="$HOME/.cargo/bin:$PATH"
fi

if [ -d "$HOME/.cabal/bin" ]
then
    PATH="$HOME/.cabal/bin:$PATH"
fi

if [ -n "$(command -v opam)" -a -d "$HOME/.opam" ]
then
    . $HOME/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
fi

PLAN9=/usr/local/plan9
if [ -d "$PLAN9" ]
then
    export PLAN9
    PATH="$PATH:$PLAN9/bin"
fi

if [ -d "/usr/local/go" ]
then
    PATH="$PATH:/usr/local/go/bin"
fi
