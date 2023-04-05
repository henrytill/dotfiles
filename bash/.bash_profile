if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi

if [ -d "$HOME/.cargo/bin" ]; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

if [ -d "$HOME/.ghcup/bin" ]; then
    PATH="$HOME/.ghcup/bin:$PATH"
fi

if [ -d "$HOME/.cabal/bin" ]; then
    PATH="$HOME/.cabal/bin:$PATH"
fi

OPAM_VARIABLES_SH="$HOME/.opam/opam-init/variables.sh"
if [ -r "$OPAM_VARIABLES_SH" ]; then
    . "$OPAM_VARIABLES_SH" >/dev/null 2>/dev/null || true
fi

PLAN9="/usr/local/plan9"
if [ -d "$PLAN9" ]; then
    export PLAN9
    PATH="$PATH:$PLAN9/bin"
fi

if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

# $HOME/bin should always come first
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

if [ -n "$(command -v emacsclient)" ]; then
    export EDITOR="emacsclient -t"
    export ALTERNATE_EDITOR=""
elif [ -n "$(command -v mg)" ]; then
    export EDITOR="mg"
fi

# Allows Emacs installed by Guix to use system terminfo dirs
TERMINFO_DIRS="/usr/share/terminfo/"
if [ -d "$TERMINFO_DIRS" ]; then
    export TERMINFO_DIRS
fi

export LC_COLLATE=C
export NO_COLOR=1
export LIBVIRT_DEFAULT_URI="qemu:///system"
export _JAVA_AWT_WM_NONREPARENTING=1
export npm_config_prefix="$HOME/.local"

# Launch sway automatically
SWAY_SESSION="/usr/local/bin/sway-session"
if [ "$(uname)" = "Linux" ] && [ -e "$SWAY_SESSION" ] && [ "$(tty)" = "/dev/tty1" ]; then
    XDG_CURRENT_DESKTOP=sway
    export XDG_CURRENT_DESKTOP
    exec sway-session
fi
