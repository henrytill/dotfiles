add_to_path_front () {
    if [ -d "$1" ]; then
        PATH="$1:$PATH"
    fi
}

if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi

OPAM_VARIABLES_SH="$HOME/.opam/opam-init/variables.sh"
if [ -r "$OPAM_VARIABLES_SH" ]; then
    . "$OPAM_VARIABLES_SH" >/dev/null 2>/dev/null || true
fi

add_to_path_front "$HOME/.cargo/bin"
add_to_path_front "$HOME/.ghcup/bin"
add_to_path_front "$HOME/.cabal/bin"

PLAN9="/usr/local/plan9"
if [ -d "$PLAN9" ]; then
    export PLAN9
    PATH="$PATH:$PLAN9/bin"
fi

add_to_path_front "$HOME/.local/bin"
add_to_path_front "$HOME/bin"

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
export GOPROXY=direct

# Launch sway automatically
SWAY_SESSION="/usr/local/bin/sway-session"
if [ "$(uname)" = "Linux" ] && [ -e "$SWAY_SESSION" ] && [ "$(tty)" = "/dev/tty1" ]; then
    XDG_CURRENT_DESKTOP=sway
    export XDG_CURRENT_DESKTOP
    exec sway-session
fi
