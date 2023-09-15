add_to_path_front () {
    if [ -d "$1" ]; then
        PATH="$1:$PATH"
    fi
}

add_to_path_back () {
    if [ -d "$1" ]; then
        PATH="$PATH:$1"
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
add_to_path_front "$HOME/.elan/bin"

export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"
add_to_path_back "/usr/local/go/bin"

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

export GOPROXY=direct
export LANG="en_US.utf8"
export LC_COLLATE="C.utf8"
export LIBVIRT_DEFAULT_URI="qemu:///system"
export NO_COLOR=1
export _JAVA_AWT_WM_NONREPARENTING=1
export npm_config_prefix="$HOME/.local"

# We are using Wayland
export MOZ_ENABLE_WAYLAND=1

# Launch sway
SWAY_SESSION="/usr/local/bin/sway-session"
if [ "$(uname)" = "Linux" ] && [ -e "$SWAY_SESSION" ] && [ "$(tty)" = "/dev/tty1" ]; then
    XDG_CURRENT_DESKTOP=sway
    export XDG_CURRENT_DESKTOP
    exec sway-session
fi
