if test -f "${HOME}/.bashrc"
then
    . "${HOME}/.bashrc"
fi

OPAM_VARIABLES_SH="${HOME}/.opam/opam-init/variables.sh"
if test -r "$OPAM_VARIABLES_SH"
then
    . "$OPAM_VARIABLES_SH" >/dev/null 2>/dev/null || true
fi

export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"

PLAN9="/usr/local/plan9"
if test -d "$PLAN9"
then
    export PLAN9
fi

paths=(
    "${HOME}/bin"
    "${HOME}/.local/bin"
    "${HOME}/.local/opt/mlton/bin"
    "${HOME}/.local/opt/racket-8.17/bin"
    "${HOME}/.local/opt/z3/bin"
    "${HOME}/.local/opt/zig-x86_64-linux-0.15.1"
    "${HOME}/.cargo/bin"
    "${HOME}/.ghcup/bin"
    "${HOME}/.elan/bin"
    "$PATH"
    "/usr/local/go/bin"
    "${PLAN9}/bin"
)

# Filter out non-existent directories
existing_paths=()
for path in "${paths[@]}"
do
    if test -d "$path" || test "$path" = "$PATH"
    then
        existing_paths+=("$path")
    fi
done

# Set the new PATH
PATH=$(IFS=':'; printf '%s' "${existing_paths[*]}")

if test -n "$(command -v emacsclient)"
then
    EDITOR="emacsclient"
    export EDITOR

    ALTERNATE_EDITOR=""
    export ALTERNATE_EDITOR
elif test  -n "$(command -v mg)"
then
    EDITOR="mg"
    export EDITOR
fi

export GOPROXY=direct
export LIBVIRT_DEFAULT_URI="qemu:///system"
export _JAVA_AWT_WM_NONREPARENTING=1
export npm_config_prefix="$HOME/.local/opt/npm"
export CHROME_EXECUTABLE=/usr/bin/chromium

export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

if test "$(uname)" = "Linux" && test "$(tty)" = "/dev/tty1"
then
    export MOZ_ENABLE_WAYLAND=1
    # https://github.com/YaLTeR/niri/issues/1914
    exec niri-session -l
fi

if test "$(uname)" = "Linux" && test "$(tty)" = "/dev/tty2"
then
    exec startx
fi
