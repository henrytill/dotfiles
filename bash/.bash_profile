if test -f "${HOME}/.bashrc"
then
    . "${HOME}/.bashrc"
fi

paths=(
    "${HOME}/bin"
    "${HOME}/.local/bin"
    "$PATH"
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

if test -n "$(command -v editor)"
then
    EDITOR="editor"
    export EDITOR

    ALTERNATE_EDITOR=""
    export ALTERNATE_EDITOR
fi

export FZF_DEFAULT_OPTS_FILE="${HOME}/.config/fzf/fzfrc"

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
