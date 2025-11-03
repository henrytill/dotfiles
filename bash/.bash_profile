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

declare -A env_vars=(
    [FZF_DEFAULT_OPTS_FILE]="${HOME}/.config/fzf/fzfrc"
    [LIBVIRT_DEFAULT_URI]="qemu:///system"
    [_JAVA_AWT_WM_NONREPARENTING]=1
    [npm_config_prefix]="$HOME/.local/opt/npm"
    [CHROME_EXECUTABLE]=/usr/bin/chromium
    [LOCALE_ARCHIVE]=/usr/lib/locale/locale-archive
)

for var in "${!env_vars[@]}"; do
    declare "${var}=${env_vars[$var]}"
    export "${var}"
done

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
