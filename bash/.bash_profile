# Set environment variables

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
    [LOCALE_ARCHIVE]=/usr/lib/locale/locale-archive
    [npm_config_prefix]="$HOME/.local/opt/npm"
)

for var in "${!env_vars[@]}"; do
    declare "${var}=${env_vars[$var]}"
    export "${var?}"
done

# Source ~/.bashrc

if test -f "${HOME}/.bashrc"
then
    . "${HOME}/.bashrc"
fi

# Update PATH

paths=(
    "${HOME}/bin"
    "${HOME}/.local/bin"
    "$PATH"
)

# Filter out non-existent directories
existing_paths=()
for path in "${paths[@]}"
do
    if test -d "${path}" || test "${path}" = "${PATH}"
    then
        existing_paths+=("${path}")
    fi
done

# Set the new PATH
PATH=$(IFS=':'; printf '%s' "${existing_paths[*]}")
