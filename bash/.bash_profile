# Restore Nix
#
# The graphical session sources nix-daemon.sh once and exports both its PATH
# and its __ETC_PROFILE_NIX_SOURCED guard to everything it launches.  A login
# shell started from inside that session (e.g. the Claude desktop app's
# terminal, which runs `bash -l`) re-runs /etc/profile, which resets PATH and
# drops nix.  /etc/profile.d/nix.sh then sources nix-daemon.sh again, but the
# inherited guard makes it return early, so nix never comes back.  Anything
# that needs nix then breaks, e.g. direnv's `use flake`.
#
# Only runs when nix is missing, so an ordinary tty or ssh login, where
# /etc/profile sets nix up itself, is unaffected.  Keep this above the
# sourcing of ~/.bashrc, whose `command -v` checks need nix on PATH.

nix_daemon_sh=/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

if test -z "$(command -v nix)" && test -r "${nix_daemon_sh}"
then
    xdg_data_dirs="${XDG_DATA_DIRS-}"

    unset __ETC_PROFILE_NIX_SOURCED
    . "${nix_daemon_sh}"

    # nix-daemon.sh appends its share dirs unconditionally; keep the
    # inherited value if it already has them.
    case ":${xdg_data_dirs}:" in
        *:/nix/var/nix/profiles/default/share:*)
            XDG_DATA_DIRS="${xdg_data_dirs}"
            ;;
    esac

    unset xdg_data_dirs
fi

unset nix_daemon_sh

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
