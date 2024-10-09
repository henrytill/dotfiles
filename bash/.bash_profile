if [ -f "$HOME/.bashrc" ]; then
  . "$HOME/.bashrc"
fi

OPAM_VARIABLES_SH="$HOME/.opam/opam-init/variables.sh"
if [ -r "$OPAM_VARIABLES_SH" ]; then
  . "$OPAM_VARIABLES_SH" >/dev/null 2>/dev/null || true
fi

export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"

PLAN9="/usr/local/plan9"
if [ -d "$PLAN9" ]; then
  export PLAN9
fi

paths=(
  "$HOME/bin"
  "$HOME/.local/bin"
  "$HOME/.local/opt/mlton/bin"
  "$HOME/.local/opt/polyml/bin"
  "$HOME/.local/opt/racket-8.14/bin"
  "$HOME/.local/opt/flutter/bin"
  "$HOME/.cargo/bin"
  "$HOME/.cabal/bin"
  "$HOME/.ghcup/bin"
  "$HOME/.elan/bin"
  "$PATH"
  "/usr/local/go/bin"
  "$PLAN9/bin"
)

# Filter out non-existent directories
existing_paths=()
for path in "${paths[@]}"; do
  if [ -d "$path" ] || [ "$path" = "$PATH" ]; then
    existing_paths+=("$path")
  fi
done

# Set the new PATH
PATH=$(IFS=':'; printf '%s' "${existing_paths[*]}")

if [ -n "$(command -v emacsclient)" ]; then
  export EDITOR="emacsclient -t"
  export ALTERNATE_EDITOR=""
elif [ -n "$(command -v mg)" ]; then
  export EDITOR="mg"
fi

export GOPROXY=direct
export LIBVIRT_DEFAULT_URI="qemu:///system"
export CARGO_TERM_PROGRESS_WHEN=never
export NO_COLOR=1
export OPAMCOLOR=never
export _JAVA_AWT_WM_NONREPARENTING=1
export npm_config_prefix="$HOME/.local"
export CHROME_EXECUTABLE=/usr/bin/chromium

# We are using Wayland
export MOZ_ENABLE_WAYLAND=1

export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
