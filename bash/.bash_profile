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
	export EDITOR="emacsclient -t"
	export ALTERNATE_EDITOR=""
elif test  -n "$(command -v mg)"
then
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

export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

# Launch sway
SWAY_SESSION="/usr/local/bin/sway-session"
if test "$(uname)" = "Linux"  && test -e "$SWAY_SESSION"  && test "$(tty)" = "/dev/tty1"
then
	# We are using Wayland
	export MOZ_ENABLE_WAYLAND=1
	XDG_CURRENT_DESKTOP=sway
	export XDG_CURRENT_DESKTOP
	exec sway-session
fi

if test "$(uname)" = "Linux" && test "$(tty)" = "/dev/tty2"
then
	exec startx
fi

# Local Variables:
# sh-basic-offset: 8
# indent-tabs-mode: t
# End:
