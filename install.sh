#!/usr/bin/env bash

set -e

eprintf() { printf "%s" "$@" >&2; }

SCRIPT="$(basename -- "${BASH_SOURCE[0]}")"

DIR="$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"

echo "DIR=${DIR}"


if test -n "$IN_DEV_CONTAINER"
then
	echo "IN_DEV_CONTAINER=${IN_DEV_CONTAINER}"
	ln -frsv "${DIR}/git/.gitignore_global" "${DIR}/.."
	printf '\nTZ="America/Los_Angeles"\nexport TZ\n' >> "${HOME}/.profile"
	git submodule update --init

	if test -n "$(command -v stow)"
	then
		stow -v emacs
		stow -v tmux
	fi

	if test "${SHELL}" = "/bin/bash" && test -f "${HOME}/.bashrc"
	then
		{ printf '\n'; cat bash/.bash_functions; } >> "${HOME}/.bashrc"
	fi

	if test "$(command -v mg)"
	then
		printf '\nEDITOR=mg\nexport EDITOR\n' >> "${HOME}/.profile"
	fi

	exit 0
fi

if test "$USER" != "ht"
then
	exit 0
fi

if test -z "$(command -v make)"
then
	eprintf "%s: you must install make" "${SCRIPT}"
	exit 1
fi

if test -z "$(command -v stow)"
then
	eprintf "%s: you must install stow" "${SCRIPT}"
	exit 1
fi

make -C "${DIR}"

# Local Variables:
# indent-tabs-mode: t
# sh-basic-offset: 8
# End:
