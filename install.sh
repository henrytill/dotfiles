#!/usr/bin/env bash

set -e

eprintf() { printf "$@" >&2; }

SCRIPT="$(basename -- ${BASH_SOURCE[0]})"

DIR="$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"

echo "DIR=${DIR}"

if [ -n "$IN_DEV_CONTAINER" ]; then
    echo "IN_DEV_CONTAINER=${IN_DEV_CONTAINER}"
    ln -frsv $DIR/git/.gitignore_global $DIR/..
    printf '\nexport TZ="America/Los_Angeles"\n' >> $HOME/.profile
    printf '\nexport npm_config_prefix="$HOME/.local"\n'>> $HOME/.profile
    git submodule update --init
    exit 0
fi

if [ "$USER" != "ht" ]; then
    exit 0
fi

if [ -z "$(command -v make)" ]; then
    eprintf "%s: you must install make" "${SCRIPT}"
    exit 1
fi

if [ -z "$(command -v stow)" ]; then
    eprintf "%s: you must install stow" "${SCRIPT}"
    exit 1
fi

make -C $DIR
