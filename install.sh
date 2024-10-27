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
    git submodule update --init
    exit 0
fi

if [ "$USER" != "ht" ]; then
    exit 0
fi

if [ -z "$(command -v make)" ]; then
    eprintf "${SCRIPT}: you must install make"
    exit 1
fi

if [ -z "$(command -v stow)" ]; then
    eprintf "${SCRIPT}: you must install stow"
    exit 1
fi

make -C $DIR
