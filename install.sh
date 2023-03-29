#!/usr/bin/env bash

DIR="$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"

echo "DIR=${DIR}"

if [ -n "$IN_DEV_CONTAINER" ]; then
    echo "IN_DEV_CONTAINER=${IN_DEV_CONTAINER}"
    ln -frsv $DIR/git/.gitignore_global $DIR/..
    exit 0
fi

if [ "$USER" = "ht" ]; then
    make -C $DIR
fi
