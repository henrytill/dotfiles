#!/usr/bin/env bash

DIR="$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"

echo "dotfiles: ${DIR}"

if [ -n "$IN_DEVCONTAINER" ]; then
    echo "In Dev Container, exiting..."
    exit 0
fi

make -C $DIR
