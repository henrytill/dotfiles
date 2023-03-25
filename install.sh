#!/usr/bin/env bash

DIR="$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"

echo "dotfiles directory: ${DIR}"

make -C $DIR
