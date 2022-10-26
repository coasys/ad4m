#!/bin/bash

set -e

rm -rf src-tauri/bins
mkdir src-tauri/bins

TARGET_TRIPLE=$(rustc -vV | sed -n 's/^.*host: \(.*\)*$/\1/p')

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     AD4M_HOST_BINARY=https://github.com/perspect3vism/ad4m-host/releases/download/v0.0.32-fix6/ad4m-linux-x64-node-16.x;;
    Darwin*)    AD4M_HOST_BINARY=https://github.com/perspect3vism/ad4m-host/releases/download/v0.0.32-fix6/ad4m-macos-x64-node-16.x;;
    *)          echo "Machine is not supported: ${unameOut}" && exit 1;;
esac
echo ${machine}

wget -O src-tauri/bins/ad4m-$TARGET_TRIPLE $AD4M_HOST_BINARY

chmod 755 src-tauri/bins/ad4m-$TARGET_TRIPLE
