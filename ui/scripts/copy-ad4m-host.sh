#!/bin/bash

set -e

rm -rf src-tauri/bins
mkdir src-tauri/bins

TARGET_TRIPLE=$(rustc -vV | sed -n 's/^.*host: \(.*\)*$/\1/p')

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     AD4M_HOST_BINARY=../host/dist/ad4m-linux-x64;;
    Darwin*)    AD4M_HOST_BINARY=../host/dist/ad4m-macos-x64;;
    *)          echo "Machine is not supported: ${unameOut}" && exit 1;;
esac
echo ${machine}

cp $AD4M_HOST_BINARY src-tauri/bins/ad4m-$TARGET_TRIPLE

chmod 755 src-tauri/bins/ad4m-$TARGET_TRIPLE
