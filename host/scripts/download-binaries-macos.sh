#!/bin/bash

[ ! -d "./temp/binary" ] && mkdir -p "./temp/binary"

if [ ! -f "./temp/swipl/bin/swipl" ]; then
    wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/swipl-darwin-x86.zip
    unzip swipl-darwin-x86.zip -d temp
    rm -rf temp/__MACOSX
    rm -rf swipl-linux-x64.zip
fi