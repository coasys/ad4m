#!/bin/bash

[ ! -d "./temp/binary" ] && mkdir -p "./temp/binary"

if [ ! -f "./temp/binary/hc" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps/hc-darwin
    mv hc-darwin ./temp/binary/hc
    chmod +x ./temp/binary/hc
fi

if [ ! -f "./temp/binary/holochain" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps/holochain-darwin
    mv holochain-darwin ./temp/binary/holochain
    chmod +x ./temp/binary/holochain
fi

if [ ! -f "./temp/swipl/bin/swipl" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps/swipl-darwin-x86.zip
    unzip swipl-darwin-x86.zip -d ./temp
    rm -rf ./temp/__MACOSX
    rm -rf swipl-linux-x64.zip
fi