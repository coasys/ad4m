#!/bin/bash

[ ! -d "./temp/binary" ] && mkdir -p "./temp/binary"

if [ ! -f "./temp/binary/hc" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.0.161-codesign-debug/hc-darwin-0.0.56
    mv hc-darwin-0.0.56 temp/binary/hc
    chmod +x temp/binary/hc
fi

if [ ! -f "./temp/binary/holochain" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.0.161-codesign-debug/holochain-darwin-0.0.161
    mv holochain-darwin-0.0.161 temp/binary/holochain
    chmod +x temp/binary/holochain
fi

if [ ! -f "./temp/binary/lair-keystore" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.0.161-codesign-debug/lair-keystore-darwin-0.2.0
    mv lair-keystore-darwin-0.2.0 temp/binary/lair-keystore
    chmod +x temp/binary/lair-keystore
fi

if [ ! -f "./temp/swipl/bin/swipl" ]; then
    wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/swipl-darwin-x86.zip
    unzip swipl-darwin-x86.zip -d temp
    rm -rf temp/__MACOSX
    rm -rf swipl-linux-x64.zip
fi