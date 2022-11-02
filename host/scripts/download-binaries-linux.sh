#!/bin/bash
[ ! -d "./temp/binary" ] && mkdir -p "./temp/binary"

if [ ! -f "./temp/binary/hc" ]; then
    wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/hc-linux-0.0.56
    mv hc-linux-0.0.56 temp/binary/hc
    chmod +x temp/binary/hc
fi

if [ ! -f "./temp/binary/holochain" ]; then
    wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/holochain-linux-0.0.161
    mv holochain-linux-0.0.161 temp/binary/holochain
    chmod +x temp/binary/holochain
fi

if [ ! -f "./temp/binary/lair-keystore" ]; then
    wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/lair-keystore-linux-0.2.0
    mv lair-keystore-linux-0.2.0 temp/binary/lair-keystore
    chmod +x temp/binary/lair-keystore
fi

if [ ! -f "./temp/swipl/bin/swipl" ]; then
    wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/swipl-linux-x86.zip
    unzip swipl-linux-x86.zip -d temp
    rm -rf swipl-linux-x64.zip
fi