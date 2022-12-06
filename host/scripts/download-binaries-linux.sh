#!/bin/bash
[ ! -d "./temp/binary" ] && mkdir -p "./temp/binary"

if [ ! -f "./temp/binary/hc" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.0.174/hc-linux-0.0.69
    mv hc-linux-0.0.69 temp/binary/hc
    chmod +x temp/binary/hc
fi

if [ ! -f "./temp/binary/holochain" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.0.174/holochain-linux-0.0.174
    mv holochain-linux-0.0.174 temp/binary/holochain
    chmod +x temp/binary/holochain
fi

if [ ! -f "./temp/swipl/bin/swipl" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.0.174/swipl-linux-x86.zip
    unzip swipl-linux-x86.zip -d temp
    rm -rf swipl-linux-x64.zip
fi