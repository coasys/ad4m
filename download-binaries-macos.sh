#!/bin/bash

[ ! -d "./executor/temp/binary" ] && mkdir -p "./executor/temp/binary"

if [ ! -f "./executor/temp/binary/hc" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/hc-darwin-0.1.0
    mv hc-darwin-0.1.0 ./executor/temp/binary/hc
    chmod +x ./executor/temp/binary/hc

    if [ ! -f "/bin/hc" ]; then
        sudo ln -s ./executor/temp/binary/hc /bin/hc
    fi
fi

if [ ! -f "./executor/temp/binary/holochain" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/holochain-darwin-0.1.0
    mv holochain-darwin-0.1.0 ./executor/temp/binary/holochain
    chmod +x ./executor/temp/binary/holochain

    if [ ! -f "/bin/holochain" ]; then
        sudo ln -s ./executor/temp/binary/holochain /bin/holochain
    fi
fi

if [ ! -f "./executor/temp/swipl/bin/swipl" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/swipl-darwin-x86.zip
    unzip swipl-darwin-x86.zip -d ./temp
    rm -rf ./executor/temp/__MACOSX
    rm -rf swipl-linux-x64.zip

    if [ ! -f "/bin/swipl" ]; then
        sudo ln -s ./executor/temp/swipl/bin/swipl /bin/swipl
    fi
fi