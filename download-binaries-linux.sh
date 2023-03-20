#!/bin/bash
[ ! -d "./executor/temp/binary" ] && mkdir -p "./executor/temp/binary"

if [ ! -f "./executor/temp/binary/hc" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/hc-linux-0.1.0
    mv hc-linux-0.1.0 ./executor/temp/binary/hc
    chmod +x ./executor/temp/binary/hc

    if [ ! -f "/usr/bin/hc" ]; then
        cp ./executor/temp/binary/hc /usr/bin/hc
    fi
fi

if [ ! -f "./executor/temp/binary/holochain" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/holochain-linux-0.1.0
    mv holochain-linux-0.1.0 ./executor/temp/binary/holochain
    chmod +x ./executor/temp/binary/holochain

    if [ ! -f "/usr/bin/holochain" ]; then
        cp ./executor/temp/binary/holochain /usr/bin/holochain
    fi
fi

if [ ! -f "./executor/temp/swipl/bin/swipl" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/swipl-linux-x86.zip
    unzip swipl-linux-x86.zip -d ./executor/temp
    rm -rf swipl-linux-x64.zip

    if [ ! -f "/usr/bin/swipl" ]; then
        cp ./executor/temp/swipl/bin/swipl /usr/bin/swipl
    fi
fi