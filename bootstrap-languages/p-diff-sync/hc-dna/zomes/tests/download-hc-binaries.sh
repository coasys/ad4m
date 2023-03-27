#!/bin/bash
[ ! -d "./temp/binary" ] && mkdir -p "./temp/binary"

if [ ! -f "./hc" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/hc-linux-0.1.0
    mv hc-linux-0.1.0 ./hc
    chmod +x ./hc
fi

if [ ! -f "./holochain" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/holochain-linux-0.1.0
    mv holochain-linux-0.1.0 ./holochain
    chmod +x ./holochain
fi