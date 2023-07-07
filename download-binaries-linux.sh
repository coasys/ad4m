#!/bin/bash
[ ! -d "./rust-executor/temp/binary" ] && mkdir -p "./rust-executor/temp/binary"

if [ ! -f "./rust-executor/temp/binary/hc" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/hc-linux-0.1.0
    mv hc-linux-0.1.0 ./rust-executor/temp/binary/hc
    chmod +x ./rust-executor/temp/binary/hc

    if [ ! -f "/usr/local/bin/hc" ]; then
        echo "Copying hc to /usr/local/bin"
        cp ./rust-executor/temp/binary/hc /usr/local/bin/hc
    fi
fi
