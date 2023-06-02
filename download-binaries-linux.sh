#!/bin/bash
[ ! -d "./executor/temp/binary" ] && mkdir -p "./executor/temp/binary"

if [ ! -f "./executor/temp/binary/hc" ]; then
    wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/hc-linux-0.1.0
    mv hc-linux-0.1.0 ./executor/temp/binary/hc
    chmod +x ./executor/temp/binary/hc

    if [ ! -f "/usr/local/bin/hc" ]; then
        echo "Copying hc to /usr/local/bin"
        cp ./executor/temp/binary/hc /usr/local/bin/hc
    fi
fi
