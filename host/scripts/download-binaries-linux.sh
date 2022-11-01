#!/bin/bash
[ ! -d "./temp/binary" ] && mkdir -p "./temp/binary"

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/hc-linux-0.0.56
mv hc-linux-0.0.56 temp/binary/hc
chmod +x temp/binary/hc

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/holochain-linux-0.0.161
mv holochain-linux-0.0.161 temp/binary/holochain
chmod +x temp/binary/holochain

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/lair-keystore-linux-0.2.0
mv lair-keystore-linux-0.2.0 temp/binary/lair-keystore
chmod +x temp/binary/lair-keystore

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/swipl-linux-x86.zip
unzip swipl-linux-x86.zip -d temp