#!/bin/bash

[ ! -d "./temp/binary" ] && mkdir -p "./temp/binary"

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/hc-darwin-0.0.56
mv hc-darwin-0.0.56 temp/binary/hc
chmod +x temp/binary/hc

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/holochain-darwin-0.0.161
mv holochain-darwin-0.0.161 temp/binary/holochain
chmod +x temp/binary/holochain

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/lair-keystore-darwin-0.2.0
mv lair-keystore-darwin-0.2.0 temp/binary/lair-keystore
chmod +x temp/binary/lair-keystore

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/swipl-darwin-x86.zip
unzip swipl-darwin-x86.zip -d temp
mv temp/swipl-bundle-x86 temp/swipl
rm -rf temp/__MACOSX
