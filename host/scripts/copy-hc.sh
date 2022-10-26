#!/bin/bash

[ ! -d "./temp/binary" ] && mkdir -p "./temp/binary"

[ -f "./temp/binary/holochain" ] && rm -f ./temp/binary/holochain
cp `which holochain` ./temp/binary/ && echo "Copied Holochain to temp/binary"

[ -f "./temp/binary/lair-keystore" ] && rm -f ./temp/binary/lair-keystore
cp `which lair-keystore` ./temp/binary/ && echo "Copied lair-keystore to temp/binary"

[ -f "./temp/binary/hc" ] && rm -f ./temp/binary/hc
cp `which hc` ./temp/binary/ && echo "Copied hc to temp/binary"
