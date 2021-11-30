#!/bin/bash
[ -d "src/test-temp" ] && rm -rf src/test-temp
mkdir src/test-temp
cd src/test-temp
mkdir agents
mkdir languages
rm -f hc
ln -sf `which hc` hc
rm -f holochain 
ln -sf `which holochain` holochain
rm -f lair-keystore
ln -sf `which lair-keystore` lair-keystore
rm -f swipl
ln -sf `which swipl` swipl
