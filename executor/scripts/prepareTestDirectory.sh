#!/bin/bash
[ -d "src/tst-tmp" ] && rm -rf src/tst-tmp
mkdir src/tst-tmp
cd src/tst-tmp
mkdir agents
mkdir languages
mkdir languages/test-language
cp -r ../tests/test-language/build languages/test-language/build

# Move binaries
rm -f hc
ln -sf `which hc` hc
rm -f holochain 
ln -sf `which holochain` holochain
rm -f lair-keystore
ln -sf `which lair-keystore` lair-keystore
rm -f swipl
ln -sf `which swipl` swipl
