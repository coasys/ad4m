#!/bin/bash
mkdir src/test-temp
cd src/test-temp
mkdir agents
mkdir languages
rm hc
ln -s `which hc` hc
rm holochain 
ln -s `which holochain` holochain
rm lair-keystore
ln -s `which lair-keystore` lair-keystore

