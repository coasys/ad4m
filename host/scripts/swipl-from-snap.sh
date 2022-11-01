#!/bin/bash
rm -rf ./temp/swipl

mkdir ./temp/swipl
mkdir ./temp/swipl/bin

SWIPL_BIN=`which swipl`
cp $SWIPL_BIN ./temp/swipl/bin

SWIPL_BIN_DIR=`dirname $SWIPL_BIN`
SWIPL_MAIN_DIR="$SWIPL_BIN_DIR/../lib/swi-prolog"

mkdir ./temp/swipl/lib
cp -r $SWIPL_MAIN_DIR ./temp/swipl/lib/swipl
echo "Copied swipl lib dir to temp"

rm -rf ./temp/swipl/lib/swipl/doc
rm -rf ./temp/swipl/lib/swipl/demo
rm -rf ./temp/swipl/lib/swipl/bin

chmod -R +w temp/swipl

echo "Copy node-swipl-stdio's top.pl to swipl home dir"
cp node_modules/swipl-stdio/top.pl temp/swipl/lib/swipl/

echo "Replacing all symlinks with their target"
cd ./temp/swipl
find -type l -exec sh -c 'PREV=$(realpath -- "$1") && rm -- "$1" && cp -ar -- "$PREV" "$1"' resolver {} \;

echo "Copying swipl done."

echo "Patching swipl executable to look for .so files in ../lib/"
cd bin
patchelf --set-rpath './../lib/swipl/lib/x86_64-linux' ./swipl