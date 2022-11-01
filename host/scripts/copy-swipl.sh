#!/bin/bash
rm -rf ./temp/swipl

SWIPL_BIN=`which swipl`
SWIPL_BIN_DIR=`dirname $SWIPL_BIN`
SWIPL_MAIN_DIR=`dirname $SWIPL_BIN_DIR`

cp -r $SWIPL_MAIN_DIR ./temp/swipl/
echo "Copied swipl lib dir to temp"

chmod -R +w temp/swipl

echo "Copy node-swipl-stdio's top.pl to swipl home dir"
cp node_modules/swipl-stdio/top.pl temp/swipl/lib/swipl/

echo "Replacing all symlinks with their target"
cd ./temp/swipl
find -type l -exec sh -c 'PREV=$(realpath -- "$1") && rm -- "$1" && cp -ar -- "$PREV" "$1"' resolver {} \;

echo "Copying swipl done."

