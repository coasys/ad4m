#!/bin/bash

cd temp/swipl

echo "Relinking executable to relative shared object / dynamic libraries"

ldd bin/swipl | grep '=> /nix' | while IFS=$'\n' read line; do
    clean_line=`echo $line | sed -r 's/\(.*//g'`
    #echo $clean_line
    IFS=' => '
    read -ra parsed_line <<< "$clean_line"
    filename=`echo ${parsed_line[0]} | tr -d '[:blank:]'`
    targetpath=`echo ${parsed_line[1]}  | tr -d '[:blank:]'`
    echo "[$filename] Copying $targetpath to temp/swipl/lib..."
    cp -f $targetpath ./lib/$filename
done

echo "Patching swipl executable to look for .so files in ../lib/"
cd bin
patchelf --set-rpath './../lib' ./swipl

