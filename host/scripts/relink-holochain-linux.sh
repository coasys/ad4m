#!/bin/bash

cd temp
mkdir lib
cd binary

echo "Relinking Holochain executables to relative shared object / dynamic libraries"

for EXECUTABLE in "holochain" "hc" "lair-keystore"
do
    chmod +w $EXECUTABLE
    ldd $EXECUTABLE | grep '=> /nix' | while IFS=$'\n' read line; do
        clean_line=`echo $line | sed -r 's/\(.*//g'`
        echo $clean_line
        IFS=' => '
        read -ra parsed_line <<< "$clean_line"
        filename=`echo ${parsed_line[0]} | tr -d '[:blank:]'`
        targetpath=`echo ${parsed_line[1]}  | tr -d '[:blank:]'`
        echo "[$filename] Copying $targetpath to temp/lib..."
        printf '\n\n\n'
        cp -f $targetpath ../lib/$filename
    done

    echo "Patching $EXECUTABLE to look for .so files in ../lib/"
    patchelf --set-rpath './../lib' ./$EXECUTABLE
done 




