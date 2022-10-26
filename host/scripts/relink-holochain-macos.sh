#!/bin/bash

cd temp
mkdir lib
cd binary

echo "Relinking Holochain executables to relative shared object / dynamic libraries"

for EXECUTABLE in "holochain" "hc" "lair-keystore"
do
    echo "Processing $EXECUTABLE"
    otool -L $EXECUTABLE | grep '/nix' | while IFS=$'\n' read line; do
        clean_line=`echo $line | sed -r 's/\(.*//g'`
        echo $clean_line
        #IFS=' => '
        #read -ra parsed_line <<< "$clean_line"
        #filename=`echo ${parsed_line[0]} | tr -d '[:blank:]'`
        #targetpath=`echo ${parsed_line[1]}  | tr -d '[:blank:]'`
        targetpath=$clean_line
        filename=`basename $targetpath`
        echo "[$filename] Copying $targetpath to temp/swipl/lib..."
        cp $targetpath ../lib/$filename

        install_name_tool -change $targetpath @executable_path/../lib/$filename $EXECUTABLE
    done
done 
