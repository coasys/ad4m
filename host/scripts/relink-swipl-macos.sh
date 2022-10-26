#!/bin/bash

cd temp/swipl

echo "Relinking executable to relatve shared object / dynamic libraries"
DLL_PATH=`otool -L bin/swipl | grep libswipl | sed -r 's/\(.*//g' | tr -d '[:blank:]'`
echo $DLL_PATH
DLL_FILENAME=`otool -L bin/swipl | grep libswipl | sed -r 's/\(.*//g' | tr -d '[:blank:]' | sed -r 's/.*\///'`
echo $DLL_FILENAME
install_name_tool -change $DLL_PATH @executable_path/../lib/$DLL_FILENAME bin/swipl
echo "Relinking done."