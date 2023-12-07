#!/bin/bash

cd languages/test-language
yarn install
yarn run build

cd ../note-store
yarn install
yarn run build
