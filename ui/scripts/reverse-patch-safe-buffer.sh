#/bin/bash

sed -i "s/const buffer = require('buffer');/import buffer from 'node:buffer';/" ../node_modules/safe-buffer/index.js

sed -i "s/const buffer = require('buffer');/import buffer from 'node:buffer';/" ../node_modules/safer-buffer/safer.js