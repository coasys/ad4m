#/bin/bash

sed -i "s/import buffer from 'node:buffer';/const buffer = require('buffer');/" ../core/node_modules/safe-buffer/index.js

sed -i "s/import buffer from 'node:buffer'/const buffer = require('buffer');/" ../core/node_modules/safer-buffer/safer.js
