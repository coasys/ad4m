#/bin/bash

sed "s/import buffer from 'node:buffer';/const buffer = require('buffer');/" ../node_modules/safe-buffer/index.js

sed "s/import buffer from 'node:buffer'/const buffer = require('buffer');/" ../node_modules/safer-buffer/safer.js
