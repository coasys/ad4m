#!/bin/bash

cd languages/test-language
pnpm i
pnpm run build

cd ../note-store
pnpm i
pnpm run build

cd ../note-store-flat
pnpm i
pnpm run build

cd ../aes-flat
pnpm i
pnpm run build
