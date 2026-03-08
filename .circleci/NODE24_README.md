# Node 24 Build Compatibility

This branch updates the CI to use Node 24.

## Changes

1. Updated Docker image to Node 24
2. Added fix-imports.mjs to handle ESM imports without --es-module-specifier-resolution flag
3. Updated CircleCI PATH to use Node 24

## Build Process

1. TypeScript compilation
2. Run fix-imports.mjs to add .js extensions to compiled imports
3. Run buildSchema.js with Node 24
4. Bundle with Rollup

