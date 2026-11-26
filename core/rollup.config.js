import nodeResolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';

const plugins = [
  nodeResolve(),
  commonjs()
];

// One build for both entries: `index` and `@coasys/ad4m/model` share the
// modules they re-export (the decorator registries in model/decorators.ts
// among them). Two separate builds would inline a copy per entry, and a
// class decorated through `@coasys/ad4m/model` would be invisible to
// getPropertiesMetadata()/Ad4mModel from `@coasys/ad4m`. With a shared
// chunk per format, both entries import the same runtime module — one
// registry, whichever entry each side imported.
module.exports = [
  {
    input: {
      index: 'lib/src/index.js',
      model: 'lib/src/model-entry.js'
    },
    output: [
      {
        format: 'esm',
        dir: "lib",
        entryFileNames: "[name].js",
        chunkFileNames: "shared-[hash].js"
      },
      {
        format: 'cjs',
        dir: "lib",
        entryFileNames: "[name].cjs",
        chunkFileNames: "shared-[hash].cjs"
      }
    ],
    plugins
  }
];
