import nodeResolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';

const plugins = [
  nodeResolve(),
  commonjs()
];

module.exports = [
  {
    input: 'lib/src/index.js',
    output: [
      {
        format: 'cjs',
        file: "lib/index.cjs",
      },
      {
        format: 'esm',
        file: "lib/index.js",
      }
    ],
    plugins
  },
  {
    // `@coasys/ad4m/model`: decorators + SHACL generation without the
    // client runtime, for language bundles (`mount.classes`).
    input: 'lib/src/model.js',
    output: [
      {
        format: 'cjs',
        file: "lib/model.cjs",
      },
      {
        format: 'esm',
        file: "lib/model.js",
      }
    ],
    plugins
  }
];
