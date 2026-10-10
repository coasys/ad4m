import nodeResolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';

module.exports = {
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
  plugins: [
    nodeResolve(),
    commonjs()
  ]
};