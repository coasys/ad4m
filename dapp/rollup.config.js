const nodeResolve = require('@rollup/plugin-node-resolve');
const commonjs = require('@rollup/plugin-commonjs');
const svelte = require('rollup-plugin-svelte');

module.exports = {
  input: 'main.js',
  output: [
    {
      format: 'cjs',
      file: "./public/bundle.cjs",
    },
    {
      format: 'esm',
      file: "./public/bundle.js",
    }
  ],
  plugins: [
    svelte({
      emitCss: false,
    }),
    nodeResolve({
      browser: true,
      exportConditions: ['svelte'],
      extensions: ['.svelte']
    }),
    commonjs()
  ]
};