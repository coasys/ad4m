import alias from '@rollup/plugin-alias';
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';

module.exports = {
  input: 'lib/src/index.js',
  output: {
    dir: 'lib',
    format: 'cjs'
  },
  plugins: [
    alias({
      entries: [
        { find: 'type-graphql', replacement: './lib/shims/type-graphql.js' },
        { find: 'reflect-metadata', replacement: './lib/shims/reflect-metadata.js' }
      ]
    }),
    resolve(),
    commonjs()
  ]
};