import alias from '@rollup/plugin-alias';
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
    alias({
      entries: [
        { find: 'type-graphql', replacement: './lib/shims/type-graphql.js' },
        { find: 'reflect-metadata', replacement: './lib/shims/reflect-metadata.js' }
      ]
    }),
    nodeResolve(),
    commonjs()
  ]
};