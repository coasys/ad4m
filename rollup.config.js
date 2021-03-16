import typescript from 'rollup-plugin-typescript2'
import pkg from './package.json'
import { nodeResolve } from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';

export default {
    input: 'src/main.ts',
    output: [
        {
            file: pkg.main,
            format: 'cjs',
        },
        {
            file: pkg.module,
            format: 'es',
        },
    ],
    external: [
      ...Object.keys(pkg.dependencies || {}),
      ...Object.keys(pkg.peerDependencies || {}),
    ],
    plugins: [
        json(),
        nodeResolve({
            modulesOnly: true,
        }),
        commonjs(),
        typescript({
          typescript: require('typescript'),
        }),
    ],
}