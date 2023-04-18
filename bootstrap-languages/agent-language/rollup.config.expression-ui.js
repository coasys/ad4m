import svelte from "rollup-plugin-svelte";
import resolve from "@rollup/plugin-node-resolve";
import commonjs from "@rollup/plugin-commonjs";
//import { terser } from 'rollup-plugin-terser';
import sveltePreprocess from "svelte-preprocess";
import postcss from "rollup-plugin-postcss";
import { string } from "rollup-plugin-string";
import json from "@rollup/plugin-json";
import typescript from '@rollup/plugin-typescript';
import dna from "@perspect3vism/rollup-plugin-dna";

const production = !process.env.ROLLUP_WATCH;

export default {
  input: "expressionUI.ts",
  external: [],
  output: {
    sourcemap: true,
    format: "esm",
    name: "AgentExpressionUI",
    file: "build/expressionUI.js",
    interop: "esModule",
    globals: {},
  },
  external: [],
  plugins: [
    string({
      include: "build/*.js",
    }),
    typescript({include: "expressionUI.ts"}),
  ],
  watch: {
    clearScreen: false,
  },
};
