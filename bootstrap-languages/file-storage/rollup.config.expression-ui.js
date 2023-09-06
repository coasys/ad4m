import { string } from "rollup-plugin-string";
import typescript from '@rollup/plugin-typescript';

export default {
  input: "expressionUI.ts",
  external: [],
  output: {
    sourcemap: true,
    format: "esm",
    name: "FileStorageExpressionUI",
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
