import resolve from "@rollup/plugin-node-resolve";
import commonjs from "@rollup/plugin-commonjs";
import { string } from "rollup-plugin-string";
import dna from "@perspect3vism/rollup-plugin-dna";

const production = !process.env.ROLLUP_WATCH;

export default {
  input: "index.js",
  output: {
    sourcemap: true,
    format: "cjs",
    name: "DirectMessageLanguage",
    file: "build/bundle.js",
  },
  plugins: [
    string({
      include: "build/*.js",
    }),
    // If you have external dependencies installed from
    // npm, you'll most likely need these plugins. In
    // some cases you'll need additional configuration -
    // consult the documentation for details:
    // https://github.com/rollup/plugins/tree/master/packages/commonjs
    resolve({
      browser: true,
      dedupe: ["svelte"],
    }),
    commonjs(),
    dna(),
  ],
  watch: {
    clearScreen: false,
  },
};
