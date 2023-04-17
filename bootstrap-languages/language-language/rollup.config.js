import svelte from "rollup-plugin-svelte";
import { nodeResolve } from "@rollup/plugin-node-resolve";
import commonjs from "@rollup/plugin-commonjs";
//import { terser } from 'rollup-plugin-terser';
import sveltePreprocess from "svelte-preprocess";
import postcss from "rollup-plugin-postcss";
import { string } from "rollup-plugin-string";
import builtins from 'rollup-plugin-node-builtins';
import urlImport from 'rollup-plugin-url-import'

const production = !process.env.ROLLUP_WATCH;

export default {
  input: "index.js",
  output: {
    sourcemap: false,
    format: "esm",
    target: "",
    name: "LanguageLanguage",
    file: "build/bundle.js",
  },
  plugins: [
    string({
      include: "build/*.js",
    }),
    svelte({
      // enable run-time checks when not in production
      dev: !production,
      // we'll extract any component CSS out into
      // a separate file - better for performance
      //css: css => {
      //	css.write('bundle.css');
      //},
      preprocess: sveltePreprocess(),
    }),

    // If you have external dependencies installed from
    // npm, you'll most likely need these plugins. In
    // some cases you'll need additional configuration -
    // consult the documentation for details:
    // https://github.com/rollup/plugins/tree/master/packages/commonjs
    commonjs({transformMixedEsModules: true}),
    nodeResolve({
      dedupe: ["svelte"],
    }),
    builtins(),
    urlImport(),
    postcss({
      extract: true,
      minimize: true,
      use: [
        [
          "sass",
          {
            includePaths: ["./src/ui/theme", "./node_modules"],
          },
        ],
      ],
    })
  ],
  watch: {
    clearScreen: false,
  },
};
