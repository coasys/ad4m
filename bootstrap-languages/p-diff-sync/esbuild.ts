import * as esbuild from "https://deno.land/x/esbuild@v0.17.18/mod.js";
// Import the WASM build on platforms where running subprocesses is not
// permitted, such as Deno Deploy, or when running without `--allow-run`.
// import * as esbuild from "https://deno.land/x/esbuild@v0.17.18/wasm.js";

import { denoPlugins } from "https://deno.land/x/esbuild_deno_loader@0.7.0/mod.ts";
import { loadSource, resolveUrl } from "./customHttpDownloader.js";

const result = await esbuild.build({
  plugins: [
    {
        name: `buffer-alias`,
        setup(build) {
            build.onResolve({ filter: new RegExp(`^buffer$`) }, (args) => {
                return { path: `https://deno.land/std@0.177.0/node/buffer.ts`, namespace: 'imports' };
            });

            build.onResolve({filter: /.*/, namespace: 'imports'}, resolveUrl)

            build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                return loadSource(args)
            })
        },
    },
    ...denoPlugins()
  ],
  entryPoints: ['index.ts'],
  outfile: 'build/bundle.js',
  bundle: true,
  platform: 'node',
  target: 'deno1.32.4',
  format: 'esm',
  globalName: 'perspective.diff.sync.language',
  charset: 'ascii',
  legalComments: 'inline'
});
console.log(result.outputFiles);

esbuild.stop();