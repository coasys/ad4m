import * as esbuild from "https://deno.land/x/esbuild@v0.17.18/mod.js";
import { denoPlugins } from "https://deno.land/x/esbuild_deno_loader@0.7.0/mod.ts";

const result = await esbuild.build({
  plugins: [...denoPlugins()],
  entryPoints: ['index.ts'],
  outfile: 'build/bundle.js',
  bundle: true,
  platform: 'node',
  target: 'deno1.32.4',
  format: 'esm',
  globalName: 'aesFlat',
  charset: 'ascii',
  legalComments: 'inline'
});
console.log(result.outputFiles);
esbuild.stop();
