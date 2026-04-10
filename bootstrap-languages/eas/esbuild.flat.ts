import * as esbuild from "https://deno.land/x/esbuild@v0.17.18/mod.js";
import { denoPlugins } from "https://deno.land/x/esbuild_deno_loader@0.7.0/mod.ts";

const result = await esbuild.build({
  plugins: [...denoPlugins()],
  entryPoints: ['index.flat.ts'],
  outfile: 'build/bundle.flat.js',
  bundle: true,
  platform: 'node',
  target: 'deno1.32.4',
  format: 'esm',
  globalName: 'eas.language.flat',
  charset: 'ascii',
  legalComments: 'inline'
});

console.log(result.outputFiles);
esbuild.stop();
console.log("Built build/bundle.flat.js");
