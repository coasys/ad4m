import * as esbuild from "https://deno.land/x/esbuild@v0.17.18/mod.js";
import { denoPlugins } from "https://deno.land/x/esbuild_deno_loader@0.7.0/mod.ts";

// Mark ad4m:host as external so the import survives bundling.
// The deno esbuild plugin intercepts resolution before esbuild's
// top-level `external` array, so we use an onResolve plugin.
const hostExternalPlugin = {
  name: "ad4m-host-external",
  setup(build: any) {
    build.onResolve({ filter: /^ad4m:host$/ }, () => ({
      path: "ad4m:host",
      external: true,
    }));
  },
};

const result = await esbuild.build({
  plugins: [hostExternalPlugin, ...denoPlugins()],
  entryPoints: ['index.ts'],
  outfile: 'build/bundle.js',
  bundle: true,
  platform: 'node',
  target: 'deno1.32.4',
  format: 'esm',
  globalName: 'noteStoreFlat',
  charset: 'ascii',
  legalComments: 'inline'
});
console.log(result.outputFiles);
esbuild.stop();
