import * as esbuild from "https://deno.land/x/esbuild@v0.17.18/mod.js";
// Import the WASM build on platforms where running subprocesses is not
// permitted, such as Deno Deploy, or when running without `--allow-run`.
// import * as esbuild from "https://deno.land/x/esbuild@v0.17.18/wasm.js";

import { denoPlugins } from "https://deno.land/x/esbuild_deno_loader@0.7.0/mod.ts";

// Resolve `@coasys/ad4m-ldk` to its compiled lib in the workspace. The
// Deno esbuild plugin does not consult the workspace's node_modules and
// claims the resolve pass first, so we install a tiny upstream plugin
// that short-circuits the workspace alias before denoPlugins sees it.
const ad4mLdkEntry = new URL(
  "../../ad4m-ldk/js/lib/index.js",
  import.meta.url,
).pathname;

const ad4mLdkAliasPlugin = {
  name: "ad4m-ldk-alias",
  setup(build: any) {
    build.onResolve({ filter: /^ad4m:host\.ts$/ }, () => ({
      path: "ad4m:host.ts",
      external: true,
    }));
    build.onResolve({ filter: /^@coasys\/ad4m-ldk$/ }, () => ({
      path: ad4mLdkEntry,
      namespace: "file",
    }));
  },
};

const result = await esbuild.build({
  plugins: [ad4mLdkAliasPlugin, ...denoPlugins()],
  entryPoints: ['index.ts'],
  outfile: 'build/bundle.js',
  bundle: true,
  platform: 'node',
  target: 'deno1.32.4',
  format: 'esm',
  globalName: 'centralized.agent.language',
  charset: 'ascii',
  legalComments: 'inline'
});
console.log(result.outputFiles);

esbuild.stop();