/**
 * Bundle the holograph-link Language module to build/bundle.js.
 *
 * Mirrors `p-diff-sync/esbuild.ts`'s plugin layout:
 *   - alias `@coasys/ad4m-ldk` to the workspace's compiled lib so deno
 *     doesn't try to resolve through `node_modules`,
 *   - mark `ad4m:host` as external (resolved at runtime by the executor's
 *     StringModuleLoader against `rust-executor/src/js_core/host.js`).
 */

import * as esbuild from "https://deno.land/x/esbuild@v0.17.18/mod.js";
import { denoPlugins } from "https://deno.land/x/esbuild_deno_loader@0.7.0/mod.ts";

const ad4mLdkEntry = new URL(
  "../../ad4m-ldk/js/lib/index.js",
  import.meta.url,
).pathname;

const ad4mLdkAliasPlugin = {
  name: "ad4m-ldk-alias",
  setup(build: any) {
    build.onResolve({ filter: /^ad4m:host$/ }, () => ({
      path: "ad4m:host",
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
  entryPoints: ["index.ts"],
  outfile: "build/bundle.js",
  bundle: true,
  platform: "node",
  target: "deno1.32.4",
  format: "esm",
  globalName: "holograph.link.language",
  charset: "ascii",
  legalComments: "inline",
});

console.log(result.outputFiles);
esbuild.stop();
