/**
 * Same bundler invocation the bootstrap languages use
 * (`bootstrap-languages/agent-language/esbuild.ts`), plus a second
 * workspace alias for the model-only SDK entry. Run:
 *   deno run --allow-all esbuild.ts
 *
 * The metafile lists every module that lands in the bundle — the proof
 * that no client code (Apollo, GraphQL, PerspectiveProxy, Ad4mClient)
 * ships with a language that imports only `@coasys/ad4m/model`.
 */
import * as esbuild from "https://deno.land/x/esbuild@v0.17.18/mod.js";
import { denoPlugins } from "https://deno.land/x/esbuild_deno_loader@0.7.0/mod.ts";

const root = new URL("../../../", import.meta.url).pathname;
const ad4mLdkEntry = `${root}ad4m-ldk/js/lib/index.js`;
const ad4mModelEntry = `${root}core/lib/model.js`;

const workspaceAlias = {
    name: "workspace-alias",
    setup(build: any) {
        build.onResolve({ filter: /^ad4m:host$/ }, () => ({
            path: "ad4m:host",
            external: true,
        }));
        build.onResolve({ filter: /^@coasys\/ad4m-ldk$/ }, () => ({
            path: ad4mLdkEntry,
            namespace: "file",
        }));
        build.onResolve({ filter: /^@coasys\/ad4m\/model$/ }, () => ({
            path: ad4mModelEntry,
            namespace: "file",
        }));
    },
};

const result = await esbuild.build({
    plugins: [workspaceAlias, ...denoPlugins()],
    entryPoints: ["index.ts"],
    outfile: "build/bundle.js",
    bundle: true,
    platform: "node",
    target: "deno1.32.4",
    format: "esm",
    globalName: "bundle.proof",
    charset: "ascii",
    legalComments: "inline",
    metafile: true,
});

console.log(JSON.stringify(result.metafile, null, 2));
esbuild.stop();
