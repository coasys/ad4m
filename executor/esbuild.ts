import * as esbuild from "https://deno.land/x/esbuild@v0.18.2/mod.js";
import { denoPlugins } from "https://deno.land/x/esbuild_deno_loader@0.8.1/mod.ts";

function denoAlias(nodeModule) {
    return {
        name: `${nodeModule}-alias`,
        setup(build) {
            build.onResolve({ filter: new RegExp(`^${nodeModule}$`) }, (args) => {
                return { path: `https://deno.land/std@0.177.0/${nodeModule}/mod.ts`, external: true };
            });
        },
    }
}

const result = await esbuild.build({
    entryPoints: ['src/main.ts'],
    outfile: 'lib/bundle.js',
    bundle: true,
    platform: 'node',
    target: 'esnext',
    format: 'esm',
    globalName: 'executor',
    charset: 'ascii',
    legalComments: 'inline',
    plugins: [
        ...denoPlugins({configPath: "/Users/josh/dev/ad4m/executor/deno.json"}), 
    ],
});
console.log(result.outputFiles);

esbuild.stop();