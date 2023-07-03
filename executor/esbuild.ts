import * as esbuild from "https://deno.land/x/esbuild@v0.18.2/mod.js";
import * as path from "https://deno.land/std@0.177.0/path/mod.ts";
import { loadSource, resolveUrl } from "./scripts/customHttpDownloader.js";

function denoAlias(nodeModule) {
    return {
        name: `${nodeModule}-alias`,
        setup(build) {
            build.onResolve({ filter: new RegExp(`^${nodeModule}$`) }, (args) => {
                return { path: `https://deno.land/std@0.177.0/node/${nodeModule}.ts`, namespace: 'imports' };
            });

            build.onResolve({filter: /.*/, namespace: 'imports'}, resolveUrl)

            build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                return loadSource(args)
            })
        },
    }
}

const result = await esbuild.build({
    entryPoints: ['src/deno.ts'],
    outfile: 'lib/bundle.js',
    bundle: true,
    platform: 'node',
    target: 'esnext',
    format: 'esm',
    globalName: 'executor',
    charset: 'ascii',
    legalComments: 'inline',
    plugins: [
        {
            name: `node:net`,
            setup(build) {
                build.onResolve({ filter: new RegExp(`^node:net$`) }, (args) => {
                    return { path: path.resolve(`deno_std-0.177.0/node/net.ts`), external: false };
                });
            },
        },
        ...[
            'crypto', 'path', 'fs', 'net', 'dns', 'cluster', 'https',
            'dgram', 'os', 'tls', 'http', 'url', 'util', 'stream', 'events', 'tty',
            'zlib', 'assert', 'buffer', 'constants', 'querystring', 'string_decoder',
            'global', 'process',
        ].map(denoAlias),
        {
            name: `dns-promisis-alias`,
            setup(build) {
                build.onResolve({ filter: new RegExp(`^dns/promises$`) }, (args) => {
                    return { path: `https://deno.land/std@0.177.0/node/dns.ts`, external: true };
                });
            },
        },
        {
            name: `child_process`,
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/std@0.177.0/node/child_process.ts$`) }, (args) => {
                    return { path: `https://deno.land/std@0.177.0/node/child_process.ts`, namespace: 'imports' };
                });

                build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                    return loadSource(args)
                })
            },
        },
        {
            name: `fs-promisis-alias`,
            setup(build) {
                build.onResolve({ filter: new RegExp(`^fs/promises$`) }, (args) => {
                    return { path: `https://deno.land/std@0.177.0/node/fs.ts`, external: true };
                });
            },
        },
        {
            name: `ws-alias`,
            setup(build) {
                build.onResolve({ filter: new RegExp(`^ws$`) }, (args) => {
                    return { path: `https://deno.land/x/websocket@v0.1.4/mod.ts`, external: true };
                });
            },
        },
        {
            name: `aloe`,
            setup(build) {
                build.onResolve({ filter: new RegExp(`^aloedb-node$`) }, (args) => {
                    return { path: 'https://deno.land/x/aloedb@0.9.0/mod.ts', namespace: 'imports' };
                });

                build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                    return loadSource(args)
                })
            },
        },
        {
            name: "https://deno.land/std@0.150.0/media_types/mod.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/std@0.150.0/media_types/mod.ts$`) }, (args) => {
                    return { path: `https://deno.land/std@0.177.0/media_types/mod.ts`, external: true };
                });
            },
        },
        {
            name: "https://deno.land/std@0.177.0/node/util.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/std@0.177.0/node/util.ts$`) }, (args) => {
                    console.log('test', args)
                    return { path: `https://deno.land/std@0.177.0/node/util.ts`, namespace: 'imports' };
                });

                build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                    return loadSource(args)
                })
            },
        },
        {
            name: "https://deno.land/std@0.177.0/node/os.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/std@0.177.0/node/os.ts$`) }, (args) => {
                    console.log('test', args)
                    return { path: `https://deno.land/std@0.177.0/node/os.ts`, namespace: 'imports' };
                });

                build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                    return loadSource(args)
                })
            },
        },
        {
            name: "https://deno.land/std@0.177.0/node/global.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/std@0.177.0/node/global.ts$`) }, (args) => {
                    console.log('test', args)
                    return { path: `https://deno.land/std@0.177.0/node/global.ts`, namespace: 'imports' };
                });

                build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                    return loadSource(args)
                })
            },
        },
        {
            name: "https://deno.land/std@0.177.0/node/path.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/std@0.177.0/node/path.ts$`) }, (args) => {
                    console.log('test', args)
                    return { path: `https://deno.land/std@0.177.0/node/path.ts`, namespace: 'imports' };
                });

                build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                    return loadSource(args)
                })
            },
        },
        {
            name: "https://deno.land/x/xhr@0.3.0/mod.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/x/xhr@0.3.0/mod.ts$`) }, (args) => {
                    console.log('test', args)
                    return { path: `https://deno.land/x/xhr@0.3.0/mod.ts`, namespace: 'imports' };
                });

                build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                    return loadSource(args)
                })
            },
        },
    ],
});
console.log(result.outputFiles);

esbuild.stop();