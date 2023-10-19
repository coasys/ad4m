import * as esbuild from "https://deno.land/x/esbuild@v0.18.2/mod.js";
import fs from "node:fs"
import { loadSource, resolveUrl } from "./customHttpDownloader.js";

function denoAlias(nodeModule) {
    return {
        name: `${nodeModule}-alias`,
        setup(build) {
            build.onResolve({ filter: new RegExp(`^node:${nodeModule}$`) }, (args) => {
                console.log('meow 1111', args)
                return { path: nodeModule, namespace: 'imports' };
            });
        },
    }
}

const nodePackages = [
    'path', 'fs', 'net', 'dns', 'cluster', 'https',
    'dgram', 'os', 'tls', 'http', 'url', 'util', 'stream', 'events', 'tty',
    'zlib', 'assert', 'buffer', 'constants', 'querystring', 'string_decoder',
    'global', 'process',
];

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
            name: `dns-promisis-alias`,
            setup(build) {
                build.onResolve({ filter: new RegExp(`^dns/promises$`) }, (args) => {
                    return { path: `https://deno.land/std@0.177.0/node/dns.ts`, external: true };
                });

                build.onResolve({filter: /.*/, namespace: 'imports'}, resolveUrl)

                build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                    const packageName = args.path.slice('node:'.length);
                    if(!nodePackages.includes(packageName)) {
                        return loadSource(args)
                    }
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
            name: `crypto-promisis-alias`,
            setup(build) {
                build.onResolve({ filter: new RegExp(`^crypto$`) }, (args) => {
                    return { path: `node:crypto`, external: true, namespace: "imports" };
                });

                build.onResolve({filter: new RegExp(`https://deno.land/std@0.203.0/crypto/crypto.ts`), namespace: 'imports'}, resolveUrl)

                build.onLoad({filter: new RegExp(`https://deno.land/std@0.203.0/crypto/crypto.ts`), namespace: 'file'}, (args) => {
                    console.log('wow 1', args)
                    return loadSource({path: 'https://deno.land/std@0.203.0/crypto/crypto.ts'})
                })
            },
        },
        {
            name: `ws-alias`,
            setup(build) {
                build.onResolve({ filter: new RegExp(`^ws$`) }, (args) => {
                    return { path: `https://deno.land/x/websocket@v0.1.4/mod.ts`, namespace: 'imports' };
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
                    const packageName = args.path.slice('node:'.length);
                    if(!nodePackages.includes(packageName)) {
                        return loadSource(args)
                    }
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
            name: "https://deno.land/x/xhr@0.3.0/mod.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/x/xhr@0.3.0/mod.ts$`) }, (args) => {
                    return { path: `https://deno.land/x/xhr@0.3.0/mod.ts`, namespace: 'imports' };
                });
            },
        },
        {
            name: "https://deno.land/std@0.177.0/node/global.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/std@0.177.0/node/global.ts$`) }, (args) => {
                    return { path: `https://deno.land/std@0.177.0/node/global.ts`, namespace: 'imports' };
                });

                build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                    return loadSource(args)
                })
            },
        },
        {
            name: "https://deno.land/std@0.203.0/crypto/crypto.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/std@0.203.0/crypto/crypto.ts$`) }, (args) => {
                    return { path: `https://deno.land/std@0.203.0/crypto/crypto.ts`, namespace: 'imports' };
                });

                build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                    return loadSource(args)
                })
            },
        },
        {
            name: "https://deno.land/x/getport/mod.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/x/getport/mod.ts$`) }, (args) => {
                    return { path: `https://deno.land/x/getport/mod.ts`, namespace: 'imports' };
                });

                build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                    return loadSource(args)
                })
            },
        },
        {
            name: "https://deno.land/std@0.203.0/path/mod.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/std@0.203.0/path/mod.ts$`) }, (args) => {
                    return { path: `https://deno.land/std@0.203.0/path/mod.ts`, namespace: 'imports' };
                });

                build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                    return loadSource(args)
                })
            },
        },
        {
            name: "https://deno.land/std@0.203.0/fs/mod.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/std@0.203.0/fs/mod.ts$`) }, (args) => {
                    return { path: `https://deno.land/std@0.203.0/fs/mod.ts`, namespace: 'imports' };
                });

                build.onLoad({filter: /.*/, namespace: 'imports'}, (args) => {
                    return loadSource(args)
                })
            },
        },
        {
            name: "https://deno.land/std@0.195.0/path/mod.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/std@0.195.0/path/mod.ts$`) }, (args) => {
                    return { path: `https://deno.land/std@0.195.0/path/mod.ts`, namespace: 'imports' };
                });
            },
        },
    ],
});

esbuild.stop();