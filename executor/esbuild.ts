import * as esbuild from "https://deno.land/x/esbuild@v0.18.2/mod.js";
import { denoPlugins } from "https://deno.land/x/esbuild_deno_loader@0.8.1/mod.ts";
import customModule, { loadSource, resolveUrl } from './scripts/customHttpDownloader.js'
import {join} from "https://deno.land/std@0.177.0/path/mod.ts";

const currentWorkingDirectory = Deno.cwd();

function denoAlias(nodeModule) {
    return {
        name: `${nodeModule}-alias`,
        setup(build) {
            build.onResolve({ filter: new RegExp(`^${nodeModule}$`) }, (args) => {
                return { path: `https://deno.land/std@0.177.0/${nodeModule}/mod.ts`, external: true};
            });
        },
    }
}

function denoStdAlias() {
    return {
        name: `alias`,
        setup(build) {
        build.onResolve({ filter:  /[^https:\/\/]*/ }, (args) => {
            if (args.path.startsWith(".")) {
                    console.log('test3', args, args.path, join(args.resolveDir, args.path))
                    return { path: join(args.resolveDir, args.path) };
                } else {
                    return { path: args.path, namespace: 'imports' }
                }
            });

            build.onLoad({filter:/.*/, namespace: 'imports'}, (build) => {
                console.log('test44', build)
                if (build.path === 'swipl-stdio') {
                    console.log('test4', build)
                    return loadSource({
                        ...build,
                        path: "https://github.com/perspect3vism/node-swipl-stdio#2550966667fa24eebbb1e4fb030e5e7e486b4999"
                    })
                } else {
                    console.log('test5', build)
                    return loadSource({
                        ...build,
                        path: `https://deno.land/std@0.177.0/${build.path}/mod.ts`
                    })
                }
            })
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
        denoStdAlias(),
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
                build.onResolve({ filter: new RegExp(`^child_process$`) }, (args) => {
                    return { path: `https://deno.land/std@0.177.0/node/child_process.ts`, external: true };
                });
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
                    return { path: 'https://deno.land/x/aloedb@0.9.0/mod.ts', external: true };
                });
            },
        },
        {
            name: "https://deno.land/std@0.150.0/media_types/mod.ts",
            setup(build) {
                build.onResolve({ filter: new RegExp(`^https://deno.land/std@0.150.0/media_types/mod.ts$`) }, (args) => {
                    console.log('test 2', args)
                    return { path: `https://deno.land/std@0.177.0/media_types/mod.ts`, external: true };
                });
            },
        },
        customModule,
        ...denoPlugins({configPath: `${currentWorkingDirectory}/deno.json`}),
    ],
});

esbuild.stop();