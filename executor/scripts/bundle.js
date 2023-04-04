import esbuild from 'esbuild'
import { polyfillNodeForDeno } from "esbuild-plugin-polyfill-node";
import path from 'path'

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

function denoAliasLocal(nodeModule) {
    return {
        name: `${nodeModule}-alias`,
        setup(build) {
            build.onResolve({ filter: new RegExp(`^${nodeModule}$`) }, (args) => {
                return { path: path.resolve(`deno_std-0.177.0/node/${nodeModule}.ts`), external: false };
            });
        },
    }
}

function denoAliasNode(nodeModule) {
    return {
        name: `${nodeModule}-alias`,
        setup(build) {
            build.onResolve({ filter: new RegExp(`^${nodeModule}$`) }, (args) => {
                return { path: `node:${nodeModule}`, external: true };
            });
        },
    }
}

esbuild
  .build({
    entryPoints: ['lib/deno.js'],
    outfile: 'lib/bundle.js',
    bundle: true,
    platform: 'node',
    target: 'deno1.3',
    format: 'esm',
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
            'crypto', 'path', 'fs', 'child_process', 'net', 'dns', 'cluster', 'https',
            'dgram', 'os', 'tls', 'http', 'url', 'util', 'stream', 'events', 'tty',
            'zlib', 'assert', 'buffer', 'constants', 'querystring', 'string_decoder',
            'global'
        ].map(denoAliasLocal),
        {
            name: `dns-promisis-alias`,
            setup(build) {
                build.onResolve({ filter: new RegExp(`^dns/promises$`) }, (args) => {
                    return { path: path.resolve(`deno_std-0.177.0/node/dns.ts`), external: false };
                });
            },
        },
        {
            name: `fs-promisis-alias`,
            setup(build) {
                build.onResolve({ filter: new RegExp(`^fs/promises$`) }, (args) => {
                    return { path: path.resolve(`deno_std-0.177.0/node/fs.ts`), external: false };
                });
            },
        },
        {
            name: `ws-alias`,
            setup(build) {
                build.onResolve({ filter: new RegExp(`^ws$`) }, (args) => {
                    return { path: path.resolve(`deno-websocket/mod.ts`), external: false };
                });
            },
        },
    ],
    
  })
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });