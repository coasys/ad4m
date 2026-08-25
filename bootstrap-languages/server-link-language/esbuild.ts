import * as esbuild from "https://deno.land/x/esbuild@v0.17.18/mod.js";
import { denoPlugins } from "https://deno.land/x/esbuild_deno_loader@0.7.0/mod.ts";
import { resolve } from "https://deno.land/std@0.177.0/path/mod.ts";

// Resolve `@coasys/ad4m-ldk` — prefer AD4M_LDK_ENTRY env, fall back to
// the monorepo's ad4m-ldk package (two levels up, same convention as
// every other bootstrap-languages/*/esbuild.ts).
const ad4mLdkEntry = Deno.env.get("AD4M_LDK_ENTRY") ||
    new URL("../../ad4m-ldk/js/lib/index.js", import.meta.url).pathname;

// Project root — only resolve .js→.ts within our own source tree
const projectRoot = new URL(".", import.meta.url).pathname.replace(/\/$/, "");

const ad4mLdkAliasPlugin = {
    name: "ad4m-ldk-alias",
    setup(build: any) {
        // Mark ad4m:host as external — resolved at runtime by the executor.
        // This is the ONE external: everything else (including the noble
        // crypto libs) gets bundled in, since the Deno sandbox can't
        // install packages itself.
        build.onResolve({ filter: /^ad4m:host$/ }, () => ({
            path: "ad4m:host",
            external: true,
        }));
        // Resolve @coasys/ad4m-ldk to the local workspace build.
        build.onResolve({ filter: /^@coasys\/ad4m-ldk$/ }, () => ({
            path: ad4mLdkEntry,
            namespace: "file",
        }));
    },
};

// Plugin to resolve .js imports to .ts source files, but ONLY within our
// own hand-written source tree (index.ts + src/**). Everything under
// node_modules — the ALDK's lib/ output AND every bundled npm package
// (@noble/*, etc.) — ships compiled .js with no .ts sibling and must
// resolve as-is. `resolveDir.startsWith(projectRoot)` alone isn't enough
// to distinguish the two: node_modules is physically nested under the
// project root too.
const tsResolverPlugin = {
    name: "ts-resolver",
    setup(build: any) {
        build.onResolve({ filter: /\.js$/ }, (args: any) => {
            if (args.namespace !== "file" || !args.path.startsWith(".")) return;
            const resolveDir = args.resolveDir || ".";
            if (!resolveDir.startsWith(projectRoot)) return;
            if (resolveDir.includes("/node_modules/") || resolveDir.endsWith("/node_modules")) return;
            const tsPath = args.path.replace(/\.js$/, ".ts");
            const resolved = resolve(resolveDir, tsPath);
            return { path: resolved, namespace: "file" };
        });
    },
};

// Resolves bare `@noble/*` specifiers (both our own imports and any
// noble-package-internal cross-imports, e.g. @noble/curves importing from
// @noble/hashes) against the local node_modules tree via the standard
// `import.meta.resolve` ESM algorithm — this correctly follows each
// package's package.json "exports" map without depending on esbuild's
// Deno loader understanding plain (non-`npm:`-prefixed) bare specifiers.
// These libraries must end up BUNDLED IN the output (never external):
// the Deno sandbox has no package manager, so anything not bundled is
// simply unavailable at runtime.
const nobleResolverPlugin = {
    name: "noble-crypto-resolver",
    setup(build: any) {
        // Force @noble/hashes/crypto to the browser/Deno entry point —
        // the Node entry imports bare "crypto" which the Deno sandbox
        // rejects. The browser entry uses globalThis.crypto (Web Crypto
        // API) which Deno provides natively.
        build.onResolve({ filter: /^@noble\/hashes\/crypto$/ }, (args: any) => {
            const resolvedUrl = import.meta.resolve("@noble/hashes");
            const basePath = new URL(resolvedUrl).pathname.replace(/\/esm\/index\.js$/, "");
            return { path: `${basePath}/esm/crypto.js`, namespace: "file" };
        });
        build.onResolve({ filter: /^@noble\// }, (args: any) => {
            const resolvedUrl = import.meta.resolve(args.path);
            return { path: new URL(resolvedUrl).pathname, namespace: "file" };
        });
    },
};

const result = await esbuild.build({
    plugins: [
        ad4mLdkAliasPlugin,
        nobleResolverPlugin,
        tsResolverPlugin,
        ...denoPlugins(),
    ],
    entryPoints: ["index.ts"],
    outfile: "build/bundle.js",
    bundle: true,
    platform: "node",
    target: "deno1.32.4",
    format: "esm",
    globalName: "adam.server.link.language",
    charset: "ascii",
    legalComments: "inline",
});

console.log("Build result:", result);

esbuild.stop();
