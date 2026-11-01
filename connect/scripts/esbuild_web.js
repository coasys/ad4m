const { default: litPlugin } = require("esbuild-plugin-lit");
const esbuild = require("esbuild");

const isDev = process.env.NODE_ENV !== "production";

esbuild
  .build({
    entryPoints: ["./src/web.ts"],
    bundle: true,
    // The SDK is the app's, not ours: a bundled copy is a second set of classes, so `instanceof`
    // against the app's import (RpcError, PerspectiveProxy) silently fails. A peer dependency.
    external: ["@coasys/ad4m"],
    format: "esm",
    minify: !isDev,
    sourcemap: isDev,
    outfile: "dist/web.js",
    watch: process.env.NODE_ENV === "dev" ? true : false,
    plugins: [litPlugin()],
  })
  .catch(() => process.exit(1));
