const esbuild = require("esbuild");

const isDev = process.env.NODE_ENV !== "production";

esbuild
  .build({
    entryPoints: ["./src/utils.ts"],
    bundle: true,
    format: "esm",
    minify: !isDev,
    sourcemap: isDev,
    outfile: "dist/utils.js",
    watch: process.env.NODE_ENV === "dev" ? true : false,
  })
  .catch(() => process.exit(1));
