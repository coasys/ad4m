const esbuild = require("esbuild");

esbuild
  .build({
    entryPoints: ["./src/utils.ts"],
    bundle: true,
    format: "esm",
    minify: true,
    sourcemap: false,
    outfile: "dist/utils.js",
    watch: process.env.NODE_ENV === "dev" ? true : false,
  })
  .catch(() => process.exit(1));
