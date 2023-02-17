const esbuild = require("esbuild");

esbuild
  .build({
    entryPoints: ["./src/electron.ts"],
    external: ["electron"],
    platform: "node",
    bundle: true,
    format: "cjs",
    minify: true,
    sourcemap: false,
    outfile: "dist/electron.js",
    watch: process.env.NODE_ENV === "dev" ? true : false,
  })
  .catch(() => process.exit(1));
