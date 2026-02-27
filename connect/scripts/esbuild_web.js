const { default: litPlugin } = require("esbuild-plugin-lit");
const esbuild = require("esbuild");

esbuild
  .build({
    entryPoints: ["./src/web.ts"],
    bundle: true,
    format: "esm",
    minify: true,
    sourcemap: process.env.NODE_ENV === "dev" ? true : false,
    outfile: "dist/web.js",
    watch: process.env.NODE_ENV === "dev" ? true : false,
    plugins: [litPlugin()],
    external: ["@coasys/ad4m"],
  })
  .catch(() => process.exit(1));
