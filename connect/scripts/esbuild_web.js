const { default: litPlugin } = require("esbuild-plugin-lit");
const esbuild = require("esbuild");

const isDev = process.env.NODE_ENV !== "production";

esbuild
  .build({
    entryPoints: ["./src/web.ts"],
    bundle: true,
    format: "esm",
    minify: !isDev,
    sourcemap: isDev,
    outfile: "dist/web.js",
    watch: process.env.NODE_ENV === "dev" ? true : false,
    plugins: [litPlugin()],
  })
  .catch(() => process.exit(1));
