// One bundle per entry point. `@coasys/ad4m` is bundled in, so an app that installs this package
// does not need it (core is only a devDependency here).
const esbuild = require("esbuild");

const minify = process.env.NODE_ENV === "production";

esbuild
  .build({
    entryPoints: {
      index: "./src/index.ts",
      core: "./src/core.ts",
      utils: "./src/utils.ts",
    },
    bundle: true,
    format: "esm",
    minify,
    sourcemap: !minify,
    outdir: "dist",
  })
  .catch(() => process.exit(1));
