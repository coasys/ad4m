const esbuild = require("esbuild");

const buildOptions = {
  entryPoints: ["./src/core.ts"],
  bundle: true,
  format: "esm",
  minify: true,
  sourcemap: false,
  outfile: "dist/core.js",
};

async function main() {
  if (process.env.NODE_ENV === "dev") {
    const ctx = await esbuild.context(buildOptions);
    await ctx.watch();
  } else {
    await esbuild.build(buildOptions);
  }
}

main().catch(() => process.exit(1));
