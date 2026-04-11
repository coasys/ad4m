const esbuild = require("esbuild");

const buildOptions = {
  entryPoints: ["./src/electron.ts"],
  external: ["electron"],
  platform: "node",
  bundle: true,
  format: "cjs",
  minify: true,
  sourcemap: false,
  outfile: "dist/electron.js",
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
