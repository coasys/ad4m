const { default: litPlugin } = require("esbuild-plugin-lit");
const esbuild = require("esbuild");

const buildOptions = {
  entryPoints: ["./src/web.ts"],
  bundle: true,
  format: "esm",
  minify: true,
  sourcemap: process.env.NODE_ENV === "dev" ? true : false,
  outfile: "dist/web.js",
  plugins: [litPlugin()],
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
