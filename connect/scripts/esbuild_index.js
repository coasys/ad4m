const esbuild = require("esbuild");

const buildOptions = {
  entryPoints: ["./src/index.ts"],
  bundle: true,
  format: "esm",
  minify: true,
  sourcemap: false,
  outfile: "dist/index.js",
};

(async () => {
  if (process.env.NODE_ENV === "dev") {
    const ctx = await esbuild.context(buildOptions);
    await ctx.watch();
    console.log("Watching for changes...");
  } else {
    // For production build, use build() directly
    await esbuild.build(buildOptions);
  }
})().catch((err) => {
  console.error(err);
  process.exit(1);
});
