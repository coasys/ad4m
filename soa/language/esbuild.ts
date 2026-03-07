import * as esbuild from "esbuild";
import { join } from "path";

const result = await esbuild.build({
  entryPoints: [join(import.meta.dirname, "src", "index.ts")],
  bundle: true,
  outfile: join(import.meta.dirname, "dist", "index.js"),
  format: "esm",
  platform: "node",
  target: "es2020",
  sourcemap: true,
  external: ["@perspect3vism/ad4m"],
});

console.log("Build completed:", result);
