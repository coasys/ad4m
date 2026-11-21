import * as esbuild from "esbuild";
import { fileURLToPath } from "url";
import { dirname, join } from "path";

const __dirname = dirname(fileURLToPath(import.meta.url));

const result = await esbuild.build({
  entryPoints: [join(__dirname, "src", "index.ts")],
  bundle: true,
  outfile: join(__dirname, "dist", "index.js"),
  format: "esm",
  platform: "node",
  target: "es2020",
  sourcemap: true,
  external: ["@perspect3vism/ad4m"],
});

console.log("Build completed:", result);
