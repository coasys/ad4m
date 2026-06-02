import { defineConfig } from "tsdown";

export default defineConfig({
  entry: ["index.ts"],
  format: "esm",
  outDir: "dist",
  target: "node20",
  platform: "node",
  dts: false,
  clean: true,
  sourcemap: true,
  deps: {
    neverBundle: [/^@coasys\//, /^node:/],
  },
});
