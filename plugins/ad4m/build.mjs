// Bundles the plugin into dist/index.cjs.
//
// `@coasys/ad4m` is aliased to core's source, so the client the plugin talks
// to the executor with is the one from this checkout. node_modules/@coasys/ad4m
// is the last released tarball: bundling it next to the core-source waker
// manager shipped two versions of the client in one file, and the old
// QuerySubscriptionProxy dropped the class argument, so every model-query
// subscription (mentions, children) reached the executor as a SPARQL
// subscribe and was rejected.
//
// Core's own imports (@holochain/client, pako, base64-js) resolve from this
// package's node_modules through nodePaths; CI installs only plugins/ad4m.
import { build } from "esbuild";
import { fileURLToPath } from "node:url";
import * as path from "node:path";

const here = path.dirname(fileURLToPath(import.meta.url));

await build({
  entryPoints: [path.join(here, "index.ts")],
  bundle: true,
  platform: "node",
  format: "cjs",
  target: "node20",
  outfile: path.join(here, "dist", "index.cjs"),
  alias: { "@coasys/ad4m": path.join(here, "..", "..", "core", "src", "index.ts") },
  nodePaths: [path.join(here, "node_modules")],
  logLevel: "warning",
});
