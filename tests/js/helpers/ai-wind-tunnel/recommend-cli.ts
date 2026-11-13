/**
 * Regenerate ai-wind-tunnel/recommendations.json from every evidence file.
 *
 *   pnpm run ai-wind-tunnel-recommend           write it
 *   pnpm run ai-wind-tunnel-recommend -- --check  fail if the checked-in file is stale
 */

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import path from "node:path";
import { loadAllEvidence } from "./evidence.js";
import { DATA_DIR } from "./paths.js";
import { deriveRecommendations } from "./recommend.js";

const file = path.join(DATA_DIR, "recommendations.json");
const evidence = loadAllEvidence();
// The newest evidence's finish time, not the clock: regenerating unchanged evidence changes nothing.
const generatedAt = evidence.map((e) => e.evidence.finishedAt).sort().pop() ?? "1970-01-01T00:00:00.000Z";
const recs = deriveRecommendations(evidence, generatedAt);
const json = JSON.stringify(recs, null, 2) + "\n";

if (process.argv.includes("--check")) {
  const current = existsSync(file) ? readFileSync(file, "utf8") : "";
  if (current !== json) {
    console.error(`${path.relative(process.cwd(), file)} is stale: run pnpm run ai-wind-tunnel-recommend`);
    process.exit(1);
  }
  console.log(`recommendations are current (${evidence.length} evidence files)`);
} else {
  writeFileSync(file, json);
  const n = Object.values(recs.classes).reduce((a, c) => a + Object.keys(c).length, 0);
  console.log(`wrote ${n} recommendations for ${Object.keys(recs.classes).length} classes from ${evidence.length} evidence files`);
}
