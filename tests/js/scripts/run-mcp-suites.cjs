// Run every MCP integration suite even when an earlier one fails, with a
// cleanup between each pair, and fail at the END with a per-suite summary.
//
// Why not `a && cleanup && b && …` (the previous shape): a short-circuiting
// chain means a red suite hides every later suite entirely — "test-mcp
// failed" only tells you about suites up to the first failure, and a
// cross-cutting change gets discovered one suite per CI round, with a push
// in between (PR #1031 paid three rounds for what one complete run would
// have shown). The chain had also lost the cleanup between the last two
// suites; here every pair gets one.
//
// CommonJS on purpose (`.cjs`): the package is `"type": "module"`.
"use strict";
const { spawnSync } = require("node:child_process");

const SUITES = [
  "test-mcp-http",
  "test-mcp-static",
  "test-mcp-auth",
  "test-mcp-neighbourhood",
  "test-mcp-mcporter",
];

// pnpm resolves as pnpm.cmd on Windows, which needs a shell.
const spawnOpts = { stdio: "inherit", shell: process.platform === "win32" };

function cleanup(label) {
  const res = spawnSync("node", ["scripts/cleanup.js"], spawnOpts);
  if (res.status !== 0) {
    console.error(`cleanup ${label} failed (exit ${res.status})`);
  }
  return res.status === 0;
}

const results = [];
for (const suite of SUITES) {
  if (!cleanup(`before ${suite}`)) {
    // A failed cleanup means the suite would start on dirty state — count
    // the suite as failed rather than run it against leftovers.
    results.push([suite, "SKIPPED (cleanup failed)"]);
    continue;
  }
  const run = spawnSync("pnpm", ["run", suite], spawnOpts);
  results.push([suite, run.status === 0 ? "pass" : `FAIL (exit ${run.status})`]);
}
cleanup("after last suite");

console.log("\n=== MCP suite summary ===");
for (const [suite, verdict] of results) {
  console.log(`  ${suite}: ${verdict}`);
}
const failed = results.filter(([, verdict]) => verdict !== "pass");
if (failed.length > 0) {
  console.error(`\n${failed.length} of ${SUITES.length} MCP suites failed.`);
  process.exit(1);
}
console.log(`\nAll ${SUITES.length} MCP suites passed.`);
