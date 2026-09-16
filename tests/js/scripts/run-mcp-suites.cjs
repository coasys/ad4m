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
// Suite MEMBERSHIP is derived from package.json (every script named
// `test-mcp-*`), so a newly added suite cannot be silently forgotten here —
// a suite that never runs produces no output to notice. Run ORDER stays
// explicit below; a mismatch in either direction fails loudly before
// anything runs. (A list that enumerates itself cannot be under-enumerated
// — the same failure shape that produced this PR.)
//
// CommonJS on purpose (`.cjs`): the package is `"type": "module"`.
"use strict";
const { spawnSync } = require("node:child_process");
const path = require("node:path");

// Explicit run order — mirrors the old chain. Membership is checked against
// package.json below.
const SUITE_ORDER = [
  "test-mcp-http",
  "test-mcp-static",
  "test-mcp-auth",
  "test-mcp-neighbourhood",
  "test-mcp-mcporter",
];

// Anchor everything on the package directory so the runner behaves the same
// from any cwd (from the repo root a relative `scripts/cleanup.js` would
// fail looking like five broken suites instead of a wrong cwd).
const PKG_DIR = path.join(__dirname, "..");

const declared = Object.keys(
  require(path.join(PKG_DIR, "package.json")).scripts,
).filter((name) => name.startsWith("test-mcp-"));
const missingHere = declared.filter((s) => !SUITE_ORDER.includes(s));
const missingThere = SUITE_ORDER.filter((s) => !declared.includes(s));
if (missingHere.length > 0 || missingThere.length > 0) {
  if (missingHere.length > 0) {
    console.error(
      `package.json declares MCP suites this runner does not order: ${missingHere.join(", ")} — add them to SUITE_ORDER.`,
    );
  }
  if (missingThere.length > 0) {
    console.error(
      `SUITE_ORDER lists suites package.json does not declare: ${missingThere.join(", ")}.`,
    );
  }
  process.exit(1);
}

// pnpm resolves as pnpm.cmd on Windows, which needs a shell.
const spawnOpts = {
  stdio: "inherit",
  cwd: PKG_DIR,
  shell: process.platform === "win32",
};

// spawnSync semantics this runner leans on: `status` is null on signal kill
// (CI timeout/OOM) and on spawn failure (e.g. binary not on PATH) — both
// fall through `status === 0` as not-a-pass, so everything fails closed.
// `error` carries the spawn-failure cause and is surfaced, not discarded.
function run(cmd, args, label) {
  const res = spawnSync(cmd, args, spawnOpts);
  if (res.error) {
    console.error(`${label}: spawn failed: ${res.error.message}`);
  }
  return res;
}

function cleanup(label) {
  const res = run(
    "node",
    [path.join(PKG_DIR, "scripts", "cleanup.js")],
    `cleanup ${label}`,
  );
  if (res.status !== 0) {
    console.error(`cleanup ${label} failed (exit ${res.status})`);
  }
  return res.status === 0;
}

const PASS = "pass";
const results = [];
for (const suite of SUITE_ORDER) {
  if (!cleanup(`before ${suite}`)) {
    // A failed cleanup means the suite would start on dirty state — skip it
    // and fail the run, rather than testing against leftovers.
    results.push([suite, "SKIPPED (cleanup failed — suite did not run)"]);
    continue;
  }
  const res = run("pnpm", ["run", suite], suite);
  results.push([suite, res.status === 0 ? PASS : `FAIL (exit ${res.status})`]);
}
cleanup("after last suite");

console.log("\n=== MCP suite summary ===");
for (const [suite, verdict] of results) {
  console.log(`  ${suite}: ${verdict}`);
}
const passed = results.filter(([, v]) => v === PASS).length;
const skipped = results.filter(([, v]) => v.startsWith("SKIPPED")).length;
const failed = results.length - passed - skipped;
// Failed and skipped are different diagnoses: failed sends you to the
// tests, skipped sends you to the environment (cleanup/cwd/PATH).
if (failed > 0 || skipped > 0) {
  console.error(
    `\n${passed} passed, ${failed} failed, ${skipped} skipped (did not run) of ${results.length} MCP suites.`,
  );
  process.exit(1);
}
console.log(`\nAll ${results.length} MCP suites passed.`);
