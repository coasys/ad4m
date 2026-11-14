import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const root = path.dirname(path.dirname(fileURLToPath(import.meta.url)));

/** Runs the CLI (`src/index.ts`) with `env` on top of this process's, without NODE_ENV. */
function runCli(args: string[], env: Record<string, string>) {
  const dataDir = mkdtempSync(path.join(tmpdir(), "link-server-cli-"));
  try {
    const base = { ...process.env };
    delete base.NODE_ENV;
    delete base.AUTO_ADMIT;
    return spawnSync(process.execPath, ["--import", "tsx", "src/index.ts", "--data", dataDir, "--port", "0", ...args], {
      cwd: root,
      env: { ...base, ...env },
      encoding: "utf8",
      timeout: 20_000,
    });
  } finally {
    rmSync(dataDir, { recursive: true, force: true });
  }
}

test("--auto-admit under NODE_ENV=production exits 1 before listening", () => {
  const run = runCli(["--auto-admit"], { NODE_ENV: "production" });
  assert.equal(run.status, 1, run.stdout + run.stderr);
  assert.match(run.stderr, /AUTO_ADMIT/);
  assert.doesNotMatch(run.stdout, /listening/);
});

test("AUTO_ADMIT=1 under NODE_ENV=production exits 1 as well", () => {
  const run = runCli([], { NODE_ENV: "production", AUTO_ADMIT: "1" });
  assert.equal(run.status, 1, run.stdout + run.stderr);
  assert.match(run.stderr, /AUTO_ADMIT/);
});

test("the usage names the identity store settings", () => {
  const run = runCli(["--help"], {});
  assert.equal(run.status, 0);
  for (const name of ["--store-url", "--store-head-keys", "--name", "STORE_URL", "STORE_HEAD_KEYS"]) {
    assert.ok(run.stdout.includes(name), `usage names ${name}`);
  }
});
