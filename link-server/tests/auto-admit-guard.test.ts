import assert from "node:assert/strict";
import { spawn, spawnSync } from "node:child_process";
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
    delete base.ALLOW_AUTO_ADMIT_DEV;
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

test("AUTO_ADMIT without NODE_ENV and without ALLOW_AUTO_ADMIT_DEV exits 1 (an image that sets no NODE_ENV)", () => {
  for (const run of [runCli([], { AUTO_ADMIT: "true" }), runCli(["--auto-admit"], {})]) {
    assert.equal(run.status, 1, run.stdout + run.stderr);
    assert.match(run.stderr, /ALLOW_AUTO_ADMIT_DEV/);
    assert.doesNotMatch(run.stdout, /listening/);
  }
});

test("ALLOW_AUTO_ADMIT_DEV=true does not lift the NODE_ENV=production refusal", () => {
  const run = runCli([], { NODE_ENV: "production", AUTO_ADMIT: "true", ALLOW_AUTO_ADMIT_DEV: "true" });
  assert.equal(run.status, 1, run.stdout + run.stderr);
  assert.match(run.stderr, /AUTO_ADMIT/);
});

test("AUTO_ADMIT with ALLOW_AUTO_ADMIT_DEV=true starts", async () => {
  const dataDir = mkdtempSync(path.join(tmpdir(), "link-server-cli-"));
  const base = { ...process.env };
  delete base.NODE_ENV;
  const child = spawn(process.execPath, ["--import", "tsx", "src/index.ts", "--data", dataDir, "--port", "0", "--host", "127.0.0.1"], {
    cwd: root,
    env: { ...base, AUTO_ADMIT: "true", ALLOW_AUTO_ADMIT_DEV: "true" },
    stdio: ["ignore", "pipe", "pipe"],
  });
  try {
    let out = "";
    const started = await new Promise<boolean>((resolve) => {
      const timer = setTimeout(() => resolve(false), 20_000);
      const seen = (b: Buffer) => {
        out += b.toString();
        if (/listening/i.test(out)) {
          clearTimeout(timer);
          resolve(true);
        }
      };
      child.stdout.on("data", seen);
      child.stderr.on("data", seen);
      child.on("exit", () => {
        clearTimeout(timer);
        resolve(false);
      });
    });
    assert.ok(started, out);
  } finally {
    child.kill();
    await new Promise((r) => (child.exitCode !== null ? r(null) : child.once("exit", r)));
    rmSync(dataDir, { recursive: true, force: true });
  }
});

test("the usage names the identity store settings", () => {
  const run = runCli(["--help"], {});
  assert.equal(run.status, 0);
  for (const name of ["--store-url", "--store-head-keys", "--name", "STORE_URL", "STORE_HEAD_KEYS"]) {
    assert.ok(run.stdout.includes(name), `usage names ${name}`);
  }
});
