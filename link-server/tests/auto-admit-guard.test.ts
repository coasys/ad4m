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
    delete base.PUBLIC_ORIGIN;
    delete base.REFRESH_INTERVAL_MS;
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

/** Starts the CLI with `args` and `env` (without NODE_ENV): whether it listens, and what it printed. */
async function startsWith(args: string[], env: Record<string, string>): Promise<{ started: boolean; out: string }> {
  const dataDir = mkdtempSync(path.join(tmpdir(), "link-server-cli-"));
  const base = { ...process.env };
  delete base.NODE_ENV;
  delete base.AUTO_ADMIT;
  delete base.ALLOW_AUTO_ADMIT_DEV;
  delete base.PUBLIC_ORIGIN;
  delete base.REFRESH_INTERVAL_MS;
  const child = spawn(
    process.execPath,
    ["--import", "tsx", "src/index.ts", "--data", dataDir, "--port", "0", "--host", "127.0.0.1", ...args],
    { cwd: root, env: { ...base, ...env }, stdio: ["ignore", "pipe", "pipe"] }
  );
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
    return { started, out };
  } finally {
    child.kill();
    await new Promise((r) => (child.exitCode !== null ? r(null) : child.once("exit", r)));
    rmSync(dataDir, { recursive: true, force: true });
  }
}

test("AUTO_ADMIT with ALLOW_AUTO_ADMIT_DEV=true starts", async () => {
  const { started, out } = await startsWith([], { AUTO_ADMIT: "true", ALLOW_AUTO_ADMIT_DEV: "true" });
  assert.ok(started, out);
});

// ---- the public origin auth is signed for ----

test("without PUBLIC_ORIGIN a server that is not marked for development exits 1 before listening", () => {
  for (const env of [{}, { NODE_ENV: "production", ALLOW_AUTO_ADMIT_DEV: "true" }]) {
    const run = runCli([], env);
    assert.equal(run.status, 1, run.stdout + run.stderr);
    assert.match(run.stderr, /PUBLIC_ORIGIN/);
    assert.doesNotMatch(run.stdout, /listening/);
  }
});

test("a PUBLIC_ORIGIN or --public-origin that is not an origin exits 1", () => {
  for (const run of [
    runCli([], { PUBLIC_ORIGIN: "https://links.example.org/rooms" }),
    runCli(["--public-origin", "links.example.org"], {}),
  ]) {
    assert.equal(run.status, 1, run.stdout + run.stderr);
    assert.match(run.stderr, /origin/);
  }
});

test("a server with PUBLIC_ORIGIN (comma-separated) or --public-origin (repeatable) starts", async () => {
  const cases: Array<[string[], Record<string, string>]> = [
    [[], { PUBLIC_ORIGIN: "https://links.example.org, http://10.0.0.5:3456" }],
    [["--public-origin", "https://links.example.org", "--public-origin", "http://10.0.0.5:3456"], {}],
  ];
  for (const [args, env] of cases) {
    const { started, out } = await startsWith(args, env);
    assert.ok(started, out);
    assert.match(out, /https:\/\/links\.example\.org/, "it logs the origins it takes auth for");
    assert.match(out, /http:\/\/10\.0\.0\.5:3456/);
  }
});

test("a development server (ALLOW_AUTO_ADMIT_DEV=true) without PUBLIC_ORIGIN starts, taking the Host header's origin", async () => {
  const { started, out } = await startsWith([], { ALLOW_AUTO_ADMIT_DEV: "true" });
  assert.ok(started, out);
  assert.match(out, /Host/);
});

test("the usage names the public origin settings", () => {
  const run = runCli(["--help"], {});
  assert.equal(run.status, 0);
  for (const name of ["--public-origin", "PUBLIC_ORIGIN"]) {
    assert.ok(run.stdout.includes(name), `usage names ${name}`);
  }
});

test("the usage names the identity store settings", () => {
  const run = runCli(["--help"], {});
  assert.equal(run.status, 0);
  for (const name of ["--store-url", "--store-head-keys", "--name", "STORE_URL", "STORE_HEAD_KEYS"]) {
    assert.ok(run.stdout.includes(name), `usage names ${name}`);
  }
});

// ---- the key refresh interval ----

test("a REFRESH_INTERVAL_MS or --refresh-interval-ms below 1000 or not a number exits 1", () => {
  const origin = ["--public-origin", "https://links.example.org"];
  for (const run of [
    runCli(origin, { REFRESH_INTERVAL_MS: "500" }),
    runCli(origin, { REFRESH_INTERVAL_MS: "soon" }),
    runCli([...origin, "--refresh-interval-ms", "999"], {}),
  ]) {
    assert.equal(run.status, 1, run.stdout + run.stderr);
    assert.match(run.stderr, /at least 1000/);
    assert.doesNotMatch(run.stdout, /listening/);
  }
});

test("a server with REFRESH_INTERVAL_MS or --refresh-interval-ms starts, and the usage names both", async () => {
  const origin = ["--public-origin", "https://links.example.org"];
  for (const [args, env] of [
    [origin, { REFRESH_INTERVAL_MS: "2000" }],
    [[...origin, "--refresh-interval-ms", "2000"], {}],
  ] as Array<[string[], Record<string, string>]>) {
    const { started, out } = await startsWith(args, env);
    assert.ok(started, out);
  }
  const usage = runCli(["--help"], {});
  for (const name of ["--refresh-interval-ms", "REFRESH_INTERVAL_MS"]) {
    assert.ok(usage.stdout.includes(name), `usage names ${name}`);
  }
});
