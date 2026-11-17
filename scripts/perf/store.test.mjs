// Tests of the git history (store.mjs) through record, check and report, against local bare
// repositories in a temp directory. Never the real history repository: every command gets
// PERF_HISTORY_REPO, and CI is unset.
//   node --test 'scripts/perf/*.test.mjs'   (tests/js: pnpm run test-perf-unit)

import assert from "node:assert/strict";
import { spawn, spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { after, test } from "node:test";

import { DEFAULT_REPO, openStore } from "./store.mjs";

const HERE = path.dirname(new URL(import.meta.url).pathname);
const PERF = path.join(HERE, "perf.mjs");
const SHA = "0123456789abcdef0123456789abcdef01234567";

const roots = [];
function tmp() {
  const d = fs.mkdtempSync(path.join(os.tmpdir(), "perf-store-"));
  roots.push(d);
  return d;
}
after(() => roots.forEach((d) => fs.rmSync(d, { recursive: true, force: true })));

const BASE_ENV = Object.fromEntries(
  Object.entries(process.env).filter(([k]) => !k.startsWith("CIRCLE") && !k.startsWith("PERF_") && k !== "WE_SHA" && k !== "CI"),
);

const git = (cwd, ...args) => {
  const r = spawnSync("git", ["-C", cwd, ...args], { encoding: "utf8" });
  assert.equal(r.status, 0, `git ${args.join(" ")}: ${r.stderr}`);
  return r.stdout.trim();
};

/** A bare repository with branch main holding one README commit (as the real one has). */
function bareRepo() {
  const root = tmp();
  const bare = path.join(root, "history.git");
  spawnSync("git", ["init", "-q", "--bare", "-b", "main", bare]);
  const seed = path.join(root, "seed");
  spawnSync("git", ["clone", "-q", bare, seed]);
  fs.writeFileSync(path.join(seed, "README.md"), "wind tunnel\n");
  git(seed, "add", "README.md");
  git(seed, "-c", "user.name=t", "-c", "user.email=t@t.invalid", "commit", "-q", "-m", "init");
  git(seed, "push", "-q", "origin", "HEAD:main");
  return { root, bare };
}

const show = (bare, file) => {
  const r = spawnSync("git", ["-C", bare, "show", `main:${file}`], { encoding: "utf8" });
  return r.status === 0 ? r.stdout : null;
};
const jsonl = (text) => (text ?? "").split("\n").filter(Boolean).map((l) => JSON.parse(l));

function envFor(bare, clone, extra = {}) {
  return { ...BASE_ENV, PERF_HISTORY_REPO: bare, PERF_HISTORY_CLONE: clone, ...extra };
}

function results(dir, metrics) {
  const f = path.join(dir, `r-${Math.random().toString(16).slice(2)}.json`);
  fs.writeFileSync(f, JSON.stringify({ metrics }));
  return f;
}

function perf(args, env) {
  const r = spawnSync(process.execPath, [PERF, ...args], { encoding: "utf8", env });
  return { code: r.status, out: r.stdout, err: r.stderr };
}

const metric = (value) => [{ metric: "m.latency_ms", value, unit: "ms", better: "lower" }];

function record(env, id, value, day, more = []) {
  const f = results(tmp(), metric(value));
  return perf(["record", "--job", "j", "--ad4m-sha", SHA, "--run-id", id, "--run-at", `2026-11-${String(day).padStart(2, "0")}T02:00:00Z`, ...more, f], env);
}

test("record commits the run to <subdir>/<job>.jsonl on the branch and pushes it", () => {
  const { root, bare } = bareRepo();
  const r = record(envFor(bare, path.join(root, "clone")), "run-1", 100, 1);
  assert.equal(r.code, 0, r.err);
  assert.match(r.out, /-> .*history\.git main:nightly\/j\.jsonl/);
  const lines = jsonl(show(bare, "nightly/j.jsonl"));
  assert.deepEqual(lines.map((l) => [l.run_id, l.value]), [["run-1", 100]]);
  assert.equal(show(bare, "README.md"), "wind tunnel\n", "the rest of the branch stays");
  assert.equal(git(bare, "log", "-1", "--format=%s|%an", "main"), "perf(j): 012345678 2026-11-01T02:00:00Z|ad4m nightly");
});

test("the branch and subdir come from PERF_HISTORY_BRANCH and PERF_HISTORY_SUBDIR; a new branch starts empty", () => {
  const { root, bare } = bareRepo();
  const env = envFor(bare, path.join(root, "clone"), { PERF_HISTORY_BRANCH: "perf", PERF_HISTORY_SUBDIR: "runs" });
  assert.equal(record(env, "run-1", 100, 1).code, 0);
  const r = spawnSync("git", ["-C", bare, "show", "perf:runs/j.jsonl"], { encoding: "utf8" });
  assert.equal(jsonl(r.stdout).length, 1);
  assert.equal(spawnSync("git", ["-C", bare, "show", "perf:README.md"]).status, 128, "the new branch has only the history");
});

test("runs from other clones (other runners, other jobs) push in turn; every line lands once", async () => {
  const { root, bare } = bareRepo();
  // Six runs at once: pushes race, and the losers retry on top. Runs 2k and 2k+1 are two jobs on
  // one host (one PERF_HISTORY_CLONE): each job has its own clone under it, so no lock is needed.
  const runs = Array.from({ length: 6 }, (_, i) => {
    const f = results(tmp(), metric(100 + i));
    const env = envFor(bare, path.join(root, `host-${i >> 1}`));
    const job = i % 2 ? "k" : "j";
    return new Promise((resolve) => {
      const p = spawn(process.execPath, [PERF, "record", "--job", job, "--ad4m-sha", SHA, "--run-id", `run-${i}`, f], { env });
      let err = "";
      p.stderr.on("data", (d) => (err += d));
      p.on("exit", (code) => resolve({ code, err }));
    });
  });
  for (const r of await Promise.all(runs)) assert.equal(r.code, 0, r.err);
  const ids = (job) => jsonl(show(bare, `nightly/${job}.jsonl`)).map((l) => l.run_id).sort();
  assert.deepEqual(ids("j"), ["run-0", "run-2", "run-4"]);
  assert.deepEqual(ids("k"), ["run-1", "run-3", "run-5"]);
  assert.equal(Number(git(bare, "rev-list", "--count", "main")), 7);
});

test("check reads the fetched history: runs another runner pushed count", () => {
  const { root, bare } = bareRepo();
  const other = envFor(bare, path.join(root, "clone-other"));
  for (const [i, v] of [100, 100, 100].entries()) assert.equal(record(other, `run-${i}`, v, i + 1).code, 0);
  const env = envFor(bare, path.join(root, "clone"));
  assert.equal(record(env, "run-3", 200, 4).code, 0);
  const r = perf(["check", "--job", "j", "--run-id", "run-3"], env);
  assert.equal(r.code, 1, r.out + r.err);
  assert.match(r.out, /REGRESSION j m\.latency_ms: 200 ms/);
  const verdicts = jsonl(show(bare, "nightly/j.checks.jsonl"));
  assert.deepEqual(verdicts.map((v) => [v.run_id, v.verdict]), [["run-3", "regressed"]]);
  // --accept writes to the history too, and the next run's baseline includes run-3.
  assert.equal(perf(["check", "--job", "j", "--accept", "run-3"], env).code, 0);
  assert.equal(jsonl(show(bare, "nightly/j.checks.jsonl")).at(-1).verdict, "accepted");
});

test("an unreachable history: record keeps the run in the artifacts, warns, exits 0; check reports only", () => {
  const root = tmp();
  const out = path.join(root, "artifacts");
  const env = envFor(path.join(root, "missing.git"), path.join(root, "clone"), { PERF_ARTIFACTS_DIR: out });
  const r = record(env, "run-1", 100, 1);
  assert.equal(r.code, 0, r.err);
  assert.match(r.err, /perf record j: history not pushed: fetch .*missing\.git/);
  assert.match(r.out, /-> .*artifacts\/records\.jsonl/);
  assert.equal(jsonl(fs.readFileSync(path.join(out, "records.jsonl"), "utf8")).length, 1);
  const c = perf(["check", "--job", "j", "--run-id", "run-1"], env);
  assert.equal(c.code, 0, c.out + c.err);
  assert.match(c.err, /history not read .*: report only/);
  assert.match(c.out, /perf check j: 1 metrics, 0 regressed, 1 report-only .* - OK/);
  const rep = perf(["report", "--job", "j", "--out", path.join(out, "trend.html")], env);
  assert.equal(rep.code, 0, rep.err);
  assert.match(fs.readFileSync(path.join(out, "trend.html"), "utf8"), /m\.latency_ms/);
});

test("the default repository is read-only outside CI: no push is tried", () => {
  const root = tmp();
  const store = openStore("j", { ...BASE_ENV, PERF_HISTORY_CLONE: path.join(root, "clone") });
  assert.equal(store.writable, false);
  assert.match(store.label, new RegExp(DEFAULT_REPO.replace(/[.]/g, "\\.")));
  const w = store.append("j.jsonl", [{ a: 1 }], "m");
  assert.equal(w.ok, false);
  assert.match(w.reason, /written only from CI/);
  assert.equal(fs.existsSync(path.join(root, "clone")), false, "nothing cloned, nothing fetched");
  assert.equal(openStore("j", { ...BASE_ENV, CI: "true" }).writable, true);
});

test("a line left by a failed write is dropped: the branch is the history", () => {
  const { root, bare } = bareRepo();
  const clone = path.join(root, "clone");
  const env = envFor(bare, clone);
  assert.equal(record(env, "run-1", 100, 1).code, 0);
  // Each job has its own clone, under PERF_HISTORY_CLONE/<job>.
  fs.appendFileSync(path.join(clone, "j", "nightly", "j.jsonl"), '{"stray":true}\n');
  fs.writeFileSync(path.join(clone, "j", "nightly", "other.jsonl"), '{"stray":true}\n');
  assert.equal(record(env, "run-2", 100, 2).code, 0);
  assert.deepEqual(jsonl(show(bare, "nightly/j.jsonl")).map((l) => l.run_id), ["run-1", "run-2"]);
  assert.equal(show(bare, "nightly/other.jsonl"), null);
});
