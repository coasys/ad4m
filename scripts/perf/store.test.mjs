// Tests of the git history (store.mjs) through record, check and report, against local bare
// repositories in a temp directory. Never the real history repository: every command gets
// PERF_HISTORY_REPO, and CI is unset.
//   node --test 'scripts/perf/*.test.mjs'   (tests/js: pnpm run test-perf-unit)

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { after, test } from "node:test";

import { cleanGitEnv } from "./lib.mjs";
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

const GIT_ENV = cleanGitEnv(process.env);

const git = (cwd, ...args) => {
  const r = spawnSync("git", ["-C", cwd, ...args], { encoding: "utf8", env: GIT_ENV });
  assert.equal(r.status, 0, `git ${args.join(" ")}: ${r.stderr}`);
  return r.stdout.trim();
};

/** A bare repository with branch main holding one README commit (as the real one has). */
function bareRepo() {
  const root = tmp();
  const bare = path.join(root, "history.git");
  spawnSync("git", ["init", "-q", "--bare", "-b", "main", bare], { env: GIT_ENV });
  const seed = path.join(root, "seed");
  spawnSync("git", ["clone", "-q", bare, seed], { env: GIT_ENV });
  fs.writeFileSync(path.join(seed, "README.md"), "wind tunnel\n");
  git(seed, "add", "README.md");
  git(seed, "-c", "user.name=t", "-c", "user.email=t@t.invalid", "commit", "-q", "-m", "init");
  git(seed, "push", "-q", "origin", "HEAD:main");
  return { root, bare };
}

const show = (bare, file) => {
  const r = spawnSync("git", ["-C", bare, "show", `main:${file}`], { encoding: "utf8", env: GIT_ENV });
  return r.status === 0 ? r.stdout : null;
};
const jsonl = (text) => (text ?? "").split("\n").filter(Boolean).map((l) => JSON.parse(l));

/** The env of a command on a host whose temp directory is `tmpdir` (where its clones go). */
function envFor(bare, tmpdir, extra = {}) {
  fs.mkdirSync(tmpdir, { recursive: true });
  return { ...BASE_ENV, PERF_HISTORY_REPO: bare, TMPDIR: tmpdir, ...extra };
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

test("a caller's GIT_DIR and GIT_COMMON_DIR never reach the store's git: the caller's repository stays as it was", () => {
  const { root, bare } = bareRepo();
  // The caller: a repository a CI step (or a hook) would export, as a worktree's git does.
  const caller = path.join(root, "caller");
  spawnSync("git", ["init", "-q", caller], { env: GIT_ENV });
  const callerGit = path.join(caller, ".git");
  const config = () => fs.readFileSync(path.join(callerGit, "config"), "utf8");
  const before = config();
  const env = envFor(bare, path.join(root, "clone"), { GIT_DIR: callerGit, GIT_COMMON_DIR: callerGit, GIT_WORK_TREE: caller });
  const r = record(env, "run-1", 100, 1);
  assert.equal(r.code, 0, r.err);
  assert.equal(jsonl(show(bare, "nightly/j.jsonl")).length, 1, "the history got the run");
  assert.equal(config(), before, "no remote or setting was added to the caller's repository");
  assert.equal(git(caller, "rev-list", "--all", "--count"), "0", "and no commit");
});

test("the branch and subdir come from PERF_HISTORY_BRANCH and PERF_HISTORY_SUBDIR; a new branch starts empty", () => {
  const { root, bare } = bareRepo();
  const env = envFor(bare, path.join(root, "clone"), { PERF_HISTORY_BRANCH: "perf", PERF_HISTORY_SUBDIR: "runs" });
  assert.equal(record(env, "run-1", 100, 1).code, 0);
  const r = spawnSync("git", ["-C", bare, "show", "perf:runs/j.jsonl"], { encoding: "utf8", env: GIT_ENV });
  assert.equal(jsonl(r.stdout).length, 1);
  assert.equal(spawnSync("git", ["-C", bare, "show", "perf:README.md"], { env: GIT_ENV }).status, 128, "the new branch has only the history");
});

test("a run that loses the push race fetches the winner's head and pushes on top; every line lands once", () => {
  const { root, bare } = bareRepo();
  // The race, made to happen: a `git` first in PATH that, at j's first push, runs a whole record
  // of job k (on the same host) to the end before letting j's push go. That push is then
  // rejected (k moved main), and j must retry on top of k's commit.
  const bin = path.join(root, "bin");
  fs.mkdirSync(bin);
  const log = path.join(root, "pushes.log");
  const mark = path.join(root, "raced");
  const k = results(tmp(), metric(200));
  const realGit = spawnSync("sh", ["-c", "command -v git"], { encoding: "utf8" }).stdout.trim();
  const other = [process.execPath, PERF, "record", "--job", "k", "--ad4m-sha", SHA, "--run-id", "run-k", k].map((a) => `'${a}'`).join(" ");
  fs.writeFileSync(
    path.join(bin, "git"),
    [
      "#!/bin/sh",
      'case " $* " in *" push "*)',
      `  echo push >> '${log}'`,
      `  if mkdir '${mark}' 2>/dev/null; then PATH='${process.env.PATH}' ${other} >&2 || exit 9; fi;;`,
      "esac",
      `exec '${realGit}' "$@"`,
      "",
    ].join("\n"),
    { mode: 0o755 },
  );
  const env = envFor(bare, path.join(root, "host"), { PATH: `${bin}:${process.env.PATH}` });
  const r = record(env, "run-j", 100, 1);
  assert.equal(r.code, 0, r.err);
  assert.doesNotMatch(r.err, /not pushed/, r.err);
  assert.ok(fs.existsSync(mark), "k ran inside j's first push");
  assert.ok(fs.readFileSync(log, "utf8").split("\n").filter(Boolean).length >= 2, "j's first push was rejected and retried");
  assert.deepEqual(jsonl(show(bare, "nightly/j.jsonl")).map((l) => l.run_id), ["run-j"]);
  assert.deepEqual(jsonl(show(bare, "nightly/k.jsonl")).map((l) => l.run_id), ["run-k"]);
  // The seed, k's commit, then j's on top: nothing lost, nothing doubled.
  assert.deepEqual(git(bare, "log", "--format=%s", "main").split("\n").map((s) => s.split(" ")[0]), ["perf(j):", "perf(k):", "init"]);
});

test("each command clones into a fresh temp directory and removes it; git runs no hook", () => {
  const { root, bare } = bareRepo();
  const host = path.join(root, "host");
  // A hook another job on the runner could set up for every repository of the user.
  const hooks = path.join(root, "hooks");
  const ran = path.join(root, "hook-ran");
  fs.mkdirSync(hooks);
  for (const h of ["pre-commit", "post-commit", "pre-push", "post-checkout"]) {
    fs.writeFileSync(path.join(hooks, h), `#!/bin/sh\ntouch '${ran}'\nexit 1\n`, { mode: 0o755 });
  }
  const globalConfig = path.join(root, "gitconfig");
  fs.writeFileSync(globalConfig, `[core]\n\thooksPath = ${hooks}\n`);
  const env = envFor(bare, host, { GIT_CONFIG_GLOBAL: globalConfig });
  const left = () => fs.readdirSync(host).filter((d) => d.startsWith("ad4m-perf-history-"));
  for (const [i, v] of [100, 100, 100, 100].entries()) {
    const r = record(env, `run-${i}`, v, i + 1);
    assert.equal(r.code, 0, r.err);
    assert.doesNotMatch(r.err, /not pushed/, r.err);
  }
  assert.equal(perf(["check", "--job", "j", "--run-id", "run-3"], env).code, 0);
  assert.equal(perf(["report", "--job", "j", "--out", path.join(root, "trend.html")], env).code, 0);
  assert.deepEqual(left(), [], "no clone left behind");
  assert.equal(fs.existsSync(ran), false, "no hook ran");
  assert.equal(jsonl(show(bare, "nightly/j.jsonl")).length, 4);
  assert.equal(jsonl(show(bare, "nightly/j.checks.jsonl")).length, 1);
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

test("git reaches the history with its own ssh command: the key alone, no ~/.ssh/config, GIT_SSH_COMMAND ignored", () => {
  const root = tmp();
  const bin = path.join(root, "bin");
  fs.mkdirSync(bin);
  const log = path.join(root, "ssh.log");
  // An ssh that only logs how it was called, then fails as an unreachable host would.
  fs.writeFileSync(path.join(bin, "ssh"), `#!/bin/sh\necho "$@" >> '${log}'\nexit 255\n`, { mode: 0o755 });
  const key = path.join(root, "key with space");
  const env = envFor("ssh://git@history.invalid/h.git", path.join(root, "host"), {
    PATH: `${bin}:${process.env.PATH}`,
    PERF_HISTORY_SSH_KEY: key,
    GIT_SSH_COMMAND: "/bin/false never-used",
  });
  const r = record(env, "run-1", 100, 1);
  assert.equal(r.code, 0, r.err);
  assert.match(r.err, /history not pushed: fetch/);
  const call = fs.readFileSync(log, "utf8").split("\n")[0];
  assert.match(call, /^-F \/dev\/null -o BatchMode=yes -i .*key with space -o IdentitiesOnly=yes .*history\.invalid/);
  assert.deepEqual(fs.readdirSync(path.join(root, "host")), [], "the clone is removed after a failed fetch too");
});

test("the default repository is read-only outside CI: no push is tried", () => {
  const host = tmp();
  const store = openStore("j", { ...BASE_ENV, TMPDIR: host });
  assert.equal(store.writable, false);
  assert.match(store.label, new RegExp(DEFAULT_REPO.replace(/[.]/g, "\\.")));
  const w = store.append("j.jsonl", [{ a: 1 }], "m");
  assert.equal(w.ok, false);
  assert.match(w.reason, /written only from CI/);
  store.close();
  assert.deepEqual(fs.readdirSync(host), [], "nothing cloned, nothing fetched");
  assert.equal(openStore("j", { ...BASE_ENV, CI: "true" }).writable, true);
});
