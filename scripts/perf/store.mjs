// Where the performance history lives: the folder $PERF_HISTORY_SUBDIR (default `nightly`) on
// branch $PERF_HISTORY_BRANCH (default `main`) of the git repository $PERF_HISTORY_REPO (default
// git@github.com:coasys/ad4m-wind-tunnel.git): `<job>.jsonl` (the records) and
// `<job>.checks.jsonl` (check's verdicts), append-only.
//
// Each command works in a clone of its own: a fresh temp directory
// ($TMPDIR/ad4m-perf-history-<job>-XXXXXX) holding a shallow fetch of the branch (its last
// commit only), removed by close(). Nothing is kept between runs: the runner also runs other
// jobs, which must not leave a hook, a config or a stray line for the next run to pick up. No
// lock: nothing is shared. Each write appends, commits "perf(<job>): ..." and pushes; when
// another run pushed first, it fetches again, appends on the new head and pushes again, up to
// 5 times.
//
// git runs with hooks off (core.hooksPath=/dev/null) and its own ssh command (core.sshCommand:
// no ~/.ssh/config, BatchMode). $PERF_HISTORY_SSH_KEY: a private key file to push with
// (offered alone).
//
// The default repository is written only from CI ($CI=true), so a local run never pushes to
// it. Set PERF_HISTORY_REPO (a local bare repository, for a test) to write elsewhere.

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

import { appendLines, checkJob, cleanGitEnv } from "./lib.mjs";

export const DEFAULT_REPO = "git@github.com:coasys/ad4m-wind-tunnel.git";
const PUSH_ATTEMPTS = 5;
// The committer of the history: no person, a reserved domain (RFC 2606).
const AUTHOR = ["-c", "user.name=ad4m nightly", "-c", "user.email=nightly@ad4m.invalid"];
const RETRY = /non-fast-forward|fetch first|incorrect old value|cannot lock ref|failed to update ref/i;

const sleepSync = (ms) => Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, ms);
const lastLine = (s) => s.split("\n").filter(Boolean).pop() ?? "unknown error";
const shellQuote = (s) => `'${s.replace(/'/g, `'\\''`)}'`;

/**
 * The history as job `job` sees it: {label, writable, read(), append(file, lines, message),
 * close()}. Call close() when done: it removes the clone.
 */
export function openStore(job, env = process.env) {
  const repo = env.PERF_HISTORY_REPO || DEFAULT_REPO;
  const branch = env.PERF_HISTORY_BRANCH || "main";
  const subdir = env.PERF_HISTORY_SUBDIR ?? "nightly";
  const prefix = `ad4m-perf-history-${checkJob(job)}-`;
  const writable = env.CI === "true" || Boolean(env.PERF_HISTORY_REPO);
  // -F /dev/null: no ~/.ssh/config. BatchMode: no prompt for a passphrase or a host key.
  const ssh = ["ssh", "-F", "/dev/null", "-o", "BatchMode=yes", ...(env.PERF_HISTORY_SSH_KEY ? ["-i", shellQuote(env.PERF_HISTORY_SSH_KEY), "-o", "IdentitiesOnly=yes"] : [])];
  const config = ["-c", "core.hooksPath=/dev/null", "-c", `core.sshCommand=${ssh.join(" ")}`];
  // GIT_SSH_COMMAND and GIT_SSH would win over core.sshCommand; GIT_DIR & friends would point
  // git at the checkout that set them (cleanGitEnv).
  const { GIT_SSH_COMMAND, GIT_SSH, ...rest } = cleanGitEnv(env);
  const gitEnv = { ...rest, GIT_TERMINAL_PROMPT: "0" };
  // Made at the first read or write: a store that is never synced leaves nothing behind.
  let clone = null;

  const git = (...args) => {
    const r = spawnSync("git", ["-C", clone, ...config, ...args], { env: gitEnv, encoding: "utf8", timeout: 120_000 });
    return { ok: r.status === 0, err: r.error ? r.error.message : (r.stderr || r.stdout || "").trim() };
  };

  /** Fetch the branch's head and check it out (a branch not there yet: an empty history). */
  const sync = () => {
    if (!clone) {
      clone = fs.mkdtempSync(path.join(env.TMPDIR || os.tmpdir(), prefix));
      if (!git("init", "-q", "-b", branch).ok) return { ok: false, reason: `git init in ${clone} failed` };
      git("remote", "add", "origin", repo);
    }
    const f = git("fetch", "-q", "--depth", "1", "origin", `+refs/heads/${branch}:refs/remotes/origin/${branch}`);
    if (!f.ok && !/couldn't find remote ref/i.test(f.err)) return { ok: false, reason: `fetch ${repo}: ${lastLine(f.err)}` };
    if (f.ok) {
      const c = git("checkout", "-q", "-f", "-B", branch, `origin/${branch}`);
      if (!c.ok) return { ok: false, reason: `checkout: ${lastLine(c.err)}` };
    } else {
      // A repository without the branch: start it, with no commit and no files.
      git("symbolic-ref", "HEAD", `refs/heads/${branch}`);
      git("update-ref", "-d", `refs/heads/${branch}`);
      git("rm", "-rq", "--cached", "--ignore-unmatch", ".");
      git("clean", "-fdqx");
    }
    return { ok: true };
  };

  return {
    label: `${repo} ${branch}:${subdir || "."}`,
    writable,
    /** {ok, dir, reason}: `dir` holds the `<job>.jsonl` files (null when nothing was fetched). */
    read() {
      const s = sync();
      return { ...s, dir: clone && path.join(clone, subdir) };
    },
    /** Append `lines` to `file` in the history, commit and push. {ok, where, reason}. */
    append(file, lines, message) {
      if (!writable) return { ok: false, reason: `${repo} is written only from CI (CI=true); set PERF_HISTORY_REPO for a local run` };
      const rel = path.posix.join(subdir, file);
      let reason = "";
      for (let attempt = 1; attempt <= PUSH_ATTEMPTS; attempt++) {
        const s = sync();
        if (!s.ok) return s;
        appendLines(path.join(clone, rel), lines);
        const commit = git("add", "--", rel).ok ? git(...AUTHOR, "commit", "-q", "-m", message) : { ok: false, err: "git add failed" };
        if (!commit.ok) return { ok: false, reason: `git commit: ${lastLine(commit.err)}` };
        const push = git("push", "-q", "origin", `HEAD:refs/heads/${branch}`);
        if (push.ok) return { ok: true, where: `${repo} ${branch}:${rel}` };
        reason = lastLine(push.err);
        // Another run pushed first, or at the same moment: take its head and append on top.
        if (!RETRY.test(push.err)) return { ok: false, reason: `push: ${reason}` };
        sleepSync(200 * attempt + Math.floor(Math.random() * 300));
      }
      return { ok: false, reason: `push still rejected after ${PUSH_ATTEMPTS} attempts: ${reason}` };
    },
    /** Remove the clone. */
    close() {
      if (clone) fs.rmSync(clone, { recursive: true, force: true });
      clone = null;
    },
  };
}
