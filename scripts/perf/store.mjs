// Where the performance history lives: the folder $PERF_HISTORY_SUBDIR (default `nightly`) on
// branch $PERF_HISTORY_BRANCH (default `main`) of the git repository $PERF_HISTORY_REPO (default
// git@github.com:coasys/ad4m-wind-tunnel.git): `<job>.jsonl` (the records) and
// `<job>.checks.jsonl` (check's verdicts), append-only.
//
// Each job works in its own cached clone, $PERF_HISTORY_CLONE/<job> (default
// ~/.cache/ad4m-ci/perf-history/<job>), so two jobs never share a working tree, and the runs of
// one job do not overlap (each nightly job has its own serial group). No lock. Each read fetches
// the branch and resets the clone to it. Each write appends, commits "perf(<job>): ..." and
// pushes; when another run pushed first, it fetches, appends again on the new head and pushes
// again, up to 5 times. $PERF_HISTORY_SSH_KEY: a private key file to push with (offered alone).
//
// The default repository is written only from CI ($CI=true), so a local run never pushes to
// it. Set PERF_HISTORY_REPO (a local bare repository, for a test) to write elsewhere.

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

import { appendLines, checkJob } from "./lib.mjs";

export const DEFAULT_REPO = "git@github.com:coasys/ad4m-wind-tunnel.git";
const PUSH_ATTEMPTS = 5;
// The committer of the history: no person, a reserved domain (RFC 2606).
const AUTHOR = ["-c", "user.name=ad4m nightly", "-c", "user.email=nightly@ad4m.invalid"];
const RETRY = /non-fast-forward|fetch first|incorrect old value|cannot lock ref|failed to update ref/i;

const sleepSync = (ms) => Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, ms);
const lastLine = (s) => s.split("\n").filter(Boolean).pop() ?? "unknown error";

/** The history as job `job` sees it: {label, writable, read(), append(file, lines, message)}. */
export function openStore(job, env = process.env) {
  const repo = env.PERF_HISTORY_REPO || DEFAULT_REPO;
  const branch = env.PERF_HISTORY_BRANCH || "main";
  const subdir = env.PERF_HISTORY_SUBDIR ?? "nightly";
  const clone = path.join(env.PERF_HISTORY_CLONE || path.join(os.homedir(), ".cache", "ad4m-ci", "perf-history"), checkJob(job));
  const writable = env.CI === "true" || Boolean(env.PERF_HISTORY_REPO);
  const dir = path.join(clone, subdir);
  // BatchMode: no prompt for a passphrase or a host key; fail instead.
  const ssh = ["ssh", "-o", "BatchMode=yes", ...(env.PERF_HISTORY_SSH_KEY ? ["-i", env.PERF_HISTORY_SSH_KEY, "-o", "IdentitiesOnly=yes"] : [])];
  const gitEnv = { ...env, GIT_TERMINAL_PROMPT: "0", GIT_SSH_COMMAND: ssh.join(" ") };

  const git = (...args) => {
    const r = spawnSync("git", ["-C", clone, ...args], { env: gitEnv, encoding: "utf8", timeout: 120_000 });
    return { ok: r.status === 0, err: r.error ? r.error.message : (r.stderr || r.stdout || "").trim() };
  };

  /** Fetch the branch and reset the clone to it (a branch not there yet: an empty history). */
  const sync = () => {
    fs.mkdirSync(clone, { recursive: true });
    if (!fs.existsSync(path.join(clone, ".git")) && !git("init", "-q", "-b", branch).ok) return { ok: false, reason: `git init in ${clone} failed` };
    if (!git("remote", "set-url", "origin", repo).ok) git("remote", "add", "origin", repo);
    const f = git("fetch", "-q", "origin", `+refs/heads/${branch}:refs/remotes/origin/${branch}`);
    if (!f.ok && !/couldn't find remote ref/i.test(f.err)) return { ok: false, reason: `fetch ${repo}: ${lastLine(f.err)}` };
    if (f.ok) {
      const c = git("checkout", "-q", "-f", "-B", branch, `origin/${branch}`);
      if (!c.ok) return { ok: false, reason: `checkout: ${lastLine(c.err)}` };
    } else {
      // A repository without the branch: start it, with no commit and no files.
      git("symbolic-ref", "HEAD", `refs/heads/${branch}`);
      git("update-ref", "-d", `refs/heads/${branch}`);
      git("rm", "-rq", "--cached", "--ignore-unmatch", ".");
    }
    // Lines a failed write left behind: the branch is the history.
    git("clean", "-fdqx");
    return { ok: true };
  };

  return {
    label: `${repo} ${branch}:${subdir || "."}`,
    writable,
    /** {ok, dir, reason}: `dir` holds the `<job>.jsonl` files (what the clone has, when not ok). */
    read: () => ({ ...sync(), dir }),
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
  };
}
