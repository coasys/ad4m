#!/usr/bin/env node
// Performance history of the nightly jobs: record each run's numbers, fail on a serious
// regression against the job's own history, and draw the trend. Run through the wrappers
// beside this file: scripts/perf/record, check and report. Node 18 or newer, no packages.
//
// ---- results JSON (what a test writes; the input of record) --------------------------------
//
//   {"metrics": [{"metric": "feed.newest_page.median", "value": 2.4, "unit": "ms",
//                 "better": "lower", "params": {"messages": 1000000}}]}
//
//   `metric`: dotted name of [A-Za-z0-9_-] segments, the same in every run. `better`: "lower"
//   or "higher". `params` (optional): strings, numbers or booleans; values with other params
//   never compare. Limits are not in the results: they are in scripts/perf/thresholds.json.
//
// ---- the history (store.mjs) ---------------------------------------------------------------
//
//   nightly/<job>.jsonl on branch main of coasys/ad4m-wind-tunnel, one line per metric per run:
//
//     {"job":"llm-e2e","metric":"llm.rust.latency_ms.median","value":4210,"unit":"ms",
//      "better":"lower","ad4m_sha":"<40 hex>","we_sha":null,"run_at":"2026-11-17T03:12:00Z",
//      "pipeline":10620,"workflow_id":"<uuid>","job_number":73990,"host":"<runner>",
//      "params":{"model":"qwen3.8-27b-48k"},"run_id":"circleci-73990","status":"passed"}
//
//   nightly/<job>.checks.jsonl, one line per check of a run:
//
//     {"job":"llm-e2e","run_id":"circleci-73990","verdict":"ok|regressed|report-only|accepted",
//      "regressions":[{"metric","params","value","median","reasons"}],"checked_at":"<ISO>"}
//
//   Environment: PERF_HISTORY_REPO (default git@github.com:coasys/ad4m-wind-tunnel.git),
//   PERF_HISTORY_BRANCH (main), PERF_HISTORY_SUBDIR (nightly), PERF_HISTORY_CLONE (the base of
//   the per-job clones, default ~/.cache/ad4m-ci/perf-history), PERF_HISTORY_SSH_KEY. The default
//   repository is written only when CI=true. PERF_ARTIFACTS_DIR: where records.jsonl and
//   trend.html go. Run fields from CircleCI: CIRCLE_SHA1, CIRCLE_BUILD_NUM, CIRCLE_WORKFLOW_ID,
//   PERF_PIPELINE_NUMBER (perf_setup exports it), WE_SHA.
//
// ---- record ----------------------------------------------------------------------------------
//
//   scripts/perf/record --job JOB [--status passed|failed] [--ad4m-sha SHA] [--we-sha SHA]
//                       [--run-id ID] [--run-at ISO] RESULTS...
//
//   RESULTS: results JSON files, or directories (every *.json directly inside). Writes one line
//   per metric to $PERF_ARTIFACTS_DIR/records.jsonl, then appends them to the history, commits
//   "perf(<job>): <ad4m sha[:9]> <run_at>" and pushes. When the history cannot be written (no
//   key, no network, not CI) it prints "perf record <job>: history not pushed: <reason>" and
//   still exits 0. Defaults: --status passed; --ad4m-sha $CIRCLE_SHA1, else git HEAD; --we-sha
//   $WE_SHA, else null; --run-id $PERF_RUN_ID, else circleci-$CIRCLE_BUILD_NUM, else
//   local-<time>; --run-at now (--run-id and --run-at: to import older runs).
//   Exit 2: a usage or input error, or no metrics in a passed run (a failed run with no
//   metrics writes nothing and exits 0).
//
// ---- check -----------------------------------------------------------------------------------
//
//   scripts/perf/check --job JOB [--run-id ID]
//   scripts/perf/check --job JOB --accept RUN_ID
//
//   Fetches the history and judges every record of the run (default: the run id record would
//   use, else the newest run) against the median of the same job + metric + params over the
//   last 7 passing runs before it. The run's records come from the history, else from
//   $PERF_ARTIFACTS_DIR/records.jsonl, so a run whose push failed is still checked. Serious
//   regression: worse than factor x that median and at least min_delta worse; or past a
//   ceiling (lower is better) or floor (higher is better), whatever the history. factor
//   (default 1.5), min_delta, ceiling and floor are per job and metric in thresholds.json.
//   Fewer than 3 earlier passing runs, or no history: the factor rule reports only.
//   Prints a line per regression, then one summary line:
//     perf check <job>: <n> metrics, <r> regressed, <k> report-only (<3 earlier runs) - OK|REGRESSION (...)
//   Appends the verdict to <job>.checks.jsonl (a failed write only warns). A run found
//   regressed is left out of later baselines until `--accept RUN_ID` takes its numbers as the
//   new normal. Exit: 0 no serious regression, 1 a serious regression, 2 usage or input error.
//
// ---- report ----------------------------------------------------------------------------------
//
//   scripts/perf/report --job JOB [--job JOB]... [--out FILE]
//
//   A static HTML page: one chart per job + metric + params over the last 90 runs, each point
//   linked to its ad4m commit. Default out: $PERF_ARTIFACTS_DIR/trend.html, else
//   ./perf-trend.html. This run's records in the artifacts are drawn too.

import { execFileSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

import {
  HERE,
  UsageError,
  appendLines,
  checkJob,
  evaluateRun,
  fmt,
  formatParams,
  latestRunId,
  loadThresholds,
  makeRecord,
  parseResults,
  readLines,
  seriesKey,
} from "./lib.mjs";
import { renderReport } from "./report.mjs";
import { openStore } from "./store.mjs";

const SHA_RE = /^[0-9a-f]{40}$/;

/** argv → {one(name), all(name), positional}. Every flag takes a value. */
function parseArgs(argv, known) {
  const flags = new Map();
  const positional = [];
  for (let i = 0; i < argv.length; i++) {
    if (!argv[i].startsWith("--")) {
      positional.push(argv[i]);
      continue;
    }
    const name = argv[i].slice(2);
    if (!known.includes(name)) throw new UsageError(`unknown option --${name}`);
    if (i + 1 >= argv.length) throw new UsageError(`--${name} needs a value`);
    flags.set(name, [...(flags.get(name) ?? []), argv[++i]]);
  }
  return { positional, one: (n) => flags.get(n)?.at(-1), all: (n) => flags.get(n) ?? [] };
}

const defaultRunId = (env) => env.PERF_RUN_ID || (env.CIRCLE_BUILD_NUM ? `circleci-${env.CIRCLE_BUILD_NUM}` : null);
const intOrNull = (v) => (v && Number.isInteger(Number(v)) ? Number(v) : null);
const artifactRecords = (env) => (env.PERF_ARTIFACTS_DIR ? readLines(path.join(env.PERF_ARTIFACTS_DIR, "records.jsonl")) : []);

/** The history's records plus those of `extra` whose run the history does not have. */
function withRunsOf(records, extra, job) {
  const known = new Set(records.map((r) => r.run_id));
  return records.concat(extra.filter((r) => r.job === job && !known.has(r.run_id)));
}

function gitHead() {
  try {
    return execFileSync("git", ["rev-parse", "HEAD"], { cwd: HERE, encoding: "utf8", stdio: ["ignore", "pipe", "ignore"] }).trim();
  } catch {
    return null;
  }
}

// ---- record ----------------------------------------------------------------------------------

export function cmdRecord(argv, env = process.env) {
  const a = parseArgs(argv, ["job", "status", "ad4m-sha", "we-sha", "run-id", "run-at"]);
  const job = checkJob(a.one("job"));
  const status = a.one("status") ?? "passed";
  if (status !== "passed" && status !== "failed") throw new UsageError("--status must be passed or failed");
  const metrics = [];
  for (const p of a.positional) {
    if (!fs.existsSync(p)) {
      if (status === "failed") continue;
      throw new UsageError(`${p} does not exist`);
    }
    const files = fs.statSync(p).isDirectory() ? fs.readdirSync(p).filter((f) => f.endsWith(".json")).sort().map((f) => path.join(p, f)) : [p];
    for (const f of files) metrics.push(...parseResults(fs.readFileSync(f, "utf8"), f));
  }
  const seen = new Set();
  for (const m of metrics) {
    if (seen.has(seriesKey(m))) throw new UsageError(`${m.metric} ${formatParams(m.params)} appears twice in this run`);
    seen.add(seriesKey(m));
  }
  if (metrics.length === 0) {
    if (status === "failed") {
      console.log(`perf record ${job}: failed run with no measurements; nothing recorded`);
      return 0;
    }
    throw new UsageError("no metrics: name results files or directories");
  }
  const ad4mSha = a.one("ad4m-sha") ?? env.CIRCLE_SHA1 ?? gitHead();
  const weSha = a.one("we-sha") ?? env.WE_SHA ?? null;
  if (!SHA_RE.test(ad4mSha ?? "")) throw new UsageError(`--ad4m-sha '${ad4mSha}' is not a 40-hex commit`);
  if (weSha !== null && !SHA_RE.test(weSha)) throw new UsageError(`--we-sha '${weSha}' is not a 40-hex commit`);
  const runAt = a.one("run-at") ?? new Date().toISOString().replace(/\.\d+Z$/, "Z");
  if (Number.isNaN(Date.parse(runAt))) throw new UsageError(`--run-at '${runAt}' is not a date`);
  const run = {
    ad4m_sha: ad4mSha,
    we_sha: weSha,
    run_at: runAt,
    pipeline: intOrNull(env.PERF_PIPELINE_NUMBER),
    workflow_id: env.CIRCLE_WORKFLOW_ID || null,
    job_number: intOrNull(env.CIRCLE_BUILD_NUM),
    host: os.hostname(),
    run_id: a.one("run-id") ?? defaultRunId(env) ?? `local-${runAt.replace(/[-:]/g, "")}`,
    status,
  };
  const records = metrics.map((m) => makeRecord(job, m, run));
  // The artifacts first: they keep the run when the history cannot be written.
  const out = env.PERF_ARTIFACTS_DIR ? path.join(env.PERF_ARTIFACTS_DIR, "records.jsonl") : null;
  if (out) appendLines(out, records);
  const w = openStore(job, env).append(`${job}.jsonl`, records, `perf(${job}): ${ad4mSha.slice(0, 9)} ${runAt}`);
  console.log(`perf record ${job}: ${records.length} metrics, run ${run.run_id} (${status}, ad4m ${ad4mSha.slice(0, 9)}) -> ${w.ok ? w.where : (out ?? "nowhere")}`);
  if (!w.ok) console.error(`perf record ${job}: history not pushed: ${w.reason}`);
  return 0;
}

// ---- check -----------------------------------------------------------------------------------

export function cmdCheck(argv, env = process.env) {
  const a = parseArgs(argv, ["job", "run-id", "accept"]);
  const job = checkJob(a.one("job"));
  const store = openStore(job, env);
  const h = store.read();
  if (!h.ok) console.error(`perf check ${job}: history not read (${h.reason}): report only`);
  const history = h.ok ? readLines(path.join(h.dir, `${job}.jsonl`)) : [];
  const checkLines = h.ok ? readLines(path.join(h.dir, `${job}.checks.jsonl`)) : [];
  const verdict = (run_id, v, regressions) => store.append(`${job}.checks.jsonl`, [{ job, run_id, verdict: v, regressions, checked_at: new Date().toISOString() }], `perf(${job}): check ${run_id} ${v}`);

  const accept = a.one("accept");
  if (accept !== undefined) {
    if (!history.some((r) => r.run_id === accept)) throw new UsageError(`no records of run '${accept}' in the ${job} history`);
    const w = verdict(accept, "accepted", []);
    if (!w.ok) throw new UsageError(`verdict not written: ${w.reason}`);
    console.log(`perf check ${job}: run ${accept} accepted; later baselines include it`);
    return 0;
  }

  const records = withRunsOf(history, artifactRecords(env), job);
  const runId = a.one("run-id") ?? defaultRunId(env) ?? latestRunId(records);
  if (!runId) throw new UsageError(`no history for ${job} in ${store.label}`);
  // Without history every series is report-only, so only a ceiling or a floor can fail.
  const { own, results } = evaluateRun(records, checkLines, runId, loadThresholds());
  const regressed = results.filter((r) => r.regressed);
  const reportOnly = results.filter((r) => r.reportOnly);
  for (const { record: r, reasons } of regressed) {
    console.log(`REGRESSION ${job} ${r.metric}${formatParams(r.params) ? " " + formatParams(r.params) : ""}: ${fmt(r.value)} ${r.unit} (${r.better} is better) ${reasons.join("; ")}`);
  }
  const v = regressed.length ? "regressed" : reportOnly.length === results.length ? "report-only" : "ok";
  if (h.ok) {
    const w = verdict(runId, v, regressed.map(({ record: r, median, reasons }) => ({ metric: r.metric, params: r.params, value: r.value, median, reasons })));
    if (!w.ok) console.error(`perf check ${job}: verdict not pushed: ${w.reason}`);
  }
  const tail = `run ${runId}, ad4m ${String(own[0].ad4m_sha).slice(0, 9)}${own[0].status === "failed" ? ", run failed" : ""}`;
  console.log(`perf check ${job}: ${results.length} metrics, ${regressed.length} regressed, ${reportOnly.length} report-only (<3 earlier runs) - ${regressed.length ? "REGRESSION" : "OK"} (${tail})`);
  return regressed.length ? 1 : 0;
}

// ---- report ----------------------------------------------------------------------------------

export function cmdReport(argv, env = process.env) {
  const a = parseArgs(argv, ["job", "out"]);
  const jobs = a.all("job").map(checkJob);
  if (jobs.length === 0) throw new UsageError("--job is required");
  // Every job's file is on the branch: the first job's clone has them all.
  const h = openStore(jobs[0], env).read();
  if (!h.ok) console.error(`perf report: history not fetched (${h.reason}); drawing what is at hand`);
  const extra = artifactRecords(env);
  const data = jobs.map((job) => ({
    job,
    records: withRunsOf(readLines(path.join(h.dir, `${job}.jsonl`)), extra, job),
    checks: readLines(path.join(h.dir, `${job}.checks.jsonl`)),
  }));
  const out = a.one("out") ?? (env.PERF_ARTIFACTS_DIR ? path.join(env.PERF_ARTIFACTS_DIR, "trend.html") : "perf-trend.html");
  fs.mkdirSync(path.dirname(path.resolve(out)), { recursive: true });
  fs.writeFileSync(out, renderReport(data));
  const series = data.reduce((n, d) => n + new Set(d.records.map(seriesKey)).size, 0);
  console.log(`perf report: ${jobs.length} jobs, ${series} series -> ${out}`);
  return 0;
}

// ---- main ------------------------------------------------------------------------------------

const COMMANDS = { record: cmdRecord, check: cmdCheck, report: cmdReport };

export function main(argv, env = process.env) {
  const [cmd, ...rest] = argv;
  if (!COMMANDS[cmd]) {
    console.error("usage: perf.mjs record|check|report [options] (see the header of scripts/perf/perf.mjs)");
    return 2;
  }
  try {
    return COMMANDS[cmd](rest, env);
  } catch (e) {
    console.error(`perf ${cmd}: ${e instanceof UsageError ? e.message : (e.stack ?? e)}`);
    return 2;
  }
}

if (process.argv[1]?.endsWith("/perf.mjs")) process.exitCode = main(process.argv.slice(2));
