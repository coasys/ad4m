// Shared parts of scripts/perf/{record,check,report}: the results JSON, the record line, the
// thresholds and the regression rule. The command line and the contract: the header of perf.mjs.

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

export const HERE = path.dirname(fileURLToPath(import.meta.url));
export const DEFAULT_FACTOR = 1.5;
/** How many earlier passing runs make the baseline (their median). */
export const WINDOW = 7;
/** Fewer earlier passing runs than this: the factor rule only reports. */
export const MIN_RUNS = 3;

export class UsageError extends Error {}

const JOB_RE = /^[a-z0-9][a-z0-9_-]*$/;
// Dotted segments of letters, digits, `_` and `-`: `sfu.t21.colocated_roster_ms`.
const METRIC_RE = /^[A-Za-z0-9_-]+(\.[A-Za-z0-9_-]+)*$/;

export function checkJob(job) {
  if (typeof job !== "string" || !JOB_RE.test(job)) {
    throw new UsageError(`job '${job}' is not a job name (lower case letters, digits, - and _)`);
  }
  return job;
}

/** A metric name made of free text (a test title): every run of other characters becomes `_`. */
export function slug(text) {
  return String(text).replace(/->/g, "_to_").replace(/[^A-Za-z0-9_-]+/g, "_").replace(/^_+|_+$/g, "").slice(0, 120) || "_";
}

/** The metrics of one results file: `{"metrics": [{metric, value, unit, better, params?}]}`. */
export function parseResults(text, source) {
  let doc;
  try {
    doc = JSON.parse(text);
  } catch (e) {
    throw new UsageError(`${source}: not JSON (${e.message})`);
  }
  if (!doc || !Array.isArray(doc.metrics)) throw new UsageError(`${source}: expected {"metrics": [...]}`);
  return doc.metrics.map((m, i) => {
    const where = `${source}: metrics[${i}] (${m?.metric})`;
    if (typeof m?.metric !== "string" || !METRIC_RE.test(m.metric)) throw new UsageError(`${where}: not a dotted name`);
    if (typeof m.value !== "number" || !Number.isFinite(m.value)) throw new UsageError(`${where}: value is not a finite number`);
    if (typeof m.unit !== "string" || m.unit === "") throw new UsageError(`${where}: unit missing`);
    if (m.better !== "lower" && m.better !== "higher") throw new UsageError(`${where}: better must be "lower" or "higher"`);
    const params = m.params ?? {};
    if (typeof params !== "object" || Array.isArray(params) || Object.values(params).some((v) => v !== null && typeof v === "object")) {
      throw new UsageError(`${where}: params must be an object of strings, numbers or booleans`);
    }
    return { metric: m.metric, value: m.value, unit: m.unit, better: m.better, params };
  });
}

/** Params with sorted keys, as one string: two values compare only when these are equal. */
export function paramsKey(params) {
  const p = params ?? {};
  return JSON.stringify(Object.fromEntries(Object.keys(p).sort().map((k) => [k, p[k]])));
}

export const seriesKey = (r) => `${r.metric}\u0000${paramsKey(r.params)}`;

export function formatParams(params) {
  const keys = Object.keys(params ?? {}).sort();
  return keys.length ? `{${keys.map((k) => `${k}=${params[k]}`).join(", ")}}` : "";
}

/** One history line: the spec's fields in this order. */
export function makeRecord(job, m, run) {
  return {
    job,
    metric: m.metric,
    value: m.value,
    unit: m.unit,
    better: m.better,
    ad4m_sha: run.ad4m_sha,
    we_sha: run.we_sha,
    run_at: run.run_at,
    pipeline: run.pipeline,
    workflow_id: run.workflow_id,
    job_number: run.job_number,
    host: run.host,
    params: m.params ?? {},
    run_id: run.run_id,
    status: run.status,
  };
}

/** Append objects to a JSON Lines file in one write. */
export function appendLines(file, objects) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  fs.appendFileSync(file, objects.map((o) => JSON.stringify(o) + "\n").join(""));
}

/** The objects of a JSON Lines file ([] when it is missing); a line that does not parse is skipped. */
export function readLines(file) {
  if (!file || !fs.existsSync(file)) return [];
  return fs
    .readFileSync(file, "utf8")
    .split("\n")
    .filter((l) => l.trim())
    .flatMap((l) => {
      try {
        return [JSON.parse(l)];
      } catch {
        console.error(`perf: ${file}: a line that is not JSON, skipped`);
        return [];
      }
    });
}

/**
 * `env` without the variables that would point a spawned git at another repository or index
 * (a CI step's own checkout exports them): every git call this tool makes gets this env.
 */
export function cleanGitEnv(env) {
  const out = { ...env };
  for (const k of ["GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_OBJECT_DIRECTORY"]) delete out[k];
  return out;
}

// ---- thresholds.json: the only place for per-metric limits ---------------------------------

const LIMIT_KEYS = ["factor", "min_delta", "ceiling", "floor"];
const globRe = (glob) => new RegExp(`^${glob.split("*").map((s) => s.replace(/[.+?^${}()|[\]\\]/g, "\\$&")).join(".*")}$`);

export function loadThresholds(file = path.join(HERE, "thresholds.json")) {
  return JSON.parse(fs.readFileSync(file, "utf8")).rules.map((r) => ({ ...r, jobRe: globRe(r.job ?? "*"), metricRe: globRe(r.metric ?? "*") }));
}

/** The limits of a record: the defaults, then every matching rule in order (later rules win). */
export function limitsFor(rec, rules) {
  const limits = { factor: DEFAULT_FACTOR, min_delta: 0 };
  for (const r of rules) {
    if (r.jobRe.test(rec.job) && r.metricRe.test(rec.metric)) for (const k of LIMIT_KEYS) if (r[k] !== undefined) limits[k] = r[k];
  }
  return limits;
}

// ---- the regression rule ---------------------------------------------------------------------

export function median(values) {
  if (values.length === 0) return null;
  const s = [...values].sort((a, b) => a - b);
  const mid = s.length >> 1;
  return s.length % 2 ? s[mid] : (s[mid - 1] + s[mid]) / 2;
}

export function fmt(n) {
  if (n === null || n === undefined || Number.isNaN(n)) return "-";
  const a = Math.abs(n);
  if (a >= 1000) return String(Math.round(n));
  if (a >= 10) return n.toFixed(1).replace(/\.0$/, "");
  return n.toFixed(2).replace(/\.?0+$/, "") || "0";
}

/** run_id → its last verdict (`ok`, `regressed`, `report-only`, `accepted`). An accept is final. */
export function verdicts(checkLines) {
  const out = new Map();
  for (const c of checkLines) if (c.run_id && out.get(c.run_id) !== "accepted") out.set(c.run_id, c.verdict);
  return out;
}

/**
 * The runs a baseline may use, oldest first: passed, not found regressed (unless accepted), and
 * not older than the newest accepted run. An accepted run resets the baseline: its numbers are
 * the new normal, so the runs before it no longer count.
 */
export function passingRuns(records, verdictOf) {
  const since = records.reduce((t, r) => (verdictOf.get(r.run_id) === "accepted" ? Math.max(t, Date.parse(r.run_at)) : t), -Infinity);
  const runs = new Map();
  for (const r of records) {
    if (r.status === "failed" || verdictOf.get(r.run_id) === "regressed" || Date.parse(r.run_at) < since) continue;
    if (!runs.has(r.run_id)) runs.set(r.run_id, { run_id: r.run_id, run_at: r.run_at, records: [] });
    runs.get(r.run_id).records.push(r);
  }
  return [...runs.values()].sort((a, b) => Date.parse(a.run_at) - Date.parse(b.run_at));
}

/**
 * Judge one record against `baseline` (its series' values in the last passing runs). Lower is
 * better: a regression is above `factor` × the median and at least `min_delta` above it, once
 * there are MIN_RUNS baseline values; or above `ceiling` at any history. Higher is better: the
 * mirror, with `floor`. A median of 0 takes no factor rule.
 */
export function judge(rec, baseline, limits, minRuns = MIN_RUNS) {
  const med = median(baseline);
  const out = { record: rec, median: med, reasons: [], reportOnly: baseline.length < minRuns };
  const lower = rec.better === "lower";
  if (lower && limits.ceiling !== undefined && rec.value > limits.ceiling) out.reasons.push(`above the ceiling ${fmt(limits.ceiling)} ${rec.unit}`);
  if (!lower && limits.floor !== undefined && rec.value < limits.floor) out.reasons.push(`below the floor ${fmt(limits.floor)} ${rec.unit}`);
  if (!out.reportOnly && med > 0) {
    const ratio = lower ? rec.value / med : med / Math.max(rec.value, Number.MIN_VALUE);
    const delta = lower ? rec.value - med : med - rec.value;
    if (ratio > limits.factor && delta >= limits.min_delta) {
      out.reasons.push(`${fmt(ratio)}x ${lower ? "above" : "below"} the median ${fmt(med)} ${rec.unit} of ${baseline.length} runs (limit ${limits.factor}x)`);
    }
  }
  out.regressed = out.reasons.length > 0;
  return out;
}

/** Judge every record of `runId` (one job's history). Throws UsageError when the run has none. */
export function evaluateRun(records, checkLines, runId, rules, { window = WINDOW, minRuns = MIN_RUNS } = {}) {
  const own = records.filter((r) => r.run_id === runId);
  if (own.length === 0) throw new UsageError(`no records of run '${runId}'`);
  const at = Date.parse(own[0].run_at);
  const earlier = passingRuns(records.filter((r) => r.run_id !== runId && Date.parse(r.run_at) < at), verdicts(checkLines));
  const results = own.map((rec) => {
    const baseline = [];
    for (let i = earlier.length - 1; i >= 0 && baseline.length < window; i--) {
      const match = earlier[i].records.find((r) => seriesKey(r) === seriesKey(rec));
      if (match) baseline.unshift(match.value);
    }
    return judge(rec, baseline, limitsFor(rec, rules), minRuns);
  });
  return { own, results };
}

/** The newest run_id in a job's history (by run_at, then file order). */
export function latestRunId(records) {
  let best = null;
  for (const r of records) if (!best || Date.parse(r.run_at) >= Date.parse(best.run_at)) best = r;
  return best?.run_id ?? null;
}
