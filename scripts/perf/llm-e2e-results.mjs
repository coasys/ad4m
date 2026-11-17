#!/usr/bin/env node
// The llm-e2e job's performance numbers, as a results JSON for scripts/perf/record.
//
//   node scripts/perf/llm-e2e-results.mjs --dir RAW --model MODEL [--out FILE]
//
// RAW holds what the job's suites wrote, named by suite (rust, js_model, js_integration; any
// name of [a-z0-9_]):
//
//   <suite>.libtest.json   scripts/perf/libtest-timer.mjs (a Rust libtest run)
//   <suite>.mocha.jsonl    tests/js/utils/perfMochaHooks.ts (PERF_MOCHA_LOG)
//   <suite>.log            the step's output, with the executor's log lines of each prompt
//                          (rust_executor::ai_service, `prompt done` at info, `prompt failed`
//                          at error):
//     ✅ 🤖 prompt done model=<id> latency=<ms>ms tokens_in=<n> tokens_out=<n>
//     ❌ 🤖 prompt failed model=<id> latency=<ms>ms tokens_in=<n> err=<...>
//
// Metrics, each with params {"model": MODEL} (another model is another series):
//
//   suite.<suite>.duration                  s     lower   the suite's test run
//   test.<suite>.<title>.duration           s     lower   each passed test
//   llm.<suite>.calls                       count lower   prompts (more prompts: more work)
//   llm.<suite>.failures                    count lower   prompts that failed
//   llm.<suite>.latency_ms.median|p90|max   ms    lower   successful prompts, start to reply
//   llm.<suite>.output_tokens_per_s.median  tok/s higher  tokens_out / latency per prompt
//                                                         (tokens_out is the executor's estimate)
//
// A suite with no file writes nothing. A failed test has no duration metric; the job's status
// says whether it passed. Writes FILE (default RAW/../llm-e2e.json).

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { median, readLines, slug, UsageError } from "./lib.mjs";

const SUITE_RE = /^([a-z0-9_]+)\.(libtest\.json|mocha\.jsonl|log)$/;
const PROMPT_RE = /🤖 prompt (done|failed) model=\S+ latency=(\d+)ms tokens_in=\d+(?: tokens_out=(\d+))?/;

/** The q-quantile (0..1) by nearest rank. */
export function quantile(values, q) {
  const s = [...values].sort((a, b) => a - b);
  return s.length ? s[Math.min(s.length - 1, Math.max(0, Math.ceil(q * s.length) - 1))] : null;
}

const round = (n, d = 1) => Math.round(n * 10 ** d) / 10 ** d;

/** The prompts in a log: [{ok, latency_ms, tokens_out}]. */
export function promptsInLog(text) {
  const out = [];
  for (const line of text.replace(/\x1b\[[0-9;]*m/g, "").split("\n")) {
    const m = PROMPT_RE.exec(line);
    if (m) out.push({ ok: m[1] === "done", latency_ms: Number(m[2]), tokens_out: m[3] === undefined ? null : Number(m[3]) });
  }
  return out;
}

/** The prompt metrics of one suite. */
export function promptMetrics(suite, prompts, params) {
  const out = [];
  const m = (metric, value, unit, better) => out.push({ metric: `llm.${suite}.${metric}`, value, unit, better, params });
  m("calls", prompts.length, "count", "lower");
  m("failures", prompts.filter((p) => !p.ok).length, "count", "lower");
  const ok = prompts.filter((p) => p.ok);
  if (ok.length) {
    const ms = ok.map((p) => p.latency_ms);
    m("latency_ms.median", median(ms), "ms", "lower");
    m("latency_ms.p90", quantile(ms, 0.9), "ms", "lower");
    m("latency_ms.max", Math.max(...ms), "ms", "lower");
  }
  const rates = ok.filter((p) => p.tokens_out > 0 && p.latency_ms > 0).map((p) => (p.tokens_out * 1000) / p.latency_ms);
  if (rates.length) m("output_tokens_per_s.median", round(median(rates), 2), "tok/s", "higher");
  return out;
}

/** {duration_ms, tests} from a perfMochaHooks log: the run spans the first start to the last end. */
export function fromMocha(events) {
  const starts = events.filter((e) => e.event === "start").map((e) => e.at);
  const ends = events.filter((e) => e.event === "end" || e.event === "test").map((e) => e.at);
  const byTitle = new Map();
  // A retried test logs again: the last entry is its outcome.
  for (const e of events) if (e.event === "test") byTitle.set(e.title, { name: e.title, state: e.state, duration_ms: e.duration_ms });
  return { duration_ms: starts.length && ends.length ? Math.max(...ends) - Math.min(...starts) : null, tests: [...byTitle.values()] };
}

/** The timing metrics of one suite's run ({duration_ms, tests: [{name, state, duration_ms}]}). */
export function runMetrics(suite, run, params) {
  const out = [];
  if (Number.isFinite(run.duration_ms)) out.push({ metric: `suite.${suite}.duration`, value: round(run.duration_ms / 1000), unit: "s", better: "lower", params });
  const seen = new Map();
  for (const t of run.tests ?? []) {
    if (t.state !== "passed" || !Number.isFinite(t.duration_ms)) continue;
    // Two titles that slug alike still get a name each.
    let name = slug(t.name);
    const n = (seen.get(name) ?? 0) + 1;
    seen.set(name, n);
    if (n > 1) name = `${name}_${n}`;
    out.push({ metric: `test.${suite}.${name}.duration`, value: round(t.duration_ms / 1000), unit: "s", better: "lower", params });
  }
  return out;
}

/** Every metric of the files in `dir`. */
export function llmE2eResults(dir, model) {
  if (!model) throw new UsageError("--model is required (another model is another series)");
  const params = { model };
  const suites = new Map();
  for (const f of fs.existsSync(dir) ? fs.readdirSync(dir).sort() : []) {
    const m = SUITE_RE.exec(f);
    if (m) suites.set(m[1], { ...suites.get(m[1]), [m[2]]: path.join(dir, f) });
  }
  const metrics = [];
  for (const [suite, files] of suites) {
    if (files["libtest.json"]) metrics.push(...runMetrics(suite, JSON.parse(fs.readFileSync(files["libtest.json"], "utf8")), params));
    else if (files["mocha.jsonl"]) metrics.push(...runMetrics(suite, fromMocha(readLines(files["mocha.jsonl"])), params));
    if (files.log) metrics.push(...promptMetrics(suite, promptsInLog(fs.readFileSync(files.log, "utf8")), params));
  }
  return { metrics };
}

function main(argv) {
  const opt = {};
  for (let i = 0; i < argv.length; i += 2) {
    if (!["--dir", "--model", "--out"].includes(argv[i]) || argv[i + 1] === undefined) throw new UsageError(`bad argument ${argv[i]}`);
    opt[argv[i].slice(2)] = argv[i + 1];
  }
  if (!opt.dir) throw new UsageError("--dir is required");
  const out = opt.out ?? path.join(opt.dir, "..", "llm-e2e.json");
  const doc = llmE2eResults(opt.dir, opt.model);
  fs.mkdirSync(path.dirname(path.resolve(out)), { recursive: true });
  fs.writeFileSync(out, JSON.stringify(doc, null, 1) + "\n");
  console.log(`llm-e2e results: ${doc.metrics.length} metrics -> ${out}`);
  return 0;
}

if (process.argv[1] && fileURLToPath(import.meta.url) === path.resolve(process.argv[1])) {
  try {
    process.exitCode = main(process.argv.slice(2));
  } catch (e) {
    console.error(`llm-e2e-results: ${e.message}`);
    process.exitCode = 2;
  }
}
