// Unit tests of scripts/perf (record, check, report). Runner: Node's built-in test runner.
//   node --test 'scripts/perf/*.test.mjs'   (tests/js: pnpm run test-perf-unit)
// CI: the js-unit-tests job. The history is a local bare repository in a temp directory, never
// the real one: every command gets PERF_HISTORY_REPO, and CI is unset. store.test.mjs tests the
// git side (push races, an unreachable history).

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { after, describe, test } from "node:test";

import { evaluateRun, judge, limitsFor, loadThresholds, parseResults } from "./lib.mjs";
import { renderReport } from "./report.mjs";

const HERE = path.dirname(new URL(import.meta.url).pathname);
const PERF = path.join(HERE, "perf.mjs");
const SHA = "0123456789abcdef0123456789abcdef01234567";
const SHA2 = "fedcba9876543210fedcba9876543210fedcba98";

const roots = [];
function tmp() {
  const d = fs.mkdtempSync(path.join(os.tmpdir(), "perf-test-"));
  roots.push(d);
  return d;
}
after(() => roots.forEach((d) => fs.rmSync(d, { recursive: true, force: true })));

// No CircleCI variables leak into the commands (CI=true would make the default repo writable).
const ENV = Object.fromEntries(
  Object.entries(process.env).filter(([k]) => !k.startsWith("CIRCLE") && !k.startsWith("PERF_") && k !== "WE_SHA" && k !== "CI"),
);

const jsonl = (text) => (text ?? "").split("\n").filter(Boolean).map((l) => JSON.parse(l));

/** A fresh history: an empty bare repository, and the env that points the commands at it. */
function history(extra = {}) {
  const root = tmp();
  const bare = path.join(root, "history.git");
  spawnSync("git", ["init", "-q", "--bare", "-b", "main", bare]);
  const env = { ...ENV, PERF_HISTORY_REPO: bare, PERF_HISTORY_CLONE: path.join(root, "clone"), ...extra };
  const read = (file) => {
    const r = spawnSync("git", ["-C", bare, "show", `main:nightly/${file}`], { encoding: "utf8" });
    return r.status === 0 ? jsonl(r.stdout) : [];
  };
  return { env, records: (job = "j") => read(`${job}.jsonl`), checks: (job = "j") => read(`${job}.checks.jsonl`) };
}

function perf(args, env) {
  const r = spawnSync(process.execPath, [PERF, ...args], { encoding: "utf8", env });
  return { code: r.status, out: r.stdout, err: r.stderr };
}

function writeResults(dir, name, metrics) {
  const f = path.join(dir, name);
  fs.writeFileSync(f, JSON.stringify({ metrics }));
  return f;
}

/** Record `value` of metric m (lower is better, ms) as run `id` at day `day`. */
function recordValue(h, id, day, value, { metric = "m.latency_ms", params, status, job = "j" } = {}) {
  const f = writeResults(tmp(), "r.json", [{ metric, value, unit: "ms", better: "lower", ...(params ? { params } : {}) }]);
  const args = ["record", "--job", job, "--ad4m-sha", SHA, "--run-id", id, "--run-at", `2026-11-${String(day).padStart(2, "0")}T02:00:00Z`];
  if (status) args.push("--status", status);
  const r = perf([...args, f], h.env);
  assert.equal(r.code, 0, r.err);
  return r;
}

const check = (h, id, more = [], job = "j") => perf(["check", "--job", job, ...(id ? ["--run-id", id] : []), ...more], h.env);

describe("judge", () => {
  const rec = (value, better = "lower") => ({ job: "j", metric: "m", value, unit: "ms", better, params: {} });
  const limits = { factor: 1.5, min_delta: 0 };

  test("a value worse than 1.5x the median regresses; at 1.5x it does not", () => {
    assert.equal(judge(rec(151), [100, 100, 100], limits).regressed, true);
    assert.equal(judge(rec(150), [100, 100, 100], limits).regressed, false);
  });

  test("the median, not the mean, is the baseline", () => {
    // Mean 400, median 100: 140 is fine.
    assert.equal(judge(rec(140), [100, 100, 1000], limits).regressed, false);
    assert.equal(judge(rec(160), [90, 100, 1000], limits).median, 100);
  });

  test("higher is better: below median / factor regresses", () => {
    assert.equal(judge(rec(60, "higher"), [100, 100, 100], limits).regressed, true);
    assert.equal(judge(rec(70, "higher"), [100, 100, 100], limits).regressed, false);
  });

  test("min_delta keeps a small absolute change from failing", () => {
    assert.equal(judge(rec(40), [10, 10, 10], { factor: 1.5, min_delta: 50 }).regressed, false);
    assert.equal(judge(rec(70), [10, 10, 10], { factor: 1.5, min_delta: 50 }).regressed, true);
  });

  test("under 3 runs the factor rule reports only", () => {
    const j = judge(rec(1000), [100, 100], limits);
    assert.equal(j.reportOnly, true);
    assert.equal(j.regressed, false);
  });

  test("a ceiling or floor fails at any history, report-only or not", () => {
    const j = judge(rec(1000), [], { ...limits, ceiling: 500 });
    assert.equal(j.reportOnly, true);
    assert.equal(j.regressed, true);
    assert.match(j.reasons[0], /ceiling 500/);
    assert.equal(judge(rec(5, "higher"), [], { ...limits, floor: 10 }).regressed, true);
    assert.equal(judge(rec(500), [], { ...limits, ceiling: 500 }).regressed, false);
  });

  test("a zero median takes no factor rule", () => {
    assert.equal(judge(rec(3), [0, 0, 0], limits).regressed, false);
  });
});

describe("thresholds.json", () => {
  test("the defaults, then every matching rule in order", () => {
    const f = path.join(tmp(), "t.json");
    fs.writeFileSync(f, JSON.stringify({ rules: [{ job: "j", metric: "a.*", factor: 2 }, { job: "*", metric: "a.b", min_delta: 5 }, { metric: "a.b", factor: 3 }] }));
    const rules = loadThresholds(f);
    assert.deepEqual(limitsFor({ job: "j", metric: "a.b" }, rules), { factor: 3, min_delta: 5 });
    assert.deepEqual(limitsFor({ job: "j", metric: "a.c" }, rules), { factor: 2, min_delta: 0 });
    assert.deepEqual(limitsFor({ job: "k", metric: "x" }, rules), { factor: 1.5, min_delta: 0 });
  });

  test("the committed file loads, and its llm rules name metrics llm-e2e-results writes", () => {
    const rules = loadThresholds();
    assert.equal(limitsFor({ job: "llm-e2e", metric: "llm.rust.latency_ms.p90" }, rules).min_delta, 1000);
    assert.equal(limitsFor({ job: "llm-e2e", metric: "llm.js_model.failures" }, rules).factor, 2);
    assert.equal(limitsFor({ job: "integration-tests-sfu", metric: "sfu.suite.duration_ms" }, rules).ceiling, 1800000);
  });
});

describe("results JSON", () => {
  const ok = { metric: "a.b", value: 1, unit: "ms", better: "lower" };

  test("reads {metrics:[...]}; params default to {}", () => {
    const [m] = parseResults(JSON.stringify({ metrics: [ok] }), "x");
    assert.deepEqual(m, { ...ok, params: {} });
  });

  test("refuses bad entries and anything but {metrics:[...]}", () => {
    const bad = [
      { metric: "a b", value: 1, unit: "ms", better: "lower" },
      { metric: "a", value: "1", unit: "ms", better: "lower" },
      { metric: "a", value: Infinity, unit: "ms", better: "lower" },
      { metric: "a", value: 1, better: "lower" },
      { metric: "a", value: 1, unit: "ms", better: "faster" },
      { metric: "a", value: 1, unit: "ms", better: "lower", params: { x: { y: 1 } } },
    ];
    for (const m of bad) assert.throws(() => parseResults(JSON.stringify({ metrics: [m] }), "x"), undefined, JSON.stringify(m));
    assert.throws(() => parseResults(JSON.stringify([ok]), "x"), /expected \{"metrics"/);
    assert.throws(() => parseResults("{", "x"));
  });
});

describe("record", () => {
  test("writes the spec's fields, one line per metric, to the history and the artifacts", () => {
    const out = tmp();
    const h = history({ PERF_ARTIFACTS_DIR: out, CIRCLE_BUILD_NUM: "123", CIRCLE_WORKFLOW_ID: "wf-1", PERF_PIPELINE_NUMBER: "77" });
    const f = writeResults(tmp(), "a.json", [
      { metric: "a.x", value: 1.5, unit: "ms", better: "lower", params: { peers: 8 } },
      { metric: "a.y", value: 3, unit: "count", better: "higher" },
    ]);
    const r = perf(["record", "--job", "j", "--ad4m-sha", SHA, "--we-sha", SHA2, f], h.env);
    assert.equal(r.code, 0, r.err);
    const lines = h.records();
    assert.equal(lines.length, 2);
    assert.deepEqual(Object.keys(lines[0]), [
      "job", "metric", "value", "unit", "better", "ad4m_sha", "we_sha", "run_at", "pipeline", "workflow_id", "job_number", "host", "params", "run_id", "status",
    ]);
    assert.equal(lines[0].we_sha, SHA2);
    assert.equal(lines[0].pipeline, 77);
    assert.equal(lines[0].job_number, 123);
    assert.equal(lines[0].workflow_id, "wf-1");
    assert.equal(lines[0].run_id, "circleci-123");
    assert.equal(lines[0].status, "passed");
    assert.deepEqual(lines[0].params, { peers: 8 });
    assert.deepEqual(jsonl(fs.readFileSync(path.join(out, "records.jsonl"), "utf8")), lines);
  });

  test("reads every *.json directly inside a directory", () => {
    const h = history();
    const dir = tmp();
    writeResults(dir, "a.json", [{ metric: "a", value: 1, unit: "ms", better: "lower" }]);
    writeResults(dir, "b.json", [{ metric: "b", value: 2, unit: "ms", better: "lower" }]);
    fs.writeFileSync(path.join(dir, "notes.txt"), "not read");
    fs.mkdirSync(path.join(dir, "raw"));
    writeResults(path.join(dir, "raw"), "c.json", [{ metric: "c", value: 3, unit: "ms", better: "lower" }]);
    assert.equal(perf(["record", "--job", "j", "--ad4m-sha", SHA, dir], h.env).code, 0);
    assert.deepEqual(h.records().map((l) => l.metric), ["a", "b"]);
  });

  test("refuses a bad job, a bad sha, a duplicate metric, an unknown option and an empty passed run", () => {
    const h = history();
    const f = writeResults(tmp(), "a.json", [{ metric: "a", value: 1, unit: "ms", better: "lower" }]);
    for (const args of [
      ["--job", "Bad Job", "--ad4m-sha", SHA, f],
      ["--job", "j", "--ad4m-sha", "abc", f],
      ["--job", "j", "--ad4m-sha", SHA, f, f],
      ["--job", "j", "--ad4m-sha", SHA, tmp()],
      ["--job", "j", "--ad4m-sha", SHA, "--factor", "2", f],
    ]) {
      assert.equal(perf(["record", ...args], h.env).code, 2, args.join(" "));
    }
    assert.deepEqual(h.records(), [], "nothing written");
  });

  test("a failed run with no measurements records nothing and succeeds", () => {
    const h = history();
    const r = perf(["record", "--job", "j", "--ad4m-sha", SHA, "--status", "failed", path.join(tmp(), "missing")], h.env);
    assert.equal(r.code, 0, r.err);
    assert.deepEqual(h.records(), []);
  });
});

describe("check", () => {
  test("reports only under 3 earlier runs, then fails a 1.5x regression", () => {
    const h = history();
    recordValue(h, "r1", 1, 100);
    recordValue(h, "r2", 2, 110);
    recordValue(h, "r3", 3, 1000);
    let r = check(h, "r3");
    assert.equal(r.code, 0, r.out + r.err);
    assert.match(r.out, /perf check j: 1 metrics, 0 regressed, 1 report-only/);
    // r3 was report-only, so it stays in the baseline: 100, 110, 1000 → median 110.
    recordValue(h, "r4", 4, 160);
    r = check(h, "r4");
    assert.equal(r.code, 0, r.out);
    assert.match(r.out, /0 regressed, 0 report-only.* - OK/);
    recordValue(h, "r5", 5, 300);
    r = check(h, "r5");
    assert.equal(r.code, 1, r.out);
    assert.match(r.out, /^REGRESSION j m\.latency_ms: 300 ms/m);
    assert.match(r.out, /1 regressed/);
    assert.equal(r.out.trim().split("\n").filter((l) => l.startsWith("perf check")).length, 1, "one summary line");
    assert.deepEqual(h.checks().map((c) => [c.run_id, c.verdict]), [["r3", "report-only"], ["r4", "ok"], ["r5", "regressed"]]);
  });

  test("values with other params never compare", () => {
    const h = history();
    for (let d = 1; d <= 4; d++) recordValue(h, `small-${d}`, d, 10, { params: { messages: 1000 } });
    recordValue(h, "big-5", 5, 5000, { params: { messages: 1000000 } });
    const r = check(h, "big-5");
    assert.equal(r.code, 0, r.out);
    assert.match(r.out, /1 report-only/);
    recordValue(h, "small-6", 6, 100, { params: { messages: 1000 } });
    assert.equal(check(h, "small-6").code, 1);
  });

  test("other jobs and metrics never compare", () => {
    const h = history();
    for (let d = 1; d <= 3; d++) recordValue(h, `a-${d}`, d, 10, { job: "a" });
    recordValue(h, "b-4", 4, 1000, { job: "b" });
    assert.equal(check(h, "b-4", [], "b").code, 0);
    for (let d = 1; d <= 3; d++) recordValue(h, `x-${d}`, d, 10, { metric: "other" });
    recordValue(h, "x-4", 4, 1000, { metric: "m.latency_ms" });
    assert.match(check(h, "x-4").out, /1 report-only/);
  });

  test("a ceiling from thresholds.json fails the first run", () => {
    const rules = loadThresholds();
    const rec = { job: "integration-tests-sfu", metric: "sfu.suite.duration_ms", value: 2_000_000, unit: "ms", better: "lower", params: {}, run_id: "r1", run_at: "2026-11-01T00:00:00Z", status: "passed" };
    const { results } = evaluateRun([rec], [], "r1", rules);
    assert.equal(results[0].reportOnly, true);
    assert.equal(results[0].regressed, true);
    assert.match(results[0].reasons[0], /above the ceiling 1800000 ms/);
  });

  test("only the last 7 passing runs make the baseline; failed and regressed runs are left out until accepted", () => {
    const h = history();
    // Seven old slow runs, then seven fast ones: only the fast ones count.
    for (let d = 1; d <= 7; d++) recordValue(h, `old-${d}`, d, 1000);
    for (let d = 8; d <= 14; d++) recordValue(h, `new-${d}`, d, 100);
    recordValue(h, "failed-15", 15, 1, { status: "failed" });
    recordValue(h, "r16", 16, 200);
    let r = check(h, "r16");
    assert.equal(r.code, 1, r.out);
    assert.match(r.out, /median 100 ms of 7 runs/);
    // r16 was found regressed: it does not move the next baseline.
    recordValue(h, "r17", 17, 140);
    r = check(h, "r17");
    assert.equal(r.code, 0, r.out);
    // Accept r16: it joins later baselines.
    r = check(h, null, ["--accept", "r16"]);
    assert.equal(r.code, 0, r.err);
    assert.equal(h.checks().at(-1).verdict, "accepted");
    const records = h.records();
    const checks = h.checks();
    // With a window of one run, r17's baseline is the run just before it: r16 (200), no longer
    // skipped as regressed.
    assert.equal(evaluateRun(records, checks, "r17", [], { window: 1, minRuns: 1 }).results[0].median, 200);
    const before = checks.filter((c) => c.verdict !== "accepted");
    assert.equal(evaluateRun(records, before, "r17", [], { window: 1, minRuns: 1 }).results[0].median, 100);
    // --accept of a run the history does not have is refused.
    assert.equal(check(h, null, ["--accept", "nope"]).code, 2);
  });

  test("defaults to the newest run in the history; exit 2 without history", () => {
    const h = history();
    assert.equal(check(h, null).code, 2);
    for (let d = 1; d <= 3; d++) recordValue(h, `r${d}`, d, 100);
    recordValue(h, "r4", 4, 1000);
    const r = check(h, null);
    assert.equal(r.code, 1);
    assert.match(r.out, /run r4/);
  });
});

describe("report", () => {
  test("draws one chart per series with commit links", () => {
    const h = history();
    for (let d = 1; d <= 3; d++) recordValue(h, `r${d}`, d, 100 + d);
    recordValue(h, "p", 4, 7, { params: { n: 2 } });
    const out = path.join(tmp(), "trend.html");
    const r = perf(["report", "--job", "j", "--out", out], h.env);
    assert.equal(r.code, 0, r.err);
    assert.match(r.out, /1 jobs, 2 series/);
    const html = fs.readFileSync(out, "utf8");
    assert.equal((html.match(/<svg /g) ?? []).length, 2);
    assert.ok(html.includes(`https://github.com/coasys/ad4m/commit/${SHA}`));
    assert.equal((html.match(/<circle /g) ?? []).length, 4);
    assert.equal(perf(["report", "--out", out], h.env).code, 2, "--job is required");
  });

  const rec = (metric, run_id, day, value) => ({ job: "j", metric, value, unit: "ms", better: "lower", ad4m_sha: SHA, run_at: `2026-11-0${day}T00:00:00Z`, run_id, status: "passed", params: {} });

  test("draws only the newest runs", () => {
    const records = [1, 2, 3, 4, 5].map((d) => rec("a", `r${d}`, d, d));
    assert.equal((renderReport([{ job: "j", records, checks: [] }], { runs: 3 }).match(/<circle /g) ?? []).length, 3);
  });

  test("marks red only the series a check found regressed, not the run's other metrics", () => {
    const records = [rec("a", "r1", 1, 1), rec("b", "r1", 1, 1), rec("a", "r2", 2, 9), rec("b", "r2", 2, 1)];
    const checks = [{ job: "j", run_id: "r2", verdict: "regressed", regressions: [{ metric: "a", params: {}, value: 9, median: 1, reasons: ["factor"] }] }];
    const html = renderReport([{ job: "j", records, checks }]);
    assert.equal((html.match(/class="pt regressed"/g) ?? []).length, 1);
    assert.equal((html.match(/class="pt passed"/g) ?? []).length, 3);
  });

  test("escapes what it prints", () => {
    const html = renderReport([{ job: "j", records: [{ ...rec("a", "r", 1, 1), unit: "<b>", params: { q: "<i>" } }], checks: [] }]);
    assert.ok(!html.includes("<b>"));
    assert.ok(!html.includes("<i>"));
  });
});
