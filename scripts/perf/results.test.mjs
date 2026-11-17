// Tests of the job-side converters: libtest-timer.mjs and llm-e2e-results.mjs.
// node --test 'scripts/perf/*.test.mjs'   (tests/js: pnpm run test-perf-unit)

import assert from "node:assert/strict";
import { spawn, spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import test from "node:test";

import { HERE, parseResults } from "./lib.mjs";
import { libtestTimer } from "./libtest-timer.mjs";
import { fromMocha, llmE2eResults, promptMetrics, promptsInLog, quantile, runMetrics } from "./llm-e2e-results.mjs";

/** Throws unless record would take these metrics. */
const valid = (metrics) => parseResults(JSON.stringify({ metrics }), "test");

// The executor's own lines (rust_executor::ai_service), as they appear in a step's output.
const ID = "a2527ccb-5f0e-4d4b-9a51-0d2b4f6c1e11";
const done = (ms, out) => `[2026-11-14T03:12:10Z INFO  rust_executor::ai_service] ✅ 🤖 prompt done model=${ID} latency=${ms}ms tokens_in=416 tokens_out=${out}`;
const failed = (ms) => `[2026-11-14T03:14:21Z ERROR rust_executor::ai_service] ❌ 🤖 prompt failed model=${ID} latency=${ms}ms tokens_in=332 err=Ollama error: llama runner process has terminated: cudaMalloc failed: out of memory`;

const tmp = () => fs.mkdtempSync(path.join(os.tmpdir(), "perf-results-"));

const LIBTEST = [
  "running 4 tests", // t=0
  "test a::one ... [e2e] interpretation against model 'm'", // t=100
  "some output",
  "ok", // t=300
  "test a::two ... ok", // t=1100
  "test a::three ... ignored", // t=1200
  "test a::four ... ", // t=1300
  "thread 'a::four' panicked",
  "FAILED",
  "",
  "failures:",
  "",
  "---- a::four stdout ----",
  "    a::one",
  "failures:",
  "    a::four",
  "",
  "test result: FAILED. 2 passed; 1 failed; 1 ignored", // t=5300
];
const AT = [0, 100, 200, 300, 1100, 1200, 1300, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 5300];

test("libtest-timer times each test to the next one and reads failures from the list", () => {
  const timer = libtestTimer();
  LIBTEST.forEach((l, i) => timer.line(l, 1_763_000_000_000 + AT[i]));
  const doc = timer.finish(1_763_000_000_000 + 6000);
  assert.equal(doc.duration_ms, 5300);
  assert.equal(doc.result, "FAILED");
  assert.equal(doc.started_at, new Date(1_763_000_000_000).toISOString());
  assert.deepEqual(doc.tests, [
    { name: "a::one", state: "passed", duration_ms: 1000 },
    { name: "a::two", state: "passed", duration_ms: 100 },
    { name: "a::three", state: "ignored", duration_ms: 0 },
    { name: "a::four", state: "failed", duration_ms: 1000 },
  ]);
});

test("libtest-timer passes the output through unchanged and keeps going on colour codes", () => {
  const dir = tmp();
  const out = path.join(dir, "rust.libtest.json");
  const input = ["running 1 test", "\x1b[32mtest x::y ... ok\x1b[0m", "", "test result: ok. 1 passed"].join("\n") + "\n";
  const r = spawnSync(process.execPath, [path.join(HERE, "libtest-timer.mjs"), out], { input, encoding: "utf8" });
  assert.equal(r.status, 0);
  assert.equal(r.stdout, input);
  const doc = JSON.parse(fs.readFileSync(out, "utf8"));
  assert.deepEqual(doc.tests.map((t) => [t.name, t.state]), [["x::y", "passed"]]);
  assert.equal(doc.result, "ok");
  fs.rmSync(dir, { recursive: true });
});

test("libtest-timer times a test from its newline-free `test <name> ... ` to its outcome, as libtest writes it", async () => {
  const dir = tmp();
  const out = path.join(dir, "rust.libtest.json");
  const p = spawn(process.execPath, [path.join(HERE, "libtest-timer.mjs"), out], { stdio: ["pipe", "pipe", "inherit"] });
  let passed = "";
  p.stdout.on("data", (d) => (passed += d));
  const exited = new Promise((resolve) => p.on("exit", resolve));
  const pause = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
  const write = (s) => new Promise((resolve) => p.stdin.write(s, resolve));
  // libtest flushes the start of each test's line, then runs it, then writes the outcome.
  await write("running 2 tests\ntest a::slow ... ");
  await pause(600);
  await write("ok\ntest a::fast ... ");
  await pause(50);
  await write("ok\n\ntest result: ok. 2 passed\n");
  p.stdin.end();
  assert.equal(await exited, 0);
  assert.equal(passed, "running 2 tests\ntest a::slow ... ok\ntest a::fast ... ok\n\ntest result: ok. 2 passed\n");
  const doc = JSON.parse(fs.readFileSync(out, "utf8"));
  const [slow, fast] = doc.tests;
  assert.equal(slow.name, "a::slow");
  // Timed at the line's end, the slow test would get the fast one's time (or none).
  assert.ok(slow.duration_ms >= 550, `slow: ${slow.duration_ms} ms`);
  assert.ok(fast.duration_ms < 450, `fast: ${fast.duration_ms} ms`);
  fs.rmSync(dir, { recursive: true });
});

test("libtest-timer: output after a started test's `... ` keeps it running to the next test", () => {
  const timer = libtestTimer();
  timer.line("running 2 tests", 0);
  timer.partial("test a::one ... ", 100);
  timer.line("test a::one ... [e2e] interpretation", 200);
  timer.line("ok", 900);
  timer.partial("test a::two ... ", 1000);
  timer.line("test a::two ... ignored, needs a model", 1000);
  timer.line("test result: ok. 1 passed; 1 ignored", 1100);
  assert.deepEqual(timer.finish(1200).tests, [
    { name: "a::one", state: "passed", duration_ms: 900 },
    { name: "a::two", state: "ignored", duration_ms: 0 },
  ]);
});

test("fromMocha spans the first start to the last end and keeps a retried test's last outcome", () => {
  const run = fromMocha([
    { event: "start", at: 1000 },
    { event: "test", title: "s t", state: "failed", duration_ms: 50, at: 1100 },
    { event: "test", title: "s t", state: "passed", duration_ms: 40, at: 1200 },
    { event: "end", at: 4000 },
  ]);
  assert.equal(run.duration_ms, 3000);
  assert.deepEqual(run.tests, [{ name: "s t", state: "passed", duration_ms: 40 }]);
});

test("prompt lines: done and failed, with colour codes and other lines around them", () => {
  const text = [
    "running 1 test",
    done(1000, 100),
    `\x1b[32m${done(3000, 150)}\x1b[0m`,
    "[2026-11-14T03:12:11Z INFO  rust_executor::ai_service] 🤖 prompt queued model=x",
    done(2000, 0),
    failed(130761),
  ].join("\n");
  assert.deepEqual(promptsInLog(text), [
    { ok: true, latency_ms: 1000, tokens_out: 100 },
    { ok: true, latency_ms: 3000, tokens_out: 150 },
    { ok: true, latency_ms: 2000, tokens_out: 0 },
    { ok: false, latency_ms: 130761, tokens_out: null },
  ]);
});

test("prompt metrics: calls, failures, latency of successful calls, tokens per second", () => {
  const prompts = promptsInLog([done(1000, 100), done(3000, 150), done(2000, 0), failed(130761)].join("\n"));
  const metrics = promptMetrics("rust", prompts, { model: "m" });
  valid(metrics);
  const m = Object.fromEntries(metrics.map((x) => [x.metric, x]));
  assert.deepEqual(Object.keys(m), [
    "llm.rust.calls",
    "llm.rust.failures",
    "llm.rust.latency_ms.median",
    "llm.rust.latency_ms.p90",
    "llm.rust.latency_ms.max",
    "llm.rust.output_tokens_per_s.median",
  ]);
  assert.equal(m["llm.rust.calls"].value, 4);
  assert.equal(m["llm.rust.failures"].value, 1);
  assert.equal(m["llm.rust.latency_ms.median"].value, 2000);
  assert.equal(m["llm.rust.latency_ms.max"].value, 3000, "a failed call is not in the latencies");
  // 100 tokens in 1 s and 150 in 3 s; the reply with no tokens is left out.
  assert.equal(m["llm.rust.output_tokens_per_s.median"].value, 75);
  assert.equal(m["llm.rust.output_tokens_per_s.median"].better, "higher");
  assert.equal(quantile([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 0.9), 9);
  // Every call failed (the runner's GPU out of memory): counts only, no latency.
  const allFailed = promptMetrics("rust", promptsInLog(failed(5) + "\n" + failed(6)), {});
  assert.deepEqual(allFailed.map((x) => [x.metric, x.value]), [["llm.rust.calls", 2], ["llm.rust.failures", 2]]);
});

test("llmE2eResults reads every suite's files into valid, uniquely named metrics with the model", () => {
  const dir = tmp();
  const timer = libtestTimer();
  LIBTEST.forEach((l, i) => timer.line(l, AT[i]));
  fs.writeFileSync(path.join(dir, "rust.libtest.json"), JSON.stringify(timer.finish(6000)));
  fs.writeFileSync(path.join(dir, "rust.log"), ["running 4 tests", done(10, 5), "test result: ok"].join("\n"));
  // A suite whose log has no prompt line still counts its calls: none.
  fs.writeFileSync(path.join(dir, "js_model.log"), "no prompts here\n");
  fs.writeFileSync(
    path.join(dir, "js_model.mocha.jsonl"),
    [
      { event: "start", at: 0 },
      { event: "test", title: "Suite: a/b", state: "passed", duration_ms: 1500, at: 1 },
      { event: "test", title: "Suite  a b", state: "passed", duration_ms: 2500, at: 2 },
      { event: "test", title: "skipped", state: "pending", duration_ms: null, at: 3 },
      { event: "end", at: 61_000 },
    ].map((e) => JSON.stringify(e)).join("\n"),
  );
  fs.writeFileSync(path.join(dir, "notes.txt"), "ignored");
  const { metrics } = llmE2eResults(dir, "qwen3.8-27b-48k");
  const names = metrics.map((m) => m.metric);
  assert.deepEqual([...names].sort(), [
    "llm.js_model.calls",
    "llm.js_model.failures",
    "llm.rust.calls",
    "llm.rust.failures",
    "llm.rust.latency_ms.max",
    "llm.rust.latency_ms.median",
    "llm.rust.latency_ms.p90",
    "llm.rust.output_tokens_per_s.median",
    "suite.js_model.duration",
    "suite.rust.duration",
    "test.js_model.Suite_a_b.duration",
    "test.js_model.Suite_a_b_2.duration",
    "test.rust.a_one.duration",
    "test.rust.a_two.duration",
  ]);
  assert.equal(new Set(names).size, names.length);
  // A title that slugs to another's suffixed name gets a free name too.
  const titles = ["a b", "a/b", "a_b_2"].map((name) => ({ name, state: "passed", duration_ms: 1000 }));
  const generated = runMetrics("s", { tests: titles }, {}).map((m) => m.metric);
  assert.deepEqual(generated, ["test.s.a_b.duration", "test.s.a_b_2.duration", "test.s.a_b_2_2.duration"]);
  const byName = Object.fromEntries(metrics.map((m) => [m.metric, m]));
  assert.equal(byName["suite.js_model.duration"].value, 61);
  assert.equal(byName["suite.rust.duration"].value, 5.3);
  assert.equal(byName["test.rust.a_one.duration"].value, 1);
  valid(metrics);
  for (const m of metrics) assert.deepEqual(m.params, { model: "qwen3.8-27b-48k" });
  assert.throws(() => llmE2eResults(dir), /--model/);
  fs.rmSync(dir, { recursive: true });
});
