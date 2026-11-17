#!/usr/bin/env node
// Times each test of a Rust libtest run for the nightly performance history.
//
//   cargo test ... -- --test-threads=1 2>&1 | node scripts/perf/libtest-timer.mjs OUT.json
//
// Stable libtest prints no time per test (`--report-time` is nightly only), so this
// filter stamps the output as it arrives. It passes every chunk through to stdout
// unchanged, so the CI log is the same. With `--test-threads=1` libtest prints
// `test <name> ... ` with no newline when a test starts and `ok` when it ends: a test
// runs from that prefix's arrival to the end of its line (its outcome), or, when its
// line is broken by its own output, to the next test or `test result:`. Run the tests
// serially, or the times overlap.
//
// OUT.json (written at the end of the input, also when the tests failed):
//
//   {"started_at": "<ISO>", "duration_ms": N,
//    "tests": [{"name": "a::b", "state": "passed|failed|ignored", "duration_ms": N}],
//    "result": "ok|FAILED|null"}
//
// `duration_ms` of the run: from `running N tests` to `test result:` (the build is not
// in it). `failed` comes from libtest's `failures:` list, `ignored` from `... ignored`.
// The filter always exits 0; use `set -o pipefail` so the pipeline keeps cargo's status.
// `scripts/perf/llm-e2e-results.mjs` turns the file into metrics.

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const START_RE = /^test (\S+) \.\.\. ?(.*)$/;
const RESULT_RE = /^test result: (\w+)/;
const RUNNING_RE = /^running \d+ tests?$/;
// A test that has started: its line so far, before its outcome.
const PARTIAL_RE = /^test (\S+) \.\.\. $/;
const OUTCOME_RE = /^(ok|FAILED|ignored\b.*)$/;

// The ANSI colour codes libtest adds on a terminal (CI may force them).
const plain = (line) => line.replace(/\x1b\[[0-9;]*m/g, "");

/** A timer fed one line at a time with its arrival time (ms). */
export function libtestTimer() {
  const tests = [];
  let current = null;
  let runStart = null;
  let runEnd = null;
  let result = null;
  let inFailures = false;
  const failed = new Set();

  const close = (at) => {
    if (current) {
      current.duration_ms = Math.max(0, Math.round(at - current.at));
      delete current.at;
      delete current.started;
      current = null;
    }
  };

  return {
    /** The input after its last newline: a `test <name> ... ` there is a test starting. */
    partial(raw, at) {
      const m = PARTIAL_RE.exec(plain(raw));
      if (!m || current?.started) return;
      close(at);
      current = { name: m[1], state: "passed", at, started: true };
      tests.push(current);
    },
    line(raw, at) {
      const line = plain(raw).trimEnd();
      if (RUNNING_RE.test(line)) {
        runStart ??= at;
        return;
      }
      const start = START_RE.exec(line);
      if (start && current?.started && current.name === start[1]) {
        // The started test's line is complete: its outcome, now.
        // Anything else after `... ` is the test's own output: it ends at the next test.
        const rest = start[2].trim();
        if (OUTCOME_RE.test(rest)) {
          current.state = rest.startsWith("ignored") ? "ignored" : "passed";
          close(at);
        } else {
          delete current.started;
        }
        return;
      }
      if (start) {
        close(at);
        const rest = start[2].trim();
        current = { name: start[1], state: rest === "ignored" ? "ignored" : "passed", at };
        tests.push(current);
        if (rest.startsWith("ignored")) close(at);
        return;
      }
      const res = RESULT_RE.exec(line);
      if (res) {
        close(at);
        runEnd = at;
        result = res[1];
        inFailures = false;
        return;
      }
      if (line === "failures:") {
        close(at);
        inFailures = true;
        // libtest prints `failures:` twice: the tests' output, then the list of names.
        // Only the last list counts, so output that looks like a name does not.
        failed.clear();
        return;
      }
      // The last `failures:` list names each failed test, indented by four spaces.
      if (inFailures && /^ {4}\S+$/.test(line)) failed.add(line.trim());
    },
    finish(at) {
      close(at);
      for (const t of tests) if (failed.has(t.name)) t.state = "failed";
      return {
        started_at: runStart == null ? null : new Date(runStart).toISOString(),
        duration_ms: runStart == null ? null : Math.round((runEnd ?? at) - runStart),
        tests,
        result,
      };
    },
  };
}

async function main(argv) {
  const out = argv[0];
  if (!out || argv.length !== 1) {
    process.stderr.write("usage: cargo test ... 2>&1 | libtest-timer.mjs OUT.json\n");
    // Pass the input through anyway, so a wrong call never hides the test output.
    process.stdin.pipe(process.stdout);
    return 2;
  }
  const timer = libtestTimer();
  // Chunks, not lines: the `test <name> ... ` that starts a test has no newline yet.
  let rest = "";
  process.stdin.setEncoding("utf8");
  for await (const chunk of process.stdin) {
    process.stdout.write(chunk);
    const at = Date.now();
    const lines = (rest + chunk).split("\n");
    rest = lines.pop();
    for (const line of lines) timer.line(line.replace(/\r$/, ""), at);
    if (rest) timer.partial(rest, at);
  }
  if (rest) timer.line(rest, Date.now());
  const doc = timer.finish(Date.now());
  try {
    fs.mkdirSync(path.dirname(path.resolve(out)), { recursive: true });
    fs.writeFileSync(out, JSON.stringify(doc, null, 1) + "\n");
  } catch (e) {
    process.stderr.write(`libtest-timer: ${out} not written: ${e.message}\n`);
  }
  return 0;
}

if (process.argv[1] && fileURLToPath(import.meta.url) === path.resolve(process.argv[1])) {
  main(process.argv.slice(2)).then((code) => {
    process.exitCode = code;
  });
}
