#!/usr/bin/env node
// Times each test of a Rust libtest run for the nightly performance history.
//
//   cargo test ... -- --test-threads=1 2>&1 | node scripts/perf/libtest-timer.mjs OUT.json
//
// Stable libtest prints no time per test (`--report-time` is nightly only), so this
// filter stamps each line as it arrives. It passes every line through to stdout
// unchanged, so the CI log is the same. With `--test-threads=1` the tests run one
// after the other: a test runs from its `test <name> ... ` line to the next test's
// line, or to `test result:`. Run the tests serially, or the times overlap.
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
import readline from "node:readline";
import { fileURLToPath } from "node:url";

const START_RE = /^test (\S+) \.\.\. ?(.*)$/;
const RESULT_RE = /^test result: (\w+)/;
const RUNNING_RE = /^running \d+ tests?$/;

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
      current = null;
    }
  };

  return {
    line(raw, at) {
      const line = plain(raw).trimEnd();
      if (RUNNING_RE.test(line)) {
        runStart ??= at;
        return;
      }
      const start = START_RE.exec(line);
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
  const rl = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });
  for await (const line of rl) {
    process.stdout.write(line + "\n");
    timer.line(line, Date.now());
  }
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
