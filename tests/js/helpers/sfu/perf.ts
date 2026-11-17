/**
 * Performance numbers of an SFU wind-tunnel scenario, for the nightly history
 * (`scripts/perf/record`, see the header of `scripts/perf/perf.mjs`).
 *
 * When `PERF_RESULTS_DIR` is set, `sfuScenario` writes the numbers of each
 * scenario that ran to `${PERF_RESULTS_DIR}/sfu-<id>.json` in the results
 * format `{"metrics": [{metric, value, unit, better}]}`:
 *
 *   sfu.<id>.duration_ms             the scenario's own run time (ScenarioResult.durationMs)
 *   sfu.<id>.<path>                  each performance number in ScenarioResult.metrics
 *   sfu.<id>.sample.<name>           each timed sample without an error
 *
 * Only performance numbers go in: times (a key with `ms`, `latency`, `rtt`
 * or `wait`), bytes, frames, sample counts and loss counts (dropped, lost,
 * repairs, renegotiations, crashes, failures). Counts a scenario asserts
 * exactly (participants per node, redirects, landed peers) are correctness,
 * not performance: the test fails on them, so they are left out. A numeric
 * array becomes `.min`, `.median` and `.max`; an object keyed by DIDs is read
 * as an array of its values, so the name stays the same from run to run.
 */

import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import type { ScenarioResult } from "./scenario.js";

export interface PerfMetric {
  metric: string;
  value: number;
  unit: string;
  better: "lower" | "higher";
}

type Kind = { unit: string; better: "lower" | "higher" } | null;

/** The words of a metric path: `preCrash_nodeA_pipeCount` → pre crash node a pipe count. */
export function words(path: string): string[] {
  return path
    .replace(/([a-z0-9])([A-Z])/g, "$1_$2")
    .toLowerCase()
    .split(/[^a-z0-9]+/)
    .filter(Boolean);
}

/** The unit and direction of a metric path, or null when it is not a performance number. */
export function classify(path: string): Kind {
  const w = words(path);
  const has = (...xs: string[]) => xs.some((x) => w.includes(x));
  const last = w[w.length - 1];
  // Wall-clock instants (`nodeAKilledAt`, `startTime`) are not measurements.
  if (last === "at" || (last === "time" && has("start", "end"))) return null;
  if (last === "samples") return { unit: "count", better: "higher" };
  // The sample count of a time summary (`alice_rtt_summary.count`).
  if (last === "count") return has("rtt", "latency") ? { unit: "count", better: "higher" } : null;
  // A count of layers that carried bytes (`simulcastActiveWithBytes`) is asserted exactly.
  if (has("with")) return null;
  if (has("ms", "latency", "rtt", "wait")) return { unit: "ms", better: "lower" };
  if (has("bytes") || (has("upload", "download") && has("mean"))) return { unit: "bytes", better: "higher" };
  if (has("frames", "delivered")) return { unit: "count", better: "higher" };
  if (has("dropped", "lost", "loss", "repairs", "renegotiations", "crashes", "failures", "errors")) {
    return { unit: "count", better: "lower" };
  }
  return null;
}

// `bob->alice` (a direction, as T21 keys its frame counts) is `bob_to_alice`.
const slug = (s: string) =>
  s.replace(/\s*->\s*/g, "_to_").replace(/[^A-Za-z0-9_-]+/g, "_").replace(/^_+|_+$/g, "") || "_";

function medianOf(values: number[]): number {
  const s = [...values].sort((a, b) => a - b);
  const mid = s.length >> 1;
  return s.length % 2 ? s[mid] : (s[mid - 1] + s[mid]) / 2;
}

const isNum = (v: unknown): v is number => typeof v === "number" && Number.isFinite(v);

/** Every finite number in `value`, as [path, number]. */
function leaves(value: unknown, path: string, out: Array<[string, number]>): void {
  if (isNum(value)) {
    out.push([path, value]);
    return;
  }
  if (Array.isArray(value)) {
    const nums = value.filter(isNum);
    if (nums.length > 0 && nums.length === value.length) {
      out.push([`${path}.min`, Math.min(...nums)]);
      out.push([`${path}.median`, medianOf(nums)]);
      out.push([`${path}.max`, Math.max(...nums)]);
    }
    return;
  }
  if (value && typeof value === "object") {
    const entries = Object.entries(value as Record<string, unknown>);
    // Keyed by DID or URL: names that change every run. Read as an array.
    if (entries.length > 0 && entries.every(([k]) => k.includes(":"))) {
      leaves(entries.map(([, v]) => v), path, out);
      return;
    }
    for (const [k, v] of entries) leaves(v, `${path}.${slug(k)}`, out);
  }
}

/** The performance metrics of one scenario result. */
export function perfMetrics(id: string, result: ScenarioResult): PerfMetric[] {
  const base = `sfu.${slug(id)}`;
  const out: PerfMetric[] = [];
  if (isNum(result.durationMs)) {
    out.push({ metric: `${base}.duration_ms`, value: result.durationMs, unit: "ms", better: "lower" });
  }
  const found: Array<[string, number]> = [];
  for (const [k, v] of Object.entries(result.metrics ?? {})) leaves(v, slug(k), found);
  for (const [path, value] of found) {
    const kind = classify(path);
    if (kind) out.push({ metric: `${base}.${path}`, value, ...kind });
  }
  const seen = new Set(out.map((m) => m.metric));
  for (const s of result.samples ?? []) {
    if (s.error || !isNum(s.durationMs) || s.durationMs <= 0) continue;
    const metric = `${base}.sample.${slug(s.name)}`;
    // A sample name repeated in one run: keep the first.
    if (seen.has(metric)) continue;
    seen.add(metric);
    out.push({ metric, value: s.durationMs, unit: "ms", better: "lower" });
  }
  return out;
}

/** Write the scenario's numbers when PERF_RESULTS_DIR is set; never fails the test. */
export function writePerfResults(id: string, result: ScenarioResult, dir = process.env.PERF_RESULTS_DIR): string | null {
  if (!dir) return null;
  try {
    mkdirSync(dir, { recursive: true });
    const file = join(dir, `sfu-${slug(id)}.json`);
    writeFileSync(file, JSON.stringify({ scenario: id, passed: result.passed, metrics: perfMetrics(id, result) }, null, 1));
    return file;
  } catch (e) {
    console.warn(`[${id}] perf results not written: ${(e as Error).message}`);
    return null;
  }
}
