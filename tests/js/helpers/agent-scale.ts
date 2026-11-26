/**
 * Pure helpers of the agent-scale wind tunnel (tests/agent-scale.test.ts): percentiles, the
 * growth of a metric between the smallest and the largest agent count, the flags, the results
 * file for the nightly history (`scripts/perf/record`, see the header of scripts/perf/perf.mjs)
 * and the table the test prints. No executor, no clock: `agent-scale.unit.test.ts` tests them.
 *
 * Names below have no prefix; the results file adds `agent_scale.`.
 */

import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

/**
 * What a metric measures decides how it may grow with the agent count:
 *  - `latency`: the time of one call, or a cost per agent. Flat is good; growth above
 *    `LATENCY_GROWTH_FLAG` between the smallest and the largest count is flagged.
 *  - `total`: the wall time of all agents together, or a resource. Linear growth is expected;
 *    growth above `TOTAL_GROWTH_FLAG` (1.5 x linear at 10 to 100 agents) is flagged.
 *  - `count`: a number that must stay what it is (leaked events). No growth is computed.
 */
export type MetricKind = "latency" | "total" | "count";

export interface Reading {
  value: number;
  unit: string;
  kind: MetricKind;
}

/** Readings by agent count, then by metric name. */
export type Readings = Map<number, Map<string, Reading>>;

export const LATENCY_GROWTH_FLAG = 2;
export const TOTAL_GROWTH_FLAG = 15;

export interface HistoryMetric {
  metric: string;
  value: number;
  unit: string;
  better: "lower";
  params: Record<string, number>;
}

/** The agent counts of `AGENT_SCALE_COUNTS`: positive integers, ascending, no repeats. */
export function parseCounts(raw: string | undefined, fallback = "10,20,50,100"): number[] {
  const text = raw && raw.trim() ? raw : fallback;
  const counts = text.split(",").map((part) => {
    const n = Number(part.trim());
    if (!Number.isInteger(n) || n < 1) throw new Error(`AGENT_SCALE_COUNTS: "${part}" is not a positive integer`);
    return n;
  });
  return [...new Set(counts)].sort((a, b) => a - b);
}

/** The p-th percentile (0 to 100) by nearest rank; NaN for no values. */
export function percentile(values: readonly number[], p: number): number {
  if (values.length === 0) return NaN;
  const sorted = [...values].sort((a, b) => a - b);
  const rank = Math.ceil((p / 100) * sorted.length);
  return sorted[Math.min(sorted.length - 1, Math.max(0, rank - 1))];
}

export const median = (values: readonly number[]): number => percentile(values, 50);

/** The value at the largest count over the value at the smallest; null when it is not defined. */
export function growth(first: number, last: number): number | null {
  if (!Number.isFinite(first) || !Number.isFinite(last) || first <= 0) return null;
  return last / first;
}

/** True when `ratio` is past the limit of the kind. A `count` and an undefined ratio never are. */
export function flagged(kind: MetricKind, ratio: number | null): boolean {
  if (ratio === null || kind === "count") return false;
  return ratio > (kind === "latency" ? LATENCY_GROWTH_FLAG : TOTAL_GROWTH_FLAG);
}

export interface GrowthRow {
  metric: string;
  kind: MetricKind;
  unit: string;
  from: number;
  to: number;
  growth: number | null;
  flag: boolean;
}

/** The growth of every metric that both the smallest and the largest count measured. */
export function growthRows(readings: Readings): GrowthRow[] {
  const counts = [...readings.keys()].sort((a, b) => a - b);
  if (counts.length < 2) return [];
  const from = counts[0];
  const to = counts[counts.length - 1];
  const rows: GrowthRow[] = [];
  for (const [metric, low] of readings.get(from)!) {
    const high = readings.get(to)!.get(metric);
    if (!high) continue;
    const ratio = growth(low.value, high.value);
    rows.push({ metric, kind: low.kind, unit: low.unit, from, to, growth: ratio, flag: flagged(low.kind, ratio) });
  }
  return rows;
}

/** The name of a growth metric: `rss_peak_mib` → `rss_peak_mib.growth_10_100`. */
export const growthName = (metric: string, from: number, to: number): string => `${metric}.growth_${from}_${to}`;

/** Every reading as a history metric (`params: {agents}`), then each growth (`params: {from, to}`). */
export function historyMetrics(readings: Readings): HistoryMetric[] {
  const metrics: HistoryMetric[] = [];
  for (const [agents, byName] of [...readings].sort((a, b) => a[0] - b[0])) {
    for (const [name, r] of byName) {
      if (!Number.isFinite(r.value)) continue;
      metrics.push({ metric: `agent_scale.${name}`, value: r.value, unit: r.unit, better: "lower", params: { agents } });
    }
  }
  for (const row of growthRows(readings)) {
    if (row.growth === null || row.kind === "count") continue;
    metrics.push({
      metric: `agent_scale.${growthName(row.metric, row.from, row.to)}`,
      value: row.growth,
      unit: "ratio",
      better: "lower",
      params: { from: row.from, to: row.to },
    });
  }
  return metrics;
}

/** Write `<dir>/agent-scale.json`, the results file of the history. Returns its path. */
export function writeResults(dir: string, readings: Readings): string {
  mkdirSync(dir, { recursive: true });
  const file = join(dir, "agent-scale.json");
  writeFileSync(file, JSON.stringify({ metrics: historyMetrics(readings) }, null, 2) + "\n");
  return file;
}

const fmt = (value: number | undefined): string => {
  if (value === undefined || !Number.isFinite(value)) return "-";
  if (Math.abs(value) >= 100) return Math.round(value).toString();
  return value.toFixed(value % 1 === 0 ? 0 : 2);
};

/** Every metric by agent count, the growth between the smallest and the largest, and its flag. */
export function formatTable(readings: Readings): string {
  const counts = [...readings.keys()].sort((a, b) => a - b);
  const names: string[] = [];
  for (const byName of readings.values()) for (const name of byName.keys()) if (!names.includes(name)) names.push(name);
  const rows = new Map(growthRows(readings).map((r) => [r.metric, r]));
  const header = ["metric", "unit", ...counts.map((n) => `${n}`), counts.length > 1 ? `growth_${counts[0]}_${counts[counts.length - 1]}` : "growth", "flag"];
  const lines: string[][] = [header];
  for (const name of names) {
    const row = rows.get(name);
    const any = [...readings.values()].map((m) => m.get(name)).find((r) => r !== undefined)!;
    lines.push([
      name,
      any.unit,
      ...counts.map((n) => fmt(readings.get(n)?.get(name)?.value)),
      row?.growth == null ? "-" : fmt(row.growth),
      row?.flag ? "SCALES POORLY" : "",
    ]);
  }
  const widths = header.map((_, col) => Math.max(...lines.map((l) => l[col].length)));
  return lines.map((l) => l.map((cell, col) => (col < 2 ? cell.padEnd(widths[col]) : cell.padStart(widths[col]))).join("  ").trimEnd()).join("\n");
}

/** VmRSS (MiB) and threads of a `/proc/<pid>/status` text. */
export function parseStatus(text: string): { rssMib: number; threads: number } {
  const num = (key: string): number => {
    const m = new RegExp(`^${key}:\\s+(\\d+)`, "m").exec(text);
    if (!m) throw new Error(`no ${key} in /proc status`);
    return Number(m[1]);
  };
  return { rssMib: num("VmRSS") / 1024, threads: num("Threads") };
}

/** utime + stime (clock ticks) of a `/proc/<pid>/stat` text. The command is in parentheses and may hold spaces. */
export function parseCpuTicks(text: string): number {
  const rest = text.slice(text.lastIndexOf(")") + 2).split(" ");
  // `rest` starts at field 3 (state): utime is field 14, stime field 15.
  const utime = Number(rest[11]);
  const stime = Number(rest[12]);
  if (!Number.isFinite(utime) || !Number.isFinite(stime)) throw new Error("no utime/stime in /proc stat");
  return utime + stime;
}

/** CPU use over a window, in percent of one core (a busy core is 100). */
export function cpuPercent(ticksBefore: number, ticksAfter: number, seconds: number, ticksPerSecond = 100): number {
  return ((ticksAfter - ticksBefore) / ticksPerSecond / seconds) * 100;
}
