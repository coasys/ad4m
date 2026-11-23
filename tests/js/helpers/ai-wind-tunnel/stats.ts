/** Summary statistics over repetitions. Pure. */

import type { Stat } from "./types.js";

/** The q-quantile (0..1) of sorted values, by linear interpolation between closest ranks. */
export function quantile(sorted: number[], q: number): number {
  if (sorted.length === 0) throw new Error("quantile of no values");
  if (sorted.length === 1) return sorted[0];
  const pos = (sorted.length - 1) * q;
  const lo = Math.floor(pos);
  const hi = Math.ceil(pos);
  return sorted[lo] + (sorted[hi] - sorted[lo]) * (pos - lo);
}

const round = (x: number) => (Math.abs(x) >= 100 ? Math.round(x * 10) / 10 : Math.round(x * 1000) / 1000);

/** Median, p95, min, max and mean of the finite values; null when there are none. */
export function summarize(values: Array<number | null | undefined>): Stat | null {
  const xs = values.filter((v): v is number => typeof v === "number" && Number.isFinite(v)).sort((a, b) => a - b);
  if (xs.length === 0) return null;
  return {
    n: xs.length,
    median: round(quantile(xs, 0.5)),
    p95: round(quantile(xs, 0.95)),
    min: round(xs[0]),
    max: round(xs[xs.length - 1]),
    mean: round(xs.reduce((a, b) => a + b, 0) / xs.length),
  };
}

/** The median of a metric that is a Stat or a plain number; null otherwise. */
export function medianOf(metric: unknown): number | null {
  if (typeof metric === "number") return metric;
  if (metric && typeof metric === "object" && typeof (metric as any).median === "number") return (metric as any).median;
  return null;
}
