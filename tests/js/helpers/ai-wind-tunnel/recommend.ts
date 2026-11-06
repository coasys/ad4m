/**
 * Recommendations from evidence. Pure: evidence in, ai-wind-tunnel/recommendations.json
 * out. Per hardware class and service, among the cells that passed every check and fit
 * the machine's memory, the candidates that clear the service's floor (fast enough to
 * use), ranked by the service's order (bigger or more accurate first, then faster).
 * A key whose newest run failed, and a cell measured through a harness-only front, is no
 * candidate. A class with no evidence, or no candidate over the floor, gets no recommendation.
 */

import { medianOf } from "./stats.js";
import type { LoadedEvidence } from "./evidence.js";
import { RECOMMENDATIONS_SCHEMA_VERSION, type CellResult, type ProviderId, type Service } from "./types.js";

type Medians = Record<string, number>;

export interface ServiceRule {
  /** What a candidate must reach, by metric median. */
  floor: { metric: string; atMost?: number; atLeast?: number }[];
  /**
   * Sort keys, the first deciding: `desc` larger first. `params` is the model size. `round`
   * buckets a metric (WER 0.00-0.0199 is one bucket at 0.02), so noise below it does not decide.
   */
  order: { metric: string; dir: "asc" | "desc"; round?: number }[];
  /** The metrics a recommendation reports. */
  report: string[];
}

export const SERVICE_RULES: Record<Service, ServiceRule> = {
  llm: {
    floor: [
      { metric: "decodeTps", atLeast: 15 },
      { metric: "ttftMs", atMost: 2000 },
    ],
    order: [
      { metric: "params", dir: "desc" },
      { metric: "decodeTps", dir: "desc" },
    ],
    report: ["ttftMs", "decodeTps", "prefillTps", "latencyMs", "throughputTpsC4", "firstRequestMs", "peakRssBytes", "peakGpuBytes", "executorOverheadMs"],
  },
  completion: {
    floor: [{ metric: "latencyMs", atMost: 3000 }],
    order: [
      { metric: "params", dir: "desc" },
      { metric: "latencyMs", dir: "asc" },
    ],
    report: ["latencyMs", "firstRequestMs", "executorOverheadMs"],
  },
  embeddings: {
    floor: [
      { metric: "textsPerSec", atLeast: 10 },
      { metric: "mrr", atLeast: 1 },
    ],
    order: [
      { metric: "textsPerSec", dir: "desc" },
    ],
    report: ["textsPerSec", "batchLatencyMs", "singleLatencyMs", "dimension", "mrr", "peakRssBytes", "executorOverheadMs"],
  },
  stt: {
    floor: [{ metric: "rtf", atMost: 0.5 }],
    order: [
      { metric: "werMean", dir: "asc", round: 0.02 },
      { metric: "rtf", dir: "asc" },
    ],
    report: ["werMean", "rtf", "latencyMs", "streamLatencyMs", "firstRequestMs", "peakRssBytes"],
  },
  tts: {
    // Faster than playback: speech can start before the sentence is synthesised.
    floor: [{ metric: "rtf", atMost: 1 }],
    order: [
      { metric: "roundTripWer", dir: "asc", round: 0.02 },
      { metric: "rtf", dir: "asc" },
    ],
    report: ["rtf", "ttfbMs", "latencyMs", "roundTripWer", "firstRequestMs", "peakRssBytes"],
  },
};

export interface Recommendation {
  provider: ProviderId;
  modelId?: string;
  remoteModel?: string;
  runtimeBuild?: string;
  backend?: string;
  /** The flags the runtime was started with when it was measured. */
  serverFlags?: string[];
  /** Medians across the cited evidence (the median of each file's median). */
  metrics: Medians;
  evidence: string[];
  /** How many candidates were measured for this class and service, and how many cleared the floor. */
  candidates: number;
  overFloor: number;
}

export interface Recommendations {
  schemaVersion: number;
  generatedAt: string;
  rules: Record<Service, ServiceRule>;
  classes: Record<string, Partial<Record<Service, Recommendation>>>;
}

/** "0.8B" -> 8e8, "137M" -> 1.37e8. */
export function parseParams(p: string | undefined): number | null {
  const m = p?.match(/^([\d.]+)\s*([KMBT])?$/i);
  if (!m) return null;
  const mult = { K: 1e3, M: 1e6, B: 1e9, T: 1e12 }[(m[2] ?? "").toUpperCase() as "K"] ?? 1;
  return Number(m[1]) * mult;
}

function median(xs: number[]): number {
  const s = [...xs].sort((a, b) => a - b);
  const mid = Math.floor(s.length / 2);
  return s.length % 2 ? s[mid] : (s[mid - 1] + s[mid]) / 2;
}

function cellKey(c: CellResult): string {
  return [c.provider, c.runtimeBuild ?? "", c.backend ?? "", c.model?.modelId ?? c.model?.remoteModel ?? ""].join("|");
}

interface Candidate {
  cell: CellResult;
  medians: Medians;
  evidence: string[];
}

function candidates(entries: { path: string; cell: CellResult }[]): Candidate[] {
  const groups = new Map<string, { path: string; cell: CellResult }[]>();
  for (const e of entries) {
    const k = cellKey(e.cell);
    groups.set(k, [...(groups.get(k) ?? []), e]);
  }
  return [...groups.values()].map((g) => {
    const names = new Set(g.flatMap((e) => Object.keys(e.cell.metrics)));
    const medians: Medians = {};
    for (const name of names) {
      const xs = g.map((e) => medianOf(e.cell.metrics[name])).filter((x): x is number => x !== null);
      if (xs.length) medians[name] = Math.round(median(xs) * 1000) / 1000;
    }
    const params = parseParams(g[0].cell.model?.params);
    if (params !== null) medians.params = params;
    return { cell: g[g.length - 1].cell, medians, evidence: [...new Set(g.map((e) => e.path))].sort() };
  });
}

function overFloor(c: Candidate, rule: ServiceRule): boolean {
  return rule.floor.every((f) => {
    const v = c.medians[f.metric];
    if (v === undefined) return false;
    return (f.atMost === undefined || v <= f.atMost) && (f.atLeast === undefined || v >= f.atLeast);
  });
}

function compare(a: Candidate, b: Candidate, rule: ServiceRule): number {
  for (const o of rule.order) {
    const q = (x: number | undefined) => (x === undefined ? undefined : o.round ? Math.floor(x / o.round + 1e-9) : x);
    const va = q(a.medians[o.metric]);
    const vb = q(b.medians[o.metric]);
    if (va === vb) continue;
    if (va === undefined) return 1;
    if (vb === undefined) return -1;
    return o.dir === "desc" ? vb - va : va - vb;
  }
  return a.evidence[0].localeCompare(b.evidence[0]);
}

/**
 * A cell measured through something the harness put in front of the server (an
 * OpenAI-shaped front, `metrics.adapter`) shows what server and front cost together; a
 * local setup without that front cannot use the route. "none: ..." names the server's own route.
 */
function throughAdapter(c: CellResult): boolean {
  const a = c.metrics.adapter;
  return typeof a === "string" && !a.startsWith("none");
}

export function deriveRecommendations(all: LoadedEvidence[], generatedAt: string, rules = SERVICE_RULES): Recommendations {
  // The newest evidence that ran a key (pass or fail) decides it: a key whose newest run failed is out.
  const newest = new Map<string, { at: string; path: string; pass: boolean }>();
  for (const { path, evidence } of all) {
    for (const cell of evidence.cells) {
      if (cell.status !== "pass" && cell.status !== "fail") continue;
      const k = `${evidence.hardwareClass}|${cell.service}|${cellKey(cell)}`;
      const n = newest.get(k);
      const order = n ? evidence.finishedAt.localeCompare(n.at) || path.localeCompare(n.path) : 1;
      if (order > 0) newest.set(k, { at: evidence.finishedAt, path, pass: cell.status === "pass" });
      else if (order === 0 && cell.status === "fail") n!.pass = false;
    }
  }
  const byClass = new Map<string, { path: string; cell: CellResult }[]>();
  for (const { path, evidence } of all) {
    for (const cell of evidence.cells) {
      if (cell.status !== "pass" || cell.fit?.fits === false || throughAdapter(cell)) continue;
      if (!newest.get(`${evidence.hardwareClass}|${cell.service}|${cellKey(cell)}`)!.pass) continue;
      byClass.set(evidence.hardwareClass, [...(byClass.get(evidence.hardwareClass) ?? []), { path, cell }]);
    }
  }
  const classes: Recommendations["classes"] = {};
  for (const cls of [...byClass.keys()].sort()) {
    const entries = byClass.get(cls)!;
    for (const service of Object.keys(rules) as Service[]) {
      const rule = rules[service];
      const cands = candidates(entries.filter((e) => e.cell.service === service));
      if (cands.length === 0) continue;
      const ok = cands.filter((c) => overFloor(c, rule)).sort((a, b) => compare(a, b, rule));
      if (ok.length === 0) continue;
      const best = ok[0];
      const metrics: Medians = {};
      for (const m of rule.report) if (best.medians[m] !== undefined) metrics[m] = best.medians[m];
      const rec: Recommendation = {
        provider: best.cell.provider,
        ...(best.cell.model?.modelId ? { modelId: best.cell.model.modelId } : { remoteModel: best.cell.model?.remoteModel }),
        ...(best.cell.runtimeBuild ? { runtimeBuild: best.cell.runtimeBuild } : {}),
        ...(best.cell.backend ? { backend: best.cell.backend } : {}),
        ...(best.cell.serverFlags ? { serverFlags: best.cell.serverFlags } : {}),
        metrics,
        evidence: best.evidence,
        candidates: cands.length,
        overFloor: ok.length,
      };
      (classes[cls] ??= {})[service] = rec;
    }
  }
  return { schemaVersion: RECOMMENDATIONS_SCHEMA_VERSION, generatedAt, rules, classes };
}
