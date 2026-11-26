/**
 * Unit tests of the agent-scale wind tunnel's pure helpers (helpers/agent-scale.ts): counts,
 * percentiles, growth and flags, the results file, the table and the /proc readers. No
 * executor. `pnpm run test-perf-unit`.
 */

import { expect } from "chai";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  cpuPercent, flagged, formatTable, growth, growthRows, historyMetrics, median, parseCounts,
  parseCpuTicks, parseStatus, percentile, writeResults, type MetricKind, type Readings,
} from "../helpers/agent-scale.js";

function readings(spec: Record<number, Record<string, [number, MetricKind]>>): Readings {
  return new Map(Object.entries(spec).map(([n, byName]) => [
    Number(n),
    new Map(Object.entries(byName).map(([name, [value, kind]]) => [name, { value, unit: "ms", kind }])),
  ]));
}

describe("agent-scale helpers", () => {
  it("parses the agent counts, ascending and without repeats", () => {
    expect(parseCounts(undefined)).to.deep.equal([10, 20, 50, 100]);
    expect(parseCounts("")).to.deep.equal([10, 20, 50, 100]);
    expect(parseCounts("20, 10,20")).to.deep.equal([10, 20]);
    expect(() => parseCounts("10,x")).to.throw(/not a positive integer/);
    expect(() => parseCounts("0")).to.throw(/not a positive integer/);
    expect(() => parseCounts("1.5")).to.throw(/not a positive integer/);
  });

  it("takes percentiles by nearest rank", () => {
    const ten = [5, 1, 4, 2, 3, 10, 9, 8, 7, 6];
    expect(median(ten)).to.equal(5);
    expect(percentile(ten, 95)).to.equal(10);
    expect(percentile(ten, 0)).to.equal(1);
    expect(percentile([7], 95)).to.equal(7);
    expect(percentile(Array.from({ length: 100 }, (_, i) => i + 1), 95)).to.equal(95);
    expect(percentile([], 50)).to.be.NaN;
    // The input is not reordered.
    expect(ten[0]).to.equal(5);
  });

  it("computes growth and leaves it undefined for a zero or missing base", () => {
    expect(growth(2, 6)).to.equal(3);
    expect(growth(0, 6)).to.equal(null);
    expect(growth(NaN, 6)).to.equal(null);
    expect(growth(1, Infinity)).to.equal(null);
  });

  it("flags a latency above 2 and a total above 15, never a count", () => {
    expect(flagged("latency", 2)).to.equal(false);
    expect(flagged("latency", 2.01)).to.equal(true);
    expect(flagged("total", 15)).to.equal(false);
    expect(flagged("total", 15.5)).to.equal(true);
    expect(flagged("count", 1000)).to.equal(false);
    expect(flagged("latency", null)).to.equal(false);
  });

  it("builds growth rows between the smallest and the largest count", () => {
    const r = readings({
      100: { "write.link_batch_ms.median": [30, "latency"], "write.total_ms": [1000, "total"], "events.leaked": [0, "count"] },
      10: { "write.link_batch_ms.median": [10, "latency"], "write.total_ms": [100, "total"], "events.leaked": [0, "count"] },
      50: { "write.link_batch_ms.median": [20, "latency"] },
    });
    const rows = growthRows(r);
    const byName = Object.fromEntries(rows.map((row) => [row.metric, row]));
    expect(byName["write.link_batch_ms.median"]).to.include({ from: 10, to: 100, growth: 3, flag: true });
    expect(byName["write.total_ms"]).to.include({ growth: 10, flag: false });
    expect(byName["events.leaked"]).to.include({ growth: null, flag: false });
    expect(growthRows(readings({ 10: { a: [1, "total"] } }))).to.deep.equal([]);
  });

  it("makes history metrics with params, and growth metrics named by the counts", () => {
    const r = readings({
      10: { "read.query_ms.median": [4, "latency"], "events.leaked": [0, "count"], "x.nan": [NaN, "latency"] },
      20: { "read.query_ms.median": [6, "latency"], "events.leaked": [0, "count"], "x.nan": [NaN, "latency"] },
    });
    const metrics = historyMetrics(r);
    expect(metrics).to.deep.include({ metric: "agent_scale.read.query_ms.median", value: 4, unit: "ms", better: "lower", params: { agents: 10 } });
    expect(metrics).to.deep.include({ metric: "agent_scale.read.query_ms.median", value: 6, unit: "ms", better: "lower", params: { agents: 20 } });
    expect(metrics).to.deep.include({ metric: "agent_scale.read.query_ms.median.growth_10_20", value: 1.5, unit: "ratio", better: "lower", params: { from: 10, to: 20 } });
    // A count has no growth; a value that is not a number is not recorded at all.
    expect(metrics.some((m) => m.metric.includes("leaked.growth"))).to.equal(false);
    expect(metrics.some((m) => m.metric.includes("x.nan"))).to.equal(false);
    expect(metrics.every((m) => /^[A-Za-z0-9_-]+(\.[A-Za-z0-9_-]+)*$/.test(m.metric))).to.equal(true);
  });

  it("writes agent-scale.json in the results format", () => {
    const dir = mkdtempSync(join(tmpdir(), "agent-scale-unit-"));
    try {
      const file = writeResults(join(dir, "nested"), readings({ 10: { a: [1, "total"] }, 20: { a: [2, "total"] } }));
      expect(file).to.equal(join(dir, "nested", "agent-scale.json"));
      const parsed = JSON.parse(readFileSync(file, "utf8"));
      expect(parsed.metrics).to.have.length(3);
      expect(parsed.metrics[0]).to.deep.equal({ metric: "agent_scale.a", value: 1, unit: "ms", better: "lower", params: { agents: 10 } });
    } finally {
      rmSync(dir, { recursive: true, force: true });
    }
  });

  it("prints every metric by count with growth and the flag", () => {
    const table = formatTable(readings({
      10: { "read.query_ms.median": [5, "latency"], "events.leaked": [0, "count"] },
      100: { "read.query_ms.median": [50, "latency"], "events.leaked": [0, "count"] },
    }));
    const lines = table.split("\n");
    expect(lines[0]).to.match(/^metric\s+unit\s+10\s+100\s+growth_10_100\s+flag$/);
    expect(lines[1]).to.match(/^read\.query_ms\.median\s+ms\s+5\s+50\s+10\s+SCALES POORLY$/);
    expect(lines[2]).to.match(/^events\.leaked\s+ms\s+0\s+0\s+-$/);
  });

  it("reads /proc status and stat", () => {
    const status = "Name:\tad4m-executor\nVmRSS:\t  2048000 kB\nThreads:\t   57\n";
    expect(parseStatus(status)).to.deep.equal({ rssMib: 2000, threads: 57 });
    expect(() => parseStatus("Name:\tx\n")).to.throw(/no VmRSS/);
    // The command holds a space and a parenthesis; utime 700, stime 300 (fields 14 and 15).
    const stat = "123 (ad4m (exec) 1) S 1 123 123 0 -1 4194560 100 0 0 0 700 300 0 0 20 0 57 0 1000 1 2 3";
    expect(parseCpuTicks(stat)).to.equal(1000);
    expect(cpuPercent(1000, 1300, 30)).to.equal(10);
  });
});
