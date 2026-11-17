// The trend page of scripts/perf/report: static HTML, one SVG chart per job + metric + params,
// nothing loaded from elsewhere. See the header of scripts/perf/perf.mjs.

import { fmt, formatParams, median, paramsKey, passingRuns, seriesKey, verdicts } from "./lib.mjs";

const AD4M_COMMIT = "https://github.com/coasys/ad4m/commit/";

const esc = (s) => String(s).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c]);

/** One job's series (metric + params) over its last `runs` runs, points oldest first. */
export function seriesOf(records, checks, runs) {
  const verdictOf = verdicts(checks);
  // The series each run's last check found regressed.
  const regressedIn = new Map(checks.filter((c) => c.run_id).map((c) => [c.run_id, new Set((c.regressions ?? []).map(seriesKey))]));
  const runOrder = [...new Map(records.map((r) => [r.run_id, r.run_at])).entries()].sort((a, b) => Date.parse(a[1]) - Date.parse(b[1]));
  const keep = new Set(runOrder.slice(-runs).map(([id]) => id));
  const passing = new Set(passingRuns(records, verdictOf).map((r) => r.run_id));
  const map = new Map();
  for (const r of records) {
    if (!keep.has(r.run_id)) continue;
    const key = seriesKey(r);
    if (!map.has(key)) map.set(key, { metric: r.metric, params: r.params ?? {}, unit: r.unit, better: r.better, points: [] });
    const regressed = verdictOf.get(r.run_id) === "regressed" && (regressedIn.get(r.run_id)?.has(key) ?? false);
    map.get(key).points.push({ ...r, cls: regressed ? "regressed" : r.status === "failed" ? "failed" : "passed" });
  }
  const out = [...map.values()];
  for (const s of out) {
    s.points.sort((a, b) => Date.parse(a.run_at) - Date.parse(b.run_at));
    s.last = s.points.at(-1);
    s.baseline = median(s.points.filter((p) => p !== s.last && passing.has(p.run_id)).slice(-7).map((p) => p.value));
  }
  return out.sort((a, b) => a.metric.localeCompare(b.metric) || paramsKey(a.params).localeCompare(paramsKey(b.params)));
}

function chart(s) {
  const [W, H, L, R, T, B] = [560, 150, 56, 10, 10, 22];
  const vals = s.points.map((p) => p.value).concat(s.baseline ?? []);
  let lo = Math.min(...vals);
  let hi = Math.max(...vals);
  if (lo > 0 && lo < hi * 0.5) lo = 0;
  if (hi === lo) [lo, hi] = [lo === 0 ? 0 : lo * 0.9, hi === 0 ? 1 : hi * 1.1];
  hi += (hi - lo) * 0.08;
  const n = s.points.length;
  const x = (i) => (L + (n === 1 ? (W - L - R) / 2 : (i * (W - L - R)) / (n - 1))).toFixed(1);
  const y = (v) => (T + (H - T - B) * (1 - (v - lo) / (hi - lo))).toFixed(1);
  const parts = [`<svg viewBox="0 0 ${W} ${H}" width="${W}" height="${H}" role="img" aria-label="${esc(s.metric)}">`];
  for (const v of [lo, (lo + hi) / 2, hi]) {
    parts.push(`<line class="grid" x1="${L}" x2="${W - R}" y1="${y(v)}" y2="${y(v)}"/><text class="axis" x="${L - 4}" y="${Number(y(v)) + 3}" text-anchor="end">${esc(fmt(v))}</text>`);
  }
  if (s.baseline !== null) parts.push(`<line class="baseline" x1="${L}" x2="${W - R}" y1="${y(s.baseline)}" y2="${y(s.baseline)}"/>`);
  if (n > 1) parts.push(`<polyline class="trend" points="${s.points.map((p, i) => `${x(i)},${y(p.value)}`).join(" ")}"/>`);
  s.points.forEach((p, i) => {
    const tip = `${fmt(p.value)} ${s.unit}\n${p.run_at} ${p.status}\nad4m ${String(p.ad4m_sha).slice(0, 9)}${p.we_sha ? ` we ${String(p.we_sha).slice(0, 9)}` : ""}\nrun ${p.run_id}`;
    parts.push(`<a href="${AD4M_COMMIT}${encodeURIComponent(p.ad4m_sha)}"><circle class="pt ${p.cls}" cx="${x(i)}" cy="${y(p.value)}" r="3.5"><title>${esc(tip)}</title></circle></a>`);
  });
  parts.push(`<text class="axis" x="${L}" y="${H - 6}">${esc(s.points[0].run_at.slice(0, 10))}</text><text class="axis" x="${W - R}" y="${H - 6}" text-anchor="end">${esc(s.last.run_at.slice(0, 10))}</text></svg>`);
  return parts.join("");
}

/** The page. `data`: [{job, records, checks}]; `runs`: how many of each job's newest runs to draw. */
export function renderReport(data, { runs = 90 } = {}) {
  const sections = data.map(({ job, records, checks }) => {
    const cards = seriesOf(records, checks, runs).map((s) => {
      const ps = formatParams(s.params);
      return `<div class="card"><h4>${esc(s.metric)} <span class="params">${esc(ps)}</span></h4><p class="meta">${esc(s.unit)}, ${esc(s.better)} is better. Newest ${esc(fmt(s.last.value))}, median before ${esc(fmt(s.baseline))}.</p>${chart(s)}</div>`;
    });
    return `<section><h2>${esc(job)}</h2><div class="grid">${cards.join("") || "<p>No records.</p>"}</div></section>`;
  });
  return `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>AD4M nightly performance</title>
<style>
body{font:14px/1.4 system-ui,sans-serif;margin:16px 24px;color:#1d1f23;background:#fafafa}
h2{font-size:17px;margin:24px 0 8px;border-bottom:1px solid #ccc}h4{margin:0 0 2px;font-size:13px;word-break:break-all}
.params{color:#666;font-weight:normal}.meta{color:#555;font-size:12px;margin:2px 0 6px}
.grid{display:flex;flex-wrap:wrap;gap:12px}.card{background:#fff;border:1px solid #ddd;border-radius:6px;padding:8px;width:576px}
svg{display:block}line.grid{stroke:#eee}.axis{font-size:10px;fill:#777}.trend{fill:none;stroke:#8aa4c8;stroke-width:1.5}
.baseline{stroke:#999;stroke-dasharray:4 3}.pt.passed{fill:#2a7de1}.pt.failed{fill:#aaa}.pt.regressed{fill:#d33}
</style></head><body>
<h1>AD4M nightly performance</h1>
<p class="meta">Each point is one run; click it to open its commit. Blue: passed. Grey: the run failed. Red: check found this metric regressed. Dashed: the median of the last 7 passing runs before the newest.</p>
${sections.join("\n")}
</body></html>
`;
}
