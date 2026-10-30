/**
 * Runs a scenario's routes: one verify script each, in order. A route passes
 * when its script exits 0 and prints its PASS line, skips when it prints SKIP
 * (a missing harness image), and fails otherwise. Each route's output goes to
 * `<logDir>/<scenario>-<route>.log` and to the console; a failing script saves
 * its pod's container logs in `logDir` (AGENT_HARNESS_LOG_DIR, see lib.sh).
 */
import { spawnSync } from "child_process";
import { mkdirSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { RouteStatus, ScenarioContext, ScenarioResult } from "./scenario.js";

const HARNESS = resolve(dirname(fileURLToPath(import.meta.url)), "..");

export interface Route {
  name: string;
  /** Relative to tests/agent-harness. */
  script: string;
  passRe: RegExp;
  timeoutMs: number;
  /** Route metrics from the script's output, stored as `<name><Key>`. */
  metrics?: (out: string) => Record<string, number | string>;
}

export interface RoutesOutcome {
  metrics: Record<string, any>;
  samples: ScenarioResult["samples"];
  passed: string[];
  skipped: string[];
  failed: string[];
}

export function runRoutes(scenarioId: string, routes: Route[], ctx: ScenarioContext): RoutesOutcome {
  mkdirSync(ctx.logDir, { recursive: true });
  const outcome: RoutesOutcome = { metrics: {}, samples: [], passed: [], skipped: [], failed: [] };
  for (const route of routes) {
    console.log(`\n[${scenarioId}] ── route ${route.name}: ${route.script}`);
    const t0 = performance.now();
    const r = spawnSync("bash", [join(HARNESS, route.script)], {
      encoding: "utf8",
      timeout: route.timeoutMs,
      env: { ...process.env, AGENT_HARNESS_LOG_DIR: ctx.logDir },
      maxBuffer: 64 * 1024 * 1024,
    });
    const out = (r.stdout ?? "") + (r.stderr ?? "") + (r.error ? `\n[runner] ${r.error.message}\n` : "");
    writeFileSync(join(ctx.logDir, `${scenarioId}-${route.name}.log`), out);
    process.stdout.write(out.replace(/\n$/, "").replace(/^/gm, "  ") + "\n");

    const status: RouteStatus =
      r.status === 0 && route.passRe.test(out) ? "pass" : r.status === 0 && /\bSKIP\b/.test(out) ? "skip" : "fail";
    console.log(`[${scenarioId}] route ${route.name}: ${status.toUpperCase()}`);
    outcome.metrics[route.name] = status;
    for (const [k, v] of Object.entries(route.metrics?.(out) ?? {})) {
      outcome.metrics[`${route.name}${k[0].toUpperCase()}${k.slice(1)}`] = v;
    }
    outcome.samples.push({
      name: route.name,
      durationMs: performance.now() - t0,
      timestamp: Date.now(),
      error: status === "fail" ? `exit ${r.status ?? r.signal}; see ${scenarioId}-${route.name}.log` : undefined,
    });
    ({ pass: outcome.passed, skip: outcome.skipped, fail: outcome.failed })[status].push(route.name);
  }
  return outcome;
}

/** The METRICS JSON line a driver prints (`[tag] METRICS {...}`), or {}. */
export function metricsLine(out: string): Record<string, any> {
  const m = out.match(/METRICS (\{.*\})/);
  if (!m) return {};
  try {
    return JSON.parse(m[1]);
  } catch {
    return {};
  }
}

/** A scenario passes when at least one route passed and none failed. */
export function scenarioResult(
  id: string,
  scenario: string,
  label: string,
  startTime: number,
  o: RoutesOutcome,
): ScenarioResult {
  const endTime = Date.now();
  const passed = o.passed.length > 0 && o.failed.length === 0;
  const parts = [
    o.passed.length ? `passed: ${o.passed.join(", ")}` : "",
    o.skipped.length ? `skipped: ${o.skipped.join(", ")}` : "",
    o.failed.length ? `FAILED: ${o.failed.join(", ")}` : "",
  ].filter(Boolean);
  return {
    id,
    scenario,
    startTime,
    endTime,
    durationMs: endTime - startTime,
    passed,
    metrics: o.metrics,
    samples: o.samples,
    summary: `${label} — ${o.passed.length === 0 && o.failed.length === 0 ? "nothing verified; " : ""}${parts.join("; ")}`,
  };
}
