/**
 * Scenario interface. Every agent-harness scenario manages its own environment:
 * it runs verify scripts that each stand up and tear down a Docker pod.
 */

export type RouteStatus = "pass" | "skip" | "fail";

export interface ScenarioResult {
  /** Short id (`a2`, `a4`, `a5`), as `--scenario` takes it. */
  id: string;
  scenario: string;
  startTime: number;
  endTime: number;
  durationMs: number;
  /** Hard pass/fail verdict — the runner gates on this field. */
  passed: boolean;
  /** Per route: its status, plus route metrics under `<route><Metric>` keys. */
  metrics: Record<string, any>;
  samples: Array<{ name: string; durationMs: number; timestamp: number; error?: string }>;
  summary: string;
}

export interface ScenarioContext {
  /** Directory for per-route output and, on failure, the pod's container logs. */
  logDir: string;
}

export interface Scenario {
  id: string;
  name: string;
  description: string;
  /** Route names, in run order: the keys of the route statuses in `metrics`. */
  routes: string[];
  run(ctx: ScenarioContext): Promise<ScenarioResult>;
}
