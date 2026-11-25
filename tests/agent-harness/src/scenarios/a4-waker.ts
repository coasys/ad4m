/**
 * A4 — Waker (multi-harness).
 *
 * A live AD4M perspective subscription detects new channel messages and wakes
 * the agent through the harness's REAL wake ingress (no stub sink), which runs
 * an agent turn. Routes:
 *   - openclaw   core's WakerSubscriptionManager on the executor's
 *                generate_waker_query channel query, delivered by the
 *                @coasys/openclaw-ad4m plugin's postWake to OpenClaw's /hooks/wake.
 *   - hermes     the same waker, delivered to Hermes's signed /webhooks/wake.
 *   - sovereign  Sovereign's own in-server waker on a mention, whose presence
 *                agent replies into the channel (presence_reply_ad4m).
 *
 * openclaw and hermes assert detection (1 msg -> 1 wake), debounce coalescing
 * (3 rapid -> 1), liveness, real-ingress delivery and an agent turn; sovereign
 * asserts the wake and the reply landing. Hardened Docker pod per route.
 */
import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { Route, metricsLine, runRoutes, scenarioResult } from "../routes.js";

const wakeMetrics = (out: string) => {
  const m = metricsLine(out);
  const r: Record<string, number> = {};
  if (typeof m.postOk === "number") r.wakeDeliveries = m.postOk;
  if (typeof m.wakeLatencyMs === "number") r.wakeLatencyMs = m.wakeLatencyMs;
  return r;
};

const ROUTES: Route[] = [
  { name: "openclaw", script: "interop/agents/verify-a4-waker.sh", passRe: /\[a4\] PASS/, timeoutMs: 600_000, metrics: wakeMetrics },
  { name: "hermes", script: "interop/agents/verify-a4-hermes.sh", passRe: /\[a4h\] PASS/, timeoutMs: 1_200_000, metrics: wakeMetrics },
  { name: "sovereign", script: "interop/agents/verify-a4-sovereign.sh", passRe: /\[a4sv\] PASS/, timeoutMs: 520_000 },
];

export const a4Waker: Scenario = {
  id: "a4",
  name: "Waker (multi-harness)",
  description:
    "A live AD4M perspective subscription detects new channel messages and wakes the agent through each harness's real wake ingress, which runs a turn: OpenClaw (/hooks/wake), Hermes (signed /webhooks/wake), and Sovereign (its native waker -> presence agent -> reply). Single-message wake, debounce coalescing, liveness, real-ingress delivery. Hardened Docker pod per route.",
  routes: ROUTES.map((r) => r.name),

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const startTime = Date.now();
    const outcome = runRoutes("a4", ROUTES, ctx);
    return scenarioResult("a4", "a4-waker", "A4 waker", startTime, outcome);
  },
};
