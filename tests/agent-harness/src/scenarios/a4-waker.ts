/**
 * A4 — Waker (multi-harness).
 *
 * Proves the waker integration surface end to end for each harness: a live AD4M
 * perspective subscription detects new channel messages and wakes the agent
 * through the harness's REAL wake ingress (no stub sink), which runs an agent
 * turn. Routes:
 *   - OpenClaw — real @coasys/openclaw-ad4m WakerSubscriptionManager + postWake
 *     to OpenClaw's real /hooks/wake endpoint.
 *   - Hermes   — the same real ad4m waker component delivering to Hermes's real
 *     signed /webhooks/wake ingress (gateway), which fires an agent turn.
 *   - Sovereign — a real ad4m subscription delivering the wake to Sovereign's
 *     real presence ingress (POST /api/chat/send, ad4m origin), which runs a
 *     presence agent turn.
 *
 * Each asserts detection (1 msg -> 1 wake), debounce coalescing (3 rapid -> 1),
 * liveness, and real-ingress delivery. Pod-managed (`managesOwnEnvironment`):
 * each self-contained, hardened verify script stands up its Docker pod and tears
 * it down. Nothing touches the host OS. A route whose script or image is absent
 * is skipped honestly.
 */

import { execSync } from "child_process";
import { existsSync } from "fs";
import { join } from "path";
import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";

const REPO = process.cwd(); // run.sh invokes the runner from the repo root

interface SubRun {
  name: string;
  script: string;
  passRe: RegExp;
}

const SUBRUNS: SubRun[] = [
  { name: "openclaw", script: "interop/agents/verify-a4-waker.sh", passRe: /\[a4\] PASS/ },
  { name: "hermes", script: "interop/agents/verify-a4-hermes.sh", passRe: /\[a4h\] PASS/ },
  { name: "sovereign", script: "interop/agents/verify-a4-sovereign.sh", passRe: /\[a4sv\] PASS/ },
];

export const a4Waker: Scenario = {
  id: "a4",
  name: "Waker (multi-harness)",
  description:
    "A live AD4M perspective subscription detects new channel messages and wakes the agent through each harness's real wake ingress, which runs a turn: OpenClaw (/hooks/wake), Hermes (signed /webhooks/wake), and Sovereign (presence ingress /api/chat/send). Single-message wake, debounce coalescing, liveness, real-ingress delivery. Self-managed, hardened Docker pod per route.",
  managesOwnEnvironment: true,

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, any> = {};
    let ran = 0;
    let passCount = 0;
    let allPassed = true;

    for (const sub of SUBRUNS) {
      const scriptPath = join(REPO, sub.script);
      if (!existsSync(scriptPath)) {
        metrics[sub.name] = "not-implemented";
        continue;
      }
      ran++;
      const t0 = performance.now();
      let out = "";
      let status: "pass" | "skip" | "fail";
      try {
        out = execSync(`bash "${scriptPath}"`, { encoding: "utf8", timeout: 520000, env: process.env });
        status = sub.passRe.test(out) ? "pass" : /\bSKIP\b/.test(out) ? "skip" : "fail";
      } catch (err: any) {
        out = (err.stdout?.toString?.() ?? "") + (err.stderr?.toString?.() ?? err.message ?? "");
        status = /\bSKIP\b/.test(out) ? "skip" : "fail";
      }
      metrics[sub.name] = status;
      const m = out.match(/METRICS (\{.*\})/);
      if (m) {
        try {
          const parsed = JSON.parse(m[1]);
          if (typeof parsed.postOk === "number") metrics[`${sub.name}WakeDeliveries`] = parsed.postOk;
          if (typeof parsed.wakeLatencyMs === "number") metrics[`${sub.name}WakeLatencyMs`] = parsed.wakeLatencyMs;
        } catch {
          /* ignore */
        }
      }
      samples.push({
        name: `${sub.name}-waker`,
        durationMs: performance.now() - t0,
        timestamp: Date.now(),
        error: status === "fail" ? "waker verify did not report PASS" : undefined,
      });
      if (status === "pass") passCount++;
      if (status === "fail") allPassed = false;
    }

    const endTime = Date.now();
    const passed = passCount > 0 && allPassed;
    const passedRoutes = Object.entries(metrics)
      .filter(([, v]) => v === "pass")
      .map(([k]) => k)
      .join(" + ");
    return {
      scenario: "a4-waker",
      branch: ctx.branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed,
      metrics,
      samples,
      summary: passed
        ? `A4 waker — real subscription -> real harness ingress: verified ${passedRoutes}`
        : ran === 0
          ? "A4 waker — no sub-run scripts available"
          : "A4 waker — one or more routes failed",
    };
  },
};
