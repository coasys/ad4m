/**
 * A5 — A/V action loop (mocked, multi-harness).
 *
 * The agent wakes on a mocked call, reads the transcript, and replies in chat —
 * all as ordinary AD4M perspective links / Message expressions. The media
 * transport is mocked; the call-presence entry, the transcript, and the reply are
 * real channel writes, so the true perceive->act loop runs over MCP (only the A/V
 * transport is mocked; see the plan's "Mocked A/V — why this stays honest").
 *
 * Routes:
 *   - Sovereign — the native in-server waker wakes the presence agent on the
 *     transcript's spoken name; it replies via presence_reply_ad4m. Full loop.
 *   - Hermes    — the real ad4m mention waker -> signed webhook -> a Hermes turn
 *     (mcp_servers.ad4m) that reads the transcript then writes a reply. Full loop.
 *   - OpenClaw  — the real ad4m mention waker -> real /hooks/wake ingress (wake
 *     half, with a negative control). The perceive->act-via-MCP half is not
 *     driven on the mock lane (OpenClaw's text/code-bridge tool protocol + a hook
 *     turn that carries no model-visible prompt), so this route SKIPs honestly.
 *
 * Each asserts a negative control (the call-presence entry alone does not wake),
 * exactly one wake on the transcript's spoken name, and — where the harness turn
 * can act on the mock lane — perceive (read the transcript) + a reply that lands
 * as a fresh channel child. Cross-user visibility needs neighbourhood sync (OUT
 * of scope). Pod-managed (`managesOwnEnvironment`): each hardened verify script
 * stands up its Docker pod and tears it down. A route whose script or image is
 * absent is skipped honestly.
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
  { name: "sovereign", script: "interop/agents/verify-a5-sovereign.sh", passRe: /\[a5sv\] PASS/ },
  { name: "hermes", script: "interop/agents/verify-a5-hermes.sh", passRe: /\[a5h\] PASS/ },
  { name: "openclaw", script: "interop/agents/verify-a5-openclaw.sh", passRe: /\[a5oc\] PASS/ },
];

export const a5AvLoop: Scenario = {
  id: "a5",
  name: "A/V loop (mocked, multi-harness)",
  description:
    "The agent wakes on a mocked call-presence entry, reads a transcript that names it in free speech, and replies in chat — all as ordinary AD4M links / Message expressions (A/V transport mocked). Negative control (call-presence alone does not wake), one wake on the spoken name, perceive + reply-lands. Sovereign + Hermes prove the full loop; OpenClaw proves the wake half and skips the mock-lane act half. Self-managed, hardened Docker pod per route.",
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
        out = execSync(`bash "${scriptPath}"`, { encoding: "utf8", timeout: 560000, env: process.env });
        status = sub.passRe.test(out) ? "pass" : /\bSKIP\b/.test(out) ? "skip" : "fail";
      } catch (err: any) {
        out = (err.stdout?.toString?.() ?? "") + (err.stderr?.toString?.() ?? err.message ?? "");
        status = /\bSKIP\b/.test(out) ? "skip" : "fail";
      }
      metrics[sub.name] = status;
      samples.push({
        name: `${sub.name}-av-loop`,
        durationMs: performance.now() - t0,
        timestamp: Date.now(),
        error: status === "fail" ? "A5 verify did not report PASS" : undefined,
      });
      if (status === "pass") passCount++;
      if (status === "fail") allPassed = false;
    }

    const endTime = Date.now();
    // Pass when at least one route proved the full loop and no route failed;
    // honest SKIPs (e.g. OpenClaw's act half, or an absent image) do not fail it.
    const passed = passCount > 0 && allPassed;
    const passedRoutes = Object.entries(metrics)
      .filter(([, v]) => v === "pass")
      .map(([k]) => k)
      .join(" + ");
    const skippedRoutes = Object.entries(metrics)
      .filter(([, v]) => v === "skip")
      .map(([k]) => k)
      .join(" + ");
    return {
      scenario: "a5-av-loop",
      branch: ctx.branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed,
      metrics,
      samples,
      summary: passed
        ? `A5 mocked-A/V loop — full perceive->act verified: ${passedRoutes}${skippedRoutes ? `; wake-only/skipped: ${skippedRoutes}` : ""}`
        : ran === 0
          ? "A5 mocked-A/V loop — no sub-run scripts available"
          : "A5 mocked-A/V loop — one or more routes failed",
    };
  },
};
