/**
 * A2 — Provision & Connect (multi-harness).
 *
 * A user instructs an AI agent harness to onboard to ADAM and end up with a
 * working, verified agent. Covered across harnesses + routes:
 *   - OpenClaw external, native MCP client (signup + login)
 *   - OpenClaw external, via the real @coasys/openclaw-ad4m plugin (ad4m-setup)
 *   - OpenClaw managed (download + install a fresh node, agent.generate)
 *   - Hermes external, native MCP client (mcp_servers.ad4m, Bearer JWT)
 *   - Sovereign external, Claude-Agent-SDK ad4m MCP injection
 *
 * This scenario is pod-managed (`managesOwnEnvironment`): it drives the
 * self-contained, hardened, fully-isolated verify scripts under
 * `interop/agents/`, each of which stands up its Docker pod (AD4M node + mock
 * LLM + the real harness), runs the onboarding turn, verifies the created
 * identity, and tears the pod down. Nothing touches the host OS.
 *
 * A sub-path whose script is absent is skipped honestly (recorded, not failed).
 */

import { execSync } from "child_process";
import { existsSync } from "fs";
import { join } from "path";
import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";

// run.sh invokes the runner from the repo root.
const REPO = process.cwd();

interface SubRun {
  name: string;
  script: string;
  passRe: RegExp;
}

const SUBRUNS: SubRun[] = [
  // external via OpenClaw's native MCP client (portable, harness-agnostic route)
  { name: "external-native", script: "interop/agents/verify-a2-openclaw.sh", passRe: /\[a2\] PASS/ },
  // external via the real @coasys/openclaw-ad4m plugin (ad4m-setup); regression-guards the plugin
  { name: "external-plugin", script: "interop/agents/verify-a2-openclaw-external-plugin.sh", passRe: /\[a2xp\] PASS/ },
  // managed (download+install a fresh node) via the plugin's managed mode
  { name: "managed", script: "interop/agents/verify-a2-openclaw-managed.sh", passRe: /\[a2m\] PASS/ },
  // Hermes harness — external, native MCP client (mcp_servers.ad4m, Bearer JWT)
  { name: "hermes", script: "interop/agents/verify-a2-hermes.sh", passRe: /\[a2h\] PASS/ },
  // Sovereign harness — external, Claude-Agent-SDK ad4m MCP injection
  { name: "sovereign", script: "interop/agents/verify-a2-sovereign.sh", passRe: /\[a2sv\] PASS/ },
];

export const a2ProvisionConnect: Scenario = {
  id: "a2",
  name: "Provision & Connect (multi-harness)",
  description:
    "AI agent harnesses onboard to ADAM and create + verify a distinct agent: OpenClaw (external native MCP, external via the @coasys/openclaw-ad4m plugin, and managed download+install), Hermes (external native MCP), and Sovereign (external Claude-Agent-SDK MCP injection). Self-managed, hardened Docker pod per route.",
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
        out = execSync(`bash "${scriptPath}"`, { encoding: "utf8", timeout: 360000, env: process.env });
        status = sub.passRe.test(out) ? "pass" : /\bSKIP\b/.test(out) ? "skip" : "fail";
      } catch (err: any) {
        out = (err.stdout?.toString?.() ?? "") + (err.stderr?.toString?.() ?? err.message ?? "");
        status = "fail";
      }
      const didMatch = out.match(/did[=:]\s*(did:key:[A-Za-z0-9]+)/i);
      const toolsMatch = out.match(/ad4m: (\d+) tools/);
      samples.push({
        name: `${sub.name}-onboard`,
        durationMs: performance.now() - t0,
        timestamp: Date.now(),
        error: status === "fail" ? "verify script did not report PASS" : undefined,
      });
      metrics[sub.name] = status;
      if (didMatch) metrics[`${sub.name}Did`] = didMatch[1];
      if (toolsMatch) metrics[`${sub.name}McpTools`] = Number(toolsMatch[1]);
      if (status === "pass") passCount++;
      if (status === "fail") allPassed = false;
    }

    const endTime = Date.now();
    const passed = passCount > 0 && allPassed;
    const passedPaths = Object.entries(metrics)
      .filter(([, v]) => v === "pass")
      .map(([k]) => k)
      .join(" + ");
    return {
      scenario: "a2-provision-connect",
      branch: ctx.branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed,
      metrics,
      samples,
      summary: passed
        ? `A2 provision & connect — verified: ${passedPaths} (harness → ADAM agent)`
        : ran === 0
          ? "A2 provision & connect — no sub-run scripts available"
          : "A2 provision & connect — one or more paths failed",
    };
  },
};
