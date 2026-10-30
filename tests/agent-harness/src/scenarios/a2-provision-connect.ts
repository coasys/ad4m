/**
 * A2 — Provision & Connect (multi-harness).
 *
 * A user instructs an AI agent harness to onboard to ADAM and end up with a
 * working, verified agent. Routes:
 *   - external-native  OpenClaw's native MCP client (signup + login)
 *   - external-plugin  OpenClaw via the @coasys/openclaw-ad4m plugin (ad4m-setup)
 *   - managed          OpenClaw managed mode (fresh node + agent.generate, no network)
 *   - hermes           Hermes's native MCP client (mcp_servers.ad4m, Bearer JWT)
 *   - sovereign        Sovereign's Claude-Agent-SDK ad4m MCP injection
 *
 * Each route's verify script under `interop/agents/` stands up its hardened
 * Docker pod (AD4M node + mock LLM + the real harness), runs the onboarding
 * turn, verifies the created identity over MCP, and tears the pod down.
 */
import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { Route, runRoutes, scenarioResult } from "../routes.js";

const ROUTES: Route[] = [
  {
    name: "external-native",
    script: "interop/agents/verify-a2-openclaw.sh",
    passRe: /\[a2\] PASS/,
    timeoutMs: 360_000,
    metrics: (out): Record<string, number> => {
      const tools = out.match(/ad4m: (\d+) tools/);
      return tools ? { mcpTools: Number(tools[1]) } : {};
    },
  },
  { name: "external-plugin", script: "interop/agents/verify-a2-openclaw-external-plugin.sh", passRe: /\[a2xp\] PASS/, timeoutMs: 600_000 },
  { name: "managed", script: "interop/agents/verify-a2-openclaw-managed.sh", passRe: /\[a2m\] PASS/, timeoutMs: 900_000 },
  { name: "hermes", script: "interop/agents/verify-a2-hermes.sh", passRe: /\[a2h\] PASS/, timeoutMs: 1_200_000 },
  { name: "sovereign", script: "interop/agents/verify-a2-sovereign.sh", passRe: /\[a2sv\] PASS/, timeoutMs: 360_000 },
];

export const a2ProvisionConnect: Scenario = {
  id: "a2",
  name: "Provision & Connect (multi-harness)",
  description:
    "AI agent harnesses onboard to ADAM and create + verify a distinct agent: OpenClaw (external native MCP, external via the @coasys/openclaw-ad4m plugin, and managed install), Hermes (external native MCP), and Sovereign (external Claude-Agent-SDK MCP injection). Hardened Docker pod per route.",
  routes: ROUTES.map((r) => r.name),

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const startTime = Date.now();
    const outcome = runRoutes("a2", ROUTES, ctx);
    return scenarioResult("a2", "a2-provision-connect", "A2 provision & connect", startTime, outcome);
  },
};
