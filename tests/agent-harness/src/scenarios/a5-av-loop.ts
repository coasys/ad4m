/**
 * A5 — A/V action loop (mocked, multi-harness).
 *
 * The agent wakes on a mocked call, reads the transcript, and replies in chat —
 * all as ordinary AD4M perspective links / Message expressions. The media
 * transport is mocked; the call-presence entry, the caller's transcript and the
 * reply are real channel writes, so the perceive->act loop runs over MCP.
 *
 * Routes:
 *   - sovereign  Sovereign's native in-server waker wakes the presence agent on
 *                the transcript's spoken name; it replies via presence_reply_ad4m.
 *   - hermes     core's WakerSubscriptionManager on the executor's mention
 *                subscription -> signed webhook -> a Hermes turn
 *                (mcp_servers.ad4m) that reads the transcript, then replies.
 *   - openclaw   the same waker -> OpenClaw's /hooks/wake, which runs a turn:
 *                the wake half only. OpenClaw's text/code-bridge tool protocol and
 *                a hook turn with no model-visible prompt keep the deterministic
 *                mock from driving its perceive->act half.
 *
 * Each asserts a negative control (the call-presence entry alone does not wake)
 * and exactly one wake on the transcript's spoken name; sovereign and hermes
 * also assert the reply landing as a fresh channel child (hermes: after reading
 * the transcript). Cross-user visibility needs neighbourhood sync (out of
 * scope). Hardened Docker pod per route.
 */
import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { Route, runRoutes, scenarioResult } from "../routes.js";

const ROUTES: Route[] = [
  { name: "sovereign", script: "interop/agents/verify-a5-sovereign.sh", passRe: /\[a5sv\] PASS/, timeoutMs: 520_000 },
  { name: "hermes", script: "interop/agents/verify-a5-hermes.sh", passRe: /\[a5h\] PASS/, timeoutMs: 1_200_000 },
  { name: "openclaw", script: "interop/agents/verify-a5-openclaw.sh", passRe: /\[a5oc\] PASS/, timeoutMs: 600_000 },
];

export const a5AvLoop: Scenario = {
  id: "a5",
  name: "A/V loop (mocked, multi-harness)",
  description:
    "The agent wakes on a mocked call-presence entry, reads a transcript that names it in free speech, and replies in chat — all as ordinary AD4M links / Message expressions (A/V transport mocked). Negative control (call-presence alone does not wake), one wake on the spoken name, perceive + reply-lands. Sovereign + Hermes prove the full loop; OpenClaw proves the wake half. Hardened Docker pod per route.",
  routes: ROUTES.map((r) => r.name),

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const startTime = Date.now();
    const outcome = runRoutes("a5", ROUTES, ctx);
    return scenarioResult("a5", "a5-av-loop", "A5 mocked-A/V loop", startTime, outcome);
  },
};
