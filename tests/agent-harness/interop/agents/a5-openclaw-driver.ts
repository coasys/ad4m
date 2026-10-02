/**
 * A5 (OpenClaw) A/V-loop driver — mocked call, real perceive→act.
 *
 * Runs the REAL @coasys/openclaw-ad4m mention waker (WakerSubscriptionManager +
 * a live perspective SPARQL mention subscription) against a live executor, seeds
 * a mocked call (call-presence entry + a transcript naming the agent in free
 * speech) via the mock-av fixture, and delivers the wake to OpenClaw's REAL
 * `/hooks/wake` ingress (Bearer wake token, action=agent). The woken OpenClaw
 * turn (mock model + mcp.servers.ad4m) reads the transcript and replies into the
 * channel.
 *
 * Owns the DELIVERY half + the negative control:
 *   - the call-presence entry alone produces NO wake;
 *   - exactly one wake fires on the transcript's spoken name;
 *   - every wake reaches the hook (2xx);
 *   - the reply Message lands as a fresh channel child (child-has marker),
 *     written by the woken OpenClaw turn.
 * The verify script asserts PERCEIVE (the agent's read tool ran before the write,
 * and the transcript body returned to the model) from the mock's request log.
 *
 * Env: AD4M_PLUGIN_DIR, A5_WS, A5_MCP, A5_ADMIN, A5_HOOK, A5_WAKE_TOKEN,
 * A5_UUID, A5_CHAN, A5_TRANSCRIPT, A5_MARKER, A5_NAME. Prints `[a5oc] METRICS|PASS|FAIL`.
 */
import { fileURLToPath } from "url";
import { dirname, resolve } from "path";

const HARNESS = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..");
const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

(async () => {
  const PD = process.env.AD4M_PLUGIN_DIR;
  if (!PD) {
    console.log("[a5oc] FATAL AD4M_PLUGIN_DIR not set");
    process.exit(2);
  }
  const require = (await import("module")).createRequire(import.meta.url);
  const { Ad4mClient, QuerySubscriptionProxy } = require(`${PD}/node_modules/@coasys/ad4m`);
  const { WakerSubscriptionManager } = await import(`${PD}/wakerSubscriptionManager.ts`);
  const { McpClient } = await import(`${HARNESS}/src/mcp-client.ts`);
  const { seedCallPresence, seedTranscript, isCallActive } = await import(`${HARNESS}/interop/agents/mock-av.ts`);
  const { findChildWithMarker } = await import(`${HARNESS}/interop/agents/sovereign/waker-ad4m.ts`);

  const WS = process.env.A5_WS || "http://127.0.0.1:14472";
  const MCP = process.env.A5_MCP || "http://127.0.0.1:14471";
  const HOOK = process.env.A5_HOOK || "http://127.0.0.1:18791/hooks/wake";
  const WT = process.env.A5_WAKE_TOKEN || "a5-wake-tok";
  const TOKEN = process.env.A5_ADMIN || "windtunnel-admin";
  const UUID = process.env.A5_UUID!;
  const CHAN = process.env.A5_CHAN || "a5oc://channel";
  const TRANSCRIPT = process.env.A5_TRANSCRIPT || "On the call someone said: hey Aria, can you summarise the last point?";
  const MARKER = process.env.A5_MARKER || "A5OC_REPLY_OK";
  const NAME = process.env.A5_NAME || "Aria";
  const DEBOUNCE = 1000;

  async function postWake(text: string): Promise<number> {
    try {
      const res = await fetch(HOOK, {
        method: "POST",
        headers: { "Content-Type": "application/json", Authorization: `Bearer ${WT}` },
        body: JSON.stringify({ text, mode: "now" }),
        signal: AbortSignal.timeout(15000),
      });
      return res.status;
    } catch {
      return 0;
    }
  }

  const logger = { info: () => {}, warn: () => {}, error: () => {}, debug: () => {} };
  const client = new Ad4mClient(WS, TOKEN, true);
  const status = await client.agent.status();
  if (!status.isInitialized || status.isUnlocked === false) {
    console.log("[a5oc] FAIL agent not ready");
    process.exit(1);
  }

  const mcp = new McpClient(MCP, TOKEN);
  await mcp.initialize("a5oc");
  // Spoken-name mention query — name-only + ontology-excluded (see the Hermes
  // driver note: get_mention_waker_config's DID term matches author-DID proof
  // metadata and would wake on the agent's own writes).
  const term = NAME.toLowerCase();
  const query = `SELECT ?source ?predicate ?target WHERE { ?source ?predicate ?target . FILTER(isIRI(?source) && isIRI(?predicate)) FILTER(!STRSTARTS(STR(?predicate), "ad4m://ontology/")) FILTER(CONTAINS(LCASE(STR(<ad4m://fn/parse_literal>(?target))), "${term}")) }`;

  let woke = 0;
  const posts: number[] = [];
  const mgr = new WakerSubscriptionManager({
    perspectiveClient: client.perspective,
    logger,
    QuerySubscriptionProxy,
    debounceMs: DEBOUNCE,
    onWake: () => {
      woke++;
      postWake("AD4M call transcript — you were mentioned by name. Read the channel transcript with your ad4m tools, then reply into the channel.").then((c) => posts.push(c));
    },
  });
  await mgr.subscribe({ id: "a5oc", type: "mention", perspective: UUID, channel: CHAN, query });
  await sleep(1500); // baseline

  // ── negative control ──
  await seedCallPresence(mcp, UUID, CHAN);
  await sleep(DEBOUNCE + 3000);
  const negWakes = woke;
  const callActive = await isCallActive(mcp, UUID, CHAN);

  // ── trigger ──
  await seedTranscript(mcp, UUID, CHAN, TRANSCRIPT);
  for (let i = 0; i < 40 && woke === 0; i++) await sleep(500);
  await sleep(1500);

  // ── the woken OpenClaw turn reads + replies; poll for the reply child ──
  let replyLanded = false;
  for (let i = 0; i < 40 && !replyLanded; i++) {
    replyLanded = await findChildWithMarker(mcp, UUID, CHAN, MARKER);
    if (!replyLanded) await sleep(3000);
  }

  const postOk = posts.filter((c) => c >= 200 && c < 300).length;
  const postFail = posts.filter((c) => c === 0 || c >= 400).length;
  const metrics = { negWakes, woke, postOk, postFail, callActive, replyLanded };
  console.log("[a5oc] METRICS " + JSON.stringify(metrics));

  // The WAKE half: the mocked call-presence entry alone does not wake; the
  // transcript's spoken name wakes exactly once and reaches OpenClaw's real
  // /hooks/wake ingress (2xx); the call-presence entry reads back.
  const wakePass = negWakes === 0 && woke === 1 && postOk >= 1 && postFail === 0 && callActive;

  // A5_WAKE_ONLY: assert only the WAKE half. OpenClaw drives MCP tools through its
  // own text / code-bridge protocol (tools listed in the system prompt, not an
  // OpenAI `tools` array — supportsTools does not flip its hook turn to native
  // tool_calls) and its `action:agent` hook turn surfaces no model-visible user
  // message, so the deterministic OpenAI-format mock cannot make OpenClaw perform
  // the MCP read+reply. The perceive->act half rides the real-model lane (the
  // reference plugin registers ad4m tools natively) — out of scope for the mock.
  if (process.env.A5_WAKE_ONLY === "1") {
    if (!wakePass) {
      console.log("[a5oc] FAIL " + JSON.stringify(metrics));
      process.exit(1);
    }
    console.log("[a5oc] WAKE-PASS mocked-call wake loop: call-presence alone did not wake; transcript spoken name -> 1 wake -> real /hooks/wake 2xx; call-presence readable");
    process.exit(0);
  }

  const pass = wakePass && replyLanded;
  if (!pass) {
    console.log("[a5oc] FAIL " + JSON.stringify(metrics));
    process.exit(1);
  }
  console.log(`[a5oc] PASS mocked-A/V loop: call-presence alone did not wake; transcript spoken name -> 1 wake -> hook 2xx -> OpenClaw turn read + replied (child landed)`);
  process.exit(0);
})().catch((e) => {
  console.error("[a5oc] FATAL", e?.stack || e);
  process.exit(2);
});
