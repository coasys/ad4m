/**
 * A5 (Hermes) A/V-loop driver — mocked call, real perceive→act.
 *
 * Runs the REAL @coasys/openclaw-ad4m mention waker (WakerSubscriptionManager +
 * a live perspective SPARQL mention subscription) against a live executor, seeds
 * a mocked call (call-presence entry + a transcript naming the agent in free
 * speech) via the mock-av fixture, and delivers the wake to Hermes's REAL signed
 * webhook ingress (`/webhooks/<route>`, HMAC-SHA256, GitHub X-Hub-Signature-256).
 * The woken Hermes turn (mock model + mcp_servers.ad4m) reads the transcript and
 * replies into the channel.
 *
 * This driver owns the DELIVERY half + the negative control:
 *   - negative control: the call-presence entry alone produces NO wake;
 *   - exactly one wake fires on the transcript's spoken name;
 *   - every wake reaches the webhook (2xx);
 *   - the reply Message lands as a fresh channel child (child-has marker),
 *     written by the woken Hermes turn.
 * The verify script asserts PERCEIVE (the agent's read tool returned the
 * transcript body — has_mark in the mock log) from the mock's request log.
 *
 * Env: AD4M_PLUGIN_DIR, A5_WS, A5_MCP, A5_ADMIN, A5H_WEBHOOK, A5H_SECRET,
 * A5_UUID, A5_CHAN, A5_TRANSCRIPT, A5_MARKER, A5_NAME. Prints `[a5h] METRICS|PASS|FAIL`.
 */
import { fileURLToPath } from "url";
import { dirname, resolve } from "path";
import { createHmac } from "crypto";

const HARNESS = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..");
const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

(async () => {
  const PD = process.env.AD4M_PLUGIN_DIR;
  if (!PD) {
    console.log("[a5h] FATAL AD4M_PLUGIN_DIR not set");
    process.exit(2);
  }
  const require = (await import("module")).createRequire(import.meta.url);
  const { Ad4mClient, QuerySubscriptionProxy } = require(`${PD}/node_modules/@coasys/ad4m`);
  const { WakerSubscriptionManager } = await import(`${PD}/wakerSubscriptionManager.ts`);
  const { McpClient } = await import(`${HARNESS}/src/mcp-client.ts`);
  const { seedCallPresence, seedTranscript, isCallActive } = await import(`${HARNESS}/interop/agents/mock-av.ts`);
  const { findChildWithMarker } = await import(`${HARNESS}/interop/agents/sovereign/waker-ad4m.ts`);

  const WS = process.env.A5_WS || "http://127.0.0.1:14462";
  const MCP = process.env.A5_MCP || "http://127.0.0.1:14461";
  const WEBHOOK = process.env.A5H_WEBHOOK || "http://127.0.0.1:18645/webhooks/wake";
  const SECRET = process.env.A5H_SECRET || "a5-wake-secret";
  const TOKEN = process.env.A5_ADMIN || "windtunnel-admin";
  const UUID = process.env.A5_UUID!;
  const CHAN = process.env.A5_CHAN || "a5h://channel";
  const TRANSCRIPT = process.env.A5_TRANSCRIPT || "On the call someone said: hey Aria, can you summarise the last point?";
  const MARKER = process.env.A5_MARKER || "A5H_REPLY_OK";
  const NAME = process.env.A5_NAME || "Aria";
  const DEBOUNCE = 1000;

  async function signedPost(text: string): Promise<number> {
    const body = JSON.stringify({ text });
    const sig = "sha256=" + createHmac("sha256", SECRET).update(body).digest("hex");
    try {
      const res = await fetch(WEBHOOK, {
        method: "POST",
        headers: { "Content-Type": "application/json", "X-Hub-Signature-256": sig },
        body,
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
    console.log("[a5h] FAIL agent not ready");
    process.exit(1);
  }

  const mcp = new McpClient(MCP, TOKEN);
  await mcp.initialize("a5h");
  // Spoken-name mention query — fires only when a message body (parse_literal-
  // decoded) contains the agent name. We build it name-only + ontology-excluded
  // rather than reuse get_mention_waker_config, which ALSO ORs the agent DID and
  // therefore matches every authored link's author-DID proof metadata — waking on
  // the agent's own writes (incl. the call-presence entry), not the spoken name.
  // (A real executor bug the A5 waker surfaces; the Sovereign waker.ts port fixes
  // it with the same ontology exclusion.)
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
      signedPost("AD4M call transcript — you were mentioned by name. Read the channel transcript with your ad4m tools, then reply into the channel.").then((c) => posts.push(c));
    },
  });
  await mgr.subscribe({ id: "a5h", type: "mention", perspective: UUID, channel: CHAN, query });
  await sleep(1500); // let the mention subscription baseline

  // ── negative control: the call-presence entry alone must NOT wake ──
  await seedCallPresence(mcp, UUID, CHAN);
  await sleep(DEBOUNCE + 3000);
  const negWakes = woke;
  const callActive = await isCallActive(mcp, UUID, CHAN);

  // ── trigger: the transcript's spoken name ──
  await seedTranscript(mcp, UUID, CHAN, TRANSCRIPT);
  for (let i = 0; i < 40 && woke === 0; i++) await sleep(500);
  await sleep(1500); // let the webhook POST resolve

  // ── the woken Hermes turn reads + replies; poll for the reply child ──
  let replyLanded = false;
  for (let i = 0; i < 40 && !replyLanded; i++) {
    replyLanded = await findChildWithMarker(mcp, UUID, CHAN, MARKER);
    if (!replyLanded) await sleep(3000);
  }

  const postOk = posts.filter((c) => c >= 200 && c < 300).length;
  const postFail = posts.filter((c) => c === 0 || c >= 400).length;
  const metrics = { negWakes, woke, postOk, postFail, callActive, replyLanded };
  console.log("[a5h] METRICS " + JSON.stringify(metrics));
  const pass = negWakes === 0 && woke === 1 && postOk >= 1 && postFail === 0 && callActive && replyLanded;
  if (!pass) {
    console.log("[a5h] FAIL " + JSON.stringify(metrics));
    process.exit(1);
  }
  console.log(`[a5h] PASS mocked-A/V loop: call-presence alone did not wake; transcript spoken name -> 1 wake -> webhook 2xx -> Hermes turn read + replied (child landed)`);
  process.exit(0);
})().catch((e) => {
  console.error("[a5h] FATAL", e?.stack || e);
  process.exit(2);
});
