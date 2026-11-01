/**
 * A5 A/V-loop driver — mocked call, real wake, and (Hermes) real perceive→act.
 *
 * Runs core's WakerSubscriptionManager (the class the @coasys/openclaw-ad4m
 * plugin re-exports) on the executor's own mention subscription
 * (`get_mention_waker_config`: TextContent text naming the agent + Mentioning
 * links to its DID, written by another agent) against a live executor. It names
 * the agent (profile username), registers a Message class whose body is its
 * TextContent text, seeds a mocked call (call-presence entry, then a caller's
 * transcript naming the agent in free speech) via the mock-av fixture, and
 * delivers the wake to the harness's real ingress:
 *   A5_TARGET=hermes    signed POST to Hermes's /webhooks/<route> (HMAC-SHA256,
 *                       X-Hub-Signature-256); the woken turn (mock model +
 *                       mcp_servers.ad4m) reads the transcript and replies;
 *   A5_TARGET=openclaw  Bearer POST to OpenClaw's /hooks/wake (wake half only).
 *
 * Asserts:
 *   - negative control: the call-presence entry alone produces NO wake;
 *   - exactly one wake fires on the transcript's spoken name, naming the
 *     transcript and its channel, by the caller with a verified signature;
 *   - every wake reaches the ingress (2xx);
 *   - (A5_EXPECT_REPLY=1) the reply Message lands as a fresh channel child.
 *
 * Env: A5_TAG, A5_TARGET, A5_WS, A5_MCP, A5_ADMIN, A5_INGRESS, A5_SECRET
 * (hermes) or A5_WAKE_TOKEN (openclaw), A5_UUID, A5_CHAN, A5_TRANSCRIPT,
 * A5_MARKER, A5_NAME, A5_EXPECT_REPLY. Prints `[<tag>] METRICS|WAKE-OK|FAIL`.
 */
import { createHmac } from "crypto";
import { Ad4mClient, QuerySubscriptionProxy, WakerSubscriptionManager } from "@coasys/ad4m";
import { McpClient } from "../../src/mcp-client.ts";
import { isCallActive, registerMessageClass, seedCallPresence, seedTranscript, transcriptAddrFor } from "./mock-av.ts";
import { findChildWithMarker } from "./sovereign/waker-ad4m.ts";

const TAG = process.env.A5_TAG || "a5";
const TARGET = process.env.A5_TARGET || "hermes";
const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

(async () => {
  const WS = process.env.A5_WS!;
  const MCP = process.env.A5_MCP!;
  const INGRESS = process.env.A5_INGRESS!;
  const TOKEN = process.env.A5_ADMIN || "windtunnel-admin";
  const UUID = process.env.A5_UUID!;
  const CHAN = process.env.A5_CHAN!;
  const TRANSCRIPT = process.env.A5_TRANSCRIPT!;
  const MARKER = process.env.A5_MARKER || "";
  const NAME = process.env.A5_NAME || "Aria";
  const EXPECT_REPLY = process.env.A5_EXPECT_REPLY === "1";
  const DEBOUNCE = 1000;

  async function postWake(text: string): Promise<number> {
    const body = JSON.stringify(TARGET === "openclaw" ? { text, mode: "now" } : { text });
    const headers: Record<string, string> = { "Content-Type": "application/json" };
    if (TARGET === "openclaw") headers.Authorization = `Bearer ${process.env.A5_WAKE_TOKEN}`;
    else headers["X-Hub-Signature-256"] = "sha256=" + createHmac("sha256", process.env.A5_SECRET || "").update(body).digest("hex");
    try {
      const res = await fetch(INGRESS, { method: "POST", headers, body, signal: AbortSignal.timeout(15000) });
      return res.status;
    } catch {
      return 0;
    }
  }

  const logs: string[] = [];
  const rec = (lvl: string) => (m: string) => logs.push(`${lvl} ${String(m)}`);
  const logger = { info: rec("I"), warn: rec("W"), error: rec("E"), debug: () => {} };
  const ad4m = new Ad4mClient(WS, TOKEN);
  const status = await ad4m.agent.status();
  if (!status.isInitialized || status.isUnlocked === false) {
    console.log(`[${TAG}] FAIL agent not ready`);
    process.exit(1);
  }

  const mcp = new McpClient(MCP, TOKEN);
  await mcp.initialize(TAG);
  // The agent answers to its profile name; the Message class makes a message's
  // body the text the mention query reads.
  await mcp.callToolJson("set_agent_profile", { username: NAME });
  await registerMessageClass(mcp, UUID);
  const config = await mcp.callToolJson("get_mention_waker_config", { perspective_id: UUID });
  const sub = config?.subscription;
  if (!sub?.query || !sub?.class || !config?.names?.includes(NAME)) {
    console.log(`[${TAG}] FAIL get_mention_waker_config did not return a subscription naming ${NAME}: ${JSON.stringify(config)}`);
    process.exit(1);
  }

  let woke = 0;
  const wakes: any[][] = [];
  const posts: Promise<number>[] = [];
  const mgr = new WakerSubscriptionManager({
    perspectiveClient: ad4m.perspective,
    logger,
    QuerySubscriptionProxy,
    debounceMs: DEBOUNCE,
    onWake: (_sub: any, _r: any, mentions: any) => {
      woke++;
      wakes.push(mentions ?? []);
      const items = (mentions ?? []).map((m: any) => `${m.address} in ${m.parents.join(", ")}`).join("; ");
      posts.push(
        postWake(`AD4M call transcript — you were mentioned by name (${items}). Read the transcript with your ad4m tools, then reply into the channel.`),
      );
    },
  });
  await mgr.subscribe({
    id: sub.id,
    type: "mention",
    perspective: UUID,
    channel: CHAN,
    query: sub.query,
    className: sub.class,
    terms: sub.terms,
    did: sub.did,
    mentionQuery: sub.mentionQuery,
    mentionClass: sub.mentionClass,
  });
  await sleep(1500); // let the mention subscription open

  // ── negative control: the call-presence entry alone must NOT wake ──
  await seedCallPresence(mcp, UUID, CHAN);
  await sleep(DEBOUNCE + 3000);
  const negWakes = woke;
  const callActive = await isCallActive(mcp, UUID, CHAN);

  // ── trigger: the caller's transcript names the agent ──
  await seedTranscript(ad4m, UUID, CHAN, TRANSCRIPT);
  for (let i = 0; i < 40 && woke === 0; i++) await sleep(500);
  await sleep(DEBOUNCE + 1500); // a second wake would have fired by now
  const codes = await Promise.all(posts);

  const mention = wakes[0]?.[0];
  const mentionOk =
    wakes.length === 1 &&
    wakes[0].length === 1 &&
    mention?.address === transcriptAddrFor(CHAN) &&
    mention?.parents?.includes(CHAN) &&
    mention?.verified === true &&
    mention?.via === "text";

  // ── the woken turn reads + replies; poll for the reply child ──
  let replyLanded = false;
  if (EXPECT_REPLY) {
    for (let i = 0; i < 40 && !replyLanded; i++) {
      replyLanded = await findChildWithMarker(mcp, UUID, CHAN, MARKER);
      if (!replyLanded) await sleep(3000);
    }
  }
  mgr.disposeAll();

  const postOk = codes.filter((c) => c >= 200 && c < 300).length;
  const postFail = codes.length - postOk;
  const metrics = { negWakes, woke, mentionOk, postOk, postFail, callActive, replyLanded };
  console.log(`[${TAG}] METRICS ` + JSON.stringify(metrics));
  const pass = negWakes === 0 && woke === 1 && mentionOk && postOk === 1 && postFail === 0 && callActive && (!EXPECT_REPLY || replyLanded);
  if (!pass) {
    console.log(`[${TAG}] FAIL ` + JSON.stringify({ metrics, mention }));
    console.log(logs.filter((l) => !l.includes("creating subscription")).slice(-20).join("\n"));
    process.exit(1);
  }
  console.log(
    `[${TAG}] WAKE-OK call-presence alone did not wake; the caller's transcript naming ${NAME} -> 1 wake (verified, ${mention.address}) -> ${TARGET} ingress 2xx` +
      (EXPECT_REPLY ? " -> reply child landed" : ""),
  );
  process.exit(0);
})().catch((e) => {
  console.error(`[${TAG}] FATAL`, e?.stack || e);
  process.exit(2);
});
