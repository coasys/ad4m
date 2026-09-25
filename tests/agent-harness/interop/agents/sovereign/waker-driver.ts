/**
 * A4 (Sovereign) waker driver — the real @coasys/openclaw-ad4m waker component
 * (WakerSubscriptionManager + a live perspective SPARQL subscription) detects a
 * new channel message on the node and delivers the wake to Sovereign's REAL
 * presence ingress: POST /api/chat/send to the presence-internal thread with a
 * `modality:'ad4m'` origin — exactly the payload Sovereign's own bootstrap
 * handler feeds the presence agent when its native waker fires. Sovereign then
 * runs a presence turn and replies into the ad4m channel via presence_reply_ad4m.
 *
 * (Sovereign's native in-server waker connects + subscribes correctly, but its
 * long-running WS churns in a headless container and re-baselines the mention
 * subscription; driving the wake through the stable external subscription + the
 * real presence ingress tests the same surface deterministically. See the A4
 * scenario notes / PR.)
 *
 * Env: AD4M_PLUGIN_DIR, A4_WS, A4_MCP, A4_ADMIN (a user JWT), A4_UUID, A4_CHAN,
 * A4_SV (Sovereign base URL), A4_THREAD (presence-internal thread id). Prints
 * `[a4sv-drv] …` and exits 0 when the ingress accepted the wake (2xx).
 */
import { fileURLToPath } from "url";
import { dirname, resolve } from "path";

const HARNESS = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..", "..");
const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

(async () => {
  const PD = process.env.AD4M_PLUGIN_DIR;
  if (!PD) { console.log("[a4sv-drv] FATAL AD4M_PLUGIN_DIR not set"); process.exit(2); }
  const require = (await import("module")).createRequire(import.meta.url);
  const { Ad4mClient, QuerySubscriptionProxy } = require(`${PD}/node_modules/@coasys/ad4m`);
  const { WakerSubscriptionManager } = await import(`${PD}/wakerSubscriptionManager.ts`);
  const { McpClient } = await import(`${HARNESS}/src/mcp-client.ts`);

  const WS = process.env.A4_WS!, MCP = process.env.A4_MCP!, TOKEN = process.env.A4_ADMIN!;
  const UUID = process.env.A4_UUID!, CHAN = process.env.A4_CHAN!;
  const SV = process.env.A4_SV!, THREAD = process.env.A4_THREAD!;
  const msgAddr = `${CHAN}/msg/1`;

  const client = new Ad4mClient(WS, TOKEN, true);
  await client.agent.status();
  const mcp = new McpClient(MCP, TOKEN);
  await mcp.initialize("a4sv");
  const gen = await mcp.callToolJson("generate_waker_query", { perspective_id: UUID, class_name: "Message", parent_address: CHAN });
  const query = gen.query ?? gen.waker_config?.query;

  let posted = -1;
  const logger = { info: () => {}, warn: () => {}, error: () => {}, debug: () => {} };
  const mgr = new WakerSubscriptionManager({
    perspectiveClient: client.perspective,
    logger,
    QuerySubscriptionProxy,
    debounceMs: 800,
    onWake: async () => {
      if (posted !== -1) return;
      const origin = { modality: "ad4m", ad4m: { perspectiveUuid: UUID, channelAddress: CHAN, messageAddress: msgAddr } };
      const body = JSON.stringify({ threadId: THREAD, message: "New AD4M channel message mentioning you — please acknowledge.", origin });
      try {
        const r = await fetch(`${SV}/api/chat/send`, { method: "POST", headers: { "Content-Type": "application/json" }, body, signal: AbortSignal.timeout(15000) });
        posted = r.status;
      } catch { posted = 0; }
      console.log("[a4sv-drv] WAKE detected -> Sovereign presence ingress POST status=" + posted);
    },
  });
  await mgr.subscribe({ id: "a4sv", type: "channel-messages", perspective: UUID, channel: CHAN, query });

  await sleep(1500); // let the subscription baseline
  await mcp.callToolJson("add_link", { perspective_id: UUID, source: CHAN, predicate: "ad4m://has_child", target: msgAddr });
  await mcp.callToolJson("add_link", { perspective_id: UUID, source: msgAddr, predicate: "ad4m://message_body", target: "literal://string:Hey%20Hex%20please%20acknowledge" });
  for (let i = 0; i < 20 && posted === -1; i++) await sleep(500);
  console.log("[a4sv-drv] RESULT posted=" + posted);
  process.exit(posted >= 200 && posted < 300 ? 0 : 1);
})().catch((e) => { console.log("[a4sv-drv] FATAL", e?.stack || e); process.exit(2); });
