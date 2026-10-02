/**
 * A4 (Hermes) waker driver — drives the REAL @coasys/openclaw-ad4m waker
 * component (WakerSubscriptionManager + a live perspective SPARQL subscription)
 * against a live executor, and delivers each detected channel message to
 * Hermes's REAL webhook wake ingress (`/webhooks/<route>`, HMAC-SHA256 signed —
 * GitHub `X-Hub-Signature-256` scheme). So the ad4m-detection half is production
 * plugin code and the delivery half hits Hermes's real, signature-validated
 * ingress — no stub sink.
 *
 * Assertions: 1 msg -> 1 wake; 3 rapid msgs -> coalesced to 1 (debounce dedup);
 * a later msg -> 1 more wake; every wake reaches the webhook (2xx), zero fails.
 *
 * Env: AD4M_PLUGIN_DIR (required), A4_WS, A4_MCP, A4_ADMIN, A4H_WEBHOOK (the
 * Hermes webhook URL), A4H_SECRET (the route's HMAC secret). Prints
 * `[a4h] METRICS|PASS|FAIL`.
 */
import { fileURLToPath } from "url";
import { dirname, resolve } from "path";
import { createHmac } from "crypto";

const HARNESS = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..");

(async () => {
  const PD = process.env.AD4M_PLUGIN_DIR;
  if (!PD) {
    console.log("[a4h] FATAL AD4M_PLUGIN_DIR not set");
    process.exit(2);
  }
  const require = (await import("module")).createRequire(import.meta.url);
  const { Ad4mClient, QuerySubscriptionProxy } = require(`${PD}/node_modules/@coasys/ad4m`);
  const { WakerSubscriptionManager } = await import(`${PD}/wakerSubscriptionManager.ts`);
  const { McpClient } = await import(`${HARNESS}/src/mcp-client.ts`);

  const WS = process.env.A4_WS || "http://127.0.0.1:14422";
  const MCP = process.env.A4_MCP || "http://127.0.0.1:14421";
  const WEBHOOK = process.env.A4H_WEBHOOK || "http://127.0.0.1:18644/webhooks/wake";
  const SECRET = process.env.A4H_SECRET || "a4-wake-secret";
  const TOKEN = process.env.A4_ADMIN || "windtunnel-admin";
  const CHAN = "a4h://channel";
  const DEBOUNCE = 1000;
  const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

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
    console.log("[a4h] FAIL agent not ready");
    process.exit(1);
  }
  const uuid = (await client.perspective.add("a4h-channel")).uuid;

  const mcp = new McpClient(MCP, TOKEN);
  await mcp.initialize("a4h");
  const gen = await mcp.callToolJson("generate_waker_query", {
    perspective_id: uuid,
    class_name: "Message",
    parent_address: CHAN,
  });
  const query = gen.query ?? gen.waker_config?.query;
  if (!query) {
    console.log("[a4h] FAIL no waker query generated");
    process.exit(1);
  }

  let woke = 0;
  const posts: number[] = [];
  const mgr = new WakerSubscriptionManager({
    perspectiveClient: client.perspective,
    logger,
    QuerySubscriptionProxy,
    debounceMs: DEBOUNCE,
    onWake: () => {
      woke++;
      signedPost(`New messages in an AD4M channel (wake #${woke}).`).then((code) => posts.push(code));
    },
  });
  await mgr.subscribe({ id: "a4h-chan", type: "channel-messages", perspective: uuid, channel: CHAN, query });

  const inject = (n: number) =>
    mcp.callToolJson("add_link", { perspective_id: uuid, source: CHAN, predicate: "ad4m://has_child", target: `a4h://msg/${n}` });
  const settle = DEBOUNCE + 2000;

  const t0 = Date.now();
  await inject(1);
  await sleep(settle);
  const afterSingle = woke;
  const wakeLatencyMs = posts.length ? Date.now() - t0 : -1;

  await inject(2);
  await inject(3);
  await inject(4);
  await sleep(settle);
  const afterBurst = woke;

  await inject(5);
  await sleep(settle);
  const afterLive = woke;
  await sleep(1000); // let posts resolve

  const postOk = posts.filter((c) => c >= 200 && c < 300).length;
  const postFail = posts.filter((c) => c === 0 || c >= 400).length;
  const singleOk = afterSingle === 1;
  const dedupOk = afterBurst - afterSingle === 1;
  const liveOk = afterLive - afterBurst === 1;
  const hookOk = postOk >= afterLive && postFail === 0;
  const pass = singleOk && dedupOk && liveOk && hookOk;

  const metrics = { wakesTotal: woke, afterSingle, afterBurst, afterLive, postOk, postFail, wakeLatencyMs, debounceMs: DEBOUNCE, singleOk, dedupOk, liveOk, hookOk };
  console.log("[a4h] METRICS " + JSON.stringify(metrics));
  if (!pass) {
    console.log("[a4h] FAIL " + JSON.stringify({ singleOk, dedupOk, liveOk, hookOk }));
    process.exit(1);
  }
  console.log(`[a4h] PASS real waker -> real Hermes webhook: 1 msg=1 wake, 3 rapid coalesced to 1, liveness ok, ${postOk} webhook 2xx`);
  process.exit(0);
})().catch((e) => {
  console.error("[a4h] FATAL", e?.stack || e);
  process.exit(2);
});
