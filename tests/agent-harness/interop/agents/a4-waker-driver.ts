/**
 * A4 waker driver — drives the REAL waker component end-to-end against a live
 * executor and a live wake ingress.
 *
 * The detection half is production code from this checkout: core's
 * `WakerSubscriptionManager` (the class the @coasys/openclaw-ad4m plugin
 * re-exports) on `Ad4mClient` + `QuerySubscriptionProxy`, subscribed to the
 * query the executor's `generate_waker_query` returns for a channel: a model
 * query over the built-in `ad4m://Container` (the channel's children). The
 * delivery half depends on the harness:
 *   A4_TARGET=openclaw  the plugin's own `postWake` to OpenClaw's real
 *                       /hooks/wake (Bearer wake token, action=agent);
 *   A4_TARGET=hermes    a signed POST to Hermes's real /webhooks/<route>
 *                       ingress (HMAC-SHA256, GitHub X-Hub-Signature-256).
 *
 * Assertions (all must hold):
 *   - single:   one injected channel message  -> exactly one wake
 *   - dedup:    three rapid messages          -> coalesced to exactly one wake (debounce)
 *   - liveness: a later message               -> exactly one more wake
 *   - ingress:  every wake reaches the real ingress (2xx), zero failures
 *
 * Env: A4_TAG (log prefix), A4_TARGET, A4_WS, A4_MCP, A4_ADMIN, A4_INGRESS (wake
 * URL), A4_WAKE_TOKEN (openclaw) or A4_SECRET (hermes), AD4M_PLUGIN_DIR
 * (openclaw). Prints `[<tag>] METRICS|PASS|FAIL` and exits 0 (pass) /
 * 1 (assertion fail) / 2 (fatal).
 */
import { createHmac } from "crypto";
import { Ad4mClient, QuerySubscriptionProxy, WakerSubscriptionManager } from "@coasys/ad4m";
import { McpClient } from "../../src/mcp-client.ts";

const TAG = process.env.A4_TAG || "a4";
const TARGET = process.env.A4_TARGET || "openclaw";
const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

(async () => {
  const WS = process.env.A4_WS!;
  const MCP = process.env.A4_MCP!;
  const INGRESS = process.env.A4_INGRESS!;
  const TOKEN = process.env.A4_ADMIN || "windtunnel-admin";
  const CHAN = `${TAG}://channel`;
  const DEBOUNCE = 1000;

  const logs: string[] = [];
  const rec = (lvl: string) => (m: string) => logs.push(`${lvl} ${String(m)}`);
  const logger = { info: rec("I"), warn: rec("W"), error: rec("E"), debug: () => {} };

  // The wake delivery for this harness: resolves true on a 2xx from its ingress.
  let deliver: (sub: any, did: string, mentions: any) => Promise<boolean>;
  if (TARGET === "openclaw") {
    const { postWake } = await import(`${process.env.AD4M_PLUGIN_DIR}/wakerHelpers.ts`);
    const config = { wakeUrl: INGRESS, wakeToken: process.env.A4_WAKE_TOKEN };
    deliver = (sub, did, mentions) => postWake(config, sub, did, logger, mentions);
  } else {
    const secret = process.env.A4_SECRET || "";
    deliver = async (_sub, _did, _mentions) => {
      const body = JSON.stringify({ text: "New messages in an AD4M channel." });
      const sig = "sha256=" + createHmac("sha256", secret).update(body).digest("hex");
      try {
        const res = await fetch(INGRESS, {
          method: "POST",
          headers: { "Content-Type": "application/json", "X-Hub-Signature-256": sig },
          body,
          signal: AbortSignal.timeout(15000),
        });
        return res.ok;
      } catch {
        return false;
      }
    };
  }

  const client = new Ad4mClient(WS, TOKEN);
  const status = await client.agent.status();
  if (!status.isInitialized || status.isUnlocked === false) {
    console.log(`[${TAG}] FAIL agent not ready`);
    process.exit(1);
  }
  const did = status.did!;
  const uuid = (await client.perspective.add(`${TAG}-channel`)).uuid;

  const mcp = new McpClient(MCP, TOKEN);
  await mcp.initialize(`${TAG}drv`);
  const gen = await mcp.callToolJson("generate_waker_query", {
    perspective_id: uuid,
    class_name: "Message",
    parent: CHAN,
  });
  if (!gen?.query || !gen?.query_class) {
    console.log(`[${TAG}] FAIL generate_waker_query returned no model query: ${JSON.stringify(gen)}`);
    process.exit(1);
  }

  let woke = 0;
  const wakeTimes: number[] = [];
  const deliveries: Promise<boolean>[] = [];
  const mgr = new WakerSubscriptionManager({
    perspectiveClient: client.perspective,
    logger,
    QuerySubscriptionProxy,
    debounceMs: DEBOUNCE,
    onWake: (sub: any, _r: any, mentions: any) => {
      woke++;
      wakeTimes.push(Date.now());
      deliveries.push(deliver(sub, did, mentions));
    },
  });
  await mgr.subscribe({
    id: `${TAG}-chan`,
    type: "channel-messages",
    perspective: uuid,
    channel: CHAN,
    query: gen.query,
    className: gen.query_class,
  });

  const inject = (n: number) =>
    mcp.callToolJson("add_link", {
      perspective_id: uuid,
      source: CHAN,
      predicate: "ad4m://has_child",
      target: `${TAG}://msg/${n}`,
    });
  const settle = DEBOUNCE + 2000;

  const t0 = Date.now();
  await inject(1);
  await sleep(settle);
  const afterSingle = woke;
  const wakeLatencyMs = wakeTimes.length ? wakeTimes[0] - t0 : -1;

  await inject(2);
  await inject(3);
  await inject(4);
  await sleep(settle);
  const afterBurst = woke;

  await inject(5);
  await sleep(settle);
  const afterLive = woke;

  const results = await Promise.all(deliveries);
  mgr.disposeAll();
  const postOk = results.filter(Boolean).length;
  const postFail = results.length - postOk;

  const singleOk = afterSingle === 1;
  const dedupOk = afterBurst - afterSingle === 1;
  const liveOk = afterLive - afterBurst === 1;
  const ingressOk = postOk === afterLive && postFail === 0;
  const pass = singleOk && dedupOk && liveOk && ingressOk;

  const metrics = {
    wakesTotal: woke,
    afterSingle,
    afterBurst,
    afterLive,
    postOk,
    postFail,
    wakeLatencyMs,
    debounceMs: DEBOUNCE,
    singleOk,
    dedupOk,
    liveOk,
    ingressOk,
  };
  console.log(`[${TAG}] METRICS ` + JSON.stringify(metrics));
  if (!pass) {
    console.log(`[${TAG}] FAIL ` + JSON.stringify({ singleOk, dedupOk, liveOk, ingressOk }));
    console.log(logs.slice(-30).join("\n"));
    process.exit(1);
  }
  console.log(
    `[${TAG}] WAKER-OK real waker -> real ${TARGET} ingress: 1 msg=1 wake, 3 rapid coalesced to 1, liveness ok, ${postOk} ingress 2xx`,
  );
  process.exit(0);
})().catch((e) => {
  console.error(`[${TAG}] FATAL`, e?.stack || e);
  process.exit(2);
});
