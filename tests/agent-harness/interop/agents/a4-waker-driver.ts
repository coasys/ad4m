/**
 * A4 waker driver — drives the REAL @coasys/openclaw-ad4m waker component
 * end-to-end against a live executor and a live OpenClaw gateway hook.
 *
 * It imports the plugin's own `WakerSubscriptionManager` + `postWake` (the exact
 * shipped code, the same way the plugin's unit tests drive it) plus `Ad4mClient`
 * + `QuerySubscriptionProxy` from the plugin's `@coasys/ad4m` — so the whole
 * subscribe → detect → debounce → wake → POST path is production code, not a
 * re-implementation. The wake is delivered to a REAL OpenClaw gateway `/hooks/wake`
 * endpoint (real Bearer-token auth, action=agent), satisfying the "no stub sink"
 * requirement.
 *
 * Assertions (all must hold):
 *   - single:   one injected channel message  -> exactly one wake
 *   - dedup:    three rapid messages          -> coalesced to exactly one wake (debounce)
 *   - liveness: a later message               -> exactly one more wake
 *   - hook:     every wake reaches the real gateway hook (2xx), zero failures
 *
 * Env: AD4M_PLUGIN_DIR (required, a plugins/ad4m checkout with dist + node_modules),
 *   A4_WS, A4_MCP, A4_HOOK, A4_WAKE_TOKEN, A4_ADMIN. Prints a `[a4] PASS|FAIL|METRICS`
 *   line and exits 0 (pass) / 1 (assertion fail) / 2 (fatal).
 */
import { fileURLToPath } from "url";
import { dirname, resolve } from "path";

const HARNESS = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..");

(async () => {
  const PD = process.env.AD4M_PLUGIN_DIR;
  if (!PD) {
    console.log("[a4] FATAL AD4M_PLUGIN_DIR not set");
    process.exit(2);
  }
  const require = (await import("module")).createRequire(import.meta.url);
  const { Ad4mClient, QuerySubscriptionProxy } = require(`${PD}/node_modules/@coasys/ad4m`);
  const { WakerSubscriptionManager } = await import(`${PD}/wakerSubscriptionManager.ts`);
  const { postWake } = await import(`${PD}/wakerHelpers.ts`);
  const { McpClient } = await import(`${HARNESS}/src/mcp-client.ts`);

  const WS = process.env.A4_WS || "http://127.0.0.1:14411";
  const MCP = process.env.A4_MCP || "http://127.0.0.1:14410";
  const HOOK = process.env.A4_HOOK || "http://127.0.0.1:18790/hooks/wake";
  const WT = process.env.A4_WAKE_TOKEN || "a4-wake-tok";
  const TOKEN = process.env.A4_ADMIN || "windtunnel-admin";
  const CHAN = "a4://channel";
  const DEBOUNCE = 1000;
  const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

  const logs: string[] = [];
  const rec = (lvl: string) => (m: string) => logs.push(`${lvl} ${String(m)}`);
  const logger = { info: rec("I"), warn: rec("W"), error: rec("E"), debug: () => {} };

  const client = new Ad4mClient(WS, TOKEN, true);
  const status = await client.agent.status();
  if (!status.isInitialized || status.isUnlocked === false) {
    console.log("[a4] FAIL agent not ready");
    process.exit(1);
  }
  const did = status.did;
  const uuid = (await client.perspective.add("a4-channel")).uuid;

  const mcp = new McpClient(MCP, TOKEN);
  await mcp.initialize("a4drv");
  const gen = await mcp.callToolJson("generate_waker_query", {
    perspective_id: uuid,
    class_name: "Message",
    parent_address: CHAN,
  });
  const query = gen.query ?? gen.waker_config?.query;
  if (!query) {
    console.log("[a4] FAIL no waker query generated");
    process.exit(1);
  }

  const config = { wakeUrl: HOOK, wakeToken: WT } as any;
  let woke = 0;
  const wakeTimes: number[] = [];
  const mgr = new WakerSubscriptionManager({
    perspectiveClient: client.perspective,
    logger,
    QuerySubscriptionProxy,
    debounceMs: DEBOUNCE,
    onWake: (sub: any, _r: any, mentions: any) => {
      woke++;
      wakeTimes.push(Date.now());
      postWake(config, sub, did, logger, mentions);
    },
  });
  await mgr.subscribe({ id: "a4-chan", type: "channel-messages", perspective: uuid, channel: CHAN, query });

  const inject = (n: number) =>
    mcp.callToolJson("add_link", {
      perspective_id: uuid,
      source: CHAN,
      predicate: "ad4m://has_child",
      target: `a4://msg/${n}`,
    });
  const settle = DEBOUNCE + 1500;

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
  await sleep(500); // let the final postWake resolve

  const postOk = logs.filter((l) => l.includes("wake POST sent successfully")).length;
  const postFail = logs.filter((l) => l.includes("wake POST failed") || l.includes("wake POST error")).length;

  const singleOk = afterSingle === 1;
  const dedupOk = afterBurst - afterSingle === 1;
  const liveOk = afterLive - afterBurst === 1;
  const hookOk = postOk >= afterLive && postFail === 0;
  const pass = singleOk && dedupOk && liveOk && hookOk;

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
    hookOk,
    did: String(did).slice(0, 40),
  };
  console.log("[a4] METRICS " + JSON.stringify(metrics));
  if (!pass) {
    console.log("[a4] FAIL " + JSON.stringify({ singleOk, dedupOk, liveOk, hookOk }));
    console.log(logs.slice(-30).join("\n"));
    process.exit(1);
  }
  console.log(
    `[a4] PASS real waker -> real OpenClaw hook: 1 msg=1 wake, 3 rapid coalesced to 1, liveness ok, ${postOk} hook 200s`,
  );
  process.exit(0);
})().catch((e) => {
  console.error("[a4] FATAL", e?.stack || e);
  process.exit(2);
});
