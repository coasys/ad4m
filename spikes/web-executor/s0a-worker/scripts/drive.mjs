// Drives headless Chrome: page on 127.0.0.1:47458 starts a Worker that runs the
// templated server-link-language bundle with the ad4m:host shim. No executor serves the tab.
// Env: S (s0a-worker dir, default: parent of this script), AD4M_BIN (ad4m CLI, default: `ad4m` on PATH),
// EXECUTOR_URL (default http://127.0.0.1:47460), PAGE_URL (default http://127.0.0.1:47458/index.html),
// CHROME (default /usr/bin/google-chrome), LANG_ADDR, PERSP, BUNDLE, OUT, NEGATIVE.
import { chromium } from "playwright-core";
import { execFileSync } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
const S = process.env.S || path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const ADDR = process.env.LANG_ADDR;
const PERSP = process.env.PERSP;
const CLI = [process.env.AD4M_BIN || "ad4m", ["-n", "--executor-url", process.env.EXECUTOR_URL || "http://127.0.0.1:47460"]];
const cli = (...a) => execFileSync(CLI[0], [...CLI[1], ...a], { encoding: "utf8", timeout: 60000 });
const out = { steps: [] };
const step = (name, data) => { const e = { t: new Date().toISOString(), name, data }; out.steps.push(e); console.log("STEP", name, JSON.stringify(data).slice(0, 600)); };
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const browser = await chromium.launch({ executablePath: process.env.CHROME || "/usr/bin/google-chrome", headless: true, args: ["--no-proxy-server"] });
const page = await browser.newPage();
const consoleLines = [];
page.on("console", (m) => consoleLines.push(`[${m.type()}] ${m.text()}`));
page.on("requestfailed", (r) => consoleLines.push(`[requestfailed] ${r.method()} ${r.url()} ${r.failure()?.errorText}`));
page.on("worker", (w) => w.on("console", (m) => consoleLines.push(`[worker ${m.type()}] ${m.text()}`)));
await page.goto(process.env.PAGE_URL || "http://127.0.0.1:47458/index.html");
await page.waitForFunction(() => window.__ready);
const rpc = (cmd, arg) => page.evaluate(([c, a]) => window.rpc(c, a), [cmd, arg]);
const events = () => page.evaluate(() => window.__events);
const waitFor = async (pred, ms, label) => {
  const end = Date.now() + ms;
  while (Date.now() < end) { const ev = await events(); const hit = ev.find(pred); if (hit) return hit; await sleep(500); }
  throw new Error("timeout waiting for " + label);
};
const linkIn = (ev, target) => ev.kind === "perspectiveDiff" && JSON.stringify(ev.payload).includes(target);

try {
  const loaded = await rpc("load", { bundleUrl: `/${process.env.BUNDLE || "templated-bundle.js"}`, address: ADDR, settings: {} });
  step("load", loaded);
  const t0 = Date.now();
  const init = await rpc("call", { fn: "init", args: [] });
  step("init", { ...init, ms: Date.now() - t0 });
  if (process.env.NEGATIVE) { await sleep(8000); throw new Error("negative run done"); }

  // Held links open once the native admin grants the room key to this DID.
  try {
    const ev = await waitFor((e) => linkIn(e, "s0a://native-before"), 60000, "pre-join link via grant");
    step("pre-join link arrived", { afterMs: ev.t - t0, payload: ev.payload });
  } catch (e) { step("pre-join link", { error: String(e) }); }

  // (a) native add -> tab
  const marker = "s0a://native-live-" + Date.now();
  const ta = Date.now();
  cli("perspectives", "add-link", PERSP, "s0a://native-live", marker, "ad4m://has"); // CLI order: source target predicate
  step("native add-link", { target: marker });
  try {
    const ev = await waitFor((e) => linkIn(e, marker), 30000, "native link in tab");
    step("ACCEPT-A native link arrived as emitPerspectiveDiff", { latencyMs: ev.t - ta, payload: ev.payload });
  } catch (e) { step("ACCEPT-A", { error: String(e) }); }

  // sync exports by name
  for (const fn of ["perspectiveSyncSync", "perspectiveSyncCurrentRevision", "perspectiveSyncRender", "perspectiveQuerySupportedKinds"]) {
    const r = await rpc("call", { fn, args: [] });
    step(`call ${fn}`, r.ok ? { ok: true, result: r.result } : r);
  }
  const q = await rpc("call", { fn: "perspectiveQueryRun", args: [{ kind: "link-pattern", payload: { predicate: "ad4m://has" } }] });
  step("call perspectiveQueryRun", q);

  // (b) tab commit -> native
  const tabTarget = "s0a://from-tab-" + Date.now();
  const tb = Date.now();
  const c = await rpc("commitLink", { source: "s0a://tab", predicate: "ad4m://has", target: tabTarget });
  step("tab perspectiveCommit", c);
  let found = null;
  for (let i = 0; i < 60 && !found; i++) {
    const res = cli("perspectives", "query-links", PERSP);
    if (res.includes(tabTarget)) found = res; else await sleep(500);
  }
  if (found) {
    const block = found.split("\n").filter((l) => l.includes(tabTarget) || l.includes("from-tab") ).slice(0, 5);
    step("ACCEPT-B tab link found in native query-links", { latencyMs: Date.now() - tb, lines: block });
  } else step("ACCEPT-B", { error: "tab link not found natively within 30 s" });
  // removal from the tab
  if (found) {
    const tr = Date.now();
    const r = await rpc("removeLink", c.result);
    step("tab removal commit", { ok: r.ok, error: r.error });
    let gone = false;
    for (let i = 0; i < 60 && !gone; i++) { if (!cli("perspectives", "query-links", PERSP).includes(tabTarget)) gone = true; else await sleep(500); }
    step(gone ? "REMOVAL tab removal reached native" : "REMOVAL", gone ? { latencyMs: Date.now() - tr } : { error: "still present after 30 s" });
  }
  out.nativeLinks = cli("perspectives", "query-links", PERSP);
} catch (e) {
  step("error", String(e && e.stack || e));
} finally {
  try { out.stats = (await rpc("stats")).result; } catch {}
  try { out.events = await events(); out.workerLogs = await page.evaluate(() => window.__logs); } catch {}
  out.console = consoleLines;
  fs.mkdirSync(`${S}/logs`, { recursive: true });
  fs.writeFileSync(`${S}/logs/${process.env.OUT || "browser-run"}.json`, JSON.stringify(out, null, 2));
  await browser.close();
}
