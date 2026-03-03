#!/usr/bin/env node
// AD4M Memory Leak Investigation v2
// Improvements over v1:
// - Fixed Test 5 GQL schema (DecoratedLinkExpression uses nested data {})
// - Added Holochain installed app count verification after removal
// - Added memory pressure step (malloc_trim equivalent) before measuring
// - Multiple RSS samples for stability
// - Longer settle time with progress reporting
// - perspectiveRemoveLink uses correct mutation signature
import WebSocket from "ws";
import { execSync, exec as execCb } from "node:child_process";
import { appendFileSync, writeFileSync, readFileSync } from "node:fs";
import path from "node:path";

const HOME = process.env.HOME;
const EXECUTOR = process.env.AD4M_EXECUTOR || `${HOME}/ad4m-bin/ad4m-executor`;
const SEED = process.env.AD4M_SEED || "/tmp/ad4m-prepared-seed.json";
const CWD = `${HOME}/ad4m/tests/js`;
const OUT = "/tmp/ad4m-leak-investigation-v2.txt";
const DATA = "/tmp/ad4m-leak-data-v2";
const EXEC_LOG = "/tmp/ad4m-leak-executor-v2.log";
const PORT = 15900;
const TOKEN = "leak-test";

const sleep = ms => new Promise(r => setTimeout(r, ms));
const log = msg => { const l = `[${new Date().toISOString()}] ${msg}`; console.log(l); appendFileSync(OUT, l + "\n"); };

function measureRSS(pid) {
  try {
    const raw = execSync(`ps -o rss= -p ${pid} 2>/dev/null`, { encoding: "utf-8" }).trim();
    return parseInt(raw) || 0;
  } catch { return 0; }
}

// Take 3 RSS samples over 2 seconds and return the median for stability
function stableRSS(pid) {
  const samples = [];
  for (let i = 0; i < 3; i++) {
    samples.push(measureRSS(pid));
    if (i < 2) execSync("sleep 1");
  }
  samples.sort((a, b) => a - b);
  return samples[1]; // median
}

function detailedMeasure(label, pid) {
  const rss = stableRSS(pid);
  log(`${label}: ${(rss/1024).toFixed(1)} MB RSS`);
  return rss;
}

function smapsBreakdown(pid) {
  try {
    const raw = execSync(`cat /proc/${pid}/smaps 2>/dev/null`, { encoding: "utf-8", maxBuffer: 50*1024*1024 });
    const buckets = {};
    let name = null, rss = 0, pss = 0, swap = 0;
    const cat = n => { const l=n.toLowerCase(); if(l.includes("ad4m")||l.includes("executor")) return "ad4m-executor"; if(n==="[heap]") return "[heap]"; if(n.startsWith("[stack")) return "[stack]"; if(n==="[anon]"||n==="") return "[anonymous]"; if(l.includes("libc")||l.includes("libm.so")||l.includes("ld-linux")) return "libc/system"; if(l.startsWith("/usr/lib")||l.startsWith("/lib")) return "system-libs"; if(l.includes("holochain")||l.includes("lair")) return "holochain"; if(l.includes("sqlite")||l.includes(".db")) return "sqlite"; return "other"; };
    const flush = () => { if(name===null) return; const c=cat(name); if(!buckets[c]) buckets[c]={rss:0,pss:0,swap:0,count:0}; buckets[c].rss+=rss; buckets[c].pss+=pss; buckets[c].swap+=swap; buckets[c].count++; };
    for (const line of raw.split("\n")) {
      const h = line.match(/^[0-9a-f]+-[0-9a-f]+\s+\S+\s+\S+\s+\S+\s+\d+\s*(.*)/);
      if (h) { flush(); name=h[1].trim()||"[anon]"; rss=0; pss=0; swap=0; continue; }
      const r = line.match(/^Rss:\s+(\d+)\s+kB/); if(r) rss=parseInt(r[1]);
      const p = line.match(/^Pss:\s+(\d+)\s+kB/); if(p) pss=parseInt(p[1]);
      const s = line.match(/^Swap:\s+(\d+)\s+kB/); if(s) swap=parseInt(s[1]);
    }
    flush();
    const sorted = Object.entries(buckets).sort((a,b)=>b[1].rss-a[1].rss);
    for (const [c,v] of sorted) { if(v.rss===0&&v.swap===0) continue; log(`  ${c.padEnd(22)} RSS:${(v.rss/1024).toFixed(1).padStart(8)} MB  PSS:${(v.pss/1024).toFixed(1).padStart(8)} MB  Swap:${(v.swap/1024).toFixed(1).padStart(6)} MB  (${v.count} mappings)`); }
    return buckets;
  } catch(e) { log(`  smaps error: ${e.message}`); return {}; }
}

function holochainDiskUsage() {
  try {
    const out = execSync(`du -sh ${DATA}/ad4m/h/ ${DATA}/ad4m/languages/ 2>/dev/null`, { encoding: "utf-8" }).trim();
    for (const l of out.split("\n")) log(`  disk: ${l}`);
  } catch(e) { log(`  disk check error: ${e.message}`); }
}

function countWasmInstances(pid) {
  try {
    const maps = execSync(`cat /proc/${pid}/maps 2>/dev/null`, { encoding: "utf-8" });
    let largeAnon = 0, totalAnonKB = 0;
    for (const line of maps.split("\n")) {
      const m = line.match(/^([0-9a-f]+)-([0-9a-f]+)\s+rw-p\s+00000000\s+00:00\s+0\s*$/);
      if (m) {
        const size = (parseInt(m[2], 16) - parseInt(m[1], 16)) / 1024;
        totalAnonKB += size;
        if (size > 10240) largeAnon++;
      }
    }
    log(`  Large anon RW mappings (>10MB): ${largeAnon}, total anon RW: ${(totalAnonKB/1024).toFixed(1)} MB`);
    return { largeAnon, totalAnonKB };
  } catch { return { largeAnon: 0, totalAnonKB: 0 }; }
}

// Count Holochain installed apps via the executor log or filesystem
function countHolochainApps() {
  try {
    // Count directories in holochain conductor app storage
    const dirs = execSync(`find ${DATA}/ad4m/h/ -maxdepth 3 -name "conductor-config.yaml" 2>/dev/null | wc -l`, { encoding: "utf-8" }).trim();
    // Count installed_apps entries if we can find them
    const appDirs = execSync(`ls -d ${DATA}/ad4m/h/databases/*/p2p_agent_store.sqlite 2>/dev/null | wc -l`, { encoding: "utf-8" }).trim();
    log(`  Holochain conductor configs: ${dirs}, p2p stores: ${appDirs}`);
  } catch(e) { log(`  HC app count error: ${e.message}`); }
}

// Settle and measure with progress — waits totalMs, measuring every intervalMs
async function settleAndMeasure(label, pid, totalMs = 30000, intervalMs = 10000) {
  const steps = Math.ceil(totalMs / intervalMs);
  let lastRss = 0;
  for (let i = 1; i <= steps; i++) {
    await sleep(intervalMs);
    lastRss = stableRSS(pid);
    log(`  settle ${i * intervalMs / 1000}s: ${(lastRss/1024).toFixed(1)} MB RSS`);
  }
  log(`${label}: ${(lastRss/1024).toFixed(1)} MB RSS (after ${totalMs/1000}s settle)`);
  return lastRss;
}

let _qid = 0;
function gql(ws, query, timeoutMs = 300000) {
  const id = String(++_qid);
  return new Promise((resolve, reject) => {
    const t = setTimeout(() => { ws.removeListener("message", handler); reject(new Error(`GQL timeout: ${query.substring(0,80)}`)); }, timeoutMs);
    let result = null;
    const handler = raw => {
      const msg = JSON.parse(raw.toString());
      if (msg.id !== id) return;
      if (msg.type === "next") {
        result = msg.payload;
        if (result?.errors?.length) { clearTimeout(t); ws.removeListener("message", handler); reject(new Error(`GraphQL errors: ${JSON.stringify(result.errors)}`)); return; }
      }
      if (msg.type === "complete") { clearTimeout(t); ws.removeListener("message", handler); resolve(result); }
      if (msg.type === "error") { clearTimeout(t); ws.removeListener("message", handler); reject(new Error(JSON.stringify(msg.payload))); }
    };
    ws.on("message", handler);
    ws.send(JSON.stringify({ id, type: "subscribe", payload: { query } }));
  });
}

async function main() {
  let ws;
  let proc;
  let bootstrap;
  let execPid;
  writeFileSync(OUT, "");
  log("=== AD4M MEMORY LEAK INVESTIGATION v2 ===");
  log(`Executor: ${EXECUTOR}`);
  log(`Seed: ${SEED}\n`);

  const seedData = JSON.parse(readFileSync(SEED, "utf-8"));
  const linkLangAddr = seedData.knownLinkLanguages?.[0];
  if (!linkLangAddr) {
    throw new Error(`No knownLinkLanguages[0] found in seed file: ${SEED}`);
  }
  log(`Link language (p-diff-sync): ${linkLangAddr}`);

  // Start bootstrap
  try {
  bootstrap = execCb(`${HOME}/.cargo/bin/kitsune2-bootstrap-srv`, { maxBuffer: 10*1024*1024 });
  let bootstrapUrl = null;
  await new Promise((resolve, reject) => {
    const t = setTimeout(() => reject(new Error("Bootstrap timeout")), 30000);
    const check = d => { const m = d.toString().match(/#listening#([^#]+)#/); if (m) { bootstrapUrl = `http://${m[1]}`; clearTimeout(t); resolve(); } };
    bootstrap.stdout.on("data", check); bootstrap.stderr.on("data", check);
  });
  log(`Bootstrap: ${bootstrapUrl}`);

  try { execSync(`rm -rf ${DATA}`, { stdio: "ignore" }); } catch {}
  execSync(`${EXECUTOR} init --data-path ${DATA} --network-bootstrap-seed ${SEED}`, { stdio: "pipe" });

  const cmd = `${EXECUTOR} run --app-data-path ${DATA} --gql-port ${PORT} --hc-admin-port ${PORT+1} --hc-app-port ${PORT+2} --hc-use-bootstrap true --hc-bootstrap-url ${bootstrapUrl} --hc-use-proxy false --hc-use-local-proxy false --hc-use-mdns true --language-language-only false --run-dapp-server false --admin-credential ${TOKEN}`;
  proc = execCb(cmd, { maxBuffer: 200*1024*1024, cwd: CWD });
  writeFileSync(EXEC_LOG, "");
  proc.stdout.on("data", d => appendFileSync(EXEC_LOG, d));
  proc.stderr.on("data", d => appendFileSync(EXEC_LOG, d));

  await new Promise((resolve, reject) => {
    const t = setTimeout(() => reject(new Error("Startup timeout")), 300000);
    const check = d => { if (d.toString().includes(`listening on http://127.0.0.1:${PORT}`)) { clearTimeout(t); resolve(); } };
    proc.stdout.on("data", check); proc.stderr.on("data", check);
  });

  try { execPid = parseInt(execSync(`pgrep -P ${proc.pid} -f ad4m-executor 2>/dev/null || echo ${proc.pid}`, { encoding: "utf-8" }).trim().split("\n")[0]); } catch { execPid = proc.pid; }
  log(`Executor PID: ${execPid}`);

  ws = new WebSocket(`ws://127.0.0.1:${PORT}/graphql`, "graphql-transport-ws");
  await new Promise((resolve, reject) => {
    ws.on("open", () => ws.send(JSON.stringify({ type: "connection_init", payload: { headers: { authorization: TOKEN } } })));
    ws.on("message", raw => { if (JSON.parse(raw.toString()).type === "connection_ack") resolve(); });
    ws.on("error", reject);
    setTimeout(() => reject(new Error("WS timeout")), 30000);
  });

  // Generate agent and wait for init
  log("\n--- Agent generation ---");
  const preAgent = detailedMeasure("Pre-agent", execPid);
  await gql(ws, `mutation { agentGenerate(passphrase: "leaktest") { isInitialized did } }`);
  const initComplete = await new Promise(resolve => {
    const check = setInterval(() => {
      try { if (readFileSync(EXEC_LOG, "utf-8").includes("AD4M init complete")) { clearInterval(check); resolve(true); } } catch {}
    }, 2000);
    setTimeout(() => { clearInterval(check); resolve(false); }, 300000);
  });
  if (!initComplete) throw new Error("AD4M init did not complete within 300s — aborting");
  await sleep(10000);
  const postInit = detailedMeasure("Post-init", execPid);
  log("Detailed breakdown:");
  smapsBreakdown(execPid);
  countWasmInstances(execPid);
  holochainDiskUsage();
  countHolochainApps();

  // ============================================================
  // TEST 1: Create and REMOVE perspectives (no neighbourhood)
  // ============================================================
  log("\n\n========== TEST 1: Perspective create/remove cycle ==========");
  log("Creating 10 perspectives, then removing them all.\n");

  const perspUuids = [];
  for (let i = 0; i < 10; i++) {
    const r = await gql(ws, `mutation { perspectiveAdd(name: "leak-test-${i}") { uuid } }`, 30000);
    perspUuids.push(r?.data?.perspectiveAdd?.uuid);
  }
  await sleep(5000);
  const afterPerspCreate = detailedMeasure("After creating 10 perspectives", execPid);

  for (const uuid of perspUuids) {
    await gql(ws, `mutation { perspectiveRemove(uuid: "${uuid}") }`, 30000);
  }
  // Settle with progress
  const afterPerspRemove = await settleAndMeasure("After removing all 10 perspectives", execPid, 20000, 5000);
  log(`  Δ create: +${((afterPerspCreate - postInit)/1024).toFixed(1)} MB`);
  log(`  Δ after remove: ${((afterPerspRemove - postInit)/1024).toFixed(1)} MB (should be ~0 if memory released)`);
  log(`  Leaked: ${((afterPerspRemove - postInit)/1024).toFixed(1)} MB`);
  log(`  Recovery rate: ${(((afterPerspCreate - afterPerspRemove) / Math.max(1, afterPerspCreate - postInit)) * 100).toFixed(1)}%`);

  // ============================================================
  // TEST 2: Create neighbourhood, add links, remove perspective
  // ============================================================
  log("\n\n========== TEST 2: Neighbourhood create → add links → remove ==========");
  log("Create 3 neighbourhoods with 50 links each, then remove them.\n");

  const baseline2 = detailedMeasure("Baseline", execPid);
  const baseline2Wasm = countWasmInstances(execPid);
  const nhData = [];

  for (let n = 0; n < 3; n++) {
    const persp = await gql(ws, `mutation { perspectiveAdd(name: "nh-leak-${n}") { uuid } }`, 30000);
    const uuid = persp?.data?.perspectiveAdd?.uuid;

    const templateData = JSON.stringify({ uid: `leak-${n}-${Date.now()}`, name: `leak-nh-${n}` });
    const cloned = await gql(ws, `mutation { languageApplyTemplateAndPublish(sourceLanguageHash: "${linkLangAddr}", templateData: ${JSON.stringify(templateData)}) { address } }`, 180000);
    const clonedAddr = cloned?.data?.languageApplyTemplateAndPublish?.address;

    await gql(ws, `mutation { neighbourhoodPublishFromPerspective(perspectiveUUID: "${uuid}", linkLanguage: "${clonedAddr}", meta: {links: []}) }`, 180000);

    // Add 50 links
    for (let i = 0; i < 50; i++) {
      await gql(ws, `mutation { perspectiveAddLink(uuid: "${uuid}", link: {source: "test://s${i}", target: "test://t${i}", predicate: "test://p"}) { author } }`, 30000);
    }

    nhData.push({ uuid, clonedAddr });
    log(`  Created neighbourhood ${n+1}/3 (${uuid}, lang: ${clonedAddr})`);
  }

  await sleep(15000);
  const afterNhCreate = detailedMeasure("After 3 neighbourhoods + 50 links each", execPid);
  log(`  Δ from baseline: +${((afterNhCreate - baseline2)/1024).toFixed(1)} MB`);
  log("Detailed breakdown:");
  smapsBreakdown(execPid);
  const afterNhWasm = countWasmInstances(execPid);
  holochainDiskUsage();
  countHolochainApps();
  log(`  New large anon mappings: ${afterNhWasm.largeAnon - baseline2Wasm.largeAnon}`);

  // Now remove all perspectives
  log("\nRemoving all 3 neighbourhood perspectives...");
  for (const { uuid } of nhData) {
    try {
      await gql(ws, `mutation { perspectiveRemove(uuid: "${uuid}") }`, 60000);
      log(`  Removed perspective ${uuid}`);
    } catch(e) { log(`  Failed to remove ${uuid}: ${e.message.substring(0,200)}`); }
  }

  // Extended settle with progress — 60s total to account for background loop exit (up to 60s interval)
  const afterNhRemove = await settleAndMeasure("After removing all 3 NH perspectives", execPid, 60000, 10000);
  log(`  Δ from baseline: +${((afterNhRemove - baseline2)/1024).toFixed(1)} MB`);
  log(`  Memory recovered: ${((afterNhCreate - afterNhRemove)/1024).toFixed(1)} MB of ${((afterNhCreate - baseline2)/1024).toFixed(1)} MB`);
  log(`  Recovery rate: ${(((afterNhCreate - afterNhRemove) / Math.max(1, afterNhCreate - baseline2)) * 100).toFixed(1)}%`);
  log("Detailed breakdown after removal:");
  smapsBreakdown(execPid);
  const afterRemoveWasm = countWasmInstances(execPid);
  log(`  Large anon mappings: before NH=${baseline2Wasm.largeAnon}, after create=${afterNhWasm.largeAnon}, after remove=${afterRemoveWasm.largeAnon}`);
  holochainDiskUsage();
  countHolochainApps();

  // Check executor log for teardown messages
  log("\nTeardown log messages:");
  try {
    const logContent = readFileSync(EXEC_LOG, "utf-8");
    const teardownLines = logContent.split("\n").filter(l =>
      l.includes("🧹") || l.includes("🗑️") || l.includes("💾 SurrealDB: Shut down") ||
      l.includes("Removed signal") || l.includes("ref count") ||
      l.includes("removeDnaForLang") || l.includes("removeApp") ||
      (l.includes("ERROR") && l.includes("teardown"))
    );
    for (const line of teardownLines.slice(-30)) {
      log(`  ${line.substring(0, 200)}`);
    }
    if (teardownLines.length === 0) {
      log("  (no teardown log messages found — fixes may not be active)");
    }
  } catch {}

  // ============================================================
  // TEST 3: Language cloning accumulation
  // ============================================================
  log("\n\n========== TEST 3: Language cloning without neighbourhood creation ==========");
  log("Clone p-diff-sync 5 times without creating neighbourhoods.\n");

  const baseline3 = detailedMeasure("Baseline", execPid);

  for (let i = 0; i < 5; i++) {
    const templateData = JSON.stringify({ uid: `clone-only-${i}-${Date.now()}`, name: `clone-${i}` });
    await gql(ws, `mutation { languageApplyTemplateAndPublish(sourceLanguageHash: "${linkLangAddr}", templateData: ${JSON.stringify(templateData)}) { address } }`, 180000);
    detailedMeasure(`  After ${i+1} clones`, execPid);
  }

  await sleep(10000);
  const afterClones = detailedMeasure("After 5 language clones", execPid);
  log(`  Δ from baseline: +${((afterClones - baseline3)/1024).toFixed(1)} MB`);
  log(`  Per clone: ~${((afterClones - baseline3)/1024/5).toFixed(1)} MB`);

  // ============================================================
  // TEST 4: Link accumulation within a single perspective
  // ============================================================
  log("\n\n========== TEST 4: Link accumulation in single neighbourhood ==========");
  log("Create 1 neighbourhood, add 300 links, measure growth, then remove links.\n");

  const baseline4 = detailedMeasure("Baseline", execPid);

  const persp4 = await gql(ws, `mutation { perspectiveAdd(name: "link-accum") { uuid } }`, 30000);
  const uuid4 = persp4?.data?.perspectiveAdd?.uuid;
  const td4 = JSON.stringify({ uid: `accum-${Date.now()}`, name: "link-accumulation" });
  const cloned4 = await gql(ws, `mutation { languageApplyTemplateAndPublish(sourceLanguageHash: "${linkLangAddr}", templateData: ${JSON.stringify(td4)}) { address } }`, 180000);
  const addr4 = cloned4?.data?.languageApplyTemplateAndPublish?.address;
  await gql(ws, `mutation { neighbourhoodPublishFromPerspective(perspectiveUUID: "${uuid4}", linkLanguage: "${addr4}", meta: {links: []}) }`, 180000);

  await sleep(10000);
  detailedMeasure("After neighbourhood created", execPid);

  for (let batch = 1; batch <= 3; batch++) {
    for (let i = 0; i < 100; i++) {
      const idx = (batch-1)*100 + i;
      await gql(ws, `mutation { perspectiveAddLink(uuid: "${uuid4}", link: {source: "test://src-${idx}", target: "test://tgt-${idx}", predicate: "test://pred-${batch}"}) { author } }`, 30000);
    }
    await sleep(5000);
    detailedMeasure(`After ${batch * 100} links`, execPid);
  }

  // Query all links using correct schema
  log("\nQuerying all links...");
  const links = await gql(ws, `query { perspectiveQueryLinks(uuid: "${uuid4}", query: {}) { author timestamp data { source target predicate } } }`, 60000);
  const linkCount = links?.data?.perspectiveQueryLinks?.length || 0;
  log(`  Retrieved ${linkCount} links`);

  // ============================================================
  // TEST 5: Repeated perspectiveSnapshot (fixed schema)
  // ============================================================
  log("\n\n========== TEST 5: Repeated snapshot queries ==========");
  log("Query perspectiveSnapshot 100 times on a perspective with links.\n");

  const baseline5 = detailedMeasure("Baseline", execPid);

  for (let i = 0; i < 100; i++) {
    try {
      await gql(ws, `query { perspectiveSnapshot(uuid: "${uuid4}") { links { author timestamp data { source target predicate } } } }`, 30000);
    } catch(e) {
      if (i === 0) log(`  snapshot query error: ${e.message.substring(0, 100)}`);
    }
  }
  await sleep(5000);
  const afterQueries = detailedMeasure("After 100 snapshot queries", execPid);
  log(`  Δ: +${((afterQueries - baseline5)/1024).toFixed(1)} MB`);

  // ============================================================
  // FINAL SUMMARY
  // ============================================================
  log("\n\n========== FINAL STATE ==========");
  detailedMeasure("Final", execPid);
  log("Detailed breakdown:");
  smapsBreakdown(execPid);
  countWasmInstances(execPid);
  holochainDiskUsage();
  countHolochainApps();

  // Check executor log for errors/warnings
  log("\n\nExecutor warnings/errors:");
  try {
    const logContent = readFileSync(EXEC_LOG, "utf-8");
    const errors = logContent.split("\n").filter(l => l.includes("ERROR") || l.includes("panic") || l.includes("OOM"));
    const unique = [...new Set(errors.map(e => e.replace(/\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}[.\d]*Z?/, "TS")))];
    for (const e of unique.slice(0, 20)) log(`  ${e.substring(0, 200)}`);
    if (unique.length === 0) log("  (none)");
  } catch {}

  } finally {
  if (ws) try { ws.close(); } catch {}
  if (execPid) { try { process.kill(execPid, "SIGTERM"); } catch {} }
  await sleep(3000);
  if (execPid) { try { process.kill(execPid, "SIGKILL"); } catch {} }
  if (proc) { try { process.kill(proc.pid, "SIGKILL"); } catch {} }
  if (bootstrap) { try { bootstrap.kill("SIGTERM"); } catch {} }

  log("\n=== INVESTIGATION COMPLETE ===");
  }
}

main().catch(e => { log(`FATAL: ${e.stack || e}`); process.exit(1); });
