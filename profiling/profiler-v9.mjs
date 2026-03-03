#!/usr/bin/env node
// AD4M Profiler v9 — With published languages, neighbourhood profiling
// Requires: npm install ws (or run from repo root where ws is in devDependencies)
import WebSocket from "ws";
import { execSync, exec as execCb } from "node:child_process";
import { appendFileSync, writeFileSync, readFileSync } from "node:fs";
import path from "node:path";

const HOME = process.env.HOME;
const EXECUTOR = `${HOME}/ad4m-bin/ad4m-executor`;
const SEED = "/tmp/ad4m-prepared-seed.json";
const AD4M_DIR = `${HOME}/ad4m`;
const CWD = `${AD4M_DIR}/tests/js`; // critical: language-language uses ./tst-tmp/languages relative to CWD
const OUT = "/tmp/ad4m-profile-v9.txt";
const DATA = "/tmp/ad4m-profile-v9-data";
const EXEC_LOG = "/tmp/ad4m-executor-v9.log";
const PORT = 15800;
const TOKEN = "profile-v9";

const sleep = ms => new Promise(r => setTimeout(r, ms));
const log = msg => { const l = `[${new Date().toISOString()}] ${msg}`; console.log(l); appendFileSync(OUT, l + "\n"); };

function getAllPids(pid) {
  const result = [String(pid)];
  try {
    const ch = execSync(`pgrep -P ${pid} 2>/dev/null || true`, { encoding: "utf-8" }).trim();
    if (ch) for (const c of ch.split("\n").filter(Boolean)) result.push(...getAllPids(parseInt(c)));
  } catch {}
  return [...new Set(result)];
}

function measure(label, pid) {
  try {
    const pids = getAllPids(pid);
    const raw = execSync(`ps -o pid=,rss=,vsz=,comm= -p ${pids.join(",")} 2>/dev/null || true`, { encoding: "utf-8" }).trim();
    let totalRSS = 0, details = [];
    for (const line of raw.split("\n").filter(Boolean)) {
      const p = line.trim().split(/\s+/);
      if (p.length >= 4) { const rss = parseInt(p[1])||0; totalRSS += rss; details.push(`  PID ${p[0]}: ${(rss/1024).toFixed(1)}MB — ${p.slice(3).join(" ")}`); }
    }
    log(`${label}: ${(totalRSS/1024).toFixed(1)} MB RSS`);
    for (const d of details) log(d);
    return totalRSS;
  } catch(e) { log(`${label}: measure failed — ${e.message}`); return 0; }
}

function smapsSummary(pid) {
  try {
    const raw = execSync(`cat /proc/${pid}/smaps 2>/dev/null`, { encoding: "utf-8", maxBuffer: 50*1024*1024 });
    const buckets = {};
    let name = null, rss = 0;
    const cat = n => { const l=n.toLowerCase(); if(l.includes("ad4m")||l.includes("executor")) return "ad4m-executor"; if(n==="[heap]") return "[heap]"; if(n.startsWith("[stack")) return "[stack]"; if(n==="[anon]"||n==="") return "[anonymous]"; if(l.includes("libc")||l.includes("libm.so")||l.includes("ld-linux")) return "libc/system"; if(l.startsWith("/usr/lib")||l.startsWith("/lib")) return "system-libs"; return "other"; };
    const flush = () => { if(name===null) return; const c=cat(name); buckets[c]=(buckets[c]||0)+rss; };
    for (const line of raw.split("\n")) {
      const h = line.match(/^[0-9a-f]+-[0-9a-f]+\s+\S+\s+\S+\s+\S+\s+\d+\s*(.*)/);
      if (h) { flush(); name=h[1].trim()||"[anon]"; rss=0; continue; }
      const kv = line.match(/^Rss:\s+(\d+)\s+kB/);
      if (kv) rss = parseInt(kv[1]);
    }
    flush();
    const sorted = Object.entries(buckets).sort((a,b)=>b[1]-a[1]);
    const total = sorted.reduce((s,[,v])=>s+v,0);
    for (const [c,v] of sorted) { if(v===0) continue; log(`  ${c.padEnd(22)} ${(v/1024).toFixed(1).padStart(7)} MB (${(v*100/total|0)}%)`); }
  } catch {}
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
  log("=== AD4M Profiler v9 — With Published Languages ===");
  
  // Start local bootstrap service
  log("Starting kitsune2-bootstrap-srv...");
  try {
  bootstrap = execCb(`${HOME}/.cargo/bin/kitsune2-bootstrap-srv`, { maxBuffer: 10*1024*1024 });
  let bootstrapUrl = null;
  await new Promise((resolve, reject) => {
    const t = setTimeout(() => reject(new Error("Bootstrap timeout")), 30000);
    const check = d => {
      const m = d.toString().match(/#listening#([^#]+)#/) || d.toString().match(/Bound with local address:\s+(\S+)/);
      if (m) { bootstrapUrl = `http://${m[1]}`; clearTimeout(t); resolve(); }
    };
    bootstrap.stdout.on("data", check);
    bootstrap.stderr.on("data", check);
  });
  log(`Bootstrap: ${bootstrapUrl}`);
  
  try { execSync(`rm -rf ${DATA}`, { stdio: "ignore" }); } catch {}
  execSync(`${EXECUTOR} init --data-path ${DATA} --network-bootstrap-seed ${SEED}`, { stdio: "pipe" });
  log("Executor initialized");
  
  const cmd = `${EXECUTOR} run --app-data-path ${DATA} --gql-port ${PORT} --hc-admin-port ${PORT+1} --hc-app-port ${PORT+2} --hc-use-bootstrap true --hc-bootstrap-url ${bootstrapUrl} --hc-use-proxy false --hc-use-local-proxy false --hc-use-mdns true --language-language-only false --run-dapp-server false --admin-credential ${TOKEN}`;
  log(`CMD: ${cmd}`);
  
  proc = execCb(cmd, { maxBuffer: 200*1024*1024, cwd: CWD });
  writeFileSync(EXEC_LOG, "");
  proc.stdout.on("data", d => appendFileSync(EXEC_LOG, d));
  proc.stderr.on("data", d => appendFileSync(EXEC_LOG, d));
  
  await new Promise((resolve, reject) => {
    const t = setTimeout(() => {
      log("Startup timeout — last 20 lines:");
      try { log(execSync(`tail -20 ${EXEC_LOG}`, { encoding: "utf-8" })); } catch {}
      reject(new Error("Startup timeout 300s"));
    }, 300000);
    const check = d => { if (d.toString().includes(`listening on http://127.0.0.1:${PORT}`)) { clearTimeout(t); resolve(); } };
    proc.stdout.on("data", check);
    proc.stderr.on("data", check);
  });
  log("GraphQL ready!");
  
    try { execPid = parseInt(execSync(`pgrep -P ${proc.pid} -f ad4m-executor 2>/dev/null || echo ${proc.pid}`, { encoding: "utf-8" }).trim().split("\n")[0]); } catch { execPid = proc.pid; }
  log(`Executor PID: ${execPid}`);
  
  await sleep(3000);
  measure("Pre-agent baseline", execPid);
  smapsSummary(execPid);
  
  ws = new WebSocket(`ws://127.0.0.1:${PORT}/graphql`, "graphql-transport-ws");
  await new Promise((resolve, reject) => {
    ws.on("open", () => ws.send(JSON.stringify({ type: "connection_init", payload: { headers: { authorization: TOKEN } } })));
    ws.on("message", raw => { if (JSON.parse(raw.toString()).type === "connection_ack") resolve(); });
    ws.on("error", reject);
    setTimeout(() => reject(new Error("WS timeout")), 30000);
  });
  log("WebSocket connected!");
  
  log("\nGenerating agent...");
  const agent = await gql(ws, `mutation { agentGenerate(passphrase: "profiler9") { isInitialized did } }`);
  log(`Agent: ${JSON.stringify(agent).substring(0, 200)}`);
  
  // Wait for AD4M init
  log("Waiting for AD4M init...");
  await new Promise(resolve => {
    const check = setInterval(() => {
      try { if (readFileSync(EXEC_LOG, "utf-8").includes("AD4M init complete")) { clearInterval(check); resolve(); } } catch {}
    }, 2000);
    setTimeout(() => { clearInterval(check); resolve(); }, 300000);
  });
  log("AD4M init complete!");
  await sleep(10000);
  
  measure("Post-init (languages loaded)", execPid);
  smapsSummary(execPid);
  
  // List languages
  log("\nListing installed languages...");
  const langs = await gql(ws, `query { languages { address name } }`, 30000);
  const langList = langs?.data?.languages || [];
  log(`Found ${langList.length} languages:`);
  for (const l of langList) log(`  ${l.name}: ${l.address}`);
  
  if (langList.length === 0) {
    log("\nNo languages — checking log...");
    try { const el = readFileSync(EXEC_LOG, "utf-8").split("\n").filter(l => l.includes("ERROR") || l.includes("language")).slice(-15); for (const l of el) log(`  ${l.substring(0, 200)}`); } catch {}
  }
  
  // Use known link language hash from seed
  const seedData = JSON.parse(readFileSync(SEED, "utf-8"));
  const linkLangAddr = seedData.knownLinkLanguages?.[0];
  log(`\nUsing link language from seed: ${linkLangAddr}`);
  
  if (linkLangAddr) {
    log(`\n=== NEIGHBOURHOOD PROFILING with perspective-diff-sync (${linkLangAddr}) ===`);
    
    const measurements = [];
    for (let n = 1; n <= 5; n++) {
      log(`\n--- Creating neighbourhood ${n}/5 ---`);
      try {
        const persp = await gql(ws, `mutation { perspectiveAdd(name: "profile-nh-${n}") { uuid } }`, 30000);
        const uuid = persp?.data?.perspectiveAdd?.uuid;
        log(`  Perspective: ${uuid}`);
        
        const templateData = JSON.stringify({ uid: `nh-${n}-${Date.now()}`, name: `profiler-nh-${n}` });
        log(`  Cloning link language...`);
        const cloned = await gql(ws, `mutation { languageApplyTemplateAndPublish(sourceLanguageHash: "${linkLangAddr}", templateData: ${JSON.stringify(templateData)}) { address name } }`, 180000);
        const clonedAddr = cloned?.data?.languageApplyTemplateAndPublish?.address;
        log(`  Cloned language: ${clonedAddr}`);
        
        if (clonedAddr && uuid) {
          log(`  Publishing neighbourhood...`);
          const nh = await gql(ws, `mutation { neighbourhoodPublishFromPerspective(perspectiveUUID: "${uuid}", linkLanguage: "${clonedAddr}", meta: {links: []}) }`, 180000);
          log(`  Neighbourhood: ${JSON.stringify(nh).substring(0, 200)}`);
          
          // Add some links
          log(`  Adding links...`);
          for (let i = 0; i < 10; i++) {
            await gql(ws, `mutation { perspectiveAddLink(uuid: "${uuid}", link: {source: "test://source-${i}", target: "test://target-${i}", predicate: "test://predicate"}) { author timestamp } }`, 30000);
          }
          log(`  Added 10 links`);
        }
        
        await sleep(15000);
        const rss = measure(`After ${n} neighbourhood(s) + 10 links each`, execPid);
        measurements.push({ n, rss: rss/1024 });
        if (n === 1 || n === 3 || n === 5) smapsSummary(execPid);
        
      } catch(e) {
        log(`  FAILED: ${e.message.substring(0, 300)}`);
        measure(`After neighbourhood ${n} attempt`, execPid);
      }
    }
    
    log("\n=== MEMORY GROWTH SUMMARY ===");
    for (const m of measurements) log(`  ${m.n} neighbourhoods: ${m.rss.toFixed(1)} MB`);
    if (measurements.length >= 2) {
      const first = measurements[0].rss;
      const last = measurements[measurements.length - 1].rss;
      const perNh = (last - first) / (measurements.length - 1);
      log(`  Growth per neighbourhood: ~${perNh.toFixed(1)} MB`);
    }
  } else {
    log("\nNo link language hash in seed — cannot create neighbourhoods");
  }
  
  log("\n=== FINAL ===");
  measure("Final", execPid);
  smapsSummary(execPid);
  log(`Data dir: ${execSync(`du -sh ${DATA}`, { encoding: "utf-8" }).trim()}`);
  
  } finally {
  if (ws) try { ws.close(); } catch {}
  if (execPid) { try { process.kill(execPid, "SIGTERM"); } catch {} }
  await sleep(2000);
  if (execPid) { try { process.kill(execPid, "SIGKILL"); } catch {} }
  if (proc) { try { process.kill(proc.pid, "SIGKILL"); } catch {} }
  if (bootstrap) { try { bootstrap.kill("SIGTERM"); } catch {} }
  
  log("\n=== PROFILING COMPLETE ===");
  }
}

main().catch(e => { log(`FATAL: ${e.stack || e}`); process.exit(1); });
