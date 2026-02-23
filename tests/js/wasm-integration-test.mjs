#!/usr/bin/env node
// WASM Language Integration Test v3 - HTTP GQL + WASM language loading
import { execSync, exec as execCb } from "node:child_process";
import { appendFileSync, writeFileSync, readFileSync, mkdirSync, copyFileSync, existsSync } from "node:fs";
import path from "node:path";

const HOME = process.env.HOME;
const EXECUTOR = process.env.AD4M_EXECUTOR || `${HOME}/ad4m-bin/ad4m-executor-wasm`;
const WASM_LANG = `${HOME}/ad4m/examples/wasm-languages/p-diff-sync-wasm/target/wasm32-unknown-unknown/release/p_diff_sync_wasm.wasm`;
const SEED = process.env.AD4M_SEED || "/tmp/ad4m-prepared-seed.json";
const DATA = "/tmp/ad4m-wasm-integ-data";
const EXEC_LOG = "/tmp/ad4m-wasm-integ.log";
const PORT = 15900;
const TOKEN = "wasm-integ-test";

const sleep = ms => new Promise(r => setTimeout(r, ms));
const log = msg => console.log(`[${new Date().toISOString()}] ${msg}`);

async function gql(query, timeoutMs = 120000) {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeoutMs);
  try {
    const res = await fetch(`http://127.0.0.1:${PORT}/graphql`, {
      method: "POST",
      headers: { "Content-Type": "application/json", "Authorization": TOKEN },
      body: JSON.stringify({ query }),
      signal: controller.signal,
    });
    clearTimeout(timer);
    const json = await res.json();
    if (json.errors) throw new Error(JSON.stringify(json.errors));
    return json;
  } catch (e) {
    clearTimeout(timer);
    throw new Error(`GQL error: ${e.message} | query: ${query.slice(0,80)}`);
  }
}

function measureRSS(pid) {
  try {
    return parseInt(execSync(`ps -o rss= -p ${pid}`, { encoding: "utf-8" }).trim()) || 0;
  } catch { return 0; }
}

async function waitForServer(maxWait = 60000) {
  const start = Date.now();
  while (Date.now() - start < maxWait) {
    try {
      const res = await fetch(`http://127.0.0.1:${PORT}/graphql`, {
        method: "POST",
        headers: { "Content-Type": "application/json", "Authorization": TOKEN },
        body: JSON.stringify({ query: "{ agentStatus { isInitialized } }" }),
        signal: AbortSignal.timeout(2000),
      });
      if (res.ok) return true;
    } catch {}
    await sleep(1000);
  }
  return false;
}

async function main() {
  log("=== WASM Language Integration Test v3 ===");
  
  if (!existsSync(WASM_LANG)) {
    log(`ERROR: WASM language not found at ${WASM_LANG}`);
    process.exit(1);
  }
  const wasmSize = readFileSync(WASM_LANG).length;
  log(`WASM language: ${(wasmSize / 1024).toFixed(0)} KB`);

  // Init executor data directory
  log("Initializing executor data...");
  execSync(`rm -rf ${DATA}`);
  execSync(`${EXECUTOR} init --data-path ${DATA} --network-bootstrap-seed ${SEED}`, { stdio: "pipe" });
  log("Init complete");
  
  // Place WASM file where the executor can find it
  const wasmDir = path.join(DATA, "languages", "wasm-pdiffsync-test");
  mkdirSync(wasmDir, { recursive: true });
  const wasmDest = path.join(wasmDir, "bundle.wasm");
  copyFileSync(WASM_LANG, wasmDest);
  log(`WASM bundle placed at: ${wasmDest}`);
  
  // Start bootstrap server
  log("Starting bootstrap server...");
  const bootstrap = execCb(`${HOME}/.cargo/bin/kitsune2-bootstrap-srv`, { maxBuffer: 10*1024*1024 });
  let bootstrapUrl = await new Promise((resolve, reject) => {
    const t = setTimeout(() => { bootstrap.kill(); reject(new Error("bootstrap timeout")); }, 10000);
    const check = d => {
      const m = d.toString().match(/#listening#([^#]+)#/);
      if (m) { clearTimeout(t); resolve(`http://${m[1]}`); }
    };
    bootstrap.stdout.on("data", check);
    bootstrap.stderr.on("data", check);
  });
  log(`Bootstrap: ${bootstrapUrl}`);
  
  // Start executor
  const cmd = `${EXECUTOR} run --app-data-path ${DATA} --gql-port ${PORT} --hc-admin-port ${PORT+1} --hc-app-port ${PORT+2} --hc-use-bootstrap true --hc-bootstrap-url ${bootstrapUrl} --hc-use-proxy false --hc-use-local-proxy false --hc-use-mdns true --language-language-only false --run-dapp-server false --network-bootstrap-seed ${SEED} --admin-credential ${TOKEN}`;
  
  writeFileSync(EXEC_LOG, "");
  const child = execCb(cmd, { env: { ...process.env, RUST_LOG: "info" } });
  child.stdout.on("data", d => appendFileSync(EXEC_LOG, d));
  child.stderr.on("data", d => appendFileSync(EXEC_LOG, d));
  
  const pid = child.pid;
  log(`Executor PID: ${pid}`);
  
  // Wait for HTTP endpoint
  log("Waiting for executor...");
  if (!await waitForServer()) {
    log("ERROR: Could not connect to executor");
    try { console.log(execSync(`tail -30 ${EXEC_LOG}`, { encoding: "utf-8" })); } catch {}
    child.kill("SIGTERM"); bootstrap.kill();
    process.exit(1);
  }
  log("Connected (HTTP)");
  
  // Generate agent
  log("Generating agent...");
  try {
    const r = await gql(`mutation { agentGenerate(passphrase: "wasmtest") { isInitialized did } }`, 120000);
    log(`Agent: ${r?.data?.agentGenerate?.did?.slice(0, 40)}...`);
  } catch(e) {
    log(`Agent error: ${e.message}`);
    log("Continuing without agent (some operations may fail)...");
  }
  
  await sleep(3000);
  const rss1 = measureRSS(pid);
  log(`Post-init RSS: ${(rss1/1024).toFixed(1)} MB`);
  
  // === Test 1: Perspective CRUD ===
  log("\n--- Test 1: Perspective CRUD ---");
  const perspResult = await gql(`mutation { perspectiveAdd(name: "wasm-test") { uuid } }`);
  const uuid = perspResult?.data?.perspectiveAdd?.uuid;
  log(`Perspective created: ${uuid}`);
  
  // Add links
  log("Adding 10 links...");
  for (let i = 0; i < 10; i++) {
    await gql(`mutation { perspectiveAddLink(uuid: "${uuid}", link: {source: "test://s${i}", target: "test://t${i}", predicate: "test://p"}) { author } }`);
  }
  
  const qr = await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: {}) { data { source target predicate } } }`);
  const linkCount = qr?.data?.perspectiveQueryLinks?.length || 0;
  log(`Query: ${linkCount} links ${linkCount === 10 ? '✓' : '✗ EXPECTED 10'}`);
  
  // Remove perspective
  await gql(`mutation { perspectiveRemove(uuid: "${uuid}") }`);
  log("Perspective removed ✓");
  
  // === Test 2: WASM Language Install ===
  log("\n--- Test 2: WASM Language Install ---");
  const wasmAddress = "wasm-pdiffsync-test";
  try {
    const installResult = await gql(`mutation { languageInstallWasm(wasmPath: "${wasmDest}", address: "${wasmAddress}") }`);
    log(`WASM language installed: ${JSON.stringify(installResult.data)} ✓`);
  } catch(e) {
    log(`WASM install error: ${e.message}`);
    // Check executor log for details
    try {
      const logTail = execSync(`tail -10 ${EXEC_LOG}`, { encoding: "utf-8" });
      log(`Executor log:\n${logTail}`);
    } catch {}
  }
  
  // === Test 3: WASM Language Expression Operations ===
  log("\n--- Test 3: WASM Language Expression Ops ---");
  try {
    // Try creating an expression through the WASM language
    const expr = await gql(`mutation { expressionCreate(content: "{\\"title\\":\\"test note\\",\\"body\\":\\"hello from WASM\\"}", languageAddress: "${wasmAddress}") }`);
    log(`Expression created: ${JSON.stringify(expr.data)} ✓`);
  } catch(e) {
    log(`Expression create: ${e.message}`);
  }
  
  try {
    // Query expression interactions
    const interactions = await gql(`query { languageByAddress(address: "${wasmAddress}") { name } }`);
    log(`Language info: ${JSON.stringify(interactions.data)}`);
  } catch(e) {
    log(`Language query: ${e.message}`);
  }

  const rss2 = measureRSS(pid);
  log(`\nFinal RSS: ${(rss2/1024).toFixed(1)} MB`);
  
  log("\n=== Test Complete ===");
  log(`RSS: init=${(rss1/1024).toFixed(0)}MB final=${(rss2/1024).toFixed(0)}MB`);
  
  child.kill("SIGTERM");
  bootstrap.kill();
  await sleep(2000);
  process.exit(0);
}

main().catch(e => { console.error("FATAL:", e); process.exit(1); });
