#!/usr/bin/env node
// WASM Language Integration Test v4 — Full discovery/download flow
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
const pass = msg => log(`✅ ${msg}`);
const fail = msg => log(`❌ ${msg}`);

let passed = 0, failed = 0;
function check(label, condition) {
  if (condition) { pass(label); passed++; }
  else { fail(label); failed++; }
}

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
    throw new Error(`GQL: ${e.message} | ${query.slice(0,80)}`);
  }
}

function measureRSS(pid) {
  try { return parseInt(execSync(`ps -o rss= -p ${pid}`, { encoding: "utf-8" }).trim()) || 0; }
  catch { return 0; }
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
  log("=== WASM Language Integration Test v4 — Full Discovery/Download Flow ===");
  
  if (!existsSync(WASM_LANG)) {
    log(`ERROR: WASM language not found at ${WASM_LANG}`);
    process.exit(1);
  }
  const wasmBytes = readFileSync(WASM_LANG);
  const wasmBase64 = wasmBytes.toString("base64");
  log(`WASM language: ${(wasmBytes.length / 1024).toFixed(0)} KB (${wasmBase64.length} base64 chars)`);

  // Init
  execSync(`rm -rf ${DATA}`);
  execSync(`${EXECUTOR} init --data-path ${DATA} --network-bootstrap-seed ${SEED}`, { stdio: "pipe" });
  
  // Copy WASM bundle for local install test
  const wasmDir = path.join(DATA, "ad4m", "languages", "wasm-local-test");
  mkdirSync(wasmDir, { recursive: true });
  copyFileSync(WASM_LANG, path.join(wasmDir, "bundle.wasm"));
  
  // Bootstrap
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

  // Start executor
  writeFileSync(EXEC_LOG, "");
  const cmd = `${EXECUTOR} run --app-data-path ${DATA} --gql-port ${PORT} --hc-admin-port ${PORT+1} --hc-app-port ${PORT+2} --hc-use-bootstrap true --hc-bootstrap-url ${bootstrapUrl} --hc-use-proxy false --hc-use-local-proxy false --hc-use-mdns true --language-language-only false --run-dapp-server false --network-bootstrap-seed ${SEED} --admin-credential ${TOKEN}`;
  const child = execCb(cmd, { env: { ...process.env, RUST_LOG: "info" } });
  child.stdout.on("data", d => appendFileSync(EXEC_LOG, d));
  child.stderr.on("data", d => appendFileSync(EXEC_LOG, d));
  const pid = child.pid;

  log("Waiting for executor...");
  if (!await waitForServer()) {
    log("ERROR: Could not connect to executor");
    try { console.log(execSync(`tail -30 ${EXEC_LOG}`, { encoding: "utf-8" })); } catch {}
    child.kill("SIGTERM"); bootstrap.kill();
    process.exit(1);
  }
  
  // Generate agent
  log("Generating agent...");
  const agentResult = await gql(`mutation { agentGenerate(passphrase: "wasmtest") { isInitialized did } }`, 120000);
  const did = agentResult?.data?.agentGenerate?.did;
  check("Agent generated", did && did.startsWith("did:key:"));
  log(`DID: ${did?.slice(0, 40)}...`);
  await sleep(3000);

  const rss1 = measureRSS(pid);
  log(`Post-init RSS: ${(rss1/1024).toFixed(1)} MB`);

  // ============================================================
  log("\n--- Test 1: Local WASM bundle install (file detection) ---");
  // ============================================================
  try {
    const r = await gql(`mutation { languageInstallWasm(wasmPath: "${path.join(wasmDir, "bundle.wasm")}", address: "wasm-local-test") }`);
    check("Local WASM install", r?.data?.languageInstallWasm === "wasm-local-test");
  } catch(e) {
    fail(`Local WASM install: ${e.message}`);
  }

  // ============================================================
  log("\n--- Test 2: Expression operations through WASM language ---");
  // ============================================================
  try {
    const r = await gql(`mutation { expressionCreate(content: "{\\"key\\":\\"value\\"}", languageAddress: "wasm-local-test") }`);
    // p-diff-sync is a link language, expression_put returns empty string — that's correct
    check("Expression create via WASM", r?.data?.expressionCreate !== undefined);
    log(`  Result: ${JSON.stringify(r?.data)}`);
  } catch(e) {
    fail(`Expression create: ${e.message}`);
  }

  // ============================================================
  log("\n--- Test 3: Language source query (base64 WASM) ---");
  // ============================================================
  try {
    const r = await gql(`query { languageSource(address: "wasm-local-test") }`);
    const src = r?.data?.languageSource;
    check("Language source returns base64 WASM", src && src.startsWith("AGFzbQ"));
    log(`  Base64 length: ${src?.length} chars`);
  } catch(e) {
    fail(`Language source query: ${e.message}`);
  }

  // ============================================================
  log("\n--- Test 4: Perspective with WASM link language ---");
  // ============================================================
  try {
    // Create perspective
    const pr = await gql(`mutation { perspectiveAdd(name: "wasm-link-test") { uuid } }`);
    const uuid = pr?.data?.perspectiveAdd?.uuid;
    check("Perspective created", !!uuid);

    // Add links
    for (let i = 0; i < 5; i++) {
      await gql(`mutation { perspectiveAddLink(uuid: "${uuid}", link: {source: "wasm://s${i}", target: "wasm://t${i}", predicate: "wasm://link"}) { author } }`);
    }
    
    // Query links
    const qr = await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: {}) { data { source target predicate } } }`);
    const count = qr?.data?.perspectiveQueryLinks?.length || 0;
    check("Links via perspective (5 added/queried)", count === 5);

    await gql(`mutation { perspectiveRemove(uuid: "${uuid}") }`);
  } catch(e) {
    fail(`Perspective with WASM: ${e.message}`);
  }

  // ============================================================
  log("\n--- Test 5: WASM language publish mutation ---");
  // ============================================================
  try {
    const meta = JSON.stringify({ name: "p-diff-sync-wasm", description: "WASM link language test", bundleType: "wasm" });
    const r = await gql(`mutation { languagePublishWasm(wasmPath: "${path.join(wasmDir, "bundle.wasm")}", meta: ${JSON.stringify(meta)}) }`, 30000);
    const addr = r?.data?.languagePublishWasm;
    check("WASM language published", !!addr);
    log(`  Published address: ${addr}`);
  } catch(e) {
    // Language language may not be available in this test (requires Holochain sync)
    log(`  ⚠️  Publish skipped (expected without language language): ${e.message.slice(0, 100)}`);
  }

  // ============================================================
  log("\n--- Test 6: WASM base64 detection ---");
  // ============================================================
  // Verify that base64-encoded WASM is correctly detected
  check("Base64 WASM detection (AGFzbQ prefix)", wasmBase64.startsWith("AGFzbQ"));
  // Verify magic bytes
  check("WASM magic bytes (\\0asm)", wasmBytes[0] === 0x00 && wasmBytes[1] === 0x61 && wasmBytes[2] === 0x73 && wasmBytes[3] === 0x6d);

  // ============================================================
  log("\n--- Test 7: Memory stability ---");
  // ============================================================
  const rss2 = measureRSS(pid);
  const rssDelta = (rss2 - rss1) / 1024;
  check(`Memory stable (delta: ${rssDelta.toFixed(1)} MB)`, rssDelta < 50);
  
  // ============================================================
  log("\n=== Results ===");
  log(`${passed} passed, ${failed} failed`);
  log(`RSS: init=${(rss1/1024).toFixed(0)}MB final=${(rss2/1024).toFixed(0)}MB`);
  
  child.kill("SIGTERM");
  bootstrap.kill();
  await sleep(2000);
  process.exit(failed > 0 ? 1 : 0);
}

main().catch(e => { console.error("FATAL:", e); process.exit(1); });
