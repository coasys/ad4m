#!/usr/bin/env node
// Publishes bootstrap languages to the language-language local store,
// producing a self-contained bootstrapSeed.json for neighbourhood creation.
// Equivalent to AD4M's `prepare-test` pipeline's publish-test-languages step.
import WebSocket from "ws";
import { execSync, exec as execCb } from "node:child_process";
import { readFileSync, writeFileSync, appendFileSync, existsSync } from "node:fs";
import path from "node:path";

const AD4M_DIR = path.join(process.env.HOME, "ad4m");
const EXECUTOR = path.join(process.env.HOME, "ad4m-bin/ad4m-executor");
const SEED_PATH = path.join(AD4M_DIR, "tests/js/bootstrapSeed.json");
const OUT_SEED = "/tmp/ad4m-prepared-seed.json";
const DATA_PATH = "/tmp/ad4m-publish-langs";
const PORT = 15700;
const TOKEN = "publish-token";
const LOG = "/tmp/ad4m-publish-langs.log";

const sleep = ms => new Promise(r => setTimeout(r, ms));
const log = msg => { const l = `[${new Date().toISOString()}] ${msg}`; console.log(l); appendFileSync(LOG, l + "\n"); };

const LANGUAGES_DIR = path.join(AD4M_DIR, "tests/js/tst-tmp/languages");
const languagesToPublish = {
  "agent-expression-store": { name: "agent-expression-store", description: "", possibleTemplateParams: ["uid", "name", "description"] },
  "direct-message-language": { name: "direct-message-language", description: "", possibleTemplateParams: ["uid", "recipient_did", "recipient_hc_agent_pubkey"] },
  "neighbourhood-store": { name: "neighbourhood-store", description: "", possibleTemplateParams: ["uid", "name", "description"] },
  "perspective-diff-sync": { name: "perspective-diff-sync", description: "", possibleTemplateParams: ["uid", "name", "description"] },
  "perspective-language": { name: "perspective-language", description: "", possibleTemplateParams: ["uid", "name", "description"] },
};

let _qid = 0;
function gql(ws, query, variables, timeoutMs = 300000) {
  const id = String(++_qid);
  return new Promise((resolve, reject) => {
    const t = setTimeout(() => { ws.removeListener("message", handler); reject(new Error(`GQL timeout`)); }, timeoutMs);
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
    const payload = variables ? { query, variables } : { query };
    ws.send(JSON.stringify({ id, type: "subscribe", payload }));
  });
}

async function main() {
  let ws;
  let proc;
  let bootstrap;
  writeFileSync(LOG, "");
  log("=== Publishing bootstrap languages ===");

  // Start kitsune2-bootstrap-srv
  log("Starting bootstrap service...");
  try {
  bootstrap = execCb("bash -lc 'kitsune2-bootstrap-srv'", { maxBuffer: 10*1024*1024 });
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
  log(`Bootstrap URL: ${bootstrapUrl}`);

  // Clean and init
  try { execSync(`rm -rf ${DATA_PATH}`, { stdio: "ignore" }); } catch {}
  execSync(`${EXECUTOR} init --data-path ${DATA_PATH} --network-bootstrap-seed ${SEED_PATH}`, { stdio: "pipe" });

  // Start executor
  const cmd = `${EXECUTOR} run --app-data-path ${DATA_PATH} --gql-port ${PORT} --hc-admin-port ${PORT+1} --hc-app-port ${PORT+2} --hc-use-bootstrap true --hc-bootstrap-url ${bootstrapUrl} --hc-use-proxy false --hc-use-local-proxy false --hc-use-mdns true --language-language-only false --run-dapp-server false --admin-credential ${TOKEN}`;
  log(`Starting executor: ${cmd}`);
  proc = execCb(cmd, { maxBuffer: 200*1024*1024, cwd: path.join(AD4M_DIR, "tests/js") });
  const execLog = "/tmp/ad4m-publish-executor.log";
  writeFileSync(execLog, "");
  proc.stdout.on("data", d => appendFileSync(execLog, d));
  proc.stderr.on("data", d => appendFileSync(execLog, d));

  await new Promise((resolve, reject) => {
    const t = setTimeout(() => reject(new Error("Startup timeout")), 300000);
    const check = d => {
      if (d.toString().includes(`listening on http://127.0.0.1:${PORT}`)) { clearTimeout(t); resolve(); }
    };
    proc.stdout.on("data", check);
    proc.stderr.on("data", check);
  });
  log("Executor ready!");

  // Connect WS
  ws = new WebSocket(`ws://127.0.0.1:${PORT}/graphql`, "graphql-transport-ws");
  await new Promise((resolve, reject) => {
    ws.on("open", () => ws.send(JSON.stringify({ type: "connection_init", payload: { headers: { authorization: TOKEN } } })));
    ws.on("message", raw => { if (JSON.parse(raw.toString()).type === "connection_ack") resolve(); });
    ws.on("error", reject);
    setTimeout(() => reject(new Error("WS timeout")), 30000);
  });
  log("WebSocket connected!");

  // Generate agent
  log("Generating agent...");
  const agent = await gql(ws, `mutation { agentGenerate(passphrase: "publishing-agent") { isInitialized did } }`);
  const did = agent?.data?.agentGenerate?.did;
  if (!did) {
    throw new Error("agentGenerate did not return a DID — cannot proceed");
  }
  log(`Agent DID: ${did}`);

  // Wait for init
  const initComplete = await new Promise(resolve => {
    const check = setInterval(() => {
      try {
        if (readFileSync(execLog, "utf-8").includes("AD4M init complete")) { clearInterval(check); resolve(true); }
      } catch {}
    }, 2000);
    setTimeout(() => { clearInterval(check); resolve(false); }, 120000);
  });
  if (!initComplete) throw new Error("AD4M init did not complete within 120s — aborting");
  log("AD4M init complete");
  await sleep(5000);

  // Trust our own agent
  await gql(ws, `mutation { addTrustedAgents(agents: ["${did}"]) }`, null, 30000);
  log("Trusted self");

  // Publish each language
  const hashes = {};
  for (const [dirName, meta] of Object.entries(languagesToPublish)) {
    const bundlePath = path.join(LANGUAGES_DIR, dirName, "build/bundle.js");
    if (!existsSync(bundlePath)) {
      log(`SKIP ${dirName}: no bundle at ${bundlePath}`);
      continue;
    }
    log(`Publishing ${dirName}...`);
    try {
      const bundleContent = readFileSync(bundlePath, "utf-8");
      // Use languagePublish mutation
      const metaInput = `{name: "${meta.name}", description: "${meta.description}", possibleTemplateParams: [${meta.possibleTemplateParams.map(p => `"${p}"`).join(",")}]}`;
      
      // Write bundle to a temp file the executor can read
      const tmpBundle = `/tmp/lang-bundle-${dirName}.js`;
      writeFileSync(tmpBundle, bundleContent);
      
      const result = await gql(ws, 
        `mutation { languagePublish(languagePath: "${tmpBundle}", languageMeta: ${metaInput}) { address name author } }`,
        null, 120000);
      
      log(`  Result: ${JSON.stringify(result).substring(0, 300)}`);
      const addr = result?.data?.languagePublish?.address;
      log(`  ${dirName}: ${addr}`);
      hashes[dirName] = addr;
    } catch (e) {
      log(`  FAILED: ${e.message.substring(0, 200)}`);
    }
  }

  log("\nPublished hashes:");
  for (const [k, v] of Object.entries(hashes)) log(`  ${k}: ${v}`);

  // Update bootstrap seed with real hashes
  const seed = JSON.parse(readFileSync(SEED_PATH, "utf-8"));
  if (hashes["agent-expression-store"]) seed.agentLanguage = hashes["agent-expression-store"];
  if (hashes["perspective-diff-sync"]) seed.knownLinkLanguages = [hashes["perspective-diff-sync"]];
  if (hashes["direct-message-language"]) seed.directMessageLanguage = hashes["direct-message-language"];
  if (hashes["perspective-language"]) seed.perspectiveLanguage = hashes["perspective-language"];
  if (hashes["neighbourhood-store"]) seed.neighbourhoodLanguage = hashes["neighbourhood-store"];
  
  // Add languageLanguageSettings with storagePath pointing to the local store
  seed.languageLanguageSettings = {
    storagePath: path.join(DATA_PATH, "ad4m/languages")
  };
  
  // Add trusted agent
  if (did && !seed.trustedAgents.includes(did)) {
    seed.trustedAgents.push(did);
  }

  writeFileSync(OUT_SEED, JSON.stringify(seed, null, 2));
  log(`\nPrepared seed written to: ${OUT_SEED}`);
  log(`Language store at: ${DATA_PATH}/ad4m/languages/`);

  } finally {
  // Cleanup
  if (ws) try { ws.close(); } catch {}
  if (proc) { try { process.kill(proc.pid, "SIGTERM"); } catch {} }
  if (bootstrap) { try { bootstrap.kill("SIGTERM"); } catch {} }
  await sleep(2000);
  if (proc) { try { process.kill(proc.pid, "SIGKILL"); } catch {} }
  if (bootstrap) { try { bootstrap.kill("SIGKILL"); } catch {} }
  
  log("=== DONE ===");
  }
}

main().catch(e => { log(`FATAL: ${e.stack || e}`); process.exit(1); });
