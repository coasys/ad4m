#!/usr/bin/env node
// Operator calls to the staging executor over its WebSocket RPC, with the
// secrets read from files so they never appear in argv, the environment of
// another process, or the output.
//
//   node agent.mjs status     prints one word: unlocked | locked | no-agent
//   node agent.mjs generate   creates the main agent with the unlock passphrase
//
// AD4M_URL                     executor base URL (default http://127.0.0.1:12400);
//                              with a non-empty credential it must be loopback
// AD4M_ADMIN_CREDENTIAL_FILE   admin credential (required)
// AD4M_UNLOCK_PASSPHRASE_FILE  agent passphrase (generate only)
//
// Exit status: 0 on an answer, 1 on an RPC error or timeout, 2 on bad usage.
// Needs Node 22 or later (global WebSocket).
import { readFileSync } from "node:fs";

function secret(variable) {
  const path = process.env[variable];
  if (!path) {
    console.error(`${variable} is not set`);
    process.exit(2);
  }
  // Same rule as the executor: one trailing newline is not part of the secret.
  return readFileSync(path, "utf8").replace(/\r?\n$/, "");
}

function call(type, params, timeoutMs) {
  const base = (process.env.AD4M_URL || "http://127.0.0.1:12400").replace(/^http/, "ws");
  const credential = secret("AD4M_ADMIN_CREDENTIAL_FILE");
  // The RPC takes the token only in the URL, and a proxy logs URLs: send the
  // admin credential to this host's own listener only.
  const host = new URL(base).hostname;
  if (credential && !["127.0.0.1", "localhost", "[::1]"].includes(host)) {
    console.error(`refusing to send the admin credential to ${host}; use 127.0.0.1`);
    process.exit(2);
  }
  const token = encodeURIComponent(credential);
  const ws = new WebSocket(`${base}/api/v1/ws?token=${token}`);
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      ws.close();
      reject(new Error(`${type}: no answer within ${timeoutMs / 1000} s`));
    }, timeoutMs);
    ws.onopen = () => ws.send(JSON.stringify({ id: "1", type, params }));
    ws.onerror = () => {
      clearTimeout(timer);
      reject(new Error(`${type}: cannot connect to ${base}`));
    };
    ws.onmessage = (event) => {
      const message = JSON.parse(event.data);
      if (message.id !== "1") return; // an event, not our answer
      clearTimeout(timer);
      ws.close();
      if (message.error) reject(new Error(`${type}: ${message.error.code} ${message.error.message}`));
      else resolve(message.result);
    };
  });
}

const command = process.argv[2];
try {
  if (command === "status") {
    const status = await call("agent.status", {}, 10_000);
    console.log(!status.isInitialized ? "no-agent" : status.isUnlocked ? "unlocked" : "locked");
  } else if (command === "generate") {
    const passphrase = secret("AD4M_UNLOCK_PASSPHRASE_FILE");
    // Generating starts the Holochain conductor and installs the system languages.
    const agent = await call("agent.generate", { passphrase }, 300_000);
    if (agent.error) throw new Error(`generated ${agent.did}, but: ${agent.error}`);
    console.log(`generated ${agent.did}`);
  } else {
    console.error("usage: node agent.mjs status|generate");
    process.exit(2);
  }
} catch (error) {
  console.error(error.message);
  process.exit(1);
}
// The executor does not always answer the close handshake; do not wait for it.
process.exit(0);
