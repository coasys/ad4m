#!/usr/bin/env node
/**
 * Smoke test for the Rust ALDK + WASM Language end-to-end pipeline.
 *
 * Validates against `tests/rust-languages/test-wasm-language/build/bundle.js`
 * (produced by `tests/rust-languages/build.sh`):
 *
 *   1. The bundle loads as a self-contained ES module (inlined wasm).
 *   2. The runtime-imports surface (agentDid, storagePut, emitSignal,
 *      languageAddress, …) is satisfied by globalThis stubs.
 *   3. Lifecycle: name / version / isPublic / init / teardown.
 *   4. **Capability-presence detection** — the test language declares only
 *      `expression` + `perspective_query`, so the macro must NOT emit
 *      perspectiveCommit / perspectiveSyncSync / peersRemote / telepresence*.
 *   5. Expression flow exercises agent_create_signed_expression +
 *      storage_put.
 *   6. Query flow accepts the supported kind and rejects unsupported kinds
 *      with a LanguageError(InvalidInput).
 *
 * Run after `bash tests/rust-languages/build.sh`:
 *   node tests/rust-languages/smoke-test.mjs
 */
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { existsSync } from "node:fs";
import { strict as assert } from "node:assert";

const __dirname = dirname(fileURLToPath(import.meta.url));
const bundlePath = join(__dirname, "test-wasm-language/build/bundle.js");

if (!existsSync(bundlePath)) {
    console.error(`bundle not found at ${bundlePath}`);
    console.error("run: bash tests/rust-languages/build.sh");
    process.exit(1);
}

// ---------- runtime-import stubs (spec §7) ----------
const store = new Map();
const recorded = [];

globalThis.languageStorageDirectory = () => "/tmp/storage";
globalThis.languageAddress = () => "test-addr";
globalThis.languageSettings = () => "{}";
globalThis.agentDid = () => "did:test:smoke";
globalThis.agentSigningKeyId = () => "test-key";
globalThis.agentCreateSignedExpression = (data) => ({
    author: "did:test:smoke",
    timestamp: new Date().toISOString(),
    data,
    proof: { signature: "sig", key: "key" },
});
globalThis.agentSignStringHex = (s) => "deadbeef".repeat(8);
globalThis.agentSign = () => new Uint8Array(64);
globalThis.storageGet = (k) => { const v = store.get(k); recorded.push(["get", k]); return v ?? null; };
globalThis.storagePut = (k, v) => { store.set(k, v); recorded.push(["put", k]); };
globalThis.storageDelete = (k) => store.delete(k);
globalThis.storageListKeys = () => [];
globalThis.emitSignal = (d) => recorded.push(["signal", d]);
globalThis.emitPerspectiveDiff = () => {};
globalThis.emitSyncStateChange = () => {};
globalThis.emitTelepresenceSignal = () => {};
globalThis.holochainCall = () => null;
globalThis.holochainRegisterDnas = () => null;

// ---------- exercise the bundle ----------
const mod = await import(bundlePath);

assert.equal(mod.name(), "test-wasm-language");
assert.equal(mod.version(), "0.1.0");
assert.equal(mod.isPublic(), true);

mod.init();

// Capability-presence detection: macro emits ONLY for declared capabilities.
assert.equal(typeof mod.expressionCreate, "function", "expression declared");
assert.equal(typeof mod.expressionGet, "function", "expression declared");
assert.equal(typeof mod.perspectiveQueryRun, "function", "query declared");
assert.equal(typeof mod.perspectiveQuerySupportedKinds, "function", "query declared");
assert.equal(mod.perspectiveCommit, undefined, "perspective_commit NOT declared");
assert.equal(mod.perspectiveSyncSync, undefined, "perspective_sync NOT declared");
assert.equal(mod.peersRemote, undefined, "peers NOT declared");
assert.equal(mod.telepresenceSendSignal, undefined, "telepresence NOT declared");

// Query capability
assert.deepEqual(mod.perspectiveQuerySupportedKinds(), ["test.echo"]);
const echoed = mod.perspectiveQueryRun({ kind: "test.echo", params: { foo: 1 } });
assert.ok(echoed && typeof echoed === "object", "query returns object");

let threw = false;
try { mod.perspectiveQueryRun({ kind: "bogus", params: {} }); } catch { threw = true; }
assert.equal(threw, true, "unsupported kind throws");

// Expression capability — round-trip via storage
const addr = mod.expressionCreate({ note: "hello" });
assert.ok(typeof addr === "string" && addr.startsWith("test:"));
const got = mod.expressionGet(addr);
assert.ok(got && got.author === "did:test:smoke", "fetched expression has author");

// Host calls observed: agent_create_signed_expression + agent_sign_string_hex
// + storage_put + storage_get + emit_signal (init log + create log)
assert.ok(recorded.some(([k]) => k === "put"), "storage_put was called");
assert.ok(recorded.some(([k]) => k === "get"), "storage_get was called");
assert.ok(recorded.some(([k]) => k === "signal"), "emit_signal was called");

mod.teardown();

console.log("OK — Rust ALDK end-to-end pipeline validated");
console.log(`     ${recorded.length} host imports invoked across the run`);
