// ad4m:host shim for a browser Web Worker (spike S0a).
// The language bundle's `import {...} from "ad4m:host"` is rewritten to this
// module. Every call is logged with a counter and a sync/async mark.
// Configure with `configure({...})` from the worker before calling `init`.

import { ed25519, x25519, edwardsToMontgomeryPriv } from "@noble/curves/ed25519";
import { sha256 } from "@noble/hashes/sha2";
import { hkdf } from "@noble/hashes/hkdf";
import { gcm } from "@noble/ciphers/aes";

const B58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
function base58(bytes) {
    const digits = [0];
    for (const b of bytes) {
        let carry = b;
        for (let j = 0; j < digits.length; j++) {
            carry += digits[j] << 8;
            digits[j] = carry % 58;
            carry = (carry / 58) | 0;
        }
        while (carry > 0) { digits.push(carry % 58); carry = (carry / 58) | 0; }
    }
    let out = "";
    for (let i = 0; i < bytes.length && bytes[i] === 0; i++) out += B58[0];
    for (let i = digits.length - 1; i >= 0; i--) out += B58[digits[i]];
    return out;
}
const hex = (u8) => Array.from(u8, (b) => b.toString(16).padStart(2, "0")).join("");
const unhex = (s) => {
    if (s.length % 2) throw new Error("odd hex");
    const out = new Uint8Array(s.length / 2);
    for (let i = 0; i < out.length; i++) out[i] = parseInt(s.substr(i * 2, 2), 16);
    return out;
};
const utf8 = (s) => new TextEncoder().encode(s);

// ---- state ----
const state = {
    secret: null, // 32-byte Ed25519 seed
    did: "",
    address: "web-s0a-language",
    settings: "{}",
    kv: new Map(),
    onEvent: (kind, payload) => {},
    log: (line) => console.log(line),
    calls: [],
    counter: 0,
    fetchAllow: null, // optional (url) => boolean
};

export function configure(opts) {
    Object.assign(state, opts);
    if (!state.secret) state.secret = ed25519.utils.randomPrivateKey();
    const pub = ed25519.getPublicKey(state.secret);
    const prefixed = new Uint8Array(34);
    prefixed.set([0xed, 0x01]);
    prefixed.set(pub, 2);
    state.did = "did:key:z" + base58(prefixed);
    return { did: state.did };
}
export function shimStats() {
    const byName = {};
    for (const c of state.calls) {
        byName[c.name] ??= { count: 0, mode: c.mode, errors: 0 };
        byName[c.name].count++;
        if (c.error) byName[c.name].errors++;
    }
    return { total: state.counter, byName };
}
export function shimHash(s) { return hashImpl(s); }

function rec(name, mode, args, fn) {
    const n = ++state.counter;
    const short = JSON.stringify(args, (k, v) => (typeof v === "string" && v.length > 80 ? v.slice(0, 80) + "…" : v));
    const entry = { n, name, mode, error: null };
    state.calls.push(entry);
    try {
        const r = fn();
        if (r && typeof r.then === "function") {
            return r.then(
                (v) => { state.log(`[shim #${n}] ${name} (${mode}) ${short} -> ok`); return v; },
                (e) => { entry.error = String(e); state.log(`[shim #${n}] ${name} (${mode}) ${short} -> REJECT ${e}`); throw e; },
            );
        }
        state.log(`[shim #${n}] ${name} (${mode}) ${short}`);
        return r;
    } catch (e) {
        entry.error = String(e);
        state.log(`[shim #${n}] ${name} (${mode}) ${short} -> THROW ${e}`);
        throw e;
    }
}
const notOffered = (name) => (...args) => rec(name, "n/a", ["<args>"], () => { throw new Error(`${name}: not offered by the web host (S0a shim)`); });

// ---- Agent (sync) ----
export function agentDid() { return rec("agentDid", "sync", [], () => state.did); }
export function agentSigningKeyId() { return rec("agentSigningKeyId", "sync", [], () => state.did + "#" + state.did.slice(8)); }
export function agentSign(payload) { return rec("agentSign", "sync", ["<bytes>"], () => ed25519.sign(payload, state.secret)); }
export function agentSignStringHex(payload) {
    // Same as the executor: Ed25519 over SHA-256(utf8(payload)).
    return rec("agentSignStringHex", "sync", [payload], () => hex(ed25519.sign(sha256(utf8(payload)), state.secret)));
}
export function agentOpenSealed(sealed) {
    return rec("agentOpenSealed", "sync", ["<box>"], () => {
        const xSecret = edwardsToMontgomeryPriv(state.secret);
        const xPub = x25519.getPublicKey(xSecret);
        const eph = unhex(sealed.ephemeralPublicKey);
        const shared = x25519.getSharedSecret(xSecret, eph);
        const salt = new Uint8Array(64); salt.set(eph); salt.set(xPub, 32);
        const key = hkdf(sha256, shared, salt, utf8("ad4m:sealed-box:v1"), 32);
        return hex(gcm(key, unhex(sealed.nonce)).decrypt(unhex(sealed.ciphertext)));
    });
}
export function agentCreateSignedExpression(data) {
    return rec("agentCreateSignedExpression", "sync", ["<data>"], () => {
        const timestamp = new Date().toISOString();
        // Executor rule (signatures.rs hash_data_and_timestamp): SHA-256(serde_json(data) || RFC3339 millis Z).
        // JSON.stringify matches serde only if the caller's key order is the Rust field order.
        const signature = hex(ed25519.sign(sha256(utf8(JSON.stringify(data) + timestamp)), state.secret));
        return { author: state.did, timestamp, data, proof: { key: agentSigningKeyId(), signature } };
    });
}
export function agentGetAllLocalUserDids() { return rec("agentGetAllLocalUserDids", "sync", [], () => []); }
export const agentCreateSignedExpressionForUser = notOffered("agentCreateSignedExpressionForUser");
export const agentDidForUser = notOffered("agentDidForUser");

// ---- Holochain: not offered ----
export const holochainRegisterDnas = notOffered("holochainRegisterDnas");
export const holochainCall = notOffered("holochainCall");
export const holochainCallAsync = notOffered("holochainCallAsync");

// ---- HTTP (async) ----
export function httpFetch(url, method, headersJson, body, maxBytes) {
    return rec("httpFetch", "async", [method, url], async () => {
        if (state.fetchAllow && !state.fetchAllow(url)) throw new Error(`httpFetch ${url} refused by shim allow-list`);
        let headers = {};
        try { headers = headersJson ? JSON.parse(headersJson) : {}; } catch {}
        const init = { method: method || "GET", headers, signal: AbortSignal.timeout(10_000) };
        if (body && init.method !== "GET" && init.method !== "HEAD") init.body = body;
        const res = await fetch(url, init);
        const text = await res.text();
        if (maxBytes !== undefined && text.length > maxBytes) { const e = new Error("too-large"); e.code = "too-large"; throw e; }
        return { status: res.status, body: text };
    });
}

// ---- hash (sync; Web Crypto digest is async, so noble sha256) ----
function hashImpl(data) {
    const digest = sha256(utf8(data));
    const cid = new Uint8Array(36);
    cid.set([0x01, 0x00, 0x12, 0x20]);
    cid.set(digest, 4);
    return "Qmz" + base58(cid);
}
export function hash(data) { return rec("hash", "sync", [data], () => hashImpl(data)); }

// ---- Language context ----
export function languageStorageDirectory() { return rec("languageStorageDirectory", "sync", [], () => "/opfs-not-used/" + state.address); }
export function languageAddress() { return rec("languageAddress", "sync", [], () => state.address); }
export function languageSettings() { return rec("languageSettings", "sync", [], () => state.settings); }

// ---- Events (sync, fire-and-forget) ----
export function emitPerspectiveDiff(diff) { return rec("emitPerspectiveDiff", "sync", ["<diff>"], () => state.onEvent("perspectiveDiff", diff)); }
export function emitSyncStateChange(s) { return rec("emitSyncStateChange", "sync", [s], () => state.onEvent("syncState", s)); }
export function emitTelepresenceSignal(payload, recipientDid) { return rec("emitTelepresenceSignal", "sync", ["<payload>", recipientDid], () => state.onEvent("telepresence", { payload, recipientDid })); }
export function emitSignal(data) { return rec("emitSignal", "sync", ["<data>"], () => state.onEvent("signal", data)); }

// ---- KV (sync, in memory) ----
export function storageGet(key) { return rec("storageGet", "sync", [key], () => (state.kv.has(key) ? state.kv.get(key) : null)); }
export function storagePut(key, value) { return rec("storagePut", "sync", [key, `<${String(value).length} chars>`], () => { state.kv.set(key, value); }); }
export function storageDelete(key) { return rec("storageDelete", "sync", [key], () => { state.kv.delete(key); }); }
export function storageListKeys(prefix) { return rec("storageListKeys", "sync", [prefix], () => [...state.kv.keys()].filter((k) => !prefix || k.startsWith(prefix))); }

// ---- File IO: optional extension, not offered ----
export const readStorageFile = notOffered("readStorageFile");
export const writeStorageFile = notOffered("writeStorageFile");

// For the worker (the "host core"), not the language: sign a link like the executor. Not logged.
export function hostSignLink(link) {
    const data = { predicate: link.predicate ?? null, source: link.source, target: link.target };
    const timestamp = new Date().toISOString();
    const signature = hex(ed25519.sign(sha256(utf8(JSON.stringify(data) + timestamp)), state.secret));
    return { author: state.did, timestamp, data, proof: { key: state.did + "#" + state.did.slice(8), signature } };
}
