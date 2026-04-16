/**
 * # AD4M Language Host Imports (`ad4m:host`)
 *
 * This ES module is the single source of truth for the host functions
 * that all AD4M Languages -- both JS-authored and Rust/WASM -- import.
 * It delegates to three runtime-provided globals:
 *
 *   - AGENT              -- signing, DID, expression creation
 *   - LANGUAGE_CONTROLLER -- language context, events, storage I/O
 *   - __holochainDelegate__ -- Holochain DNA registration and zome calls
 *
 * This file is deliberately plain JavaScript (no TypeScript) so that
 * any runtime (Deno, browser, Node) can load it without a transpiler.
 * The runtime only needs to install the three globals above before
 * languages are loaded. See `docs/host-contract.md` for the full
 * specification of what each global must provide.
 *
 * ## How languages consume this module
 *
 * The executor's StringModuleLoader registers this file under the
 * `ad4m:host` specifier (see `options.rs::language_module_loader`).
 * Language bundles import from it as a standard ES module:
 *
 *     import { agentDid, holochainCall, ... } from "ad4m:host";
 *
 * - Rust/WASM languages: wasm-bindgen emits this import from the
 *   `#[wasm_bindgen(module = "ad4m:host")]` extern block in
 *   `ad4m-ldk/rust/src/imports.rs`.
 * - JS languages: esbuild marks `ad4m:host` as external, so the
 *   bundled output retains the import. The JS ALDK
 *   (`ad4m-ldk/js/src/imports.ts`) re-exports from this module.
 *
 * Both paths resolve to the same functions at runtime.
 *
 * ## Architecture
 *
 * ```
 * Language bundle (JS or WASM)
 *     |  import { agentDid, holochainCall, ... } from "ad4m:host"
 *     v
 * host.js  (this file -- runtime-agnostic)
 *     |  accesses AGENT / LANGUAGE_CONTROLLER / __holochainDelegate__
 *     v
 * Runtime globals (installed by the host before languages load)
 *     |
 *     v
 * Host-specific backend (Deno ops, HTTP API, browser storage, ...)
 * ```
 */

// ============================================================================
// Access runtime-provided globals
// ============================================================================
// Lazy accessors -- the globals are installed by the host runtime before
// any language is loaded, but we defer access to call time to avoid
// module-load-order issues.

var agent = function() { return globalThis.AGENT; };
var langCtrl = function() { return globalThis.LANGUAGE_CONTROLLER; };

// Holochain imports are routed through the per-language delegate
// (`globalThis.__holochainDelegate__`) that the runtime installs
// before invoking a language's init().
function holochainDelegate() {
    var d = globalThis.__holochainDelegate__;
    if (!d) {
        throw new Error(
            "[ad4m:host] __holochainDelegate__ is not installed. " +
            "Holochain imports are only usable after the runtime " +
            "has wired the per-language delegate (i.e. from within init()/post-init)."
        );
    }
    return d;
}

// ============================================================================
// Agent (Spec section 7.1)
// ============================================================================

export function agentDid() { return agent().did(); }
export function agentSigningKeyId() { return agent().signingKeyId(); }
export function agentSign(payload) { return agent().sign(payload); }
export function agentSignStringHex(payload) { return agent().signStringHex(payload); }
export function agentCreateSignedExpression(data) { return agent().createSignedExpression(data); }
export function agentGetAllLocalUserDids() { return agent().getAllLocalUserDIDs(); }
export function agentCreateSignedExpressionForUser(userEmail, data) {
    return agent().createSignedExpressionForUser(userEmail, data);
}
export function agentDidForUser(userEmail) { return agent().didForUser(userEmail); }

// ============================================================================
// Holochain (Spec section 7.2)
// ============================================================================
// These forward to the per-language __holochainDelegate__. The delegate
// is async; the wrappers return Promises so languages should `await` them.

export function holochainRegisterDnas(dnas) {
    return holochainDelegate().registerDNAs(dnas, undefined);
}
export function holochainCall(dnaNick, zome, fnName, params) {
    return holochainDelegate().call(dnaNick, zome, fnName, params);
}
export async function holochainCallAsync(dnaNick, zome, fnName, params) {
    var results = await holochainDelegate().callAsync(
        [{ dnaNick: dnaNick, zomeName: zome, fnName: fnName, params: params }],
        undefined
    );
    return Array.isArray(results) ? results[0] : results;
}

// ============================================================================
// HTTP fetch (Spec section 7.2b)
// ============================================================================
// Wraps the standard fetch() API so WASM languages can call HTTP APIs
// without linking against web_sys. Returns the response body as a string.

async function httpFetchImpl(url, method, headersJson, body) {
    var headers = {};
    if (headersJson && headersJson.length > 0) {
        try {
            var parsed = JSON.parse(headersJson);
            if (parsed && typeof parsed === "object") {
                headers = parsed;
            }
        } catch (_) { /* fall through -- empty headers */ }
    }
    var init = { method: method || "GET", headers: headers };
    if (body && body.length > 0 && init.method !== "GET" && init.method !== "HEAD") {
        init.body = body;
    }
    var res = await globalThis.fetch(url, init);
    var text = await res.text();
    if (!res.ok) {
        throw new Error("http_fetch " + init.method + " " + url + " -> " + res.status + ": " + text);
    }
    return text;
}

export function httpFetch(url, method, headersJson, body) {
    return httpFetchImpl(url, method, headersJson, body);
}

// ============================================================================
// Language context (Spec section 7.3)
// ============================================================================

export function languageStorageDirectory() {
    return langCtrl().languageStorageDirectory();
}

export function languageAddress() {
    return langCtrl().languageAddress();
}

export function languageSettings() {
    return langCtrl().languageSettings();
}

// ============================================================================
// Event emission (Spec section 7.5)
// ============================================================================

function currentLanguageAddress() {
    try { return langCtrl().languageAddress(); } catch (_) { return ""; }
}

export function emitPerspectiveDiff(diff) {
    var lc = langCtrl();
    if (lc) lc.perspectiveDiffReceived(diff, currentLanguageAddress());
}
export function emitSyncStateChange(state) {
    var lc = langCtrl();
    if (lc) lc.syncStateChanged(state, currentLanguageAddress());
}
export function emitTelepresenceSignal(payload, recipientDid) {
    var lc = langCtrl();
    if (lc) lc.telepresenceSignalReceived(payload, currentLanguageAddress(), recipientDid);
}
export function emitSignal(data) {
    var lc = langCtrl();
    if (lc) lc.ad4mSignalEmitted(data, currentLanguageAddress());
}

// ============================================================================
// Storage KV (Spec section 7.4)
// ============================================================================
//
// Per-language key-value storage with an in-memory cache backed by
// persistent storage. The actual I/O is delegated to LANGUAGE_CONTROLLER
// methods (readStorageFile / writeStorageFile) so the host module stays
// runtime-agnostic. The Deno executor implements these via filesystem;
// a browser runtime could use localStorage or IndexedDB.
//
// Design:
//   * Read-through cache: persistent storage is loaded lazily into an
//     in-memory Map on the first storage call and kept consistent on
//     every mutation via a full rewrite.
//   * Writes are flushed synchronously on every put/delete so that a
//     process crash cannot lose already-observed data.
//   * Any I/O failure degrades to in-memory semantics and logs a warning.

var KV_FILENAME = "ad4m-language-kv.json";
var __storage = new Map();
var __storageLoaded = false;
var __storagePersistOk = true;

function kvFilePath() {
    try {
        var dir = langCtrl().languageStorageDirectory();
        if (!dir) return null;
        var sep = dir.endsWith("/") || dir.endsWith("\\") ? "" : "/";
        return dir + sep + KV_FILENAME;
    } catch (_) {
        return null;
    }
}

function ensureStorageLoaded() {
    if (__storageLoaded) return;
    __storageLoaded = true;
    var path = kvFilePath();
    if (!path) {
        __storagePersistOk = false;
        return;
    }
    try {
        var raw = langCtrl().readStorageFile(path);
        var parsed = JSON.parse(raw);
        if (parsed && typeof parsed === "object") {
            var entries = Object.entries(parsed);
            for (var i = 0; i < entries.length; i++) {
                if (typeof entries[i][1] === "string") __storage.set(entries[i][0], entries[i][1]);
            }
        }
    } catch (e) {
        var msg = String(e && e.message ? e.message : e);
        if (msg.indexOf("NotFound") === -1 && msg.indexOf("No such file") === -1) {
            try {
                console.warn(
                    "[ad4m-kv] Failed to load KV file '" + path + "': " + msg + ". " +
                    "Falling back to in-memory storage for this isolate."
                );
            } catch (_) { /* ignore */ }
            __storagePersistOk = false;
        }
    }
}

function flushStorage() {
    if (!__storagePersistOk) return;
    var path = kvFilePath();
    if (!path) {
        __storagePersistOk = false;
        return;
    }
    try {
        var obj = {};
        var keys = Array.from(__storage.keys()).sort();
        for (var i = 0; i < keys.length; i++) obj[keys[i]] = __storage.get(keys[i]);
        langCtrl().writeStorageFile(path, JSON.stringify(obj));
    } catch (e) {
        var msg = String(e && e.message ? e.message : e);
        try {
            console.warn(
                "[ad4m-kv] Failed to persist KV file '" + path + "': " + msg + ". " +
                "Subsequent writes will not be persisted in this isolate."
            );
        } catch (_) { /* ignore */ }
        __storagePersistOk = false;
    }
}

function storageKey(key) {
    var addr = "";
    try { addr = languageAddress(); } catch (_) { addr = "unknown"; }
    return addr + "::" + key;
}
export function storageGet(key) {
    ensureStorageLoaded();
    var v = __storage.get(storageKey(key));
    return v === undefined ? null : v;
}
export function storagePut(key, value) {
    ensureStorageLoaded();
    __storage.set(storageKey(key), value);
    flushStorage();
}
export function storageDelete(key) {
    ensureStorageLoaded();
    __storage.delete(storageKey(key));
    flushStorage();
}
export function storageListKeys(prefix) {
    ensureStorageLoaded();
    var addr;
    try { addr = languageAddress(); } catch (_) { addr = "unknown"; }
    var scopePrefix = addr + "::";
    var fullPrefix = prefix ? scopePrefix + prefix : scopePrefix;
    var out = [];
    for (var k of __storage.keys()) {
        if (k.indexOf(fullPrefix) === 0) out.push(k.substring(scopePrefix.length));
    }
    return out;
}
