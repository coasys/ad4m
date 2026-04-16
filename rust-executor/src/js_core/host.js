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
// Storage File I/O -- OPTIONAL EXTENSION (Spec section 7.6)
// ============================================================================
//
// Raw read/write access to a filesystem-like storage layer, scoped to a
// language-chosen path. This is an OPTIONAL extension to the core host
// interface, analogous to Holochain: runtimes are not required to
// implement it, and languages that import it must be prepared for it
// to be unavailable.
//
// Most languages should prefer the KV API (storageGet / storagePut)
// below -- it is part of the core spec and always works. Use the File
// I/O extension only when you genuinely need:
//   * Custom storage layouts (e.g. one file per expression)
//   * Large blobs where the KV's full-rewrite semantics are a problem
//   * Shared paths outside the per-language storage scope
//     (e.g. test fixtures storing language bundles in a shared dir)
//
// When the extension is installed, the runtime sets up
// `LANGUAGE_CONTROLLER.readStorageFile` and `writeStorageFile`. When
// it is not, these exports throw a descriptive error on call.

function fileIoExtension() {
    var lc = langCtrl();
    if (!lc || typeof lc.readStorageFile !== "function"
           || typeof lc.writeStorageFile !== "function") {
        throw new Error(
            "[ad4m:host] Storage File I/O extension is not installed on this " +
            "runtime. readStorageFile/writeStorageFile are an optional extension " +
            "to the host interface; use the KV API (storageGet/storagePut) for " +
            "portable storage, or ensure the runtime installs " +
            "LANGUAGE_CONTROLLER.readStorageFile and writeStorageFile."
        );
    }
    return lc;
}

export function readStorageFile(path) {
    return fileIoExtension().readStorageFile(path);
}

export function writeStorageFile(path, content) {
    fileIoExtension().writeStorageFile(path, content);
}

// ============================================================================
// Storage KV -- CORE (Spec section 7.4)
// ============================================================================
//
// Per-language key-value storage, always available. This is part of the
// core host interface -- every runtime implements it.
//
// Persistence is best-effort: the KV store tries to back the in-memory
// cache with a single JSON file via LANGUAGE_CONTROLLER.readStorageFile /
// writeStorageFile (the underlying methods of the optional File I/O
// extension). If those methods are not installed, or I/O fails, the KV
// degrades to in-memory-only for the isolate and logs a warning. The
// storage API itself never throws -- callers don't have to handle the
// absence of persistence.
//
// Note: because the KV uses the same underlying runtime methods as the
// File I/O extension, a runtime that wants the KV to actually persist
// has to install those methods. The difference is just in the contract:
// the KV *gracefully degrades* when they are missing, while the File I/O
// exports throw a clear error -- because a language importing
// readStorageFile explicitly has opted into filesystem-like semantics.
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

function hasFileIo() {
    var lc = langCtrl();
    return !!lc && typeof lc.readStorageFile === "function"
                && typeof lc.writeStorageFile === "function";
}

function ensureStorageLoaded() {
    if (__storageLoaded) return;
    __storageLoaded = true;
    var path = kvFilePath();
    if (!path || !hasFileIo()) {
        // No File I/O extension installed: the KV is in-memory only for
        // this isolate. Not an error -- the runtime simply didn't opt in.
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
    if (!path || !hasFileIo()) {
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
