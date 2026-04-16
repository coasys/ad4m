/**
 * # AD4M Language Host Imports (`ad4m:host.ts`)
 *
 * This ES module is the single source of truth for the host functions
 * that all AD4M Languages -- both JS-authored and Rust/WASM -- import.
 * It bridges the executor's Deno extension ops (AGENT,
 * LANGUAGE_CONTROLLER, __holochainDelegate__) into the canonical
 * camelCase API surface defined by Spec section 7.
 *
 * ## How languages consume this module
 *
 * The executor's StringModuleLoader registers this file under the
 * `ad4m:host.ts` specifier (see `options.rs::language_module_loader`).
 * Language bundles import from it as a standard ES module:
 *
 *     import { agentDid, holochainCall, ... } from "ad4m:host.ts";
 *
 * - **Rust/WASM languages**: wasm-bindgen emits this import from the
 *   `#[wasm_bindgen(module = "ad4m:host.ts")]` extern block in
 *   `ad4m-ldk/rust/src/imports.rs`.
 * - **JS languages**: esbuild marks `ad4m:host.ts` as external, so the
 *   bundled output retains the import. The JS ALDK
 *   (`ad4m-ldk/js/src/imports.ts`) re-exports from this module.
 *
 * Both paths resolve to the same functions at runtime.  No globalThis
 * mirroring, no setup/teardown lifecycle.
 *
 * ## Architecture
 *
 * ```
 * Language bundle (JS or WASM)
 *     |  import { agentDid, holochainCall, ... } from "ad4m:host.ts"
 *     v
 * host.ts  (this file)
 *     |  accesses AGENT / LANGUAGE_CONTROLLER / __holochainDelegate__
 *     v
 * Deno extension globals (agent_extension.js, languages_extension.js)
 *     |
 *     v
 * deno_core ops -> rust-executor services
 * ```
 */

// ============================================================================
// Access Deno extension ops via globalThis globals
// ============================================================================
// The ops are registered by the deno_core extension system and exposed on
// globalThis via extension JS files (agent_extension.js, languages_extension.js).
// This file is loaded as a user-space module (not an ext: module) so it CANNOT
// import from 'ext:core/ops' directly. Instead, we access the ops through the
// global objects that the extension JS files install.

// Lazy accessors -- the globals (AGENT, LANGUAGE_CONTROLLER) are installed by
// extension modules before any language is loaded, but we defer access to call
// time to avoid module-load-order issues.
const agent = (): any => (globalThis as any).AGENT;
const langCtrl = (): any => (globalThis as any).LANGUAGE_CONTROLLER;

// Holochain imports are routed through the per-language JS delegate
// (`globalThis.__holochainDelegate__`) that `language_bootstrap.js`
// installs before invoking a language's init(). The delegate wraps
// the existing holochain_service_extension ops (install_app, call_zome_function)
// and also maintains the cell_id -> languageAddress mapping used by the
// central signal router.
function holochainDelegate(): any {
    const d = (globalThis as any).__holochainDelegate__;
    if (!d) {
        throw new Error(
            "[ad4m:host] __holochainDelegate__ is not installed. " +
            "Holochain imports are only usable after language_bootstrap.js " +
            "has wired the per-language delegate (i.e. from within init()/post-init)."
        );
    }
    return d;
}

// ============================================================================
// Agent (Spec section 7.1)
// ============================================================================

export function agentDid(): string { return agent().did() as string; }
export function agentSigningKeyId(): string { return agent().signingKeyId() as string; }
export function agentSign(payload: Uint8Array): Uint8Array { return agent().sign(payload) as Uint8Array; }
export function agentSignStringHex(payload: string): string { return agent().signStringHex(payload) as string; }
export function agentCreateSignedExpression(data: unknown): object { return agent().createSignedExpression(data) as object; }
export function agentGetAllLocalUserDids(): string[] { return agent().getAllLocalUserDIDs() as string[]; }
export function agentCreateSignedExpressionForUser(userEmail: string, data: unknown): object {
    return agent().createSignedExpressionForUser(userEmail, data) as object;
}
export function agentDidForUser(userEmail: string): string { return agent().didForUser(userEmail) as string; }

// ============================================================================
// Holochain (Spec section 7.2)
// ============================================================================
// These forward to the per-language __holochainDelegate__ installed by
// language_bootstrap.js. The delegate is async; the wrappers return
// Promises so languages should `await` them.

export function holochainRegisterDnas(dnas: object[]): Promise<object[]> {
    return holochainDelegate().registerDNAs(dnas, /*signalCallback*/ undefined);
}
export function holochainCall(dnaNick: string, zome: string, fnName: string, params: unknown): Promise<unknown> {
    return holochainDelegate().call(dnaNick, zome, fnName, params);
}
export async function holochainCallAsync(
    dnaNick: string,
    zome: string,
    fnName: string,
    params: unknown
): Promise<unknown> {
    const results = await holochainDelegate().callAsync(
        [{ dnaNick, zomeName: zome, fnName, params }],
        undefined
    );
    return Array.isArray(results) ? results[0] : results;
}

// ============================================================================
// HTTP fetch (Spec section 7.2b)
// ============================================================================
// Wraps Deno's native fetch() so WASM languages can call HTTP APIs
// without linking against web_sys. Returns the response body as a
// string; headers are passed as a JSON object, body as a string.

async function httpFetchImpl(
    url: string,
    method: string,
    headersJson: string,
    body: string,
): Promise<string> {
    let headers: Record<string, string> = {};
    if (headersJson && headersJson.length > 0) {
        try {
            const parsed = JSON.parse(headersJson);
            if (parsed && typeof parsed === "object") {
                headers = parsed as Record<string, string>;
            }
        } catch (_) { /* fall through -- empty headers */ }
    }
    const init: RequestInit = { method: method || "GET", headers };
    if (body && body.length > 0 && init.method !== "GET" && init.method !== "HEAD") {
        init.body = body;
    }
    const res = await (globalThis as any).fetch(url, init);
    const text = await res.text();
    if (!res.ok) {
        throw new Error(`http_fetch ${init.method} ${url} -> ${res.status}: ${text}`);
    }
    return text;
}

export function httpFetch(
    url: string,
    method: string,
    headersJson: string,
    body: string,
): Promise<string> {
    return httpFetchImpl(url, method, headersJson, body);
}

// ============================================================================
// Language context (Spec section 7.3)
// ============================================================================

export function languageStorageDirectory(): string {
    return langCtrl().languageStorageDirectory() as string;
}

export function languageAddress(): string {
    return langCtrl().languageAddress() as string;
}

export function languageSettings(): string {
    return langCtrl().languageSettings() as string;
}

// ============================================================================
// Event emission (Spec section 7.5)
// ============================================================================
// emit* calls are routed to the LANGUAGE_CONTROLLER global, which dispatches
// to the runtime's perspective/sync/telepresence fan-out paths.

function currentLanguageAddress(): string {
    try { return langCtrl().languageAddress() as string; } catch { return ""; }
}

export function emitPerspectiveDiff(diff: unknown): void {
    const lc = langCtrl();
    if (lc) lc.perspectiveDiffReceived(diff, currentLanguageAddress());
}
export function emitSyncStateChange(state: unknown): void {
    const lc = langCtrl();
    if (lc) lc.syncStateChanged(state, currentLanguageAddress());
}
export function emitTelepresenceSignal(payload: unknown, recipientDid?: string): void {
    const lc = langCtrl();
    if (lc) lc.telepresenceSignalReceived(payload, currentLanguageAddress(), recipientDid);
}
export function emitSignal(data: unknown): void {
    const lc = langCtrl();
    if (lc) lc.ad4mSignalEmitted(data, currentLanguageAddress());
}

// ============================================================================
// Storage KV (Spec section 7.4)
// ============================================================================
//
// File-backed per-language KV. Each language isolate has its own
// `language_storage_directory()` and Deno's FS permission list grants
// the isolate read+write access to exactly that directory.
//
// Design:
//   * Read-through cache: the on-disk file is loaded lazily into an
//     in-memory Map on the first storage call and kept consistent on
//     every mutation via a full rewrite.
//   * Writes are flushed synchronously on every put/delete so that a
//     process crash cannot lose already-observed data.
//   * Any FS failure degrades to in-memory semantics and logs a warning.

const KV_FILENAME = "ad4m-language-kv.json";
const __storage = new Map<string, string>();
let __storageLoaded = false;
let __storagePersistOk = true;

function kvFilePath(): string | null {
    try {
        const dir = langCtrl().languageStorageDirectory() as string;
        if (!dir) return null;
        const sep = dir.endsWith("/") || dir.endsWith("\\") ? "" : "/";
        return `${dir}${sep}${KV_FILENAME}`;
    } catch (_) {
        return null;
    }
}

function ensureStorageLoaded(): void {
    if (__storageLoaded) return;
    __storageLoaded = true;
    const path = kvFilePath();
    if (!path) {
        __storagePersistOk = false;
        return;
    }
    try {
        // @ts-ignore -- Deno is provided by the per-language worker
        const raw: string = (globalThis as any).Deno.readTextFileSync(path);
        const parsed = JSON.parse(raw);
        if (parsed && typeof parsed === "object") {
            for (const [k, v] of Object.entries(parsed)) {
                if (typeof v === "string") __storage.set(k, v);
            }
        }
    } catch (e: any) {
        const msg = String(e && e.message ? e.message : e);
        if (!msg.includes("NotFound") && !msg.includes("No such file")) {
            try {
                console.warn(
                    `[ad4m-kv] Failed to load KV file '${path}': ${msg}. ` +
                    `Falling back to in-memory storage for this isolate.`
                );
            } catch (_) { /* ignore */ }
            __storagePersistOk = false;
        }
    }
}

function flushStorage(): void {
    if (!__storagePersistOk) return;
    const path = kvFilePath();
    if (!path) {
        __storagePersistOk = false;
        return;
    }
    try {
        const obj: Record<string, string> = {};
        const keys = Array.from(__storage.keys()).sort();
        for (const k of keys) obj[k] = __storage.get(k) as string;
        // @ts-ignore -- Deno is provided by the per-language worker
        (globalThis as any).Deno.writeTextFileSync(path, JSON.stringify(obj));
    } catch (e: any) {
        const msg = String(e && e.message ? e.message : e);
        try {
            console.warn(
                `[ad4m-kv] Failed to persist KV file '${path}': ${msg}. ` +
                `Subsequent writes will not be persisted in this isolate.`
            );
        } catch (_) { /* ignore */ }
        __storagePersistOk = false;
    }
}

function storageKey(key: string): string {
    let addr = "";
    try { addr = languageAddress(); } catch (_) { addr = "unknown"; }
    return `${addr}::${key}`;
}
export function storageGet(key: string): string | null {
    ensureStorageLoaded();
    const v = __storage.get(storageKey(key));
    return v === undefined ? null : v;
}
export function storagePut(key: string, value: string): void {
    ensureStorageLoaded();
    __storage.set(storageKey(key), value);
    flushStorage();
}
export function storageDelete(key: string): void {
    ensureStorageLoaded();
    __storage.delete(storageKey(key));
    flushStorage();
}
export function storageListKeys(prefix?: string): string[] {
    ensureStorageLoaded();
    const addr = (() => { try { return languageAddress(); } catch { return "unknown"; } })();
    const scopePrefix = `${addr}::`;
    const fullPrefix = prefix ? `${scopePrefix}${prefix}` : scopePrefix;
    const out: string[] = [];
    for (const k of __storage.keys()) {
        if (k.startsWith(fullPrefix)) out.push(k.substring(scopePrefix.length));
    }
    return out;
}
