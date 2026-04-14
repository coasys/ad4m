/**
 * # WASM Host Imports (JS/Deno side)
 *
 * This file defines the host imports that Rust-compiled Language WASM
 * modules link against. Rust languages built with `ad4m-ldk` use
 * `#[wasm_bindgen] extern "C"` to declare imports such as `__agent_did`,
 * `__holochain_call`, `__signal_emit`, etc. — wasm-bindgen then expects
 * those symbols to exist on the JS host at instantiation time. We
 * install them on `globalThis` here, wrapping the matching Deno ops
 * (AGENT, LANGUAGE_CONTROLLER, __holochainDelegate__).
 *
 * JS-authored languages do NOT link against any of this. They reach the
 * same underlying ops through the JS ALDK wrappers in
 * `ad4m-ldk/js/src/imports.ts` (or directly via the `AGENT` /
 * `LANGUAGE_CONTROLLER` / `__holochainDelegate__` globals if they're
 * hand-rolled) — but the resulting function calls all end up on the
 * same Deno ops.
 *
 * ## Rust-side declaration example
 *
 * ```rust
 * #[wasm_bindgen]
 * extern "C" {
 *     fn __agent_did() -> String;
 *     fn __agent_sign(payload: &[u8]) -> Vec<u8>;
 *     fn __holochain_call(dna: &str, zome: &str, fn_name: &str, params: JsValue) -> JsValue;
 *     fn __signal_emit(data: JsValue);
 * }
 * ```
 *
 * ## Architecture
 *
 * ```
 * WASM Language (compiled from Rust via ad4m-ldk)
 *     │
 *     │ calls __agent_did(), __signal_emit(), etc.
 *     ▼
 * wasm_imports.ts (this file — globalThis functions)
 *     │
 *     │ accesses AGENT / LANGUAGE_CONTROLLER / __holochainDelegate__
 *     ▼
 * Deno extension JS globals (agent_extension.js, languages_extension.js)
 * ```
 *
 * ## Bootstrap integration
 *
 * `language_bootstrap.js` calls `setupWasmImports()` before each
 * language's `init()` and `teardownWasmImports()` after teardown.
 * Both are refcounted so multiple languages sharing an isolate install
 * the globals only once.
 */

// ============================================================================
// Access Deno extension ops via globalThis globals
// ============================================================================
// The ops are registered by the deno_core extension system and exposed on
// globalThis via extension JS files (agent_extension.js, languages_extension.js).
// This file is loaded as a user-space module (not an ext: module) so it CANNOT
// import from 'ext:core/ops' directly. Instead, we access the ops through the
// global objects that the extension JS files install.

// Lazy accessors — the globals (AGENT, LANGUAGE_CONTROLLER) are installed by
// extension modules before any language is loaded, but we defer access to call
// time to avoid module-load-order issues.
const agent = (): any => (globalThis as any).AGENT;
const langCtrl = (): any => (globalThis as any).LANGUAGE_CONTROLLER;

// Holochain imports are routed through the per-language JS delegate
// (`globalThis.__holochainDelegate__`) that `language_bootstrap.js`
// installs before invoking a flat language's init(). The delegate wraps
// the existing holochain_service_extension ops (install_app, call_zome_function)
// and also maintains the cell_id → languageAddress mapping used by the
// central signal router. There is no direct `holochain_register_dnas` op
// so there is no corresponding global to delegate to — use __holochainDelegate__ directly.
function holochainDelegate(): any {
    const d = (globalThis as any).__holochainDelegate__;
    if (!d) {
        throw new Error(
            "[flat-imports] __holochainDelegate__ is not installed. " +
            "Holochain imports are only usable after language_bootstrap.js " +
            "has wired the per-language delegate (i.e. from within init()/post-init)."
        );
    }
    return d;
}

// ============================================================================
// Agent Imports — bridge to agent_extension.rs ops
// ============================================================================

/**
 * Returns the current agent's DID.
 * Rust: `agent_extension::agent_did()`
 */
export function __agent_did(): string {
    return agent().did() as string;
}

/**
 * Returns the signing key ID for the current agent.
 * Rust: `agent_extension::agent_signing_key_id()`
 */
export function __agent_signing_key_id(): string {
    return agent().signingKeyId() as string;
}

/**
 * Signs arbitrary bytes with the current agent's signing key.
 * Rust: `agent_extension::agent_sign()`
 */
export function __agent_sign(payload: Uint8Array): Uint8Array {
    const result = agent().sign(payload) as Uint8Array;
    return result;
}

/**
 * Signs a hex string with the current agent's signing key.
 * Rust: `agent_extension::agent_sign_string_hex()`
 */
export function __agent_sign_string_hex(payload: string): string {
    return agent().signStringHex(payload) as string;
}

/**
 * Creates a signed expression with the given data using the current agent.
 * Rust: `agent_extension::agent_create_signed_expression()`
 */
export function __agent_create_signed_expression(data: unknown): object {
    return agent().createSignedExpression(data) as object;
}

/**
 * Gets all local user DIDs (main agent + managed users).
 * Rust: `agent_extension::agent_get_all_local_user_dids()`
 */
export function __agent_get_all_local_user_dids(): string[] {
    return agent().getAllLocalUserDIDs() as string[];
}

/**
 * Creates a signed expression for a specific user (by email).
 * Rust: `agent_extension::agent_create_signed_expression_for_user()`
 */
export function __agent_create_signed_expression_for_user(userEmail: string, data: unknown): object {
    return agent().createSignedExpressionForUser(userEmail, data) as object;
}

/**
 * Gets the DID for a specific user (by email).
 * Rust: `agent_extension::agent_did_for_user()`
 */
export function __agent_did_for_user(userEmail: string): string {
    return agent().didForUser(userEmail) as string;
}

// ============================================================================
// Holochain Imports — bridge to the per-language __holochainDelegate__
// ============================================================================
// The flat language calls these from init(); they forward to the JS
// delegate installed by language_bootstrap.js, which in turn reaches
// the Rust-side holochain_service_extension ops. Signals are routed
// via the language's exported handleHolochainSignal (bridged into
// globalThis.__handleHolochainSignal__ in language_bootstrap.js) —
// there is no per-call signalCallback argument in the flat API.

/**
 * Registers one or more DNAs with the Holochain conductor. Returns the
 * resulting AppInfo list. Side effect: registers cell_id → lang_addr
 * in the central signal router so the language's handleHolochainSignal
 * export can receive signals.
 */
export function __holochain_register_dnas(dnas: object[]): Promise<object[]> {
    return holochainDelegate().registerDNAs(dnas, /*signalCallback*/ undefined);
}

/**
 * Synchronous (awaitable) call to a zome function.
 */
export function __holochain_call(
    dnaNick: string,
    zome: string,
    fnName: string,
    params: unknown
): Promise<unknown> {
    return holochainDelegate().call(dnaNick, zome, fnName, params);
}

/**
 * Async call to a zome function. Single-call form matching the
 * Rust/JS ALDK typings — internally wraps into the delegate's batch
 * API and unwraps the single result.
 */
export async function __holochain_call_async(
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
// Signal Imports — emits to the AD4M signal bus
// ============================================================================

/**
 * Emits a signal to the AD4M signal bus.
 * Rust: `languages_extension::ad4m_signal_emitted(signal, language_address)`.
 *
 * The op REQUIRES the language address as the second arg — without it the
 * Deno op call fails with an arg-count mismatch and the calling Language
 * crashes. Resolve the address from the per-isolate thread-local set up
 * via `setupWasmImports()`; fall back to "" if no isolate state has
 * been initialized yet (early init logging).
 */
export function __signal_emit(data: unknown): void {
    let addr = "";
    try { addr = langCtrl().languageAddress() as string; } catch (_) { addr = ""; }
    langCtrl().ad4mSignalEmitted(data, addr);
}

// ============================================================================
// Language context imports — set by runtime before calling init()
// ============================================================================

/**
 * Returns the storage directory for this language instance.
 * Rust: `languages_extension.rs::language_storage_directory()`
 */
export function __language_storage_directory(): string {
    return langCtrl().languageStorageDirectory() as string;
}

/**
 * Returns the address (DID) of this language instance.
 * Rust: `js_core/mod.rs op_language_address()`
 */
export function __language_address(): string {
    return langCtrl().languageAddress() as string;
}

/**
 * Returns the settings JSON for this language instance.
 * Rust: `languages_extension.rs::language_settings()`
 */
export function __language_settings(): string {
    return langCtrl().languageSettings() as string;
}

// ============================================================================
// Language context imports — camelCase versions for languages to call
// Per spec: languageStorageDirectory(), languageAddress(), languageSettings()
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
// Canonical camelCase surface (spec §7)
// ============================================================================
// Per the new spec, languages call canonical camelCase imports — no `__`
// prefix. The Deno op bindings keep their snake_case internals; only the
// JS surface is renamed. Both `__foo_bar` and `fooBar` wrappers are installed
// on globalThis for the duration of a flat-language init so legacy in-tree
// callers keep working alongside migrated languages.

// ----- Agent -----
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

// ----- Holochain -----
// Spec §7.2 — these forward to the per-language __holochainDelegate__
// installed by language_bootstrap.js. The delegate is async; the wrappers
// return Promises so flat languages should `await` them.
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

// ----- Event emission (spec §7.5) -----
// emit* calls are routed to the LANGUAGE_CONTROLLER global, which dispatches
// to the runtime's perspective/sync/telepresence fan-out paths. The
// languageAddress() of the calling Language is derived from the thread-local
// IsolateState set up in setupWasmImports().
function currentLanguageAddress(): string {
    try { return langCtrl().languageAddress() as string; } catch { return ""; }
}
function languageController(): any {
    return (globalThis as any).LANGUAGE_CONTROLLER;
}
export function emitPerspectiveDiff(diff: unknown): void {
    const lc = languageController();
    if (lc) lc.perspectiveDiffReceived(diff, currentLanguageAddress());
}
export function emitSyncStateChange(state: unknown): void {
    const lc = languageController();
    if (lc) lc.syncStateChanged(state, currentLanguageAddress());
}
export function emitTelepresenceSignal(payload: unknown, recipientDid?: string): void {
    const lc = languageController();
    if (lc) lc.telepresenceSignalReceived(payload, currentLanguageAddress(), recipientDid);
}
export function emitSignal(data: unknown): void {
    const lc = languageController();
    if (lc) lc.ad4mSignalEmitted(data, currentLanguageAddress());
}

// ----- Storage key/value (spec §7.4) -----
//
// File-backed per-language KV. Each language isolate has its own
// `language_storage_directory()` (see js_core/mod.rs
// new_for_language) and Deno's FS permission list grants the isolate
// read+write access to exactly that directory, so we can persist the
// KV as a single JSON file there without leaking into any other
// language's scope.
//
// Design:
//   * Read-through cache: the on-disk file is loaded lazily into an
//     in-memory Map on the first storage call and kept consistent on
//     every mutation via a full rewrite. The cache is strictly local
//     to the isolate, so there is exactly one writer.
//   * Writes are flushed synchronously on every put/delete so that a
//     process crash between mutation and the next call cannot lose
//     already-observed data. Languages that need a high-write
//     workload should batch externally.
//   * Any FS failure (directory missing, permission denied, disk
//     full) degrades to in-memory semantics and logs a warning. The
//     KV still satisfies read-your-writes *within the process*, which
//     is the spec §7.4 minimum. Tests that rely on the in-memory
//     fallback (e.g. unit-test isolates with no real storage dir)
//     keep working unchanged.
//
// The on-disk layout is deliberately flat: `{ key: value }` with
// string keys and string values. No key-prefix namespacing is needed
// because each language isolate has its own file.
const KV_FILENAME = "ad4m-language-kv.json";
const __storage = new Map<string, string>();
let __storageLoaded = false;
let __storagePersistOk = true;

function kvFilePath(): string | null {
    try {
        const dir = langCtrl().languageStorageDirectory() as string;
        if (!dir) return null;
        // Normalize a trailing slash so the join is platform-agnostic
        // without pulling in node:path (not guaranteed to be available
        // inside the per-language Deno worker).
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
        // No storage directory wired yet (e.g. unit-test isolate that
        // never called set_language_context). Fall through to the
        // in-memory Map; flag persistence as disabled so subsequent
        // mutations don't keep trying and logging.
        __storagePersistOk = false;
        return;
    }
    try {
        // @ts-ignore — Deno is provided by the per-language worker
        const raw: string = (globalThis as any).Deno.readTextFileSync(path);
        const parsed = JSON.parse(raw);
        if (parsed && typeof parsed === "object") {
            for (const [k, v] of Object.entries(parsed)) {
                if (typeof v === "string") __storage.set(k, v);
            }
        }
    } catch (e: any) {
        // Missing file is the normal first-run case; silently treat
        // as empty. Any other error is worth surfacing once so an
        // author or operator can diagnose permissions issues.
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
        // Serialize with sorted keys so the file is byte-stable across
        // runs with the same data — useful for debugging and for any
        // consumer that cares about content hashing of the KV file.
        const obj: Record<string, string> = {};
        const keys = Array.from(__storage.keys()).sort();
        for (const k of keys) obj[k] = __storage.get(k) as string;
        // @ts-ignore — Deno is provided by the per-language worker
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

// Namespace keys by language address so that a single isolate hosting
// more than one language (hypothetical future refactor; today it's 1:1)
// cannot cross-contaminate KV entries. The file-backed implementation
// already scopes by language_storage_directory(), but the prefix keeps
// the in-memory fallback safe too.
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

// ============================================================================
// Bootstrap helper — set up globals for a Language instance
// ============================================================================

// Refcount for setup/teardown. These imports are stateless wrappers
// around Deno ops that read per-call context from a thread-local
// IsolateState, so it is safe for multiple languages to share the same
// globals. The count is here so that teardown of language A does NOT
// delete globals that language B (still running) depends on.
let __wasmImportsRefcount = 0;

/**
 * Installs the host import functions on globalThis for a Language.
 * `language_bootstrap.js` calls this before every language's init().
 *
 * Refcounted — safe to call once per language load. The actual install
 * only runs on the first call; subsequent calls just bump the refcount.
 */
export function setupWasmImports(): void {
    __wasmImportsRefcount += 1;
    if (__wasmImportsRefcount > 1) return;
    // Agent imports
    (globalThis as any).__agent_did = __agent_did;
    (globalThis as any).__agent_signing_key_id = __agent_signing_key_id;
    (globalThis as any).__agent_sign = __agent_sign;
    (globalThis as any).__agent_sign_string_hex = __agent_sign_string_hex;
    (globalThis as any).__agent_create_signed_expression = __agent_create_signed_expression;
    (globalThis as any).__agent_get_all_local_user_dids = __agent_get_all_local_user_dids;
    (globalThis as any).__agent_create_signed_expression_for_user = __agent_create_signed_expression_for_user;
    (globalThis as any).__agent_did_for_user = __agent_did_for_user;

    // Holochain imports
    (globalThis as any).__holochain_register_dnas = __holochain_register_dnas;
    (globalThis as any).__holochain_call = __holochain_call;
    (globalThis as any).__holochain_call_async = __holochain_call_async;

    // Signal imports
    (globalThis as any).__signal_emit = __signal_emit;

    // Language context imports (camelCase per spec)
    (globalThis as any).__language_storage_directory = __language_storage_directory;
    (globalThis as any).__language_address = __language_address;
    (globalThis as any).__language_settings = __language_settings;
    (globalThis as any).languageStorageDirectory = languageStorageDirectory;
    (globalThis as any).languageAddress = languageAddress;
    (globalThis as any).languageSettings = languageSettings;

    // Canonical camelCase surface (spec §7) — agent
    (globalThis as any).agentDid = agentDid;
    (globalThis as any).agentSigningKeyId = agentSigningKeyId;
    (globalThis as any).agentSign = agentSign;
    (globalThis as any).agentSignStringHex = agentSignStringHex;
    (globalThis as any).agentCreateSignedExpression = agentCreateSignedExpression;
    (globalThis as any).agentGetAllLocalUserDids = agentGetAllLocalUserDids;
    (globalThis as any).agentCreateSignedExpressionForUser = agentCreateSignedExpressionForUser;
    (globalThis as any).agentDidForUser = agentDidForUser;

    // Canonical camelCase surface — holochain
    (globalThis as any).holochainRegisterDnas = holochainRegisterDnas;
    (globalThis as any).holochainCall = holochainCall;
    (globalThis as any).holochainCallAsync = holochainCallAsync;

    // Event emission (spec §7.5) — fan out via LANGUAGE_CONTROLLER to
    // the runtime's perspective/sync/telepresence/signal subscribers.
    (globalThis as any).emitPerspectiveDiff = emitPerspectiveDiff;
    (globalThis as any).emitSyncStateChange = emitSyncStateChange;
    (globalThis as any).emitTelepresenceSignal = emitTelepresenceSignal;
    (globalThis as any).emitSignal = emitSignal;

    // Storage KV (spec §7.4) — file-backed, flushed through Deno's
    // sync FS ops into the per-language storage directory.
    (globalThis as any).storageGet = storageGet;
    (globalThis as any).storagePut = storagePut;
    (globalThis as any).storageDelete = storageDelete;
    (globalThis as any).storageListKeys = storageListKeys;
}

/**
 * Removes the globalThis import functions installed by setupWasmImports.
 * Called during language teardown() to avoid leaks across isolate re-use.
 *
 * Refcounted to mirror setupWasmImports — globals are only removed when
 * the last live language is torn down. This prevents tearing down
 * language A from breaking sibling languages B/C still running in the
 * same isolate.
 */
export function teardownWasmImports(): void {
    if (__wasmImportsRefcount === 0) return;
    __wasmImportsRefcount -= 1;
    if (__wasmImportsRefcount > 0) return;
    const g = globalThis as any;
    delete g.__agent_did;
    delete g.__agent_signing_key_id;
    delete g.__agent_sign;
    delete g.__agent_sign_string_hex;
    delete g.__agent_create_signed_expression;
    delete g.__agent_get_all_local_user_dids;
    delete g.__agent_create_signed_expression_for_user;
    delete g.__agent_did_for_user;
    delete g.__holochain_register_dnas;
    delete g.__holochain_call;
    delete g.__holochain_call_async;
    delete g.__signal_emit;
    delete g.__language_storage_directory;
    delete g.__language_address;
    delete g.__language_settings;
    delete g.languageStorageDirectory;
    delete g.languageAddress;
    delete g.languageSettings;

    delete g.agentDid;
    delete g.agentSigningKeyId;
    delete g.agentSign;
    delete g.agentSignStringHex;
    delete g.agentCreateSignedExpression;
    delete g.agentGetAllLocalUserDids;
    delete g.agentCreateSignedExpressionForUser;
    delete g.agentDidForUser;
    delete g.holochainRegisterDnas;
    delete g.holochainCall;
    delete g.holochainCallAsync;
    delete g.emitPerspectiveDiff;
    delete g.emitSyncStateChange;
    delete g.emitTelepresenceSignal;
    delete g.emitSignal;
    delete g.storageGet;
    delete g.storagePut;
    delete g.storageDelete;
    delete g.storageListKeys;
}
