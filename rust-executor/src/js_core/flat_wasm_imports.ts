/**
 * # Flat WASM Import Wrapper (JS/Deno side)
 * 
 * Provides JavaScript/Deno implementations of the flat WASM import functions.
 * A WASM-compiled language links against these — the same signatures exist on the Rust side.
 * 
 * ## Usage
 * 
 * A WASM language (compiled via wasm-bindgen) declares these as imports:
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
 * WASM Language (compiled from Rust)
 *     │
 *     │ calls __agent_did(), __signal_emit(), etc.
 *     ▼
 * flat_wasm_imports.js (this file — globalThis functions)
 *     │
 *     │ imports from ext:core/ops
 *     ▼
 * Deno extension ops (agent_extension.rs, languages_extension.rs, signature_extension.rs)
 * ```
 * 
 * ## Bootstrap integration
 * 
 * In language_bootstrap.js, for flat-pattern languages, setupFlatWasmImports()
 * is called before init() to make the import functions available on globalThis.
 */

// ============================================================================
// Import Deno extension ops
// ============================================================================
// These come from ext:core/ops — the built-in Deno ops registered by the
// deno_core extension system. They are plain functions when imported.

import {
    agent_did,
    agent_signing_key_id,
    agent_sign,
    agent_sign_string_hex,
    agent_create_signed_expression,
    agent_get_all_local_user_dids,
    agent_create_signed_expression_for_user,
    agent_did_for_user,
} from 'ext:core/ops';

import {
    ad4m_signal_emitted,
} from 'ext:core/ops';

// Holochain imports are routed through the per-language JS delegate
// (`globalThis.__holochainDelegate__`) that `language_bootstrap.js`
// installs before invoking a flat language's init(). The delegate wraps
// the existing holochain_service_extension ops (install_app, call_zome_function)
// and also maintains the cell_id → languageAddress mapping used by the
// central signal router. There is no direct `holochain_register_dnas` op
// to import from `ext:core/ops`.
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
    return agent_did() as string;
}

/**
 * Returns the signing key ID for the current agent.
 * Rust: `agent_extension::agent_signing_key_id()`
 */
export function __agent_signing_key_id(): string {
    return agent_signing_key_id() as string;
}

/**
 * Signs arbitrary bytes with the current agent's signing key.
 * Rust: `agent_extension::agent_sign()`
 */
export function __agent_sign(payload: Uint8Array): Uint8Array {
    const result = agent_sign(payload) as Uint8Array;
    return result;
}

/**
 * Signs a hex string with the current agent's signing key.
 * Rust: `agent_extension::agent_sign_string_hex()`
 */
export function __agent_sign_string_hex(payload: string): string {
    return agent_sign_string_hex(payload) as string;
}

/**
 * Creates a signed expression with the given data using the current agent.
 * Rust: `agent_extension::agent_create_signed_expression()`
 */
export function __agent_create_signed_expression(data: unknown): object {
    return agent_create_signed_expression(data) as object;
}

/**
 * Gets all local user DIDs (main agent + managed users).
 * Rust: `agent_extension::agent_get_all_local_user_dids()`
 */
export function __agent_get_all_local_user_dids(): string[] {
    return agent_get_all_local_user_dids() as string[];
}

/**
 * Creates a signed expression for a specific user (by email).
 * Rust: `agent_extension::agent_create_signed_expression_for_user()`
 */
export function __agent_create_signed_expression_for_user(userEmail: string, data: unknown): object {
    return agent_create_signed_expression_for_user(userEmail, data) as object;
}

/**
 * Gets the DID for a specific user (by email).
 * Rust: `agent_extension::agent_did_for_user()`
 */
export function __agent_did_for_user(userEmail: string): string {
    return agent_did_for_user(userEmail) as string;
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
 * Rust: `languages_extension::ad4m_signal_emitted()`
 */
export function __signal_emit(data: unknown): void {
    ad4m_signal_emitted(data);
}

// ============================================================================
// Language context imports — set by runtime before calling init()
// ============================================================================

import {
    language_storage_directory,
    language_address,
    language_settings,
} from 'ext:core/ops';

/**
 * Returns the storage directory for this language instance.
 * Rust: `js_core/mod.rs op_language_storage_directory()`
 */
export function __language_storage_directory(): string {
    return language_storage_directory() as string;
}

/**
 * Returns the address (DID) of this language instance.
 * Rust: `js_core/mod.rs op_language_address()`
 */
export function __language_address(): string {
    return language_address() as string;
}

/**
 * Returns the settings JSON for this language instance.
 * Rust: `js_core/mod.rs op_language_settings()`
 */
export function __language_settings(): string {
    return language_settings() as string;
}

// ============================================================================
// Language context imports — camelCase versions for languages to call
// Per spec: languageStorageDirectory(), languageAddress(), languageSettings()
// ============================================================================

export function languageStorageDirectory(): string {
    return language_storage_directory() as string;
}

export function languageAddress(): string {
    return language_address() as string;
}

export function languageSettings(): string {
    return language_settings() as string;
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
export function agentDid(): string { return agent_did() as string; }
export function agentSigningKeyId(): string { return agent_signing_key_id() as string; }
export function agentSign(payload: Uint8Array): Uint8Array { return agent_sign(payload) as Uint8Array; }
export function agentSignStringHex(payload: string): string { return agent_sign_string_hex(payload) as string; }
export function agentCreateSignedExpression(data: unknown): object { return agent_create_signed_expression(data) as object; }
export function agentGetAllLocalUserDids(): string[] { return agent_get_all_local_user_dids() as string[]; }
export function agentCreateSignedExpressionForUser(userEmail: string, data: unknown): object {
    return agent_create_signed_expression_for_user(userEmail, data) as object;
}
export function agentDidForUser(userEmail: string): string { return agent_did_for_user(userEmail) as string; }

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
// IsolateState set up in setupFlatWasmImports().
function currentLanguageAddress(): string {
    try { return language_address() as string; } catch { return ""; }
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
    else ad4m_signal_emitted(data);
}

// ----- Storage key/value (spec §7.4) -----
//
// Current backing store: a process-local in-memory Map, scoped per
// language instance via languageAddress(). Values do NOT persist
// across executor restarts.
//
// This satisfies the §7.4 *interface* contract (read-your-writes
// within an instance, address-scoped namespacing, returns null for
// missing keys) but not its *durability* hint. Languages that require
// persistence across restarts must either roll their own files in
// language_storage_directory() or wait for the runtime to gain a real
// persistent KV op.
//
// TODO(persistent-storage): replace this Map with a Deno op that
// writes under language_storage_directory(), once the executor has a
// shared KV abstraction we can hook into. Tracked separately from the
// language interface refactor.
const __storage = new Map<string, string>();
function storageKey(key: string): string {
    let addr = "";
    try { addr = languageAddress(); } catch (_) { addr = "unknown"; }
    return `${addr}::${key}`;
}
export function storageGet(key: string): string | null {
    const v = __storage.get(storageKey(key));
    return v === undefined ? null : v;
}
export function storagePut(key: string, value: string): void {
    __storage.set(storageKey(key), value);
}
export function storageDelete(key: string): void {
    __storage.delete(storageKey(key));
}
export function storageListKeys(prefix?: string): string[] {
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
// Bootstrap helper — set up globals for WASM language
// ============================================================================

// Refcount for setup/teardown. The flat imports are stateless wrappers
// around Deno ops that read per-call context from a thread-local
// IsolateState, so it is safe for multiple languages to share the same
// globals. The count is here so that teardown of language A does NOT
// delete globals that language B (still running) depends on.
let __flatImportsRefcount = 0;

/**
 * Sets up the globalThis import functions for a flat WASM language.
 * Call this in language_bootstrap.js for flat-pattern languages,
 * before calling the language's init() function.
 *
 * Refcounted — safe to call once per language load. The actual install
 * only runs on the first call; subsequent calls just bump the refcount.
 */
export function setupFlatWasmImports(): void {
    __flatImportsRefcount += 1;
    if (__flatImportsRefcount > 1) return;
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

    // Storage KV (spec §7.4) — in-memory map; see comment above for the
    // durability caveat and persistence TODO.
    (globalThis as any).storageGet = storageGet;
    (globalThis as any).storagePut = storagePut;
    (globalThis as any).storageDelete = storageDelete;
    (globalThis as any).storageListKeys = storageListKeys;
}

/**
 * Cleans up the globalThis import functions.
 * Call this during language teardown() to avoid leaks.
 *
 * Refcounted to mirror setupFlatWasmImports — globals are only removed
 * when the last live flat language is torn down. This prevents tearing
 * down language A from breaking sibling languages B/C still running in
 * the same isolate.
 */
export function teardownFlatWasmImports(): void {
    if (__flatImportsRefcount === 0) return;
    __flatImportsRefcount -= 1;
    if (__flatImportsRefcount > 0) return;
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
