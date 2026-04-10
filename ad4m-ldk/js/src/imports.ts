/**
 * Typed wrappers around the runtime imports the AD4M executor installs
 * on `globalThis` before calling a Language's `init()`.
 *
 * Spec §7 (`docs/language-interface-spec.md`) defines the canonical
 * import surface. This file is the JS-side counterpart of the WIT
 * imports in `docs/ad4m-lang.wit`.
 *
 * In production, the executor's `setupFlatWasmImports()` puts every
 * function on `globalThis` before init runs. In tests, you can stub
 * the same names on `globalThis` and these wrappers will pick them up.
 */

import type { DID, Expression, PerspectiveDiff, DnaSpec, AppInfo } from "./types.js";

type GlobalAny = Record<string, any>;
const G: GlobalAny = globalThis as any;

function need<T = any>(name: string): T {
    const fn = G[name];
    if (typeof fn !== "function") {
        throw new Error(
            `[ad4m-ldk] Missing runtime import \`${name}\`. ` +
            `The AD4M executor must install this on globalThis before init().`
        );
    }
    return fn as T;
}

// ============================================================================
// Agent (spec §7.1)
// ============================================================================

export function agentDid(): DID { return need("agentDid")(); }
export function agentSigningKeyId(): string { return need("agentSigningKeyId")(); }
export function agentSign(payload: Uint8Array): Uint8Array { return need("agentSign")(payload); }
export function agentSignStringHex(payload: string): string { return need("agentSignStringHex")(payload); }
export function agentCreateSignedExpression<T = unknown>(data: T): Expression<T> {
    return need("agentCreateSignedExpression")(data);
}
export function agentGetAllLocalUserDids(): DID[] { return need("agentGetAllLocalUserDids")(); }
export function agentCreateSignedExpressionForUser<T = unknown>(userEmail: string, data: T): Expression<T> {
    return need("agentCreateSignedExpressionForUser")(userEmail, data);
}
export function agentDidForUser(userEmail: string): DID { return need("agentDidForUser")(userEmail); }

// ============================================================================
// Holochain (spec §7.2)
// ============================================================================

// NOTE: Both `holochainRegisterDnas` and `holochainCall` are async — the
// runtime installs Promise-returning functions via `setupFlatWasmImports`
// in rust-executor/src/js_core/flat_wasm_imports.ts. Authors MUST await
// these; the prior `unknown`/`AppInfo[]` synchronous return types were
// wrong and silently handed a Promise object to the caller.
export function holochainRegisterDnas(dnas: DnaSpec[]): Promise<AppInfo[]> {
    return need("holochainRegisterDnas")(dnas);
}
export function holochainCall(dnaNick: string, zome: string, fnName: string, params: unknown): Promise<unknown> {
    return need("holochainCall")(dnaNick, zome, fnName, params);
}
export function holochainCallAsync(
    dnaNick: string, zome: string, fnName: string, params: unknown
): Promise<unknown> {
    return need("holochainCallAsync")(dnaNick, zome, fnName, params);
}

// ============================================================================
// Language context (spec §7.3)
// ============================================================================

export function languageAddress(): string { return need("languageAddress")(); }
export function languageSettings(): string { return need("languageSettings")(); }
export function languageStorageDirectory(): string { return need("languageStorageDirectory")(); }

// ============================================================================
// Storage KV (spec §7)
// ============================================================================

export function storageGet(key: string): string | null { return need("storageGet")(key); }
export function storagePut(key: string, value: string): void { need("storagePut")(key, value); }
export function storageDelete(key: string): void { need("storageDelete")(key); }
export function storageListKeys(prefix?: string): string[] { return need("storageListKeys")(prefix); }

// ============================================================================
// Event emission (spec §7.5)
// ============================================================================
// Languages no longer register callbacks. They emit perspective diffs,
// sync state changes, telepresence signals, and arbitrary signals via
// these imports. The runtime fans out to subscribers.

export function emitPerspectiveDiff(diff: PerspectiveDiff): void {
    need("emitPerspectiveDiff")(diff);
}
export function emitSyncStateChange(state: string): void {
    need("emitSyncStateChange")(state);
}
export function emitTelepresenceSignal(payload: unknown, recipientDid?: DID): void {
    need("emitTelepresenceSignal")(payload, recipientDid);
}
export function emitSignal(data: unknown): void {
    need("emitSignal")(data);
}
