/**
 * Ambient module declaration for the `ad4m:host` synthetic Deno module.
 *
 * At runtime, the executor's StringModuleLoader resolves `ad4m:host`
 * to the host import bridge (`rust-executor/src/js_core/host.js`).
 * This declaration lets TypeScript resolve imports from the same
 * specifier at compile time.
 */

declare module "ad4m:host" {
    // Agent (Spec section 7.1)
    export function agentDid(): string;
    export function agentSigningKeyId(): string;
    export function agentSign(payload: Uint8Array): Uint8Array;
    export function agentSignStringHex(payload: string): string;
    export function agentCreateSignedExpression(data: unknown): object;
    export function agentGetAllLocalUserDids(): string[];
    export function agentCreateSignedExpressionForUser(userEmail: string, data: unknown): object;
    export function agentDidForUser(userEmail: string): string;

    // Holochain (Spec section 7.2)
    export function holochainRegisterDnas(dnas: object[]): Promise<object[]>;
    export function holochainCall(dnaNick: string, zome: string, fnName: string, params: unknown): Promise<unknown>;
    export function holochainCallAsync(dnaNick: string, zome: string, fnName: string, params: unknown): Promise<unknown>;

    // HTTP fetch (Spec section 7.2b)
    export function httpFetch(url: string, method: string, headersJson: string, body: string): Promise<string>;

    // Language context (Spec section 7.3)
    export function languageStorageDirectory(): string;
    export function languageAddress(): string;
    export function languageSettings(): string;

    // Event emission (Spec section 7.5)
    export function emitPerspectiveDiff(diff: unknown): void;
    export function emitSyncStateChange(state: unknown): void;
    export function emitTelepresenceSignal(payload: unknown, recipientDid?: string): void;
    export function emitSignal(data: unknown): void;

    // Storage KV -- CORE (Spec section 7.4)
    // Always available; every runtime implements this.
    export function storageGet(key: string): string | null;
    export function storagePut(key: string, value: string): void;
    export function storageDelete(key: string): void;
    export function storageListKeys(prefix?: string): string[];

    // Storage File I/O -- OPTIONAL EXTENSION (Spec section 7.6)
    // Raw filesystem-like read/write. NOT required by all runtimes.
    // Languages that import these must be prepared for them to throw
    // at call time on runtimes that don't install the extension.
    // Prefer the KV API above unless you specifically need custom
    // storage layouts, large blobs, or shared paths outside per-
    // language scope (e.g. test fixtures storing language bundles).
    export function readStorageFile(path: string): string;
    export function writeStorageFile(path: string, content: string): void;
}
