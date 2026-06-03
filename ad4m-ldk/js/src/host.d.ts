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

    // Runtime utilities (Spec section 7.7)
    // Canonical AD4M content-address hash: SHA-256 -> CIDv1 -> base58btc,
    // prefixed with "Qm". The deterministic address function used by
    // every content-addressed Language.
    export function hash(data: string): string;

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

    // Holograph (Spec section 7.8 -- OPTIONAL EXTENSION, new in v1 of the
    // holograph-link Language). Surface lives behind the
    // __holographDelegate__ runtime global, populated by
    // rust-executor's holograph_wires module (Step 5 stub /
    // Step 6 real). Every call throws "[ad4m:host] holograph wire ..."
    // if the runtime hasn't installed the delegate. See the holograph
    // spike's SPIKE.md §2.2 Step 5 for the contract.
    //
    // EmittedOp shape returned by holographNextEmitted (string fields
    // are base64-encoded raw bytes; ms timestamp is an i64-compatible
    // number; JS number precision is fine for sub-millisecond
    // timestamps from the Step-4 envelope decoder).
    export interface EmittedOpWire {
        op_id_b64: string;
        created_at_ms: number;
        envelope_b64: string;
    }
    /** Open or create a neighborhood-scoped substrate, returning a
     *  numeric handle threaded through every other holograph call. */
    export function holographCreateNeighborhood(spaceId: string, storageDir: string): Promise<number>;
    /** Commit a locally-authored envelope (base64-encoded CBOR bytes).
     *  Returns the op-id base64. */
    export function holographCommit(handle: number, envelopeB64: string): Promise<string>;
    /** Drive the algorithm-crate render entry point. Returns a JSON-
     *  shaped Perspective `{ links: [...] }`. */
    export function holographRender(handle: number): Promise<{ links: any[] }>;
    /** Pop the next-available EmittedOp for the handle, or null if the
     *  channel is currently drained. JS subscribers loop on this — no
     *  setInterval/setTimeout polling required because the underlying
     *  op awaits the mpsc receiver inside Rust. */
    export function holographNextEmitted(handle: number): Promise<EmittedOpWire | null>;
    /** Register a local agent for the neighborhood (= `local_agent_join`
     *  on the K2 space). Returns the K2 URL this node is reachable at. */
    export function holographJoinAgent(handle: number, agentKeyB64: string): Promise<string>;
    /** Read the current revision pointer (op-id base64) or null. */
    export function holographCurrentRevision(handle: number): Promise<string | null>;
    /** Read the latest revision pointer (op-id base64) or null. */
    export function holographLatestRevision(handle: number): Promise<string | null>;
    /** Tear down the neighborhood. Idempotent. */
    export function holographCloseNeighborhood(handle: number): Promise<void>;
}
