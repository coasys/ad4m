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
    // A box sealed to a DID's Ed25519 key; every field is hex (see
    // docs-src/host-contract.md, "Sealed boxes").
    export interface SealedBox {
        ephemeralPublicKey: string;
        nonce: string;
        ciphertext: string;
    }
    // Opens a box sealed to the calling agent (the node or the managed user
    // the call runs for) and returns the plaintext as hex. Throws for a box
    // sealed to any other DID, or an altered one.
    export function agentOpenSealed(sealed: SealedBox): string;
    // The 32-byte seed of the calling agent's room key X25519 key:
    // HKDF-SHA256(agent signing seed, info "adam-server-link-language:x25519-seed:v1").
    // No signing import signs a message equal to or starting with that info.
    export function agentRoomKeySeed(): Promise<Uint8Array>;
    export function agentCreateSignedExpression(data: unknown): object;
    export function agentGetAllLocalUserDids(): string[];
    export function agentCreateSignedExpressionForUser(userEmail: string, data: unknown): object;
    export function agentDidForUser(userEmail: string): string;

    // Holochain (Spec section 7.2)
    export function holochainRegisterDnas(dnas: object[]): Promise<object[]>;
    export function holochainCall(dnaNick: string, zome: string, fnName: string, params: unknown): Promise<unknown>;
    export function holochainCallAsync(dnaNick: string, zome: string, fnName: string, params: unknown): Promise<unknown>;

    // HTTP fetch (Spec section 7.2b)
    export interface HttpFetchResponse {
        status: number;
        body: string;
    }
    // Rejects on network failures, when the call (headers and body) takes
    // longer than 10 s (the timeout error names the URL), and when the reply
    // body passes the host's limit (32 MiB) or the lower `maxBytes`: the body
    // is read as a stream and refused as soon as it passes, with an error
    // whose `code` is "too-large".
    export function httpFetch(url: string, method: string, headersJson: string, body: string, maxBytes?: number): Promise<HttpFetchResponse>;

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

    // Membership (Spec section 7.5b)
    // Why a space does not admit a key (the executor's JoinRefusalReason).
    export type JoinRefusal =
        | "not-a-member"
        | "invite-only"
        | "needs-identity"
        | "invite-expired"
        | "invite-used"
        | "server-refused";
    // What the space made of one membership link: its hash (hex sha256 of
    // author, source, predicate, target and timestamp), whether it counts,
    // and the replay's refusal ("notAdmin", "malformed", ...) when not.
    export interface MembershipLinkResult {
        hash: string;
        counted: boolean;
        reason?: string;
    }
    // Hands the membership links (`coasys://membership/...`) the language
    // synced to its space's membership; a result per link, in order. They
    // never go in a perspective diff.
    export function membershipLinks(links: object[]): Promise<MembershipLinkResult[]>;
    // Whether the language's space admits the key `didKey` now.
    export function membershipAdmits(didKey: string): Promise<{ admitted: boolean; reason?: JoinRefusal }>;
    // Reports a room key change to the space's owners
    // (`neighbourhood-room-key-changed`).
    export function roomKeyEvent(e: { kind: "dropped" | "rotated"; removedKeys: string[]; version?: number }): Promise<void>;

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
