/**
 * Standalone types for the AD4M Language Development Kit.
 *
 * These mirror the canonical types in `@coasys/ad4m` but are kept
 * separate so that the LDK has no runtime dependency on the executor
 * package — Languages built against the LDK can ship as standalone
 * bundles.
 */

export type DID = string;
export type Address = string;

export interface Expression<T = unknown> {
    author: DID;
    timestamp: string;
    data: T;
    proof: ExpressionProof;
}

export interface ExpressionProof {
    signature: string;
    key: string;
    valid?: boolean;
    invalid?: boolean;
}

export interface Link {
    source: string;
    target: string;
    predicate?: string;
}

export interface LinkExpression extends Expression<Link> {
    status?: string;
}

export interface PerspectiveDiff {
    additions: LinkExpression[];
    removals: LinkExpression[];
}

export interface Perspective {
    links: LinkExpression[];
}

/**
 * Coalesced query request — discriminated by `kind`. The set of kinds
 * a Language supports is reported via `perspective-query.supportedKinds`.
 */
export interface QueryRequest {
    kind: string;
    payload: unknown;
}

export interface QueryResponse {
    kind: string;
    payload: unknown;
}

export interface DnaSpec {
    nick: string;
    bundle?: Uint8Array;
    path?: string;
    network_seed?: string;
    source?: { type: "path" | "bundle" | "bytes"; value: string | Uint8Array };
}

export interface AppInfo {
    [key: string]: unknown;
}

export interface ZomeCall {
    dnaNick: string;
    zomeName: string;
    fnName: string;
    params: unknown;
}

// ----- JSON -----

export type JsonValue = null | boolean | number | string | JsonValue[] | JsonObject;
export interface JsonObject {
    [key: string]: JsonValue;
}

// ----- Identity wire types (AR-01 4.5.2, AR-02 4.3.1) -----
//
// Shared by the store, the executor and the agent language. Events travel
// as opaque JSON objects: the store and the executor read them with the
// identity core, the agent language never does. Hashes, key ids and
// multikeys are strings; a bot scope is plain JSON. Optional fields are
// `null` when absent, as the Rust types serialise them.

/** The store's signature over a log's head (`coasys:head`). */
export interface SignedHead {
    scid: string;
    seq: number;
    hash: string;
    time: number;
    /** did:key of the store. */
    store: string;
    sig: string;
}

/** A bot key's pause record (AR-02 4.10.6). */
export interface PauseRecord {
    scid: string;
    /** A bot key. */
    keyId: string;
    paused: boolean;
    version: number;
    /** A non-bot device key of the owner, at the head. */
    by: SignedBy;
    /** `coasys:pause`. */
    sig: string;
    /** Set by the store, outside the signature. */
    storedAt: number;
}

/** `GET /v1/logs/{scid}?from=`. */
export interface LogPage {
    events: JsonObject[];
    /** When the store appended each event (unix seconds), one per event. */
    appendedAt: number[];
    head: SignedHead;
    note: VouchNote | null;
    vouched: boolean;
    pauses: PauseRecord[];
}

/** A proof of work on a new log with no note (AR-01 4.7). */
export interface Pow {
    /** base58btc, 16 bytes. */
    nonce: string;
    bits: number;
}

/** `POST /v1/logs/{scid}/events`. A request carries a vouching token or a proof, never both. */
export interface AppendRequest {
    events: JsonObject[];
    vouch?: string | null;
    pow?: Pow | null;
}

/** `GET /v1/pow`. */
export interface PowParams {
    bits: number;
    previousBits: number;
    changedAt: number;
}

/** `GET /v1/keys/{multikey}`: the identity a key belongs to. */
export interface KeyIndexEntry {
    /** The owner's did:scid. */
    did: string;
    /** "d3". */
    keyId: string;
    added: number;
    /** Seq of the revoke or rotate that ended it. */
    revoked: number | null;
    /** The bot scope in force at the head. */
    bot: JsonObject | null;
    /** An Until key's expiry, or the bot scope's. */
    expires: number | null;
    /** A bot key under a pause record in force; false for every other key. */
    paused: boolean;
    /** The position the answer describes. */
    head: SignedHead;
}

/** The signed part of a store request. */
export interface RequestBody {
    did: string;
    keyId: string;
    kelSeq: number;
    kelHash: string;
    method: string;
    path: string;
    bodyHash: string;
    /** 16 bytes, base58btc. */
    nonce: string;
    expires: number;
}

/** Header `X-Coasys-Request` = base64url(JCS(SignedRequest)). */
export interface SignedRequest {
    req: RequestBody;
    /** `coasys:store-request`. */
    sig: string;
}

/** The log position a signature by a logged key names (AR-01 4.8.3). */
export interface Anchor {
    kelDid: string;
    keyId: string;
    kelSeq: number;
    kelHash: string;
}

/** `PUT /v1/agents/{did}`. */
export interface AgentRecord {
    /** did:scid, or did:key for a bot or a device-only identity. */
    did: string;
    /** Today's Agent: perspective, directMessageLanguage. */
    profile: JsonObject;
    version: number;
    /** The device did:key that signed. */
    signedBy: string;
    /** Required for a did:scid record. */
    anchor: Anchor | null;
    /** `coasys:link` scheme over {profile, version, anchor}. */
    sig: string;
}

/** The platform's note that it checked one email address for this identity. */
export interface VouchNote {
    did: string;
    /** "email". */
    method: string;
    time: number;
    /** did:key of the vouching key. */
    voucher: string;
    /** `coasys:vouch`. */
    sig: string;
}

/** The position of the master key that signed an offer or a pause record. */
export interface SignedBy {
    keyId: string;
    kelSeq: number;
    kelHash: string;
}

/** "node" = an executor. */
export type OfferKind = "device" | "node" | "bot";

export interface Offer {
    scid: string;
    /** 16 random bytes, base58btc. */
    offerId: string;
    kind: OfferKind;
    /** multihash_b58(sha256("coasys:offer" ‖ s)). */
    commit: string;
    /** Unix seconds, at most issue time + 300. */
    expires: number;
    /** A bound offer: only this signing key may claim. Absent on an unbound offer. */
    deviceKey?: string;
    /** A bot offer's scope. Absent on any other offer. */
    bot?: JsonObject;
}

/** `coasys:offer` over {offer, by}, by the current master key. */
export interface SignedOffer {
    offer: Offer;
    by: SignedBy;
    sig: string;
}

export interface Claim {
    offerId: string;
    /** 32 bytes, base58btc; the store checks it against `commit` and never stores it. */
    s: string;
    deviceKey: string;
    /** X25519 of the device key. */
    encKey: string;
    /** Present (true) only for an app that may keep the master key. */
    canKeepMaster?: boolean;
    /** "Pixel 8", at most 64 characters; never enters the log. */
    hint?: string;
    /** The device key over signing_bytes("coasys:claim", ClaimBody). */
    coSig: string;
}

/** What the holder sees for a claim; `s` is gone. */
export interface ClaimView {
    deviceKey: string;
    encKey: string;
    canKeepMaster: boolean;
    hint: string | null;
    coSig: string;
    claimedAt: number;
}

export type OfferState =
    | { state: "open" }
    | { state: "claimed"; claim: ClaimView }
    | { state: "spent"; seq: number }
    | { state: "expired" }
    | { state: "cancelled" }
    | { state: "contested" }
    | { state: "locked" }
    | { state: "superseded" };

/** One item of `GET /v1/offers/{scid}`. */
export interface OfferListing {
    offer: SignedOffer;
    state: OfferState;
}

/** `POST /v1/offers/{scid}`. */
export interface OfferOpened {
    offerId: string;
}

/** `POST /v1/offers/{scid}/{offerId}/claim`: kind and expires come from the stored offer. */
export interface ClaimReceipt {
    state: "claimed";
    kind: OfferKind;
    expires: number;
}

/** `GET /v1/offers/{scid}/{offerId}/state`: `claimed`, `spent` with the add's seq, or a terminal state. */
export interface OfferProgress {
    state: string;
    seq?: number;
}

/**
 * The agent language's identity interface (AR-01 4.9, AR-02 4.16).
 * Each call maps to one store route. The language signs nothing and holds
 * no key: signed events and requests pass through unchanged. A refusal
 * throws an `IdentityError` carrying an `IdentityErrorCode`.
 */
export interface IdentitySpec {
    /** GET /v1/logs/{scid}?from= */
    log(did: string, from: number): Promise<LogPage | null>;
    /** GET /v1/heads/{scid} */
    head(did: string): Promise<SignedHead | null>;
    /** POST /v1/logs/{scid}/events */
    append(did: string, req: AppendRequest): Promise<SignedHead>;
    /** GET /v1/keys/{multikey}?wait= (0–8 seconds) */
    keyOwner(multikey: string, waitSec?: number): Promise<KeyIndexEntry | null>;
    /** GET /v1/pow */
    powParams(): Promise<PowParams>;
    /** POST /v1/notes/{scid} */
    attachNote(did: string, token: string, req: SignedRequest): Promise<VouchNote>;
    /** PUT /v1/agents/{did}; no signed request for a did:key record. */
    putAgent(record: AgentRecord, req?: SignedRequest): Promise<void>;
    /** POST /v1/offers/{scid} */
    openOffer(offer: SignedOffer): Promise<OfferOpened>;
    /** GET /v1/offers/{scid}?wait=, signed by the current master key */
    listOffers(did: string, req: SignedRequest, waitSec?: number): Promise<OfferListing[]>;
    /** DELETE /v1/offers/{scid}/{offerId}, signed by the current master key */
    cancelOffer(did: string, offerId: string, req: SignedRequest): Promise<void>;
    /** POST /v1/offers/{scid}/{offerId}/claim */
    claimOffer(did: string, offerId: string, claim: Claim): Promise<ClaimReceipt>;
    /** GET /v1/offers/{scid}/{offerId}/state?key=&wait= */
    offerState(did: string, offerId: string, key: string, waitSec?: number): Promise<OfferProgress>;
}

export type IdentityForbiddenReason =
    | "vouch"
    | "pow"
    | "claim"
    | "signer"
    | "secret"
    | "bound"
    | "cosig"
    | "writer";

/** How the identity interface refuses a call; HTTP statuses map one to one. */
export type IdentityErrorCode =
    | { code: "conflict"; head: SignedHead }
    | { code: "key-taken"; key: string }
    | { code: "forbidden"; reason: IdentityForbiddenReason }
    | { code: "invalid"; reason: string }
    | { code: "rate-limited"; scope: "address" | "global" | "identity"; retryAfterSec: number }
    /** 410: an offer, inbox message or request that ended, with its state. */
    | { code: "gone"; state: string }
    /** 404: an unknown offer, inbox address or record. */
    | { code: "not-found" }
    | { code: "unavailable" };

// ----- Token interface (AR-01 4.9.1, interfaces only) -----

/** A decimal string: no floats, no 2^53 limit. */
export interface Amount {
    value: string;
    decimals: number;
}

export interface TokenBalance {
    account: string;
    asset: string;
    amount: Amount;
    asOf: number;
}

export interface TokenTransfer {
    id: string;
    /** Accounts are DIDs: did:scid or did:key. */
    from: string;
    to: string;
    /** The implementation's asset id, for example "unyt:<id>" or "synergy:<id>". */
    asset: string;
    amount: Amount;
    memo?: string;
    time: number;
    state: "pending" | "settled" | "refused";
}

export interface TransferPage {
    items: TokenTransfer[];
    cursor: string | null;
}

export interface TokenSpec {
    assets(): Promise<string[]>;
    balance(account: string, asset: string): Promise<TokenBalance>;
    /** Sends from the agent that the language runs for. The language signs through its context, as for expressions. */
    send(to: string, asset: string, amount: Amount, memo?: string): Promise<TokenTransfer>;
    /** Transfers to the agent, newest last, after `cursor`. */
    received(cursor: string | null, limit: number): Promise<TransferPage>;
    /** Fires for every new transfer in or out, and for every change of state. */
    onTransfer(callback: (transfer: TokenTransfer) => void): void;
}

export type TokenErrorCode =
    | { code: "disabled" }
    | { code: "unknown-asset" }
    | { code: "unknown-account" }
    | { code: "insufficient" }
    | { code: "refused"; reason: string }
    | { code: "unavailable" };

/** The tag of the language signal that carries a token transfer. */
export const TOKEN_TRANSFER_SIGNAL = "token-transfer";

/** The language signal a transfer reaches the executor as. */
export interface TokenTransferSignal {
    tag: typeof TOKEN_TRANSFER_SIGNAL;
    transfer: TokenTransfer;
}

export function tokenTransferSignal(transfer: TokenTransfer): TokenTransferSignal {
    return { tag: TOKEN_TRANSFER_SIGNAL, transfer };
}
