/**
 * `defineLanguage` — pure transform from a grouped Language spec to the
 * flat-export shape the AD4M runtime dispatcher consumes.
 *
 * No state, no side effects. Importable from tests.
 *
 * Spec §9 (`docs-src/language-interface-spec.md`).
 */

import type {
    Address,
    Expression,
    Perspective,
    PerspectiveDiff,
    QueryRequest,
    QueryResponse,
    DID,
    IdentitySpec,
    TokenSpec,
    LogPage,
    SignedHead,
    AppendRequest,
    KeyIndexEntry,
    PowParams,
    SignedRequest,
    VouchNote,
    AgentRecord,
    SignedOffer,
    OfferOpened,
    OfferListing,
    Claim,
    ClaimReceipt,
    OfferProgress,
    Amount,
    TokenBalance,
    TokenTransfer,
    TransferPage,
} from "./types.js";

// ----- Capability sub-objects -----

export interface ExpressionCapability {
    get?(address: Address): Promise<Expression | null>;
    create?(content: object): Promise<Address>;
    addressOf?(content: object): Promise<Address>;
    isImmutable?(address: Address): boolean;
    icon?(): string;
    constructorIcon?(): string;
}

export interface PerspectiveCommitCapability {
    commit(diff: PerspectiveDiff): Promise<void> | Promise<string>;
}

export interface PerspectiveSyncCapability {
    sync(): Promise<PerspectiveDiff>;
    render(): Promise<Perspective | { links: any[] }>;
    currentRevision(): Promise<string | null> | string | null;
}

export interface PerspectiveQueryCapability {
    supportedKinds(): string[];
    run(req: QueryRequest): Promise<QueryResponse>;
}

export interface PeersCapability {
    setLocal(agents: DID[]): void | Promise<void>;
    remote(): Promise<DID[]>;
}

export interface TelepresenceCapability {
    setOnlineStatus?(status: unknown): Promise<void>;
    getOnlineAgents?(): Promise<unknown[]>;
    sendSignal?(remoteDid: DID, payload: unknown): Promise<object>;
    sendBroadcast?(payload: unknown): Promise<object>;
    registerSignalCallback?(callback: any): Promise<void>;
}

export interface LanguageSourceCapability {
    getSource(address: Address): Promise<string>;
}

// ----- Top-level language spec -----

export interface LanguageSpec {
    name: string;
    version?: string;
    /** Static lifecycle-level privacy hint (spec §5). */
    isPublic?: boolean;
    init(): Promise<void>;
    teardown?(): void | Promise<void>;
    /** Interactions available for the given expression. Spec §5.7 —
     *  the runtime always passes a concrete address. */
    interactions?(address: Address): unknown[];

    expression?: ExpressionCapability;
    languageSource?: LanguageSourceCapability;
    commit?: PerspectiveCommitCapability;
    sync?: PerspectiveSyncCapability;
    query?: PerspectiveQueryCapability;
    peers?: PeersCapability;
    telepresence?: TelepresenceCapability;
    /** The agent language's identity interface (AR-01 4.9). The executor
     *  calls it only on the language at `system_addresses.agent_language`. */
    identity?: IdentitySpec;
    /** The agent language's token interface (AR-01 4.9.1), agent language only. */
    token?: TokenSpec;

    /** Holochain signal handler (spec §8). The runtime routes Holochain
     * signals to this export based on a DnaHash → instance map. */
    handleHolochainSignal?(signal: unknown): void | Promise<void>;
}

/**
 * The flat shape produced by `defineLanguage`. The Language module
 * spreads these into its top-level exports.
 */
export interface FlatLanguageExports {
    name: string;
    version?: string;
    isPublic?(): boolean;
    init(): Promise<void>;
    teardown?(): void | Promise<void>;
    /** Interactions available for the given expression. Spec §5.7 —
     *  the runtime always passes a concrete address. */
    interactions?(address: Address): unknown[];

    // Expression capability
    expressionGet?(address: Address): Promise<Expression | null>;
    expressionCreate?(content: object): Promise<Address>;
    expressionAddressOf?(content: object): Promise<Address>;
    isImmutableExpression?(address: Address): boolean;
    expressionIcon?(): string;
    expressionConstructorIcon?(): string;

    // Language source
    languageGetSource?(address: Address): Promise<string>;

    // Perspective-commit
    perspectiveCommit?(diff: PerspectiveDiff): Promise<any>;

    // Perspective-sync
    perspectiveSyncSync?(): Promise<PerspectiveDiff>;
    perspectiveSyncRender?(): Promise<Perspective | { links: any[] }>;
    perspectiveSyncCurrentRevision?(): Promise<string | null> | string | null;

    // Perspective-query
    perspectiveQuerySupportedKinds?(): string[];
    perspectiveQueryRun?(req: QueryRequest): Promise<QueryResponse>;

    // Peers
    peersSetLocal?(agents: DID[]): void | Promise<void>;
    peersRemote?(): Promise<DID[]>;

    // Telepresence
    telepresenceSetOnlineStatus?(status: unknown): Promise<void>;
    telepresenceGetOnlineAgents?(): Promise<unknown[]>;
    telepresenceSendSignal?(remoteDid: DID, payload: unknown): Promise<object>;
    telepresenceSendBroadcast?(payload: unknown): Promise<object>;
    telepresenceRegisterSignalCallback?(callback: any): Promise<void>;

    // Identity (agent language only)
    identityLog?(did: string, from: number): Promise<LogPage | null>;
    identityHead?(did: string): Promise<SignedHead | null>;
    identityAppend?(did: string, req: AppendRequest): Promise<SignedHead>;
    identityKeyOwner?(multikey: string, waitSec?: number): Promise<KeyIndexEntry | null>;
    identityPowParams?(): Promise<PowParams>;
    identityAttachNote?(did: string, token: string, req: SignedRequest): Promise<VouchNote>;
    identityPutAgent?(record: AgentRecord, req?: SignedRequest): Promise<void>;
    identityOpenOffer?(offer: SignedOffer): Promise<OfferOpened>;
    identityListOffers?(did: string, req: SignedRequest, waitSec?: number): Promise<OfferListing[]>;
    identityCancelOffer?(did: string, offerId: string, req: SignedRequest): Promise<void>;
    identityClaimOffer?(did: string, offerId: string, claim: Claim): Promise<ClaimReceipt>;
    identityOfferState?(did: string, offerId: string, key: string, waitSec?: number): Promise<OfferProgress>;

    // Token (agent language only)
    tokenAssets?(): Promise<string[]>;
    tokenBalance?(account: string, asset: string): Promise<TokenBalance>;
    tokenSend?(to: string, asset: string, amount: Amount, memo?: string): Promise<TokenTransfer>;
    tokenReceived?(cursor: string | null, limit: number): Promise<TransferPage>;
    /** The runtime registers its sink here; it forwards each transfer as the
     *  `token-transfer` language signal (`tokenTransferSignal`). */
    tokenOnTransfer?(callback: (transfer: TokenTransfer) => void): void;

    // Holochain signal
    handleHolochainSignal?(signal: unknown): void | Promise<void>;
}

/**
 * Pure transform: grouped → flat.
 */
export function defineLanguage(spec: LanguageSpec): FlatLanguageExports {
    // `init`, `teardown`, `interactions`, and `handleHolochainSignal` must
    // be bound to the spec object for the same reason every capability
    // method below is: the runtime calls them as free functions
    // (`mod.init()` inside the bootstrap, `language.interactions(addr)`
    // inside the dispatcher, `globalThis.__handleHolochainSignal__(sig)`
    // inside the Holochain signal bridge), and any `this` reference inside
    // the author's implementation would otherwise bind to `mod` (the
    // exports bag) rather than the author's class instance. The
    // capability sub-objects below already .bind(parent) — the top-level
    // hooks were missed, which broke class-style authors whose init
    // called `this.something`.
    const out: FlatLanguageExports = {
        name: spec.name,
        version: spec.version,
        init: spec.init.bind(spec),
    };

    if (typeof spec.isPublic === "boolean") {
        const v = spec.isPublic;
        out.isPublic = () => v;
    }

    if (spec.teardown) out.teardown = spec.teardown.bind(spec);
    if (spec.interactions) out.interactions = spec.interactions.bind(spec);

    // All capability methods below are bound to their parent capability
    // object. Authors frequently write method shorthand like
    // `commit: { async commit(diff) { return this.foo(...); } }` and the
    // runtime calls the flat export as a free function — without binding
    // the method would lose its `this` and crash the dispatcher deep in
    // rust-executor/src/languages/language.rs with an unrelated-looking
    // TypeError. Bind once here so authors don't have to think about it.

    // Expression capability
    if (spec.expression) {
        const e = spec.expression;
        if (e.get) out.expressionGet = e.get.bind(e);
        if (e.create) out.expressionCreate = e.create.bind(e);
        if (e.addressOf) out.expressionAddressOf = e.addressOf.bind(e);
        if (e.isImmutable) out.isImmutableExpression = e.isImmutable.bind(e);
        if (e.icon) out.expressionIcon = e.icon.bind(e);
        if (e.constructorIcon) out.expressionConstructorIcon = e.constructorIcon.bind(e);
    }

    // Language source
    if (spec.languageSource) {
        out.languageGetSource = spec.languageSource.getSource.bind(spec.languageSource);
    }

    // Perspective-commit
    if (spec.commit) {
        out.perspectiveCommit = spec.commit.commit.bind(spec.commit) as any;
    }

    // Perspective-sync
    if (spec.sync) {
        const s = spec.sync;
        out.perspectiveSyncSync = s.sync.bind(s);
        out.perspectiveSyncRender = s.render.bind(s);
        out.perspectiveSyncCurrentRevision = s.currentRevision.bind(s) as any;
    }

    // Perspective-query
    if (spec.query) {
        out.perspectiveQuerySupportedKinds = spec.query.supportedKinds.bind(spec.query);
        out.perspectiveQueryRun = spec.query.run.bind(spec.query);
    }

    // Peers
    if (spec.peers) {
        out.peersSetLocal = spec.peers.setLocal.bind(spec.peers);
        out.peersRemote = spec.peers.remote.bind(spec.peers);
    }

    // Telepresence
    if (spec.telepresence) {
        const t = spec.telepresence;
        if (t.setOnlineStatus) out.telepresenceSetOnlineStatus = t.setOnlineStatus.bind(t);
        if (t.getOnlineAgents) out.telepresenceGetOnlineAgents = t.getOnlineAgents.bind(t);
        if (t.sendSignal) out.telepresenceSendSignal = t.sendSignal.bind(t);
        if (t.sendBroadcast) out.telepresenceSendBroadcast = t.sendBroadcast.bind(t);
        if (t.registerSignalCallback) out.telepresenceRegisterSignalCallback = t.registerSignalCallback.bind(t);
    }

    // Identity
    if (spec.identity) {
        const i = spec.identity;
        out.identityLog = i.log.bind(i);
        out.identityHead = i.head.bind(i);
        out.identityAppend = i.append.bind(i);
        out.identityKeyOwner = i.keyOwner.bind(i);
        out.identityPowParams = i.powParams.bind(i);
        out.identityAttachNote = i.attachNote.bind(i);
        out.identityPutAgent = i.putAgent.bind(i);
        out.identityOpenOffer = i.openOffer.bind(i);
        out.identityListOffers = i.listOffers.bind(i);
        out.identityCancelOffer = i.cancelOffer.bind(i);
        out.identityClaimOffer = i.claimOffer.bind(i);
        out.identityOfferState = i.offerState.bind(i);
    }

    // Token
    if (spec.token) {
        const t = spec.token;
        out.tokenAssets = t.assets.bind(t);
        out.tokenBalance = t.balance.bind(t);
        out.tokenSend = t.send.bind(t);
        out.tokenReceived = t.received.bind(t);
        out.tokenOnTransfer = t.onTransfer.bind(t);
    }

    if (spec.handleHolochainSignal) {
        out.handleHolochainSignal = spec.handleHolochainSignal.bind(spec);
    }

    return out;
}
