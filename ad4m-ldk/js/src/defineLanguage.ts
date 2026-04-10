/**
 * `defineLanguage` — pure transform from a grouped Language spec to the
 * flat-export shape the AD4M runtime dispatcher consumes.
 *
 * No state, no side effects. Importable from tests.
 *
 * Spec §9 (`docs/language-interface-spec.md`).
 */

import type {
    Address,
    Expression,
    Perspective,
    PerspectiveDiff,
    QueryRequest,
    QueryResponse,
    DID,
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
    commit?: PerspectiveCommitCapability;
    sync?: PerspectiveSyncCapability;
    query?: PerspectiveQueryCapability;
    peers?: PeersCapability;
    telepresence?: TelepresenceCapability;

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

    if (spec.handleHolochainSignal) {
        out.handleHolochainSignal = spec.handleHolochainSignal.bind(spec);
    }

    return out;
}
