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
    const out: FlatLanguageExports = {
        name: spec.name,
        version: spec.version,
        init: spec.init,
    };

    if (typeof spec.isPublic === "boolean") {
        const v = spec.isPublic;
        out.isPublic = () => v;
    }

    if (spec.teardown) out.teardown = spec.teardown;
    if (spec.interactions) out.interactions = spec.interactions;

    // Expression capability
    if (spec.expression) {
        const e = spec.expression;
        if (e.get) out.expressionGet = e.get;
        if (e.create) out.expressionCreate = e.create;
        if (e.addressOf) out.expressionAddressOf = e.addressOf;
        if (e.isImmutable) out.isImmutableExpression = e.isImmutable;
        if (e.icon) out.expressionIcon = e.icon;
        if (e.constructorIcon) out.expressionConstructorIcon = e.constructorIcon;
    }

    // Perspective-commit
    if (spec.commit) {
        out.perspectiveCommit = spec.commit.commit as any;
    }

    // Perspective-sync
    if (spec.sync) {
        const s = spec.sync;
        out.perspectiveSyncSync = s.sync;
        out.perspectiveSyncRender = s.render;
        out.perspectiveSyncCurrentRevision = (() => {
            const v = s.currentRevision();
            return v;
        }) as any;
    }

    // Perspective-query
    if (spec.query) {
        out.perspectiveQuerySupportedKinds = spec.query.supportedKinds;
        out.perspectiveQueryRun = spec.query.run;
    }

    // Peers
    if (spec.peers) {
        out.peersSetLocal = spec.peers.setLocal;
        out.peersRemote = spec.peers.remote;
    }

    // Telepresence
    if (spec.telepresence) {
        const t = spec.telepresence;
        if (t.setOnlineStatus) out.telepresenceSetOnlineStatus = t.setOnlineStatus;
        if (t.getOnlineAgents) out.telepresenceGetOnlineAgents = t.getOnlineAgents;
        if (t.sendSignal) out.telepresenceSendSignal = t.sendSignal;
        if (t.sendBroadcast) out.telepresenceSendBroadcast = t.sendBroadcast;
        if (t.registerSignalCallback) out.telepresenceRegisterSignalCallback = t.registerSignalCallback;
    }

    if (spec.handleHolochainSignal) out.handleHolochainSignal = spec.handleHolochainSignal;

    return out;
}
