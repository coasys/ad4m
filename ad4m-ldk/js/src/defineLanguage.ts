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
    /** The links the remote holds; null when the language does not know them
     *  (the executor's fallback sync then skips, rather than push what is missing). */
    render(): Promise<Perspective | { links: any[] } | null>;
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

export interface MountShapeEntry { name: string; shape: object }
export interface MountShapes { version: string; shapes: MountShapeEntry[] }
export interface MountEntry { class: string; address: Address; data: Record<string, unknown> }
export interface MountSnapshot { entries: MountEntry[] }
export interface MountCapability {
    shapes(): MountShapes | Promise<MountShapes>;
    snapshot(): MountSnapshot | Promise<MountSnapshot>;
}

/**
 * A class `mount.classes` generates a shape for. The client SDK's `@Model`
 * decorator (`@coasys/ad4m/model`) attaches `generateSHACL()` to the
 * constructor; a class declared without the SDK can provide the same
 * method by hand. `{ name, shape }` is what `generateSHACL()` returns.
 */
export interface MountClass {
    new (...args: any[]): any;
    /** The mounted name — `@Model({ name })` sets it on the constructor.
     *  `generateSHACL().name` wins when both exist. */
    className?: string;
    generateSHACL?(): { name: string; shape: { toJSON(): object } };
}

/**
 * The `mount.classes` form: describe the mounted classes with the SDK's
 * model decorators and let `mountShapes()` build itself from them.
 * `mountShapes()` produces `{ version, shapes: [{ name, shape }] }` — the
 * same wire form `mount.shapes` writes by hand.
 */
export interface MountClassesSpec {
    /** The classes this language mounts. */
    classes: MountClass[];
    /**
     * The `mountShapes()` format version — the mount engine re-installs
     * when it changes. A thunk when the version moves at runtime.
     */
    version: string | (() => string | Promise<string>);
    /** The records to show in the mounted perspective. */
    snapshot(): MountSnapshot | Promise<MountSnapshot>;
    /**
     * How the `{language}` placeholder in a call action's `source`/`target`
     * resolves to this language's own address. Defaults to the
     * `languageAddress()` host call — override only where the host import
     * is unavailable (e.g. tests outside a language runtime).
     */
    languageAddress?: string | (() => string | Promise<string>);
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
    /** The agent language's identity interface (section 5.8 of
     *  `docs-src/language-interface-spec.md`). The executor
     *  calls it only on the language at `system_addresses.agent_language`. */
    identity?: IdentitySpec;
    /** The agent language's token interface (section 5.9 of the same spec), agent
     *  language only. */
    token?: TokenSpec;

    /** Holochain signal handler (spec §8). The runtime routes Holochain
     * signals to this export based on a DnaHash → instance map. */
    handleHolochainSignal?(signal: unknown): void | Promise<void>;

    /** Mount capability: shapes and records shown in a perspective (Acorn spike). Export both or neither. */
    mount?: MountCapability | MountClassesSpec;
}

/**
 * Replace every `{language}` in a call action's `source` (and `target`
 * params text) with the language's own address. Call actions are the only
 * shape field carrying the placeholder — a class is decorated before it
 * knows which language mounts it, so `source: "{language}://class/X"`
 * defers the address to `mountShapes()` time. Returns a new shape; the
 * `generateSHACL()` output (which callers may memoize) is never mutated.
 */
export function resolveMountCallSources<T>(shape: T, languageAddress: string): T {
    const sub = (v: unknown) =>
        typeof v === "string" && v.includes("{language}")
            ? v.replaceAll("{language}", languageAddress)
            : v;
    const actions = (list: unknown) =>
        Array.isArray(list)
            ? list.map((a) => (a && typeof a === "object"
                ? { ...a, source: sub((a as any).source), target: sub((a as any).target) }
                : a))
            : list;
    const s = shape as any;
    if (!s || typeof s !== "object") return shape;
    return {
        ...s,
        ...(s.constructor_actions && { constructor_actions: actions(s.constructor_actions) }),
        ...(s.destructor_actions && { destructor_actions: actions(s.destructor_actions) }),
        ...(Array.isArray(s.properties) && {
            properties: s.properties.map((p: any) => (p && typeof p === "object"
                ? {
                    ...p,
                    ...(p.setter && { setter: actions(p.setter) }),
                    ...(p.adder && { adder: actions(p.adder) }),
                    ...(p.remover && { remover: actions(p.remover) }),
                }
                : p)),
        }),
    };
}

/** Whether a shape's JSON text contains the `{language}` placeholder at all. */
function usesLanguagePlaceholder(shape: object): boolean {
    return JSON.stringify(shape).includes("{language}");
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
    perspectiveSyncRender?(): Promise<Perspective | { links: any[] } | null>;
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

    // Mount
    mountShapes?(): MountShapes | Promise<MountShapes>;
    mountSnapshot?(): MountSnapshot | Promise<MountSnapshot>;
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

    if (spec.mount) {
        const m = spec.mount;
        if ("classes" in m) {
            // `mount.classes` form: `mountShapes()` builds itself from each
            // class's `generateSHACL()`; `{language}` in call action
            // sources resolves to the language's own address (the
            // `languageAddress()` host call unless overridden).
            const resolveAddress = async (): Promise<string> => {
                const la = m.languageAddress;
                if (typeof la === "function") return la();
                if (la) return la;
                // Lazy so that loading the LDK outside a language runtime
                // (unit tests in Node) never resolves `ad4m:host`; a file
                // specifier keeps the import intact under the language
                // bundler — a bare `import("ad4m:host")` would degrade to a
                // `require()` call the runtime cannot serve.
                const { languageAddress } = await import("./imports.js");
                return languageAddress();
            };
            out.mountShapes = async () => {
                const version = typeof m.version === "function" ? await m.version() : m.version;
                const shapes: MountShapeEntry[] = [];
                let address: string | undefined;
                for (const cls of m.classes) {
                    const gen = cls.generateSHACL?.();
                    if (!gen?.shape?.toJSON) {
                        throw new Error(
                            `mount class ${cls.className ?? cls.name} has no generateSHACL() — ` +
                            `decorate it with @Model (from "@coasys/ad4m/model") or provide one`
                        );
                    }
                    const name = gen.name ?? cls.className ?? cls.name;
                    const shape = gen.shape.toJSON() as Record<string, unknown>;
                    // A mount shape keeps no node_shape_uri: the executor's
                    // shape parser derives `{ns}{Class}Shape` from
                    // `target_class` when it is absent — the form
                    // hand-written mountShapes() already use.
                    delete shape.node_shape_uri;
                    if (usesLanguagePlaceholder(shape) && address === undefined) {
                        address = await resolveAddress();
                    }
                    shapes.push({
                        name,
                        shape: address === undefined ? shape : resolveMountCallSources(shape, address),
                    });
                }
                return { version, shapes };
            };
            out.mountSnapshot = m.snapshot.bind(m);
        } else {
            out.mountShapes = m.shapes.bind(m);
            out.mountSnapshot = m.snapshot.bind(m);
        }
    }

    return out;
}
