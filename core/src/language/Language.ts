import type { Address } from '../Address'
import { DID } from '../DID';
import type { Expression } from '../expression/Expression'
import { Perspective, PerspectiveExpression } from '../perspectives/Perspective';
import { PerspectiveDiff } from '../perspectives/PerspectiveDiff';
import { InputType, Field, ObjectType } from "type-graphql";
import { PerspectiveState } from '../perspectives/PerspectiveHandle';
import { LinkQuery } from '../perspectives/LinkQuery';

/**
 * # AD4M Language Interface
 * 
 * All AD4M languages use the **flat export format** — direct function exports,
 * no factory, no wrapper object. This is the single interface for both JS/Deno
 * and WASM-compiled languages.
 * 
 * ## Flat Export Format (primary interface)
 * 
 * ```javascript
 * export const name = "my-language";
 * export const version = "0.1.0";
 * 
 * export async function init(contextJson) {
 *     // contextJson: JSON string with { storageDirectory, customSettings, languageAddress }
 *     // Non-serializable delegates available via globalThis:
 *     //   globalThis.__agentProxy__      — agent identity & signing
 *     //   globalThis.__holochainDelegate__ — Holochain DNA registration & zome calls
 *     //   globalThis.__ad4mSignal__      — signal emission
 * }
 * 
 * // Capability functions (presence = capability):
 * export async function expressionGet(address) { /* ... *\/ }
 * export async function expressionCreate(content) { /* returns address *\/ }
 * export function interactions(expressionAddress) { /* returns interaction spec *\/ }
 * export async function linkQuery(query) { /* ... *\/ }
 * // etc.
 * ```
 * 
 * The flat export format is converted to the internal `Language` interface
 * by the `language_bootstrap.js` adapter. The internal `Language` interface
 * is an implementation detail — languages only need to export flat functions.
 * 
 * ## Legacy Languages (create() factory) — adapter shim
 * 
 * Legacy languages that use the `create(context) -> Language` factory pattern
 * are supported via a thin shim in `language_bootstrap.js`. This shim wraps
 * the legacy factory and exposes flat exports internally.
 * 
 * **Migration:** All languages should migrate to flat exports. Once migrated,
 * the legacy adapter shim can be removed.
 */

// ============================================================================
// FLAT WASM IMPORT INTERFACE TYPES (for WASM-compiled languages)
// ============================================================================
// When a language is compiled to WASM (instead of running in JS/Deno), it cannot
// access the globalThis delegates directly. Instead, the WASM host exposes
// flat import functions that the WASM module links against.
//
// Both the Rust executor AND the JS/Deno bootstrap implement the same functions.
// The language doesn't know or care which one it's running on.
//
// Usage (in WASM language Rust source with wasm-bindgen):
// ```rust
// #[wasm_bindgen]
// extern "C" {
//     fn __agent_did() -> String;
//     fn __agent_sign(payload: &[u8]) -> Vec<u8>;
//     fn __holochain_call(dna: &str, zome: &str, fn_name: &str, params: JsValue) -> JsValue;
//     fn __signal_emit(data: JsValue);
// }
// ```

/** Agent flat import functions — these map to AgentDelegate methods */
export interface AgentWasmImports {
    /** Returns the current agent's DID */
    __agent_did(): string;
    /** Returns the signing key ID */
    __agent_signing_key_id(): string;
    /** Signs arbitrary bytes, returns signature bytes */
    __agent_sign(payload: Uint8Array): Uint8Array;
    /** Signs a hex string, returns hex signature */
    __agent_sign_string_hex(payload: string): string;
    /** Creates a signed expression with the given data */
    __agent_create_signed_expression(data: unknown): Expression;
    /** Gets all local user DIDs */
    __agent_get_all_local_user_dids(): string[];
    /** Creates a signed expression for a specific user (by email) */
    __agent_create_signed_expression_for_user(userEmail: string, data: unknown): Expression;
    /** Gets DID for a specific user (by email) */
    __agent_did_for_user(userEmail: string): string;
}

/** Holochain flat import functions — these map to HolochainDelegate methods */
export interface HolochainWasmImports {
    /** Registers DNAs, returns AppInfo array */
    __holochain_register_dnas(dnas: DnaSpec[]): AppInfo[];
    /** Synchronous call to a zome function */
    __holochain_call(dnaNick: string, zome: string, fnName: string, params: unknown): unknown;
    /** Asynchronous call to a zome function */
    __holochain_call_async(dnaNick: string, zome: string, fnName: string, params: unknown): Promise<unknown>;
}

/** Signal flat import function — emits signals to the AD4M signal bus */
export type SignalWasmImport = (data: unknown) => void;

/** All flat WASM import functions combined */
export interface FlatWasmImports extends AgentWasmImports, HolochainWasmImports {
    /** Emits a signal to the signal bus */
    __signal_emit: SignalWasmImport;
}

// ============================================================================
// FLAT EXPORT LANGUAGE INTERFACE TYPES (for JS/Deno languages)
// ============================================================================
// These types define the flat export pattern for AD4M languages.
// Languages export functions directly (flat) instead of via a create() factory.
// The language_bootstrap.js adapter wrapper converts flat exports to the
// internal Language interface.

// ----- Context passed via init() -----

/** Context passed to flat-export languages via init(contextJson: string).
 * This is the only serializable data that crosses the WASM/JS boundary.
 */
export interface LanguageInitContext {
    /** Directory path for language-specific file storage */
    storageDirectory: string;
    /** Language-specific settings configured by the agent */
    customSettings: Record<string, unknown>;
    /** This language's address in the Holochain network */
    languageAddress: string;
}

// ----- Delegates available via globalThis -----

/** Agent delegate — available via globalThis.__agentProxy__ in flat-export languages.
 * Handles identity, signing, and expression creation.
 */
export interface AgentDelegate {
    did: string;
    signingKeyId: string;
    createSignedExpression(data: unknown): Expression;
    sign(payload: string): string;
    signStringHex(payload: string): string;
    getAllLocalUserDids(): string[];
    createSignedExpressionForUser(userEmail: string, data: unknown): Expression;
    didForUser(userEmail: string): string;
}

/** Holochain delegate — available via globalThis.__holochainDelegate__ in flat-export languages.
 * Provides access to DNA calls and DNA registration.
 */
export interface HolochainDelegate {
    registerDNAs(dnas: DnaSpec[]): AppInfo[];
    call(dnaNick: string, zome: string, fnName: string, params: unknown): unknown;
    callAsync(dnaNick: string, zome: string, fnName: string, params: unknown): Promise<unknown>;
}

/** Signal delegate — available via globalThis.__ad4mSignal__ in flat-export languages.
 * Emits signals to the AD4M signal bus.
 */
export type SignalDelegate = (signal: unknown) => void;

// ----- Supporting types for delegates -----

/** Specification for registering a DNA with the Holochain delegate */
export interface DnaSpec {
    nick: string;
    source: DnaSource;
}

/** Source of a DNA bundle */
export interface DnaSource {
    type: 'path' | 'bundle' | 'bytes';
    value: string | Uint8Array;
}

/** Result of DNA registration */
export interface AppInfo {
    appId: string;
    dnaHash: string;
    cellId: string;
}

// ----- Flat export function signatures -----
// These signatures match the flat export functions that languages can provide.
// All are optional — languages only implement the capabilities they need.

/** Minimal required exports for a flat-export language */
export interface FlatLanguageBase {
    name: string;
    version?: string;
    init(contextJson: string): Promise<void>;
    teardown?(): void;
}

/** Expression language flat exports (expressionAdapter) */
export interface FlatExpressionLanguage extends FlatLanguageBase {
    /** ExpressionAdapter.get */
    expressionGet?(address: string): Promise<unknown | null>;
    /** ExpressionAdapter.putAdapter.createPublic */
    expressionCreate?(content: object): Promise<string>;
    /** ExpressionAdapter.putAdapter.addressOf (ReadOnlyLanguage) */
    expressionAddressOf?(content: object): Promise<string>;
    /** Language.isImmutableExpression */
    isImmutableExpression?(address: string): boolean;
    /** ExpressionUI.icon */
    expressionIcon?(): string;
    /** ExpressionUI.constructorIcon */
    expressionConstructorIcon?(): string;
}

/** Link/sync language flat exports (perspectiveSyncAdapter) */
export interface FlatLinkLanguage extends FlatLanguageBase {
    /** PerspectiveSyncAdapter.sync */
    linkSyncSync?(): Promise<PerspectiveDiff>;
    /** PerspectiveSyncAdapter.commit */
    linkSyncCommit?(diff: PerspectiveDiff): Promise<string>;
    /** PerspectiveSyncAdapter.render */
    linkSyncRender?(address: string): Promise<string>;
    /** PerspectiveSyncAdapter.currentRevision */
    linkSyncCurrentRevision?(): Promise<string | null>;
    /** PerspectiveSyncAdapter.others */
    linkSyncOthers?(): Promise<Expression[]>;
    /** PerspectiveSyncAdapter.writable */
    linkSyncWritable?(): Promise<boolean>;
    /** PerspectiveSyncAdapter.public */
    linkSyncPublic?(): Promise<boolean>;
    /** PerspectiveSyncAdapter.addCallback */
    linkSyncAddCallback?(callback: PerspectiveDiffObserver): void;
    /** PerspectiveSyncAdapter.removeCallback */
    linkSyncRemoveCallback?(callback: PerspectiveDiffObserver): void;
    /** PerspectiveSyncAdapter.addSyncStateChangeCallback */
    linkSyncAddSyncStateChangeCallback?(callback: SyncStateChangeObserver): void;
    /** PerspectiveSyncAdapter.setLocalAgents */
    linkSyncSetLocalAgents?(agents: string[]): void;
}

/** Interaction-capable flat language exports */
export interface FlatInteractionLanguage {
    /** Language.interactions */
    interactions?(address: string): Interaction[];
}

// ----- Union type for all flat exports -----
// Languages can extend FlatExpressionLanguage, FlatLinkLanguage, or both.

export type FlatLanguageExports = FlatLanguageBase &
    Partial<FlatExpressionLanguage> &
    Partial<FlatLinkLanguage> &
    Partial<FlatInteractionLanguage>;

/** Interface of AD4M Languages
 * 
 * The AD4M-internal representation of a language (after adapter wrapper conversion).
 * This is what the executor works with internally.
 * 
 * Since there are a few different kinds of languages, this interface is split into optional sub-interfaces.
 * The only required property is the name of the language.
 * 
 * The most usual kind of language is the "Expression Language", which is a language that can be used to create
 * and share Expressions.
 * For that, implement the expressionsAdapter and expressionUI interface.
 * 
 * The second most common kind of language is the "Link Language", which is a language that builds the core
 * of AD4M Neighbourhoods.
 * For that, implement the linksAdapter interface.
 */
export interface Language {
    readonly name: string;

    /** Flagging expressions as immutable to enable
     * expression caching in the ad4m-executor
     */
    isImmutableExpression?(expression: Address): boolean;

    // Adapter implementations:

    /** ExpressionAdapter implements means of getting an Expression
     * by address and putting an expression
     */
    readonly expressionAdapter?: ExpressionAdapter;

    /** Interface for getting UI/web components for rendering Expressions of this Language */
    readonly expressionUI?: ExpressionUI;

    // TODO: Rename linksAdapter to perspectiveSyncAdapter needs a lot of changes elsewhere...
    /** Interface of LinkLanguages for the core implementation of Neighbourhoods */
    readonly perspectiveSyncAdapter?: PerspectiveSyncAdapter;

    /** Interface for Languages that implement a query return a Perspective (snapshot)
     * Used for back-links and wrapping of APIs that implement queries.
     */
    readonly pespectiveQueryAdapter?: PerspectiveQueryAdapter;

    /** Additional Interface of LinkLanguages that support telepresence features, 
     * that is: 
     *  - seeing who is online and getting a status
     *  - sending/receiveing p2p signals to other online agents without affecting
     *    the shared Perspective of the Neighbourhood
     *  (see TelepresenceAdapter for more details)
    */
    readonly telepresenceAdapter?: TelepresenceAdapter;

    /** Implementation of a Language that defines and stores Languages*/
    readonly languageAdapter?: LanguageAdapter;

    /** Optional adapter for getting Expressions by author */
    readonly getByAuthorAdapter?: GetByAuthorAdapter;
    /** Optional adapter for getting all Expressions */
    readonly getAllAdapter?: GetAllAdapter;

    /** Optional adapter for direct messaging between agents */
    readonly directMessageAdapter?: DirectMessageAdapter;
    
    /** Interface for providing UI components for the settings of this Language */
    readonly settingsUI?: SettingsUI;

    /** Optional function to make any cleanup/teardown if your language gets deleting in the ad4m-executor */
    readonly teardown?: () => void;

    /** All available interactions this agent could execute on given expression */
    interactions(expression: Address): Interaction[];
}

/** UI factories returning web components */
export interface ExpressionUI {
    /** Returns JS code of a web component that renders the given expression */
    icon(): string; 
    /** Returns JS code of a web component used to create new expressions */
    constructorIcon(): string;
}

export interface SettingsUI {
    settingsIcon(): string;
}
/** Interface for the most common Expression Languages */
export interface ExpressionAdapter {
    /** Returns an Expression by address, or null if there is no Expression
     * with that given address
     */
    get(address: Address): Promise<Expression | null>;

    /** Strategy for putting an expression with needs to be different
     * for those two cases:
     * 1. PublicSharing means that this language supports the creation
     *    and sharing of Expressions, which is the common use-case
     * 2. ReadOnlyLanguage means that the Language implements a pre-defined
     *    set of expressions (which can be infinite or finite).
     *    For example the url-iframe Language which directly maps URLs to
     *    addresses - meaning every well formed URL is an address in this
     *    Language. Or a potential Language implementing the verbs/predicates
     *    of a spec like FOAF.
     */
    putAdapter: PublicSharing | ReadOnlyLanguage;
}

/** Implement this interface if your Language supports creation of sharing
 * of Expressions.
 * See ExpressionAdapter
 */
export interface PublicSharing {
    /** Create an Expression and shares it.
     * Return the Expression's address.
     * @param content is the object created by the constructorIcon component
     */
    createPublic(content: object): Promise<Address>;
}

/** Implement this interface if your Language is defined over a static
 * set of pre-defined Expressions.
 */
export interface ReadOnlyLanguage {
    /** This just calculates the address of an object
     * @param content is the object created by the constructorIcon component
     */
    addressOf(content: object): Promise<Address>;
}

export interface LanguageAdapter {
    getLanguageSource(address: Address): Promise<string>;
}

// Implement this if your Language supports retrieval of all Expressions
// authored by a given agent
export interface GetByAuthorAdapter {
    /// Get expressions authored by a given Agent/Identity
    getByAuthor(author: DID, count: number, page: number): Promise<Expression[] | null>;
}

// Implement this if your Language supports retrieval of all Expressions
// stored in the space of that Language.
// Might not be trivial (without trade-off) for Holochain or DHTs
// in general - hence not a required interface.
export interface GetAllAdapter {
    /// Get expressions authored by a given Agent/Identity
    getAll(filter: any, count: number, page: number): Promise<Expression[] | null>;
}

export type PerspectiveDiffObserver = (diff: PerspectiveDiff)=>void;
export type SyncStateChangeObserver = (state: PerspectiveState)=>void;

/** Interface for "Link Languages" that facilitate the synchronization
 * between agents' local Perspectives inside a Neighbourhood.
 * The assumption is that every version of the shared Perspective
 * is labeled with a unique revision string.
 * Changes are committed and retrieved through diffs.
 * Think of a PerspectiveSyncAdapter as a git branch to which agents commit
 * their changes to and pull diffs from their current revision
 * to the latest one.
 */
export interface PerspectiveSyncAdapter {
    writable(): boolean;
    public(): boolean;
    others(): Promise<DID[]>;

    /** What revision are we on now -> what changes are included in output of render() */
    currentRevision(): Promise<string>;

    /**
     * Check for and get new changes,
     * notify others of local changes.
     * This function will be called every
     * few seconds by the ad4m-executor.
     *  */
    sync(): Promise<PerspectiveDiff>;

    /** Returns the full, rendered Perspective at currentRevision */
    render(): Promise<Perspective>;

    /** Publish changes */
    commit(diff: PerspectiveDiff): Promise<string>;

    /** Get push notification when a diff got published */
    addCallback(callback: PerspectiveDiffObserver);

    /** Add a sync state callback method */
    addSyncStateChangeCallback(callback: SyncStateChangeObserver);

    /**
     * Set the local agents (DIDs) that own this perspective/neighbourhood.
     * This is used to determine which agents should be registered in the DHT.
     * Optional - if not implemented, all local agents may be registered.
     * 
     * This is a temporary hack to support multiple users on one node joining the same neighbourhood.
     * Once we migrate the LanguageController to Rust and run Languages per user, each user will get their
     * own language instance and we won't need to explicitly set local agents. This will provide better
     * isolation and avoid the need to share language state between users.
     */
    setLocalAgents?(agents: DID[]): void;
}

export type MessageCallback = (message: PerspectiveExpression) => void;
export type StatusCallback = (caller: DID) => Perspective;
export interface DirectMessageAdapter {
    recipient(): DID;

    status(): Promise<PerspectiveExpression | void>;
    sendP2P(message: Perspective): Promise<PerspectiveExpression|void>;
    sendInbox(message: Perspective): Promise<PerspectiveExpression|void>;

    setStatus(status: PerspectiveExpression);
    inbox(filter?: string): Promise<PerspectiveExpression[]>
    addMessageCallback(callback: MessageCallback);
}

@ObjectType()
export class InteractionParameter {
    @Field()
    name: string

    @Field()
    type: string
}

@ObjectType()
export class InteractionMeta {
    @Field()
    label: string;

    @Field()
    name: string;

    @Field(type => [InteractionParameter])
    parameters: InteractionParameter[]
}
export interface Interaction {
    readonly label: string;
    readonly name: string;
    readonly parameters: InteractionParameter[];
    execute(parameters: object): Promise<string|null>;
}

@InputType()
export class InteractionCall {
    @Field()
    name: string;
    @Field()
    parametersStringified: string;

    public get parameters(): object {
        return JSON.parse(this.parametersStringified)
    }

    constructor(name: string, parameters: object) {
        this.name = name
        this.parametersStringified = JSON.stringify(parameters)
    }
}

@ObjectType()
export class OnlineAgent {
    @Field()
    did: DID
    @Field()
    status: PerspectiveExpression
}

export type TelepresenceSignalCallback = (payload: PerspectiveExpression, recipientDid?: string) => void;
export interface TelepresenceAdapter {
    setOnlineStatus(status: PerspectiveExpression): Promise<void>;
    getOnlineAgents(): Promise<OnlineAgent[]>;

    sendSignal(remoteAgentDid: string, payload: PerspectiveExpression): Promise<object>;
    sendBroadcast(payload: PerspectiveExpression): Promise<object>;
    registerSignalCallback(callback: TelepresenceSignalCallback): Promise<void>;
}


/** Interface for Languages that implement queries returning links, i.e. a Perspective (-snapshot)
 * Main differentiation between these Languages and LinkLanguages is that this enables
 * access to other shared perspectives without forcing a full-sync.
 * 
 * All PerspectiveQuery Languages are supposed to implement `linkQuery` which is a simple graph query
 * specifying non or all of source, predicate, target of the links we want to get.
 * This is enough to implement simple back-links.
 * 
 * Prolog queries are optional since this requires a Prolog engine to be available.
 * 
 */
export interface PerspectiveQueryAdapter {
    /** Same semantic as PerspectiveProxy.get(LinkQuery) */
    linkQuery(query: LinkQuery): Promise<Perspective>;

    /** Tells ADAM if Prolog queries are implemented by this Language.
     * If not, prologQuery won't be used, instead in some circumstances,
     * linkQuery() might be called with an all-query, followed by using
     * ADAM's internal Prolog engine on the result.
     */
    supportsPrologQueries(): boolean;

    /** Same semantic as PerspectiveProxy.infer, will return plain Prolog results */
    infer(prologQuery: string): Promise<any>

    /** Specify which links shall be returned through Prolog.
     * Assumes unbound variables in query that describe a LinkExpression:
     *  - Source
     *  - Predicate
     *  - Target
     *  - Author
     *  - Timestamp
     * Will construct a Perspective where each link is a solution to the query.
     */
    prologQuery(query: string): Promise<Perspective>;
}