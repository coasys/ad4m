# 3. Language Interface

## 3.1 Overview

A **Language** in AD4M is a plugin that implements one or more adapter interfaces. Languages are JavaScript/TypeScript modules that export a `create()` function returning a `Language` object.

Languages are the core extensibility mechanism — they define how data is stored, retrieved, and shared. Every piece of content in AD4M (including agents, neighbourhoods, and other languages) is accessed through a Language.

## 3.2 Language Object

```typescript
interface Language {
  readonly name: string;

  // Content storage
  readonly expressionAdapter?: ExpressionAdapter;
  readonly expressionUI?: ExpressionUI;

  // Neighbourhood sync
  readonly linksAdapter?: LinkSyncAdapter;
  readonly telepresenceAdapter?: TelepresenceAdapter;

  // Meta-language (for the Language Language)
  readonly languageAdapter?: LanguageAdapter;

  // Optional query adapters
  readonly getByAuthorAdapter?: GetByAuthorAdapter;
  readonly getAllAdapter?: GetAllAdapter;

  // Direct messaging
  readonly directMessageAdapter?: DirectMessageAdapter;

  // Settings UI
  readonly settingsUI?: SettingsUI;

  // Cleanup
  readonly teardown?: () => void;

  // Expression caching hint
  isImmutableExpression?(address: Address): boolean;

  // Available interactions for a given expression
  interactions(address: Address): Interaction[];
}
```

### Language Creation

A Language module MUST export a default `create` function:

```typescript
export default async function create(context: LanguageContext): Promise<Language>
```

## 3.3 LanguageContext

The executor provides a `LanguageContext` to each Language during creation:

```typescript
interface LanguageContext {
  agent: AgentService;
  signatures: SignaturesService;
  storageDirectory: string;
  customSettings: object;
  Holochain: HolochainLanguageDelegate | undefined;
  ad4mSignal: Ad4mSignalCB;
}

interface AgentService {
  readonly did: string;
  createSignedExpression(data: any): Expression;
}

interface SignaturesService {
  verify(expr: Expression): boolean;
}
```

### HolochainLanguageDelegate

For Holochain-backed Languages, the executor provides a delegate for DNA registration and zome calls:

```typescript
interface HolochainLanguageDelegate {
  registerDNAs(dnas: Dna[], holochainSignalCallback?: AppSignalCb): Promise<void>;
  call(dnaNick: string, zomeName: string, fnName: string, params: object | string): Promise<any>;
  callAsync(calls: CallSpec[], timeoutMs?: number): Promise<any[]>;
}

interface Dna {
  file: Buffer;
  nick: string;
  zomeCalls: [string, string][];
}
```

## 3.4 Expression Languages

Expression Languages store and retrieve content. They are the most common Language type.

### ExpressionAdapter

```typescript
interface ExpressionAdapter {
  get(address: Address): Promise<Expression | null>;
  putAdapter: PublicSharing | ReadOnlyLanguage;
}

// For languages that support content creation
interface PublicSharing {
  createPublic(content: object): Promise<Address>;
}

// For languages with pre-defined/computed addresses
interface ReadOnlyLanguage {
  addressOf(content: object): Promise<Address>;
}
```

### ExpressionUI

```typescript
interface ExpressionUI {
  icon(): string;              // JS code for a web component that renders expressions
  constructorIcon(): string;   // JS code for a web component to create expressions
}
```

## 3.5 Link Languages (LinkSyncAdapter)

Link Languages power Neighbourhood synchronization. They implement a diff-based sync model similar to a distributed version control system.

```typescript
interface LinkSyncAdapter {
  writable(): boolean;
  public(): boolean;
  others(): Promise<DID[]>;

  /** Current revision identifier */
  currentRevision(): Promise<string>;

  /** Poll for changes and notify others of local changes. Called periodically by the executor. */
  sync(): Promise<PerspectiveDiff>;

  /** Get full rendered state at current revision */
  render(): Promise<Perspective>;

  /** Publish a diff */
  commit(diff: PerspectiveDiff): Promise<string>;

  /** Register callback for incoming diffs */
  addCallback(callback: PerspectiveDiffObserver): number;

  /** Register callback for sync state changes */
  addSyncStateChangeCallback(callback: SyncStateChangeObserver): number;

  /** Set local agent DIDs that own this perspective (optional) */
  setLocalAgents?(agents: DID[]): void;
}

type PerspectiveDiffObserver = (diff: PerspectiveDiff) => void;
type SyncStateChangeObserver = (state: PerspectiveState) => void;
```

### Sync Model

1. The executor periodically calls `sync()` on the LinkSyncAdapter.
2. When local links are added/removed, the executor calls `commit(diff)`.
3. The Link Language is responsible for propagating diffs to other agents.
4. Incoming diffs trigger the registered `PerspectiveDiffObserver` callback.
5. `render()` returns the full materialized state — all links after applying all diffs.

## 3.6 TelepresenceAdapter

For real-time presence and signaling within a Neighbourhood:

```typescript
interface TelepresenceAdapter {
  setOnlineStatus(status: PerspectiveExpression): Promise<void>;
  getOnlineAgents(): Promise<OnlineAgent[]>;
  sendSignal(remoteAgentDid: string, payload: PerspectiveExpression): Promise<object>;
  sendBroadcast(payload: PerspectiveExpression): Promise<object>;
  registerSignalCallback(callback: TelepresenceSignalCallback): Promise<void>;
}

interface OnlineAgent {
  did: DID;
  status: PerspectiveExpression;
}

type TelepresenceSignalCallback = (payload: PerspectiveExpression, recipientDid?: string) => void;
```

## 3.7 Other Adapters

### LanguageAdapter

Used only by the Language Language to store/retrieve Language source code:

```typescript
interface LanguageAdapter {
  getLanguageSource(address: Address): Promise<string>;
}
```

### DirectMessageAdapter

```typescript
interface DirectMessageAdapter {
  recipient(): DID;
  status(): Promise<PerspectiveExpression | void>;
  sendP2P(message: Perspective): Promise<PerspectiveExpression | void>;
  sendInbox(message: Perspective): Promise<PerspectiveExpression | void>;
  setStatus(status: PerspectiveExpression): void;
  inbox(filter?: string): Promise<PerspectiveExpression[]>;
  addMessageCallback(callback: MessageCallback): void;
}
```

### GetByAuthorAdapter / GetAllAdapter

```typescript
interface GetByAuthorAdapter {
  getByAuthor(author: DID, count: number, page: number): Promise<Expression[] | null>;
}

interface GetAllAdapter {
  getAll(filter: any, count: number, page: number): Promise<Expression[] | null>;
}
```

## 3.8 Language Metadata

Languages are registered with metadata:

```typescript
interface LanguageMeta {
  address: string;
  author: string;
  description?: string;
  name: string;
  possibleTemplateParams?: string[];
  sourceCodeLink?: string;
  templateAppliedParams?: string;
  templateSourceLanguageAddress?: string;
  templated?: boolean;
}
```

### Language Templating

Languages can be **templated** — a base Language can be instantiated with parameters to create a new Language. This is how link languages are created for new Neighbourhoods: the p-diff-sync template is instantiated with a new Holochain DNA, producing a unique Language for that Neighbourhood.

## 3.9 Interactions

Languages can define interactions — callable actions on expressions:

```typescript
interface Interaction {
  readonly label: string;
  readonly name: string;
  readonly parameters: InteractionParameter[];
  execute(parameters: object): Promise<string | null>;
}

interface InteractionParameter {
  name: string;
  type: string;
}
```
