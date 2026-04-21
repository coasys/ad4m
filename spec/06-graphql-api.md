# 6. GraphQL API

## 6.1 Overview

The AD4M executor exposes a **GraphQL** API over HTTP and WebSocket (using `graphql-transport-ws` protocol). This is the primary client interface.

- **HTTP endpoint:** `POST /graphql`
- **WebSocket endpoint:** `ws://host:port/graphql` (for subscriptions)
- **Playground:** `GET /playground`

### Authentication

All requests include an `Authorization` header containing either:
- The admin credential token (pre-shared secret)
- A JWT capability token (see [Agent Model](./02-agent-model.md#25-capability-tokens))

WebSocket connections pass the token in the `connection_init` payload under `headers.authorization`.

## 6.2 Queries

### Agent

```graphql
type Query {
  agent: Agent!
  agentByDID(did: String!): Agent
  agentGetApps: [Apps!]!
  agentGetEntanglementProofs: [EntanglementProof!]!
  agentIsLocked: Boolean!
  agentStatus: AgentStatus!
}
```

### Perspectives

```graphql
type Query {
  perspective(uuid: String!): PerspectiveHandle
  perspectives: [PerspectiveHandle!]!
  perspectiveQueryLinks(uuid: String!, query: LinkQuery!): [DecoratedLinkExpression!]!
  perspectiveQueryProlog(uuid: String!, query: String!): String!
  perspectiveQuerySparql(uuid: String!, query: String!): String!
  perspectiveSnapshot(uuid: String!): Perspective!
}

input LinkQuery {
  source: String
  target: String
  predicate: String
  fromDate: DateTime
  untilDate: DateTime
  limit: Int
}
```

### Languages

```graphql
type Query {
  language(address: String!): LanguageHandle!
  languageMeta(address: String!): LanguageMeta!
  languages(filter: String): [LanguageHandle!]!
  languageSource(address: String!): String!
}
```

### Runtime

```graphql
type Query {
  runtimeFriendStatus(did: String!): PerspectiveExpression
  runtimeInfo: RuntimeInfo!
  runtimeKnownLinkLanguageTemplates: [String!]!
  runtimeFriendSendMessage(did: String!, message: PerspectiveInput!): Boolean!
  getTrustedAgents: [String!]!
}
```

### Neighbourhoods

```graphql
type Query {
  neighbourhoodOtherAgents(perspectiveUUID: String!): [String!]!
  neighbourhoodOnlineAgents(perspectiveUUID: String!): [OnlineAgent!]!
  neighbourhoodHasTelepresence(perspectiveUUID: String!): Boolean!
}
```

## 6.3 Mutations

### Agent

```graphql
type Mutation {
  agentGenerate(passphrase: String!): AgentStatus!
  agentLock(passphrase: String!): AgentStatus!
  agentUnlock(passphrase: String!): AgentStatus!
  agentUpdatePublicPerspective(perspective: PerspectiveInput!): Agent!
  agentAddEntanglementProofs(proofs: [EntanglementProofInput!]!): [EntanglementProof!]!
  agentDeleteEntanglementProofs(proofs: [EntanglementProofInput!]!): [EntanglementProof!]!
  agentEntanglementProofPreFlight(deviceKey: String!, deviceKeyType: String!): EntanglementProof!
  agentRequestCapability(authInfo: AuthInfoInput!): String!
  agentPermitCapability(auth: String!): String!
  agentRevokeToken(requestId: String!): [Apps!]!
  agentSignMessage(message: String!): AgentSignature!
}
```

### Perspectives

```graphql
type Mutation {
  perspectiveAdd(name: String!): PerspectiveHandle!
  perspectiveUpdate(uuid: String!, name: String!): PerspectiveHandle!
  perspectiveRemove(uuid: String!): Boolean!

  perspectiveAddLink(uuid: String!, link: LinkInput!): DecoratedLinkExpression!
  perspectiveAddLinks(uuid: String!, links: [LinkInput!]!): [DecoratedLinkExpression!]!
  perspectiveLinkMutations(uuid: String!, mutations: LinkMutations!): DecoratedPerspectiveDiff!
  perspectiveRemoveLink(uuid: String!, link: LinkExpressionInput!): Boolean!
  perspectiveUpdateLink(uuid: String!, oldLink: LinkExpressionInput!, newLink: LinkInput!): DecoratedLinkExpression!

  perspectiveAddSdna(uuid: String!, name: String!, sdnaCode: String!, sdnaType: String!): Boolean!
}
```

### Neighbourhoods

```graphql
type Mutation {
  neighbourhoodPublishFromPerspective(
    linkLanguage: String!
    meta: PerspectiveInput!
    uuid: String!
  ): String!

  neighbourhoodJoinFromUrl(url: String!): PerspectiveHandle!

  neighbourhoodSetOnlineStatus(
    perspectiveUUID: String!
    status: PerspectiveInput!
  ): Boolean!

  neighbourhoodSendSignal(
    perspectiveUUID: String!
    remoteAgentDid: String!
    payload: PerspectiveInput!
  ): Boolean!

  neighbourhoodSendBroadcast(
    perspectiveUUID: String!
    payload: PerspectiveInput!
  ): Boolean!
}
```

### Languages

```graphql
type Mutation {
  languagePublish(languagePath: String!, languageMeta: LanguageMetaInput!): LanguageMeta!
  languageApplyTemplateAndPublish(
    sourceLanguageHash: String!
    templateData: String!
  ): LanguageRef!
  languageRemove(address: String!): Boolean!
  languageWriteSettings(languageAddress: String!, settings: String!): Boolean!
}
```

### Runtime

```graphql
type Mutation {
  addTrustedAgents(agents: [String!]!): [String!]!
  removeTrustedAgents(agents: [String!]!): [String!]!
  runtimeAddFriend(did: String!): [String!]!
  runtimeRemoveFriend(did: String!): [String!]!
  runtimeAddKnownLinkLanguageTemplate(address: String!): [String!]!
  runtimeRemoveKnownLinkLanguageTemplate(address: String!): [String!]!
}
```

## 6.4 Subscriptions

```graphql
type Subscription {
  agentStatusChanged: AgentStatus!
  agentUpdated: Agent!

  perspectiveAdded: PerspectiveHandle!
  perspectiveUpdated: PerspectiveHandle!
  perspectiveRemoved: String!

  perspectiveLinkAdded(uuid: String!): DecoratedLinkExpression!
  perspectiveLinkRemoved(uuid: String!): DecoratedLinkExpression!
  perspectiveLinkUpdated(uuid: String!): LinkExpressionUpdated!
  perspectiveSyncStateChange(uuid: String!): String!

  neighbourhoodSignal(perspectiveUUID: String!): PerspectiveExpression!

  exceptionOccurred: ExceptionInfo!
  runtimeMessageReceived: PerspectiveExpression!
}
```

## 6.5 Core Types

```graphql
type Agent {
  did: String!
  perspective: Perspective
}

type AgentStatus {
  did: String
  didDocument: String
  error: String
  isInitialized: Boolean!
  isUnlocked: Boolean!
}

type Perspective {
  links: [DecoratedLinkExpression!]!
}

type DecoratedLinkExpression {
  author: String!
  timestamp: String!
  data: Link!
  proof: DecoratedExpressionProof!
  status: LinkStatus
}

type Link {
  source: String!
  target: String!
  predicate: String
}

type DecoratedExpressionProof {
  key: String!
  signature: String!
  valid: Boolean
  invalid: Boolean
}

enum LinkStatus {
  shared
  local
}

type PerspectiveHandle {
  uuid: String!
  name: String
  neighbourhood: DecoratedNeighbourhoodExpression
  sharedUrl: String
  state: PerspectiveState!
}

enum PerspectiveState {
  Private
  NeighbourhoodCreationInitiated
  NeighbourhoodJoinInitiated
  LinkLanguageFailedToInstall
  LinkLanguageInstalledButNotSynced
  Synced
}

type RuntimeInfo {
  ad4mExecutorVersion: String!
  isInitialized: Boolean!
  isUnlocked: Boolean!
}
```

> **Note:** This is a representative subset of the full schema. The executor generates a `schema.gql` file at startup. Alternative implementations SHOULD generate a compatible schema. See the source at `rust-executor/src/graphql/` for the complete type definitions.

### Query Engine Notes

- `perspectiveQuerySparql` — Executes a SPARQL 1.1 query against the Oxigraph triple store. The query string MUST be valid SPARQL 1.1. Only read-only queries (SELECT/ASK/CONSTRUCT/DESCRIBE) are accepted; UPDATE operations are rejected.
- `perspectiveQueryProlog` — Executes a Prolog query for SHACL inference and subject-class resolution.
- `perspectiveQueryLinks` — Structured link-pattern queries, translated internally to SPARQL.
- The `Agent` type does not include a `directMessageLanguage` field. Inbox discovery uses `ad4m://inbox` predicate links in the agent's public perspective (see [§2.3](./02-agent-model.md#23-agent-expression)).
