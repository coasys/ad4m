# ADAM

ADAM is an agent-centric layer for the web. A **person** is an identity (`did:scid`) whose public **key log** lists the **device keys** that speak for them. An **executor** process hosts one or more **agents**, each signing with one device key; each agent keeps its data in **perspectives**, graphs of signed links; and **languages**, code loaded into the executor, store, share and sync that data with other agents. Any executor maps a link's device key to its person through the key log, and a **neighbourhood** admits members by person.

**Why:** Data belongs to the person, not to an app, a server or one machine. The person holds many device keys, and a master key that never rests in an executor changes which keys count, so a lost or retired device is revoked without losing the identity. Links name the device key that signed them, so their format stays and the person is one lookup away. Apps and other agents reach the data only through an executor, and only with the person's permission. Languages keep the transport replaceable: the same perspective can sync over a link server, Holochain or another network, and the key logs reach executors through a language too.

This folder holds the mental models of this repository: a high-level expression of the code beside it, in a tree of short pages. The pages are kept 1-1 with the code. A pull request that changes a mental model changes its page in the same pull request. How the pages are written: [Conventions](CONVENTIONS.md).

## Parts
- [Agents](agents/README.md): who acts and through what — identities and their key logs, devices, the executor that hosts agents, what each caller may do, and the interfaces callers use.
- [Languages](languages/README.md): how data leaves the executor — code that stores, shares and syncs expressions, including the agent language that carries the key logs.
- [Perspectives](perspectives/README.md): where meaning lives — an agent's graph of signed links with a verdict per link, its shape, its behaviour, and the neighbourhoods that share it by person.

Terms: [Glossary](GLOSSARY.md).

<details><summary>How it works</summary>

```
 person: 12 words ─► master key (in the person's apps, never in an executor)
   │ signs incept, add, revoke, rotate, scope
   ▼
 key log of did:scid ──► agent language ──► shared store ──► KelCache + resolver in each executor
   │ lists device keys (phone, browser, executor, host, bot)          [Agents: Identity]
   ▼
 app / bot / CLI / AI agent ── key login or token ──► executor API: RPC, MCP   [Executor interfaces]
   │ Session: operator │ tenant │ app grant │ bot grant; a guard on every method  [Access]
   ▼
 executor: sealed until its storage secret ─► keyring ─► encrypted SQLite, sealed language storage
   │ agent signs a link with its device key + anchor                           [Executor]
   ▼
 perspective: Oxigraph store, a link verdict per link, membership graph   [Perspectives]
   │ shared write: commit(diff)                 ▲ inbound: diff_from_link_language
   ▼                                            │
 link language: server link language by default ─► link-server room, admits by person  [Languages]
   ▼                                            │
 other members' executors ──────────────────────┘
```

1. The person's apps hold the master key and sign key-log events; the shared store keeps the log, and each executor reaches it only through the agent language ([Key log](agents/identity/key-log/README.md)).
2. A device joins the log from an app that holds the master key: offer, claim, match code, `add` ([Devices](agents/identity/devices/README.md)). An executor is a device: it holds device keys only.
3. A caller logs in by key with one of the person's device keys, or presents a token. The executor opens a [session](agents/access/sessions.md) that acts as one agent within a grant; the operator record decides who runs the executor ([Key login](agents/access/key-login.md)).
4. The executor opens nothing until its storage secret arrives; the keyring then unwraps each person's keys in memory ([Keyring and storage secret](agents/executor/data/keyring.md)).
5. A write signs a [link](perspectives/links/link-expressions.md) with the agent's device key and the key-log position, and stores it with a [link verdict](perspectives/links/verdicts.md) ([Link write path](perspectives/links/write-path.md)).
6. In a [neighbourhood](perspectives/neighbourhoods/README.md), shared links go to the [link language](languages/link-languages/README.md), which carries them to the other members' executors; the [membership graph](perspectives/neighbourhoods/membership/README.md) decides who counts, by person.
7. Every change emits events and updates live queries on the caller's socket ([Events](agents/interfaces/events.md), [Live queries](perspectives/querying/live-queries.md)).

Beside agents, languages and perspectives, the executor holds AI, SFU, Holochain and billing as process-wide services ([Executor services](agents/executor/services/README.md)), and the keyring and the grants store as process-wide singletons. Every perspective runs its background loops on fixed intervals ([Neighbourhood sync](perspectives/neighbourhoods/sync.md)). The default network uses central services: the language store for languages and neighbourhoods, the shared store for key logs and agent records, and one link server for new neighbourhoods; link sync can also run peer-to-peer over Holochain ([Bootstrap languages](languages/runtime/bootstrap-languages.md)). A host admits tenants by key login and keeps the email-and-password sign-up beside it ([Managed agents](agents/executor/managed-agents/README.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/](../rust-executor/): the executor: agents, sessions, grants and guards, the language runtime, perspectives, executor services, the RPC and MCP servers. Start with its [AGENTS.md](../rust-executor/AGENTS.md). New modules: [identity/](../rust-executor/src/identity/) (new) for the key-log cache, the resolver, device linking and bots; [at_rest/](../rust-executor/src/at_rest/) (new) for the keyring and encrypted stores; [hosting/](../rust-executor/src/hosting/) (new) for tenants and the hosting-index client.
- [ad4m-identity-core/](../ad4m-identity-core/) (new): the identity core: key-log events, replay, the words, signing domains, membership replay; native and WASM.
- [ad4m-hash-chain/](../ad4m-hash-chain/) (new): the generic hash-chain module under the key log.
- [identity-store/](../identity-store/) (new): the shared store's binary, behind the default agent language.
- [identity-client/](../identity-client/) (new): the identity client core, which the apps run for every step with the words, the password or the master key.
- [keyring/](../keyring/) (new): crate `ad4m-keyring`: key wrapping, subkeys and the room-key seal.
- [core/](../core/): the TypeScript SDK `@coasys/ad4m`: `Ad4mClient`, `PerspectiveProxy`, model classes, generated RPC contracts; [ad4m-hooks/](../ad4m-hooks/) holds its React and Vue hooks.
- [connect/](../connect/): `@coasys/ad4m-connect`, with Coasys Connect for web apps.
- [cli/](../cli/): the `ad4m` client and the `ad4m-executor` binary; [rust-client/](../rust-client/) is the Rust client they use.
- [bootstrap-languages/](../bootstrap-languages/): the languages that ship with ADAM (language, agent, neighbourhood, link and file languages); [ad4m-ldk/](../ad4m-ldk/) is the kit to write new ones and holds the identity wire types.
- [link-server/](../link-server/): the server behind the server link language, which admits keys by person.
- [docs-src/](../docs-src/): the user documentation and the language interface specs.
- [tests/](../tests/): integration tests against a running executor (`tests/js/`, `*.bats`).
- [ui/](../ui/): the launcher, which embeds the executor.

</details>

## Index

- [ADAM](README.md)
  - [Agents](agents/README.md)
    - [Identity](agents/identity/README.md)
      - [Key log](agents/identity/key-log/README.md)
        - [Key-log events](agents/identity/key-log/events.md)
        - [Hash-chain module](agents/identity/key-log/hash-chain.md)
        - [Key-log cache and resolver](agents/identity/key-log/cache.md)
      - [Devices](agents/identity/devices/README.md)
        - [Device linking](agents/identity/devices/linking.md)
        - [Device inbox and private record](agents/identity/devices/inbox-and-record.md)
        - [Master key](agents/identity/devices/master-key.md)
        - [Bots](agents/identity/devices/bots.md)
      - [Wallet and keys](agents/identity/wallet-and-keys.md)
      - [Signing and verification](agents/identity/signing-and-verification.md)
      - [Agent profiles](agents/identity/profiles.md)
    - [Executor](agents/executor/README.md)
      - [Executor startup](agents/executor/startup.md)
      - [Executor config](agents/executor/config.md)
      - [Executor data](agents/executor/data/README.md)
        - [Keyring and storage secret](agents/executor/data/keyring.md)
        - [Encrypted stores](agents/executor/data/encrypted-stores.md)
        - [Export and import](agents/executor/data/export-and-import.md)
      - [Managed agents](agents/executor/managed-agents/README.md)
        - [Tenants](agents/executor/managed-agents/tenants.md)
        - [Leaving a host](agents/executor/managed-agents/leaving.md)
        - [Password users](agents/executor/managed-agents/password-users.md)
      - [Executor services](agents/executor/services/README.md)
        - [AI service](agents/executor/services/ai.md)
        - [SFU](agents/executor/services/sfu.md)
        - [Holochain service](agents/executor/services/holochain.md)
        - [Billing and hosting](agents/executor/services/billing-and-hosting.md)
        - [Hosting-index client](agents/executor/services/hosting-index.md)
    - [Access](agents/access/README.md)
      - [Sessions](agents/access/sessions.md)
      - [Key login](agents/access/key-login.md)
      - [Operator and main agent](agents/access/operator-and-main-agent.md)
      - [Grants](agents/access/grants/README.md)
        - [Capabilities](agents/access/grants/capabilities.md)
        - [App approval](agents/access/grants/app-approval.md)
        - [Bot grants](agents/access/grants/bot-grants.md)
      - [Guards](agents/access/guards.md)
    - [Executor interfaces](agents/interfaces/README.md)
      - [RPC API](agents/interfaces/rpc-api.md)
      - [Events](agents/interfaces/events.md)
      - [MCP server](agents/interfaces/mcp-server.md)
      - [TypeScript SDK](agents/interfaces/typescript-sdk/README.md)
        - [ad4m-connect](agents/interfaces/typescript-sdk/ad4m-connect.md)
        - [Model classes](agents/interfaces/typescript-sdk/model-classes.md)
      - [Rust client and CLI](agents/interfaces/rust-client-and-cli.md)
  - [Languages](languages/README.md)
    - [Expressions and addresses](languages/expressions/README.md)
      - [Language addresses](languages/expressions/addresses.md)
      - [Literal expressions](languages/expressions/literals.md)
    - [Language runtime](languages/runtime/README.md)
      - [Language controller](languages/runtime/controller.md)
      - [Language isolates](languages/runtime/isolates.md)
      - [Language interface](languages/runtime/interface.md)
      - [Installing languages](languages/runtime/installing/README.md)
        - [Language trust and templates](languages/runtime/installing/trust-and-templates.md)
        - [Language language](languages/runtime/installing/language-language.md)
      - [Bootstrap languages](languages/runtime/bootstrap-languages.md)
    - [Link languages](languages/link-languages/README.md)
      - [Server link language](languages/link-languages/server-link-language/README.md)
        - [Room keys](languages/link-languages/server-link-language/room-keys.md)
        - [Link server](languages/link-languages/server-link-language/link-server.md)
        - [Link-server directory](languages/link-languages/server-link-language/link-server-directory.md)
      - [Perspective diff sync](languages/link-languages/perspective-diff-sync.md)
      - [Local link language](languages/link-languages/local-link-language.md)
    - [Other language kinds](languages/other-kinds/README.md)
      - [Agent language](languages/other-kinds/agent-language/README.md)
        - [Identity interface](languages/other-kinds/agent-language/identity-interface.md)
        - [Token interface](languages/other-kinds/agent-language/token-interface.md)
        - [Shared store](languages/other-kinds/agent-language/shared-store.md)
      - [Neighbourhood and perspective languages](languages/other-kinds/neighbourhood-and-perspective-languages.md)
      - [File storage languages](languages/other-kinds/file-storage-languages.md)
  - [Perspectives](perspectives/README.md)
    - [Links and store](perspectives/links/README.md)
      - [Links](perspectives/links/link-expressions.md)
      - [Link verdicts](perspectives/links/verdicts.md)
      - [Local and shared links](perspectives/links/local-and-shared.md)
      - [Oxigraph store](perspectives/links/oxigraph-store.md)
      - [Link write path](perspectives/links/write-path.md)
    - [Querying](perspectives/querying/README.md)
      - [SPARQL queries](perspectives/querying/sparql.md)
      - [Model queries](perspectives/querying/model-queries.md)
      - [Live queries](perspectives/querying/live-queries.md)
    - [Social DNA](perspectives/social-dna/README.md)
      - [Subject classes](perspectives/social-dna/subject-classes.md)
      - [Flows](perspectives/social-dna/flows/README.md)
        - [Flow consensus](perspectives/social-dna/flows/consensus.md)
        - [Flow spawn and guards](perspectives/social-dna/flows/spawn-and-guards.md)
        - [Flow receipts](perspectives/social-dna/flows/receipts.md)
      - [Interpretation](perspectives/social-dna/interpretation/README.md)
        - [Interpretation overlay](perspectives/social-dna/interpretation/overlay.md)
        - [Auto-processor](perspectives/social-dna/interpretation/auto-processor.md)
    - [Neighbourhoods](perspectives/neighbourhoods/README.md)
      - [Creating and joining](perspectives/neighbourhoods/creating-and-joining.md)
      - [Membership](perspectives/neighbourhoods/membership/README.md)
        - [Open joins and invites](perspectives/neighbourhoods/membership/invites.md)
        - [Guests](perspectives/neighbourhoods/membership/guests.md)
        - [Counts by person](perspectives/neighbourhoods/membership/counts-by-person.md)
      - [Neighbourhood sync](perspectives/neighbourhoods/sync.md)
      - [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md)
      - [Neighbourhood calls](perspectives/neighbourhoods/calls.md)
- [Glossary](GLOSSARY.md)
- [Conventions](CONVENTIONS.md)
