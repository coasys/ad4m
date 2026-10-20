# ADAM

ADAM is an agent-centric layer for the web. An **executor** process hosts one or more **agents**; each agent keeps its data in **perspectives**, graphs of links; and **languages**, code loaded into the executor, store, share and sync that data with other agents.

**Why:** Data belongs to the agent, not to an app or a server. Apps and other agents reach the data only through the agent's executor, and only with the agent's permission. Languages keep the transport replaceable: the same perspective can sync over Holochain, a link server, or another network.

This folder holds the mental models of this repository: a high-level expression of the code beside it, in a tree of short pages. The pages are kept 1-1 with the code. A pull request that changes a mental model changes its page in the same pull request. How the pages are written: [Conventions](CONVENTIONS.md).

## Parts
- [Agents](agents/README.md): who acts and through what — identity, the executor that hosts agents, what each caller may do, and the interfaces callers use.
- [Languages](languages/README.md): how data leaves the executor — code that stores, shares and syncs expressions.
- [Perspectives](perspectives/README.md): where meaning lives — an agent's graph of links, its shape and its behaviour.

Terms: [Glossary](GLOSSARY.md).

<details><summary>How it works</summary>

```
 app / bot / CLI / AI agent  ◄──────────── events, live-query updates ────────────┐
   │ key login │ admin credential │ app grant │ user token                         │
   ▼                                                                               │
 executor API: RPC /api/v1/ws, MCP          [Agents: Executor interfaces, Access]  │
   │ Session → principal + grant; a guard on every method                          │
   ▼                                                                               │
 agent: AgentContext (main │ managed) ─ device key signs   [Agents: Identity]      │
   │ the person's key log maps the device key to the person (did:scid)             │
   ▼                                                                               │
 perspective: PerspectiveInstance + Oxigraph store          [Perspectives] ────────┘
   │ shared write: commit(diff)                 ▲ inbound: diff_from_link_language
   ▼                                            │
 link language (Deno isolate)                   │           [Languages]
   │ link server │ Holochain DHT │ local        │
   ▼                                            │
 network ───────► other executors ──────────────┘
```

1. A caller opens one connection with a key login or a token. The executor turns it into a [session](agents/access/sessions.md) that acts as one agent within a grant.
2. A write signs a [link](perspectives/links/link-expressions.md) with that agent's device key and stores it in the perspective ([Link write path](perspectives/links/write-path.md)). Any executor maps the device key to its person through the person's [key log](agents/identity/key-log/README.md).
3. In a [neighbourhood](perspectives/neighbourhoods/README.md), shared links go to the [link language](languages/link-languages/README.md) (the server link language by default), which carries them to the other members' executors. Their diffs come back the same way. [Membership](perspectives/neighbourhoods/membership/README.md) admits members by person.
4. Every change emits events and updates live queries on the caller's socket ([Events](agents/interfaces/events.md), [Live queries](perspectives/querying/live-queries.md)).

Beside agents, languages and perspectives, the executor holds AI, SFU, Holochain and billing as process-wide services ([Executor services](agents/executor/services/README.md)). Every perspective runs background loops: some wake on events, the others poll on fixed intervals ([Neighbourhood sync](perspectives/neighbourhoods/sync.md)). In the default network, languages and neighbourhoods are published to central HTTP services, and key logs and agent records to the shared store; link sync can also run peer-to-peer over Holochain ([Bootstrap languages](languages/runtime/bootstrap-languages.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/](../rust-executor/): the executor: agents, sessions, the language runtime, perspectives, executor services, the RPC and MCP servers. Start with its [AGENTS.md](../rust-executor/AGENTS.md).
- [ad4m-identity-core/](../ad4m-identity-core/) (new), [ad4m-hash-chain/](../ad4m-hash-chain/) (new), [identity-store/](../identity-store/) (new), [identity-client/](../identity-client/) (new), [keyring/](../keyring/) (new): the identity crates; see [Identity](agents/identity/README.md) and [Executor data](agents/executor/data/README.md).
- [core/](../core/): the TypeScript SDK `@coasys/ad4m`: `Ad4mClient`, `PerspectiveProxy`, model classes, generated RPC contracts; [ad4m-hooks/](../ad4m-hooks/) holds its React and Vue hooks.
- [connect/](../connect/): `@coasys/ad4m-connect`, which finds an executor and gets an app its token.
- [cli/](../cli/): the `ad4m` client and the `ad4m-executor` binary; [rust-client/](../rust-client/) is the Rust client they use.
- [bootstrap-languages/](../bootstrap-languages/): the languages that ship with ADAM (language, agent, neighbourhood, link and file languages); [ad4m-ldk/](../ad4m-ldk/) is the kit to write new ones.
- [link-server/](../link-server/): the self-hosted server behind the server link language.
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
