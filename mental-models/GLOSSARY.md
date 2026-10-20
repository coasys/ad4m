# Glossary

One definition per term, in alphabetical order. Each entry gives the code name in brackets and the page that explains the term. "not: …" lists words that pages must not use for this concept.

**12 words** (`words` feature of the identity core): The BIP-39 phrase that makes every master key and the private record's recovery key; no executor sees it. [Key-log events](agents/identity/key-log/events.md)

**Abstract class** (`model_query/abstract_classes.rs`, built-in `ad4m://TextContent`): A subject class that reads the instances of several concrete classes, sends each write to the record's own classes, and cannot be created. [Subject classes](perspectives/social-dna/subject-classes.md)

**ACL** (`acl` table in `link-server/src/db.ts`): The list of DIDs admitted to one legacy link-server room, checked on every request; rooms with a creator binding admit from the membership graph instead. [Link server](languages/link-languages/server-link-language/link-server.md)

**Action** (`AD4MAction`, `Command`): A JSON list of commands stored on a subject class for its constructor, its destructor, or a property's setter, adder or remover. [Subject classes](perspectives/social-dna/subject-classes.md)

**ad4m-connect** (`@coasys/ad4m-connect`, `connect/`): The browser and Node library that finds an executor, runs a login flow (Coasys Connect by default) and gives an app an authenticated `Ad4mClient`. [ad4m-connect](agents/interfaces/typescript-sdk/ad4m-connect.md)

**ad4m:host** (`rust-executor/src/js_core/host.js`): The ES module that every language imports to reach the executor's ops: agent, Holochain, HTTP, hash, language context, emit, membership and storage functions. [Language isolates](languages/runtime/isolates.md)

**Admin credential** (`Ad4mConfig.admin_credential: Option<Secret>`, `OperatorSource::Credential`): The secret that gives the operator role on the executor's own machine; the desktop shell keeps it in its main process and no web view receives it; with the loopback caller, it alone reaches local-only methods. [Operator and main agent](agents/access/operator-and-main-agent.md)

**Admitted** (`admitted` in the link server's auth answer): The state of a key that the link server's replay of a room's membership graph names as a member's or guest's key; only an admitted key may commit, sync or fetch room keys. [Link server](languages/link-languages/server-link-language/link-server.md)

**Agent** (`AgentContext`): A device key (`did:key`) in an executor's wallet on whose behalf the executor signs and stores data; it may belong to an identity. [Agents](agents/README.md) — not: user, person (a person is an identity)

**Agent context** (`AgentContext::{Main, Managed { did }}`): The value that names which agent a call acts as; the language runtime holds it in a thread-local while a call runs. [Identity](agents/identity/README.md)

**Agent language** (`LanguageController::get_agent_language`, `bootstrap-languages/centralized-agent-language/`): The system language behind the `did` alias: it resolves a `did:key` or `did:scid` to its agent record and carries the executor's identity calls to the shared store; it signs nothing and holds no key. [Agent language](languages/other-kinds/agent-language/README.md)

**Agent passphrase** (`derive_agent_passphrase`, tag `ad4m:agent-passphrase:v1`): The wallet's and Holochain's passphrase, derived from the storage secret at each start and never typed or stored. [Wallet and keys](agents/identity/wallet-and-keys.md)

**Agent profile** (`Agent` in `types/domain.rs`): The public record of one agent: its DID, its identity (`did:scid` or `null`), its public perspective and its direct-message language address. [Agent profiles](agents/identity/profiles.md) — not: public profile

**Agent record** (`AgentRecord` in `ad4m-ldk`, `/v1/agents/{did}`): The signed form of an agent profile in the shared store, with a version: under a `did:scid`, signed by a device key of that identity with an anchor; under a `did:key`, signed by that key. [Agent profiles](agents/identity/profiles.md) — not: profile server

**AI service** (`AIService`, `rust-executor/src/ai_service/`): The executor service that runs local and remote LLMs, embedding models, transcription and AI tasks. [AI service](agents/executor/services/ai.md)

**AI task** (`tasks` table, `AIService::add_task`): A stored system prompt with examples, bound to one model. [AI service](agents/executor/services/ai.md)

**ALDK** (`ad4m-ldk/`): The AD4M Language Development Kit, for writing languages in TypeScript (`defineLanguage`) or in Rust compiled to WASM (`ad4m_language!`); it also holds the identity wire types. [Language interface](languages/runtime/interface.md)

**Alias** (`LanguageController::language_aliases`): One of the names `did`, `lang`, `neighbourhood` and `perspective` that stand for the address of a system language. [Language addresses](languages/expressions/addresses.md)

**Anchor** (`Anchor` on `ExpressionProof`): The key-log position (identity, key id, sequence number, head hash) at which a device key signed a link; the signature covers it. [Signing and verification](agents/identity/signing-and-verification.md)

**Anonymous caller** (`Principal::Anonymous`, `anonymous_capabilities`): A session opened without a token, which may request a capability, ask for a session challenge and log in by key, and, in multi-user mode, sign up or log in by password. [Sessions](agents/access/sessions.md)

**App approval** (`agent.requestCapability`, `agent.permitCapability`, `agent.generateJwt`): The flow in which an app asks for capabilities on a target agent, that agent's owner approves a subset and gets a six-digit code, and the app redeems the code for a token. [App approval](agents/access/grants/app-approval.md)

**App data path** (`Ad4mConfig.app_data_path`, default `~/.ad4m`): The directory that holds all state of one executor. [Executor data](agents/executor/data/README.md)

**App grant**: A grant that app approval mints on its target agent, with the approved capabilities and a 180-day life. [App approval](agents/access/grants/app-approval.md) — not: cap token

**Audience** (`Audience::{Everyone, Dids}` in `events.rs`): The set of agents whose sessions may receive one event, named by its publisher. [Events](agents/interfaces/events.md)

**Auth domain** (`coasys.id`, outside this repository): The site whose Connect frame holds a browser device key and signs key logins for web apps; it never holds a token for an executor. [ad4m-connect](agents/interfaces/typescript-sdk/ad4m-connect.md)

**Author (of a link)** (`LinkExpression.author`): The `did:key` of the device key that signed the link; `author_of` maps it to the person. [Links](perspectives/links/link-expressions.md) — not: the person's `did:scid`

**`author_of`** (`identity/resolver.rs`): The resolver call that maps a device key to `Person`, `Bot`, `NoLog` or `Unavailable`. [Link verdicts](perspectives/links/verdicts.md)

**Auto-processor** (`AutoProcessorConfig`, `auto_processor_supervisor`): A declared instance in a perspective that makes the executor run interpretation by itself on new messages in a scope. [Auto-processor](perspectives/social-dna/interpretation/auto-processor.md)

**Base** (`FlowInstance.subject`, `expression_address` in `create_subject`): The URI that an instance, an interpretation overlay or a flow instance is about. [Flows](perspectives/social-dna/flows/README.md) — not: base expression, base URI

**Batch** (`create_batch`, `commit_batch`, `batch_store`): An in-memory set of link changes that the executor stores and commits as one diff. [Link write path](perspectives/links/write-path.md)

**Billing** (`rust-executor/src/billing.rs`): The pricing of managed agents' link writes and AI calls by the host rates, logged in `compute_log` and charged only under `Charge`. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**Bootstrap languages** (`bootstrap-languages/`): The language packages that ship in the repository and that bootstrap seeds point at. [Bootstrap languages](languages/runtime/bootstrap-languages.md)

**Bootstrap seed** (`BootstrapSeed`, `mainnet_seed.json`): The JSON file that gives an executor its language-language bundle, the system language addresses, the known link languages, the seed's trusted agents, the shared store's URL and head keys (`identity`) and the default link servers (`linkServers`). [Bootstrap languages](languages/runtime/bootstrap-languages.md) — not: network seed (that is the Holochain DNA `network_seed`)

**Bot** (`KeyLife::Bot`, `BotScope`): A device key in its owner's key log with the bot mark, its allowed actions (`read`, `write`, `join`) and an expiry; it never counts as the person in votes. [Bots](agents/identity/devices/bots.md)

**Bot activity log** (`BotActivity`, `bot_activity` table, `bots.activity`): Each call of a bot on the executor that runs it, with its reason and outcome, refusals included. [Bots](agents/identity/devices/bots.md)

**Bot agent**: An agent on an executor whose device key the resolver answers with the bot mark; its sessions are limited by its bot grant. [Bot grants](agents/access/grants/bot-grants.md)

**Bot grant** (`BotGrant`, `SignedBotGrant`, `bot.setGrant`): A statement signed by the owner's current master key that names a bot key, the executor that runs it, at most 100 neighbourhoods (the field `spaces`), the actions (`read`, `write`, `join`) and an expiry, 30 days by default. [Bot grants](agents/access/grants/bot-grants.md)

**Bot mark** (`kind: "bot"`, `can`, `expires` on a device key in a key log): The only device attribute that enters a key log; it makes the key a bot's, which never counts as the person. [Bots](agents/identity/devices/bots.md)

**Brake** (`pow` module of the identity core, `GET /v1/pow`): The shared store's limits on new key logs that arrive with no vouching note: rate limits and a proof of work bound to the SCID. [Shared store](languages/other-kinds/agent-language/shared-store.md)

**Broadcast** (`PerspectiveInstance::send_broadcast`, `neighbourhood.sendBroadcast`): An ephemeral signed payload sent to all agents of a neighbourhood. [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md)

**Browser device key**: A non-extractable 30-day device key that the auth domain's Connect frame makes for one browser and one embedding site, and with which it signs web key logins. [ad4m-connect](agents/interfaces/typescript-sdk/ad4m-connect.md)

**Call** (SDK `Session` in `core/src/neighbourhood/Session.ts`, `createSession`): An audio and video connection among members of a neighbourhood, over mesh or the SFU. [Neighbourhood calls](perspectives/neighbourhoods/calls.md) — not: session (a session is the auth `Session`)

**Call policy** (`CallPolicy`, `SfuConfig`, link predicate `ad4m://sfu_config`): A neighbourhood's call mode (`mesh`, `designated`, `gateway`, `cascaded`) and mesh size, stored as a link authored by the neighbourhood's creator. [SFU](agents/executor/services/sfu.md)

**Call topology** (`SessionTopology`: `mesh`, `sfu`, `auto`): The client-side choice of how one call connects: directly between participants, or through an SFU. [Neighbourhood calls](perspectives/neighbourhoods/calls.md)

**Capability** (`Capability { with: Resource, can }` in `agent/capabilities/types.rs`): A domain, a list of pointers and a list of verbs that together name a kind of action a caller may take. [Capabilities](agents/access/grants/capabilities.md). For languages, see Language capability.

**Capability request** (`agent.requestCapability { authInfo, agent? }`, `PendingRequest`): An app's request for capabilities on one target agent, held in memory with a short life until its owner approves it and the app redeems the code. [App approval](agents/access/grants/app-approval.md)

**Cascade** (`rust-executor/src/sfu/cascade.rs`, `sfu/gossip/`): Several executors' SFUs that share load and pipe media between them. [SFU](agents/executor/services/sfu.md)

**Charge mode** (`ChargeMode { Free, Charge }`, config `billing.charge_mode`): The setting that decides whether anyone pays; under `Free` operations are priced and logged at amount 0. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**Claim** (`Claim`, `ClaimService::{claim_for_main, claim_for_tenant}`): A new device's answer to an offer, with its keys and a co-signature; on an executor, the claim made with one of its wallet keys. [Device linking](agents/identity/devices/linking.md)

**Client kind** (`ClientKind::{Native, Web, Local, Guest}`): What kind of app a grant was minted for: a native shell, a web origin (with its first-party flag), the desktop's local session, or a guest. [Sessions](agents/access/sessions.md)

**Co-owner** (`PerspectiveHandle.owners`): One of several agents on one executor who joined the same neighbourhood and so share one perspective; added only after the membrane admits their person. [Creating and joining](perspectives/neighbourhoods/creating-and-joining.md)

**Coasys Connect** (`@coasys/ad4m-connect`, `ConnectRequest`, `user.redeemCode`): The route by which a web app gets a key session: the auth domain's frame signs a key login with PKCE, and the app redeems a one-time code at the executor. [ad4m-connect](agents/interfaces/typescript-sdk/ad4m-connect.md)

**Compute credits** (`get_agent_credits`, `deduct_credits_and_log`): The balance that a managed agent spends on billed work, stored on its `users` row. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**Conductor** (`HolochainService`): The embedded Holochain runtime inside the executor. [Holochain service](agents/executor/services/holochain.md)

**Consensus rule** (`ConsensusRule` in `shacl_parser.rs`): The number of distinct DIDs (device keys), optionally holding a role, that must sign a flow transition before it settles. [Flows](perspectives/social-dna/flows/README.md)

**Count by person** (`neighbourhood.countByPerson`, `PersonCount`): A read that counts each member person once, from their latest valid link, and never counts bots, guests or device-only identities. [Counts by person](perspectives/neighbourhoods/membership/counts-by-person.md)

**Creating (a neighbourhood)** (`neighbourhood.create`): Turning a perspective into a neighbourhood in one call that picks, fills and fixes its link language and writes the membership genesis. [Creating and joining](perspectives/neighbourhoods/creating-and-joining.md)

**Crypto-shredding** (`DataKeys::destroy_person`): Destroying a person's data key, so that their encrypted rows and language storage on that executor become unreadable; plain Oxigraph store files are deleted, not shredded. [Encrypted stores](agents/executor/data/encrypted-stores.md)

**Custody channel** (`--custody-channel`, `identity.takeMaster`): The run flag with which the desktop shell starts its executor, so that only that executor may hand a received master key to the custody service. [Master key](agents/identity/devices/master-key.md)

**Custody service** (outside this repository): The code in the desktop and phone shells, apart from every web view and the executor, that keeps and uses the master key. [Master key](agents/identity/devices/master-key.md)

**Data key** (person data key, `PersonEntry`, `KeyOwner::{Main, User}`): One versioned key per person on an executor, wrapped under the root key, that wraps that person's store keys. [Keyring and storage secret](agents/executor/data/keyring.md)

**Delta update** (`result_delta`, `send_delta_update`): The change of a live query's result since its previous revision, sent to the subscribing connection. [Live queries](perspectives/querying/live-queries.md)

**Derived state** (`DerivedState`): A flow instance's state as the fold computes it from the links present now. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Device**: Anything that signs as a person and holds its own device key: a phone, a computer, a browser, a server, a host, a bot. [Devices](agents/identity/devices/README.md)

**Device inbox** (`InboxAddress::{Pre, Joined}`, `identity.inbox`): The one mailbox per device in the shared store for sealed messages between a person's devices. [Device inbox and private record](agents/identity/devices/inbox-and-record.md)

**Device key** (wallet key of an agent, `DeviceKey` in a key log): A key that one device holds and that the person's key log lists; on an executor, a wallet key. It signs links, claims and logins. [Identity](agents/identity/README.md) — the key that an entanglement proof links (`sign_device_key`) is a different thing

**Device-only identity** (`Author::NoLog`): An agent whose device key no key log lists, such as a guest; its DID is its `did:key`, its links carry no anchor, and it cannot create a neighbourhood, join one as a member or count in a vote. [Key-log cache and resolver](agents/identity/key-log/cache.md)

**DID** (`did_for_context`): The identifier of an agent, always the `did:key` of its device key; a person's identifier is a `did:scid`. [Identity](agents/identity/README.md)

**`did:scid`** (`did:scid:coasys:1:<scid>`): The identifier of a person; the SCID is the hash of the key log's first event. [Key log](agents/identity/key-log/README.md)

**Direct-message language** (`Agent.direct_message_language`): The language address in an agent profile through which other agents would send that agent messages; direct messages are not implemented ([Executor services](agents/executor/services/README.md)). [Agent profiles](agents/identity/profiles.md)

**Display read**: A read that hides only `Invalid` links and carries each link's verdict. [Link verdicts](perspectives/links/verdicts.md)

**Embedded mode** (ad4m-connect, `AD4M_CONFIG` message): The ad4m-connect mode in which a host page passes the executor URL and token to an app by `postMessage`. [ad4m-connect](agents/interfaces/typescript-sdk/ad4m-connect.md)

**Encryption key** (`enc`, `deviceEnc`, `WalletBackend::open_sealed`): The X25519 key that goes with a device key; for an executor, derived from the Ed25519 wallet key. Messages and the master key are sealed to it. [Wallet and keys](agents/identity/wallet-and-keys.md)

**Entanglement proof** (`EntanglementProof`, `entanglement_service`): Signatures in both directions that link the main agent's DID to a key outside the key log, such as an Ethereum address or a Holochain agent key. [Signing and verification](agents/identity/signing-and-verification.md)

**Event** (`Event { topic, audience, capability, perspective, payload }` in `events.rs`): A typed message that the executor pushes on a socket whose session is in its audience, holds its capability and asked for its topic with `events.watch`. [Events](agents/interfaces/events.md) — not: subscription (that is a live query), notification

**Event interest** (`events.watch`, `events.unwatch`, `EventInterest` in `api/event_interest.rs`): The topics, each optionally narrowed to some perspectives, that one socket asked for; a socket with no interest gets no events. [Events](agents/interfaces/events.md)

**Evidence seal** (`recompute_evidence_seal`): A hash over the content that a transition's flow guard cited, written by the proposer and checked again by each voter. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Executor** (crate `ad4m-executor`, `rust_executor::run`): The ADAM process that hosts agents, perspectives, language runtimes, executor services and the APIs; it is a device of its operator. [Executor](agents/executor/README.md) — not: node, ADAM node, conductor (that is Holochain's)

**Executor service** (`ai_service/`, `sfu/`, `holochain_service/`, `billing.rs`): A Rust module inside the executor that is not a language, such as AI, SFU, Holochain, billing, the hosting-index client, email or the runtime service. [Executor services](agents/executor/services/README.md)

**Executor store key** (`SK(Executor)`): The key, wrapped under the root key, from which the SQLCipher key, the system languages' storage key, the archive key and the seal of executor secret files derive. [Keyring and storage secret](agents/executor/data/keyring.md)

**Expression** (`Expression<T>`, `create_signed_expression`): A piece of data signed by an agent's device key: `{author, timestamp, data, proof}`. [Expressions and addresses](languages/expressions/README.md)

**Expression language** (`Capability::ExpressionGet`, `ExpressionCreate`): A language that exports `expressionGet` and usually `expressionCreate`, and stores one kind of signed data. [Other language kinds](languages/other-kinds/README.md)

**Expression proof** (`ExpressionProof`): The key, the signature and, for a key in a key log, the anchor of an expression; the reading executor stores a link verdict for a link and `valid` or `invalid` for other expressions. [Signing and verification](agents/identity/signing-and-verification.md)

**Expression URL** (`ExpressionRef::try_from`, `parse_expr_url`): `<language address or alias>://<expression address>`, a DID, or a `literal:` URL. [Expressions and addresses](languages/expressions/README.md)

**File storage language** (`bootstrap-languages/file-storage/`, `centralized-file-storage/`): An expression language whose expressions are files. [File storage languages](languages/other-kinds/file-storage-languages.md)

**First-link rule** (`OperatorRecord::admits_first_link`): A claim for an executor's main agent passes only when the offer's identity equals the operator record. [Device linking](agents/identity/devices/linking.md)

**Flow** (`SHACLFlow`): A state machine stored as social DNA: states, transitions, flow guards and a consensus rule. [Flows](perspectives/social-dna/flows/README.md)

**Flow evaluator** (`perspectives/flow_evaluator.rs`): The pass after interpretation that checks the flow guards of reachable states and writes a transition proposal for each flow guard that now holds. [Flow spawn and guards](perspectives/social-dna/flows/spawn-and-guards.md)

**Flow guard** (`requires` on `FlowState`): A model query on a flow state that must match before a transition into that state may be proposed. [Flow spawn and guards](perspectives/social-dna/flows/spawn-and-guards.md) — not: guard (alone), which is the RPC access rule

**Flow instance** (`FlowInstance`): One run of a flow bound to one base. [Flows](perspectives/social-dna/flows/README.md)

**Flow receipt** (`FlowReceipt`): A content-addressed record of a finished flow run that any executor can check by running the fold again. [Flow receipts](perspectives/social-dna/flows/receipts.md)

**Flow spawn** (`run_flow_spawn_pass`): The automatic start of flow instances on instances that an interpretation run created. [Flow spawn and guards](perspectives/social-dna/flows/spawn-and-guards.md)

**Fold** (`fold`, `fold_read_set`): The pure function that walks a flow from its first state over the read set and returns the derived state. [Flow consensus](perspectives/social-dna/flows/consensus.md) — the hash-chain module's `fold`, which replays a key log, is a different function ([Hash-chain module](agents/identity/key-log/hash-chain.md))

**Fork** (`ForkEvidence`, event `identity-fork`): Two different valid entries at one position of a key log, or a shorter log than one already held; detected and recorded, never resolved. [Hash-chain module](agents/identity/key-log/hash-chain.md)

**Free hosting** (`free_hosting_enabled`): The runtime form of the charge mode `Free`, seeded from `billing.charge_mode`. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**Grant** (`LiveGrant` in `agent/grants.rs`, table `grants`): The stored permission behind one token: its agent, capabilities, app, role, client kind, optional key binding, state and expiry; a token opens a session only while its grant is active. [Grants](agents/access/grants/README.md)

**Grants store** (`Grants`): The executor's one list of grants, in memory and in the `grants` table; it is the allow list for every token. [Sessions](agents/access/sessions.md) — not: apps registry

**Guard** (`Guard::{Open, Operator, Cap, Executor, Owns}` in `api/guards.rs`): The access rule that each RPC method declares where it registers, and that the dispatcher runs before the handler; MCP tools run the same guards. [Guards](agents/access/guards.md) — not: access check. For flows, see Flow guard.

**Guest** (`UserKind::Guest`, `GuestRecord`, `guest.create`): A web visitor held as a managed agent with only a device key and a guest grant, on a host whose sign-up setting is "anyone"; it enters a neighbourhood with a guest invite and is named from its key. [Guests](perspectives/neighbourhoods/membership/guests.md)

**Guest grant** (`guest.create`, `ClientKind::Guest`): A grant on a guest's managed agent with no key binding and no expiry; the web app keeps its token in `localStorage`. [Sessions](agents/access/sessions.md)

**Hard-wired class** (`hardwired_class.rs`, `hardwired_sdna/`): A subject class that the executor registers itself from bundled JSON, such as `FlowInstance`, `InterpretationRun` or `AutoProcessor`. [Social DNA](perspectives/social-dna/README.md) — not: built-in class (except for the abstract `ad4m://TextContent`)

**Harness** (`ai_service/harness/`, `run_with_tools`): The tool-calling LLM loop that interpretation can use instead of a single prompt. [Interpretation](perspectives/social-dna/interpretation/README.md)

**Hash-chain module** (`ad4m-hash-chain`, `ChainPolicy`, `ChainStore`): The generic library of entry hashes, links, proofs, fold, import and fork evidence under the key log. [Hash-chain module](agents/identity/key-log/hash-chain.md)

**Held tenant** (`TenantState::Held`): A tenant whose host keeps its data after a guest's "Keep this" but does not admit it: it can read, export and leave, not write. [Tenants](agents/executor/managed-agents/tenants.md)

**Holder**: Any app that holds the current master key. [Devices](agents/identity/devices/README.md)

**Holochain service** (`HolochainService`, `HolochainServiceInterface`): The executor service that runs the conductor behind one actor channel. [Holochain service](agents/executor/services/holochain.md)

**Home-network listener** (`rust-executor/src/lan_listener.rs`, config `lan.*`): The desktop executor's second RPC listener, on its private network addresses under a self-signed certificate, which admits key logins only and marks every request `ListenerReach::Network`. [RPC API](agents/interfaces/rpc-api.md) — not: LAN listener

**Host** (multi-user executor, `HostPolicy`): An executor that serves tenants, people who do not run it. [Tenants](agents/executor/managed-agents/tenants.md)

**Host rates** (`host_rates` table, `bill_ai_operation`): The prices in HOT per billed operation or model name. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**Hosted mode** (`wallet_backend = "shared"`, `db_backend = "shared"`, `SharedWallet`, `SharedDb`): An executor that reads its wallet and its user table from a hosting service over HTTP. [Executor data](agents/executor/data/README.md) — not: shared mode

**Hosting** (`api/hosting_ws.rs`): The executor's offer to managed agents: host rates, a hot wallet, credit top-ups and the guest list. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**Hosting index** (`hosting/index_client.rs`, `index.*`): Coasys's private list of approved hosts; the executor signs in to it by device key, publishes its listing and requests membrane proofs. [Hosting-index client](agents/executor/services/hosting-index.md)

**Hosting service** (`wallet_backend_url`, `INTERNAL_API_TOKEN`): The external service that serves keys and users to executors in hosted mode and calls their `/health` and `/internal/shutdown` routes. [Executor data](agents/executor/data/README.md) — not: hosting index

**Identity** (`AgentService.scid`, `Agent.identity`): A person's `did:scid` with its key log; the field `identity` on an agent profile carries it. [Identity](agents/identity/README.md)

**Identity client core** (`ad4m-identity-client`, `@coasys/identity-client`): The library that runs every flow touching the words, the password or the master key, over `Vault`, `StoreClient`, `AccountClient` and `Prompt`. [Master key](agents/identity/devices/master-key.md)

**Identity core** (`ad4m-identity-core`): The Rust library with no I/O that builds, signs, checks and replays key-log events, replays membership graphs and derives master keys from the words; native and WASM. [Key log](agents/identity/key-log/README.md)

**Identity interface** (`IdentitySpec`, `IdentityCapability`, `Capability::Identity*`): The `identity*` calls that an agent language exports (key logs, heads, the key index, notes, agent records, offers, inboxes, records, requests, pauses), which the executor makes only on the agent language. [Identity interface](languages/other-kinds/agent-language/identity-interface.md)

**In-process session** (`Session::in_process`): A session with no token and no grant that the executor opens for its own callers, such as an interpretation pass with no caller; it is never the operator. [Sessions](agents/access/sessions.md)

**Installing (a language)** (`language_by_ref`, `install_language`, `install_language_from_address`): Getting a bundle by address, checking its hash, saving it and loading it into an isolate. [Installing languages](languages/runtime/installing/README.md)

**Instance** (`create_subject`, `update_subject`, MCP `instance_*` tools): A base whose links satisfy a subject class's shape; in full, a subject instance. [Subject classes](perspectives/social-dna/subject-classes.md) — not: record, item, subject (alone)

**Interaction** (`expression.interactions`, `expression.interact`, the language export `interactions`): A named action that a language offers on one of its expressions and that a caller can run through the executor. [Expressions and addresses](languages/expressions/README.md)

**Interpretation** (`run_interpretation`): An executor pass that sends a transcript and the perspective's subject classes to an LLM and writes what it extracts as instances. [Interpretation](perspectives/social-dna/interpretation/README.md)

**Interpretation overlay** (`InterpretationOverlay`): A second instance on the same base that records the LLM's inferred values and the run that wrote them, so a person can accept or reject them. [Interpretation overlay](perspectives/social-dna/interpretation/overlay.md)

**Interpretation run** (`InterpretationRun`): The record of one run of interpretation: model, prompt version, time and sources. [Interpretation](perspectives/social-dna/interpretation/README.md)

**Invite** (`membership:invite`, `neighbourhood.createInvite`): A public invite key with a role, expiry and use count in the membership graph; its secret travels in the share link's fragment. [Open joins and invites](perspectives/neighbourhoods/membership/invites.md) — a host invite (`host.createInvite`) is a different thing ([Tenants](agents/executor/managed-agents/tenants.md))

**Invite proof** (`invite: { pub, sig }`): The invite key's signature on a self-join link, without which the link does not count in an invite-only neighbourhood. [Open joins and invites](perspectives/neighbourhoods/membership/invites.md)

**Join gate** (`host.join`): The ordered checks a host runs before it makes a tenant: multi-user mode, rate limit, known identity, one record per person, sign-up setting and invite, capacity, vouching note, offer. [Tenants](agents/executor/managed-agents/tenants.md)

**Join mode** (`membership:join`, `Join::{Open, Invite}`): A neighbourhood's rule for newcomers: open to anyone with the link who maps to a person (the default), or invites only. [Open joins and invites](perspectives/neighbourhoods/membership/invites.md)

**JWT signing key** (`signing_key_name()`): The wallet key whose secret signs and checks JWTs: `main` on a local wallet, `platform` in hosted mode. [Wallet and keys](agents/identity/wallet-and-keys.md)

**"Keep this"**: A guest's step that makes its device key the first device key of a new identity, so its links show under the new person. [Guests](perspectives/neighbourhoods/membership/guests.md)

**KelCache** (`identity/cache.rs`): The executor's verified copy of key logs, which accepts only events that continue its head. [Key-log cache and resolver](agents/identity/key-log/cache.md)

**Key binding** (`KeyBinding { scid, key, key_id, head_seq, checked_at }`): The part of a grant that records the identity and the device key of the key login that made it; a change in that key's verdict ends the grant. [Key login](agents/access/key-login.md)

**Key index** (`KeyIndexEntry`, `GET /v1/keys/{multikey}`): The shared store's map from any key to the identity that holds it and whether the key is still valid. [Shared store](languages/other-kinds/agent-language/shared-store.md)

**Key log** (KEL, `KelChain`, `KeyState`): The signed, hash-chained list of an identity's key events: `incept`, `add`, `revoke`, `rotate`, `scope`. [Key log](agents/identity/key-log/README.md) — not: KEL (in prose)

**Key login** (`user.sessionChallenge`, `user.loginWithKey`, `KeyStatement`): A login on any executor with a statement signed by one of the person's device keys, checked against the key index and mapped to the operator, tenant or app role. [Key login](agents/access/key-login.md)

**Key session**: A session from a key login: a key-bound grant that lives 1 hour and renews with `user.renewSession` and a fresh signed statement. [Key login](agents/access/key-login.md)

**Keyring** (`keyring.json`, `DataKeys`): The executor's file of wrapped keys and the service that unwraps them in memory at `storage.unlock`. [Keyring and storage secret](agents/executor/data/keyring.md)

**Keystore** (`AgentStore.keystore`, `KeystoreEnvelope`): The encrypted form of all wallet keys inside `agent.json`, under the agent passphrase. [Wallet and keys](agents/identity/wallet-and-keys.md)

**Known link languages** (`RuntimeService::get_know_link_languages`): The link language addresses from the bootstrap seed that the executor loads at start. [Bootstrap languages](languages/runtime/bootstrap-languages.md)

**Language** (`rust-executor/src/languages/`): A JavaScript bundle, loaded into its own isolate, that does some of these, one per language capability it exports: create and fetch expressions; commit, sync, render and query links; list peers; carry telepresence. [Languages](languages/README.md)

**Language address** (`calculate_language_hash`, op `hash`): `"Qm"` followed by the base58btc multibase string of a CIDv1 (codec `0x00`) over the SHA-256 of the bundle text. [Language addresses](languages/expressions/addresses.md)

**Language capability** (`Capability` in `languages/capability.rs`): One function group that a language exports, detected once at load; the presence of the export is the capability. The identity and token groups are called only on the agent language. [Language interface](languages/runtime/interface.md) — not: capability (alone), which means the access capability

**Language context** (`LanguageContext`): The data a language gets at load: main agent DID, signing key id, settings, storage directory and address. [Language runtime](languages/runtime/README.md)

**Language controller** (`LanguageController`): The global registry of loaded languages and the only way Rust code calls into a language. [Language controller](languages/runtime/controller.md)

**Language isolate** (`JsCore::new_for_language`, `LanguageRuntimeHandle`): One Deno worker, on its own OS thread, that runs one language bundle in a sandbox. [Language isolates](languages/runtime/isolates.md) — not: language instance

**Language language** (`bootstrap-languages/language-language/`, alias `lang`): The system language that stores language bundles and their language meta. [Language language](languages/runtime/installing/language-language.md)

**Language meta** (`LanguageMeta`): The signed record that the language language holds for a bundle: name, address, author and template fields. [Language language](languages/runtime/installing/language-language.md)

**Language runtime** (`rust-executor/src/languages/`, `rust-executor/src/js_core/`): The part of the executor that loads and runs languages. [Language runtime](languages/runtime/README.md)

**Language store** (`PROXY_URL` in `bootstrap-languages/language-language/index.ts`): The central Cloudflare Worker behind the default language, neighbourhood and centralised file-storage languages; it refuses to overwrite a key and has no delete route. [Language language](languages/runtime/installing/language-language.md)

**Launcher** (`ui/src-tauri`): The desktop application in this repository that embeds the executor in its own process, holds the admin credential and shows the operator's UI. [Executor](agents/executor/README.md)

**Legacy room** (link server): A link-server room made before protocol 2, with no creator binding, that keeps the ACL rules. [Link server](languages/link-languages/server-link-language/link-server.md)

**Link** (`Link` in `types/core.rs`): A triple of source, optional predicate and target. [Links](perspectives/links/link-expressions.md)

**Link expression** (`LinkExpression`, `DecoratedLinkExpression`): A link signed by a device key, with a timestamp, a proof (with an anchor when the key sits in a log), a link verdict and a link status. [Links](perspectives/links/link-expressions.md)

**Link language** (`Language::commit`, `Language::sync`): A language that holds and syncs the shared links of one neighbourhood and carries its telepresence. [Link languages](languages/link-languages/README.md)

**Link-language choice** (`LinkLanguageChoice`, `neighbourhood.linkLanguageChoice`): The record of a neighbourhood's link language, server URL, room id and creator, written once at creation or join and never changed. [Creating and joining](perspectives/neighbourhoods/creating-and-joining.md)

**Link server** (`link-server/`): A Node.js server that stores and relays the encrypted links of link-server rooms, replays each room's membership graph and admits keys by person. [Link server](languages/link-languages/server-link-language/link-server.md)

**Link-server directory** (`LinkServerDirectory` in `rust-executor/src/link_servers.rs`): The executor's list of link servers for new neighbourhoods, from three sources in order: operator, platform, seed. [Link-server directory](languages/link-languages/server-link-language/link-server-directory.md)

**Link-server operator** (`link-server/src/operator.ts`): Whoever runs a link server; its loopback listener reads rooms and may suspend and resume one, and admits or removes no one. [Link server](languages/link-languages/server-link-language/link-server.md) — not: operator (alone), which is the executor's operator

**Link-server protocol 2** (`GET /.well-known/coasys-link-server`): The link-server version with membership admission, required ownership signatures and any-member key grants; the executor accepts a link server for new neighbourhoods only at protocol 2 or later. [Link server](languages/link-languages/server-link-language/link-server.md)

**Link-server room** (`/rooms/:roomId/*`): One neighbourhood's link store on a link server, with id `base58(sha256(CREATOR ‖ 0x00 ‖ UID))`, created by its creator's genesis and admitting keys from the membership graph. [Server link language](languages/link-languages/server-link-language/README.md) — not: room (alone)

**Link status** (`LinkStatus`): Whether a link is `Shared` (committed to the neighbourhood) or `Local` (kept on this executor). [Local and shared links](perspectives/links/local-and-shared.md)

**Link verdict** (`LinkVerdict`: `Valid`, `Invalid`, `CantCheck`, `Flagged`): The stored result of checking a link's signature and anchor against its author's key log. [Link verdicts](perspectives/links/verdicts.md) — not: proof valid

**Listing** (`LocalListing`, `index.publish`): An executor's entry in the hosting index. [Hosting-index client](agents/executor/services/hosting-index.md)

**Literal expression** (`Literal`, `literal_encode`, `literal_decode`): A value encoded in the URL itself: `literal:string:`, `literal:number:`, `literal:boolean:` or `literal:json:`. [Literal expressions](languages/expressions/literals.md) — not: literal target (a literal target is a literal expression in a link's target)

**Live query** (`SubscribedQuery`, `perspective.subscribeQuery`, `perspective.modelSubscribe`): A SPARQL or model query that the executor runs again after relevant writes and whose delta updates go only to the RPC connection that opened it, as long as its agent is an owner. [Live queries](perspectives/querying/live-queries.md) — not: query subscription, subscription

**Local bootstrap languages** (`bootstrap-languages/local/`): Plain JavaScript system languages that run ADAM with no Holochain and no central service. [Local link language](languages/link-languages/local-link-language.md)

**Local link language** (`bootstrap-languages/local/link-language.js`): A link language with no peers that keeps links in the executor's key-value store. [Local link language](languages/link-languages/local-link-language.md)

**Local session** (`runtime.localSession`, `ClientKind::Local`): The 1-hour operator-role grant that the desktop shell mints with the admin credential for its main window and keeps in memory only. [Operator and main agent](agents/access/operator-and-main-agent.md)

**Local-only method** (`.local_only()` in `ws_handler.rs`): An RPC method that answers only the admin credential or the loopback caller, and refuses a key session, a local session and the home-network listener with `403 local-only`. [Operator and main agent](agents/access/operator-and-main-agent.md)

**Locked** (`AgentService::{lock, unlock}`, `AuthError::Locked`): The wallet state in which the keys are encrypted; it ends when `agent.unlock` runs with the derived passphrase. [Wallet and keys](agents/identity/wallet-and-keys.md) — the executor's state before its storage secret arrives is Sealed

**Logic read**: A read that uses only `Valid` links: values, counts, votes, roles and ordering. [Link verdicts](perspectives/links/verdicts.md)

**Main agent** (`AgentContext::Main`, wallet key `main`): Agent #0 of an executor, whose key is a device key in the operator's key log once linked. [Operator and main agent](agents/access/operator-and-main-agent.md)

**Managed agent** (`AgentContext::Managed { did }`): An agent that the executor hosts in multi-user mode for someone other than its operator: a tenant, a password user or a guest. [Managed agents](agents/executor/managed-agents/README.md) — not: managed user, hosted agent, user

**Master key**: The key that signs every change to a key log, made from the 12 words; it never rests in an executor. [Master key](agents/identity/devices/master-key.md)

**MCP server** (`rust-executor/src/mcp/server.rs`, `Ad4mMcpHandler`): The executor's Model Context Protocol server that exposes tools to AI agents on the same session and guards as the RPC API. [MCP server](agents/interfaces/mcp-server.md)

**Membership channel** (`POST /rooms/:id/membership`): The plaintext route on which the server link language sends a neighbourhood's membership links, apart from the encrypted commits. [Server link language](languages/link-languages/server-link-language/README.md)

**Membership graph** (named graph `coasys:membership`, `MembershipGraph`): The add-only, plaintext graph in a neighbourhood's perspective that names its rules, join mode, admins, members, guests, invites, removals and bots. [Membership](perspectives/neighbourhoods/membership/README.md)

**Membership imports** (`membershipRules`, `membershipIsMember`, `membershipMembers`, `membershipPersonOf`, export `handleMembershipChange`): The `ad4m:host` calls through which a link language reads the executor's replay of the membership graph, answered from cache. [Language isolates](languages/runtime/isolates.md)

**Membrane interface** (`Membrane`, `membership*` host imports, `handleMembershipChange`): The one interface through which every link language and the link server read a neighbourhood's membership replay. [Membership](perspectives/neighbourhoods/membership/README.md)

**Membrane proof** (`unyt_membrane_proof`, `ProofState`): Unyt's proof that admits the executor's Unyt agent key to the Unyt network, requested through the hosting index. [Hosting-index client](agents/executor/services/hosting-index.md)

**Mesh** (`SfuConfig.mode == "mesh"`, `MeshManager`): A call in which every participant sends media to every other directly; the executor forwards none. [Neighbourhood calls](perspectives/neighbourhoods/calls.md)

**Model class** (SDK `Ad4mModel`, `@Model` in `core/src/model/decorators.ts`): A TypeScript class whose decorators generate a subject class and whose methods run model queries. [Model classes](agents/interfaces/typescript-sdk/model-classes.md)

**Model query** (`execute_model_query`, `ModelQueryInput`): A query for instances of subject classes, built from their SHACL shapes and run in the executor. [Model queries](perspectives/querying/model-queries.md)

**Model shape** (`ModelShape`, `load_shape_from_store`): The parsed, cached form of a subject class's SHACL that model queries are built from. [Model queries](perspectives/querying/model-queries.md)

**Multi-user mode** (`enable_multi_user`): The executor setting that makes it a host: tenants join through `host.join`, and password users sign up and log in. [Managed agents](agents/executor/managed-agents/README.md)

**Neighbourhood** (`Neighbourhood`, `rust-executor/src/neighbourhoods.rs`): A perspective shared between members through a link language, at a `neighbourhood://` URL, with a membership graph. [Neighbourhoods](perspectives/neighbourhoods/README.md) — not: space (the apps' name; see Space)

**Neighbourhood creator** (author of the neighbourhood expression): The agent who signed a neighbourhood's expression; the call policy trusts only this agent's links. [SFU](agents/executor/services/sfu.md) — not: neighbourhood owner (an owner is a local `owners` entry)

**Neighbourhood expression** (`NeighbourhoodExpression`, `DecoratedNeighbourhoodExpression`): The signed `Neighbourhood { link_language, meta }` stored in the neighbourhood language. [Neighbourhood and perspective languages](languages/other-kinds/neighbourhood-and-perspective-languages.md)

**Neighbourhood language** (`bootstrap-languages/neighbourhood-language/`, alias `neighbourhood`): The system language that stores neighbourhood expressions. [Neighbourhood and perspective languages](languages/other-kinds/neighbourhood-and-perspective-languages.md)

**Notification** (`notification_check_loop`, `runtime_service/notification_access.rs`): An app-registered SPARQL trigger whose new matches go to the owner's events and to an optional webhook. [Live queries](perspectives/querying/live-queries.md)

**Offer** (`Offer`, `OfferKind { Device, Node, Bot }`): A holder's signed, single-use invitation for one device to join a key log, as a link or a QR code. [Device linking](agents/identity/devices/linking.md)

**Online status** (`set_online_status`, `OnlineAgent`, `hosted_status.rs`): An agent's signed, ephemeral status in a neighbourhood. [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md)

**Op** (`#[op2]` in `js_core/*_extension.rs`): A Rust function exposed to the JavaScript of a language isolate. [Language isolates](languages/runtime/isolates.md)

**Operator** (`Principal::Operator`, `OperatorSource`): The identity named in an executor's operator record, and the role that gives every capability; the role comes from the admin credential, the loopback rule, a key login of that identity from the person's own app, or the local session. It is not an agent and acts as the main agent. [Operator and main agent](agents/access/operator-and-main-agent.md) — not: admin, owner (an owner is a perspective owner)

**Operator record** (`OperatorRecord`, table `operator_record`, `runtime.operator`): The one `did:scid` of an executor's operator, set at first run (`--operator`, or the desktop shell) and changed only by that operator. [Operator and main agent](agents/access/operator-and-main-agent.md)

**Operator source** (`OperatorSource::{Credential, Loopback, Key, Local}`): Where a session's operator role came from; local-only methods admit only the first two. [Operator and main agent](agents/access/operator-and-main-agent.md)

**Owner** (`PerspectiveHandle.owners`, `is_owned_by`): An agent whose DID is in a perspective's `owners` list and who may therefore reach that perspective. [Guards](agents/access/guards.md)

**Oxigraph store** (`SparqlStore` in `perspectives/sparql_store.rs`): The per-perspective Oxigraph RDF store on RocksDB that holds all of a perspective's links, plain on disk behind file modes. [Oxigraph store](perspectives/links/oxigraph-store.md) — not: SPARQL store, link store

**Password user** (`users` row keyed by email, `user.create`): A managed agent whose login is an email and a password, with no key log. [Password users](agents/executor/managed-agents/password-users.md)

**Peers (local, remote)** (`Language::set_local_agents`, `Language::others`): The agents on this executor that use a link language, and the other agents that the link language reports. [Link languages](languages/link-languages/README.md)

**Pending diff** (`perspective_diff` table, `add_pending_diff`, `pending_diffs_loop`): A shared diff that the link language did not accept yet, queued in SQLite under the perspective's store key for retry. [Neighbourhood sync](perspectives/neighbourhoods/sync.md)

**Perspective** (`PerspectiveInstance`, `PerspectiveHandle`): A graph of links signed by device keys that the executor stores, with a verdict per link, for one or more owning agents. [Perspectives](perspectives/README.md)

**Perspective diff** (`PerspectiveDiff`, `DecoratedPerspectiveDiff`): A set of link additions and removals. [Links and store](perspectives/links/README.md)

**Perspective diff sync** (`bootstrap-languages/p-diff-sync/`): The Holochain link language that syncs diffs as a revision graph. [Perspective diff sync](languages/link-languages/perspective-diff-sync.md) — short form: p-diff-sync

**Perspective handle** (`PerspectiveHandle` in `types/domain.rs`): The stored description of a perspective: uuid, name, owners, neighbourhood and perspective state. [Perspectives](perspectives/README.md)

**Perspective language** (`bootstrap-languages/perspective-language/`, alias `perspective`): The system language meant for perspective snapshots; a stub that stores nothing. [Neighbourhood and perspective languages](languages/other-kinds/neighbourhood-and-perspective-languages.md)

**Perspective state** (`PerspectiveState`): Where a perspective stands with its neighbourhood: `Private`, `NeighbourhoodCreationInitiated`, `LinkLanguageFailedToInstall`, `LinkLanguageInstalledButNotSynced` or `Synced`. [Neighbourhood sync](perspectives/neighbourhoods/sync.md) — not: sync state

**Principal** (`Principal` in `agent/session.rs`): The kind of caller behind a session: operator, agent or anonymous. [Sessions](agents/access/sessions.md)

**Private record** (`SealedRecord`, `identity.record`): One sealed record per identity with device names, kinds, places, addresses, master-key holders and pending revocations. [Device inbox and private record](agents/identity/devices/inbox-and-record.md)

**Processing claim** (`ProcessingClaim`, `try_claim`): A shared link that an executor writes before it runs an auto-processor batch, so that peers back off. [Auto-processor](perspectives/social-dna/interpretation/auto-processor.md)

**Public perspective** (`Agent.perspective`): The signed links in an agent profile that describe the agent. [Agent profiles](agents/identity/profiles.md)

**Publishing (a language)** (`publish_language`, `apply_template`): Storing a bundle and its signed language meta in the language language. [Installing languages](languages/runtime/installing/README.md)

**Publishing (a neighbourhood)** (`neighbourhood.publish`): The older call that makes a neighbourhood on a link language other than the server link language; it writes no membership genesis. [Creating and joining](perspectives/neighbourhoods/creating-and-joining.md)

**Read set** (`ReadSet`): All signed links that a flow state read uses: proposals, votes, role grants and tombstones. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Reifier** (`make_reifier_iri`): The RDF 1.2 node that carries a stored link's author, timestamp, proof, link verdict and status. [Oxigraph store](perspectives/links/oxigraph-store.md)

**Replica state** (`FlowReplicaState`): Per-agent flow bookkeeping kept in SQLite outside the graph: last derived state and fired marks. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Request box** (`identity.requests`, `identity.postRequest`): Short signed requests between a person's own devices that change nothing until an app with the master key approves them. [Device inbox and private record](agents/identity/devices/inbox-and-record.md)

**Request context** (`RequestContext` in `types/domain.rs`): The value that each RPC handler receives: one `Arc<Session>`, a cancel token and the RPC connection id. [Sessions](agents/access/sessions.md)

**Resolver** (`identity/resolver.rs`, `author_of`, `KeyResolver`): The one function set that maps a device key to its person and gives key verdicts, over `KelCache`. [Key-log cache and resolver](agents/identity/key-log/cache.md)

**Revision** (`Language::current_revision`): A link language's name for its current state: in p-diff-sync a diff-entry hash, in the link server a hash of the active link hashes. [Link languages](languages/link-languages/README.md)

**Revocation reach** (`RevocationReach`, `neighbourhood.revocationReach`): The record of which local neighbourhoods dropped a revoked key and when a room-key rotation left it out. [Counts by person](perspectives/neighbourhoods/membership/counts-by-person.md)

**Revocation sweep**: The host's check, every 10 minutes and at each key login, that detaches a tenant whose device key was revoked. [Leaving a host](agents/executor/managed-agents/leaving.md)

**Role gate** (`ConsensusRule.from_role`, `flow_instance/roles/`): A model query that a voter must match, as of the vote's time, for the vote to count. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Room key** (`ad4m-keyring`, `bootstrap-languages/server-link-language/src/encryption.ts`): A versioned AES-256-GCM key that encrypts links in a link-server room on the wire, sealed to each admitted member's X25519 key; any admitted member grants and rotates it. [Room keys](languages/link-languages/server-link-language/room-keys.md)

**Room-key seed** (`agentRoomKeySeed`, `room_key_seed_for_context`): The secret from which an agent's X25519 key derives, given to a language only through its own host call; every other signing API refuses its derivation message. [Room keys](languages/link-languages/server-link-language/room-keys.md)

**Root key**: The key derived from the storage secret, in memory only, that wraps the executor store key and every data key. [Keyring and storage secret](agents/executor/data/keyring.md)

**RPC API** (`rust-executor/src/api/ws_rpc.rs`, `HandlerMap`): The executor's WebSocket at `/api/v1/ws` that carries method calls, events and live-query updates. [RPC API](agents/interfaces/rpc-api.md) — not: GraphQL API (removed)

**Runtime service** (`RuntimeService`, `rust-executor/src/runtime_service/`, `runtime.*` RPC): The executor service that holds the bootstrap seed and the trusted-agent, known-link-language and friend lists; its `runtime.*` calls also cover notifications, export and import, the operator record and quitting. [Executor services](agents/executor/services/README.md)

**Scribe** (`gossip()` in `bootstrap-languages/p-diff-sync/index.ts`): In p-diff-sync, the one agent (lowest DID among known peers) that may make merge commits. [Perspective diff sync](languages/link-languages/perspective-diff-sync.md)

**Sealed** (state `sealed`, `storage-sealed`): The executor state before the storage secret arrives, in which it answers only `runtime.storageStatus`, `storage.unlock` and the health check. [Executor startup](agents/executor/startup.md) — for a sealed value or file, see Sealed column

**Sealed column** (`s1:` values, `AD4MSEAL` files): A database value or language-storage file encrypted under a person's or a perspective's store key. [Encrypted stores](agents/executor/data/encrypted-stores.md)

**Sealed language storage** (`op_language_storage_read`, `op_language_storage_write`, `AD4MSEAL`): Language storage that the host encrypts per file, under the perspective's store key for a link language and the executor store key for other languages. [Language isolates](languages/runtime/isolates.md)

**Secret** (`Secret` in `secret.rs`): The type of a credential (the admin credential, AI provider keys, webhook credentials, capability codes) that cannot be serialised or printed, so it never reaches a reply, an event or a log. [App approval](agents/access/grants/app-approval.md)

**Server link language** (`bootstrap-languages/server-link-language/`): The default link language for new neighbourhoods: it syncs through one link-server room, encrypts every link on the wire and admits members by person. [Server link language](languages/link-languages/server-link-language/README.md)

**Session** (`Session` in `agent/session.rs`): A connection's identity: a principal and its grant, opened once by `Session::open` for RPC and MCP alike and checked again on each request, tool call and event. [Sessions](agents/access/sessions.md) — not: call (an SDK call object is also named `Session`)

**SFU** (`SfuService`, `rust-executor/src/sfu/`): The WebRTC selective forwarding unit inside the executor that relays call media. [SFU](agents/executor/services/sfu.md)

**SFU room** (`SfuRoom`): One call on an SFU, keyed by neighbourhood URL and room name. [SFU](agents/executor/services/sfu.md) — not: room (alone)

**SHACL shape** (`SHACLShape`): The W3C SHACL description of a subject class, stored as links. [Subject classes](perspectives/social-dna/subject-classes.md)

**Shared store** (`identity-store/`): The service behind the default agent language that keeps key logs, signed heads, the key index, vouching notes, agent records and device-linking data, and admits new logs through the brake. [Shared store](languages/other-kinds/agent-language/shared-store.md)

**Sign-up setting** (`SignupPolicy { Anyone, ByInvite, NoOne }`, `host.setPolicy`): Who may join a host through `host.join`; a new executor starts at "no one". [Tenants](agents/executor/managed-agents/tenants.md)

**Signal** (`PerspectiveInstance::send_signal`, `neighbourhood.sendSignal`): An ephemeral signed payload sent to one agent of a neighbourhood. [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md) — a Holochain signal is a different thing ([Holochain service](agents/executor/services/holochain.md))

**Signed head** (`SignedHead`, `GET /v1/heads/{scid}`): The shared store's signature, under `coasys:head`, on the latest position and hash of one key log; an executor accepts a head from the agent language only with it. [Shared store](languages/other-kinds/agent-language/shared-store.md)

**Signing domain** (`signing_bytes(domain, value)`, `coasys:kel`, `coasys:link`, …): The purpose that every signature names, so that it verifies for no other purpose. [Signing and verification](agents/identity/signing-and-verification.md)

**Social DNA (SDNA)** (`add_sdna`, `SdnaType`): The subject class and flow definitions stored as shared links in a perspective. [Social DNA](perspectives/social-dna/README.md)

**Source language** (`LanguageMeta::template_source_language_address`): The language that a templated language was made from. [Language trust and templates](languages/runtime/installing/trust-and-templates.md)

**Space**: The apps' name for a neighbourhood; in code it appears in `spaces.list`, `SpaceHolding`, a bot grant's `spaces` and the export's `kind: "space"`. Pages say neighbourhood. [Neighbourhoods](perspectives/neighbourhoods/README.md)

**Space holding** (`SpaceHolding`, `spaces.list`): One neighbourhood that this executor holds for the session's agent, with how it came there, its link-language choice, sync state and the person's role. [Creating and joining](perspectives/neighbourhoods/creating-and-joining.md)

**SPARQL query** (`SparqlStore::query_arbitrary`, `perspective.querySparql`): A read-only SPARQL 1.2 query over a perspective's Oxigraph store. [SPARQL queries](perspectives/querying/sparql.md)

**Storage secret** (`AD4M_STORAGE_SECRET_FILE`, `storage.unlock`): 32 random bytes per executor, kept off the data disk, from which it derives the root key and the agent passphrase; no recovery code exists. [Keyring and storage secret](agents/executor/data/keyring.md) — not: operator secret (the operator is an identity)

**Store key** (`SK(Person(..))`, `SK(Perspective(uuid))`): A random key for one store's sealed data, wrapped under each owner's data key. [Keyring and storage secret](agents/executor/data/keyring.md)

**Subject class** (`SdnaType::SubjectClass`, `parse_shacl_to_links`): A named SHACL shape with actions, stored in a perspective. [Subject classes](perspectives/social-dna/subject-classes.md) — not: model (use model class for the SDK side)

**Sync choice** (`SyncChoice`: `Default`, `OwnLinkServer`, `Holochain`): The creator's pick of a neighbourhood's link language at `neighbourhood.create`; the executor records it as the link-language choice. [Creating and joining](perspectives/neighbourhoods/creating-and-joining.md)

**System language** (`is_system_language`, `SystemLanguageAddresses`): A language from the bootstrap seed or a known link language, loaded with file access to the executor's working directory; its storage is sealed under the executor store key. [Bootstrap languages](languages/runtime/bootstrap-languages.md) — not: core language

**Telepresence** (`has_telepresence_adapter`, `Language::set_online_status`, `send_signal`, `send_broadcast`): The ephemeral, unstored side of a neighbourhood: online status, signals and broadcasts, carried by the link language. [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md) — not: presence (alone)

**Template variable** (`//!@ad4m-template-variable`, `apply_template_data`): A constant in a bundle, marked by a comment line, that templating replaces; the server link language's are `SERVER_URL`, `UID` and `CREATOR`. [Language trust and templates](languages/runtime/installing/trust-and-templates.md)

**Templated language** (`language_apply_template_on_source`): A language made by templating a source language; its meta carries `templateAppliedParams` and `templateSourceLanguageAddress`. [Language trust and templates](languages/runtime/installing/trust-and-templates.md)

**Tenant** (`TenantRecord`, `tenants` table, `role: "tenant"`): A person who uses an executor they do not run: a managed agent whose key is a device key in the person's key log; pending, active, held, detached or deleted. It logs in by key. [Tenants](agents/executor/managed-agents/tenants.md) — not: user, managed user

**Token interface** (`TokenSpec`, `TokenCapability`, `Capability::Token*`): The `token*` calls for assets, balances and transfers on the agent language; interfaces only, answering `disabled`. [Token interface](languages/other-kinds/agent-language/token-interface.md)

**Topic** (`Topic` in `events.rs`, `Topic::as_str`): The kind of an executor event, with its wire name and the capability that reads it. [Events](agents/interfaces/events.md) — not: pub/sub topic

**Transition proposal** (`FlowTransitionProposal`): A signed set of links that proposes moving a flow instance to a state and carries its votes. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Trusted agent** (`RuntimeService::get_trusted_agents`): A DID whose languages this executor installs without a template check: the seed list, the main agent and the database list. [Language trust and templates](languages/runtime/installing/trust-and-templates.md)

**TypeScript SDK** (`@coasys/ad4m`, `core/`, `Ad4mClient`, `ApiClient`): The TypeScript client of the RPC API, with the shared types and the model class decorators. [TypeScript SDK](agents/interfaces/typescript-sdk/README.md) — not: core (as a product name)

**Unyt** (`unyt_service.rs`, alliance DNA): The Holochain currency DNA (HOT) for hosting credits and earnings, on its test network; its agent key is separate from every device key. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**User account** (`users` table, `user_management.rs`): The login of a password user: email, DID and password hash; a tenant's `users` row holds its tenant id and an unusable password. [Password users](agents/executor/managed-agents/password-users.md) — not: user (for the agent itself)

**User token** (`generate_user_jwt`): A JWT issued at a password login whose `sub` is a managed agent's DID, backed by a grant with the default user capabilities. [Password users](agents/executor/managed-agents/password-users.md) — not: user JWT, login JWT

**Vouching note** (`VouchNote`, `vouched`): A note beside a key log that says an email address was checked, with no address in it; a host requires it at `host.join`. [Tenants](agents/executor/managed-agents/tenants.md)

**Wallet** (`WalletBackend`, `LocalWallet`, `SharedWallet`): The store of every Ed25519 device key of the executor, by name, under the agent passphrase. [Wallet and keys](agents/identity/wallet-and-keys.md). For the encrypted file form, see Keystore.
