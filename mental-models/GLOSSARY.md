# Glossary

This file defines each ADAM term once, in alphabetical order. Each entry gives the term, its code names in brackets, the definition and the page that owns the term. "not: …" lists words that pages must not use for the term. General technical terms, such as DID, SPARQL, SHACL, WebRTC, SFU and Holochain, keep their usual meaning and have no entry.

**Abstract class** (`model_query/abstract_classes.rs`, built-ins `ad4m://TextContent`, `ad4m://Mentioning`, `ad4m://Container` in `model_query/builtin_classes.rs`): A subject class that reads the instances of several concrete classes and sends each write to the instance's own classes. No caller can create an instance of an abstract class. [Subject classes](perspectives/social-dna/subject-classes.md)

**Access check** (`Guard` in `api/guards.rs`, `get_perspective_with_access`): The code in a method guard that applies a session's grant to one RPC call or MCP tool call. It checks a capability, an ownership, or both. [Access checks](agents/access/checks.md). Event delivery: [Events](agents/interfaces/events.md). Language calls: [Signing and verification](agents/identity/signing-and-verification.md) — not: guard (alone), which is a flow term

**Action** (`AD4MAction`, `Command`): A JSON list of commands that a subject class stores for its constructor, its destructor, or the setter, adder or remover of a property. [Subject classes](perspectives/social-dna/subject-classes.md)

**ad4m-connect** (`@coasys/ad4m-connect`, `connect/`): The browser and Node library that finds an executor, runs a login flow and gives an app an authenticated `Ad4mClient`. [ad4m-connect](agents/interfaces/typescript-sdk/ad4m-connect.md)

**ad4m:host** (`rust-executor/src/js_core/host.js`): The ES module that every language imports to call the executor: agent, Holochain, HTTP, hash, context, emit and storage functions. [Language isolates](languages/runtime/isolates.md)

**Admin credential** (`Ad4mConfig.admin_credential`, `is_admin_credential_token_on`, `insecure_no_admin_credential`): The static secret token that makes a caller the operator. `run` does not start the executor without one, unless the config sets `insecure_no_admin_credential`. [Operator and main agent](agents/access/operator-and-main-agent.md)

**Agent** (`AgentContext`, `did_for_context`): An Ed25519 key pair in the executor's wallet, named by its `did:key` DID. The executor signs and stores data on behalf of the agent. [Agents](agents/README.md) — not: user (a user account is only the login of a managed agent)

**Agent context** (`AgentContext::{Main, Managed { did }}`): The value that names the agent a call acts as. The executor keeps it in a thread-local while a language call runs. [Identity](agents/identity/README.md)

**Agent language** (`LanguageController::get_agent_language`, `bootstrap-languages/centralized-agent-language/`): The expression language behind the `did` alias. Its expressions are agent profiles, keyed by DID. [Agent languages](languages/other-kinds/agent-languages.md)

**Agent profile** (`Agent` in `types/domain.rs`): The public record of one agent: its DID, its public perspective and the address of its direct-message language. [Agent profiles](agents/identity/profiles.md) — not: public profile, `Agent` record

**Agent session** (`perspective.runAgent`, `run_agent_session`): A conversation with an LLM in one perspective, which the harness runs as the caller. The LLM gets the perspective's static MCP reads and the writes that the caller names. [MCP server](agents/interfaces/mcp-server.md) — not: session (a `Session` is the identity of a connection)

**AI service** (`AIService`, `ai_service/endpoints/`, `ai_service/local/`): The executor service that sends LLM, embedding, transcription and speech requests to models on endpoints, which are model servers such as Ollama, llama-server or OpenAI. One-click local AI (`ai.localInstall`) installs model servers on the device and registers them as endpoints. [AI service](agents/executor/services/ai.md)

**ALDK** (`ad4m-ldk/`, `defineLanguage`, `ad4m_language!`): The AD4M Language Development Kit. Developers use it to write languages in TypeScript or in Rust compiled to WASM. [Language interface](languages/runtime/interface.md)

**Alias** (`LanguageController::language_aliases`): One of the names `did`, `lang`, `neighbourhood` and `perspective`, each of which stands for the address of a system language. [Language addresses](languages/expressions/addresses.md)

**App grant** (`generate_capability_token`, `grants::mint`): The grant and the JWT that an app gets when the owner of an agent approves the app's capability request. [App grants](agents/access/app-grants.md) — not: cap token

**Auto-processor** (`AutoProcessorConfig`, `auto_processor_supervisor`): An instance in a perspective that makes the executor run interpretation on new messages in a scope, with no caller. [Auto-processor](perspectives/social-dna/interpretation/auto-processor.md)

**Base** (`FlowInstance.subject`, `expression_address` in `create_subject`): The URI that an instance, an interpretation overlay or a flow instance is about. [Flows](perspectives/social-dna/flows/README.md) — not: base expression, base URI

**Batch** (`create_batch`, `commit_batch`, `batch_store`): A set of link changes in memory that the executor stores and commits as one perspective diff. [Link write path](perspectives/links/write-path.md)

**Billing** (`rust-executor/src/billing.rs`, `deduct_credits_and_log`, `host_rates` table, `get_free_hosting_enabled`): The charge of compute credits to a managed agent for link writes and AI calls, at the prices in `host_rates`. Free hosting is on by default and bills nothing. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**Bootstrap languages** (`bootstrap-languages/`, `bootstrap-languages/local/`): The language packages in this repository that bootstrap seeds point at. `bootstrap-languages/local/` holds plain JavaScript system languages that need no Holochain and no central service. [Bootstrap languages](languages/runtime/bootstrap-languages.md)

**Bootstrap seed** (`BootstrapSeed`, `mainnet_seed.json`): The JSON file that gives an executor its language-language bundle, the system language addresses, the known link languages and the seed's trusted agents. [Bootstrap languages](languages/runtime/bootstrap-languages.md) — not: network seed (that is the Holochain DNA `network_seed`)

**Broadcast** (`PerspectiveInstance::send_broadcast`, `neighbourhood.sendBroadcast`): A signed payload that the link language sends to all agents of a neighbourhood and does not store. [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md)

**Call** (SDK `Session` in `core/src/neighbourhood/Session.ts`, `createSession`, `SessionTopology`): An audio and video connection among members of a neighbourhood. Media goes directly between the participants (mesh) or through the executor's SFU. [Neighbourhood calls](perspectives/neighbourhoods/calls.md) — not: session (a session is the auth `Session`)

**Call policy** (`CallPolicy`, `SfuConfig`, link predicate `ad4m://sfu_config`): A link by the neighbourhood creator that sets the neighbourhood's call mode and mesh size. The call modes are `mesh`, `designated`, `gateway` and `cascaded`. [SFU](agents/executor/services/sfu.md)

**Capability** (`Capability { with: Resource, can }` in `agent/capabilities/types.rs`): A domain, a list of pointers and a list of verbs that together name one kind of action a caller may take. [Capabilities](agents/access/capabilities.md). For languages, see Language capability.

**Capability request** (`agent.requestCapability`, `agent.permitCapability`, `agent.generateJwt`): An app's request for a set of capabilities on one agent. The operator, or a session of that agent with `agent/PERMIT`, approves it, and the app redeems it with a six-digit code. [App grants](agents/access/app-grants.md)

**Consensus rule** (`ConsensusRule` in `shacl_parser.rs`, `ConsensusRule.from_role`): The number of different DIDs that must sign a transition proposal before the transition settles. An optional role is a model query that each voter must match at the time of the vote. [Flows](perspectives/social-dna/flows/README.md)

**Entanglement proof** (`EntanglementProof`, `entanglement_service`): Two signatures, one in each direction, that link the main agent's DID to another key, such as a device key or a Holochain agent key. [Signing and verification](agents/identity/signing-and-verification.md)

**Event** (`Event` and `Topic` in `events.rs`, `event_specs()`, `events.watch`): A typed message that the executor pushes to each socket that watches its type with `events.watch`. A socket gets it only when its session is in the audience that the publisher names and holds the event's capability. [Events](agents/interfaces/events.md) — not: subscription (that is a live query), notification

**Executor** (crate `ad4m-executor`, `rust_executor::run`): The ADAM process that hosts agents, perspectives, language isolates, executor services and the APIs. [Executor](agents/executor/README.md) — not: node, ADAM node, conductor (that is Holochain's)

**Executor service** (`ai_service/`, `sfu/`, `holochain_service/`, `billing.rs`, `email_service.rs`): A Rust module in the executor that is not a language, such as AI, SFU, Holochain, billing, email or the runtime service. [Executor services](agents/executor/services/README.md)

**Expression** (`Expression<T>`, `create_signed_expression`, `ExpressionProof`, `verify_expression_proof`): Data that an agent signs, in the form `{author, timestamp, data, proof}`. The executor that reads it marks the proof `valid` or `invalid`. [Expressions and addresses](languages/expressions/README.md)

**Expression language** (`Capability::ExpressionGet`, `ExpressionCreate`): A language that stores one kind of expression and exports `expressionGet` and usually `expressionCreate`. [Other language kinds](languages/other-kinds/README.md)

**Expression URL** (`ExpressionRef::try_from`, `parse_expr_url`): The address of one expression: `<language address or alias>://<expression address>`, a DID, or a `literal:` URL. [Expressions and addresses](languages/expressions/README.md)

**Flow** (`SHACLFlow`): A state machine that social DNA stores: states, transitions, guards and a consensus rule. [Flows](perspectives/social-dna/flows/README.md)

**Flow instance** (`FlowInstance`, `run_flow_spawn_pass`): One run of a flow on one base. The flow spawn pass starts flow instances on the instances that an interpretation run creates. [Flows](perspectives/social-dna/flows/README.md)

**Flow receipt** (`FlowReceipt`): A content-addressed record of a finished flow instance. Any executor checks it by running the fold again. [Flow receipts](perspectives/social-dna/flows/receipts.md)

**Fold** (`fold`, `fold_read_set`, `ReadSet`, `DerivedState`): The pure function that computes the state of a flow instance. It walks the flow from its first state over the signed proposals, votes, role grants and role revocations. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Grant** (`LiveGrant` in `agent/grants.rs`, table `grants`): The executor's record of one issued token: the agent it acts as, its capabilities and the app that holds it. A token works only while its grant is active and not expired. [App grants](agents/access/app-grants.md)

**Guard** (`requires` on `FlowState`): A model query on a flow state. The executor proposes a transition into that state only when the query matches. [Flow spawn and guards](perspectives/social-dna/flows/spawn-and-guards.md) — not: method guard

**Hard-wired class** (`hardwired_class.rs`, `hardwired_sdna/`): A subject class that the executor registers itself from bundled JSON, such as `FlowInstance`, `InterpretationRun` or `AutoProcessor`. [Social DNA](perspectives/social-dna/README.md) — not: built-in class (that is an abstract class that the executor declares and never stores, such as `ad4m://TextContent`)

**Harness** (`ai_service/harness/`, `run_with_tools`): The tool-calling LLM loop that runs agent sessions. Interpretation can use it in place of one prompt. [Interpretation](perspectives/social-dna/interpretation/README.md)

**Holochain service** (`HolochainService`, `HolochainServiceInterface`): The executor service that runs the embedded Holochain conductor behind one actor channel. [Holochain service](agents/executor/services/holochain.md)

**Hosted mode** (`wallet_backend = "shared"`, `db_backend = "shared"`, `SharedWallet`, `SharedDb`, `wallet_backend_url`, `db_backend_url`): An executor that reads its wallet, its users and the shared perspective metadata over HTTP from a hosting service. All other tables stay in the executor's own SQLite database. [Executor data](agents/executor/data.md) — not: shared mode

**Installing (a language)** (`language_by_ref`, `install_language`, `install_language_from_address`): Getting a bundle by its address, checking its hash, saving it and loading it into a language isolate. [Installing languages](languages/runtime/installing/README.md)

**Instance** (`create_subject`, `update_subject`, MCP `instance_*` tools): A base whose links match the shape of a subject class. The full name is subject instance. [Subject classes](perspectives/social-dna/subject-classes.md) — not: record, item, subject (alone)

**Interpretation** (`run_interpretation`, `InterpretationRun`): An executor pass that sends a transcript and the perspective's subject classes to an LLM and writes what the LLM extracts as instances. Each run stores an `InterpretationRun` with the model, prompt version, time and sources. [Interpretation](perspectives/social-dna/interpretation/README.md)

**Interpretation overlay** (`InterpretationOverlay`): A second instance on the same base that records the values the LLM inferred and the run that wrote them. A person uses it to accept or reject those values. [Interpretation overlay](perspectives/social-dna/interpretation/overlay.md)

**Keystore** (`AgentStore.keystore`, `KeystoreEnvelope`): The encrypted form of all wallet keys in `agent.json`. [Wallet and keys](agents/identity/wallet-and-keys.md)

**Language** (`rust-executor/src/languages/`): A JavaScript bundle that runs in its own language isolate. Each language capability it exports adds one job: create and get expressions, commit, sync, render and query links, list peers, or carry telepresence. [Languages](languages/README.md)

**Language address** (`calculate_language_hash`, op `hash`): The ID of a language: `Qm` followed by the base58btc string of a CIDv1, with codec `0x00`, over the SHA-256 of the bundle text. [Language addresses](languages/expressions/addresses.md)

**Language capability** (`Capability` in `languages/capability.rs`): One function group that a language exports. The executor detects it once at load, and the presence of the export is the capability. [Language interface](languages/runtime/interface.md) — not: capability (alone), which means the access capability

**Language controller** (`LanguageController`): The global registry of loaded languages. Rust code calls a language only through it. [Language controller](languages/runtime/controller.md)

**Language isolate** (`JsCore::new_for_language`, `LanguageRuntimeHandle`): One Deno worker, on its own OS thread, that runs one language bundle in a sandbox. [Language isolates](languages/runtime/isolates.md) — not: language instance

**Language language** (`bootstrap-languages/language-language/`, alias `lang`, `LanguageMeta`): The system language that stores language bundles and their signed language meta: name, address, author and template fields. [Language language](languages/runtime/installing/language-language.md)

**Launcher** (`ui/src-tauri`): The desktop application that embeds the executor, holds the admin credential and shows the operator's UI. [Executor](agents/executor/README.md)

**Link** (`Link` in `types/core.rs`): A triple of source, optional predicate and target. [Links](perspectives/links/link-expressions.md)

**Link expression** (`LinkExpression`, `DecoratedLinkExpression`): A link that its author signed, with a timestamp, a proof and a link status. [Links](perspectives/links/link-expressions.md)

**Link language** (`Language::commit`, `Language::sync`): A language that stores and syncs the shared links of one neighbourhood and carries its telepresence. [Link languages](languages/link-languages/README.md)

**Link server** (`link-server/`): A self-hosted Node.js server that stores the links of link-server rooms for the server link language. [Link server](languages/link-languages/link-server.md)

**Link-server operator** (`link-server/src/operator.ts`): The person who runs a link server. [Link server](languages/link-languages/link-server.md) — not: operator (alone), which is the executor's administrator

**Link-server room** (`/rooms/:roomId/*`, `acl` table in `link-server/src/db.ts`): The link store of one neighbourhood on a link server, named by the `UID` template value. Its first authenticated DID is its admin, and the server checks each request against the room's list of admitted DIDs. [Link server](languages/link-languages/link-server.md) — not: room (alone)

**Link status** (`LinkStatus`): The status of a link: `Shared` when it is committed to the neighbourhood, `Local` when it stays on this executor. [Local and shared links](perspectives/links/local-and-shared.md)

**Literal expression** (`Literal`, `literal_encode`, `literal_decode`): A value that its URL holds in full: `literal:string:`, `literal:number:`, `literal:boolean:` or `literal:json:`. [Literal expressions](languages/expressions/literals.md) — not: literal target (a literal target is a literal expression in a link's target)

**Live query** (`SubscribedQuery`, `perspective.subscribeQuery`, `perspective.modelSubscribe`): A SPARQL query or model query that the executor runs again after relevant writes. The executor sends each change of the result only to the RPC connection that opened the query. [Live queries](perspectives/querying/live-queries.md) — not: query subscription, subscription

**Local link language** (`bootstrap-languages/local/link-language.js`): A link language with no peers that keeps links in the executor's key-value store. [Local link language](languages/link-languages/local-link-language.md)

**Locked** (`AgentService::{lock, unlock}`, `AuthError::Locked`): The wallet state in which the keys are encrypted. The executor signs nothing and checks no token until the operator unlocks the wallet. [Wallet and keys](agents/identity/wallet-and-keys.md)

**Main agent** (`AgentContext::Main`, wallet key `main`): Agent #0 of an executor, which `agent.generate` creates and the operator owns. [Operator and main agent](agents/access/operator-and-main-agent.md)

**Managed agent** (`AgentContext::Managed { did }`): An agent other than the main agent that the executor hosts for a user account in multi-user mode. [Managed agents](agents/executor/managed-agents.md) — not: user, hosted agent

**MCP server** (`rust-executor/src/mcp/server.rs`, `Ad4mMcpHandler`): The executor's Model Context Protocol server, which gives AI agents the executor's tools. [MCP server](agents/interfaces/mcp-server.md)

**Method guard** (`Guard` in `api/guards.rs`): The rule that each RPC method registers for who may call it. Dispatch checks the method's capability, then this rule, before the handler runs. [Access checks](agents/access/checks.md) — not: guard (alone), which is a flow term

**Model class** (SDK `Ad4mModel`, `@Model` in `core/src/model/decorators.ts`): A TypeScript class whose decorators generate a subject class and whose methods run model queries. [Model classes](agents/interfaces/typescript-sdk/model-classes.md)

**Model query** (`execute_model_query`, `ModelQueryInput`, `ModelShape`): A query for the instances of subject classes, which the executor builds from their SHACL shapes and runs. [Model queries](perspectives/querying/model-queries.md)

**Monotonic predicate** (`MONOTONIC_PREDICATES`, `Ending` in `perspectives/monotonic.rs`): A predicate, such as any predicate under `ad4m://flow/` and `ad4m://acceptedBy`, under which no diff removes a `Shared` link. Such a link ends only by a tombstone, by a revocation, or never. [Link write path](perspectives/links/write-path.md)

**Multi-user mode** (`enable_multi_user`, `Ad4mDb::get_multi_user_enabled`): The executor setting that lets anonymous callers sign up and log in as managed agents. [Managed agents](agents/executor/managed-agents.md)

**Neighbourhood** (`Neighbourhood`, `rust-executor/src/neighbourhoods.rs`): A perspective that agents share through a link language, published at a `neighbourhood://` URL. [Neighbourhoods](perspectives/neighbourhoods/README.md)

**Neighbourhood creator** (author of the neighbourhood expression): The agent who signed a neighbourhood expression. The call policy trusts only the links of this agent. [SFU](agents/executor/services/sfu.md) — not: neighbourhood owner (an owner is a local `owners` entry)

**Neighbourhood expression** (`NeighbourhoodExpression`, `DecoratedNeighbourhoodExpression`, `bootstrap-languages/neighbourhood-language/`): The signed `Neighbourhood { link_language, meta }` that the system language with the alias `neighbourhood` stores. [Neighbourhood and perspective languages](languages/other-kinds/neighbourhood-and-perspective-languages.md)

**Notification** (`notification_check_loop`, `runtime_service/notification_access.rs`): A SPARQL trigger that an app registers. The executor sends each new match to the owner's events and to an optional webhook. [Live queries](perspectives/querying/live-queries.md)

**Operator** (`Principal::Operator`): The administrator role of the executor, which the caller who presents the admin credential holds. The operator is not an agent and acts as the main agent. [Operator and main agent](agents/access/operator-and-main-agent.md) — not: admin, owner (an owner is a perspective owner)

**Owner** (`PerspectiveHandle.owners`, `is_owned_by`): An agent whose DID is in the `owners` list of a perspective, which lets the agent reach that perspective. Agents of one executor that join the same neighbourhood share one perspective as its owners. [Access checks](agents/access/checks.md)

**Oxigraph store** (`SparqlStore` in `perspectives/sparql_store.rs`, `make_reifier_iri`): The Oxigraph RDF store on RocksDB that holds all links of one perspective. For each link, an RDF 1.2 reifier holds the author, timestamp, proof, verdict and link status. [Oxigraph store](perspectives/links/oxigraph-store.md) — not: SPARQL store, link store

**Peers (local, remote)** (`Language::set_local_agents`, `Language::others`): The agents on this executor that use a link language, and the other agents that the link language reports. [Link languages](languages/link-languages/README.md)

**Perspective** (`PerspectiveInstance`, `PerspectiveHandle`): A graph of signed links that the executor stores for one or more owners. [Perspectives](perspectives/README.md)

**Perspective diff** (`PerspectiveDiff`, `DecoratedPerspectiveDiff`): A set of link additions and link removals. [Links and store](perspectives/links/README.md)

**Perspective diff sync** (`bootstrap-languages/p-diff-sync/`): The Holochain link language that syncs perspective diffs as a revision graph. Only its scribe, the known peer with the lowest DID, makes merge commits. [Perspective diff sync](languages/link-languages/perspective-diff-sync.md) — short form: p-diff-sync

**Perspective state** (`PerspectiveState`): The stage that a perspective has reached with its neighbourhood: `Private`, `NeighbourhoodCreationInitiated`, `NeighbourhoodJoinInitiated`, `LinkLanguageFailedToInstall`, `LinkLanguageInstalledButNotSynced` or `Synced`. [Neighbourhood sync](perspectives/neighbourhoods/sync.md) — not: sync state

**Principal** (`Principal` in `agent/session.rs`, `anonymous_capabilities`): The kind of caller behind a session: operator, agent or anonymous. An anonymous caller can only request a capability and, in multi-user mode, sign up or log in. [Sessions](agents/access/sessions.md)

**Publishing (a language)** (`publish_language`, `apply_template`): Storing a bundle and its signed language meta in the language language. [Installing languages](languages/runtime/installing/README.md)

**Publishing (a neighbourhood)** (`neighbourhood.publish`, `neighbourhood_publish_from_perspective_with_context`): Storing a neighbourhood expression for a perspective and recording its URL on that perspective. The perspective is then a neighbourhood. [Publishing and joining](perspectives/neighbourhoods/publishing-and-joining.md)

**Revision** (`Language::current_revision`): The ID of the current state of a link language, such as a diff-entry hash in p-diff-sync or the XOR of the active link hashes on a link server. [Link languages](languages/link-languages/README.md)

**RPC API** (`rust-executor/src/api/ws_rpc.rs`, `HandlerMap`): The executor's WebSocket at `/api/v1/ws`, which carries method calls, events and live-query updates. [RPC API](agents/interfaces/rpc-api.md) — not: GraphQL API (removed)

**Runtime service** (`RuntimeService`, `rust-executor/src/runtime_service/`, `runtime.*` RPC, `known_link_languages` table): The executor service that holds the bootstrap seed and the lists of trusted agents, known link languages and friends. Its `runtime.*` calls also cover notifications, export, import and quit. [Executor services](agents/executor/services/README.md)

**Server link language** (`bootstrap-languages/server-link-language/`, `src/encryption.ts`): The link language that syncs through a link server and encrypts links end to end. Each link-server room has an AES-256-GCM room key, which the language seals to the DID of each member. [Server link language](languages/link-languages/server-link-language.md)

**Session** (`Session` in `agent/session.rs`): The identity of a connection: a principal, its capabilities and its grant. The executor checks it again on each request and each event. [Sessions](agents/access/sessions.md) — not: call (an SDK call object is also named `Session`), MCP session (an MCP session holds one `Session`)

**SFU room** (`SfuRoom`): One call on the executor's SFU, keyed by neighbourhood URL and room name. [SFU](agents/executor/services/sfu.md) — not: room (alone)

**Signal** (`PerspectiveInstance::send_signal`, `neighbourhood.sendSignal`): A signed payload that the link language sends to one agent of a neighbourhood and does not store. [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md) — a Holochain signal is a different thing ([Holochain service](agents/executor/services/holochain.md))

**Social DNA (SDNA)** (`add_sdna`, `SdnaType`): The subject class and flow definitions that a perspective stores as shared links. [Social DNA](perspectives/social-dna/README.md)

**Subject class** (`SdnaType::SubjectClass`, `parse_shacl_to_links`): A named SHACL shape with actions, which a perspective stores. [Subject classes](perspectives/social-dna/subject-classes.md) — not: model (use model class for the SDK side)

**System language** (`is_system_language`, `SystemLanguageAddresses`): A language that the bootstrap seed or the known link languages name. The executor loads it with extra file access. [Bootstrap languages](languages/runtime/bootstrap-languages.md) — not: core language

**Telepresence** (`has_telepresence_adapter`, `Language::set_online_status`, `send_signal`, `send_broadcast`): The part of a neighbourhood that no executor stores: online status, signals and broadcasts. The link language carries it. [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md) — not: presence (alone)

**Templated language** (`language_apply_template_on_source`, `//!@ad4m-template-variable`, `apply_template_data`): A language that the executor makes from a source language by replacing the constants that a template comment marks. Its language meta records `templateAppliedParams` and `templateSourceLanguageAddress`. [Language trust and templates](languages/runtime/installing/trust-and-templates.md)

**Tombstone** (`ad4m://retracted`, `retraction_for`, `retraction_effects`): A signed link `<source> ad4m://retracted literal:string:<signature>` with which the author of a `Shared` link under a monotonic predicate ends that link. Each executor removes the named link from its store and stores no later copy of it. [Flows](perspectives/social-dna/flows/README.md) — not: removal

**Tool bundle** (`tool_bundles/`, `BundleSandbox`, `tool_bundles::builtin`): The JavaScript of an app's agent tools, which the executor runs in a sandbox isolate with no access to the executor. The executor compiles in one tool bundle, WE's, behind `template_read` and `template_update`. [MCP server](agents/interfaces/mcp-server.md) — not: language

**Transition proposal** (`FlowTransitionProposal`): A set of signed links that proposes to move a flow instance to a state and carries the votes on it. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Trusted agent** (`RuntimeService::get_trusted_agents`): A DID whose languages this executor installs with no template check. The list holds the seed's agents, the main agent and the agents in the database. [Language trust and templates](languages/runtime/installing/trust-and-templates.md)

**TypeScript SDK** (`@coasys/ad4m`, `core/`, `Ad4mClient`, `ApiClient`): The TypeScript client of the RPC API, with the shared types and the model class decorators. [TypeScript SDK](agents/interfaces/typescript-sdk/README.md) — not: core (as a product name)

**Unyt** (`rust-executor/src/unyt_service.rs`, alliance DNA): The Holochain currency DNA whose mHOT pays for hosting. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**User account** (`users` table, `user_management.rs`): The login of a managed agent: email, DID and password hash. [Managed agents](agents/executor/managed-agents.md) — not: user (for the agent itself)

**User token** (`generate_user_jwt`): A JWT that the executor issues at login, with the DID of a managed agent as its `sub`. Its grant holds the default user capabilities. [Managed agents](agents/executor/managed-agents.md) — not: user JWT, login JWT

**Wallet** (`WalletBackend`, `LocalWallet`, `SharedWallet`): The executor's store of all its Ed25519 keys, by name, which the operator's passphrase locks. The keystore is its encrypted form in a file. [Wallet and keys](agents/identity/wallet-and-keys.md)
