# Glossary

One definition per term, in alphabetical order. Each entry gives the code name in brackets and the page that explains the term. "not: …" lists words that pages must not use for this concept.

**Abstract class** (`model_query/abstract_classes.rs`, built-in `ad4m://TextContent`): A subject class that reads the instances of several concrete classes, sends each write to the record's own classes, and cannot be created. [Subject classes](perspectives/social-dna/subject-classes.md)

**Access check** (`ctx.require`, `get_perspective_with_access`, `refuse_user_session`): The code in a handler, MCP tool, event filter or language op that applies a session's grant to one call, as a capability check plus an ownership check. [Access checks](agents/access/checks.md) — not: guard (a guard is a flow term)

**ACL** (`acl` table in `link-server/src/db.ts`): The list of DIDs admitted to one link-server room, checked on every request. [Link server](languages/link-languages/link-server.md)

**Action** (`AD4MAction`, `Command`): A JSON list of commands stored on a subject class for its constructor, its destructor, or a property's setter, adder or remover. [Subject classes](perspectives/social-dna/subject-classes.md)

**ad4m-connect** (`@coasys/ad4m-connect`, `connect/`): The browser and Node library that finds an executor, runs a login flow and gives an app an authenticated `Ad4mClient`. [ad4m-connect](agents/interfaces/typescript-sdk/ad4m-connect.md)

**ad4m:host** (`rust-executor/src/js_core/host.js`): The ES module that every language imports to reach the executor's ops: agent, Holochain, HTTP, hash, language context, emit and storage functions. [Language isolates](languages/runtime/isolates.md)

**Admin credential** (`Ad4mConfig.admin_credential`, `is_admin_credential_token_on`): The static secret token that makes a caller the operator; `run` refuses to start without one unless `insecure_no_admin_credential` is set. [Operator and main agent](agents/access/operator-and-main-agent.md)

**Agent** (`AgentContext`): A DID with an Ed25519 key pair in the executor's wallet, on whose behalf the executor signs and stores data. [Agents](agents/README.md) — not: user (a user account is only the login of a managed agent)

**Agent context** (`AgentContext::{Main, Managed { did }}`): The value that names which agent a call acts as; the language runtime holds it in a thread-local while a call runs. [Identity](agents/identity/README.md)

**Agent language** (`LanguageController::get_agent_language`, `bootstrap-languages/centralized-agent-language/`): The expression language behind the `did` alias, whose expressions are agent profiles keyed by DID. [Agent languages](languages/other-kinds/agent-languages.md)

**Agent profile** (`Agent` in `types/domain.rs`): The public record of one agent: its DID, its public perspective and its direct-message language address. [Agent profiles](agents/identity/profiles.md) — not: public profile, `Agent` record

**AI service** (`AIService`, `rust-executor/src/ai_service/`): The executor service that runs local and remote LLMs, embedding models, transcription and AI tasks. [AI service](agents/executor/services/ai.md)

**AI task** (`tasks` table, `AIService::add_task`): A stored system prompt with examples, bound to one model. [AI service](agents/executor/services/ai.md)

**ALDK** (`ad4m-ldk/`): The AD4M Language Development Kit, for writing languages in TypeScript (`defineLanguage`) or in Rust compiled to WASM (`ad4m_language!`). [Language interface](languages/runtime/interface.md)

**Alias** (`LanguageController::language_aliases`): One of the names `did`, `lang`, `neighbourhood` and `perspective` that stand for the address of a system language. [Language addresses](languages/expressions/addresses.md)

**Anonymous caller** (`Principal::Anonymous`, `anonymous_capabilities`): A session opened without a token, which may only request a capability and, in multi-user mode, sign up or log in. [Sessions](agents/access/sessions.md)

**App data path** (`Ad4mConfig.app_data_path`, default `~/.ad4m`): The directory that holds all state of one executor. [Executor data](agents/executor/data.md)

**App grant** (`generate_capability_token`): A JWT that the executor issues to an app after the operator approves the app's capability request. [App grants](agents/access/app-grants.md) — not: cap token

**Apps registry** (`apps_map.rs`, file `apps_data.json`): The file that records every issued app grant with its app info and revoked flag, for listing and revocation. [App grants](agents/access/app-grants.md)

**Auto-processor** (`AutoProcessorConfig`, `auto_processor_supervisor`): A declared instance in a perspective that makes the executor run interpretation by itself on new messages in a scope. [Auto-processor](perspectives/social-dna/interpretation/auto-processor.md)

**Base** (`FlowInstance.subject`, `expression_address` in `create_subject`): The URI that an instance, an interpretation overlay or a flow instance is about. [Flows](perspectives/social-dna/flows/README.md) — not: base expression, base URI

**Batch** (`create_batch`, `commit_batch`, `batch_store`): An in-memory set of link changes that the executor stores and commits as one diff. [Link write path](perspectives/links/write-path.md)

**Billing** (`rust-executor/src/billing.rs`): The charging of compute credits to managed agents for link writes and AI calls. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**Bootstrap languages** (`bootstrap-languages/`): The language packages that ship in the repository and that bootstrap seeds point at. [Bootstrap languages](languages/runtime/bootstrap-languages.md)

**Bootstrap seed** (`BootstrapSeed`, `mainnet_seed.json`): The JSON file that gives an executor its language-language bundle, the system language addresses, the known link languages and the seed's trusted agents. [Bootstrap languages](languages/runtime/bootstrap-languages.md) — not: network seed (that is the Holochain DNA `network_seed`)

**Broadcast** (`PerspectiveInstance::send_broadcast`, `neighbourhood.sendBroadcast`): An ephemeral signed payload sent to all agents of a neighbourhood. [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md)

**Call** (SDK `Session` in `core/src/neighbourhood/Session.ts`, `createSession`): An audio and video connection among members of a neighbourhood, over mesh or the SFU. [Neighbourhood calls](perspectives/neighbourhoods/calls.md) — not: session (a session is the auth `Session`)

**Call policy** (`CallPolicy`, `SfuConfig`, link predicate `ad4m://sfu_config`): A neighbourhood's call mode (`mesh`, `designated`, `gateway`, `cascaded`) and mesh size, stored as a link authored by the neighbourhood's creator. [SFU](agents/executor/services/sfu.md)

**Call topology** (`SessionTopology`: `mesh`, `sfu`, `auto`): The client-side choice of how one call connects: directly between participants, or through an SFU. [Neighbourhood calls](perspectives/neighbourhoods/calls.md)

**Capability** (`Capability { with: Resource, can }` in `agent/capabilities/types.rs`): A domain, a list of pointers and a list of verbs that together name a kind of action a caller may take. [Capabilities](agents/access/capabilities.md). For languages, see Language capability.

**Capability request** (`agent.requestCapability`, `agent.permitCapability`, `agent.generateJwt`): An app's request for a set of capabilities, approved by the operator and redeemed with a six-digit code. [App grants](agents/access/app-grants.md)

**Cascade** (`rust-executor/src/sfu/cascade.rs`, `sfu/gossip/`): Several executors' SFUs that share load and pipe media between them. [SFU](agents/executor/services/sfu.md)

**Co-owner** (`PerspectiveHandle.owners`): One of several agents on one executor who joined the same neighbourhood and so share one perspective. [Publishing and joining](perspectives/neighbourhoods/publishing-and-joining.md)

**Compute credits** (`get_agent_credits`, `deduct_credits_and_log`): The balance that a managed agent spends on billed work, stored on its `users` row. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**Conductor** (`HolochainService`): The embedded Holochain runtime inside the executor. [Holochain service](agents/executor/services/holochain.md)

**Consensus rule** (`ConsensusRule` in `shacl_parser.rs`): The number of distinct DIDs, optionally holding a role, that must sign a flow transition before it settles. [Flows](perspectives/social-dna/flows/README.md)

**Delta update** (`result_delta`, `send_delta_update`): The change of a live query's result since its previous revision, sent to the subscribing connection. [Live queries](perspectives/querying/live-queries.md)

**Derived state** (`DerivedState`): A flow instance's state as the fold computes it from the links present now. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**DID** (`did_for_context`, `WalletBackend::get_did_document`): The identifier of an agent, on `dev` always a `did:key` that contains the Ed25519 public key. [Identity](agents/identity/README.md)

**Direct-message language** (`Agent.direct_message_language`): The language address in an agent profile through which other agents would send that agent messages; direct messages are not implemented on `dev` ([Executor services](agents/executor/services/README.md)). [Agent profiles](agents/identity/profiles.md)

**Embedded mode** (ad4m-connect, `AD4M_CONFIG` message): The ad4m-connect mode in which a host page passes the executor URL and token to an app by `postMessage`. [ad4m-connect](agents/interfaces/typescript-sdk/ad4m-connect.md)

**Entanglement proof** (`EntanglementProof`, `entanglement_service`): Signatures in both directions that link the main agent's DID to another key, such as a device key or a Holochain agent key. [Signing and verification](agents/identity/signing-and-verification.md)

**Event** (`events_ws::events`, `event_specs()`): A typed message that the executor pushes on a socket that asked for it with `events.watch`. [Events](agents/interfaces/events.md) — not: subscription (that is a live query), notification

**Event interest** (`events.watch`, `events.unwatch`, `EventInterest` in `api/event_interest.rs`): The event types, each optionally narrowed to some perspectives, that one socket asked for; a socket with no interest gets no events. [Events](agents/interfaces/events.md)

**Evidence seal** (`recompute_evidence_seal`): A hash over the content that a transition's guard cited, written by the proposer and checked again by each voter. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Executor** (crate `ad4m-executor`, `rust_executor::run`): The ADAM process that hosts agents, perspectives, language runtimes, executor services and the APIs. [Executor](agents/executor/README.md) — not: node, ADAM node, conductor (that is Holochain's)

**Executor service** (`ai_service/`, `sfu/`, `holochain_service/`, `billing.rs`): A Rust module inside the executor that is not a language, such as AI, SFU, Holochain, billing, email or the runtime service. [Executor services](agents/executor/services/README.md)

**Expression** (`Expression<T>`, `create_signed_expression`): A piece of data signed by an agent: `{author, timestamp, data, proof}`. [Expressions and addresses](languages/expressions/README.md)

**Expression language** (`Capability::ExpressionGet`, `ExpressionCreate`): A language that exports `expressionGet` and usually `expressionCreate`, and stores one kind of signed data. [Other language kinds](languages/other-kinds/README.md)

**Expression proof** (`ExpressionProof`, `verify_expression_proof`): The key and signature of an expression; the reading executor adds `valid` or `invalid`. [Signing and verification](agents/identity/signing-and-verification.md)

**Expression URL** (`ExpressionRef::try_from`, `parse_expr_url`): `<language address or alias>://<expression address>`, a DID, or a `literal:` URL. [Expressions and addresses](languages/expressions/README.md)

**File storage language** (`bootstrap-languages/file-storage/`, `centralized-file-storage/`): An expression language whose expressions are files. [File storage languages](languages/other-kinds/file-storage-languages.md)

**Flow** (`SHACLFlow`): A state machine stored as social DNA: states, transitions, guards and a consensus rule. [Flows](perspectives/social-dna/flows/README.md)

**Flow evaluator** (`perspectives/flow_evaluator.rs`): The pass after interpretation that checks the guards of reachable states and writes a transition proposal for each guard that now holds. [Flow spawn and guards](perspectives/social-dna/flows/spawn-and-guards.md)

**Flow instance** (`FlowInstance`): One run of a flow bound to one base. [Flows](perspectives/social-dna/flows/README.md)

**Flow receipt** (`FlowReceipt`): A content-addressed record of a finished flow run that any executor can check by running the fold again. [Flow receipts](perspectives/social-dna/flows/receipts.md)

**Flow spawn** (`run_flow_spawn_pass`): The automatic start of flow instances on instances that an interpretation run created. [Flow spawn and guards](perspectives/social-dna/flows/spawn-and-guards.md)

**Fold** (`fold`, `fold_read_set`): The pure function that walks a flow from its first state over the read set and returns the derived state. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Free hosting** (`get_free_hosting_enabled`): The executor setting, on by default, under which nothing is billed. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**Grant** (`Claims.capabilities`, `Session.caps`): The list of capabilities that a session's token carries. [Capabilities](agents/access/capabilities.md)

**Guard** (`requires` on `FlowState`): A model query on a flow state that must match before a transition into that state may be proposed. [Flow spawn and guards](perspectives/social-dna/flows/spawn-and-guards.md)

**Hard-wired class** (`hardwired_class.rs`, `hardwired_sdna/`): A subject class that the executor registers itself from bundled JSON, such as `FlowInstance`, `InterpretationRun` or `AutoProcessor`. [Social DNA](perspectives/social-dna/README.md) — not: built-in class (except for the abstract `ad4m://TextContent`)

**Harness** (`ai_service/harness/`, `run_with_tools`): The tool-calling LLM loop that interpretation can use instead of a single prompt. [Interpretation](perspectives/social-dna/interpretation/README.md)

**Holochain service** (`HolochainService`, `HolochainServiceInterface`): The executor service that runs the conductor behind one actor channel. [Holochain service](agents/executor/services/holochain.md)

**Host rates** (`host_rates` table, `bill_ai_operation`): The prices in HOT per billed operation or model name. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**Hosted mode** (`wallet_backend = "shared"`, `db_backend = "shared"`, `SharedWallet`, `SharedDb`): An executor that reads its wallet and its user table from a hosting service over HTTP. [Executor data](agents/executor/data.md) — not: shared mode

**Hosting** (`api/hosting_ws.rs`): The executor's offer to managed agents: host rates, a hot wallet and credit top-ups. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**Hosting service** (`wallet_backend_url`, `INTERNAL_API_TOKEN`): The external service that serves keys and users to executors in hosted mode and calls their `/health` and `/internal/shutdown` routes. [Executor data](agents/executor/data.md)

**Installing (a language)** (`language_by_ref`, `install_language`, `install_language_from_address`): Getting a bundle by address, checking its hash, saving it and loading it into an isolate. [Installing languages](languages/runtime/installing/README.md)

**Instance** (`create_subject`, `update_subject`, MCP `instance_*` tools): A base whose links satisfy a subject class's shape; in full, a subject instance. [Subject classes](perspectives/social-dna/subject-classes.md) — not: record, item, subject (alone)

**Interaction** (`expression.interactions`, `expression.interact`, the language export `interactions`): A named action that a language offers on one of its expressions and that a caller can run through the executor. [Expressions and addresses](languages/expressions/README.md)

**Interpretation** (`run_interpretation`): An executor pass that sends a transcript and the perspective's subject classes to an LLM and writes what it extracts as instances. [Interpretation](perspectives/social-dna/interpretation/README.md)

**Interpretation overlay** (`InterpretationOverlay`): A second instance on the same base that records the LLM's inferred values and the run that wrote them, so a person can accept or reject them. [Interpretation overlay](perspectives/social-dna/interpretation/overlay.md)

**Interpretation run** (`InterpretationRun`): The record of one run of interpretation: model, prompt version, time and sources. [Interpretation](perspectives/social-dna/interpretation/README.md)

**JWT signing key** (`signing_key_name()`): The wallet key whose secret signs and checks JWTs: `main` on a local wallet, `platform` in hosted mode. [Wallet and keys](agents/identity/wallet-and-keys.md)

**Keystore** (`AgentStore.keystore`, `KeystoreEnvelope`): The encrypted form of all wallet keys inside `agent.json`. [Wallet and keys](agents/identity/wallet-and-keys.md)

**Known link languages** (`RuntimeService::get_know_link_languages`): The link language addresses from the bootstrap seed plus the database table `known_link_languages`; the executor loads them after the Holochain conductor starts. [Bootstrap languages](languages/runtime/bootstrap-languages.md)

**Language** (`rust-executor/src/languages/`): A JavaScript bundle, loaded into its own isolate, that does some of these, one per language capability it exports: create and fetch expressions; commit, sync, render and query links; list peers; carry telepresence. [Languages](languages/README.md)

**Language address** (`calculate_language_hash`, op `hash`): `"Qm"` followed by the base58btc multibase string of a CIDv1 (codec `0x00`) over the SHA-256 of the bundle text. [Language addresses](languages/expressions/addresses.md)

**Language capability** (`Capability` in `languages/capability.rs`): One function group that a language exports, detected once at load; the presence of the export is the capability. [Language interface](languages/runtime/interface.md) — not: capability (alone), which means the access capability

**Language context** (`LanguageContext`): The data a language gets at load: main agent DID, signing key id, settings, storage directory and address. [Language runtime](languages/runtime/README.md)

**Language controller** (`LanguageController`): The global registry of loaded languages and the only way Rust code calls into a language. [Language controller](languages/runtime/controller.md)

**Language isolate** (`JsCore::new_for_language`, `LanguageRuntimeHandle`): One Deno worker, on its own OS thread, that runs one language bundle in a sandbox. [Language isolates](languages/runtime/isolates.md) — not: language instance

**Language language** (`bootstrap-languages/language-language/`, alias `lang`): The system language that stores language bundles and their language meta. [Language language](languages/runtime/installing/language-language.md)

**Language meta** (`LanguageMeta`): The signed record that the language language holds for a bundle: name, address, author and template fields. [Language language](languages/runtime/installing/language-language.md)

**Language runtime** (`rust-executor/src/languages/`, `rust-executor/src/js_core/`): The part of the executor that loads and runs languages. [Language runtime](languages/runtime/README.md)

**Launcher** (`ui/src-tauri`): The desktop application that embeds the executor, holds the admin credential and shows the operator's UI. [Executor](agents/executor/README.md)

**Link** (`Link` in `types/core.rs`): A triple of source, optional predicate and target. [Links](perspectives/links/link-expressions.md)

**Link expression** (`LinkExpression`, `DecoratedLinkExpression`): A link signed by its author, with a timestamp, a proof and a link status. [Links](perspectives/links/link-expressions.md)

**Link language** (`Language::commit`, `Language::sync`): A language that holds and syncs the shared links of one neighbourhood and carries its telepresence. [Link languages](languages/link-languages/README.md)

**Link server** (`link-server/`): A self-hosted Node.js server that stores the links of link-server rooms for the server link language. [Link server](languages/link-languages/link-server.md)

**Link-server operator** (`link-server/src/operator.ts`): The person who runs a link server. [Link server](languages/link-languages/link-server.md) — not: operator (alone), which is the executor's administrator

**Link-server room** (`/rooms/:roomId/*`): One neighbourhood's link store on a link server, named by the `UID` template value; its first authenticated DID is its admin. [Link server](languages/link-languages/link-server.md) — not: room (alone)

**Link status** (`LinkStatus`): Whether a link is `Shared` (committed to the neighbourhood) or `Local` (kept on this executor). [Local and shared links](perspectives/links/local-and-shared.md)

**Literal expression** (`Literal`, `literal_encode`, `literal_decode`): A value encoded in the URL itself: `literal:string:`, `literal:number:`, `literal:boolean:` or `literal:json:`. [Literal expressions](languages/expressions/literals.md) — not: literal target (a literal target is a literal expression in a link's target)

**Live query** (`SubscribedQuery`, `perspective.subscribeQuery`, `perspective.modelSubscribe`): A SPARQL or model query that the executor runs again after relevant writes and whose delta updates go only to the RPC connection that opened it. [Live queries](perspectives/querying/live-queries.md) — not: query subscription, subscription

**Local bootstrap languages** (`bootstrap-languages/local/`): Plain JavaScript system languages that run ADAM with no Holochain and no central service. [Local link language](languages/link-languages/local-link-language.md)

**Local link language** (`bootstrap-languages/local/link-language.js`): A link language with no peers that keeps links in the executor's key-value store. [Local link language](languages/link-languages/local-link-language.md)

**Locked** (`AgentService::{lock, unlock}`, `AuthError::Locked`): The wallet state in which the keys are encrypted; nothing signs and no token is checked until the operator unlocks it. [Wallet and keys](agents/identity/wallet-and-keys.md)

**Main agent** (`AgentContext::Main`, wallet key `main`): Agent #0 of an executor, created by `agent.generate` and owned by the operator. [Operator and main agent](agents/access/operator-and-main-agent.md)

**Managed agent** (`AgentContext::Managed { did }`): An agent other than the main agent that the executor hosts for a user account in multi-user mode. [Managed agents](agents/executor/managed-agents.md) — not: user, hosted agent

**MCP server** (`rust-executor/src/mcp/server.rs`, `Ad4mMcpHandler`): The executor's Model Context Protocol server that exposes tools to AI agents. [MCP server](agents/interfaces/mcp-server.md)

**Mesh** (`SfuConfig.mode == "mesh"`, `MeshManager`): A call in which every participant sends media to every other directly; the executor forwards none. [Neighbourhood calls](perspectives/neighbourhoods/calls.md)

**Model class** (SDK `Ad4mModel`, `@Model` in `core/src/model/decorators.ts`): A TypeScript class whose decorators generate a subject class and whose methods run model queries. [Model classes](agents/interfaces/typescript-sdk/model-classes.md)

**Model query** (`execute_model_query`, `ModelQueryInput`): A query for instances of subject classes, built from their SHACL shapes and run in the executor. [Model queries](perspectives/querying/model-queries.md)

**Model shape** (`ModelShape`, `load_shape_from_store`): The parsed, cached form of a subject class's SHACL that model queries are built from. [Model queries](perspectives/querying/model-queries.md)

**Multi-user mode** (`enable_multi_user`, `Ad4mDb::get_multi_user_enabled`): The executor setting that lets anonymous callers sign up and log in as managed agents. [Managed agents](agents/executor/managed-agents.md)

**Neighbourhood** (`Neighbourhood`, `rust-executor/src/neighbourhoods.rs`): A perspective shared between agents through a link language, published at a `neighbourhood://` URL. [Neighbourhoods](perspectives/neighbourhoods/README.md)

**Neighbourhood creator** (author of the neighbourhood expression): The agent who signed a neighbourhood's expression; the call policy trusts only this agent's links. [SFU](agents/executor/services/sfu.md) — not: neighbourhood owner (an owner is a local `owners` entry)

**Neighbourhood expression** (`NeighbourhoodExpression`, `DecoratedNeighbourhoodExpression`): The signed `Neighbourhood { link_language, meta }` stored in the neighbourhood language. [Neighbourhood and perspective languages](languages/other-kinds/neighbourhood-and-perspective-languages.md)

**Neighbourhood language** (`bootstrap-languages/neighbourhood-language/`, alias `neighbourhood`): The system language that stores neighbourhood expressions. [Neighbourhood and perspective languages](languages/other-kinds/neighbourhood-and-perspective-languages.md)

**Notification** (`notification_check_loop`, `runtime_service/notification_access.rs`): An app-registered SPARQL trigger whose new matches go to the owner's events and to an optional webhook. [Live queries](perspectives/querying/live-queries.md)

**Online status** (`set_online_status`, `OnlineAgent`, `hosted_status.rs`): An agent's signed, ephemeral status in a neighbourhood. [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md)

**Op** (`#[op2]` in `js_core/*_extension.rs`): A Rust function exposed to the JavaScript of a language isolate. [Language isolates](languages/runtime/isolates.md)

**Operator** (`Principal::Operator`): The executor's administrator role, held by whoever presents the admin credential; it is not an agent and acts as the main agent. [Operator and main agent](agents/access/operator-and-main-agent.md) — not: admin, owner (an owner is a perspective owner)

**Owner** (`PerspectiveHandle.owners`, `is_owned_by`): An agent whose DID is in a perspective's `owners` list and who may therefore reach that perspective. [Access checks](agents/access/checks.md)

**Oxigraph store** (`SparqlStore` in `perspectives/sparql_store.rs`): The per-perspective Oxigraph RDF store on RocksDB that holds all of a perspective's links. [Oxigraph store](perspectives/links/oxigraph-store.md) — not: SPARQL store, link store

**Peers (local, remote)** (`Language::set_local_agents`, `Language::others`): The agents on this executor that use a link language, and the other agents that the link language reports. [Link languages](languages/link-languages/README.md)

**Pending diff** (`perspective_diff` table, `add_pending_diff`, `pending_diffs_loop`): A shared diff that the link language did not accept yet, queued in SQLite for retry. [Neighbourhood sync](perspectives/neighbourhoods/sync.md)

**Perspective** (`PerspectiveInstance`, `PerspectiveHandle`): A graph of signed links that the executor stores for one or more owning agents. [Perspectives](perspectives/README.md)

**Perspective diff** (`PerspectiveDiff`, `DecoratedPerspectiveDiff`): A set of link additions and removals. [Links and store](perspectives/links/README.md)

**Perspective diff sync** (`bootstrap-languages/p-diff-sync/`): The Holochain link language that syncs diffs as a revision graph. [Perspective diff sync](languages/link-languages/perspective-diff-sync.md) — short form: p-diff-sync

**Perspective handle** (`PerspectiveHandle` in `types/domain.rs`): The stored description of a perspective: uuid, name, owners, neighbourhood and perspective state. [Perspectives](perspectives/README.md)

**Perspective language** (`bootstrap-languages/perspective-language/`, alias `perspective`): The system language meant for perspective snapshots; a stub that stores nothing today. [Neighbourhood and perspective languages](languages/other-kinds/neighbourhood-and-perspective-languages.md)

**Perspective state** (`PerspectiveState`): Where a perspective stands with its neighbourhood: `Private`, `NeighbourhoodCreationInitiated`, `LinkLanguageFailedToInstall`, `LinkLanguageInstalledButNotSynced` or `Synced`. [Neighbourhood sync](perspectives/neighbourhoods/sync.md) — not: sync state

**Principal** (`Principal` in `agent/session.rs`): The kind of caller behind a session: operator, agent or anonymous. [Sessions](agents/access/sessions.md)

**Processing claim** (`ProcessingClaim`, `try_claim`): A shared link that an executor writes before it runs an auto-processor batch, so that peers back off. [Auto-processor](perspectives/social-dna/interpretation/auto-processor.md)

**Pub/sub topic** (`PubSub`, `*_TOPIC` in `rust-executor/src/pubsub.rs`): A named in-process broadcast channel on which executor code publishes a change once; every socket's event stream reads it. [Events](agents/interfaces/events.md)

**Public perspective** (`Agent.perspective`): The signed links in an agent profile that describe the agent. [Agent profiles](agents/identity/profiles.md)

**Publishing (a language)** (`publish_language`, `apply_template`): Storing a bundle and its signed language meta in the language language. [Installing languages](languages/runtime/installing/README.md)

**Publishing (a neighbourhood)** (`neighbourhood.publish`, `neighbourhood_publish_from_perspective_with_context`): Turning a perspective into a neighbourhood by storing a neighbourhood expression and recording its URL on the perspective. [Publishing and joining](perspectives/neighbourhoods/publishing-and-joining.md)

**Read set** (`ReadSet`): All signed links that a flow state read uses: proposals, votes, role grants and tombstones. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Reifier** (`make_reifier_iri`): The RDF 1.2 node that carries a stored link's author, timestamp, proof, verdict and status. [Oxigraph store](perspectives/links/oxigraph-store.md)

**Replica state** (`FlowReplicaState`): Per-agent flow bookkeeping kept in SQLite outside the graph: last derived state and fired marks. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Request context** (`RequestContext` in `types/domain.rs`): The value that each RPC handler receives: one `Arc<Session>`, a cancel token and the RPC connection id. [Sessions](agents/access/sessions.md)

**Revision** (`Language::current_revision`): A link language's name for its current state: in p-diff-sync a diff-entry hash, in the link server the XOR of the active link hashes. [Link languages](languages/link-languages/README.md)

**Role gate** (`ConsensusRule.from_role`, `flow_instance/roles/`): A model query that a voter must match, as of the vote's time, for the vote to count. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Room key** (`bootstrap-languages/server-link-language/src/encryption.ts`): The AES-256-GCM key that encrypts links in a link-server room, sealed to each member's X25519 key. [Server link language](languages/link-languages/server-link-language.md)

**RPC API** (`rust-executor/src/api/ws_rpc.rs`, `HandlerMap`): The executor's WebSocket at `/api/v1/ws` that carries method calls, events and live-query updates. [RPC API](agents/interfaces/rpc-api.md) — not: GraphQL API (removed)

**Runtime service** (`RuntimeService`, `rust-executor/src/runtime_service/`, `runtime.*` RPC): The executor service that holds the bootstrap seed and the trusted-agent, known-link-language and friend lists; its `runtime.*` calls also cover notifications, export and import, and quitting. [Executor services](agents/executor/services/README.md)

**Scribe** (`gossip()` in `bootstrap-languages/p-diff-sync/index.ts`): In p-diff-sync, the one agent (lowest DID among known peers) that may make merge commits. [Perspective diff sync](languages/link-languages/perspective-diff-sync.md)

**Server link language** (`bootstrap-languages/server-link-language/`): The link language that syncs through a link server, with end-to-end encryption. [Server link language](languages/link-languages/server-link-language.md)

**Session** (`Session` in `agent/session.rs`): A connection's identity: a principal, its capabilities and its token's expiry, checked again on each request and event. [Sessions](agents/access/sessions.md) — not: call (an SDK call object is also named `Session`), MCP session (MCP keeps its own per-session token)

**SFU** (`SfuService`, `rust-executor/src/sfu/`): The WebRTC selective forwarding unit inside the executor that relays call media. [SFU](agents/executor/services/sfu.md)

**SFU room** (`SfuRoom`): One call on an SFU, keyed by neighbourhood URL and room name. [SFU](agents/executor/services/sfu.md) — not: room (alone)

**SHACL shape** (`SHACLShape`): The W3C SHACL description of a subject class, stored as links. [Subject classes](perspectives/social-dna/subject-classes.md)

**Signal** (`PerspectiveInstance::send_signal`, `neighbourhood.sendSignal`): An ephemeral signed payload sent to one agent of a neighbourhood. [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md) — a Holochain signal is a different thing ([Holochain service](agents/executor/services/holochain.md))

**Social DNA (SDNA)** (`add_sdna`, `SdnaType`): The subject class and flow definitions stored as shared links in a perspective. [Social DNA](perspectives/social-dna/README.md)

**Source language** (`LanguageMeta::template_source_language_address`): The language that a templated language was made from. [Language trust and templates](languages/runtime/installing/trust-and-templates.md)

**SPARQL query** (`SparqlStore::query_arbitrary`, `perspective.querySparql`): A read-only SPARQL 1.2 query over a perspective's Oxigraph store. [SPARQL queries](perspectives/querying/sparql.md)

**Subject class** (`SdnaType::SubjectClass`, `parse_shacl_to_links`): A named SHACL shape with actions, stored in a perspective. [Subject classes](perspectives/social-dna/subject-classes.md) — not: model (use model class for the SDK side)

**System language** (`is_system_language`, `SystemLanguageAddresses`): A language from the bootstrap seed or a known link language, loaded with extra file access. [Bootstrap languages](languages/runtime/bootstrap-languages.md) — not: core language

**Telepresence** (`has_telepresence_adapter`, `Language::set_online_status`, `send_signal`, `send_broadcast`): The ephemeral, unstored side of a neighbourhood: online status, signals and broadcasts, carried by the link language. [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md) — not: presence (alone)

**Template variable** (`//!@ad4m-template-variable`, `apply_template_data`): A constant in a bundle, marked by a comment line, that templating replaces. [Language trust and templates](languages/runtime/installing/trust-and-templates.md)

**Templated language** (`language_apply_template_on_source`): A language made by templating a source language; its meta carries `templateAppliedParams` and `templateSourceLanguageAddress`. [Language trust and templates](languages/runtime/installing/trust-and-templates.md)

**Transition proposal** (`FlowTransitionProposal`): A signed set of links that proposes moving a flow instance to a state and carries its votes. [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Trusted agent** (`RuntimeService::get_trusted_agents`): A DID whose languages this executor installs without a template check: the seed list, the main agent and the database list. [Language trust and templates](languages/runtime/installing/trust-and-templates.md)

**TypeScript SDK** (`@coasys/ad4m`, `core/`, `Ad4mClient`, `ApiClient`): The TypeScript client of the RPC API, with the shared types and the model class decorators. [TypeScript SDK](agents/interfaces/typescript-sdk/README.md) — not: core (as a product name)

**Unyt** (`rust-executor/src/unyt_service.rs`, alliance DNA): The Holochain currency DNA (mHOT) that pays for hosting. [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**User account** (`users` table, `user_management.rs`): The login of a managed agent: email, DID and password hash. [Managed agents](agents/executor/managed-agents.md) — not: user (for the agent itself)

**User token** (`generate_user_jwt`): A JWT issued at login whose `sub` is a managed agent's DID and that carries the default user capabilities. [Managed agents](agents/executor/managed-agents.md) — not: user JWT, login JWT

**Wallet** (`WalletBackend`, `LocalWallet`, `SharedWallet`): The store of every Ed25519 key of the executor, by name, locked under the operator's passphrase. [Wallet and keys](agents/identity/wallet-and-keys.md). For the encrypted file form, see Keystore.
