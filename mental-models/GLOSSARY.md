# Glossary

This file defines each term that ADAM uses in its own way, once, in alphabetical order. Each entry gives the term, its code names in brackets, the definition, one usage example, and the page that owns the term. "not: …" lists words that pages must not use for the term. A word is on a "not:" list only when it never names anything else in the repository. General technical terms and external products keep their usual meaning and have no entry, such as DID, SPARQL, RDF, SHACL, JSON-RPC, WebSocket, JWT, WebRTC, SFU, ICE, MCP, Holochain and Unyt. An entry states only what is specific to ADAM.

**Abstract class** (`model_query/abstract_classes.rs`, built-ins `ad4m://TextContent`, `ad4m://Mentioning`, `ad4m://Container` in `model_query/builtin_classes.rs`): A subject class that reads the instances of several concrete classes and sends each write to the instance's own classes. No caller can create an instance of an abstract class. Example: "`ad4m://TextContent` is an abstract class that reads every instance with text." [Subject classes](perspectives/social-dna/subject-classes.md)

**Access check** (`Guard` in `api/guards.rs`, `get_perspective_with_access`): The code that applies a session's grant to one RPC call or MCP tool call. It checks a capability, an ownership, or both. Example: "The access check refuses a call on a perspective that the session's agent does not own." [Access checks](agents/access/checks.md). Event delivery: [Events](agents/interfaces/events.md). Language calls: [Signing and verification](agents/identity/signing-and-verification.md)

**Action** (`AD4MAction`, `Command`): A JSON list of commands that a subject class stores for its constructor, its destructor, or the setter, adder or remover of a property. Example: "The constructor action of the class writes one `ad4m://type` link." [Subject classes](perspectives/social-dna/subject-classes.md)

**ad4m-connect** (`@coasys/ad4m-connect`, `connect/`): The browser and Node library that finds an executor, runs a login flow and gives an app an authenticated `Ad4mClient`. Example: "The app gets its `Ad4mClient` from ad4m-connect." [ad4m-connect](agents/interfaces/typescript-sdk/ad4m-connect.md)

**ad4m:host** (`rust-executor/src/js_core/host.js`): The ES module that every language imports to call the executor: agent, Holochain, HTTP, hash, context, emit and storage functions. Example: "The language reads its agent DID through `ad4m:host`." [Language isolates](languages/runtime/isolates.md)

**Admin credential** (`Ad4mConfig.admin_credential`, `is_admin_credential_token_on`, `insecure_no_admin_credential`): The static secret token that makes a caller the operator. `run` does not start the executor without one, unless the config sets `insecure_no_admin_credential`. Example: "The launcher holds the admin credential." [Operator and main agent](agents/access/operator-and-main-agent.md)

**Agent** (`AgentContext`, `did_for_context`): An Ed25519 key pair in the executor's wallet, named by its `did:key` DID. The executor signs and stores data on behalf of the agent. Example: "The executor signs the link as the agent." [Agents](agents/README.md)

**Agent context** (`AgentContext::{Main, Managed { did }}`): The value that names the agent a call acts as. Each language call carries its own; the call's script holds it as a call token. Example: "A language call runs in the agent context of the main agent." [Identity](agents/identity/README.md)

**Agent language** (`LanguageController::get_agent_language`, `bootstrap-languages/centralized-agent-language/`): The expression language behind the `did` alias. Its expressions are agent profiles, keyed by DID. Example: "`agent.byDID` reads the profile from the agent language." [Agent languages](languages/other-kinds/agent-languages.md)

**Agent profile** (`Agent` in `types/domain.rs`): A DID, its public perspective and the address of its direct-message language. Example: "Other agents find an agent profile by DID alone." [Agent profiles](agents/identity/profiles.md) — not: public profile

**Agent session** (`perspective.runAgent`, `run_agent_session`): A conversation with an LLM in one perspective, which the harness runs as the caller. The LLM gets the perspective's static MCP reads and the writes that the caller names. Example: "`perspective.runAgent` starts an agent session in one perspective." [MCP server](agents/interfaces/mcp-server.md)

**AI model** (`ai.addModel`, `ai_models` in the config file): A name that the AI service registers for one model on one endpoint. Example: "`ai.addModel` registers `qwen` on the llama-server endpoint." [AI service](agents/executor/services/ai.md)

**AI service** (`AIService`, `ai_service/endpoints/`, `ai_service/local/`): The executor service that sends LLM, embedding, transcription and speech requests to models on endpoints. One-click local AI (`ai.localInstall`) installs model servers on the device and registers them as endpoints. Example: "The AI service sends the prompt to the model's endpoint." [AI service](agents/executor/services/ai.md)

**AI session** (`AiSession`, `ai.sessionSend`, `ai_sessions` and `ai_messages` tables): A conversation with the executor's AI that the executor stores in its own database. Each turn runs the harness with the MCP tools, within the session's capabilities and the caller's own. Example: "`ai.sessionSend` adds a turn to the AI session." [AI service](agents/executor/services/ai.md)

**AI task** (`AITask`, `ai.addTask`, `tasks` table): A stored system prompt with examples, bound to one AI model. `ai.prompt` sends a prompt through a task. Example: "The AI task `summarise` holds the system prompt for summaries." [AI service](agents/executor/services/ai.md)

**ALDK** (`ad4m-ldk/`, `defineLanguage`, `ad4m_language!`): The AD4M Language Development Kit. Developers use it to write languages in TypeScript or in Rust compiled to WASM. Example: "This link language uses the ALDK `defineLanguage`." [Language interface](languages/runtime/interface.md)

**Alias** (`LanguageController::language_aliases`): One of the names `did`, `lang`, `neighbourhood` and `perspective`, each of which stands for the address of a system language. Example: "`did://<did>` is an expression URL with the alias `did`." [Language addresses](languages/expressions/addresses.md)

**App grant** (`generate_capability_token`, `grants::mint`): The grant that an app gets when the owner of an agent approves the app's capability request. The app holds it as a JWT. Example: "The app grant lets the app read `perspective` on `*`." [App grants](agents/access/app-grants.md)

**Auto-processor** (`AutoProcessorConfig`, `auto_processor_supervisor`): An instance in a perspective that makes the executor run interpretation on new messages in a scope, with no caller. Example: "The auto-processor runs interpretation on each new message in the channel." [Auto-processor](perspectives/social-dna/interpretation/auto-processor.md)

**Base** (`FlowInstance.subject`, `expression_address` in `create_subject`): The URI that an instance, an interpretation overlay or a flow instance is about. Write "base", or "base URI" where a type name needs it. Example: "A flow instance binds one flow to one base." [Flows](perspectives/social-dna/flows/README.md) — not: base expression

**Batch** (`create_batch`, `commit_batch`, `batch_store`): A set of link changes in memory that the executor stores and commits as one perspective diff. Example: "The batch commits 40 link additions as one perspective diff." [Link write path](perspectives/links/write-path.md)

**Billing** (`rust-executor/src/billing.rs`, `deduct_credits_and_log`, `host_rates` table, `get_free_hosting_enabled`): The charge of compute credits to a managed agent for link writes and AI calls, at the prices in `host_rates`. Free hosting is on by default and bills nothing. Example: "Billing deducts credits for each link write of a managed agent." [Billing and hosting](agents/executor/services/billing-and-hosting.md)

**Bootstrap languages** (`bootstrap-languages/`, `bootstrap-languages/local/`): The language packages in this repository that bootstrap seeds point at. `bootstrap-languages/local/` holds plain JavaScript system languages that need no Holochain and no central service. Example: "The tests use the bootstrap languages in `bootstrap-languages/local/`." [Bootstrap languages](languages/runtime/bootstrap-languages.md)

**Bootstrap seed** (`BootstrapSeed`, `mainnet_seed.json`): The JSON file that gives an executor its language-language bundle, the system language addresses, the known link languages and the seed's trusted agents. Example: "The executor reads the system language addresses from the bootstrap seed." [Bootstrap languages](languages/runtime/bootstrap-languages.md)

**Broadcast** (`PerspectiveInstance::send_broadcast`, `neighbourhood.sendBroadcast`): A signed payload that the link language sends to all agents of a neighbourhood and does not store. Example: "A device sends a `beat` as a broadcast every 20 s." [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md)

**Call** (SDK `Session` in `core/src/neighbourhood/Session.ts`, `createSession`, `SessionTopology`): An audio and video connection among members of a neighbourhood. Media goes directly between the participants (mesh) or through the executor's SFU. In RPC pages, "method call" names a request to an RPC method. Example: "Three members join the call through the SFU." [Neighbourhood calls](perspectives/neighbourhoods/calls.md)

**Call policy** (`CallPolicy`, `SfuConfig`, link predicate `ad4m://sfu_config`): A link by the neighbourhood creator that sets the neighbourhood's call mode and mesh size. The call modes are `mesh`, `designated`, `gateway` and `cascaded`. Example: "The call policy sets the call mode `designated`." [SFU](agents/executor/services/sfu.md)

**Capability** (`Capability { with: Resource, can }` in `agent/capabilities/types.rs`): A domain, a list of pointers and a list of verbs that together name one kind of action a caller may take. For languages, see Language capability. Example: "The capability `perspective` `QUERY` on `*` lets a caller query any perspective that it owns." [Capabilities](agents/access/capabilities.md)

**Capability request** (`agent.requestCapability`, `agent.permitCapability`, `agent.generateJwt`): An app's request for a set of capabilities on one agent. The operator, or a session of that agent with `agent/PERMIT`, approves it, and the app redeems it with a six-digit code. Example: "The app sends a capability request and shows the six-digit code." [App grants](agents/access/app-grants.md)

**Consensus rule** (`ConsensusRule` in `shacl_parser.rs`, `ConsensusRule.from_role`): The number of different DIDs that must sign a transition proposal before the transition settles. An optional role is a model query that each voter must match at the time of the vote. Example: "The consensus rule needs two DIDs with the role `reviewer`." [Flows](perspectives/social-dna/flows/README.md)

**Device key** (an agent's `did:key` in a `did:scid` key log, `record_agent_if_listed`): An agent key that the person's key log lists as one of its devices; the agent then speaks for the person. The executor holds device keys only, never the master keys. Example: "The phone's agent is a device key of the person's identity." [Person identity](agents/identity/person-identity.md)

**Endpoint** (`Endpoint`, `EndpointKind`, `ai_endpoints` table, `ai.addEndpoint`): A model server that the AI service sends requests to, such as Ollama, llama-server, vLLM, OpenAI, Anthropic or a speech server. It holds the URL, the key, the health and the request limits. An AI model names one endpoint. A managed endpoint is one that local AI registers. Example: "`ai.addEndpoint` registers the llama-server at `127.0.0.1:12460`." [AI service](agents/executor/services/ai.md)

**Entanglement proof** (`EntanglementProof`, `entanglement_service`): Two signatures, one in each direction, that link the main agent's DID to another key, such as a device key or a Holochain agent key. Example: "The entanglement proof ties the Holochain agent key to the main agent's DID." [Signing and verification](agents/identity/signing-and-verification.md)

**Event** (`Event` and `Topic` in `events.rs`, `event_specs()`, `events.watch`): A typed message that the executor pushes to each socket that watches its type with `events.watch`. A socket gets it only when its session is in the audience that the publisher names and holds the event's capability. The key log's `Event` in `ad4m-identity-core` is a different thing. Example: "The socket sends `events.watch` for `flow-state-changed` and then gets that event." [Events](agents/interfaces/events.md)

**Executor** (crate `ad4m-executor`, `rust_executor::run`): The ADAM process that hosts agents, perspectives, language isolates, executor services and the APIs. Example: "The app connects to the executor over the RPC API." [Executor](agents/executor/README.md) — not: ADAM node

**Executor service** (`ai_service/`, `sfu/`, `holochain_service/`, `billing.rs`, `email_service.rs`): A Rust module in the executor that is not a language, such as AI, SFU, Holochain, billing, email or the runtime service. Example: "The SFU is an executor service." [Executor services](agents/executor/services/README.md)

**Expression** (`Expression<T>`, `create_signed_expression`, `ExpressionProof`, `verify_expression_proof`): Data that an agent signs, in the form `{author, timestamp, data, proof}`. The executor that reads it marks the proof `valid` or `invalid`. Example: "The executor marks the expression's proof `valid`." [Expressions and addresses](languages/expressions/README.md)

**Expression language** (`Capability::ExpressionGet`, `ExpressionCreate`): A language that stores one kind of expression and exports `expressionGet` and usually `expressionCreate`. Example: "The agent language is an expression language for agent profiles." [Other language kinds](languages/other-kinds/README.md)

**Expression URL** (`ExpressionRef::try_from`, `parse_expr_url`): The address of one expression: `<language address or alias>://<expression address>`, a DID, or a `literal:` URL. Example: "`neighbourhood://Qm…` is the expression URL of a neighbourhood expression." [Expressions and addresses](languages/expressions/README.md)

**Flow** (`SHACLFlow`): A state machine that social DNA stores: states, transitions, transition guards and a consensus rule. Example: "The flow moves a task from `todo` to `done`." [Flows](perspectives/social-dna/flows/README.md)

**Flow instance** (`FlowInstance`, `run_flow_spawn_pass`): One run of a flow on one base. The flow spawn pass starts flow instances on the instances that an interpretation run creates. Example: "The flow spawn pass starts a flow instance for each new task." [Flows](perspectives/social-dna/flows/README.md)

**Flow receipt** (`FlowReceipt`): A content-addressed record of a finished flow instance. Any executor checks it by running the fold again. Example: "A verifier outside the neighbourhood checks the flow receipt." [Flow receipts](perspectives/social-dna/flows/receipts.md)

**Fold** (`fold`, `fold_read_set`, `ReadSet`, `DerivedState`): The pure function that computes the state of a flow instance. It walks the flow from its first state over the signed proposals, votes, role grants and role revocations. The `fold` of `ad4m-hash-chain` computes chain state and is a different thing. Example: "Every executor runs the fold and gets the same state." [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Grant** (`LiveGrant` in `agent/grants.rs`, table `grants`): The executor's record of one issued token: the agent it acts as, its capabilities and the app that holds it. A token works only while its grant is active and not expired. Example: "The operator revokes the grant, and the token stops working." [App grants](agents/access/app-grants.md)

**Hard-wired class** (`hardwired_class.rs`, `hardwired_sdna/`): A subject class that the executor registers itself from bundled JSON, such as `FlowInstance`, `InterpretationRun` or `AutoProcessor`. Example: "`InterpretationRun` is a hard-wired class." [Social DNA](perspectives/social-dna/README.md)

**Harness** (`ai_service/harness/`, `run_with_tools`): The tool-calling LLM loop that runs agent sessions and AI sessions. Interpretation can use it in place of one prompt. Example: "The harness lets the LLM call MCP tools in a loop." [Interpretation](perspectives/social-dna/interpretation/README.md)

**Holochain service** (`HolochainService`, `HolochainServiceInterface`): The executor service that runs the embedded Holochain conductor behind one actor channel. Example: "Languages call Holochain through the Holochain service." [Holochain service](agents/executor/services/holochain.md)

**Hosted mode** (`wallet_backend = "shared"`, `db_backend = "shared"`, `SharedWallet`, `SharedDb`, `wallet_backend_url`, `db_backend_url`): An executor that reads its wallet, its users and the shared perspective metadata over HTTP from a hosting service. All other tables stay in the executor's own SQLite database. Identifiers keep the word `shared`. Example: "In hosted mode the executor reads its wallet from the hosting service." [Executor data](agents/executor/data.md)

**Identity store** (`identity-store/`, `identity.storeUrl`, `identity.storeKeys`): The shared store of key logs and agent records that the agent language reads and writes. The executor accepts a log head only when one of `identity.storeKeys` signed it. Example: "The agent language fetches the key log from the identity store." [Person identity](agents/identity/person-identity.md)

**Installing (a language)** (`language_by_ref`, `install_language`, `install_language_from_address`): Getting a bundle by its address, checking its hash, saving it and loading it into a language isolate. Example: "Installing a language checks the bundle's hash against its address." [Installing languages](languages/runtime/installing/README.md)

**Instance** (`create_subject`, `update_subject`, MCP `instance_*` tools): A base whose links match the shape of a subject class. The full name is subject instance. Example: "`instance_create` makes an instance of `Message`." [Subject classes](perspectives/social-dna/subject-classes.md)

**Interpretation** (`run_interpretation`, `InterpretationRun`): An executor pass that sends a transcript and the perspective's subject classes to an LLM and writes what the LLM extracts as instances. Each run stores an `InterpretationRun` with the model, prompt version, time and sources. Example: "The interpretation run extracted three tasks from the call transcript." [Interpretation](perspectives/social-dna/interpretation/README.md)

**Interpretation overlay** (`InterpretationOverlay`): A second instance on the same base that records the values the LLM inferred and the run that wrote them. A person uses it to accept or reject those values. Example: "The person accepts the interpretation overlay, and the values go to the real instance." [Interpretation overlay](perspectives/social-dna/interpretation/overlay.md)

**Key log** (`KelCache`, `ad4m-hash-chain/`, `ad4m-identity-core/`): The signed chain of events that defines a `did:scid` identity: its master keys and its device keys. The executor keeps each log it checked and trusts it up to the seq of the last store-signed head. Example: "Removing a device adds an event to the key log." [Person identity](agents/identity/person-identity.md)

**Keystore** (`AgentStore.keystore`, `KeystoreEnvelope`): The encrypted form of all wallet keys in `agent.json`. Example: "The operator's passphrase unlocks the keystore." [Wallet and keys](agents/identity/wallet-and-keys.md)

**Language** (`rust-executor/src/languages/`): A JavaScript bundle that runs in its own language isolate. Each language capability it exports adds one job: create and get expressions, commit, sync, render and query links, list peers, or carry telepresence. Example: "The language exports `expressionGet`, so it is an expression language." [Languages](languages/README.md)

**Language address** (`calculate_language_hash`, op `hash`): The ID of a language: `Qm` followed by the base58btc string of a CIDv1, with codec `0x00`, over the SHA-256 of the bundle text. Example: "Installing checks that the bundle hashes to the language address." [Language addresses](languages/expressions/addresses.md)

**Language capability** (`Capability` in `languages/capability.rs`): One function group that a language exports. The executor detects it once at load, and the presence of the export is the capability. Example: "The language has the capability `PerspectiveCommit`, so the executor can commit to it." [Language interface](languages/runtime/interface.md)

**Language controller** (`LanguageController`): The global registry of loaded languages. Rust code calls a language only through it. Example: "The executor asks the language controller for the agent language." [Language controller](languages/runtime/controller.md)

**Language isolate** (`JsCore::new_for_language`, `LanguageRuntimeHandle`): One Deno worker, on its own OS thread, that runs one language bundle in a sandbox. Example: "Each language runs in its own language isolate." [Language isolates](languages/runtime/isolates.md)

**Language language** (`bootstrap-languages/language-language/`, alias `lang`, `LanguageMeta`): The system language that stores language bundles and their signed language meta: name, address, author and template fields. Example: "Publishing a language stores its bundle in the language language." [Language language](languages/runtime/installing/language-language.md)

**Launcher** (`ui/src-tauri`): The desktop application that embeds the executor, holds the admin credential and shows the operator's UI. Example: "The launcher starts the executor with its admin credential." [Executor](agents/executor/README.md)

**Link** (`Link` in `types/core.rs`): A triple of source, optional predicate and target. Example: "The link `alice://self ad4m://has_post post://1` has a source, a predicate and a target." [Links](perspectives/links/link-expressions.md)

**Link expression** (`LinkExpression`, `DecoratedLinkExpression`): A link that its author signed, with a timestamp, a proof and a link status. Example: "`perspective.get()` returns link expressions." [Links](perspectives/links/link-expressions.md)

**Link language** (`Language::commit`, `Language::sync`): A language that stores and syncs the shared links of one neighbourhood and carries its telepresence. Example: "p-diff-sync is a link language." [Link languages](languages/link-languages/README.md)

**Link language sync status** (`emitSyncStateChange`): The state that a link language reports about its own sync with its peers. The executor uses it to set the perspective state. Example: "p-diff-sync reports `LinkLanguageInstalledButNotSynced` when peer revisions differ." [Link languages](languages/link-languages/README.md)

**Link server** (`link-server/`): A self-hosted Node.js server that stores the links of link-server rooms for the server link language. Example: "The link server stores the links of 12 link-server rooms." [Link server](languages/link-languages/link-server.md)

**Link-server operator** (`link-server/src/operator.ts`): The person who runs a link server. Example: "The link-server operator lists the rooms in the dashboard." [Link server](languages/link-languages/link-server.md)

**Link-server room** (`/rooms/:roomId/*`, `acl` table in `link-server/src/db.ts`): The link store of one neighbourhood on a link server, named by the `UID` template value. Its first authenticated DID is its admin, and the server checks each request against the room's list of admitted DIDs. The bare word "room" is ambiguous: write "link-server room" or "SFU room". Example: "The first DID that authenticates to an unknown room id creates the link-server room." [Link server](languages/link-languages/link-server.md)

**Link status** (`LinkStatus`): The status of a link: `Shared` when it is committed to the neighbourhood, `Local` when it stays on this executor. Example: "A link with the link status `Local` never leaves this executor." [Local and shared links](perspectives/links/local-and-shared.md)

**Literal expression** (`Literal`, `literal_encode`, `literal_decode`): A value that its URL holds in full: `literal:string:`, `literal:number:`, `literal:boolean:` or `literal:json:`. Example: "The link's target is the literal expression `literal:string:hello`." [Literal expressions](languages/expressions/literals.md)

**Live query** (`SubscribedQuery`, `perspective.subscribe`, SDK `LiveQuery`): A SPARQL query or model query that the executor runs again after relevant writes. The executor sends each change of the result only to the RPC connection that opened the query. Identifiers and event names keep the word "subscription"; prose says "live query". Example: "The app opens a live query and gets each change of the result." [Live queries](perspectives/querying/live-queries.md)

**Local AI** (`ai_service/local/`, `ai.localInstall`): The AI service part that installs model servers and models on the device, supervises them as child processes and registers them as managed endpoints. It downloads only when the operator asks. Example: "Local AI installs llama-server and registers it as a managed endpoint." [AI service](agents/executor/services/ai.md)

**Local link language** (`bootstrap-languages/local/link-language.js`): A link language with no peers that keeps links in the executor's key-value store. Example: "The tests use the local link language, so no Holochain runs." [Local link language](languages/link-languages/local-link-language.md)

**Local model** (`ai_service/local/`): An AI model that a managed endpoint serves from the device. Example: "The executor sends the prompt to a local model, not to OpenAI." [AI service](agents/executor/services/ai.md)

**Local peers** (`Language::set_local_agents`, `peersSetLocal`): The agents on this executor that use a link language. Example: "The executor sets the local peers when a second local agent joins." [Link languages](languages/link-languages/README.md)

**Locked** (`AgentService::{lock, unlock}`, `AuthError::Locked`): The wallet state in which the keys are encrypted. The executor signs nothing and checks no token until the operator unlocks the wallet. Example: "A locked wallet refuses every token." [Wallet and keys](agents/identity/wallet-and-keys.md)

**Main agent** (`AgentContext::Main`, wallet key `main`): Agent #0 of an executor, which `agent.generate` creates and the operator owns. Example: "The operator acts as the main agent." [Operator and main agent](agents/access/operator-and-main-agent.md)

**Main key** (wallet key `main`, `KEY_NAME_MAIN`): The wallet key of the main agent, which the operator's passphrase unlocks. It is not the master key of an identity in `ad4m-identity-client`, which is a different key that never reaches an executor. Example: "`agent.generate` writes the main key to the wallet." [Wallet and keys](agents/identity/wallet-and-keys.md)

**Managed agent** (`AgentContext::Managed { did }`): An agent other than the main agent that the executor hosts for a user account in multi-user mode. Identifiers and RPC text keep "managed user". Example: "Each signed-up user gets a managed agent." [Managed agents](agents/executor/managed-agents.md) — not: hosted agent

**Method guard** (`Guard` in `api/guards.rs`): The rule that each RPC method registers for who may call it. Dispatch checks the method's capability, then this rule, before the handler runs. The word "guard" alone means this. Example: "The method guard `Owns` checks that the session's agent owns the perspective." [Access checks](agents/access/checks.md)

**Model class** (SDK `Ad4mModel`, `@Model` in `core/src/model/decorators.ts`): A TypeScript class whose decorators generate a subject class and whose methods run model queries. Example: "`Message` is a model class, so `Message.findAll()` runs a model query." [Model classes](agents/interfaces/typescript-sdk/model-classes.md)

**Model query** (`execute_model_query`, `ModelQueryInput`, `ModelShape`): A query for the instances of subject classes, which the executor builds from their SHACL shapes and runs. Example: "The model query returns all `Message` instances with `author` equal to Alice." [Model queries](perspectives/querying/model-queries.md)

**Monotonic predicate** (`MONOTONIC_PREDICATES`, `Ending` in `perspectives/monotonic.rs`): A predicate, such as any predicate under `ad4m://flow/` and `ad4m://acceptedBy`, under which no diff removes a `Shared` link. Such a link ends only by its author's tombstone (`is_retractable`), or never (`ad4m://retracted`, `ad4m://monotonic`, `ad4m://flow/role_grant_revoked`). Example: "A peer's diff cannot remove a `Shared` link under `ad4m://acceptedBy`." [Link write path](perspectives/links/write-path.md)

**Multi-user mode** (`enable_multi_user`, `Ad4mDb::get_multi_user_enabled`): The executor setting that lets anonymous callers sign up and log in as managed agents. Example: "In multi-user mode, a caller signs up with an email and gets a managed agent." [Managed agents](agents/executor/managed-agents.md)

**Neighbourhood** (`Neighbourhood`, `rust-executor/src/neighbourhoods.rs`): A perspective that agents share through a link language, published at a `neighbourhood://` URL. Example: "Bob joins the neighbourhood with its `neighbourhood://` URL." [Neighbourhoods](perspectives/neighbourhoods/README.md)

**Neighbourhood creator** (author of the neighbourhood expression): The agent who signed a neighbourhood expression. The call policy trusts only the links of this agent. Example: "Only the neighbourhood creator can set the call policy." [SFU](agents/executor/services/sfu.md)

**Neighbourhood expression** (`NeighbourhoodExpression`, `DecoratedNeighbourhoodExpression`, `bootstrap-languages/neighbourhood-language/`): The signed `Neighbourhood { link_language, meta }` that the system language with the alias `neighbourhood` stores. Example: "Publishing stores the neighbourhood expression and gives its URL." [Neighbourhood and perspective languages](languages/other-kinds/neighbourhood-and-perspective-languages.md)

**Notification** (`notification_check_loop`, `runtime_service/notification_access.rs`): A SPARQL trigger that an app registers. The executor sends each new match to the owner's events and to an optional webhook. Example: "The app registers a notification for each new mention." [Live queries](perspectives/querying/live-queries.md)

**Operator** (`Principal::Operator`): The administrator role of the executor, which the caller who presents the admin credential holds. On an executor with no admin credential, a caller with no token on a loopback listener holds it. The operator is not an agent and acts as the main agent. Example: "Only the operator can start multi-user mode." [Operator and main agent](agents/access/operator-and-main-agent.md)

**Operator record** (`record_operator_at_init`, `operator_record`, `first_link_check`): The `did:scid` that `ad4m-executor init --operator` records as the operator's identity. The main agent's first link to an identity must go to it. Example: "The main agent can join only the identity the operator record names." [Person identity](agents/identity/person-identity.md)

**Owner** (`PerspectiveHandle.owners`, `is_owned_by`): An agent whose DID is in the `owners` list of a perspective, which lets the agent reach that perspective. Agents of one executor that join the same neighbourhood share one perspective as its owners. Example: "Alice and Bob are owners of the shared perspective on this executor." [Access checks](agents/access/checks.md)

**Oxigraph store** (`SparqlStore` in `perspectives/sparql_store.rs`, `make_reifier_iri`): The store that holds all links of one perspective, on RocksDB. For each link, a reifier holds the author, timestamp, proof, verdict and link status. Example: "The Oxigraph store keeps the link's proof on its reifier." [Oxigraph store](perspectives/links/oxigraph-store.md) — not: SPARQL store

**Person identity** (`did:scid:coasys:1`, `rust-executor/src/identity/`): A person's DID, defined by a key log that the person's master keys sign. Agents whose keys the log lists as devices speak for the person. Example: "The laptop and phone agents speak for one person identity." [Person identity](agents/identity/person-identity.md)

**Person profile** (`IdentityService::publish_person_profile` in `identity/person.rs`): The agent record published under a person's `did:scid`, signed by one of its device keys. A device agent's profile publish writes it too. Example: "The phone's new name shows on the person profile." [Person identity](agents/identity/person-identity.md)

**Perspective** (`PerspectiveInstance`, `PerspectiveHandle`): A graph of signed links that the executor stores for one or more owners. Example: "Alice's perspective holds her notes as links." [Perspectives](perspectives/README.md)

**Perspective diff** (`PerspectiveDiff`, `DecoratedPerspectiveDiff`): A set of link additions and link removals. Example: "A batch commit sends one perspective diff to the link language." [Links and store](perspectives/links/README.md)

**Perspective diff sync** (`bootstrap-languages/p-diff-sync/`): The Holochain link language that syncs perspective diffs as a revision graph. Only its scribe makes merge commits. Short form: p-diff-sync. Example: "p-diff-sync syncs the neighbourhood over Holochain." [Perspective diff sync](languages/link-languages/perspective-diff-sync.md)

**Perspective state** (`PerspectiveState`): The stage that a perspective has reached with its neighbourhood: `Private`, `NeighbourhoodCreationInitiated`, `NeighbourhoodJoinInitiated`, `LinkLanguageFailedToInstall`, `LinkLanguageInstalledButNotSynced` or `Synced`. It is not the link language sync status, which is one input to it. Example: "The perspective state is `Synced` after each good `sync()`." [Neighbourhood sync](perspectives/neighbourhoods/sync.md)

**Principal** (`Principal` in `agent/session.rs`, `anonymous_capabilities`): The kind of caller behind a session: operator, agent or anonymous. An anonymous caller can only request a capability and, in multi-user mode, sign up or log in. Example: "The principal of this session is an agent." [Sessions](agents/access/sessions.md)

**Publishing (a language)** (`publish_language`, `apply_template`): Storing a bundle and its signed language meta in the language language. Example: "Publishing the language gives its address." [Installing languages](languages/runtime/installing/README.md)

**Publishing (a neighbourhood)** (`neighbourhood.publish`, `neighbourhood_publish_from_perspective_with_context`): Storing a neighbourhood expression for a perspective and recording its URL on that perspective. The perspective is then a neighbourhood. Example: "`neighbourhood.publish` turns Alice's perspective into a neighbourhood." [Publishing and joining](perspectives/neighbourhoods/publishing-and-joining.md)

**Remote peers** (`Language::others`, `peersRemote`): The other agents that a link language reports. Example: "`peersRemote` returns `[]` in the local link language." [Link languages](languages/link-languages/README.md)

**Revision** (`Language::current_revision`): The ID of the current state of a link language, such as a diff-entry hash in p-diff-sync or the XOR of the active link hashes on a link server. Example: "Two link-server rooms with the same links have the same revision." [Link languages](languages/link-languages/README.md)

**RPC API** (`rust-executor/src/api/ws_rpc.rs`, `HandlerMap`): The executor's WebSocket at `/api/v1/ws`, which carries method calls, events and live-query updates. Example: "The SDK calls `perspective.get` over the RPC API." [RPC API](agents/interfaces/rpc-api.md)

**Runtime service** (`RuntimeService`, `rust-executor/src/runtime_service/`, `runtime.*` RPC, `known_link_languages` table): The executor service that holds the bootstrap seed and the lists of trusted agents, known link languages and friends. Its `runtime.*` calls also cover notifications, export, import and quit. Example: "The runtime service lists the trusted agents." [Executor services](agents/executor/services/README.md)

**Scribe** (`bootstrap-languages/p-diff-sync/`): In p-diff-sync, the one agent that makes merge commits: the one with the lowest DID among the peers it sees and itself. Example: "Each agent computes the scribe from the peers it sees." [Perspective diff sync](languages/link-languages/perspective-diff-sync.md)

**Server link language** (`bootstrap-languages/server-link-language/`, `src/encryption.ts`): The link language that syncs through a link server and encrypts links end to end. Each link-server room has an AES-256-GCM room key, which the language seals to the DID of each member. Example: "The server link language seals the room key to each member's DID." [Server link language](languages/link-languages/server-link-language.md)

**Session** (`Session` in `agent/session.rs`): The identity of a connection: a principal, its capabilities and its grant. The executor checks it again on each request and each event. The SDK's call object `Session` in `core/src/neighbourhood/Session.ts` is a call, and an MCP session holds one session. Example: "The executor opens a session from the token on each socket." [Sessions](agents/access/sessions.md)

**SFU room** (`SfuRoom`): One call on the executor's SFU, keyed by neighbourhood URL and room name. The bare word "room" is ambiguous: write "SFU room" or "link-server room". Example: "`sfu.callJoin` puts the caller in the SFU room." [SFU](agents/executor/services/sfu.md)

**Signal** (`PerspectiveInstance::send_signal`, `neighbourhood.sendSignal`): A signed payload that the link language sends to one agent of a neighbourhood and does not store. A Holochain signal is a different thing ([Holochain service](agents/executor/services/holochain.md)). Example: "Alice sends Bob a signal with an offer for the call." [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md)

**Social DNA (SDNA)** (`register_shapes`, `shape_links`): The subject class and flow definitions that a perspective stores as shared links. Example: "`register_shapes` stores a subject class in the perspective." [Social DNA](perspectives/social-dna/README.md)

**Subject class** (`SHACLShape`, `parse_shacl_to_links`): A named SHACL shape with actions, which a perspective stores, keyed by its target class IRI. Use "model class" for the SDK side. Example: "The perspective stores the subject class `Message`." [Subject classes](perspectives/social-dna/subject-classes.md)

**System language** (`is_system_language`, `SystemLanguageAddresses`): A language that the bootstrap seed or the known link languages name. The executor loads it with extra file access. Example: "The language language is a system language." [Bootstrap languages](languages/runtime/bootstrap-languages.md) — not: core language

**Telepresence** (`has_telepresence_adapter`, `Language::set_online_status`, `send_signal`, `send_broadcast`): The part of a neighbourhood that no executor stores: online status, signals and broadcasts. The link language carries it. Example: "Telepresence carries the online status of each member." [Presence and signals](perspectives/neighbourhoods/presence-and-signals.md)

**Templated language** (`language_apply_template_on_source`, `//!@ad4m-template-variable`, `apply_template_data`): A language that the executor makes from a source language by replacing the constants that a template comment marks. Its language meta records `templateAppliedParams` and `templateSourceLanguageAddress`. Example: "The templated language has the `UID` of its neighbourhood." [Language trust and templates](languages/runtime/installing/trust-and-templates.md)

**Tombstone** (`ad4m://retracted`, `retraction_for`, `retraction_effects`): A signed link `<source> ad4m://retracted literal:string:<signature>` with which the author of a `Shared` link under a monotonic predicate ends that link. Each executor removes the named link from its store and stores no later copy of it. "Removal" is the local act of deleting a link; a tombstone is the signed link that other executors apply. Example: "The author's tombstone ends the accepted link on every executor." [Flows](perspectives/social-dna/flows/README.md)

**Tool bundle** (`tool_bundles/`, `BundleSandbox`, `tool_bundles::builtin`): The JavaScript of an app's agent tools, which the executor runs in a sandbox isolate with no access to the executor. The executor compiles in one tool bundle, WE's, behind `template_read` and `template_update`. Example: "The tool bundle runs `template_read` in a sandbox isolate." [MCP server](agents/interfaces/mcp-server.md)

**Transition guard** (`requires` on `FlowState`): A model query on a flow state. The executor proposes a transition into that state only when the query matches. Example: "The transition guard needs a `Deliverable` instance in the scope." [Flow spawn and transition guards](perspectives/social-dna/flows/spawn-and-guards.md)

**Transition proposal** (`FlowTransitionProposal`): A set of signed links that proposes to move a flow instance to a state and carries the votes on it. Example: "Two DIDs voted on the transition proposal." [Flow consensus](perspectives/social-dna/flows/consensus.md)

**Trusted agent** (`RuntimeService::get_trusted_agents`): A DID whose languages this executor installs with no template check. The list holds the seed's agents, the main agent and the agents in the database. Example: "The executor installs the language with no template check because its author is a trusted agent." [Language trust and templates](languages/runtime/installing/trust-and-templates.md)

**Trusted seq** (`KelCache::trusted`): The seq of the last key-log head that one of `identity.storeKeys` signed and the executor accepted. The executor checks device keys at this seq, not at a newer event it has not accepted. Example: "The new device key signs only after the trusted seq reaches its event." [Person identity](agents/identity/person-identity.md)

**TypeScript SDK** (`@coasys/ad4m`, `core/`, `Ad4mClient`, `ApiClient`): The TypeScript client of the RPC API, with the shared types and the model class decorators. Example: "The app uses `Ad4mClient` from the TypeScript SDK." [TypeScript SDK](agents/interfaces/typescript-sdk/README.md)

**User account** (`users` table, `user_management.rs`): The login of a managed agent: email, DID and password hash. Example: "A user account holds the email and the password hash." [Managed agents](agents/executor/managed-agents.md)

**User cell** (`UserCells`, `user_app_id`, app id `<app id>@<did>`): A managed agent's own copy of a language's Holochain app, installed from the main app's payload under the agent's own key for that language on its first call. It has the main cell's DNA hash, so it joins the same network, and it writes and signs as the managed agent. System languages have no user cells. Example: "The user's zome call goes to its user cell, not to the main agent's cell." [Holochain service](agents/executor/services/holochain.md)

**User token** (`generate_user_jwt`): A JWT that the executor issues at login, with the DID of a managed agent as its `sub`. Its grant holds the default user capabilities. Example: "`user.login` returns the user token." [Managed agents](agents/executor/managed-agents.md) — not: user JWT, login JWT

**Wallet** (`WalletBackend`, `LocalWallet`, `SharedWallet`): The executor's store of all its Ed25519 keys, by name, which the operator's passphrase locks. The keystore is its encrypted form in a file. Example: "The wallet holds the main key and the key of each managed agent." [Wallet and keys](agents/identity/wallet-and-keys.md)
