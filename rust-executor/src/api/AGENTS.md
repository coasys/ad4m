# api/ — agent guide

axum HTTP + WebSocket surface. Two protocols: AD4M WS RPC and an OpenAI-compatible
REST/WS shim. Split plan: spec item 6.

## Routes (`mod.rs`)

| Route | Handler | Notes |
|---|---|---|
| `GET /api/v1/ws` | `ws_rpc.rs` | JSON-RPC-ish: `{type, id, ...params}` → `HandlerMap::dispatch`. Auth once at upgrade (`auth.rs`). Per-request cancel token (`request.cancel`). **Also carries events** (`events_ws::build_event_stream`, filtered by `events.watch`) and the connection's live query updates; the only event socket |
| `GET /health`, `POST /internal/shutdown` | `internal.rs` | `INTERNAL_API_TOKEN` |
| `/v1/*`, `/api/v1/openai/v1/*` | `openai_compat/router.rs` | chat/completions, embeddings, audio, realtime WS |

## Handler modules

`*_ws.rs`, one per RPC namespace: `agent`, `ai`, `expressions`, `hosting`,
`languages`, `neighbourhoods`, `perspectives` (largest; also SHACL + interpretation
handlers), `runtime`, `users`. Each exposes `register_ws_handlers(&mut HandlerMap)`
called from `ws_handler::build_handler_map`.

Handler shape (`perspectives_ws.rs::add_link` is representative):

```rust
async fn add_link(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AddLinkRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let mut perspective = guarded_perspective(&params).await?;     // the guard checked access
    let agent_context = ctx.acts_as();                              // the grant's agent, or the main agent
    ...
    Ok(serde_json::to_value(result)?)
}
// registered with .guard(Guard::perspective(perspective_update_capability))
```

A guard (next section) checks the capability and the access, so the handler does neither: a
perspective handler only fetches the perspective (`guarded_perspective`). **Every method
registers a guard**: the executor does not start with a method without one (a method open to
callers without any capability registers `Guard::Open`, with a comment saying why). A handler
takes `AgentContext` from the session
(`ctx.acts_as()`) for anything that signs, bills or writes. Never
re-derive it from the token: a token that stops decoding must not turn into the main agent.

**An agent is its DID.** A user session is known by its managed agent's DID
(`ctx.user_did()`); there is no `ctx.user_email()`. Bill, own, filter and log by that DID. A
parameter that names a user by email (`runtime.computeLog { userEmail }`, the operator's
`user.*` calls) resolves to the DID at the handler with `AgentService::did_for_email`; a reply
field that shows an address (`hosting.info`'s `email`, the compute log's `userEmail`) reads it
back with `AgentService::email_for_did` or a join on `users.did`.

`RequestContext` (`types/domain.rs`) is the caller for one request: `session: Arc<Session>`,
the per-request `cancel_token`, and `auth_token` (read only by the interpretation pass; Plan 20
PR 6 removes it). The RPC socket opens one per connection (`RequestContext::open`); REST and the
OpenAI-compatible API take it as an axum extractor (`auth.rs`), which answers 401 for a token
that opens no session and 503 while the wallet is locked. `auto_permit_cap_requests` is read
from the global config where `agent.requestCapability` uses it.

## Guards (`guards.rs`)

A method declares who may call it where it registers:

```rust
map.method::<PerspectiveUuidParams, Option<PerspectiveHandle>>("perspective.get", handler)
    .read()
    .or_null()
    .guard(Guard::perspective(perspective_query_capability));
map.method::<LockAgentRequest, AgentStatus>("agent.lock", lock_agent)
    .guard(Guard::Executor(AGENT_UPDATE_CAPABILITY.clone()));
map.method::<AiIdParams, bool>("ai.removeTask", remove_task)
    .guard(Guard::Owns { cap: |_| AI_CREATE_CAPABILITY.clone(), id: REMOVED_TASK_ID, owner: Owner::Task });
```

`Guard::perspective(cap)` builds `Guard::Owns { owner: Perspective }` on the `uuid` param.

`HandlerMap::dispatch` runs, in order: find the method (404) → params deserialize into `P`
(400) → the guard's capability (403) → the guard's rule (403, or 404 for a resource) → the
handler. `HandlerMap::admit` runs the steps before the handler alone.

| Guard | Rule after the capability |
|---|---|
| `Open` | no capability and no rule: every caller, anonymous included, reaches the handler, which decides |
| `Operator` | no capability; `ctx.is_operator()` (the admin credential, or no token on a loopback listener of an executor without one), else 403 "Admin credential required" (`guards::OPERATOR_ONLY`), whatever the caller's capabilities |
| `Cap(cap)` | none: any caller holding `cap`. An anonymous caller holds only the anonymous set (request a capability, redeem it, sign up, log in, verify an email); the handler acts on `ctx.acts_as()` |
| `Executor(cap)` | the operator or a grant on the main agent: 403 naming the method to a session on a managed agent, whatever its capabilities, and to an anonymous caller. A grant on the main agent passes (D5): WE web administers a local executor with an app token holding `*` |
| `Owns { cap, id, owner }` | the resource `id` reads from the params exists and the operator or the session's agent owns it (`guards::owns`), else 404, missing or not |

`Owns` per `Owner` (who owns the resource; the operator reaches all):

- `Perspective`: its `owners` DIDs; none = the main agent (`get_perspective_with_access`).
  A grant on the main agent reaches the main agent's and the unowned perspectives.
- `Task`: `tasks.agent_did`; none = the main agent. A grant on the main agent reaches the main
  agent's tasks only, not a managed agent's.
- `Grant`: the agent of the grant a token or request id names (`Grants::find`), for
  `agent.revokeToken` and `agent.removeApp` (D14). `agent.getApps` is `Cap`: it lists only the
  grants of `ctx.acts_as()` (the main agent's for the operator and main-agent apps).
- `Request`: the target agent of a pending capability request (`PendingRequests`), for
  `agent.permitCapability` (D12): a user approves requests for their own agent, the operator
  any.

**Missing and not yours are one answer: 404** (D6), on every perspective, task, grant and
request, so a caller cannot learn which ids exist. A method flagged `.or_null()` answers `null`
in place of 404 (the guard's or a handler's, e.g. a perspective removed between the guard and
the handler) without running the handler: only `perspective.get` and `perspective.snapshot`,
whose SDK results are optional. A perspective handler fetches with `guarded_perspective`.

Capabilities stay: every guard but `Open` and `Operator` checks one first, and the rule adds
whose data the call may touch. A method takes exactly one guard. A second panics where it
registers. A method without one makes `build_handler_map` panic, naming it
(`HandlerMap::require_guards`): the API thread builds the map before it serves, so the executor
binary exits at start. Dispatch also refuses (500) a method without a guard in a map built
without that check. A guard runs before the handler's own checks. A handler check that is not
part of the guard (compute credits, param validation beyond the contract type) runs after the
access check: a call that fails both gets the access answer.

Notes on some methods:

- `Open` handlers check the session's principal (`user.logout`). `user.revokeAllSessions` is
  `Cap(agent:UPDATE)`, as `agent.removeApp`: since #1399 an app a user approved acts as the
  user's agent too; its handler checks the principal for a named user.
- `Operator`: `runtime.importData` writes rows as they are in the file, granted notifications
  and password hashes included. The stubs `user.credits` and `runtime.unyt*` answer 501 to the
  operator until each gets a handler and its real guard.
- `runtime.exportData` (`Executor`) of one perspective checks `get_perspective_with_access` in
  its handler (a `db` export names none), so an app exports only the main agent's perspectives.
- `Cap` methods that read executor-wide data users read today: `agent.getTrustedAgents`,
  `ai.models`, `language.*` reads, link-language templates,
  `runtime.hcAgentInfos`/`networkMetrics`/`hostRates`/`unytVersionInfo`. `Executor` would refuse
  users there, a behaviour change.
- `perspective.all` lists the caller's agent's perspectives, `ai.tasks` its tasks (every one to
  the operator); `ai.addTask` and `perspective.create` record the caller's agent as owner.
- `neighbourhood.*` methods that name a perspective (`publish` by `perspectiveUuid`, the others
  by `uuid`) use `Owns { Perspective }` on the neighbourhood capability: the perspective that
  holds a neighbourhood decides who may use it.

Which guard each method has, generated from the handler map
(`tests/handler_table_tests.rs::render_guard_table`). `the_guard_table_in_agents_md_is_current`
fails when it is stale; `pnpm run generate:api-types` in `core/` rewrites it with the SDK
contracts. An `Owns` row names the resource, the param the guard reads its id from (an
`IdParam`: a field path, `a or b` for aliases, `auth(JSON).requestId` for a field inside a
JSON string param) and the capability. A handler reads the id through the same `IdParam`
(`guards::UUID_PARAM.require(&params)`, `guarded_perspective`), so it acts on the resource
the guard checked:

<!-- guard-table:start (generated from the handler map) -->

170 methods: Cap 55, Executor 33, Open 5, Operator 15, Owns 62.

| Method | Guard |
|---|---|
| `agent.addEntanglementProofs` | `Executor(agent:UPDATE)` |
| `agent.addTrustedAgents` | `Executor(runtime.trusted_agents:CREATE)` |
| `agent.byDid` | `Cap(agent:READ)` |
| `agent.deleteEntanglementProofs` | `Executor(agent:UPDATE)` |
| `agent.deleteTrustedAgents` | `Executor(runtime.trusted_agents:DELETE)` |
| `agent.entanglementProofPreflight` | `Executor(agent:READ)` |
| `agent.generate` | `Executor(agent:CREATE)` |
| `agent.generateJwt` | `Cap(agent:AUTHENTICATE)` |
| `agent.get` | `Cap(agent:READ)` |
| `agent.getApps` | `Cap(agent:READ)` |
| `agent.getEntanglementProofs` | `Executor(agent:READ)` |
| `agent.getTrustedAgents` | `Cap(runtime.trusted_agents:READ)` |
| `agent.import` | `Cap(agent:CREATE)` |
| `agent.isLocked` | `Open` |
| `agent.lock` | `Executor(agent:UPDATE)` |
| `agent.permitCapability` | `Owns(Request auth(JSON).requestId, agent:PERMIT)` |
| `agent.removeApp` | `Owns(Grant id, agent:UPDATE)` |
| `agent.requestCapability` | `Cap(agent:AUTHENTICATE)` |
| `agent.revokeToken` | `Owns(Grant token, agent:UPDATE)` |
| `agent.sign` | `Cap(agent:SIGN)` |
| `agent.status` | `Cap(agent:READ)` |
| `agent.unlock` | `Executor(agent:SIGN)` |
| `agent.updateProfile` | `Cap(agent:UPDATE)` |
| `ai.addModel` | `Executor(artificial intelligence:CREATE)` |
| `ai.addTask` | `Cap(artificial intelligence:CREATE)` |
| `ai.discoverModels` | `Executor(artificial intelligence:CREATE)` |
| `ai.embed` | `Cap(artificial intelligence:PROMPT)` |
| `ai.getDefaultModel` | `Cap(artificial intelligence:READ)` |
| `ai.modelLoadingStatus` | `Cap(artificial intelligence:READ)` |
| `ai.models` | `Cap(artificial intelligence:READ)` |
| `ai.prompt` | `Owns(Task taskId, artificial intelligence:PROMPT)` |
| `ai.removeModel` | `Executor(artificial intelligence:CREATE)` |
| `ai.removeTask` | `Owns(Task id, artificial intelligence:CREATE)` |
| `ai.setDefaultModel` | `Executor(artificial intelligence:CREATE)` |
| `ai.tasks` | `Cap(artificial intelligence:READ)` |
| `ai.transcriptionClose` | `Cap(artificial intelligence:TRANSCRIBE)` |
| `ai.transcriptionOpen` | `Cap(artificial intelligence:TRANSCRIBE)` |
| `ai.updateModel` | `Executor(artificial intelligence:CREATE)` |
| `ai.updateTask` | `Owns(Task task.taskId, artificial intelligence:CREATE)` |
| `expression.create` | `Cap(expression:CREATE)` |
| `expression.get` | `Cap(expression:READ)` |
| `expression.getMany` | `Cap(expression:READ)` |
| `expression.getRaw` | `Cap(expression:READ)` |
| `expression.interact` | `Cap(expression:UPDATE)` |
| `expression.interactions` | `Cap(expression:READ)` |
| `hosting.info` | `Cap(runtime.hosting:READ)` |
| `hosting.requestPayment` | `Cap(agent:UPDATE)` |
| `hosting.setHotWallet` | `Cap(agent:UPDATE)` |
| `hosting.wallet` | `Executor(runtime.hosting:READ)` |
| `hosting.walletHistory` | `Executor(runtime.hosting:READ)` |
| `language.all` | `Cap(language:READ)` |
| `language.applyTemplate` | `Cap(language:CREATE)` |
| `language.get` | `Cap(language:READ)` |
| `language.meta` | `Cap(language:READ)` |
| `language.publish` | `Executor(language:CREATE)` |
| `language.remove` | `Executor(language:DELETE)` |
| `language.source` | `Cap(language:READ)` |
| `language.writeSettings` | `Executor(language:UPDATE)` |
| `neighbourhood.hasTelepresence` | `Owns(Perspective uuid, neighbourhood:READ)` |
| `neighbourhood.join` | `Cap(neighbourhood:READ)` |
| `neighbourhood.onlineAgents` | `Owns(Perspective uuid, neighbourhood:READ)` |
| `neighbourhood.otherAgents` | `Owns(Perspective uuid, neighbourhood:READ)` |
| `neighbourhood.publish` | `Owns(Perspective perspectiveUuid or perspectiveUUID, neighbourhood:CREATE)` |
| `neighbourhood.sendBroadcast` | `Owns(Perspective uuid, neighbourhood:UPDATE)` |
| `neighbourhood.sendSignal` | `Owns(Perspective uuid, neighbourhood:UPDATE)` |
| `neighbourhood.setOnlineStatus` | `Owns(Perspective uuid, neighbourhood:UPDATE)` |
| `perspective.acceptFlowProposal` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.acceptInterpretation` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.addAutoProcessor` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.addLink` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.addLinkExpression` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.addLinks` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.addSdna` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.all` | `Cap(perspective:READ)` |
| `perspective.commitBatch` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.create` | `Cap(perspective:CREATE)` |
| `perspective.createBatch` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.createSubject` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.disposeQuery` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.disposeSparql` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.evaluateGetters` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.executeCommands` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.flowFired` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.flowStates` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.flowValidOutputs` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.get` | `Owns(Perspective uuid, perspective:READ)`, 404 → `null` |
| `perspective.getAllShacl` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.getShacl` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.getShaclNames` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.getShaclTargetClass` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.getSubjectData` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.interpretationOverlays` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.linkMutations` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.mintFlowReceipt` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.modelQuery` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.modelSubscribe` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.proposeFlowTransition` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.publishSnapshot` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.queryLinks` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.queryProlog` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.querySparql` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.rejectFlowProposal` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.rejectInterpretation` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.remove` | `Owns(Perspective uuid, perspective:DELETE)` |
| `perspective.removeAutoProcessor` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.removeLink` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.removeLinks` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.resyncSubscription` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.runInterpretation` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.runInterpretationWithHarness` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.snapshot` | `Owns(Perspective uuid, perspective:READ)`, 404 → `null` |
| `perspective.subjectClassesOf` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.subscribeQuery` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.subscribeSparql` | `Owns(Perspective uuid, perspective:READ)` |
| `perspective.update` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.updateLink` | `Owns(Perspective uuid, perspective:UPDATE)` |
| `perspective.verifyFlowReceipt` | `Owns(Perspective uuid, perspective:READ)` |
| `runtime.addFriends` | `Executor(runtime.friends:CREATE)` |
| `runtime.addHcAgentInfos` | `Executor(runtime.hc_agent_info:CREATE)` |
| `runtime.addLinkLanguageTemplates` | `Executor(runtime.known_link_languages:CREATE)` |
| `runtime.computeLog` | `Cap(agent:READ)` |
| `runtime.createNotification` | `Cap(agent:UPDATE)` |
| `runtime.deleteNotification` | `Cap(agent:UPDATE)` |
| `runtime.exportData` | `Executor(agent:UPDATE)` |
| `runtime.freeHostingEnabled` | `Open` |
| `runtime.friendStatus` | `Cap(runtime.friend_status:READ)` |
| `runtime.friends` | `Executor(runtime.friends:READ)` |
| `runtime.grantNotification` | `Operator` |
| `runtime.hcAgentInfos` | `Cap(runtime.hc_agent_info:READ)` |
| `runtime.hostRates` | `Cap(runtime.hosting:READ)` |
| `runtime.importData` | `Operator` |
| `runtime.inbox` | `Cap(runtime.messages:READ)` |
| `runtime.info` | `Open` |
| `runtime.linkLanguageTemplates` | `Cap(runtime.known_link_languages:READ)` |
| `runtime.networkMetrics` | `Cap(runtime.hc_agent_info:READ)` |
| `runtime.notifications` | `Cap(agent:UPDATE)` |
| `runtime.openLink` | `Operator` |
| `runtime.outbox` | `Executor(runtime.messages:READ)` |
| `runtime.quit` | `Executor(runtime:QUIT)` |
| `runtime.removeFriends` | `Executor(runtime.friends:DELETE)` |
| `runtime.removeLinkLanguageTemplates` | `Executor(runtime.known_link_languages:DELETE)` |
| `runtime.restartHolochain` | `Operator` |
| `runtime.sendFriendMessage` | `Executor(runtime.messages:CREATE)` |
| `runtime.setFreeHostingEnabled` | `Executor(*:*)` |
| `runtime.setHostRates` | `Operator` |
| `runtime.setStatus` | `Cap(runtime.my_status:UPDATE)` |
| `runtime.setUnytMembraneProof` | `Operator` |
| `runtime.tlsDomain` | `Open` |
| `runtime.unytAgentKey` | `Operator` |
| `runtime.unytHotAgentPubkey` | `Operator` |
| `runtime.unytReinstallDna` | `Operator` |
| `runtime.unytSendHot` | `Operator` |
| `runtime.unytVersionInfo` | `Cap(runtime.hosting:READ)` |
| `runtime.unytWalletBalance` | `Operator` |
| `runtime.unytWalletHistory` | `Operator` |
| `runtime.updateNotification` | `Cap(agent:UPDATE)` |
| `runtime.verifySignature` | `Cap(agent:READ)` |
| `user.create` | `Cap(runtime.user_management:CREATE)` |
| `user.credits` | `Operator` |
| `user.emailTest` | `Executor(*:*)` |
| `user.freeAccess` | `Operator` |
| `user.list` | `Executor(runtime.user_management:READ)` |
| `user.login` | `Cap(runtime.user_management:LOGIN)` |
| `user.logout` | `Open` |
| `user.multiUserEnabled` | `Cap(runtime.user_management[enabled]:READ)` |
| `user.requestVerification` | `Cap(runtime.user_management:VERIFY)` |
| `user.revokeAllSessions` | `Cap(agent:UPDATE)` |
| `user.setMultiUserEnabled` | `Operator` |
| `user.verifyEmail` | `Cap(runtime.user_management:VERIFY)` |
| `user.wallet` | `Executor(runtime.user_management:READ)` |

<!-- guard-table:end -->

`tests/method_matrix_tests.rs` walks every method in `build_handler_map()` (one without a
guard fails it; `every_registered_method_has_a_guard` says so on its own) and calls
`admit` as each kind of caller (operator, app on the main agent with and without the
capability, user, another user, anonymous). `Cap` rows admit an anonymous caller exactly when
the anonymous set covers the capability. `Owns` rows name the user's own, the main agent's,
an unowned (perspectives) and a missing resource of the guard's `Owner`. A new guard is
covered without a new test; a method whose params need more than `uuid` adds an entry to
`EXTRA_PARAMS`, with `TARGET` where it names the row's resource in another param.

## Protocol

- Clients (core SDK, rust-client) ship with the executor from the same revision: no feature
  discovery and no compatibility modes.
- Events: a socket gets no events until it sends `events.watch { "<type>": null | [perspective
  uuids] }` (replaces the last watch; `events.unwatch` clears it). `event_interest.rs`, handled
  inline on the RPC socket (per-connection state, like `request.cancel`). A list matches events
  whose `Event::perspective` (the payload's `perspectiveUuid`) it names; unscoped payloads carry
  none. There is no separate events socket (`/api/v1/ws/events` was removed).
- Live query updates (`query-subscription-update`) pass `events.watch` untouched. A live query
  belongs to the RPC connection that opened it (`RequestContext::connection_id`, #1207): its
  update is an `Event` with `Event::connection` set (`Event::for_connection`), and a socket's
  stream drops an event whose connection is not its own before `may_receive` runs, so the update
  needs both its connection and its audience. The key is not on the wire. When a socket's stream
  lags it sends `{ "type": "query-subscription-update", "lagged": true }` and `LiveQuery` resyncs.
  Closing the socket disposes its subscriptions (`ws_rpc::serve`).
- One event path. A publisher calls `pubsub.publish_event(Event::new(Topic, Audience, payload))`
  (`src/events.rs`); there is no default audience. Wire names live in `Topic::as_str` only;
  `events_ws::event_specs()` is keyed by `Topic`. A socket's stream
  (`events_ws::build_event_stream`) delivers an event when `Session::may_receive` and
  `event_interest::wants` (the watch, keyed by `Topic`) hold, as `events_ws::wire_message`.
- `Audience = Everyone | Dids(Vec<String>)`. `may_receive` needs both: the session is in the
  audience (everyone, or `Session::acts_as_did` is one of the DIDs; a grant acts as its agent,
  the operator as the main agent it owns), and it holds `Event::capability`. `Event::new` fills
  the capability from `Topic::capability(perspective)`, the capability of the RPC method that
  reads the same data (#1284): per-perspective topics name the event's perspective
  (`perspective.queryLinks`' `perspective_query_capability([uuid])`; a signal: neighbourhood READ
  on it); an event without a perspective needs the grant on `*`. Approval requests carry
  AGENT_PERMIT instead (`Event::approval_request`). An app acts as the main agent, so without the
  capability an app approved for AI or one perspective got every perspective's events. A user
  token holds every topic's capability (AGENT_PERMIT on its own agent included, #1399) and the
  operator may do anything, so only narrower tokens lose events. A new topic picks its capability in the same
  exhaustive `match`. No check takes a lock or parses the payload. The stream builder takes the
  socket's interest (`Connection::interest()`). Only authenticated sessions
  (`Session::is_authenticated`: not anonymous) get a stream. The stream ends at the first event
  the session may receive after `Session::check()` fails. `audience_matrix_tests` in
  `events_ws.rs` pins who receives each audience; a new topic or audience adds its cases there.
- The operator has no view of other agents' events: it receives what the main agent receives.
- Audiences. Every per-perspective topic reads its recipients from one helper,
  `perspectives::event_recipients`: the owners, or the main agent for an unowned perspective
  (or one whose owner list is empty), read when the event is published. No per-perspective event
  goes to `Everyone`. Each goes out once, to all recipients (`perspectives::publish_to_owners`),
  and no payload names an `owner`: `perspective-added/-updated/-removed`, `sync-state-change`,
  `link-added/-removed/-updated`. `query-subscription-update`
  (`PerspectiveInstance::send_delta_update`, which reads the recipients when it publishes)
  goes only to the subscriber (`SubscribedQuery::subscriber`, the main agent for `None`) when it
  is one of them: the query ran as the subscriber, so a co-owner must not get its results.
  `signal` goes to its `recipient` (`perspectives::signal_event`); one without a recipient goes
  once to each of `event_recipients`, each copy naming that DID as `recipient`
  (`perspectives::publish_telepresence_signal`, used by both link-language paths and by
  `send_signal` / `send_broadcast` for the executor's own agents: a neighbourhood's owners are
  only this executor's agents). `notification-triggered` goes to the notification's `ownerDid`,
  never to co-owners, and needs the query capability on its perspective (its `triggerMatch` is
  query results), not `runtime.notifications`' AGENT_UPDATE. `auto-processor-event` goes to the pass owner when it is one of
  `event_recipients`, else nobody (`auto_processor::events::pass_audience`);
  `auto-processor-neighbourhood-state` to `event_recipients` (empty for an unknown
  perspective). `auto_processor::events::emit` reads the handle when it publishes, so no emitter
  may hold the perspective's handle lock while it emits. `hosting-user-info-changed` goes out twice
  (`Event::hosting_user_info`): to the user's DID (looked up by email in the local users table
  at publish, never imported) with RUNTIME_HOSTING_READ, and to the main agent's with
  RUNTIME_USER_MANAGEMENT_READ (the operator and apps that may list users see every user's). `model-loading-status` goes to everyone; `transcription-text` to the stream
  owner's DID. `agent-status-changed` and `agent-updated` go to the DID the payload names
  (`Audience::agent`; a status without a DID, before `agent.generate()`, to everyone).
  `apps-changed` (`Event::apps_changed`) and the approval exceptions (`CapabilityRequested`,
  `InstallNotificationRequest`, `Event::approval_request`) go to the DID of the agent the
  approval is for (the agent a capability request names, #1399) with capability AGENT_PERMIT:
  for the main agent the operator and main-agent apps whose token holds it, for a managed agent
  that user's sessions (users hold AGENT_PERMIT), whether or not they subscribe to exceptions. Other exceptions go to everyone (`Event::exception`) with the
  exception subscription's capability.
- Every socket (RPC, `/v1/realtime`) keeps the caller's `Arc<Session>` and calls
  `Session::check()` again on each request and each delivered event, so revocation and expiry end
  an open connection. Tests build sessions with `Session::for_test(principal)` plus
  `.with_caps(..)`, `.with_did(..)`, `.with_grant(..)`, `.expiring_at(..)`, and contexts with
  `RequestContext::{operator,app,user}_for_test` / `for_session` (`.on_connection(id)` for a
  context on an RPC connection).
- Each socket's "session over" token is `ctx.session.ended()`: a child of the grant's `ended`
  token (a token that never fires for the operator and anonymous callers). Revoking or removing
  the grant cancels it, and the socket closes at once with `ws_rpc::token_ended_frame()` (1008),
  the same frame a failed check sends. A grant revoked before the socket starts serving gives an
  already-cancelled child, so there is no race to check. Only the socket's own task writes the
  close frame. The RPC socket first delivers in-flight replies for up to `REPLY_GRACE`, so
  `user.logout` gets its answer; each call's `cancel_token` is a child of the socket's token, so
  the calls still running are cancelled (401). Expiry closes an idle socket the same way:
  every socket (and the SSE chat stream, and the RPC socket's event stream,
  `build_event_stream`) also waits on `session.over()`, which completes when
  the grant ends or the token expires (leeway included). A new socket type must watch
  `session.over()` too.
- Revocation is on grants (`agent::grants::Grants`): `agent.revokeToken` (by request id or by
  the token) and `agent.removeApp` act on grants of the caller's own agent, or any for the
  operator; another agent's grant answers as missing. `user.logout` revokes the caller's grant;
  `user.revokeAllSessions` revokes every grant on that user's agent. `agent.getApps` lists the
  grants of `acts_as()`.
- Capability flow (#1399, executor side; `agent/capabilities/mod.rs` + `pending.rs`):
  1. `agent.requestCapability { authInfo, agent? }` (anyone, AGENT_AUTH): `agent` is a managed
     user's email or an agent's DID, absent = the main agent (`requested_agent` returns a
     `RequestTarget`: an agent keyed by DID, D16). An email resolves to its user's DID there,
     read-only (`AgentService::did_for_email_read_only`: a shared database's record is read,
     not imported; the import waits for the user's login); an email no user has is still
     taken, as `RequestTarget::NoAgent`, so the answer reveals no accounts: it reaches nobody,
     only the operator may approve it, its code mints nothing, and it expires after 24 hours
     like a managed agent's. An unknown DID is 400; an app name, description, domain, URL or
     icon over 1 KiB is 400. The request is a `PendingRequest` in memory only: never on disk,
     1024 at most and 64 per target (all `NoAgent` requests are one target). Past a limit a
     new request evicts an unapproved one, oldest first: for no agent, then for its own
     target, then (for an agent below its share) the target holding the most; never an
     approved one, so when none may go it is refused (429; MCP's `request_capability` answers
     `success: false`). A request lives 10 minutes for the main agent and 24 hours for a
     managed agent, restarted at approval. Its approval request goes to the target agent's DID
     (`Event::approval_request`). Every request waits for that owner: the declared app name
     and domain are the caller's claim and approve nothing. `AuthInfo.user_email` on the
     request and the grant is only a display hint (the owner's email, from `email_for_did`).
  2. `agent.permitCapability { auth }`: its guard, `Owns { owner: Request }`, admits the
     operator or an authenticated session acting as the request's agent (the same DID) with
     AGENT_PERMIT; anyone else gets 404, as for a missing request. The approver may grant
     fewer capabilities than asked, and no more than it holds (`caps_within`: each approved
     capability keeps its overlap with the held ones, so several actions or `*` on one
     pointer narrow rather than drop); the target is fixed by the request. It returns a
     six-digit code, stored as a `Secret`.
  3. `agent.generateJwt { requestId, rand }` (anyone): `PendingRequests::redeem` checks the code
     and mints the grant on the request's agent in one locked step, with the approved
     capabilities capped at that agent's own (`grant_caps_for`: a managed agent holds its user's
     capabilities). The request goes only when the grant exists; a failed mint (locked wallet,
     database) leaves it approved and redeemable. Five wrong codes delete it; a code redeems
     once. `apps-changed` then goes to the grant's agent.
  `capabilities::request_capability`, `permit_capability` and `generate_capability_token` are the
  same steps for MCP (`mcp/tools/auth.rs`, which auto-permits only when `caller_can_permit` says
  the caller may approve for the target) and the dev-mode auto-permit.
- Live queries: `subscribeQuery` / `modelSubscribe` reply `{ subscriptionId, result, revision }`
  (revision 0, or later if a write landed while the first result was computed: the subscription is
  registered first);
  each `query-subscription-update` carries the change (models: `ids` + `upsert`; queries: `added` /
  `removed` rows, or the whole `result` when the rows would lose their order; see
  `perspectives/perspective_instance/subscriptions.rs`) and `revision` (+1 per update). On a gap,
  `perspective.resyncSubscription { uuid, subscriptionId }` → `{ revision, result }`. When the
  update topic lags, the socket gets one `{ type: 'query-subscription-update', lagged: true }` and
  the client resyncs every live query.
- A live query belongs to the RPC connection that opened it (`RequestContext::connection_id`, set
  by `ws_rpc::serve` next to the connection's `Session`): only that socket gets its updates (they
  bypass `events.watch`), and `ws_rpc::serve` ends them when the socket closes, and again as each
  call still in flight ends (a late subscribe). No keepalive; subscribing without a connection
  (REST) is a 400.
- `ws_rpc::serve` runs one RPC connection over any text stream; `tests/connection_tests.rs` drives
  it through channels in place of a WebSocket.
- Contracts: each handler registers its params and result types (`map.method::<P, R>`), and
  `event_specs()` in `events_ws.rs` types every event payload. `tests/handler_table_tests.rs`
  writes `RpcMethods.ts` and `Events.ts` (ts-rs export dir) and fails if the copies in
  `core/src/generated/api/` or any type they import are stale — regenerate
  (`pnpm run generate:api-types` in `core/`) after changing a handler or an event.

## Types

- `types.rs`: request/response structs for WS (`ts-rs` exported for the SDK).
  `pnpm run generate:api-types` (in `core/`) writes one file per type into
  `core/src/generated/api/`, but not its `index.ts`: add the export line there by hand.
- `crate::types::core` (domain) vs `crate::types::domain` (wire/input). Some duplicates,
  see spec item 5. Prefer `crate::types::X` re-exports.
- `WsRpcError { code, message }` (`ws_handler.rs`): constructors `bad_request`,
  `forbidden`, `not_found`, `internal`. No `From` impls for domain errors yet.

## openai_compat/

Self-contained wire translation over `AIService`. Exception: `harness_bridge.rs` +
`tool_grammar.rs` are the interpretation harness's `CompletionSource` and tool
grammar, used by `perspectives/interpretation/run.rs`. They move to `agentic/`
(spec item 7); don't add more non-wire code here.

## Tests

`tests/` (`types_tests.rs`, `shacl_ws_tests.rs`) and `openai_compat/tests.rs`.
`tests/multi_user.rs` starts a multi-user executor and opens sessions from tokens the executor
signs,
as the WS upgrade does; the `*_access_tests.rs` files drive `build_handler_map().dispatch` as
different users with it. Behaviour changes to any handler also need the JS integration suite (`pnpm run
test-main` at repo root).
