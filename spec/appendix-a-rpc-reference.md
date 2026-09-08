# Appendix A — RPC Reference (Informative)

This appendix lists every WebSocket RPC operation the reference executor exposes, plus the event types it may push. It is **informative**: the wire envelope and connection rules in §7 are normative; the operation list evolves with each MINOR protocol release and any conforming executor MAY add operations beyond this set.

Per-operation `params` and `result` shapes are not duplicated here — they are defined by the request/response structs in:

- [`rust-executor/src/types/`](../rust-executor/src/types/) — primary wire types.
- The per-domain handler files in [`rust-executor/src/api/`](../rust-executor/src/api/) (`agent_ws.rs`, `perspectives_ws.rs`, etc.) — each handler's input/output types.

A future SHOULD: extract these into a JSON-Schema or OpenAPI document checked into the repo for machine-readable consumption.

## A.1 Operations by domain

### A.1.1 `agent`

| Operation | Capability | Purpose |
|---|---|---|
| `agent.generate` | `agent:CREATE` | Generate a new agent (DID + wallet) |
| `agent.import` | `agent:CREATE` | Import existing keys to bootstrap an agent |
| `agent.get` | `agent:READ` | Return the local agent's full record |
| `agent.byDid` | `agent:READ` | Resolve a DID to its `AgentExpression` |
| `agent.status` | `agent:READ` | Current lock/init status |
| `agent.isLocked` | `agent:READ` | Boolean wallet-lock status |
| `agent.lock` | `agent:LOCK` | Lock the wallet |
| `agent.unlock` | `agent:UNLOCK` | Unlock with passphrase |
| `agent.updateProfile` | `agent:UPDATE` | Update the public agent profile |
| `agent.sign` | `agent:SIGN` | Sign a string with the main key |
| `agent.requestCapability` | `agent:AUTHENTICATE` | Begin the app-consent flow |
| `agent.permitCapability` | `agent:PERMIT` | User-side approval (executor UI only) |
| `agent.generateJwt` | `agent:AUTHENTICATE` | Issue a JWT after approval |
| `agent.revokeToken` | `agent:AUTHENTICATE` | Revoke a previously issued JWT |
| `agent.getApps` | `agent:READ` | List currently authorized apps |
| `agent.removeApp` | `agent:UPDATE` | Revoke an authorized app |
| `agent.getTrustedAgents` | `runtime.trusted_agents:READ` | List `trustedAgents` |
| `agent.addTrustedAgents` | `runtime.trusted_agents:CREATE` | Add to `trustedAgents` |
| `agent.deleteTrustedAgents` | `runtime.trusted_agents:DELETE` | Remove from `trustedAgents` |
| `agent.getEntanglementProofs` | `agent:READ` | List entanglement proofs |
| `agent.addEntanglementProofs` | `agent:CREATE` | Add entanglement proofs |
| `agent.deleteEntanglementProofs` | `agent:DELETE` | Remove entanglement proofs |
| `agent.entanglementProofPreflight` | `agent:CREATE` | Validate a proposed entanglement proof |

### A.1.2 `expression`

| Operation | Capability | Purpose |
|---|---|---|
| `expression.create` | `expression:CREATE` | Create + sign + publish a new Expression |
| `expression.get` | `expression:READ` | Resolve and verify an Expression by URL |
| `expression.getMany` | `expression:READ` | Bulk resolve |
| `expression.interact` | `expression:UPDATE` | Call a Language-defined interaction on an Expression |
| `expression.interactions` | `expression:READ` | List available interactions on an Expression |

### A.1.3 `language`

| Operation | Capability | Purpose |
|---|---|---|
| `language.all` | `language:READ` | List installed Languages |
| `language.get` | `language:READ` | Get a Language by address |
| `language.meta` | `language:READ` | Get a Language's metadata |
| `language.source` | `language:READ` | Get the source code of a Language |
| `language.publish` | `language:CREATE` | Publish a new Language (subject to §8.5 trust check) |
| `language.applyTemplate` | `language:CREATE` | Instantiate a templated Language (§6.9) |
| `language.writeSettings` | `language:UPDATE` | Update a Language's persistent settings |
| `language.remove` | `language:DELETE` | Uninstall a Language |

### A.1.4 `perspective`

| Operation | Capability | Purpose |
|---|---|---|
| `perspective.all` | `perspective:READ` | List all Perspectives |
| `perspective.get` | `perspective:READ` | Get a PerspectiveHandle |
| `perspective.create` | `perspective:CREATE` | Create a new Perspective |
| `perspective.update` | `perspective:UPDATE` | Update Perspective metadata |
| `perspective.remove` | `perspective:DELETE` | Delete a Perspective |
| `perspective.snapshot` | `perspective:READ` | Get a full Perspective snapshot |
| `perspective.publishSnapshot` | `perspective:CREATE` | Publish a snapshot via the Perspective Language |
| `perspective.queryLinks` | `perspective:READ` | Query links by LinkQuery |
| `perspective.querySparql` | `perspective:READ` | Run a SPARQL 1.1 query |
| `perspective.addLink` | `perspective:UPDATE` | Add a new link |
| `perspective.addLinkExpression` | `perspective:UPDATE` | Add a pre-signed LinkExpression |
| `perspective.addLinks` | `perspective:UPDATE` | Bulk add |
| `perspective.updateLink` | `perspective:UPDATE` | Update an existing link |
| `perspective.removeLink` | `perspective:UPDATE` | Remove a single link |
| `perspective.removeLinks` | `perspective:UPDATE` | Bulk remove |
| `perspective.linkMutations` | `perspective:UPDATE` | Combined add/remove in one call |
| `perspective.executeCommands` | `perspective:UPDATE` | Run a sequence of `AD4MAction` commands |
| `perspective.addSdna` | `perspective:UPDATE` | Add an SDNA entry (Subject Class / Flow / custom) |
| `perspective.createSubject` | `perspective:UPDATE` | Create a Subject Class instance |
| `perspective.getSubjectData` | `perspective:READ` | Read a Subject Class instance |
| `perspective.modelQuery` | `perspective:READ` | Server-side typed model query (see §7.8.2 snake_case note) |
| `perspective.modelSubscribe` | `perspective:SUBSCRIBE` | Subscribe to reactive model query updates |
| `perspective.evaluateGetters` | `perspective:READ` | Evaluate computed getter expressions for a set of instances |
| `perspective.createBatch` | `perspective:CREATE` | Start a multi-step batch transaction |
| `perspective.commitBatch` | `perspective:UPDATE` | Commit a batch |
| `perspective.subscribeSparql` | `perspective:SUBSCRIBE` | Subscribe to SPARQL query results |
| `perspective.keepAliveSparql` | `perspective:SUBSCRIBE` | Heartbeat a SPARQL subscription |
| `perspective.disposeSparql` | `perspective:SUBSCRIBE` | Cancel a SPARQL subscription |

### A.1.5 `neighbourhood`

| Operation | Capability | Purpose |
|---|---|---|
| `neighbourhood.publish` | `neighbourhood:CREATE` | Publish a `NeighbourhoodExpression` |
| `neighbourhood.join` | `neighbourhood:CREATE` | Join a Neighbourhood by URL (§2.5.1) |
| `neighbourhood.otherAgents` | `neighbourhood:READ` | List all DIDs ever seen in the Neighbourhood |
| `neighbourhood.onlineAgents` | `neighbourhood:READ` | List currently-online DIDs (requires telepresence) |
| `neighbourhood.hasTelepresence` | `neighbourhood:READ` | Whether the backing Link Language exports telepresence |
| `neighbourhood.setOnlineStatus` | `neighbourhood:UPDATE` | Publish own online-status payload |
| `neighbourhood.sendSignal` | `neighbourhood:UPDATE` | Send a targeted signal to one agent |
| `neighbourhood.sendBroadcast` | `neighbourhood:UPDATE` | Broadcast a signal to the Neighbourhood |

### A.1.6 `runtime`

| Operation | Capability | Purpose |
|---|---|---|
| `runtime.info` | `runtime:READ` | Executor + protocol version, init/unlock state |
| `runtime.quit` | `runtime:UPDATE` | Graceful shutdown |
| `runtime.openLink` | `runtime:UPDATE` | OS-level open of a URL (e.g. browser) |
| `runtime.verifySignature` | `runtime:VERIFY` | Verify a string signature against a DID |
| `runtime.exportData` / `runtime.importData` | `runtime:READ` / `CREATE` | Bulk export/import of agent state |
| `runtime.friends` / `runtime.addFriends` / `runtime.removeFriends` | `runtime.friends:READ/CREATE/DELETE` | Friend list management |
| `runtime.friendStatus` | `runtime.friends:READ` | One friend's online status |
| `runtime.sendFriendMessage` | `runtime.messages:CREATE` | Send a DM to a friend |
| `runtime.inbox` / `runtime.outbox` | `runtime.messages:READ` | Local message cache |
| `runtime.linkLanguageTemplates` | `runtime.known_link_languages:READ` | List `knownLinkLanguages` |
| `runtime.addLinkLanguageTemplates` | `runtime.known_link_languages:CREATE` | Add to `knownLinkLanguages` |
| `runtime.removeLinkLanguageTemplates` | `runtime.known_link_languages:DELETE` | Remove from `knownLinkLanguages` |
| `runtime.hcAgentInfos` / `runtime.addHcAgentInfos` | `runtime:READ/CREATE` | Holochain agent info pass-through |
| `runtime.networkMetrics` | `runtime:READ` | Network telemetry |
| `runtime.restartHolochain` | `runtime:UPDATE` | Restart the Holochain conductor |
| `runtime.tlsDomain` | `runtime:READ` | TLS domain config |
| `runtime.notifications` / `createNotification` / `deleteNotification` / `updateNotification` / `grantNotification` | `runtime:READ/CREATE/UPDATE/DELETE` | Local notification system |
| `runtime.computeLog` | `runtime:READ` | Compute-credit log |
| `runtime.hostRates` / `setHostRates` | `runtime.hosting:READ/UPDATE` | Hosting pricing |
| `runtime.freeHostingEnabled` / `setFreeHostingEnabled` | `runtime.hosting:READ/UPDATE` | Hosting free-tier toggle |
| `runtime.setStatus` | `runtime:UPDATE` | Set general status |
| `runtime.unyt*` | `runtime:READ/UPDATE` | UNYT-network payment/key operations |

### A.1.7 `ai` (optional, see §11.4 O-LANG-1)

| Operation | Capability | Purpose |
|---|---|---|
| `ai.models` / `ai.addModel` / `ai.removeModel` / `ai.updateModel` | `artificial intelligence:READ/CREATE/DELETE/UPDATE` | Model registry |
| `ai.getDefaultModel` / `ai.setDefaultModel` | `artificial intelligence:READ/UPDATE` | Default-model selection |
| `ai.modelLoadingStatus` | `artificial intelligence:READ` | Model load progress |
| `ai.prompt` | `artificial intelligence:PROMPT` | LLM inference |
| `ai.embed` | `artificial intelligence:PROMPT` | Embedding generation |
| `ai.tasks` / `addTask` / `removeTask` / `updateTask` | `artificial intelligence:*` | Task registry |
| `ai.transcriptionOpen` / `transcriptionClose` | `artificial intelligence:TRANSCRIBE` | Streaming transcription |

### A.1.8 `user` (multi-user mode, optional)

| Operation | Capability | Purpose |
|---|---|---|
| `user.create` / `list` | `runtime.user_management:CREATE/READ` | Sub-account management |
| `user.login` | `runtime.user_management:UPDATE` | Switch active user |
| `user.multiUserEnabled` / `setMultiUserEnabled` | `runtime.user_management:READ/UPDATE` | Toggle multi-user mode |
| `user.credits` / `wallet` | `runtime.user_management:READ` | User-scoped billing |
| `user.freeAccess` | `runtime.user_management:READ` | Free-tier check |
| `user.emailTest` / `requestVerification` / `verifyEmail` | `runtime.user_management:CREATE/UPDATE` | Email verification flow |

### A.1.9 `hosting`

| Operation | Capability | Purpose |
|---|---|---|
| `hosting.info` | `runtime.hosting:READ` | Hosting plan / status |
| `hosting.wallet` / `walletHistory` | `runtime.hosting:READ` | Hosting wallet balance / tx log |
| `hosting.setHotWallet` | `runtime.hosting:UPDATE` | Configure hot wallet |
| `hosting.requestPayment` | `runtime.hosting:CREATE` | Trigger a payment |

## A.2 Event types

Events arrive on the same socket as RPC responses, distinguished by absence of `id` (§7.3.4). The reference executor emits:

| Event `type` | Source topic | When |
|---|---|---|
| `agent-status-changed` | `AGENT_STATUS_CHANGED_TOPIC` | Agent init/lock state changed |
| `agent-updated` | `AGENT_UPDATED_TOPIC` | Agent profile changed |
| `apps-changed` | `APPS_CHANGED` | Authorized-apps list changed |
| `hosting-user-info-changed` | `HOSTING_USER_INFO_CHANGED_TOPIC` | Hosting plan / wallet changed |
| `perspective-added` | `PERSPECTIVE_ADDED_TOPIC` | New Perspective created |
| `perspective-removed` | `PERSPECTIVE_REMOVED_TOPIC` | Perspective deleted |
| `perspective-updated` | `PERSPECTIVE_UPDATED_TOPIC` | Perspective metadata changed |
| `sync-state-change` | `PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC` | A Perspective's sync state moved |
| `link-added` | `PERSPECTIVE_LINK_ADDED_TOPIC` | New link in any Perspective |
| `link-removed` | `PERSPECTIVE_LINK_REMOVED_TOPIC` | Link removed |
| `link-updated` | `PERSPECTIVE_LINK_UPDATED_TOPIC` | Link updated |
| `signal` | `NEIGHBOURHOOD_SIGNAL_TOPIC` | Telepresence signal received |
| `message-received` | `RUNTIME_MESSAGED_RECEIVED_TOPIC` | DM received |
| `notification-triggered` | `RUNTIME_NOTIFICATION_TRIGGERED_TOPIC` | Local notification fired |
| `runtime-exception` | `EXCEPTION_OCCURRED_TOPIC` | Internal exception surfaced to clients |
| `ai-transcription-text` | `AI_TRANSCRIPTION_TEXT_TOPIC` | Streaming transcription token |
| `query-subscription-update` | `PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC` | Subscribed query produced new rows |
| `compute-log-updated` | `COMPUTE_LOG_UPDATED_TOPIC` | Compute-credit log changed |

Each event's payload is a `data: {...}` field (per `wrap_event`) whose shape depends on the topic. Definitions in [`rust-executor/src/api/events_ws.rs`](../rust-executor/src/api/events_ws.rs) and [`rust-executor/src/pubsub.rs`](../rust-executor/src/pubsub.rs).
