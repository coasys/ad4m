# Events

An **event** is a typed JSON message (`{ type, …payload }`, no `id`) that the executor pushes on a client's socket. Executor code publishes each change once on an in-process **pub/sub topic**. Each socket merges all topics into one stream and filters each event for its session. It forwards only the event types in its **event interest**, which the client sets with `events.watch`.

**Why:** A change (a link write, a sync-state change, a call roster) must reach every interested client, and the writer must not need to know who listens. A broadcast channel per topic separates the two. The filter runs per socket, because one executor serves several agents and apps whose events must not mix. `events.watch` keeps a client from receiving traffic it never asked for.

**Parent:** [Executor interfaces](README.md)

<details><summary>How it works</summary>

```
PerspectiveInstance, AgentService, SFU, AI, auto-processor …
   │ get_global_pubsub().publish(TOPIC, json)        (tokio broadcast, 10 000 per topic)
   ▼
build_event_stream(session)   one per socket: ~25 topic streams merged,
   │                          each with its own filter (owner DID, recipient, capability)
   │                          ends at the first event after Session::check() fails
   ▼
events.watch interest         { "<event>": null | [perspective uuids] }   (nothing until the first watch)
   ▼
socket: /api/v1/ws (with live-query updates)  or  /api/v1/ws/events (events only)
```

| Events | Who receives them |
|---|---|
| `link-added`, `link-removed`, `link-updated`, `perspective-added`, `-removed`, `-updated` | sessions whose agent DID equals the event's `owner`; a write publishes one copy per owner |
| `sync-state-change`, `auto-processor-neighbourhood-state` | the perspective's owners |
| `agent-status-changed`, `agent-updated`, `flow-state-changed`, `transcription-text`, `auto-processor-event` | the one agent the event names |
| `signal` | the recipient DID; a broadcast is published once per owner ([Presence and signals](../../perspectives/neighbourhoods/presence-and-signals.md)); a signal with no recipient (or a payload that does not parse) goes to every socket that receives events (step 1) (`matches_signal_recipient` in [events_ws.rs](../../../rust-executor/src/api/events_ws.rs)) |
| `notification-triggered` | the notification's owner ([Live queries](../../perspectives/querying/live-queries.md)) |
| `apps-changed`, capability requests in `exception-occurred` | sessions with `agent/PERMIT` |
| `sfu-*` | sessions with `neighbourhood/READ`, and only the named participant or room member ([SFU](../executor/services/sfu.md)) |
| `hosting-user-info-changed` | the user it names (by email); the operator and main agent see all |
| `model-loading-status`, `message-received`, other exceptions | every session that receives events |
| `query-subscription-update` | only the RPC connection that opened the live query |

1. Only an authenticated session (operator or a grant) gets a stream on `/api/v1/ws`. An anonymous socket gets none. `/api/v1/ws/events` also needs `agent/READ`.
2. `events.watch` replaces the socket's interest; `events.unwatch` clears it. Some event types carry `perspectiveUuid` (`EventSpec.scoped`). For those, a list of UUIDs narrows the interest to those perspectives.
3. A socket that falls more than 10 000 messages behind on a topic loses them. For live queries it then gets `{ "type": "query-subscription-update", "lagged": true }` and resyncs. Other lost events are only logged.
4. `event_specs()` names every event and its payload type. A unit test writes them to the SDK's `Events.ts`.
5. `message-received` belongs to direct messages, which are not implemented; no code publishes it ([Executor services](../executor/services/README.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/pubsub.rs](../../../rust-executor/src/pubsub.rs): `PubSub`, the topic constants, `get_global_pubsub`.
- [rust-executor/src/api/events_ws.rs](../../../rust-executor/src/api/events_ws.rs): the event table (module header), `events::*` names, `event_specs`, `build_event_stream`, the `matches_*` filters, `LazyDid`, `/api/v1/ws/events`.
- [rust-executor/src/api/event_interest.rs](../../../rust-executor/src/api/event_interest.rs): `events.watch`, `events.unwatch`, `wants`.
- [rust-executor/src/api/ws_rpc.rs](../../../rust-executor/src/api/ws_rpc.rs): `may_receive_events`, the event stream of the RPC socket.
- [core/src/generated/api/Events.ts](../../../core/src/generated/api/Events.ts): generated event types; `ApiClient.on` in [core/src/apiClient.ts](../../../core/src/apiClient.ts) keeps `events.watch` in step with the handlers.

</details>
