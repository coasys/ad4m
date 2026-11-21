# Events

An **event** is a typed JSON message (`{ type, …payload }`, no `id`) that the executor pushes on a client's RPC socket. Executor code publishes each change as an `Event` that names its topic, its **audience** (some DIDs, everyone, or the operator) and the capability a receiver needs. Each socket's event stream delivers the events that its session may receive, and only the event types in its **event interest**, which the client sets with `events.watch`.

**Why:** A change (a link write, a sync-state change, a call roster) must reach every interested client, and the writer must not need to know who listens. The publisher holds the data, so it names the audience, and a socket only compares. One executor serves several agents and apps whose events must not mix. The capability check gives a session no event whose data the matching RPC method would refuse it. `events.watch` keeps a client from receiving traffic it never asked for.

**Parent:** [Executor interfaces](README.md)

<details><summary>How it works</summary>

```
PerspectiveInstance, AgentService, SFU, AI, auto-processor …
   │ get_global_pubsub().publish_event(Event::new(Topic, Audience, payload))
   ▼                       (one tokio broadcast channel, 10 000 events)
build_event_stream(session, interest, connection)     one per RPC socket
   │  Event::connection names another socket   → drop  (live-query updates)
   │  Session::may_receive: in the Audience and holds Event::capability
   │  event_interest::wants: { "<event>": null | [perspective uuids] }   (nothing until the first watch)
   │  ends when Session::over() fires, or at the first event after Session::check() fails
   ▼
socket: /api/v1/ws (the only event socket)
```

| Events | Audience |
|---|---|
| `link-added`, `link-removed`, `link-updated`, `perspective-added`, `-removed`, `-updated`, `sync-state-change`, `auto-processor-neighbourhood-state` | the perspective's owners, or the main agent for an unowned perspective (`event_recipients`); one event for all of them, and no payload names an owner |
| `query-subscription-update` | the subscriber, if it is one of those, and only on the RPC connection that opened the live query, whatever its event interest |
| `signal` | the recipient DID; a signal without a recipient goes once to each owner, each copy naming that owner as `recipient` ([Presence and signals](../../perspectives/neighbourhoods/presence-and-signals.md)) |
| `agent-status-changed`, `agent-updated`, `transcription-text` | the one agent the event names |
| `auto-processor-event`, `flow-state-changed` | the agent the event names, if it is one of the perspective's owners; the main agent's flow state on a perspective it does not own goes to the operator only |
| `notification-triggered` | the notification's owner ([Live queries](../../perspectives/querying/live-queries.md)) |
| `apps-changed`, capability requests and notification install requests in `exception-occurred` | the agent that the approval is for, with `agent/PERMIT` ([App grants](../access/app-grants.md)) |
| `sfu-*` | the participant or the room members that the event names; `sfu-room-roster` after a departure also goes to the departed DID ([SFU](../executor/services/sfu.md)) |
| `hosting-user-info-changed` | the user it names, with `RUNTIME_HOSTING_READ`; a copy to the main agent, with `RUNTIME_USER_MANAGEMENT_READ` |
| `model-loading-status`, other exceptions | everyone |

1. Only an authenticated session (operator or a grant) gets a stream. An anonymous socket gets none.
2. `Session::may_receive`: a DID audience matches the DID that the session acts as (`acts_as_did`). The operator acts as the main agent, so it gets the main agent's events, not other agents'. The session must also hold the event's capability: the one that the RPC method for the same data checks (`Topic::capability`). A per-perspective event needs the query capability on its perspective, a signal neighbourhood READ on it.
3. `events.watch` replaces the socket's interest; `events.unwatch` clears it. Some event types carry `perspectiveUuid` (`EventSpec.scoped`). For those, a list of UUIDs narrows the interest to those perspectives.
4. A socket that falls more than 10 000 events behind loses them. It then gets `{ "type": "query-subscription-update", "lagged": true }`, and the client resyncs its live queries. Other lost events are only logged.
5. `event_specs()` names every event and its payload type, keyed by `Topic`. A unit test writes them to the SDK's `Events.ts`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/events.rs](../../../rust-executor/src/events.rs): `Topic` (wire names, `capability`), `Audience`, `Event` and its constructors.
- [rust-executor/src/pubsub.rs](../../../rust-executor/src/pubsub.rs): `PubSub::publish_event`, `subscribe_events`, `get_global_pubsub`.
- [rust-executor/src/api/events_ws.rs](../../../rust-executor/src/api/events_ws.rs): the event table (module header), `event_specs`, `build_event_stream`, `wire_message`.
- [rust-executor/src/agent/session.rs](../../../rust-executor/src/agent/session.rs): `Session::may_receive`, `Session::over`.
- [rust-executor/src/api/event_interest.rs](../../../rust-executor/src/api/event_interest.rs): `events.watch`, `events.unwatch`, `wants`.
- [rust-executor/src/perspectives/mod.rs](../../../rust-executor/src/perspectives/mod.rs): `event_recipients`, `publish_to_owners`, `publish_telepresence_signal`.
- [rust-executor/src/api/ws_rpc.rs](../../../rust-executor/src/api/ws_rpc.rs): the event stream of the RPC socket.
- [core/src/generated/api/Events.ts](../../../core/src/generated/api/Events.ts): generated event types; `ApiClient.on` in [core/src/apiClient.ts](../../../core/src/apiClient.ts) keeps `events.watch` in step with the handlers.

</details>
