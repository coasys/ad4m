# Events

An **event** is a typed message that the executor pushes on a client's RPC socket (`{ type, …payload }`, no `id`). Its publisher builds an `Event { topic, audience, capability, perspective, payload }`: the **topic** names the kind, the **audience** names who may receive it (`Everyone`, or `Dids`: the sessions that act as one of those agents), and the **capability** is the one that reads the same data over RPC. A socket delivers an event when its session is in the audience, holds the capability, and asked for the topic with `events.watch`.

**Why:** The publisher already holds the data, so it names the audience when it creates the event. Delivery is then one rule with no lock and no payload parse, where before each socket worked out the owners of each event at delivery time, topic by topic. A new topic does not compile until someone chooses its capability, so an app receives only what its grant can read. The operator acts as the main agent, so two audiences are enough. Language code cannot publish executor events, because a language could otherwise forge them.

**Parent:** [Executor interfaces](README.md)

<details><summary>How it works</summary>

```
publisher (PerspectiveInstance, AgentService, identity, host, storage, SFU, AI …)
   │ publish_event(Event::new(topic, Audience::Dids(recipients), payload))   one typed channel
   ▼
event_stream_of(session)     per socket: drop another connection's live-query update,
   │                         keep session.may_receive(e) && interest.wants(e)
   │                         ends at session.over() (grant revoked or expired)
   ▼
socket: /api/v1/ws (calls, events, live-query updates)
```

| Topics | Audience | Capability |
|---|---|---|
| `perspective-added/updated/removed`, `link-added/removed/updated`, `sync-state-change`, `query-subscription-update` | the perspective's owners (else the main agent), one event for all; a live-query update to its subscriber only | perspective query on that perspective |
| `signal` | its recipient; without one, the perspective's owners | neighbourhood `READ` |
| `agent-status-changed`, `agent-updated`, `flow-state-changed`, `transcription-text`, `auto-processor-*` | the agent the event names | the matching read |
| `apps-changed`, `exception-occurred` (`CapabilityRequested`) | the target agent | `AGENT_PERMIT` |
| `notification-triggered`, `hosting-user-info-changed`, `model-loading-status`, other exceptions, `sfu-*` | the owner, the user, `Everyone`, or the room's participants | the matching read |
| `operator-changed`, `storage-state-changed`, `storage-migration-progress` | the main agent | `agent:READ` |
| `tenant-joined`, `tenant-left`, `host-policy-changed`, `guest-removed`, `index-listing-changed`, `unyt-state-changed` | the main agent, which the operator's sessions act as | host and index reads |
| `tenant-usage-changed` | the tenant's agent | hosting read |
| `bot-activity` | the bot owner's sessions on this executor | — |
| `neighbourhood-membership-changed`, `neighbourhood-join-refused` | the perspective's owners; the calling connection | neighbourhood `READ` |

1. Only an authenticated session gets events. `events.watch` replaces the socket's interest; `events.unwatch` clears it. Topics scoped to a perspective take a list of UUIDs.
2. A socket that falls more than 10,000 messages behind on the channel loses them. For live queries it then gets `{ "type": "query-subscription-update", "lagged": true }` and resyncs. Other lost events are only logged; the client gets no notice of them.
3. `Topic::as_str` is the only list of wire names; `event_specs()` names each payload type, and a test writes them to the SDK's `Events.ts`.
4. No event announces the end of a session: its socket closes with 1008 ([Sessions](../access/sessions.md)).
5. No event carries direct messages: direct messages are not implemented ([Executor services](../executor/services/README.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/events.rs](../../../rust-executor/src/events.rs) (new): `Topic`, `Audience`, `Event`, its constructors (`approval_request`, `apps_changed`, `hosting_user_info`, `for_connection`, `sfu_*`).
- [rust-executor/src/pubsub.rs](../../../rust-executor/src/pubsub.rs): `publish_event`, `subscribe_events`.
- [rust-executor/src/agent/session.rs](../../../rust-executor/src/agent/session.rs): `Session::may_receive`.
- [rust-executor/src/api/events_ws.rs](../../../rust-executor/src/api/events_ws.rs): `event_stream_of`, `event_specs`, `wire_message`.
- [rust-executor/src/api/event_interest.rs](../../../rust-executor/src/api/event_interest.rs): `events.watch`, `events.unwatch`, `wants`.
- [rust-executor/src/perspectives/mod.rs](../../../rust-executor/src/perspectives/mod.rs): `event_recipients`, `publish_to_owners`.
- [rust-executor/src/js_core/pubsub_extension.rs](../../../rust-executor/src/js_core/pubsub_extension.rs): `PUBSUB.publish` refuses language code.
- [core/src/generated/api/Events.ts](../../../core/src/generated/api/Events.ts): generated event types; `ApiClient.on` in [core/src/apiClient.ts](../../../core/src/apiClient.ts) keeps `events.watch` in step with the handlers.

</details>
