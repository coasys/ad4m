# Live queries

A **live query** is a SPARQL or model query that the executor re-runs after relevant writes. Each change goes only to the RPC connection that opened it, as a `query-subscription-update` event with a revision number and a **delta update** against the previous result. A **notification** is an app-registered SPARQL trigger: the executor sends its new matches to the owner's events and, if set, to a webhook.

**Why:** UIs need data that stays current without polling. The executor re-runs the query and computes the difference, so there is one source of truth for what changed. Deltas instead of full results keep updates small for large lists. `perspective.subscribeQuery` refuses any query that `is_sparql_query` does not see as SPARQL, with an RPC error of code 400 (in [perspectives_ws.rs](../../../rust-executor/src/api/perspectives_ws.rs)).

**Parent:** [Querying](README.md)

<details><summary>How it works</summary>

1. `perspective.subscribeQuery` (SPARQL) or `perspective.modelSubscribe` (model query) registers a `SubscribedQuery` under a new id, bound to the calling RPC connection, and returns the first result at revision 0.
2. Every write records its changed predicates (`record_changed_predicates`) and sets `trigger_subscription_check` ([Link write path](../links/write-path.md)).
3. `subscribed_queries_loop` checks the flag every 200 ms. When it is set, it waits 250 ms to collect a burst, then runs `check_subscribed_queries`.
4. A subscription is skipped when the writes cannot affect it: SPARQL by predicate overlap; model queries by `TriggerRules` (`any`, `joins`, `near`, `via`), which also watch classes reached through `include`, projections and `where`.
5. Re-run the whole query and compare with `last_result` (`result_delta`). The delta makes the update small on the socket; the executor still computes the full result. Model results send `ids`, `upsert`, `totalCount`; SPARQL results send `added` / `removed` rows; otherwise the whole `result`.
6. `send_delta_update` publishes a typed `query-subscription-update` event. Its audience is the subscribing agent, when that agent is still an owner; the subscription's connection is its routing key. A socket drops another connection's update, then checks that its session may receive the event ([Events](../../agents/interfaces/events.md)). A co-owner on the same executor gets no copy. A lagging stream gets `{ lagged: true }`.
7. A client that misses a revision calls `perspective.resyncSubscription`. Closing the socket disposes its subscriptions (`dispose_connection_subscriptions`).

Notifications: `notification_check_loop` checks its flag every 5 s. When writes happened, it runs each granted notification's SPARQL trigger, subtracts the previous matches and publishes only the new ones (`publish_notification_matches`). Triggers run through the internal `SparqlStore::query`, which re-encodes `?target` / `?t` bindings as `literal:` strings; client SPARQL does not ([SPARQL queries](sparql.md)). Who may receive a notification is in [Guards](../../agents/access/guards.md).

</details>

<details><summary>Where in the code</summary>

- [perspectives/perspective_instance/subscriptions.rs](../../../rust-executor/src/perspectives/perspective_instance/subscriptions.rs): `open_subscription`, `send_delta_update`, `subscription_state`, `dispose_*`, `result_delta`.
- [perspectives/perspective_instance/subscription_triggers.rs](../../../rust-executor/src/perspectives/perspective_instance/subscription_triggers.rs): `TriggerRules`, which writes re-run a model subscription.
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `subscribe_and_query`, `model_subscribe_and_query`, `subscribed_queries_loop`, `check_subscribed_queries`, `notification_check_loop`, `calc_notification_trigger_matches`.
- [events.rs](../../../rust-executor/src/events.rs) (new): `Event::for_connection`, the typed live-query event.
- [runtime_service/notification_access.rs](../../../rust-executor/src/runtime_service/notification_access.rs): which notifications may fire.
- SDK: [core/src/perspectives/LiveQuery.ts](../../../core/src/perspectives/LiveQuery.ts) (applies deltas), [core/src/perspectives/WakerSubscriptionManager.ts](../../../core/src/perspectives/WakerSubscriptionManager.ts), `subscribeQuery` / `modelSubscribe` in [PerspectiveProxy.ts](../../../core/src/perspectives/PerspectiveProxy.ts).

</details>
