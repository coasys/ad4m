# Live queries

A **live query** is a SPARQL or model query that the executor re-runs after relevant writes. Each change goes only to the RPC connection that opened it, as a `query-subscription-update` event with a revision number and a **delta update** against the previous result. A **notification** is an app-registered SPARQL trigger: the executor sends its new matches to the owner's events and, if set, to a webhook.

**Why:** UIs need data that stays current without polling. The executor re-runs the query and computes the difference, so there is one source of truth for what changed. Deltas instead of full results keep updates small for large lists. `perspective.subscribe` refuses a `sparql` query that `is_sparql_query` does not see as SPARQL, with an RPC error of code 400 (in [perspectives_ws.rs](../../../rust-executor/src/api/perspectives_ws.rs)).

**Parent:** [Querying](README.md)

<details><summary>How it works</summary>

1. `perspective.subscribe` takes exactly one of `sparql` (`subscribe_and_query`) and `model` (`model_subscribe_and_query`; a model query with an `after` cursor is refused). It registers a `SubscribedQuery` under the `subscriptionId` that the client chose (an id already in use is refused with 409), bound to the calling RPC connection, and returns the first result at revision 0.
2. Every write records its changed predicates (`record_changed_predicates`) and sets `trigger_subscription_check` ([Link write path](../links/write-path.md)).
3. `subscribed_queries_loop` checks the flag every 200 ms. When it is set, it waits 250 ms to collect a burst, then runs `check_subscribed_queries`.
4. A subscription is skipped when the writes cannot affect it: SPARQL by predicate overlap; model queries by `TriggerRules` (`any`, `joins`, `near`, `holders`, `scan`, `via`), which also watch classes reached through `include`, projections and `where`.
5. Re-run the whole query and compare with `last_result` (`result_delta`). Identical subscriptions share one run per re-check (`evaluation_key`, [Model queries](model-queries.md)). The delta makes the update small on the socket; the executor still computes the full result. Model results send `ids`, `upsert`, `totalCount`; SPARQL results send `added` / `removed` rows; otherwise the whole `result`.
6. `send_delta_update` publishes on the query-subscription topic with the connection id. The event stream delivers it only to that connection, and only when the subscribing agent still owns the perspective ([Events](../../agents/interfaces/events.md)).
7. A client that misses a revision calls `perspective.resyncSubscription`. Closing the socket disposes its subscriptions (`dispose_connection_subscriptions`). A subscribe call cancelled before its first result (`request.cancel`, or the socket closing) unregisters its query (`open_subscription`).
8. In the SDK, `Ad4mModel.query` and `Ad4mModel.queryOf` (model queries) and `PerspectiveProxy.liveSparql` (SPARQL) return a `LiveQuery`, which calls `perspective.subscribe` and applies each delta. After a reconnect the `LiveQuery` subscribes again. It retries a failed re-open with backoff (1 s, doubling, at most 60 s) until it succeeds or the query is disposed, and reports it to `onStatus` listeners (`reopening`, then `open`).

Notifications: a write sets the notification flag and wakes `notification_check_loop` (`request_notification_check`). The loop waits 100 ms to collect a burst and runs at most one check per second. It runs each granted notification's SPARQL trigger, subtracts the previous matches and publishes only the new ones (`publish_notification_matches`). Triggers run through the internal `SparqlStore::query`, which re-encodes `?target` / `?t` bindings as `literal:` strings; client SPARQL does not ([SPARQL queries](sparql.md)). Who may receive a notification is in [Access checks](../../agents/access/checks.md).

</details>

<details><summary>Where in the code</summary>

- [perspectives/perspective_instance/subscriptions.rs](../../../rust-executor/src/perspectives/perspective_instance/subscriptions.rs): `open_subscription`, `send_delta_update`, `subscription_state`, `dispose_query_subscription`, `dispose_connection_subscriptions`, `result_delta`.
- [perspectives/perspective_instance/subscription_triggers.rs](../../../rust-executor/src/perspectives/perspective_instance/subscription_triggers.rs): `TriggerRules`, which writes re-run a model subscription.
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `subscribe_and_query`, `model_subscribe_and_query`, `subscribed_queries_loop`, `check_subscribed_queries`, `notification_check_loop`, `calc_notification_trigger_matches`.
- [runtime_service/notification_access.rs](../../../rust-executor/src/runtime_service/notification_access.rs): which notifications may fire.
- SDK: [core/src/perspectives/LiveQuery.ts](../../../core/src/perspectives/LiveQuery.ts) (applies deltas), [core/src/perspectives/WakerSubscriptionManager.ts](../../../core/src/perspectives/WakerSubscriptionManager.ts), `liveSparql` in [PerspectiveProxy.ts](../../../core/src/perspectives/PerspectiveProxy.ts), `query` and `queryOf` in [Ad4mModel.ts](../../../core/src/model/Ad4mModel.ts), `subscribe` in [PerspectiveClient.ts](../../../core/src/perspectives/PerspectiveClient.ts).

</details>
