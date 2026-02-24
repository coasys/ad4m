# Ad4mModel Subscription Strategy

## Background

There are two distinct subscription systems in ad4m that need to be understood separately:

1. **Prolog query subscriptions** — `perspective.subscribeInfer(query)` → `QuerySubscriptionProxy` → `perspectiveQuerySubscription` GQL subscription. Used for live Prolog inference. **Actively used, not touched.**
2. **SurrealDB query subscriptions (server-push)** — `perspective.subscribeSurrealDB(query)` → `QuerySubscriptionProxy (isSurrealDB=true)` → `perspectiveSubscribeSurrealQuery` mutation → Rust server-side re-query loop → pubsub push → `perspectiveQuerySubscription` GQL subscription. **This is the old system being replaced.**

`Ad4mModel.subscribe()` is the new system that supersedes #2 entirely.

---

## The Old Architecture (Server-Push)

When a consumer called `perspective.subscribeSurrealDB(query)`, this is what happened:

```
Client                                    Rust Executor
──────                                    ─────────────
subscribeSurrealDB(query)
  → perspectiveSubscribeSurrealQuery mutation ──────────→ subscribe_and_query_surreal()
                                                              runs query immediately
                                                              inserts SurrealSubscribedQuery {
                                                                query, last_result,
                                                                last_keepalive: Instant::now(),
                                                                user_email,
                                                              }
  ← { subscriptionId, initialResult } ←────────────────

  opens perspectiveQuerySubscription WS ──────────────→ (GQL subscription stream)

  [every 30s]
  → perspectiveKeepAliveSurrealQuery ───────────────────→ query.last_keepalive = Instant::now()

  [on any link change in perspective]
                                          trigger_surreal_subscription_check = true
                                          surreal_subscription_cleanup_loop wakes up
                                            for each SurrealSubscribedQuery:
                                              re-runs SurrealQL query
                                              if result changed:
                                                publishes to PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC

  ← perspectiveQuerySubscription fires ←───────────────

  [on dispose]
  → perspectiveDisposeSurrealQuerySubscription ─────────→ removes from HashMap
```

**Problems:**

- **Server-side state accumulation**: every active subscription is a `SurrealSubscribedQuery` entry in a `HashMap<String, SurrealSubscribedQuery>` inside `PerspectiveInstance`. The Rust process holds this state indefinitely until the client explicitly disposes or the keepalive times out.
- **Keepalive fragility**: if the client crashes, navigates away, or loses connectivity, the subscription leaks until the 30s timeout fires and the cleanup loop evicts it. During that window the server keeps re-running SurrealQL on every link change for a client that isn't listening.
- **Cleanup loop complexity**: `surreal_subscription_cleanup_loop`, `check_surreal_subscribed_queries`, `trigger_surreal_subscription_check`, `keepalive_surreal_query` — ~150 lines of Rust dedicated purely to managing lifecycle of a feature that can be eliminated.
- **Polling semantics hidden behind push API**: despite the push appearance, the Rust loop re-runs the full query on every link change. For large perspectives with many subscriptions, this is N query executions per link write where N = number of active subscriptions. No batching, no predicate filtering before re-query.
- **Round-trip latency for every update**: link change → Rust detects → re-queries SurrealDB → pubsub → WS → client. The client has the SurrealDB connection anyway and could have queried directly.

---

## The New Architecture (Client-Side)

`Ad4mModel.subscribe()` uses `PerspectiveProxy.addListener('link-added', ...)` and `addListener('link-removed', ...)`, which are backed by the existing `perspectiveLinkAdded` / `perspectiveLinkRemoved` GraphQL subscriptions — persistent WebSocket streams that already exist on every `PerspectiveProxy` instance.

```
Client (PerspectiveProxy + createSubscription)           Rust Executor
──────────────────────────────────────────               ─────────────
Ad4mModel.subscribe(perspective, options, cb)

  registers listener on perspectiveLinkAdded/Removed
  (these WS subscriptions already exist — no new
   connections opened)

  runs initial findAll() immediately ──────────────────→ SurrealQL query
  ← results ←─────────────────────────────────────────

  invokes cb(results) immediately

  [on any link change]
                                          perspectiveLinkAdded fires on WS ──→
  checkPredicateRelevance(link, metadata)
    (is this predicate used by this model?)
  if relevant:
    re-runs findAll() ───────────────────────────────→ SurrealQL query
    ← results ←─────────────────────────────────────
    invokes cb(results)

  [on unsubscribe()]
  removes listener from PerspectiveProxy arrays
  (no server call needed — no server state to clean up)
```

**Properties:**

- **Zero server state**: no Rust HashMap entries, no keepalive, no cleanup loop, no timeout
- **No extra connections**: piggybacks on the link-event WS subscriptions that `PerspectiveProxy` already maintains for every perspective
- **Client controls re-query**: the debounce and predicate relevance check happen in the same process as the caller — no round-trip before deciding whether to re-query
- **Composable with IncludeMap**: `findAll()` with `include` runs multiple SurrealDB queries client-side. The old system had no concept of includes — it ran a single flat query and the client would have needed to hydrate separately anyway
- **Correct failure mode**: if the client disconnects, the listener is garbage-collected with the `PerspectiveProxy`. There is nothing to clean up on the server.

---

## Multi-User Node Compatibility

This was the primary concern when evaluating whether the client-side approach was safe to adopt.

### How multi-user nodes actually work

Ad4m's multi-user node is **not** a stateless HTTP server. Every user connects via their own **persistent WebSocket connection** to the same executor process. Authentication is via JWT token containing `user_email`, resolved by `AgentContext::from_auth_token()`.

The `perspectiveLinkAdded` subscription resolver in Rust already handles multi-user isolation:

```rust
// subscription_resolvers.rs
async fn perspective_link_added(..., uuid: String) -> ... {
    // 1. Verify the user's token grants access to this perspective
    let user_email = user_email_from_token(context.auth_token.clone());
    if !can_access_perspective(&user_email, &handle) {
        return Err("Access denied");
    }

    // 2. Filter pubsub events by "uuid|agent_did"
    //    Each user's WS only receives link events for perspectives they own/can access
    let filter = get_agent_did_filter(context.auth_token.clone(), ...);
    subscribe_and_process::<PerspectiveLinkWithOwner>(pubsub, topic, filter).await
}
```

**Consequence:** when `Ad4mModel.subscribe()` attaches a listener via `addListener('link-added', cb)`, that listener is only triggered by link events that have already passed the `uuid|agent_did` filter on the server. The access control that the old server-push system had to re-implement on `SurrealSubscribedQuery.user_email` is inherited for free from the link-event WS subscription.

### Comparison

| Concern                      | Old server-push                                    | New client-side                             |
| ---------------------------- | -------------------------------------------------- | ------------------------------------------- |
| Works with multi-user nodes  | ✅ (explicit `user_email` on subscription state)   | ✅ (inherits WS-level DID filter)           |
| Access control correctness   | ⚠️ race window between link event and cleanup loop | ✅ no race — filter applied at event source |
| Requires persistent WS       | ✅ (for keepalive)                                 | ✅ (for link-event stream)                  |
| Works with HTTP-only (no WS) | ✅ keepalive via polling                           | ❌ (see Future section)                     |

Both approaches require a persistent WebSocket. The new approach is not weaker — it has the same transport requirement but fewer moving parts.

---

## Shared Subscription Registry (`createSubscription`)

A naive client-side implementation would create one `addListener` call per `Ad4mModel.subscribe()` call. If 10 React components each subscribe to `Post.subscribe(perspective, { where: { published: true } }, cb)`, that's 10 independent listeners each firing a SurrealDB query on every link change.

`createSubscription()` in `subscription.ts` prevents this via a shared registry:

```
WeakMap<PerspectiveProxy, Map<queryFingerprint, SubscriptionEntry>>
```

- **Same query + same perspective** → single shared `SubscriptionEntry` with one listener
- All 10 components share that one listener; only one SurrealDB query fires per link change
- When the last subscriber calls `unsubscribe()`, the `SubscriptionEntry` is torn down and the listener removed
- `stableQueryKey(query)` produces a deterministic fingerprint from the `Query<T>` object (JSON-stable, handles object key ordering)
- Late subscribers (attaching after the first result has arrived) immediately receive the cached result, then receive live updates — no initial re-query

This registry is the reason the client-side approach scales well for typical application use cases (multiple UI components observing the same model).

---

## Debounce

Rapid writes — e.g. a transaction writing 20 links at once — would trigger 20 re-queries without debouncing. `SubscribeOptions.debounce` (default: configurable, recommended 100–300ms for UI) collapses the burst into a single re-query after the last event.

Debounce is on `subscribe()` options, not on `Query<T>`, because it's a delivery concern not a data concern. `findAll()` is unaffected.

---

## Predicate Relevance Check

Before re-running a query, `createSubscription` checks whether the changed link's predicate is one that the model cares about. This is a fast in-memory check against `ModelMetadata.properties` and `ModelMetadata.relations` predicate lists. If the link change is for an unrelated predicate (e.g. a `rdf://comment` link changing when you're subscribed to `Post` which uses `rdf://title` and `flux://content`), no query is issued.

This is a significant optimisation in busy perspectives with many concurrent model types.

---

## Future Scaling Path

The current approach has one real limitation: it requires a persistent WebSocket connection between each client and the executor. This is fine for the current deployment model (Electron app, `we` multi-user node, ad4m-connect). It would not work for a future hypothetical HTTP-only API.

**If that becomes necessary, the correct path is SurrealDB native `LIVE SELECT`**, not the old polling loop:

```sql
-- SurrealDB native live query (available today)
LIVE SELECT * FROM link WHERE predicate = 'rdf://title';
-- SurrealDB pushes a diff to the connection on every matching write
```

SurrealDB's native live queries eliminate the server-side polling loop entirely — the database itself pushes deltas. The Rust executor would subscribe to relevant `LIVE SELECT` results and push them via pubsub, which is a much cleaner server-push implementation than re-running full queries on every link change.

This is a future item, not a current concern. The client-side approach is correct for all current deployment targets.

---

## Prolog Subscriptions (`subscribeInfer`) — Why They Stay Server-Side

### Are Prolog subscriptions still needed?

Yes. The SHACL migration (PR #654) disabled Prolog for the _model system's internal query pipeline_ — `Ad4mModel.findAll()` no longer generates Prolog queries, and the SDNA pipeline no longer stores model definitions as Prolog facts. But Prolog is explicitly preserved as an **opt-in tool for hand-crafted queries** — recursive graph traversal, multi-hop reachability, constraint solving — where it has genuine expressive advantages over SurrealQL. `subscribeInfer` is the live/reactive version of `infer()` for those use cases, and it is actively tested in `tests/js/tests/perspective.ts`.

### Why server-side is correct for Prolog (and not just a legacy holdover)

The critical difference from SurrealDB: **there is no Prolog engine on the client**. SurrealDB queries are sent over the same Apollo WS connection and executed against a process the client shares. Prolog inference requires a SWI-Prolog (or similar) engine with the perspective's triple facts loaded — that engine lives entirely inside the Rust executor.

The `PrologService` in `rust-executor` maintains a `SimpleEngine` struct per perspective per user, containing two dedicated `PrologEngine` instances:

- `query_engine` — for ad-hoc `infer()` calls
- `subscription_engine` — a **separate, dedicated engine kept warm specifically for subscription re-runs**

When `subscribeInfer(query)` registers a subscription, the Rust executor:

1. Calls `ensure_engine_updated()` to load/refresh the perspective's Prolog facts into the `subscription_engine`
2. Runs the query immediately on that warm engine for the initial result
3. On every subsequent link change, `check_subscribed_queries()` re-runs the query on the **already-warm** `subscription_engine` — no re-initialization, no facts reload

This dedicated warm engine is important. Loading a perspective's triple base into a fresh Prolog engine is non-trivial — the subscription pool exists specifically to amortize that cost across many re-runs.

### Could you do Prolog subscriptions client-side anyway?

Technically you could mirror the `Ad4mModel.subscribe()` pattern for Prolog:

```typescript
// Hypothetical client-side Prolog subscription
addListener("link-added", async () => {
  const result = await perspective.infer(query);
  callback(result);
});
```

This would work, but it is _worse_ than the server-push approach for Prolog specifically:

| Factor                           | Server-push (current)                                 | Client-side `infer()` on each change                                 |
| -------------------------------- | ----------------------------------------------------- | -------------------------------------------------------------------- |
| Engine warm-up cost              | Paid once, engine stays warm                          | Paid on every re-run (or via separate keep-warm mechanism)           |
| Filtered pool reference counting | `subscription_ended()` decrements pool refs correctly | Never decremented — pool resources would leak                        |
| Fact freshness                   | Server coordinates fact update → re-query atomically  | Race: `infer()` may arrive before facts are updated for the new link |
| `run_query_smart` routing        | Dedicated `subscription_engine` path                  | Goes through general `query_engine`, competing with ad-hoc queries   |

The server-push model for Prolog is _architecturally motivated_, not just legacy. The server has context the client doesn't: it knows when the fact base has been updated, it has a pre-warmed engine, and it can reference-count pool resources correctly.

### The fundamental asymmetry

|                               | SurrealDB query subscription                 | Prolog query subscription                                    |
| ----------------------------- | -------------------------------------------- | ------------------------------------------------------------ |
| Engine lives...               | Client can connect to same SurrealDB         | Server only (SWI-Prolog in Rust process)                     |
| Re-query cost                 | Stateless SQL-like query, negligible warm-up | Prolog engine init + fact loading, significant               |
| Warm state benefit            | None — each query is independent             | High — `subscription_engine` stays loaded                    |
| Client can re-query directly? | ✅ Yes, same result, no overhead             | ❌ No — must call `infer()` which goes to server anyway      |
| Server-side advantage         | None                                         | Warm engine, atomic fact-update coordination, pool lifecycle |

**Conclusion:** `subscribeInfer` staying server-side is the _correct_ architecture for Prolog, for the same reasons `Ad4mModel.subscribe()` moving client-side is the correct architecture for SurrealDB. The asymmetry isn't inconsistency — it reflects the fundamentally different nature of the two query engines.

---

## Summary

| Property                                    | Old server-push                        | New client-side                        |
| ------------------------------------------- | -------------------------------------- | -------------------------------------- |
| Server state per subscription               | ✅ HashMap entry + keepalive timer     | ❌ none                                |
| Keepalive required                          | ✅ every 30s                           | ❌ not needed                          |
| Cleanup loop on server                      | ✅ ~150 lines Rust                     | ❌ deleted                             |
| Extra round-trip per update                 | ✅ link event → server re-query → push | ❌ client re-queries directly          |
| IncludeMap hydration                        | ❌ server only ran flat query          | ✅ full findAll() with includes        |
| Shared registry (N components → 1 listener) | ❌ N server subscriptions              | ✅ 1 shared listener                   |
| Debounce                                    | ❌ not supported                       | ✅ configurable                        |
| Predicate relevance filtering               | ❌ re-queries on every link change     | ✅ skips irrelevant predicates         |
| Multi-user node compatible                  | ✅                                     | ✅ (inherits WS-level DID filter)      |
| HTTP-only compatible (future)               | ✅                                     | ❌ (SurrealDB LIVE SELECT when needed) |
