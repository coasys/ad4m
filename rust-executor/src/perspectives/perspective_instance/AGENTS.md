# perspectives/perspective_instance/ — agent guide

Child modules of `perspective_instance.rs`. New `impl PerspectiveInstance` code goes here
instead of growing `perspective_instance.rs` (split plan: spec item 3). Being children of
that module, files here can use the instance's private fields.

| File | Role |
|---|---|
| `subscriptions.rs` | Live query subscriptions: delta updates, resync state (`perspective.resyncSubscription`), disposal per subscription or per connection |
| `runner.rs` | `run_model_query`: the one path of every model query (call, subscription's first run, re-check); identical in-flight runs share one store run on a `RECHECK_SLOTS` slot. See `../AGENTS.md` |
| `local_agents.rs` | Registering this executor's owners with the link language (`peersSetLocal`), one registration at a time |
| `commit_tokens.rs` | The commit token bucket (`CommitTokens`, `COMMIT_BURST`, `COMMIT_REFILL_INTERVAL`): every `link_language.commit` of a perspective takes a token (`take_commit_token`); tests set them with `set_commit_tokens`. See `../AGENTS.md` (`commit`) |
| `expected_links.rs` | `commit_batch_if` (`commit_batch` delegates to it): a batch commits only if the links it expects (`expect_links`) still stand, checked and written under `persist_lock`; else `LinksChanged` and nothing is written. Also `has_batch` |
| `batch_view.rs` | A batch's effective graph: `get_links_effective` (committed + staged additions − staged removals, one `EffectiveLink` per link identity with `committed`/`staged` presences kept apart — `addLinkExpression` can restage the stored expression verbatim — for the validated write core's reads inside a caller's batch) and `cancel_batch_addition` (drop staged additions — the removal of a link that exists only inside the batch; a committed copy's removal is `remove_link`'s) |

Invariants: same lock order as the parent (`batch_store` → `persisted`); never hold
`subscribed_queries` across an await that takes another instance lock.
`local_agents_mutex` is held across `peersSetLocal`; take it before `persisted`.
