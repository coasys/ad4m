# perspectives/perspective_instance/ — agent guide

Child modules of `perspective_instance.rs`. New `impl PerspectiveInstance` code goes here
instead of growing `perspective_instance.rs` (split plan: spec item 3). Being children of
that module, files here can use the instance's private fields.

| File | Role |
|---|---|
| `subscriptions.rs` | Live query subscriptions: delta updates, resync state (`perspective.resyncSubscription`), disposal per subscription or per connection |
| `local_agents.rs` | Registering this executor's owners with the link language (`peersSetLocal`), one registration at a time |
| `expected_links.rs` | `commit_batch_if` (`commit_batch` delegates to it): a batch commits only if the links it expects (`expect_links`) still stand, checked and written under `persist_lock`; else `LinksChanged` and nothing is written |

Invariants: same lock order as the parent (`batch_store` → `persisted`); never hold
`subscribed_queries` across an await that takes another instance lock.
`local_agents_mutex` is held across `peersSetLocal`; take it before `persisted`.
