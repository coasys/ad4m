# perspectives/perspective_instance/ — agent guide

Child modules of `perspective_instance.rs`. New `impl PerspectiveInstance` code goes here
instead of growing `perspective_instance.rs` (split plan: spec item 3). Being children of
that module, files here can use the instance's private fields.

| File | Role |
|---|---|
| `subscriptions_v2.rs` | Live query subscriptions: delta updates, resync state (`perspective.resyncSubscription`), disposal per subscription or per connection |

Invariants: same lock order as the parent (`batch_store` → `persisted`); never hold
`subscribed_queries` across an await that takes another instance lock.
