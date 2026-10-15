# perspectives/perspective_instance/ — agent guide

Child modules of `perspective_instance.rs`. New `impl PerspectiveInstance` code goes here
instead of growing `perspective_instance.rs` (split plan: spec item 3). Being children of
that module, files here can use the instance's private fields.

| File | Role |
|---|---|
| `subscriptions.rs` | Live query subscriptions: delta updates, resync state (`perspective.resyncSubscription`), disposal per subscription or per connection |
| `abstract_classes.rs` | Abstract classes (#1435): resolution cache, reads, subscriptions, writes dispatched to the record's classes |
| `subject_writes.rs` | Member writes through adders / removers (`update_subject`'s `{action, value}`), `member_links`, and MCP's cascading delete (`remove_subject_cascading`) |

Invariants: same lock order as the parent (`batch_store` → `persisted`); never hold
`subscribed_queries` across an await that takes another instance lock.
