# Link write path

Every link write, local or arriving from a neighbourhood, follows one sequence inside `PerspectiveInstance`: store the change, flag live queries and notifications, emit link events, and, for a local shared write, commit to the link language. A **batch** collects writes in memory and runs the sequence once on commit.

**Why:** One sequence keeps the store, the subscribers and the neighbourhood consistent, whatever path the write came in by. A batch lets an instance with many properties reach readers and peers as one change, not as a burst of partial states.

**Parent:** [Links and store](README.md)

<details><summary>How it works</summary>

Local write (`add_link` → `add_link_expression`):

1. For a managed agent, check compute credits (see [Billing and hosting](../../agents/executor/services/billing-and-hosting.md)).
2. `Link::validate`, then sign as the caller's `AgentContext`.
3. `check_consensus_rule_links`: refuse a flow consensus rule this executor could not read back.
4. With a `batch_id`: append to the batch's `PerspectiveDiff` in `batch_store` and return.
5. `persist_link_diff` → `SparqlStore` (removals first, then additions).
6. `update_prolog_engines(diff)` → `spawn_prolog_facts_update`: in a spawned task, set the notification and live-query flags and record the changed predicates.
7. If `Shared`: `spawn_commit_and_handle_error` → `commit` ([Neighbourhood sync](../neighbourhoods/sync.md)).
8. `pubsub_publish_diff`: queue a flow pass if a flow definition changed, drop derived shapes if SHACL changed, seed bookkeeping for newly minted flow instances, then emit `link-added` / `link-removed` events once **per owner DID** ([Events](../../agents/interfaces/events.md)).

`link_mutations` (also behind `addLinks` and `removeLinks`) runs the same steps for many links, emits events before it commits, and commits only the shared part.

Inbound write (`diff_from_link_language`): de-duplicate, set status `Shared`, persist, clear the shape cache if SHACL arrived, then steps 6 and 8, plus `schedule_flow_consensus_pass`.

Batch: `perspective.createBatch` → writes with `batchId` → `perspective.commitBatch` signs the added links again, splits the diff into shared and local, starts the commit of the shared part when a link language is attached, then persists both. `create_batch` drops batches older than `BATCH_TIMEOUT_SECS` (300 s). Batches live only in memory: an executor restart loses an open batch, and the next call with its id fails with "Batch not found" or "No batch found with given UUID".

</details>

<details><summary>Where in the code</summary>

- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `add_link`, `add_link_expression`, `add_links`, `link_mutations`, `update_link`, `remove_link(s)`, `persist_link_diff`, `update_prolog_engines`, `spawn_prolog_facts_update`, `pubsub_publish_diff`, `diff_from_link_language`, `create_batch`, `commit_batch`, `discard_batch`.
- [pubsub.rs](../../../rust-executor/src/pubsub.rs): topic constants; events reach clients through `api/events_ws.rs` ([Events](../../agents/interfaces/events.md)).
- [api/perspectives_ws.rs](../../../rust-executor/src/api/perspectives_ws.rs): the RPC handlers and their credit checks (`check_credits`, `reserve_credits`).
- [billing.rs](../../../rust-executor/src/billing.rs): `check_compute_credits`, `bill_compute`.
- Guide: [docs-src/pages/developer-guides/batch-operations.mdx](../../../docs-src/pages/developer-guides/batch-operations.mdx).

</details>
