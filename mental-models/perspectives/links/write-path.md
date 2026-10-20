# Link write path

Every link write, local or arriving from a neighbourhood, follows one sequence inside `PerspectiveInstance`: sign with the acting agent's device key, store the change with a link verdict, feed membership links to the membership replay, flag live queries and notifications, emit one link event to the perspective's owners, and, for a local shared write, commit to the link language. A **batch** collects writes in memory and runs the sequence once on commit.

**Why:** One sequence keeps the store, the membership replay, the subscribers and the neighbourhood consistent, whatever path the write came in by. A batch lets an instance with many properties reach readers and peers as one change, not as a burst of partial states.

**Parent:** [Links and store](README.md)

<details><summary>How it works</summary>

Local write (`add_link` → `add_link_expression`):

1. The RPC guard admits the session for this perspective ([Guards](../../agents/access/guards.md)). A session that acts as a bot agent also needs `write` for this neighbourhood in its [bot grant](../../agents/access/grants/bot-grants.md).
2. For a managed agent, one compute-credit check. Under the charge mode `Free` every cost is zero ([Billing and hosting](../../agents/executor/services/billing-and-hosting.md)).
3. `Link::validate`, then sign as the caller's `AgentContext`, with an anchor when the agent has a person ([Links](link-expressions.md)).
4. `check_consensus_rule_links`: refuse a flow consensus rule this executor could not read back.
5. A removal of a membership link is refused: the membership graph only grows ([Membership](../neighbourhoods/membership/README.md)).
6. With a `batch_id`: append to the batch's `PerspectiveDiff` in `batch_store` and return.
7. `persist_link_diff` → `SparqlStore` (removals first, then additions), with the verdict of each addition ([Link verdicts](verdicts.md)). A membership link also goes to `Membership::on_link`.
8. `update_prolog_engines(diff)` → `spawn_prolog_facts_update`: in a spawned task, set the notification and live-query flags and record the changed predicates.
9. If `Shared`: `spawn_commit_and_handle_error` → `commit` ([Neighbourhood sync](../neighbourhoods/sync.md)).
10. `pubsub_publish_diff`: queue a flow pass if a flow definition changed, drop derived shapes if SHACL changed, seed bookkeeping for newly minted flow instances, then publish one typed `link-added` / `link-removed` event whose audience is the perspective's owners ([Events](../../agents/interfaces/events.md)).

`link_mutations` (also behind `addLinks` and `removeLinks`) runs the same steps for many links, emits events before it commits, and commits only the shared part.

Inbound write (`diff_from_link_language`): de-duplicate; check each addition against its author and store its verdict; drop a removal of a membership link; ignore a membership link inside an encrypted diff (it counts only through the plaintext membership channel); set status `Shared`, persist, clear the shape cache if SHACL arrived, then steps 8 and 10, plus `schedule_flow_consensus_pass`. Links by authors who are not members stay stored; reads skip them ([Querying](../querying/README.md)).

Batch: `perspective.createBatch` → writes with `batchId` → `perspective.commitBatch` splits the diff into shared and local, persists both, then commits the shared part. An open batch expires after `BATCH_TIMEOUT_SECS` (300 s). Batches live only in memory: an executor restart loses an open batch, and the next call with its id fails with "Batch not found" or "No batch found with given UUID".

</details>

<details><summary>Where in the code</summary>

- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `add_link`, `add_link_expression`, `add_links`, `link_mutations`, `update_link`, `remove_link(s)`, `persist_link_diff`, `update_prolog_engines`, `spawn_prolog_facts_update`, `pubsub_publish_diff`, `diff_from_link_language`, `create_batch`, `commit_batch`, `discard_batch`.
- [membership.rs](../../../rust-executor/src/membership.rs) (new): `Membership::on_link`.
- [events.rs](../../../rust-executor/src/events.rs) (new): `Event`, `Topic`, `Audience`; [pubsub.rs](../../../rust-executor/src/pubsub.rs) carries them to the RPC socket ([Events](../../agents/interfaces/events.md)).
- [api/perspectives_ws.rs](../../../rust-executor/src/api/perspectives_ws.rs): the RPC handlers and their credit check.
- [billing.rs](../../../rust-executor/src/billing.rs): `check_compute_credits`, `bill_compute`.
- Guide: [docs-src/pages/developer-guides/batch-operations.mdx](../../../docs-src/pages/developer-guides/batch-operations.mdx).

</details>
