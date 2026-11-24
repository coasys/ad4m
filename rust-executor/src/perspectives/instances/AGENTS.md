# perspectives/instances/ — agent guide

The validated instance writes: the one implementation behind the MCP `instance_*` write tools
(`mcp/tools/instances/`) and the `perspective.mutate` RPC method
(`api/perspective_instances_ws.rs`).

| File | Role |
|---|---|
| `write.rs` | `create`, `update`, `remove`, `add_to_collection`, `remove_from_collection`; their params (`Instance*`, camelCase, also the RPC bodies) and results |
| `mutate.rs` | `mutate`: an ordered list of `MutateOp` (create, update with `set` / `add` / `remove`, delete) run as one all-or-nothing call over the `write.rs` functions; `ParentRef`, `Returns`, `PerspectiveMutateResult`, `MutateError` (the failing op's index and its `InstanceError`) |
| `validate.rs` | `validate_properties` / `coerce_scalar`: types, cardinality, required, `sh:in`; `ValidationError` (typed: `ValidationProblem`, `Cardinality`, `allowedValues`) |
| `error.rs` | `InstanceError` (tagged by `kind`, the RPC error's `data`), `WriteStep`, `CascadeStep` |
| `mod.rs` | `PropView` over a `ModelShape`, class resolution, instance ids, `fetch_instance`, `check_setters`, `property_target`, the remove cascade, `Pending` |

Entry points take `(&mut PerspectiveInstance, &AgentContext, params)`; the agent is the
caller's `acts_as()`.

Invariants:
- Validate before writing: a refused call writes and stages no link (validation runs first).
  A failure that is not a refusal — a store error mid-write — can leave part of the write
  staged in the caller's batch; the core never discards a batch it does not own, so the
  caller must (`perspective.discardBatch`), not commit it to keep earlier calls.
- No text here. Errors and results are data; MCP (`mcp/tools/instances/validate.rs`,
  `*_error` in each tool file) and the RPC (`instance_error`) word them. A new
  `ValidationProblem` or `WriteStep` needs its text in both.
- Without `batch_id`: create/update commit their own batch (discarded on failure); collection
  writes and the remove cascade write directly. With `batch_id`: checked open
  (`BatchNotFound`) before writing, everything goes into it, nothing is committed, and the
  caller's batch is never discarded here.
- In a caller's batch, the reads a write does of the links it manages — collection
  membership, the remove cascade — see the batch's effective graph
  (`get_links_effective` in `perspective_instance/batch_view.rs`: committed + staged
  additions − staged removals, deduplicated to one `EffectiveLink` per link identity
  that flags the `committed`/`staged` presences — `addLinkExpression` can restage a
  stored expression, so both can hold); removal drops the staged copies
  (`cancel_batch_addition`) and stages the committed copy's (`remove_link`). Instance
  existence (`fetch_instance`, `require_instance`) reads committed state, overlaid by `Pending`
  (the ids this `mutate` call created or removed, each as a fact about one class of the id: an
  id the call did not touch as that class is whatever the store says) when the call passes
  one. `Pending` lives for one call: an instance an earlier call staged into the caller's batch is not found, and
  the single writes (`create`, `update`, ...) called alone pass none.
- With no caller's batch, `mutate` commits its own batch with `commit_batch_if`, expecting
  (`expected_by_batch`) what the batch's replacing commands read — a single-valued setter, a
  collection setter, a clear: each records the `(source, predicate)` links it read, an empty
  read included (`record_read`, the batch's `reads`) — plus, for removals by id that no such
  read covers (`remove`), what the store holds now, a removal no longer held failing at once.
  Links are compared in full (author, timestamp, target). Another writer that changed them
  meanwhile makes the call fail with `ConcurrentWrite` (409, retryable): two calls that set one
  single-valued property or relation cannot both commit, whether it had a value or not.
- `mutate` applies all ops or none. Its own batch is discarded on failure. With a caller's
  batch it works in a private fork of it (`fork_batch`: a copy, so its reads see the caller's
  work plus its own) and, when every op succeeded, merges what the fork changed into the
  caller's batch in one step under the batch store's write lock (`merge_batch`); on failure
  it drops the fork. The merge is conditional: the caller's batch must still equal what the
  fork was copied from. A commit makes the merge find no batch (`BatchNotFound`); a raw
  add, a raw update or another call's merge during the call makes it find a changed one
  (`BatchConflict`, 409, retryable: the caller reads again and repeats the call). Either way
  the call publishes nothing and nothing the other writer staged is lost or half-undone. The failing op's index travels
  with the error (`MutateError`) and the RPC puts it in the message and in `data.opIndex`.
  A call cancelled by `request.cancel` or the end of its session drops the handler's future:
  `DropDiscard` discards the fork (or the call's own batch) on a task, and stays armed
  until the awaited merge, discard or commit has returned, so a cancel that lands while that
  step waits for the batch store's lock still cleans up. A closed socket does not cancel the
  call (`ws_rpc` fires the token only for those two), so after a transport failure the
  caller cannot know whether the call merged or committed.

Write rules (one set, for the MCP tools and `perspective.mutate`, whose handlers call these
functions):
- Create: a property must be declared by the class (`unknownProperty` otherwise). `null` or
  `""` writes nothing. An array on a relation writes one link per member, and for an
  ordered relation the ordering entries its strategy needs (`PerspectiveInstance::order_appended`,
  the engine's `apply_collection_ordering`); a create with a `Relation` parent does the same
  for the parent's relation.
- Update: `null` or `""` clears a property (a required one is `missingRequired`); an array on a
  relation sets its members (adds the missing, removes the extra, keeps the ordering
  entries). `add_to_collection` / `remove_from_collection` (`add` / `remove` of an update) are
  incremental; an `add` to an ordered relation writes its ordering entry. A `set` of `null`,
  `""` or `[]` on a required collection is `missingRequired`, as for a scalar. A single-valued
  relation takes one id (or `null` to clear); only a collection takes an array.
  Through an abstract class, `add` and `remove` go to each record class's own collection
  (`collection_targets`), as `set` does.
- `ParentRef::Relation.model` is optional: without it the class is the one of the parent
  instance that declares the relation (`ParentClassUnresolved` unless exactly one does). A
  parent created earlier in the same call is read from `Pending`, so it needs no `model`.
- Delete: run the class's destructor if it has a non-empty one; else follow each record class
  through an abstract class, or remove the class's own declared predicates; then remove every
  link that targets the instance (`remove_incoming_links_of`). `links_removed` counts both.
  Removing the incoming links does not respect a second class on the same node: a node of
  several classes loses the links that point at it.
- `PerspectiveInstance::{create,update,remove}_subject_raw` are the class-side half, called
  by these functions after validation and by the executor's own hard-wired classes
  (auto processor, flows, interpretation overlay), which write executor-owned shapes.
  User-facing code never calls them.

Do not: add MCP- or RPC-specific wording, or read the token: take the `AgentContext`.
Known wart: still calls `crate::mcp::shacl` for class names, predicates and link status.
