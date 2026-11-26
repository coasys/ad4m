# perspectives/instances/ — agent guide

The validated instance writes: the one implementation behind the MCP `instance_*` write tools
(`mcp/tools/instances/`) and the `perspective.instance*` RPC methods
(`api/perspective_instances_ws.rs`). TS `Ad4mModel` is to call the RPC methods (A3b).

| File | Role |
|---|---|
| `write.rs` | `create`, `update`, `remove`, `add_to_collection`, `remove_from_collection`; their params (`Instance*`, camelCase, also the RPC bodies) and results |
| `plan.rs` | `apply`: a write plan, several creates and collection adds, each through `create` / `add_to_collection`, in one batch committed at the end or discarded; records the plan makes are named by `ref` (`{ "ref": "…" }` wherever an id goes, resolved value by value). Every create's class is resolved before the batch opens, and a part under a record of the plan goes through that class's own `children` relation (`create_under`: the store cannot class a staged parent). It registers no class: an unregistered hard-wired one is refused (`UnregisteredClass`), and sets no property stored through a language of its own (`resolveLanguage` other than `literal`; `LanguageProperty`): neither could be taken back with the batch. A staged parent is of the class it is created as only, not every class it matches. Creates and adds only. Used by a bundle tool that answers with `writes` (`mcp/tools/instances/template_tools.rs`) |
| `validate.rs` | `validate_properties` / `coerce_scalar`: types, cardinality, required, `sh:in`; `ValidationError` (typed: `ValidationProblem`, `Cardinality`, `allowedValues`) |
| `error.rs` | `InstanceError` (tagged by `kind`, the RPC error's `data`), `WriteStep`, `CascadeStep` |
| `mod.rs` | `PropView` over a `ModelShape`, class resolution, instance ids, `fetch_instance`, `check_setters`, `property_target`, the remove cascade |

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
  existence (`fetch_instance`, `require_instance`) still reads committed state: a
  create+update/-remove/collection-write of a new instance in one batch fails `notFound`.

Do not: add MCP- or RPC-specific wording, or read the token: take the `AgentContext`.
Known wart: still calls `crate::mcp::shacl` for class names, predicates and link status.
