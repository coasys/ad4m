# perspectives/instances/ — agent guide

The validated instance writes: the one implementation behind the MCP `instance_*` write tools
(`mcp/tools/instances/`) and the `perspective.instance*` RPC methods
(`api/perspective_instances_ws.rs`). TS `Ad4mModel` is to call the RPC methods (A3b).

| File | Role |
|---|---|
| `write.rs` | `create`, `update`, `remove`, `add_to_collection`, `remove_from_collection`; their params (`Instance*`, camelCase, also the RPC bodies) and results |
| `validate.rs` | `validate_properties` / `coerce_scalar`: types, cardinality, required, `sh:in`; `ValidationError` (typed: `ValidationProblem`, `Cardinality`, `allowedValues`) |
| `error.rs` | `InstanceError` (tagged by `kind`, the RPC error's `data`), `WriteStep`, `CascadeStep` |
| `mod.rs` | `PropView` over a `ModelShape`, class resolution, instance ids, `fetch_instance`, `check_setters`, `property_target`, the remove cascade |

Entry points take `(&mut PerspectiveInstance, &AgentContext, params)`; the agent is the
caller's `acts_as()`.

Invariants:
- Validate before writing: a refused write writes no link.
- No text here. Errors and results are data; MCP (`mcp/tools/instances/validate.rs`,
  `*_error` in each tool file) and the RPC (`instance_error`) word them. A new
  `ValidationProblem` or `WriteStep` needs its text in both.
- Without `batch_id`: create/update commit their own batch (discarded on failure); collection
  writes and the remove cascade write directly. With `batch_id`: checked open
  (`BatchNotFound`) before writing, everything goes into it, nothing is committed, and the
  caller's batch is never discarded here.

Do not: add MCP- or RPC-specific wording, or read the token: take the `AgentContext`.
Known wart: still calls `crate::mcp::shacl` for class names, predicates and link status.
