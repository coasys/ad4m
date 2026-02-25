# MCP PR #665 — Phase 3: Nico's Detailed Feedback

## Context
Nico reviewed the dynamic tool generation and has detailed feedback. All changes in `rust-executor/src/mcp/tools.rs` and `tests/js/tests/mcp-http.test.ts`.

## Tasks (Priority Order)

### 1. Class-first tool naming
Change tool naming from `create_{class}` to `{class}_create`, `{class}_set_{property}`, `{class}_add_{collection}`, etc.
Examples: `task_create`, `task_set_title`, `post_add_comment`, `channel_query`

### 2. Extract SHACL parsing into own Rust module
Create `rust-executor/src/mcp/shacl.rs` (or similar) with proper Rust types:
- `ShaclClass` struct with name, properties, collections
- `ShaclProperty` struct with path, datatype, min_count, max_count
- Methods: `load_all_classes(perspective)`, `load_class(perspective, name)`
- Move inline SHACL parsing from tools.rs into this module

### 3. Per-property set tools + collection tools
For each SHACL class, generate:
- `{class}_create` — create instance
- `{class}_query` — find all instances
- `{class}_get` — get one instance
- `{class}_delete` — delete instance
- `{class}_set_{property}` — set individual property (for each property with count=1)
- `{class}_get_{collection}` — get collection items (for properties with count>1)
- `{class}_add_{collection}` — add to collection (for properties with count>1)
- `{class}_remove_{collection}` — remove from collection (for properties with count>1)

Count distinguishes property (max_count=1 or unset) from collection (max_count>1).

### 4. Rename `add_sdna` → `add_model` + add related tools
- Rename `add_sdna` tool to `add_model`
- Add `get_models` (alias for list_subject_classes)
- Add `add_flow` tool
- Add `get_flows` tool
- Add `get_flow_state` tool
- Add `get_flow_actions` tool (possible actions in current state)
- Add `start_flow` tool

For flow tools, study:
- `tests/js/tests/` for flow test patterns
- PerspectiveProxy flow-related methods
- Flows = finite state machines defined in SDNA

### 5. Update tests
- Verify existing dynamic tool tests still pass with new naming
- Add tests for per-property set tools
- Add tests for collection tools
- Add tests for flow tools
- Ensure CI output is visible (add console.log for test results)

## Important Notes
- Don't enforce SHACL counts yet (that's for later)
- Count>1 = collection, count=1 or unset = property
- Nested classes: defer for now, but be aware of James's Ad4mModel refactor PR
- Look at `perspective_instance.rs` for flow/state machine handling

When completely finished, run:
openclaw system event --text "Done: MCP Phase 3 - class-first naming, SHACL module, collections, flows" --mode now
