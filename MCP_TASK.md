# MCP PR #665 — Review Feedback Implementation

## Context
This is the `feature/mcp-server` branch of AD4M. Nico (lucksus) reviewed and requested changes.

## Tasks (in priority order)

### 1. Add `get_subject_children` MCP tool (Rust)
**File:** `rust-executor/src/mcp/tools.rs`

Add a new tool that returns all subjects that are children of a given subject (i.e. linked via `has_child` predicate or similar). Look at how `get_subject_collection` works and follow that pattern.

The tool should:
- Take `perspective_id`, `class_name` (parent class), `expression_address` (parent), and optionally `child_class_name`
- Query links where the parent is the source and predicate matches the collection/children pattern
- Return the list of child addresses with their class info

Look at `Ad4mModel.ts` around line 1093 for how children work:
```
// Add source filter if specified (filter to nodes that are children of this source)
```

Also look at how `get_subject_collection` already works — `get_subject_children` is conceptually similar but returns subjects that are linked as children (via `has_child` links) rather than collection items.

### 2. Add `delete_subject` MCP tool (Rust)
**File:** `rust-executor/src/mcp/tools.rs`

Add a tool to delete a subject instance. Look at how PerspectiveProxy.removeSubject works in the JS client. It should remove all links associated with the subject.

### 3. Fix mcp-http.test.ts — Replace raw link operations
**File:** `tests/js/tests/mcp-http.test.ts`

The test file already has high-level subject operations working in section 3 ("Bot Discovery"). But Nico's review comments point to steps 9 and 10 in section 2 which still use raw `add_link` calls.

**Find and fix:**
- "step 9: set message body via add_link" → Replace with `set_subject_property` call
- "step 10: link message as child of channel via add_link" → Replace with `add_to_collection` call

These tests should use the same subject-level tools that section 3 already demonstrates.

### 4. Fix mcp-auth.test.ts — HTTP-only auth tests
**File:** `tests/js/tests/mcp-auth.test.ts`

Nico's comment on line 133: "This is still using the ad4m client. we need auth tests that only exercise the new HTTP MCP interface."

Rewrite the auth flow tests to use raw HTTP calls to the MCP endpoint (similar to how `callMcpTool` works in mcp-http.test.ts) instead of using `adminAd4mClient`. The MCP server has `login_email`, `set_token`, and `auth_status` tools — use those.

### 5. Remove Prolog SDNA from tests
If `mcp-integration.test.ts` still exists, convert its Prolog SDNA to SHACL format. If it's been removed already, check remaining test files for any Prolog SDNA usage and convert to SHACL.

SHACL SDNA is set up using `add_sdna` tool with SHACL JSON format. Look at how `core/src/model/Ad4mModel.ts` generates SHACL via the `@SDNAClass` / `@PropertyField` decorators for reference on the SHACL structure.

## Important Notes
- All CI checks currently pass — don't break them
- The `rmcp` crate is used for the MCP protocol implementation
- Follow existing patterns in tools.rs (parameter structs, error handling, capability checks)
- Run `cargo check` after Rust changes to verify compilation
- The test setup uses `callMcpTool()` helper for HTTP MCP calls — reuse it

## Files to study first
1. `rust-executor/src/mcp/tools.rs` — All existing MCP tools
2. `tests/js/tests/mcp-http.test.ts` — Main integration test  
3. `tests/js/tests/mcp-auth.test.ts` — Auth tests that need HTTP-only rewrite
4. `core/src/model/Ad4mModel.ts` — The JS Ad4mModel API (reference for what tools to expose)

When completely finished, run this command to notify me:
openclaw system event --text "Done: MCP PR #665 review feedback implemented - added get_subject_children, delete_subject tools, fixed tests to use subject-level ops, converted auth tests to HTTP-only" --mode now
