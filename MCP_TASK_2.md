# MCP PR #665 — Phase 3: Nico's Detailed Feedback (Feb 25 17:49)

## Context
Nico wants this IN THIS PR. When new subject classes are added to a perspective's SDNA, the MCP server should dynamically generate typed MCP tools for those classes and notify clients.

## Tasks

### 1. Enable `list_changed` in MCP server capabilities
**File:** `rust-executor/src/mcp/tools.rs`

Change `list_changed: Some(false)` to `list_changed: Some(true)` in `get_info()`.

### 2. Generate typed MCP tools from SHACL subject classes
When `tools/list` is called, in addition to the static generic tools, also:
1. Query the perspective for SHACL subject class definitions (links with SHACL predicates)
2. For each subject class found, generate typed tools like:
   - `create_{class_name}` with parameters derived from SHACL properties
   - `query_{class_name}` to find all instances
   - `get_{class_name}` to read one instance
   - `update_{class_name}` to modify properties
   - `delete_{class_name}` to remove an instance

Look at how `list_subject_classes` already queries SHACL shapes. Use the same approach but generate tool definitions from the properties.

### 3. Emit `notifications/tools/list_changed` on SDNA changes
When new SDNA (subject classes) are added to a perspective:
- Detect the change (the `add_sdna` tool or link diffs with SHACL predicates)
- Emit the MCP notification so clients refetch the tool list

### 4. Test: Create subject class via MCP, verify new tools appear
**File:** `tests/js/tests/mcp-http.test.ts`

Add a test section (e.g. "4. Dynamic Tool Generation"):
1. Call `add_sdna` to register a new subject class (e.g. "Task" with properties: title, description, status)
2. Call `tools/list` and verify new typed tools appear (e.g. `create_task`, `query_tasks`)
3. Use the new typed tools to create and query instances
4. Verify the `notifications/tools/list_changed` notification was sent

### 5. Test the Waker Bridge
Add tests for the AD4M Waker Bridge (watches perspectives via GraphQL WS, wakes OpenClaw).
Use the Flux scenario: subscribe to a channel → new message arrives → waker fires.

## Important
- The `rmcp` crate handles tool registration via the `#[tool]` macro on methods
- Dynamic tools may need a different approach since they can't use compile-time macros
- Look at how rmcp handles tool listing — you may need to override `list_tools` to add dynamic entries
- Keep the static tools as-is, just ADD dynamic ones alongside them

When completely finished, run:
openclaw system event --text "Done: MCP PR #665 Phase 2 - dynamic SHACL tool generation, list_changed notifications, waker tests" --mode now
