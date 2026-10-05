# LLM harness — design v2

*2026-08-21, revision after Nico's 09:26 voice message. Supersedes [[planning/llm-harness-design-2026-08-21.md]] (v1). Written after inspecting Josh's `feat/openai-compat-endpoint` branch (#854), the `rust-executor/src/mcp/` tool surface, and the OpenClaw AD4M plugin layout.*

**What changed from v1**
- Sentinel format (fenced `ad4m-tool` blocks) — **dropped**. We use OpenAI-standard tool-calling: `tools`/`tool_choice` on request, `tool_calls` on response, `role: "tool"` results.
- Tool set — **not a hand-rolled 4-tool minimum**. We expose the AD4M MCP tool registry directly, filtered by "read-only + relevant to interpretation". That's already 15+ tools, with per-class dynamic tools auto-added when SHACL classes are registered — exactly matching how OpenClaw agents already reason about AD4M.
- Harness location — moves from `perspectives/interpretation/harness/` to a **general-purpose** `ai_service/harness/` module, because both interpretation and flow-runner will consume it and both can share the tool registry.

---

## 1. Direct answers to Nico's six questions

### Q1. Config flag name? Should it always be activated?

**Recommendation: always on. No flag on the extraction path.**

- The harness with an empty tool list = today's single-shot behaviour (LLM emits final answer immediately, no tool calls). So "off" is a subset of "on".
- The extraction pass benefits from tools even in the simple case: `{Class}_query` for dedup lookup replaces the pre-computed `existing_instance_context` block cleanly. No user has to think about a flag.
- Where a flag IS useful: **per-flow-step tool allowlist**. A given flow step might legitimately want to say "no tools" (pure summarisation) or "only these two tools". That's a per-step config on the flow definition, not a global switch.

Config flag name if we ship one anyway (belt-and-braces for a bad-tool-loop escape hatch): `AutoProcessorConfig.disableHarness: Option<bool>`. Default `None` = on. Negative-form so the default schema doesn't need a field.

### Q2. Cap tool calls?

**Yes, but as a config setting with a generous default. Ship 20.**

Rationale:
- Cost of runaway loops is real (each call re-prompts the LLM with growing context). A cap is cheap insurance.
- 20 is generous enough for graph traversal (walk 3 hops = ~6-9 tool calls) and small enough to catch pathological "keep looking for the perfect Topic" loops.
- Field name: `AutoProcessorConfig.maxToolCalls: Option<u32>` with default `20`.
- On cap hit: append a system message ("Tool budget exhausted. Answer with what you have."), force one final non-tool call, log a warn.

Not part of v1 — but the cap costs one `if` statement so it goes in for safety.

### Q3. Async external LLM adapter — did Josh do it?

**No. Josh's #854 wraps async at the public API but keeps `rt.block_on(remote_client.chat(...))` inside the model thread.** Verified in `rust-executor/src/ai_service/mod.rs` (branch `feat/openai-compat-endpoint`, method `prompt_messages` handler + the new `PromptStream` variant at lines ~868-1010): both remote-model paths still call `rt.block_on(remote_client.chat(chat_input))` on a dedicated thread. The `chat_gpt_lib_rs` crate version was not bumped and no streaming was added to the remote path — for remote models the "stream" path emits one token chunk (the entire response) and closes.

Implication for us:
- **We don't need to fix async on this PR.** The per-model thread + `block_on` pattern is Josh's contract; we call `AIService::prompt_messages(...).await` from the harness and let his layer handle threading. It works today, it will work for us.
- **When we want to add streaming interception later** (v2 optimisation, not in scope now), we'll need to help Josh land a real async remote client (`async-openai` bump) — because his `PromptStream` for remote currently defeats the point by emitting the full response as one chunk.
- **Recommendation for now:** use `prompt_messages` (non-streaming), one call per harness iteration, take the re-prompt-loop cost. It's fine for background interpretation and acceptable for flows. Streaming becomes a v2 concern.

### Q4. Should the harness tools BE the MCP tools?

**Yes. Exactly the same tools, exposed via a shared Rust `ToolProvider` trait — no code duplication.**

The AD4M MCP tool surface lives in `rust-executor/src/mcp/tools/`:
- `perspectives.rs` (7 tools: `list_perspectives`, `add_perspective`, `add_link`, `query_links`, `add_model`, `remove_perspective`, `infer`)
- `subjects.rs` (10 tools: subject class definition + Prolog inference)
- `neighbourhoods.rs` (3: publish, join, describe)
- `flows.rs` (6: flow definitions + state)
- `profiles.rs` (6: agent profiles)
- `auth.rs` (7: capabilities, request/grant tokens)
- `children.rs` (3: hierarchical link nav)
- `languages.rs` (1: language install)
- `subscriptions.rs` (2)
- `dynamic.rs` (**N × 5-9 per SHACL class**: `{Class}_create`, `{Class}_query`, `{Class}_list`, `{Class}_get`, `{Class}_delete`, plus `{Class}_set_{prop}` per scalar property and `{Class}_get/add/remove_{coll}` per collection)

That's **45 static tools + roughly 7 tools per registered SHACL class**.

**How to share.** Right now the tools are declared with `#[tool(description = "...")]` macros on `impl Ad4mMcpHandler`. Every tool method is already a pure Rust `async fn` returning a `String` (or `CallToolResult` for dynamic ones). The MCP transport layer (`rmcp` crate) wraps them into a JSON-RPC surface. To reuse them in-executor:

1. **Introduce a `ToolProvider` trait** in `rust-executor/src/ai_service/harness/`:
   ```rust
   pub trait ToolProvider: Send + Sync {
       fn tools(&self) -> Vec<ToolSchema>;              // (name, description, input JSON Schema)
       async fn call(&self, name: &str, args: Value) -> Result<String>;
   }
   ```
2. **Implement it once for the AD4M MCP surface.** `Ad4mToolProvider` is a thin wrapper around `Ad4mMcpHandler` that:
   - iterates `#[tool]`-annotated methods (or a manually-listed static array) to build `tools()`
   - dispatches `call(name, args)` back to the handler method by name
   - includes dynamic per-class tools by calling `generate_dynamic_tools()` at query time
3. **The `rmcp` transport becomes one consumer of that provider; the harness is another.** Zero duplication. If Josh adds a tool tomorrow, both surfaces get it.

Effort: ~1 day to add the trait + the wrapper + swap the MCP server to consume via the trait (with a compat shim). The dispatch-by-name is 45 match arms, mechanical.

### Q5. Are 4 tools enough? Just use all MCP tools?

**Use the MCP surface, filtered to read-only + interpretation-relevant. That's ~15 tools static, plus per-class dynamic query tools.** The v1 "4 tools" was under-scoped.

Concrete allowlist for the interpretation pass (extraction context):

**Read-only, always exposed:**
- `list_perspectives`, `list_subject_classes` — introspection
- `query_links` — raw link filter
- `{Class}_query`, `{Class}_list`, `{Class}_get`, `{Class}_get_{coll}` — dynamic per-class (auto-added when SHACL registers)
- `infer` — Prolog query for anything richer than one-hop
- `get_children` (from `children.rs`) — hierarchical nav

**Deliberately excluded (write path — happens post-parse):**
- Any `add_*`, `remove_*`, `_create`, `_delete`, `_set_*`, `_add_*` tool
- `add_model` (SHACL registration is deliberate, not LLM-driven mid-reasoning)
- Neighbourhood publish/join (not the LLM's job)
- Auth tools (out of scope)

**Deliberately excluded (write-adjacent surface too broad for interpretation):**
- Flow tools (`flows.rs`) — flow-runner is a separate consumer that will get flow tools; extraction doesn't touch flows
- Profile / language tools — orthogonal domain

Net: **15-20 read-only tools static + 3-4 per registered SHACL class**. In a typical Flux/WE perspective that's 25-50 tools total.

**Is that too many?** Not for models we care about. GPT-4o handles hundreds; Claude 4/5 handles hundreds; Qwen 2.5 32B and Llama 3.1 70B handle 50+ reliably. Marvin-hosted qwen3.5-27b-opus is fine. For very small local models (gemma3:12b) we can add a per-model tool-limit config that trims to a narrower subset (`{Class}_query` + `query_links` + `infer` = 6-10 tools), but that's a tuning knob, not a v1 blocker.

### Q6. Skill file — borrow from OpenClaw plugin?

**Nothing to borrow. The OpenClaw AD4M plugin doesn't declare tools itself — it proxies to this same AD4M MCP surface via `mcporter`.** Verified: no plugin directory in `/home/data/code/openclaw/packages/` mentions AD4M or `neighbourhood_join`. The plugin binary at `/home/data/.ad4m-plugin/` is a compiled wrapper; the tool schemas it exposes to OpenClaw agents come from AD4M's own MCP `list_tools` at runtime.

**Where the "skill file" content actually comes from:** the `description = "..."` strings on each `#[tool]` macro in `rust-executor/src/mcp/tools/*.rs` — already LLM-friendly natural language. Example from `perspectives.rs:92`:

> "List all AD4M perspectives. A perspective is a subjective graph database — a personal collection of links (RDF-like triples: source → predicate → target) that can be queried, modified, and optionally shared as a 'neighbourhood' for real-time P2P collaboration. Each has a UUID and a human-readable name."

Every tool has this quality of description. The `ToolProvider` trait (Q4) just passes them through to the LLM verbatim as OpenAI tool schemas. **No new skill file authoring.**

Bonus system-prompt paragraph the harness prepends (per extraction pass or per flow step) explains *when* to use tools, not *what* each tool is. That's ~150 words, task-specific, lives in the caller (extraction prompt builder / flow step definition), not in the harness.

---

## 2. Revised architecture

```
rust-executor/src/ai_service/harness/
├── mod.rs              — run_with_tools() entry, iteration cap, telemetry
├── provider.rs         — ToolProvider trait + ToolSchema type
└── openai_shape.rs     — request/response serde types for tool_calls
                          (thin — reuses Josh's api/openai_compat/types.rs where possible)

rust-executor/src/mcp/tools/
├── (existing)
└── provider_impl.rs    — Ad4mToolProvider: impl ToolProvider for Ad4mMcpHandler
                          (~200 lines: dispatch table + description enumeration)

rust-executor/src/mcp/server.rs
└── swap direct calls to go through ToolProvider (compat shim)
```

**Harness loop shape (v1):**

```
harness.run(model_id, initial_messages, provider, max_calls) -> String
├─ messages = initial_messages
├─ messages[0].content += tool-use guidance paragraph
├─ loop up to max_calls:
│   ├─ response = ai_service.prompt_messages_with_tools(
│   │      model_id, messages, provider.tools()
│   │  ).await
│   ├─ if response.tool_calls.is_empty():
│   │   └─ return response.content
│   ├─ for each tool_call in response.tool_calls:
│   │   ├─ result = provider.call(tool_call.name, tool_call.args).await
│   │   └─ messages += [
│   │        {role: "assistant", tool_calls: [call]},
│   │        {role: "tool", tool_call_id: call.id, content: result}
│   │      ]
│   └─ continue
└─ if loop exhausted:
    messages += {role: "system", content: "Tool budget exhausted. Answer now."}
    return ai_service.prompt_messages(model_id, messages).await
```

`ai_service.prompt_messages_with_tools` is a new AIService method — **thin extension of Josh's `prompt_messages`** that adds the `tools:` + `tool_choice:` fields to the outgoing OpenAI request (for remote models), or applies `tool_grammar.rs`-style constrained decoding for local kalosm models. Josh's remote path today doesn't emit tool schemas — we add that.

**Two concrete AIService additions needed on top of Josh's #854:**
1. `prompt_messages_with_tools(model_id, messages, tools) -> Result<PromptResponse>` where `PromptResponse { content: String, tool_calls: Vec<ToolCall>, usage: ... }`.
2. For local kalosm: constrained decoding via ArcParser grammar built from the tool schemas. This is where **Josh's `#875` `tool_grammar.rs` genuinely helps** — we cherry-pick just that one file (~300 lines, no other #875 deps) into our branch. If #875 lands on `dev` first, we get it for free.

For remote (OpenAI-compat) models: pass `tools` + `tool_choice` through to `chat_gpt_lib_rs`. Note — the current pinned `chat_gpt_lib_rs` version may not support the `tools` field on `ChatInput`. If not, this becomes a one-line bump or a small crate switch. Verify at implementation time; may extend sizing by 0.5-1 day.

---

## 3. Tool caching

**Not doing it. Per Nico 09:26.** The LLM occasionally repeating a `{Class}_query` is not worth the LRU + invalidation bookkeeping. If it becomes a measured cost, revisit.

---

## 4. Sizing (revised)

| Piece | Days |
|---|---|
| `ToolProvider` trait + `Ad4mToolProvider` wrapper + MCP compat shim | 1 |
| `prompt_messages_with_tools` in AIService (remote path: tools field on request; verify `chat_gpt_lib_rs` supports it) | 0.5 (± 1 if crate bump needed) |
| Cherry-pick `tool_grammar.rs` from #875 + wire it for local kalosm | 1 |
| Harness loop + iteration cap + telemetry | 1 |
| Wire into `run_interpretation_with_strategy_and_model` (delete pre-computed `existing_instance_context` from prompt scaffold) | 0.5 |
| Read-only allowlist filter + per-flow-step allowlist config | 0.5 |
| Unit tests (mock provider, loop termination, tool_call parse, cap) | 1 |
| Real-LLM e2e on Marvin (qwen3.5-27b-opus + gpt-4o-mini via Josh's own endpoint for symmetry) — 3 scenarios | 1 |

**Total: ~6-7 days.** Up from v1's 5 because the trait extraction + `prompt_messages_with_tools` + kalosm grammar wiring are more work than a sentinel parser — but we get the entire MCP tool surface for free forever, plus a clean seam for flows.

Compared to Task B (3-4 days for a narrow one-hop reference-only solution): **1.75× the cost, orders of magnitude more powerful, and reused by flows.** Same recommendation as v1: skip Task B, do this.

---

## 5. Test plan (revised)

### Unit (LLM-free)
1. `tool_provider_enumerates_all_static_mcp_tools`
2. `tool_provider_includes_dynamic_class_tools_after_shacl_register`
3. `tool_provider_call_dispatches_by_name`
4. `readonly_filter_excludes_add_delete_set`
5. `harness_loop_terminates_when_no_tool_calls`
6. `harness_loop_appends_tool_call_and_tool_result_in_order`
7. `harness_loop_hits_max_calls_and_forces_final`
8. `openai_shape_parses_multi_tool_call_response`

### Real-LLM e2e (on Marvin)
9. `e2e_no_tools_used_when_transcript_is_pure_summary` — control: baseline behaviour unchanged.
10. `e2e_uses_class_query_for_existing_topic_dedup` — pre-seed a `Topic`; LLM calls `Topic_query(query="webhook retry")`, gets id, references it instead of minting.
11. `e2e_uses_query_links_for_graph_reasoning` — pre-seed `Topic --evidence_for--> Decision`; feed transcript where the natural attach point is the `Decision`; assert LLM calls `query_links` before minting the `SemanticRelationship`.

Pass on qwen3.5-27b-opus is the ship bar. Pass on gemma3:12b is aspirational (may need constrained-decode via `tool_grammar.rs`).

---

## 6. Rollout

- Land on `feat/openai-compat-endpoint` branch (Josh's #854) once Lal's payment-tracking fixups clear CI and Josh merges to `dev`.
- Coordinate with Josh: if #875 lands first, we get `tool_grammar.rs` for free — otherwise cherry-pick just that file (independent of the `assistant_runtime/` subsystem).
- Single PR against `dev` once #854 is in. No stacking multiple pending PRs.
- Behind the always-on default (Q1), so no A/B flag flip.

---

## 7. Open questions (much smaller now)

1. **`chat_gpt_lib_rs` — does the pinned version accept `tools:` on `ChatInput`?** Check at implementation time. Impact: 0.5-1 day if crate needs bump/switch.
2. **Cherry-pick or wait for #875?** If Josh's #875 is close to landing, wait (0-3 days). If it looks stale like #854 was, cherry-pick `tool_grammar.rs` immediately (~0.5 day).
3. **Per-model max-tools trim for very small local models** (gemma3:12b, phi-3, etc.) — do we ship this in v1 or add when we see a small model failing on 50 tools? Recommend defer to when we see it.

None block starting once #854 lands.

---

## 8. What still doesn't change from v1

- Writes stay post-parse. LLM cannot mutate the graph mid-reasoning.
- Flow runner (Task 8) is a second consumer of the same harness + `ToolProvider`. Different tool allowlist per flow step.
- The `reference_only` / pre-computed `existing_instance_context` block is deleted. LLM asks for existing instances via `{Class}_query`.
- Same recommendation: skip Task B, do the harness. Every line of Task B code becomes part of `Topic_query` or `query_links`.

---

*Nico: this replaces v1. Six questions answered in §1. If any of the answers don't land — especially Q4 (share MCP tools via a trait vs re-declare a narrow subset) — say so and I'll iterate before touching code. Not writing any code until #854 is on `dev`.*
