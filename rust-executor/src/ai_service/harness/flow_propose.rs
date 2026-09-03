//! Flow-transition propose tool for the interpretation-pass harness.
//!
//! Companion to [`super::propose`]: where `propose` gives the LLM
//! `<class>_propose_create` / `<class>_propose_link_child` tools that
//! accumulate SHACL-instance writes, this module gives the LLM
//! `<flow>_propose_transition` tools that accumulate
//! [`crate::perspectives::interpretation::LlmFlowProposal`]s.
//!
//! Symmetry with `propose`:
//!  * per-flow surface (one tool per active [`FlowContext`]) instead of
//!    per-class
//!  * shared [`FlowProposalBuffer`] that the engine drains after the
//!    harness loop terminates
//!  * tool call is a proposal, not a write — the deterministic
//!    [`crate::perspectives::flow_evaluator::run_engine_proposal_pass`] gate
//!    still owns whether it lands (the proposal's `(instance, to_state)`
//!    is matched against a
//!    [`crate::perspectives::flow_evaluator::SatisfiedTransition`]'s
//!    `(instance_uri, to_state)`)
//!
//! ## Why decorator (10.7b), not new dynamic MCP tools
//!
//! Same reason as `propose`: the propose-transition tool only makes sense
//! inside a harness pass, it carries a per-pass buffer and its side effect
//! is "queue a hint," not "mutate the graph." Putting it in
//! `mcp/tools/dynamic.rs` would leak it onto the external MCP transport
//! where it'd have no buffer to write to and no engine draining it.

use super::provider::{HintBuffer, ToolSchema};
use crate::perspectives::flow_context::FlowContext;
use crate::perspectives::interpretation::LlmFlowProposal;
use serde_json::{json, Value};
use std::sync::Arc;

// ── buffer ────────────────────────────────────────────────────────────────

/// Per-pass accumulator for [`LlmFlowProposal`]s emitted by
/// `_propose_transition` tool calls. The engine drains it at pass end and
/// threads the hints into
/// [`crate::perspectives::flow_evaluator::run_engine_proposal_pass`].
pub type FlowProposalBuffer = HintBuffer<LlmFlowProposal>;

// ── tool naming ───────────────────────────────────────────────────────────

/// Tool-name suffix reserved for the flow-propose surface. Kept `pub`
/// so [`super`]'s decorator (slice 10.7b) can dispatch on it symmetrically
/// with `propose::strip_class_suffix`.
pub const PROPOSE_TRANSITION_SUFFIX: &str = "_propose_transition";

/// `{FlowName}_propose_transition` — the LLM sees the flow's declared
/// `name` verbatim (same as SHACL classes: no case-mangling).
pub fn propose_transition_tool_name(flow_name: &str) -> String {
    format!("{flow_name}{PROPOSE_TRANSITION_SUFFIX}")
}

/// Inverse of [`propose_transition_tool_name`]. Returns `Some(flow_name)`
/// when `tool_name` ends with [`PROPOSE_TRANSITION_SUFFIX`] and the prefix
/// is non-empty; `None` otherwise. Slice 10.7b's dispatcher uses this to
/// route tool calls back to a specific [`FlowContext`].
pub fn strip_flow_suffix(tool_name: &str) -> Option<&str> {
    tool_name
        .strip_suffix(PROPOSE_TRANSITION_SUFFIX)
        .filter(|prefix| !prefix.is_empty())
}

// ── schema ────────────────────────────────────────────────────────────────

/// Build the `_propose_transition` [`ToolSchema`] for one active flow.
///
/// The schema mirrors [`LlmFlowProposal`] shape:
///  * `instance` (required, string) — must equal `context.instance_uri`.
///    Locked in via the JSON Schema `const` keyword so the LLM can't
///    address a different instance by mistake; the decorator (slice
///    10.7b) also validates it as a defence-in-depth measure.
///  * `toState` (required, string) — enumerated over
///    `context.reachable_next_states` when non-empty. If the flow has
///    zero reachable next states from the current state, the tool is
///    STILL emitted (with an empty enum + a description telling the LLM
///    to skip); this keeps the tool surface stable across passes so the
///    prompt cache doesn't churn.
///  * `reason` (optional, string) — free text; lands as the on-graph
///    `rationale` field once the deterministic guard matches.
///
/// The description prefixes the flow's `interpretationHint` (if any) so
/// the LLM has the same guidance it would get from the `## Active flows`
/// prompt block, without having to cross-reference.
pub fn propose_transition_tool_schema(context: &FlowContext) -> ToolSchema {
    let mut description = format!(
        "Propose advancing flow '{flow}' (instance {inst}, currently in state '{state}') to a \
         reachable next state. Only propose when the transcript provides evidence for the state's \
         `requires` clause; the deterministic post-processor will discard hints that don't match a \
         satisfied transition. Provide a short natural-language `reason` if useful — it lands as \
         the proposal's on-graph rationale.",
        flow = context.flow_name,
        inst = context.instance_uri,
        state = context.current_state,
    );
    if let Some(hint) = context.flow_interpretation_hint.as_deref() {
        if !hint.trim().is_empty() {
            description.push_str("\n\nFlow-level frame: ");
            description.push_str(hint.trim());
        }
    }

    let next_state_names: Vec<String> = context
        .reachable_next_states
        .iter()
        .map(|s| s.name.clone())
        .collect();

    let mut to_state_schema = json!({
        "type": "string",
        "description":
            "Name of the target state to propose. Must be one of the reachable next states."
    });
    if !next_state_names.is_empty() {
        to_state_schema["enum"] = json!(next_state_names);
    }

    ToolSchema {
        name: propose_transition_tool_name(&context.flow_name),
        description,
        parameters: json!({
            "type": "object",
            "properties": {
                "instance": {
                    "type": "string",
                    "const": context.instance_uri,
                    "description":
                        "Instance URI this proposal is about. Must equal the URI shown in the \
                         `## Active flows` prompt block."
                },
                "toState": to_state_schema,
                "reason": {
                    "type": "string",
                    "description":
                        "Optional short natural-language rationale. Written to the proposal's \
                         `rationale` field on the graph."
                }
            },
            "required": ["instance", "toState"],
            "additionalProperties": false
        }),
        side_effect: crate::ai_service::harness::provider::SideEffect::Write,
    }
}

// ── decorator ─────────────────────────────────────────────────────────────

use super::provider::ToolProvider;
use anyhow::{anyhow, Result};
use async_trait::async_trait;

/// [`ToolProvider`] decorator that adds one `{FlowName}_propose_transition`
/// tool per active [`FlowContext`] on top of an inner (read-tool / SHACL
/// propose-write) provider. Symmetric to [`super::propose::ProposeWritesProvider`]:
///  * `tools()` = inner.tools() + one de-duplicated `_propose_transition`
///    schema per distinct flow name (first FlowContext wins)
///  * `call()` intercepts the `_propose_transition` suffix, validates the
///    args against the pass's active FlowContexts, and pushes an
///    [`LlmFlowProposal`] into the shared [`FlowProposalBuffer`]
///
/// The FlowContext list is fixed at construction — new flow instances
/// minted mid-pass will NOT appear until the next pass. Matches the
/// design v3 §6 stability guarantee for the tool surface.
///
/// Multiple FlowContexts sharing the same `flow_name` (multiple live
/// instances of the same flow visible to the same pass) share ONE tool
/// name — the LLM disambiguates via the required `instance` field. The
/// dispatcher below matches on (flow_name, instance_uri) so hints route
/// to the correct FlowContext.
pub struct FlowTransitionProposeProvider<P: ToolProvider + ?Sized> {
    inner: Arc<P>,
    contexts: Vec<FlowContext>,
    buffer: FlowProposalBuffer,
}

impl<P: ToolProvider + ?Sized> FlowTransitionProposeProvider<P> {
    pub fn new(inner: Arc<P>, contexts: Vec<FlowContext>, buffer: FlowProposalBuffer) -> Self {
        Self {
            inner,
            contexts,
            buffer,
        }
    }
}

#[async_trait]
impl<P> ToolProvider for FlowTransitionProposeProvider<P>
where
    P: ToolProvider + ?Sized + Send + Sync,
{
    async fn tools(&self) -> Vec<ToolSchema> {
        let mut out = self.inner.tools().await;
        // De-duplicate schemas by tool name so multiple instances of the
        // same flow don't produce colliding entries in the tools[] array
        // (OpenAI + kalosm both reject duplicate function names). The
        // FIRST occurrence wins; the JSON Schema `const` on its `instance`
        // field pins one URI, and other instances of the same flow rely
        // on the dispatcher accepting any of their URIs (see `call()`).
        //
        // For obedient models this means the "extra" instances are
        // effectively hidden from the tool surface — they still get
        // proposed for through the deterministic engine pass, just not
        // via LLM attribution. Trade-off explicitly accepted rather than
        // rewriting tool names to disambiguate (which would break the
        // clean `{FlowName}_propose_transition` naming the prompt
        // documents).
        let mut seen = std::collections::HashSet::new();
        for ctx in &self.contexts {
            let name = propose_transition_tool_name(&ctx.flow_name);
            if seen.insert(name) {
                out.push(propose_transition_tool_schema(ctx));
            }
        }
        out
    }

    async fn call(&self, name: &str, args: Value) -> Result<String> {
        if let Some(flow_name) = strip_flow_suffix(name) {
            return self.handle_propose_transition(flow_name, args);
        }
        self.inner.call(name, args).await
    }
}

impl<P: ToolProvider + ?Sized> FlowTransitionProposeProvider<P> {
    fn handle_propose_transition(&self, flow_name: &str, args: Value) -> Result<String> {
        // Collect FlowContexts for this flow name up front — used both to
        // reject unknown flows and to disambiguate by instance URI when
        // multiple live instances of the same flow are in the pass.
        let matching: Vec<&FlowContext> = self
            .contexts
            .iter()
            .filter(|c| c.flow_name == flow_name)
            .collect();
        if matching.is_empty() {
            return Err(anyhow!(
                "{flow_name}_propose_transition: no active flow named `{flow_name}` in this \
                 pass. This tool should not have been advertised — treat this as a signal \
                 to stop calling it and answer with the extraction JSON."
            ));
        }

        // One parser per wire shape: `LlmFlowProposal`'s serde derive is the
        // same parser the strategy path uses for `flow_proposals` entries.
        // Unknown extra fields are ignored — the schema's
        // `additionalProperties: false` is a hint to the LLM, not a gate.
        let hint: LlmFlowProposal = serde_json::from_value(args).map_err(|e| {
            anyhow!(
                "{flow_name}_propose_transition: invalid arguments ({e}). Required: \
                 `instance` (string), `toState` (string); optional: `reason` (string)."
            )
        })?;

        // Defence-in-depth: `instance` matches one of the active FlowContexts.
        // Schema `const` enforced this on obedient LLMs; chat models that
        // ignore JSON Schema constraints can still send a mismatched URI.
        // A hint that names an instance we're not tracking would never
        // match a SatisfiedTransition in `run_engine_proposal_pass`, so we
        // reject early with an actionable error the LLM can recover from.
        let ctx = matching
            .iter()
            .find(|c| c.instance_uri == hint.instance)
            .ok_or_else(|| {
                let valid = matching
                    .iter()
                    .map(|c| c.instance_uri.as_str())
                    .collect::<Vec<_>>()
                    .join(", ");
                anyhow!(
                    "{flow_name}_propose_transition: `instance` `{}` is not an active \
                     `{flow_name}` FlowInstance in this pass. Valid instance URIs: {valid}. \
                     Copy the URI verbatim from the `## Active flows` prompt block.",
                    hint.instance
                )
            })?;

        // Defence-in-depth: `toState` is one of the reachable next states.
        // The schema's enum advertised the same set (empty for a terminal
        // state), but chat models may ignore JSON Schema constraints.
        let valid_targets: Vec<&str> = ctx
            .reachable_next_states
            .iter()
            .map(|s| s.name.as_str())
            .collect();
        if !valid_targets.iter().any(|s| *s == hint.to_state) {
            return Err(anyhow!(
                "{flow_name}_propose_transition: `toState` `{}` is not a reachable next \
                 state from `{}`. Valid targets: {}.",
                hint.to_state,
                ctx.current_state,
                if valid_targets.is_empty() {
                    "none — the instance is in a terminal state; do not propose \
                     transitions for it in this pass"
                        .to_string()
                } else {
                    valid_targets.join(", ")
                }
            ));
        }

        // Buffered hint is a plain data record; the deterministic
        // post-processor (`run_engine_proposal_pass`) decides whether it
        // actually turns into a FlowTransitionProposal on the graph.
        let ack_uri = hint.instance.clone();
        let ack_to_state = hint.to_state.clone();
        self.buffer.push(hint);

        Ok(format!(
            "proposed transition: {ack_uri} → `{ack_to_state}` (buffered; the deterministic \
             post-processor validates against the state's `requires` clause before writing \
             a proposal)"
        ))
    }
}

// ── tests ─────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::flow_context::NextStateSummary;

    fn ctx_delivery_scoped() -> FlowContext {
        FlowContext {
            flow_name: "Delivery".to_string(),
            instance_uri: "ad4m://flow/instance/abc".to_string(),
            subject: "literal:string:task-42".to_string(),
            current_state: "Identified".to_string(),
            flow_interpretation_hint: Some(
                "Team task board — advance items as evidence accrues.".to_string(),
            ),
            reachable_next_states: vec![
                NextStateSummary {
                    name: "Scoped".to_string(),
                    interpretation_hint: Some("Owner + acceptance criteria named.".to_string()),
                    requires_human_readable: String::new(),
                    semantic_check: None,
                    consensus_rule: None,
                },
                NextStateSummary {
                    name: "InProgress".to_string(),
                    interpretation_hint: None,
                    requires_human_readable: String::new(),
                    semantic_check: None,
                    consensus_rule: None,
                },
            ],
            consensus_rule: None,
        }
    }

    fn ctx_terminal_state() -> FlowContext {
        FlowContext {
            flow_name: "Delivery".to_string(),
            instance_uri: "ad4m://flow/instance/done".to_string(),
            subject: "literal:string:task-done".to_string(),
            current_state: "Done".to_string(),
            flow_interpretation_hint: None,
            reachable_next_states: vec![],
            consensus_rule: None,
        }
    }

    // ── name helpers ─────────────────────────────────────────────────────

    #[test]
    fn tool_name_composes_flow_plus_suffix() {
        assert_eq!(
            propose_transition_tool_name("Delivery"),
            "Delivery_propose_transition"
        );
    }

    #[test]
    fn strip_flow_suffix_recovers_flow_name() {
        assert_eq!(
            strip_flow_suffix("Delivery_propose_transition"),
            Some("Delivery")
        );
        assert_eq!(
            strip_flow_suffix("Deliberation_propose_transition"),
            Some("Deliberation")
        );
    }

    #[test]
    fn strip_flow_suffix_rejects_non_matches() {
        assert_eq!(strip_flow_suffix("Delivery_create"), None);
        assert_eq!(strip_flow_suffix("_propose_transition"), None);
        assert_eq!(strip_flow_suffix(""), None);
    }

    // ── schema shape ─────────────────────────────────────────────────────

    #[test]
    fn schema_carries_flow_name_and_instance_const() {
        let schema = propose_transition_tool_schema(&ctx_delivery_scoped());
        assert_eq!(schema.name, "Delivery_propose_transition");
        assert_eq!(
            schema.parameters["properties"]["instance"]["const"],
            json!("ad4m://flow/instance/abc")
        );
    }

    #[test]
    fn schema_to_state_enumerates_reachable_states() {
        let schema = propose_transition_tool_schema(&ctx_delivery_scoped());
        let enum_values = schema.parameters["properties"]["toState"]["enum"]
            .as_array()
            .expect("toState enum should be present when reachable states exist")
            .clone();
        assert_eq!(enum_values, vec![json!("Scoped"), json!("InProgress")]);
    }

    #[test]
    fn schema_omits_enum_when_no_reachable_states() {
        // Terminal state (Done) has no outbound transitions — the tool
        // should still exist (surface stability) but not constrain toState.
        let schema = propose_transition_tool_schema(&ctx_terminal_state());
        assert_eq!(schema.name, "Delivery_propose_transition");
        assert!(
            schema.parameters["properties"]["toState"]
                .get("enum")
                .is_none(),
            "no enum expected for terminal state"
        );
    }

    #[test]
    fn schema_required_lists_instance_and_to_state() {
        let schema = propose_transition_tool_schema(&ctx_delivery_scoped());
        let required = schema.parameters["required"]
            .as_array()
            .expect("required[] should be present")
            .clone();
        assert_eq!(required, vec![json!("instance"), json!("toState")]);
    }

    #[test]
    fn schema_forbids_additional_properties() {
        let schema = propose_transition_tool_schema(&ctx_delivery_scoped());
        assert_eq!(
            schema.parameters["additionalProperties"],
            json!(false),
            "additionalProperties: false keeps grammar-decoding LLMs on rails"
        );
    }

    #[test]
    fn schema_description_includes_interpretation_hint_when_present() {
        let schema = propose_transition_tool_schema(&ctx_delivery_scoped());
        assert!(
            schema.description.contains("Team task board"),
            "expected flow interpretationHint to be inlined in description; got: {}",
            schema.description
        );
    }

    #[test]
    fn schema_description_omits_hint_section_when_hint_is_blank_or_absent() {
        let mut ctx = ctx_delivery_scoped();
        ctx.flow_interpretation_hint = Some("   ".to_string());
        let schema = propose_transition_tool_schema(&ctx);
        assert!(!schema.description.contains("Flow-level frame:"));

        ctx.flow_interpretation_hint = None;
        let schema = propose_transition_tool_schema(&ctx);
        assert!(!schema.description.contains("Flow-level frame:"));
    }

    // ── decorator tests (slice 10.7b) ─────────────────────────────────

    /// Inner provider that surfaces one static read tool and echoes any
    /// call name — lets the decorator tests verify delegation without
    /// pulling in a real perspective / MCP.
    struct EchoInner;

    #[async_trait]
    impl ToolProvider for EchoInner {
        async fn tools(&self) -> Vec<ToolSchema> {
            vec![ToolSchema::zero_arg("noop", "inner read tool")]
        }
        async fn call(&self, name: &str, _args: Value) -> Result<String> {
            Ok(format!("inner:{name}"))
        }
    }

    fn make_provider(
        contexts: Vec<FlowContext>,
    ) -> (
        Arc<FlowTransitionProposeProvider<EchoInner>>,
        FlowProposalBuffer,
    ) {
        let buffer = FlowProposalBuffer::new();
        let p = Arc::new(FlowTransitionProposeProvider::new(
            Arc::new(EchoInner),
            contexts,
            buffer.clone(),
        ));
        (p, buffer)
    }

    #[tokio::test]
    async fn tools_include_inner_plus_one_per_flow_context() {
        let (p, _buf) = make_provider(vec![ctx_delivery_scoped(), ctx_terminal_state()]);
        let names: Vec<String> = p.tools().await.into_iter().map(|t| t.name).collect();
        assert!(names.contains(&"noop".to_string()), "inner tool preserved");
        // Both contexts share flow_name `Delivery`, so ONE tool entry
        // survives de-dup (documented trade-off — the dispatcher still
        // routes both instances).
        assert_eq!(
            names
                .iter()
                .filter(|n| *n == "Delivery_propose_transition")
                .count(),
            1,
            "duplicate flow-name contexts collapse to one tool entry"
        );
    }

    #[tokio::test]
    async fn tools_emit_one_entry_per_distinct_flow_name() {
        let mut deliberation = ctx_delivery_scoped();
        deliberation.flow_name = "Deliberation".into();
        deliberation.instance_uri = "ad4m://flow/instance/xyz".into();
        let (p, _buf) = make_provider(vec![ctx_delivery_scoped(), deliberation]);
        let names: Vec<String> = p.tools().await.into_iter().map(|t| t.name).collect();
        assert!(names.contains(&"Delivery_propose_transition".to_string()));
        assert!(names.contains(&"Deliberation_propose_transition".to_string()));
    }

    #[tokio::test]
    async fn call_unknown_tool_delegates_to_inner() {
        let (p, buf) = make_provider(vec![ctx_delivery_scoped()]);
        let out = p.call("noop", json!({})).await.expect("delegated call");
        assert_eq!(out, "inner:noop");
        assert_eq!(buf.len(), 0, "delegation must not touch the buffer");
    }

    #[tokio::test]
    async fn call_propose_transition_buffers_hint_and_returns_ack() {
        let (p, buf) = make_provider(vec![ctx_delivery_scoped()]);
        let out = p
            .call(
                "Delivery_propose_transition",
                json!({
                    "instance": "ad4m://flow/instance/abc",
                    "toState": "Scoped",
                    "reason": "owner and acceptance criteria named in message m17"
                }),
            )
            .await
            .expect("buffered call");
        assert!(out.contains("proposed transition"));
        assert!(out.contains("Scoped"));

        let drained = buf.drain();
        assert_eq!(drained.len(), 1);
        assert_eq!(drained[0].instance, "ad4m://flow/instance/abc");
        assert_eq!(drained[0].to_state, "Scoped");
        assert_eq!(
            drained[0].reason.as_deref(),
            Some("owner and acceptance criteria named in message m17")
        );
    }

    #[tokio::test]
    async fn call_omitted_reason_buffers_hint_with_none_rationale() {
        let (p, buf) = make_provider(vec![ctx_delivery_scoped()]);
        p.call(
            "Delivery_propose_transition",
            json!({
                "instance": "ad4m://flow/instance/abc",
                "toState": "Scoped"
            }),
        )
        .await
        .expect("buffered call");
        let drained = buf.drain();
        assert_eq!(drained.len(), 1);
        assert_eq!(drained[0].reason, None);
    }

    #[tokio::test]
    async fn call_unknown_flow_name_errors_actionably() {
        let (p, buf) = make_provider(vec![ctx_delivery_scoped()]);
        let err = p
            .call(
                "Ghost_propose_transition",
                json!({"instance": "x", "toState": "Y"}),
            )
            .await
            .expect_err("unknown flow rejects");
        let msg = err.to_string();
        assert!(msg.contains("no active flow named `Ghost`"), "msg: {msg}");
        assert_eq!(buf.len(), 0);
    }

    #[tokio::test]
    async fn call_mismatched_instance_errors_with_valid_uris() {
        let (p, buf) = make_provider(vec![ctx_delivery_scoped()]);
        let err = p
            .call(
                "Delivery_propose_transition",
                json!({
                    "instance": "ad4m://flow/instance/OTHER",
                    "toState": "Scoped"
                }),
            )
            .await
            .expect_err("mismatched instance rejects");
        let msg = err.to_string();
        assert!(msg.contains("ad4m://flow/instance/OTHER"), "msg: {msg}");
        assert!(msg.contains("ad4m://flow/instance/abc"), "msg: {msg}");
        assert_eq!(buf.len(), 0);
    }

    #[tokio::test]
    async fn call_unreachable_to_state_errors_with_valid_targets() {
        let (p, buf) = make_provider(vec![ctx_delivery_scoped()]);
        let err = p
            .call(
                "Delivery_propose_transition",
                json!({
                    "instance": "ad4m://flow/instance/abc",
                    "toState": "Done"
                }),
            )
            .await
            .expect_err("bad target state rejects");
        let msg = err.to_string();
        assert!(msg.contains("`Done`"), "msg: {msg}");
        assert!(msg.contains("Scoped"), "msg: {msg}");
        assert!(msg.contains("InProgress"), "msg: {msg}");
        assert_eq!(buf.len(), 0);
    }

    #[tokio::test]
    async fn call_on_terminal_instance_errors() {
        let (p, buf) = make_provider(vec![ctx_terminal_state()]);
        let err = p
            .call(
                "Delivery_propose_transition",
                json!({
                    "instance": "ad4m://flow/instance/done",
                    "toState": "Anything"
                }),
            )
            .await
            .expect_err("terminal instance rejects");
        let msg = err.to_string();
        assert!(msg.contains("terminal state"), "msg: {msg}");
        assert!(msg.contains("`Done`"), "msg: {msg}");
        assert_eq!(buf.len(), 0);
    }

    #[tokio::test]
    async fn call_missing_required_field_errors() {
        let (p, buf) = make_provider(vec![ctx_delivery_scoped()]);
        let err = p
            .call(
                "Delivery_propose_transition",
                json!({"instance": "ad4m://flow/instance/abc"}),
            )
            .await
            .expect_err("missing toState rejects");
        assert!(err.to_string().contains("missing field `toState`"));
        assert_eq!(buf.len(), 0);
    }

    #[tokio::test]
    async fn call_non_object_args_error() {
        let (p, buf) = make_provider(vec![ctx_delivery_scoped()]);
        let err = p
            .call("Delivery_propose_transition", json!("nope"))
            .await
            .expect_err("scalar args reject");
        assert!(err.to_string().contains("invalid arguments"));
        assert_eq!(buf.len(), 0);
    }

    #[tokio::test]
    async fn call_disambiguates_multi_instance_by_uri() {
        let mut second = ctx_delivery_scoped();
        second.instance_uri = "ad4m://flow/instance/second".into();
        second.current_state = "Scoped".into();
        second.reachable_next_states = vec![NextStateSummary {
            name: "InProgress".into(),
            interpretation_hint: None,
            requires_human_readable: String::new(),
            semantic_check: None,
            consensus_rule: None,
        }];
        let (p, buf) = make_provider(vec![ctx_delivery_scoped(), second]);

        // Route to the SECOND instance — reachable_next_states differs
        // (only `InProgress`) so routing correctness is observable via
        // which target-state error surfaces.
        p.call(
            "Delivery_propose_transition",
            json!({
                "instance": "ad4m://flow/instance/second",
                "toState": "InProgress"
            }),
        )
        .await
        .expect("routed to second instance");
        let drained = buf.drain();
        assert_eq!(drained.len(), 1);
        assert_eq!(drained[0].instance, "ad4m://flow/instance/second");

        // And confirm the FIRST instance still refuses `InProgress` when
        // called with its own URI — it accepts `Scoped` OR `InProgress`
        // per fixture, so pick something clearly out of range.
        let err = p
            .call(
                "Delivery_propose_transition",
                json!({
                    "instance": "ad4m://flow/instance/abc",
                    "toState": "Done"
                }),
            )
            .await
            .expect_err("first instance target validation still fires");
        assert!(err.to_string().contains("`Done`"));
    }
}
