//! Flow-transition propose tool for the interpretation-pass harness.
//!
//! Companion to [`super::propose`]: where `propose` gives the LLM
//! `<class>_propose_create` / `<class>_propose_link_child` tools that
//! accumulate SHACL-instance writes, this module gives the LLM
//! `<flow>_propose_transition` tools that accumulate
//! [`crate::perspectives::flow_evaluator::LlmProposalHint`]s.
//!
//! Symmetry with `propose`:
//!  * per-flow surface (one tool per active [`FlowContext`]) instead of
//!    per-class
//!  * shared [`FlowProposalBuffer`] that the engine drains after the
//!    harness loop terminates
//!  * tool call is a proposal, not a write — the deterministic
//!    [`crate::perspectives::flow_evaluator::run_engine_proposal_pass`] gate
//!    still owns whether it lands (matched against a
//!    [`crate::perspectives::flow_evaluator::SatisfiedTransition`] by
//!    `(instance_uri, to_state)`)
//!
//! ## Why decorator (10.7b), not new dynamic MCP tools
//!
//! Same reason as `propose`: the propose-transition tool only makes sense
//! inside a harness pass, it carries a per-pass buffer and its side effect
//! is "queue a hint," not "mutate the graph." Putting it in
//! `mcp/tools/dynamic.rs` would leak it onto the external MCP transport
//! where it'd have no buffer to write to and no engine draining it.

use super::provider::ToolSchema;
use crate::perspectives::flow_context::FlowContext;
use crate::perspectives::flow_evaluator::LlmProposalHint;
use serde_json::{json, Value};
use std::sync::{Arc, Mutex};

// ── buffer ────────────────────────────────────────────────────────────────

/// Per-pass accumulator for [`LlmProposalHint`]s emitted by
/// `_propose_transition` tool calls. Cloneable `Arc` so the ToolProvider
/// decorator (which the harness owns for the duration of the loop) and the
/// engine (which drains at pass end and threads the hints into
/// [`crate::perspectives::flow_evaluator::run_engine_proposal_pass`]) hold
/// independent references.
///
/// The mutex is only held during a single push/drain — tool calls are
/// serialised through the harness loop anyway, so contention is nil.
#[derive(Debug, Clone, Default)]
pub struct FlowProposalBuffer {
    inner: Arc<Mutex<Vec<LlmProposalHint>>>,
}

impl FlowProposalBuffer {
    pub fn new() -> Self {
        Self::default()
    }

    /// Recover-on-poison lock: if the mutex was poisoned by an earlier
    /// panic during dispatch, take the guard anyway. `Vec<LlmProposalHint>`
    /// is plain data — a panic mid-`push` can't have left it in a torn
    /// state, only in whatever state it was in when the panic fired.
    /// Same policy as [`super::propose::ProposalBuffer`] (Lal's PR #911
    /// review notes).
    fn lock(&self) -> std::sync::MutexGuard<'_, Vec<LlmProposalHint>> {
        self.inner.lock().unwrap_or_else(|poisoned| {
            log::warn!(
                "harness: FlowProposalBuffer mutex was poisoned by an earlier panic; \
                 continuing with the recovered inner data ({} hint(s) so far)",
                poisoned.get_ref().len()
            );
            poisoned.into_inner()
        })
    }

    pub fn push(&self, hint: LlmProposalHint) {
        self.lock().push(hint);
    }

    pub fn drain(&self) -> Vec<LlmProposalHint> {
        std::mem::take(&mut *self.lock())
    }

    pub fn len(&self) -> usize {
        self.lock().len()
    }
}

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
/// The schema mirrors [`LlmProposalHint`] shape:
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
    }
}

/// Build one [`ToolSchema`] per active [`FlowContext`]. Order preserved.
///
/// The decorator (slice 10.7b) hands this straight through to the inner
/// [`super::provider::ToolProvider::tools`] output. Duplicate flow-name
/// FlowContexts (multiple live instances of the same flow) each get their
/// own tool — the tool name would collide, so the caller is expected to
/// de-duplicate upstream. We do not filter here so the layering stays
/// pure.
pub fn propose_transition_tool_schemas(contexts: &[FlowContext]) -> Vec<ToolSchema> {
    contexts
        .iter()
        .map(propose_transition_tool_schema)
        .collect()
}

// ── args parsing ──────────────────────────────────────────────────────────

/// Errors [`parse_propose_transition_args`] surfaces to the harness. The
/// decorator (slice 10.7b) turns these into `Err(anyhow!(...))` payloads
/// the harness slots into the `role: "tool"` response so the LLM can
/// recover.
#[derive(Debug, PartialEq)]
pub enum ArgError {
    NotAnObject,
    MissingInstance,
    InstanceNotString,
    MissingToState,
    ToStateNotString,
    ReasonNotString,
}

impl std::fmt::Display for ArgError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotAnObject => f.write_str("arguments must be a JSON object"),
            Self::MissingInstance => f.write_str("missing required `instance` field"),
            Self::InstanceNotString => f.write_str("`instance` must be a string"),
            Self::MissingToState => f.write_str("missing required `toState` field"),
            Self::ToStateNotString => f.write_str("`toState` must be a string"),
            Self::ReasonNotString => f.write_str("`reason` must be a string when present"),
        }
    }
}

impl std::error::Error for ArgError {}

/// Parse the JSON args the LLM sent for a `_propose_transition` call into
/// an [`LlmProposalHint`]. Pure — the decorator (slice 10.7b) is
/// responsible for pushing the result into a [`FlowProposalBuffer`].
///
/// `instance` and `toState` are required strings; `reason` is optional
/// and, when present, must be a string. `additionalProperties: false` on
/// the schema means an obedient LLM won't send extras, but we don't
/// reject them here — the JSON-Schema enforcement is a hint to the LLM,
/// not a validation gate. Grammar-decoding models honour the schema
/// exactly, chat models may add noise; either way, we take what we can
/// use.
pub fn parse_propose_transition_args(args: &Value) -> Result<LlmProposalHint, ArgError> {
    let obj = args.as_object().ok_or(ArgError::NotAnObject)?;

    let instance_val = obj.get("instance").ok_or(ArgError::MissingInstance)?;
    let instance_uri = instance_val
        .as_str()
        .ok_or(ArgError::InstanceNotString)?
        .to_string();

    let to_state_val = obj.get("toState").ok_or(ArgError::MissingToState)?;
    let to_state = to_state_val
        .as_str()
        .ok_or(ArgError::ToStateNotString)?
        .to_string();

    let reason = match obj.get("reason") {
        None => None,
        Some(Value::Null) => None,
        Some(v) => Some(v.as_str().ok_or(ArgError::ReasonNotString)?.to_string()),
    };

    Ok(LlmProposalHint {
        instance_uri,
        to_state,
        reason,
    })
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

    #[test]
    fn schemas_preserve_context_order() {
        let a = ctx_delivery_scoped();
        let mut b = ctx_delivery_scoped();
        b.flow_name = "Deliberation".to_string();
        let schemas = propose_transition_tool_schemas(&[a.clone(), b.clone()]);
        assert_eq!(schemas.len(), 2);
        assert_eq!(schemas[0].name, "Delivery_propose_transition");
        assert_eq!(schemas[1].name, "Deliberation_propose_transition");
    }

    // ── args parsing ─────────────────────────────────────────────────────

    #[test]
    fn parse_args_accepts_minimal_valid_object() {
        let args = json!({
            "instance": "ad4m://flow/instance/abc",
            "toState": "Scoped"
        });
        let hint = parse_propose_transition_args(&args).expect("valid args should parse");
        assert_eq!(
            hint,
            LlmProposalHint {
                instance_uri: "ad4m://flow/instance/abc".to_string(),
                to_state: "Scoped".to_string(),
                reason: None,
            }
        );
    }

    #[test]
    fn parse_args_accepts_reason_when_present() {
        let args = json!({
            "instance": "ad4m://flow/instance/abc",
            "toState": "Scoped",
            "reason": "Owner named in message 3; acceptance criteria in message 5."
        });
        let hint = parse_propose_transition_args(&args).expect("valid args should parse");
        assert_eq!(
            hint.reason.as_deref(),
            Some("Owner named in message 3; acceptance criteria in message 5.")
        );
    }

    #[test]
    fn parse_args_treats_null_reason_as_absent() {
        let args = json!({
            "instance": "ad4m://flow/instance/abc",
            "toState": "Scoped",
            "reason": null
        });
        let hint = parse_propose_transition_args(&args).expect("null reason should parse as None");
        assert_eq!(hint.reason, None);
    }

    #[test]
    fn parse_args_rejects_non_object() {
        assert_eq!(
            parse_propose_transition_args(&json!("nope")).unwrap_err(),
            ArgError::NotAnObject
        );
    }

    #[test]
    fn parse_args_rejects_missing_required_fields() {
        assert_eq!(
            parse_propose_transition_args(&json!({"toState": "Scoped"})).unwrap_err(),
            ArgError::MissingInstance
        );
        assert_eq!(
            parse_propose_transition_args(&json!({"instance": "u"})).unwrap_err(),
            ArgError::MissingToState
        );
    }

    #[test]
    fn parse_args_rejects_wrong_types() {
        assert_eq!(
            parse_propose_transition_args(&json!({"instance": 42, "toState": "S"})).unwrap_err(),
            ArgError::InstanceNotString
        );
        assert_eq!(
            parse_propose_transition_args(&json!({"instance": "u", "toState": 42})).unwrap_err(),
            ArgError::ToStateNotString
        );
        assert_eq!(
            parse_propose_transition_args(&json!({"instance": "u", "toState": "S", "reason": 42}))
                .unwrap_err(),
            ArgError::ReasonNotString
        );
    }

    // ── buffer ───────────────────────────────────────────────────────────

    #[test]
    fn buffer_new_is_empty() {
        let buf = FlowProposalBuffer::new();
        assert_eq!(buf.len(), 0);
    }

    #[test]
    fn buffer_push_then_drain_returns_hints_in_order_and_clears() {
        let buf = FlowProposalBuffer::new();
        buf.push(LlmProposalHint {
            instance_uri: "a".into(),
            to_state: "S".into(),
            reason: None,
        });
        buf.push(LlmProposalHint {
            instance_uri: "b".into(),
            to_state: "T".into(),
            reason: Some("r".into()),
        });
        assert_eq!(buf.len(), 2);

        let drained = buf.drain();
        assert_eq!(drained.len(), 2);
        assert_eq!(drained[0].instance_uri, "a");
        assert_eq!(drained[1].instance_uri, "b");
        assert_eq!(drained[1].reason.as_deref(), Some("r"));
        assert_eq!(buf.len(), 0, "drain should empty the buffer");
    }

    #[test]
    fn buffer_arc_clones_share_state() {
        let buf = FlowProposalBuffer::new();
        let buf2 = buf.clone();
        buf.push(LlmProposalHint {
            instance_uri: "x".into(),
            to_state: "Y".into(),
            reason: None,
        });
        assert_eq!(buf2.len(), 1, "cloned handle should observe the push");
        buf2.drain();
        assert_eq!(buf.len(), 0, "cloned drain should clear the original");
    }
}
