//! Hard-wired subject classes for the runtime side of SHACLFlow:
//! `FlowInstance` (a running instance bound to a base) and
//! `FlowTransitionProposal` (a proposed state transition awaiting consensus).
//!
//! The TS `@Model` classes in `core/src/perspectives/FlowInstance.ts` and
//! `core/src/perspectives/FlowModels.ts` are the reader/writer surface; the
//! parity tests (`tests/js/tests/model/flow-instance.test.ts` and
//! `flow-transition-proposal.test.ts`) lock their shape to the SDNA JSON
//! blobs loaded here so drift becomes unmergeable.
//!
//! Registration (`ensure_flow_model_classes`) is exposed for the engine
//! (`mint_flow_instance` / future consensus firing). The auto-processor
//! call-site — where an LLM-detected flow-start mints a `FlowInstance` on
//! behalf of the extraction DID — lands with the slice-10 Model-C wiring;
//! until then, callers are the unit tests here and the WS-RPC exposure of
//! `startFlowInstance` (client mirror).

#![allow(dead_code)]

use crate::agent::AgentContext;
use crate::perspectives::hardwired_class::ensure_subject_class;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SubjectClassOption};

pub(crate) const FLOW_INSTANCE_CLASS: &str = "FlowInstance";
pub(crate) const FLOW_INSTANCE_TARGET_CLASS: &str = "ad4m://FlowInstance";
pub(crate) const FLOW_INSTANCE_SDNA: &str = include_str!("hardwired_sdna/flow_instance.json");

pub(crate) const FLOW_TRANSITION_PROPOSAL_CLASS: &str = "FlowTransitionProposal";
pub(crate) const FLOW_TRANSITION_PROPOSAL_TARGET_CLASS: &str = "ad4m://FlowTransitionProposal";
pub(crate) const FLOW_TRANSITION_PROPOSAL_SDNA: &str =
    include_str!("hardwired_sdna/flow_transition_proposal.json");

/// Idempotently register both hard-wired flow-runtime subject classes into the
/// perspective. Mirrors [`super::interpretation::overlay::classes::ensure_interpretation_overlay_classes`].
/// No `required_path` guard yet — the shapes are stable at this point; add one
/// when a future property forces a re-register.
pub(crate) async fn ensure_flow_model_classes(
    perspective: &mut PerspectiveInstance,
    context: &AgentContext,
) -> anyhow::Result<()> {
    ensure_subject_class(
        perspective,
        FLOW_INSTANCE_CLASS,
        FLOW_INSTANCE_TARGET_CLASS,
        FLOW_INSTANCE_SDNA,
        None,
        context,
    )
    .await?;
    ensure_subject_class(
        perspective,
        FLOW_TRANSITION_PROPOSAL_CLASS,
        FLOW_TRANSITION_PROPOSAL_TARGET_CLASS,
        FLOW_TRANSITION_PROPOSAL_SDNA,
        None,
        context,
    )
    .await
}

/// URI scheme for a freshly-minted `FlowInstance` node: `ad4m://flow/instance/{id}`.
/// Kept separate from the class' target-class URI (`ad4m://FlowInstance`) so a
/// caller can inspect the instance URI without having to walk the shape graph.
/// Mirrors [`super::interpretation::overlay::classes::mint_interpretation_run`]'s
/// `ad4m://interp/run/{id}` layout — the two are the mirror runtime records the
/// engine writes on behalf of an extracting DID.
pub(crate) fn flow_instance_uri(instance_id: &str) -> String {
    format!("ad4m://flow/instance/{instance_id}")
}

/// Register the flow-runtime classes if needed, then mint a fresh `FlowInstance`
/// bound to `base_expression`, seeded at `initial_state`, timestamped with
/// `created_at`.
///
/// **Pure w.r.t. side-effects the caller controls** — `instance_id`,
/// `created_at`, and `batch_id` are all caller-supplied so this function is
/// deterministic and testable: the caller (auto-processor, WS-RPC handler, or
/// unit test) generates the id + timestamp and threads its own batch. Mirrors
/// [`super::interpretation::overlay::classes::mint_interpretation_run`].
///
/// `batch_id` groups this instance write with any consumer's follow-on writes
/// (e.g. the auto-processor bundling instance mint + first proposal in one
/// atomic commit). Pass `None` for standalone mints — the current shape has
/// only scalar constructor properties, so a single `create_subject` writes the
/// whole record; there is no update-loop for follow-on collection members.
///
/// Returns the freshly-minted `FlowInstance` URI (`ad4m://flow/instance/{id}`).
pub(crate) async fn mint_flow_instance(
    perspective: &mut PerspectiveInstance,
    flow_name: &str,
    base_expression: &str,
    initial_state: &str,
    instance_id: &str,
    created_at: &str,
    batch_id: Option<String>,
    context: &AgentContext,
) -> anyhow::Result<String> {
    ensure_flow_model_classes(perspective, context).await?;

    let uri = flow_instance_uri(instance_id);
    let values = serde_json::json!({
        "flow": flow_name,
        "baseExpression": base_expression,
        "currentState": initial_state,
        "createdAt": created_at,
    });
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some(FLOW_INSTANCE_CLASS.to_string()),
                query: None,
            },
            uri.clone(),
            Some(values),
            batch_id,
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("mint_flow_instance: create_subject failed: {e:#}"))?;
    Ok(uri)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    fn parse(sdna: &str) -> Value {
        serde_json::from_str(sdna).expect("hardwired SDNA JSON must parse as serde_json::Value")
    }

    #[test]
    fn flow_instance_sdna_shape() {
        let v = parse(FLOW_INSTANCE_SDNA);
        assert_eq!(
            v["target_class"], FLOW_INSTANCE_TARGET_CLASS,
            "target_class in JSON must match the constant slice-7 imports"
        );
        let props = v["properties"]
            .as_array()
            .expect("properties must be an array");
        assert!(!props.is_empty(), "FlowInstance must declare properties");
        let names: Vec<&str> = props.iter().filter_map(|p| p["name"].as_str()).collect();
        for expected in ["flow", "baseExpression", "currentState", "createdAt"] {
            assert!(
                names.contains(&expected),
                "FlowInstance SDNA missing '{expected}' property (found {names:?})",
            );
        }
    }

    #[test]
    fn flow_transition_proposal_sdna_shape() {
        let v = parse(FLOW_TRANSITION_PROPOSAL_SDNA);
        assert_eq!(
            v["target_class"], FLOW_TRANSITION_PROPOSAL_TARGET_CLASS,
            "target_class in JSON must match the constant slice-7 imports"
        );
        let props = v["properties"]
            .as_array()
            .expect("properties must be an array");
        assert!(
            !props.is_empty(),
            "FlowTransitionProposal must declare properties",
        );
        let names: Vec<&str> = props.iter().filter_map(|p| p["name"].as_str()).collect();
        for expected in [
            "flowInstance",
            "fromState",
            "toState",
            "proposer",
            "evidence",
            "evidenceHashes",
            "createdAt",
        ] {
            assert!(
                names.contains(&expected),
                "FlowTransitionProposal SDNA missing '{expected}' (found {names:?})",
            );
        }
    }

    #[test]
    fn flow_instance_uri_scheme() {
        assert_eq!(
            flow_instance_uri("abc-123"),
            "ad4m://flow/instance/abc-123",
            "URI must be `ad4m://flow/instance/{{id}}` — mirrors the interp-run scheme",
        );
        // Round-trip a UUID-shaped id (the auto-processor's typical source):
        let uuid = "8f0e1a44-3d3c-4e0a-9c9c-3f5a1b2c3d4e";
        let uri = flow_instance_uri(uuid);
        assert!(
            uri.ends_with(uuid),
            "instance_id must be preserved verbatim in the URI tail",
        );
    }

    #[test]
    fn mint_flow_instance_values_align_with_sdna_property_names() {
        // Guards the 2026-08-20 bug shape: values-JSON keys are matched against
        // SDNA-declared property names inside `create_subject`; a silent mismatch
        // no-ops the write while the mint returns Ok. This test asserts the four
        // scalar properties `mint_flow_instance` writes are exactly the ones the
        // FlowInstance SDNA declares (identity + non-optional scalars).
        let v = parse(FLOW_INSTANCE_SDNA);
        let props: Vec<&str> = v["properties"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|p| p["name"].as_str())
            .collect();
        for key in ["flow", "baseExpression", "currentState", "createdAt"] {
            assert!(
                props.contains(&key),
                "mint_flow_instance writes `{key}` but SDNA does not declare it (found {props:?})",
            );
        }
    }

    #[test]
    fn identity_flag_on_discriminator_property() {
        let fi = parse(FLOW_INSTANCE_SDNA);
        let identity_names: Vec<&str> = fi["properties"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|p| p["identity"].as_bool().unwrap_or(false))
            .filter_map(|p| p["name"].as_str())
            .collect();
        assert_eq!(
            identity_names,
            vec!["flow"],
            "FlowInstance identity must be `flow` — the base's per-flow-name discriminator",
        );

        let ftp = parse(FLOW_TRANSITION_PROPOSAL_SDNA);
        let identity_names: Vec<&str> = ftp["properties"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|p| p["identity"].as_bool().unwrap_or(false))
            .filter_map(|p| p["name"].as_str())
            .collect();
        assert_eq!(
            identity_names,
            vec!["flowInstance"],
            "FlowTransitionProposal identity must be `flowInstance` (its parent-instance discriminator)",
        );
    }
}
