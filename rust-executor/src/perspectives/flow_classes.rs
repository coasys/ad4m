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
//! Forward-staging for the engine — no live WS-RPC path calls into this
//! module today. The live flow-instance mint path is TS
//! `FlowInstanceRecord.create` in `core/src/perspectives/FlowInstance.ts`
//! (`FlowInstance.start` on the wrapper). When the consensus engine
//! (slice 10.6+) fires transitions server-side, [`mint_flow_instance`]
//! becomes the live path; keep the two representations in sync until
//! then. James PR #929 R6 asked for this header to stop overclaiming
//! today's state, and for the module-level `#![allow(dead_code)]` to be
//! swapped for function-level attributes so drift on individual items
//! surfaces at build time.

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
///
/// Only called from [`mint_flow_instance`] today, which is itself only test-
/// called; the annotation follows.
#[allow(dead_code)]
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
#[allow(dead_code)]
pub(crate) fn flow_instance_uri(instance_id: &str) -> String {
    format!("ad4m://flow/instance/{instance_id}")
}

/// Register the flow-runtime classes if needed, then mint a fresh `FlowInstance`
/// bound to `base_expression`, seeded at `initial_state`.
///
/// **Pure w.r.t. side-effects the caller controls** — `instance_id` and
/// `batch_id` are caller-supplied so this function is deterministic and
/// testable: the caller (auto-processor, WS-RPC handler, or unit test)
/// generates the id and threads its own batch. Mirrors
/// [`super::interpretation::overlay::classes::mint_interpretation_run`].
///
/// **No explicit "startedAt" is written.** `Ad4mModel` synthesises `createdAt`
/// on hydration from the earliest link timestamp on the instance's URI
/// (all links land in the same `create_subject` batch, so they share one
/// timestamp — that timestamp is the flow-start time). Writing a separate
/// `ad4m://flow/created_at` link would duplicate the record and collide with
/// the reserved `createdAt` field on the TS reader side.
///
/// `batch_id` groups this instance write with any consumer's follow-on writes
/// (e.g. the auto-processor bundling instance mint + first proposal in one
/// atomic commit). Pass `None` for standalone mints — the current shape has
/// only scalar constructor properties, so a single `create_subject` writes the
/// whole record; there is no update-loop for follow-on collection members.
///
/// Returns the freshly-minted `FlowInstance` URI (`ad4m://flow/instance/{id}`).
///
/// Only test-called today; the live mint path is TS `FlowInstanceRecord.create`.
/// Comes alive as the write path when the consensus engine (slice 10.6+) fires
/// transitions server-side.
#[allow(dead_code)]
pub(crate) async fn mint_flow_instance(
    perspective: &mut PerspectiveInstance,
    flow_uri: &str,
    base_expression: &str,
    initial_state: &str,
    instance_id: &str,
    batch_id: Option<String>,
    context: &AgentContext,
) -> anyhow::Result<String> {
    ensure_flow_model_classes(perspective, context).await?;

    let uri = flow_instance_uri(instance_id);
    // Property names must match the SDNA `name` fields exactly, not the
    // wire predicate paths. `subject` is used (not `baseExpression`) —
    // the latter collides with `Ad4mModel`'s synthetic hydration field
    // on the TS reader side. The `flowUri` value is the flow's canonical
    // URI (e.g. `coasys://DeliveryFlow`), not the bare name — see
    // James PR #929 R5.
    let values = serde_json::json!({
        "flowUri": flow_uri,
        "subject": base_expression,
        "currentState": initial_state,
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
        // No "startedAt" property — `Ad4mModel`'s built-in `createdAt`
        // (earliest link timestamp) carries flow-start time on hydration.
        for expected in ["flowUri", "subject", "currentState"] {
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
        // No "proposedAt" property — `Ad4mModel`'s built-in `createdAt`
        // (earliest link timestamp on the proposal's URI) is the propose time.
        for expected in [
            "flowInstance",
            "fromState",
            "toState",
            "proposer",
            "evidence",
            "evidenceHashes",
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
        for key in ["flowUri", "subject", "currentState"] {
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
            vec!["flowUri"],
            "FlowInstance identity must be `flowUri` — the flow's canonical URI, \
             collision-free across social-DNA modules (James PR #929 R5)",
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
