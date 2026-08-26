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
//! Registration (`ensure_flow_model_classes`) is exposed for the slice-7
//! engine (`start_flow` / consensus firing) — no live call-site yet, hence
//! the module-level `dead_code` allow. Removed as soon as slice 7 calls in.

#![allow(dead_code)]

use crate::agent::AgentContext;
use crate::perspectives::hardwired_class::ensure_subject_class;
use crate::perspectives::perspective_instance::PerspectiveInstance;

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
