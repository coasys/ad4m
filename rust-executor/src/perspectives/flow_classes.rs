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
use crate::types::{Link, LinkQuery, LinkStatus};
use ad4m_client::literal::Literal;

pub(crate) const FLOW_INSTANCE_CLASS: &str = "FlowInstance";
pub(crate) const FLOW_INSTANCE_TARGET_CLASS: &str = "ad4m://FlowInstance";
pub(crate) const FLOW_INSTANCE_SDNA: &str = include_str!("hardwired_sdna/flow_instance.json");
/// `FlowInstance → flowUri` — the row's identity: which `SHACLFlow` it runs.
pub(crate) const FLOW_URI_PREDICATE: &str = "ad4m://flow/flow_uri";
/// `FlowInstance → subject` — the base expression the flow runs on.
pub(crate) const FLOW_BASE_PREDICATE: &str = "ad4m://flow/base";
/// `FlowInstance → currentState` — the engine's per-replica cache of the
/// derived state. Written [`LinkStatus::Local`] only (see
/// [`write_local_current_state`]); the fold never reads it.
pub(crate) const FLOW_CURRENT_STATE_PREDICATE: &str = "ad4m://flow/current_state";

pub(crate) const FLOW_TRANSITION_PROPOSAL_CLASS: &str = "FlowTransitionProposal";
pub(crate) const FLOW_TRANSITION_PROPOSAL_TARGET_CLASS: &str = "ad4m://FlowTransitionProposal";
pub(crate) const FLOW_TRANSITION_PROPOSAL_SDNA: &str =
    include_str!("hardwired_sdna/flow_transition_proposal.json");

/// Idempotently register both hard-wired flow-runtime subject classes into the
/// perspective. Mirrors [`super::interpretation::overlay::classes::ensure_interpretation_overlay_classes`].
/// No `required_path` guard yet — the shapes are stable at this point; add one
/// when a future property forces a re-register.
///
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

/// URI scheme for a freshly-minted `FlowTransitionProposal` node:
/// `ad4m://flow/proposal/{id}`. Sibling of [`flow_instance_uri`] — the two
/// hard-wired flow-runtime records live in parallel URI spaces so a caller
/// can tell instance-URIs and proposal-URIs apart without reading the
/// shape graph.
pub(crate) fn flow_transition_proposal_uri(proposal_id: &str) -> String {
    format!("ad4m://flow/proposal/{proposal_id}")
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
/// atomic commit). Pass `None` for standalone mints — a single
/// `create_subject` writes the whole record.
///
/// Returns the freshly-minted `FlowInstance` URI (`ad4m://flow/instance/{id}`).
///
/// Live-called by [`super::flow_spawn::run_flow_spawn_pass`] — the Rust-side
/// spawn path — alongside TS `FlowInstanceRecord.create` for human-initiated
/// starts.
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
    //
    // `currentState` is deliberately NOT in this value set: it is the
    // engine's per-replica cache and goes in as a `Local` link below, in the
    // same batch, so the shared row never carries a state claim for peers to
    // read (#987). The SDNA setter is `local: true` as well, for writers that
    // go through `update_subject`; this path writes the link directly so the
    // status does not depend on which shape revision this perspective
    // registered.
    let values = serde_json::json!({
        "flowUri": flow_uri,
        "subject": base_expression,
    });
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some(FLOW_INSTANCE_CLASS.to_string()),
                query: None,
            },
            uri.clone(),
            Some(values),
            batch_id.clone(),
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("mint_flow_instance: create_subject failed: {e:#}"))?;
    write_local_current_state(perspective, &uri, initial_state, batch_id, context)
        .await
        .map_err(|e| anyhow::anyhow!("mint_flow_instance: {e:#}"))?;
    Ok(uri)
}

/// Mint one `FlowTransitionProposal` at `ad4m://flow/proposal/{proposal_id}`.
///
/// `proposal_id` and `batch_id` are caller-supplied, as in
/// [`mint_flow_instance`], so the caller controls id generation and atomic
/// commit. Propose-time comes from `Ad4mModel`'s built-in `createdAt`.
///
/// `evidence` is a collection: it is passed as a JSON array and
/// `create_subject` expands it into one `addLink` per element.
/// `rationale` is written only when `Some` and non-empty. `runUri` is not
/// written; engine-emitted proposals do not track back to a run today.
///
/// Property names must match the SDNA `name` fields exactly. A mismatched
/// key is silently dropped by `create_subject`; the alignment test below
/// locks the mapping.
#[allow(clippy::too_many_arguments)]
pub(crate) async fn write_flow_transition_proposal(
    perspective: &mut PerspectiveInstance,
    proposal_id: &str,
    proposer_did: &str,
    flow_instance_uri: &str,
    from_state: &str,
    to_state: &str,
    evidence_ids: &[String],
    evidence_hash: &str,
    rationale: Option<&str>,
    batch_id: Option<String>,
    context: &AgentContext,
) -> anyhow::Result<String> {
    ensure_flow_model_classes(perspective, context).await?;

    let uri = flow_transition_proposal_uri(proposal_id);

    let mut values = serde_json::json!({
        "flowInstance": flow_instance_uri,
        "fromState": from_state,
        "toState": to_state,
        "proposer": proposer_did,
        "evidenceHashes": evidence_hash,
    });
    if let Some(text) = rationale {
        if !text.is_empty() {
            values["rationale"] = text.to_string().into();
        }
    }

    if !evidence_ids.is_empty() {
        values["evidence"] = serde_json::json!(evidence_ids);
    }

    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some(FLOW_TRANSITION_PROPOSAL_CLASS.to_string()),
                query: None,
            },
            uri.clone(),
            Some(values),
            batch_id,
            context,
        )
        .await
        .map_err(|e| {
            anyhow::anyhow!("write_flow_transition_proposal: create_subject failed: {e:#}")
        })?;

    Ok(uri)
}

/// Write a `FlowInstance`'s `currentState` link — the engine's **cache** of
/// what [`crate::perspectives::flow_instance::fold_read_set`] derived.
///
/// Nothing reads it back as authority; it exists so a reader without a
/// perspective (a UI, a prompt block, a model query filtering on state) can
/// see the fold's answer without walking the atoms. It is written
/// [`LinkStatus::Local`]: every replica materialises only its own
/// derivation, so a peer can neither show this replica an unverified claim
/// nor overwrite its cache with a stale one (#987). A replica that has not
/// derived yet simply has no `currentState` link — readers treat absence as
/// "not yet derived", never as an error.
///
/// An empty `to_state` is rejected up front: an empty cache would be
/// indistinguishable from "not yet derived".
pub(crate) async fn advance_flow_instance_state(
    perspective: &mut PerspectiveInstance,
    flow_instance_uri: &str,
    to_state: &str,
    batch_id: Option<String>,
    context: &AgentContext,
) -> anyhow::Result<()> {
    if to_state.is_empty() {
        return Err(anyhow::anyhow!(
            "advance_flow_instance_state: to_state must not be empty (an empty cache reads as `not yet derived`)"
        ));
    }
    ensure_flow_model_classes(perspective, context).await?;
    write_local_current_state(perspective, flow_instance_uri, to_state, batch_id, context)
        .await
        .map_err(|e| anyhow::anyhow!("advance_flow_instance_state: {e:#}"))
}

/// Replace this replica's own `currentState` link with `state`, as a
/// `Local` link. Same single-target semantics as the SDNA setter, restricted
/// to what is ours: only existing **`Local`** `currentState` links are
/// removed. A `Shared` value some peer wrote (the pre-#987 executor did) is
/// left where it is — this engine deletes nothing shared, and hydration
/// prefers the later write, which is ours.
pub(crate) async fn write_local_current_state(
    perspective: &mut PerspectiveInstance,
    flow_instance_uri: &str,
    state: &str,
    batch_id: Option<String>,
    context: &AgentContext,
) -> anyhow::Result<()> {
    let existing = perspective
        .get_links(&LinkQuery {
            source: Some(flow_instance_uri.to_string()),
            predicate: Some(FLOW_CURRENT_STATE_PREDICATE.to_string()),
            ..Default::default()
        })
        .await
        .map_err(|e| anyhow::anyhow!("reading the currentState cache failed: {e:#}"))?;
    for link in existing
        .into_iter()
        .filter(|l| l.status == Some(LinkStatus::Local))
    {
        perspective
            .remove_link(link.into(), batch_id.clone())
            .await
            .map_err(|e| anyhow::anyhow!("dropping the old currentState cache failed: {e:#}"))?;
    }
    let target = Literal::from_string(state.to_string())
        .to_url()
        .map_err(|e| anyhow::anyhow!("encoding state `{state}` failed: {e:#}"))?;
    perspective
        .add_link(
            Link {
                source: flow_instance_uri.to_string(),
                predicate: Some(FLOW_CURRENT_STATE_PREDICATE.to_string()),
                target,
            },
            LinkStatus::Local,
            batch_id,
            context,
        )
        .await
        .map_err(|e| anyhow::anyhow!("writing the currentState cache failed: {e:#}"))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
    use crate::perspectives::perspective_instance::SdnaType;
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
        // The predicate constants the sync trigger and the direct cache
        // write use must be the paths the shape declares.
        let path_of = |name: &str| {
            props
                .iter()
                .find(|p| p["name"].as_str() == Some(name))
                .and_then(|p| p["path"].as_str())
        };
        assert_eq!(path_of("flowUri"), Some(FLOW_URI_PREDICATE));
        assert_eq!(path_of("subject"), Some(FLOW_BASE_PREDICATE));
        assert_eq!(path_of("currentState"), Some(FLOW_CURRENT_STATE_PREDICATE));
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
        // no-ops the write while the mint returns Ok. This test asserts the
        // scalar properties `mint_flow_instance` passes to `create_subject`
        // are exactly ones the FlowInstance SDNA declares. `currentState` is
        // not among them: it bypasses the setter and goes in as a direct
        // `Local` link (see `write_local_current_state`).
        let v = parse(FLOW_INSTANCE_SDNA);
        let props: Vec<&str> = v["properties"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|p| p["name"].as_str())
            .collect();
        for key in ["flowUri", "subject"] {
            assert!(
                props.contains(&key),
                "mint_flow_instance writes `{key}` but SDNA does not declare it (found {props:?})",
            );
        }
    }

    /// The `currentState` cache is per-replica (#987): the SDNA declares the
    /// property `local`, its setter writes `local`, and the predicate the
    /// direct write path uses is the one the shape declares — so a
    /// `Local` link written by `write_local_current_state` hydrates as the
    /// `currentState` property, and an `update_subject` caller lands on the
    /// same status.
    #[test]
    fn current_state_property_is_local_in_sdna() {
        let v = parse(FLOW_INSTANCE_SDNA);
        let prop = v["properties"]
            .as_array()
            .unwrap()
            .iter()
            .find(|p| p["name"].as_str() == Some("currentState"))
            .expect("currentState property must exist");
        assert_eq!(
            prop["path"].as_str(),
            Some(FLOW_CURRENT_STATE_PREDICATE),
            "the direct write path and the SDNA must agree on the predicate",
        );
        assert_eq!(
            prop["local"].as_bool(),
            Some(true),
            "property must be local"
        );
        // Optional: a row synced from a peer carries no cache until this
        // replica's pass runs, and `model_query` only returns instances that
        // satisfy every `min_count >= 1` property — so a required cache would
        // hide every remote instance.
        assert_eq!(
            prop["min_count"].as_u64(),
            Some(0),
            "currentState must be optional (min_count 0)"
        );
        let setter = prop["setter"].as_array().expect("setter array");
        assert!(
            !setter.is_empty() && setter.iter().all(|a| a["local"].as_bool() == Some(true)),
            "every setter action must be local, got {setter:?}",
        );
    }

    #[test]
    fn flow_transition_proposal_uri_scheme() {
        assert_eq!(
            flow_transition_proposal_uri("p-42"),
            "ad4m://flow/proposal/p-42",
            "URI must be `ad4m://flow/proposal/{{id}}` — sibling of flow_instance_uri",
        );
        let uuid = "8f0e1a44-3d3c-4e0a-9c9c-3f5a1b2c3d4e";
        let uri = flow_transition_proposal_uri(uuid);
        assert!(
            uri.ends_with(uuid),
            "proposal_id must be preserved verbatim in the URI tail",
        );
        // Instance-URIs and proposal-URIs must live in disjoint spaces so a
        // caller can tell them apart without walking the shape graph.
        assert_ne!(
            flow_transition_proposal_uri("x"),
            flow_instance_uri("x"),
            "proposal + instance URIs must be distinguishable at the prefix",
        );
    }

    #[test]
    fn write_flow_transition_proposal_values_align_with_sdna_property_names() {
        // Same 2026-08-20-bug guard as `mint_flow_instance_values_align_with_sdna_property_names`
        // but for the writer's payload: every JSON key the writer sends
        // to `create_subject` MUST be a declared SDNA property name.
        // A silent mismatch would return Ok while never writing.
        let v = parse(FLOW_TRANSITION_PROPOSAL_SDNA);
        let props: Vec<&str> = v["properties"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|p| p["name"].as_str())
            .collect();
        for key in [
            "flowInstance",
            "fromState",
            "toState",
            "proposer",
            "evidence",
            "evidenceHashes",
            // Optional LLM-attribution field. Same alignment guard as
            // the required scalars — a rename in the SDNA that did not
            // land here would silently drop the rationale from the
            // on-graph proposal without erroring.
            "rationale",
        ] {
            assert!(
                props.contains(&key),
                "write_flow_transition_proposal writes `{key}` but SDNA does not declare it \
                 (found {props:?})",
            );
        }
    }

    #[test]
    fn evidence_property_is_a_collection_with_add_link_setter() {
        // The writer passes `evidence` as a JSON array, and
        // `create_subject` only expands an array into per-element
        // `addLink`s when every setter action is `addLink` — on a
        // `setSingleTarget` setter the array would be stored as one
        // `literal:json:` blob instead. Locking the shape here so a
        // well-meaning SDNA edit that switches to `setSingleTarget`
        // (which would type-check) breaks this test instead of silently
        // changing the on-graph representation of evidence at runtime.
        let v = parse(FLOW_TRANSITION_PROPOSAL_SDNA);
        let evidence = v["properties"]
            .as_array()
            .unwrap()
            .iter()
            .find(|p| p["name"].as_str() == Some("evidence"))
            .expect("evidence property must exist");
        assert_eq!(
            evidence["collection"].as_bool(),
            Some(true),
            "evidence must be declared `collection: true`",
        );
        let setter_actions: Vec<&str> = evidence["setter"]
            .as_array()
            .expect("evidence must declare a setter array")
            .iter()
            .filter_map(|s| s["action"].as_str())
            .collect();
        assert_eq!(
            setter_actions,
            vec!["addLink"],
            "evidence collection setter must be `addLink` — `setSingleTarget` would clobber",
        );
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

    /// Regression guard for issue #1007: after `add_sdna` with `SdnaType::Flow`
    /// the two hard-wired runtime classes are registered, so
    /// `model_query("FlowTransitionProposal", "{}")` must return an empty list
    /// rather than the "No SHACL shape stored" RPC 500 the engine was producing
    /// on perspectives that had never had a proposal written to them.
    #[tokio::test(flavor = "multi_thread")]
    async fn add_flow_registers_runtime_classes() {
        let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[]).await;

        // Before add_sdna, both classes are absent — model_query must error.
        let before = perspective
            .model_query("FlowTransitionProposal", "{}")
            .await;
        assert!(
            before.is_err(),
            "FlowTransitionProposal must not be queryable before any flow is added (got Ok)"
        );

        // Register a flow (no SHACL body needed — the sdna_type hook fires regardless).
        perspective
            .add_sdna(
                "TestFlow".to_string(),
                String::new(),
                SdnaType::Flow,
                None,
                &ctx,
            )
            .await
            .expect("add_sdna(Flow) must succeed");

        // After add_sdna the runtime classes are present: findAll returns [] not 500.
        let result_json = perspective
            .model_query("FlowTransitionProposal", "{}")
            .await
            .expect("FlowTransitionProposal.findAll must return Ok after add_flow (#1007)");

        let result: Value =
            serde_json::from_str(&result_json).expect("model_query result must be valid JSON");
        let instances = result["instances"]
            .as_array()
            .expect("model_query result must contain an 'instances' array");
        assert!(
            instances.is_empty(),
            "fresh perspective must return empty FlowTransitionProposal list, got {instances:?}"
        );

        // FlowInstance must also be queryable.
        let fi_json = perspective
            .model_query("FlowInstance", "{}")
            .await
            .expect("FlowInstance.findAll must return Ok after add_flow (#1007)");
        let fi: Value = serde_json::from_str(&fi_json).expect("FlowInstance result must be JSON");
        assert!(
            fi["instances"].as_array().map_or(false, |a| a.is_empty()),
            "fresh perspective must return empty FlowInstance list"
        );
    }
}
