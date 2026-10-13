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
use crate::types::{Link, LinkStatus};

pub(crate) const FLOW_INSTANCE_CLASS: &str = "FlowInstance";
pub(crate) const FLOW_INSTANCE_TARGET_CLASS: &str = "ad4m://FlowInstance";
pub(crate) const FLOW_INSTANCE_SDNA: &str = include_str!("hardwired_sdna/flow_instance.json");
/// `FlowInstance → flowUri` — the row's identity: which `SHACLFlow` it runs.
pub(crate) const FLOW_URI_PREDICATE: &str = "ad4m://flow/flow_uri";
/// `FlowInstance → subject` — the base expression the flow runs on.
pub(crate) const FLOW_BASE_PREDICATE: &str = "ad4m://flow/base";
/// `FlowInstance → currentState`: the engine's per-replica state cache as it
/// was written before #1424, as a `Local` link. Nothing writes or reads it any
/// more — the state lives in
/// [`replica_state`](crate::perspectives::flow_instance::replica_state) — and
/// old links under it are dead data. Kept only so a perspective whose
/// registered `FlowInstance` shape still declares it can be told to
/// re-register ([`ensure_flow_model_classes`]).
pub(crate) const LEGACY_FLOW_CURRENT_STATE_PREDICATE: &str = "ad4m://flow/current_state";

pub(crate) const FLOW_TRANSITION_PROPOSAL_CLASS: &str = "FlowTransitionProposal";
pub(crate) const FLOW_TRANSITION_PROPOSAL_TARGET_CLASS: &str = "ad4m://FlowTransitionProposal";
pub(crate) const FLOW_TRANSITION_PROPOSAL_SDNA: &str =
    include_str!("hardwired_sdna/flow_transition_proposal.json");

/// Idempotently register both hard-wired flow-runtime subject classes into the
/// perspective. Mirrors [`super::interpretation::overlay::classes::ensure_interpretation_overlay_classes`].
///
/// A registered shape is replaced when it is out of date in either direction:
/// it lacks the newest path, or it still declares a path #1424 removed.
/// `acceptedBy` (read through a getter) is the newest `FlowTransitionProposal`
/// path; `nonce` was #1108's and `outputs_hash` #1104's — a shape from before
/// them reads proposals without their votes, and before `nonce` would silently
/// drop the nonce every proposal now writes. The removed paths are the old
/// `Local` bookkeeping (`currentState`, `resolvedAs`): a shape still declaring
/// them would hydrate those dead links as if they were current.
pub(crate) async fn ensure_flow_model_classes(
    perspective: &mut PerspectiveInstance,
    context: &AgentContext,
) -> anyhow::Result<()> {
    use crate::perspectives::flow_instance::atom::{
        ACCEPTED_BY_PREDICATE, LEGACY_RESOLVED_AS_PREDICATE,
    };
    for (class_name, target_class, sdna, required, removed) in [
        (
            FLOW_INSTANCE_CLASS,
            FLOW_INSTANCE_TARGET_CLASS,
            FLOW_INSTANCE_SDNA,
            None,
            LEGACY_FLOW_CURRENT_STATE_PREDICATE,
        ),
        (
            FLOW_TRANSITION_PROPOSAL_CLASS,
            FLOW_TRANSITION_PROPOSAL_TARGET_CLASS,
            FLOW_TRANSITION_PROPOSAL_SDNA,
            Some(ACCEPTED_BY_PREDICATE),
            LEGACY_RESOLVED_AS_PREDICATE,
        ),
    ] {
        if crate::perspectives::hardwired_class::subject_class_registered(perspective, target_class)
            .await?
            && crate::perspectives::hardwired_class::shacl_path_present(
                perspective,
                target_class,
                removed,
            )
            .await?
        {
            perspective
                .add_sdna(
                    class_name.to_string(),
                    String::new(),
                    crate::perspectives::perspective_instance::SdnaType::SubjectClass,
                    Some(sdna.to_string()),
                    context,
                )
                .await
                .map_err(|e| {
                    anyhow::anyhow!("re-registering {class_name} without `{removed}`: {e:#}")
                })?;
            continue;
        }
        ensure_subject_class(
            perspective,
            class_name,
            target_class,
            sdna,
            required,
            context,
        )
        .await?;
    }
    Ok(())
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
/// `create_subject` writes the whole record. With a batch, the minter's
/// bookkeeping row is NOT written here: call [`record_minted_state`] after
/// `commit_batch` succeeds.
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
    // No state in the row: the state is per-replica bookkeeping and lives in
    // `FlowReplicaState` (#1424), seeded below at `initial_state` for the
    // minting agent so its first pass reports rather than catches up.
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
            None,
        )
        .await
        .map_err(|e| anyhow::anyhow!("mint_flow_instance: create_subject failed: {e:#}"))?;
    // A standalone mint is on the graph now, so its row is written now. A
    // batched mint's links exist only once the caller's `commit_batch`
    // succeeds, and `perspective.flowStates` lists every row: written before
    // that, the row would name an instance that never exists if the batch is
    // discarded or the commit fails. The caller records it after the commit.
    if batch_id.is_none() {
        record_minted_state(perspective, &uri, initial_state, context)
            .map_err(|e| anyhow::anyhow!("mint_flow_instance: {e:#}"))?;
    }
    Ok(uri)
}

/// Record `initial_state` as the minting agent's state for `instance_uri`, so
/// its first pass reports rather than catches up (#1424).
///
/// [`mint_flow_instance`] calls it for a standalone mint. A caller that
/// minted inside a batch calls it for each instance once `commit_batch` has
/// succeeded, and never when the batch is discarded or the commit fails.
pub(crate) fn record_minted_state(
    perspective: &PerspectiveInstance,
    instance_uri: &str,
    initial_state: &str,
    context: &AgentContext,
) -> anyhow::Result<()> {
    let agent_did = crate::agent::did_for_context(context)?;
    crate::perspectives::flow_instance::replica_state::FlowReplicaState::set_current_state(
        &perspective.uuid,
        &agent_did,
        instance_uri,
        initial_state,
        &perspective.is_teardown,
    )
    .map(|_| ())
}

/// Mint one `FlowTransitionProposal` at its **content-addressed** URI:
/// `ad4m://flow/proposal/{hash}`, where the hash covers the instance, the
/// edge, the seal, the outputs commitment, the proposer and `nonce`
/// ([`crate::perspectives::flow_instance::atom::proposal_uri`] — the one
/// definition; a co-signer's vote signs the URI, so it must cover the
/// fields, #1108).
///
/// `nonce` and `batch_id` are caller-supplied, as in
/// [`mint_flow_instance`], so the caller controls uniqueness and atomic
/// commit. Propose-time comes from `Ad4mModel`'s built-in `createdAt`.
///
/// `evidence` and `outputs` are collections, written as one link per element
/// **directly**, not through `create_subject`: the class may be registered
/// under a client's shape rather than the hardwired one — the TS SDK's
/// `FlowInstance.start` registers `FlowTransitionProposal` itself, with both
/// as `@HasMany` relations (an `ad4m://adder`, no `ad4m://setter`) — and
/// `create_subject` writes values only through setters. The engine's own
/// commitment must not depend on which shape a client registered. Each
/// target is resolved exactly as the hardwired setter would (#1127).
/// `rationale` is written only when `Some` and non-empty. `runUri` is not
/// written; engine-emitted proposals do not track back to a run today.
///
/// `outputs` is `Some` exactly for a proposal into a terminal state: the
/// instances the proposer names as the run's outputs, with their content.
/// The writer stores each one's
/// [`OutputRef::encode`](crate::perspectives::flow_instance::atom::OutputRef::encode)
/// as an `outputs` link and signs `outputsHash` =
/// [`outputs_hash`](crate::perspectives::flow_instance::atom::outputs_hash)
/// over the content, so the named refs and the hash come from one list on an
/// honest write. `Some(&[])` commits to "no outputs"; `None` writes neither.
///
/// Property names must match the SDNA `name` fields exactly. A mismatched
/// key is silently dropped by `create_subject`; the alignment test below
/// locks the mapping.
#[allow(clippy::too_many_arguments)]
pub(crate) async fn write_flow_transition_proposal(
    perspective: &mut PerspectiveInstance,
    nonce: &str,
    proposer_did: &str,
    flow_instance_uri: &str,
    from_state: &str,
    to_state: &str,
    evidence_ids: &[String],
    evidence_hash: &str,
    outputs: Option<&[crate::perspectives::flow_evaluator::EvidenceItem]>,
    rationale: Option<&str>,
    batch_id: Option<String>,
    context: &AgentContext,
) -> anyhow::Result<String> {
    use crate::perspectives::flow_instance::atom::{normalised_outputs, outputs_hash, OutputRef};

    ensure_flow_model_classes(perspective, context).await?;

    // The commitment is part of the URI preimage, so it is computed before
    // the URI. `Some(&[])` commits to "no outputs"; `None` commits to none.
    let committed = outputs.map(outputs_hash);
    let uri = crate::perspectives::flow_instance::atom::proposal_uri(
        flow_instance_uri,
        from_state,
        to_state,
        evidence_hash,
        committed.as_deref(),
        proposer_did,
        nonce,
    );

    let mut values = serde_json::json!({
        "flowInstance": flow_instance_uri,
        "fromState": from_state,
        "toState": to_state,
        "proposer": proposer_did,
        "evidenceHashes": evidence_hash,
        "nonce": nonce,
    });
    if let Some(text) = rationale {
        if !text.is_empty() {
            values["rationale"] = text.to_string().into();
        }
    }

    // The collections, as `(property, predicate, elements)`, written below.
    let mut collections: Vec<(&str, &str, Vec<String>)> = Vec::new();
    if !evidence_ids.is_empty() {
        collections.push((
            "evidence",
            PROPOSAL_EVIDENCE_PREDICATE,
            evidence_ids.to_vec(),
        ));
    }

    if let Some(items) = outputs {
        values["outputsHash"] = committed
            .clone()
            .expect("committed is Some exactly when outputs is")
            .into();
        let refs: Vec<OutputRef> = items.iter().map(OutputRef::of).collect();
        let encoded: Vec<String> = normalised_outputs(&refs)
            .iter()
            .map(OutputRef::encode)
            .collect();
        if !encoded.is_empty() {
            collections.push((
                "outputs",
                crate::perspectives::flow_instance::atom::OUTPUT_PREDICATE,
                encoded,
            ));
        }
    }

    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some(FLOW_TRANSITION_PROPOSAL_CLASS.to_string()),
                query: None,
            },
            uri.clone(),
            Some(values),
            batch_id.clone(),
            context,
            None,
        )
        .await
        .map_err(|e| {
            anyhow::anyhow!("write_flow_transition_proposal: create_subject failed: {e:#}")
        })?;

    let mut links = Vec::new();
    for (property, predicate, elements) in collections {
        for element in elements {
            let target = perspective
                .resolve_property_value(
                    FLOW_TRANSITION_PROPOSAL_CLASS,
                    property,
                    &serde_json::Value::String(element),
                    context,
                )
                .await
                .map_err(|e| {
                    anyhow::anyhow!("write_flow_transition_proposal: `{property}` value: {e:#}")
                })?;
            links.push(Link {
                source: uri.clone(),
                predicate: Some(predicate.to_string()),
                target,
            });
        }
    }
    if !links.is_empty() {
        perspective
            .add_links(links, LinkStatus::Shared, batch_id, context, None)
            .await
            .map_err(|e| {
                anyhow::anyhow!("write_flow_transition_proposal: collection links: {e:#}")
            })?;
    }

    Ok(uri)
}

/// The predicate a proposal's `evidence` collection is written under — the
/// hardwired SDNA's `evidence` path (locked by the SDNA tests below).
pub(crate) const PROPOSAL_EVIDENCE_PREDICATE: &str = "ad4m://flow/evidence";

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
        for expected in ["flowUri", "subject"] {
            assert!(
                names.contains(&expected),
                "FlowInstance SDNA missing '{expected}' property (found {names:?})",
            );
        }
        // The predicate constants the sync trigger uses must be the paths
        // the shape declares.
        let path_of = |name: &str| {
            props
                .iter()
                .find(|p| p["name"].as_str() == Some(name))
                .and_then(|p| p["path"].as_str())
        };
        assert_eq!(path_of("flowUri"), Some(FLOW_URI_PREDICATE));
        assert_eq!(path_of("subject"), Some(FLOW_BASE_PREDICATE));
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
            "nonce",
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
        // are exactly ones the FlowInstance SDNA declares.
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

    /// The engine's bookkeeping left the graph in #1424: neither hardwired
    /// shape may declare the old `Local` paths, or hydration would surface
    /// dead links as the current state and as fired marks — and
    /// `ensure_flow_model_classes` would re-register on every call.
    #[test]
    fn no_shape_declares_the_removed_bookkeeping_paths() {
        use crate::perspectives::flow_instance::atom::LEGACY_RESOLVED_AS_PREDICATE;
        for (sdna, removed) in [
            (FLOW_INSTANCE_SDNA, LEGACY_FLOW_CURRENT_STATE_PREDICATE),
            (FLOW_TRANSITION_PROPOSAL_SDNA, LEGACY_RESOLVED_AS_PREDICATE),
        ] {
            let v = parse(sdna);
            for prop in v["properties"].as_array().unwrap() {
                assert_ne!(prop["path"].as_str(), Some(removed), "{prop}");
                assert_ne!(prop["local"].as_bool(), Some(true), "{prop}");
            }
            assert!(
                !sdna.contains(removed),
                "{removed} must not appear anywhere in the SDNA, setters included"
            );
        }
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
        // `evidence` and `outputs` are written as direct links (#1127), but
        // their values still resolve through these property names.
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
            "outputs",
            "outputsHash",
            // The URI salt (#1108). Dropping this write silently makes every
            // freshly-minted proposal a non-atom (`MissingField(nonce)`), so
            // the alignment guard matters as much as for the scalars above.
            "nonce",
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
    fn evidence_and_outputs_properties_are_collections_with_add_link_setters() {
        // The engine writes these as one link per element itself (#1127),
        // but any other writer — a client calling `create_subject` with an
        // array — relies on `create_subject` expanding it into per-element
        // `addLink`s, which it does only when every setter action is
        // `addLink`; on a `setSingleTarget` setter the array would be stored
        // as one `literal:json:` blob instead. Locking the shape here so a
        // well-meaning SDNA edit that switches to `setSingleTarget`
        // (which would type-check) breaks this test instead of silently
        // changing the on-graph representation at runtime. For `outputs`
        // that would also break the atom reader, which reads one
        // `ad4m://flow/output` link per named output.
        let v = parse(FLOW_TRANSITION_PROPOSAL_SDNA);
        for name in ["evidence", "outputs"] {
            let property = v["properties"]
                .as_array()
                .unwrap()
                .iter()
                .find(|p| p["name"].as_str() == Some(name))
                .unwrap_or_else(|| panic!("{name} property must exist"));
            assert_eq!(
                property["collection"].as_bool(),
                Some(true),
                "{name} must be declared `collection: true`",
            );
            let setter_actions: Vec<&str> = property["setter"]
                .as_array()
                .unwrap_or_else(|| panic!("{name} must declare a setter array"))
                .iter()
                .filter_map(|s| s["action"].as_str())
                .collect();
            assert_eq!(
                setter_actions,
                vec!["addLink"],
                "{name} collection setter must be `addLink` — `setSingleTarget` would clobber",
            );
        }
    }

    #[test]
    fn outputs_properties_write_the_predicates_the_atom_reads() {
        // The writer goes through the SDNA; the atom reads raw predicates.
        // If the two drift, a proposal into a terminal state is written with
        // outputs no voter can see, and every co-sign refuses it as
        // `Uncommitted`.
        use crate::perspectives::flow_instance::atom::{
            OUTPUTS_HASH_PREDICATE, OUTPUT_PREDICATE, PROPOSAL_NONCE_PREDICATE,
        };
        let v = parse(FLOW_TRANSITION_PROPOSAL_SDNA);
        let path_of = |name: &str| {
            v["properties"]
                .as_array()
                .unwrap()
                .iter()
                .find(|p| p["name"].as_str() == Some(name))
                .and_then(|p| p["path"].as_str())
                .map(str::to_string)
        };
        assert_eq!(path_of("outputs").as_deref(), Some(OUTPUT_PREDICATE));
        // The writer names the evidence predicate itself (#1127).
        assert_eq!(
            path_of("evidence").as_deref(),
            Some(super::PROPOSAL_EVIDENCE_PREDICATE)
        );
        assert_eq!(
            path_of("outputsHash").as_deref(),
            Some(OUTPUTS_HASH_PREDICATE)
        );
        // Same drift guard for the nonce (#1108): the writer stores it via
        // the SDNA, the atom re-reads it raw to recompute the URI.
        assert_eq!(path_of("nonce").as_deref(), Some(PROPOSAL_NONCE_PREDICATE));
    }

    #[test]
    fn votes_are_read_through_the_predicate_the_engine_writes() {
        // `acceptedBy` is the engine's, not a client's: a vote goes through
        // `acceptProposal`. The shape reads it and must never offer to write it.
        use crate::perspectives::flow_instance::atom::ACCEPTED_BY_PREDICATE;
        let v = parse(FLOW_TRANSITION_PROPOSAL_SDNA);
        let property = |name: &str| {
            v["properties"]
                .as_array()
                .unwrap()
                .iter()
                .find(|p| p["name"].as_str() == Some(name))
                .unwrap_or_else(|| panic!("{name} property must exist"))
                .clone()
        };
        for (name, predicate) in [("acceptedBy", ACCEPTED_BY_PREDICATE)] {
            let p = property(name);
            assert_eq!(p["path"].as_str(), Some(predicate), "{name} path");
            assert_eq!(p["writable"].as_bool(), Some(false), "{name} is read-only");
            assert!(p["setter"].is_null(), "{name} declares no setter");
        }
        // A plain read of `acceptedBy` would list votes the fold ignores; the
        // getter is what applies `signed_by`.
        assert!(property("acceptedBy")["getter"].is_string());
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
            .model_query("FlowTransitionProposal", "{}", None)
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
            .model_query("FlowTransitionProposal", "{}", None)
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
            .model_query("FlowInstance", "{}", None)
            .await
            .expect("FlowInstance.findAll must return Ok after add_flow (#1007)");
        let fi: Value = serde_json::from_str(&fi_json).expect("FlowInstance result must be JSON");
        assert!(
            fi["instances"].as_array().map_or(false, |a| a.is_empty()),
            "fresh perspective must return empty FlowInstance list"
        );
    }
}
