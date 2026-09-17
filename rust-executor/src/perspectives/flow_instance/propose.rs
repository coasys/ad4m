//! Client-driven manual flow-transition proposals.
//!
//! A UI button that moves an instance from one state to the next calls
//! `propose_flow_transition`. The executor evaluates the guard on THIS
//! replica, seals the evidence, writes the proposal, and — if the flow's
//! `consensusRule.n` is 1 — fires immediately.
//!
//! The three invariants enforced here, each with a matching integration test:
//! 1. `toState` must be reachable from the instance's **derived** state
//!    (not the `currentState` Local cache).
//! 2. If the target state carries a `requires` guard it must be satisfied on
//!    this replica at mint time; an unmet guard is a hard error, not a skip.
//! 3. Re-proposing the same (instance, to_state, evidence_hash) triple is a
//!    no-op — no duplicate is written; the consensus pass still runs so the
//!    caller gets the latest settled state.

use super::pass::{run_flow_consensus_pass, FireOutcome};
use super::FlowInstance;
use crate::agent::AgentContext;
use crate::perspectives::flow_context::FlowInstanceRecord;
use crate::perspectives::flow_context::{
    load_all_flow_instances, load_shacl_flows, reachable_next_states,
};
use crate::perspectives::flow_evaluator::{
    evaluate_requires, evidence_hash, proposal_already_exists, write_proposal, EvidenceItem,
    EvidenceSeal, RequiresResult, SatisfiedTransition,
};
use crate::perspectives::perspective_instance::PerspectiveInstance;

/// Mint a manual flow-transition proposal from a client's perspective.
///
/// Returns the fired outcomes: non-empty when the flow's `consensusRule.n`
/// is 1 and the proposal fires immediately; empty when it is queued for
/// other voters. Calling with the same arguments a second time is a no-op
/// (the proposal is not duplicated); the consensus pass still runs.
///
/// Errors when:
/// - the instance or its flow cannot be loaded,
/// - `to_state` is not reachable from the derived state,
/// - the target state has a `requires` guard that is not currently satisfied.
pub async fn propose_flow_transition(
    perspective: &mut PerspectiveInstance,
    instance_uri: &str,
    to_state: &str,
    rationale: Option<&str>,
    context: &AgentContext,
) -> anyhow::Result<Vec<FireOutcome>> {
    let flows = load_shacl_flows(perspective).await?;
    let instances = load_all_flow_instances(perspective).await?;
    let record = instances
        .iter()
        .find(|r| r.instance_uri == instance_uri)
        .ok_or_else(|| anyhow::anyhow!("no FlowInstance at {instance_uri}"))?;
    let flow = flows
        .get(&record.flow_uri)
        .ok_or_else(|| anyhow::anyhow!("flow `{}` is not in the catalogue", record.flow_uri))?;

    let instance = FlowInstance::from_record(record, flow);
    let derived = instance.derive_state(perspective).await?;
    let acting_did = crate::agent::did_for_context(context)?;

    let reachable = reachable_next_states(flow, &derived.state);
    let target_state = reachable
        .iter()
        .find(|s| s.name == to_state)
        .ok_or_else(|| {
            anyhow::anyhow!(
                "`{to_state}` is not reachable from `{}` — no proposal written",
                derived.state
            )
        })?;

    let record_now = FlowInstanceRecord {
        current_state: derived.state.clone(),
        ..record.clone()
    };

    let requires = target_state.requires.as_deref().unwrap_or_default();
    let (evidence_ids, evidence, evidence_hash_val): (Vec<String>, Vec<EvidenceItem>, String) =
        if requires.is_empty() {
            // Guard-free state: use the canonical NoGuard seal so voters agree.
            let seal = EvidenceSeal::NoGuard.hash().unwrap();
            (vec![], vec![], seal)
        } else {
            match evaluate_requires(perspective, requires, &record_now, &acting_did).await {
                RequiresResult::Satisfied(class_names, ev) => {
                    let hash = evidence_hash(&class_names, &ev);
                    let ids = ev.iter().map(|e| e.id.clone()).collect();
                    (ids, ev, hash)
                }
                RequiresResult::Unmet => {
                    return Err(anyhow::anyhow!(
                        "guard for `{to_state}` is not satisfied on this replica — proposal not written"
                    ));
                }
                RequiresResult::Untranslatable(e) => {
                    return Err(anyhow::anyhow!(
                        "guard for `{to_state}` is untranslatable: {e:#}"
                    ));
                }
                RequiresResult::QueryFailed(e) => return Err(e),
            }
        };

    let transition = SatisfiedTransition {
        flow_name: flow.name.clone(),
        instance_uri: instance_uri.to_string(),
        from_state: derived.state.clone(),
        to_state: to_state.to_string(),
        evidence_ids,
        evidence,
        evidence_hash: evidence_hash_val,
        semantic_check: target_state.semantic_check.clone(),
    };

    if proposal_already_exists(perspective, &transition).await {
        log::debug!(
            "propose_flow_transition: {instance_uri} → {to_state} already proposed; \
             skipping mint, running consensus pass"
        );
    } else {
        write_proposal(perspective, &transition, &acting_did, rationale, context).await?;
    }

    Ok(run_flow_consensus_pass(
        perspective,
        None,
        context,
        None,
        Some(std::slice::from_ref(&instance_uri.to_string())),
    )
    .await)
}
