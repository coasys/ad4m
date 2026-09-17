//! Client-driven manual flow-transition proposals.
//!
//! A UI button that moves an instance from one state to the next calls
//! `propose_flow_transition`. The executor evaluates the guard on THIS
//! replica, seals the evidence, writes the proposal, and — if the flow's
//! `consensusRule.n` is 1 — fires immediately.
//!
//! The invariants enforced here, each with a matching test:
//! 1. `toState` must be reachable from the instance's **derived** state
//!    (not the `currentState` Local cache).
//! 2. If the target state carries a `requires` guard it must be satisfied on
//!    this replica at mint time; an unmet guard is a hard error, not a skip.
//!    The seal comes from [`recompute_evidence_seal`] — the same code every
//!    voter re-runs in [`super::accept`] — so there is exactly one definition
//!    of "the seal for state S" and the proposer's own vote is sealed by it.
//! 3. A contested instance is refused. Two edges out of the current state
//!    already carry quorum, so the fold cannot settle a third one; minting
//!    into it would hand the caller an outcome indistinguishable from
//!    "queued" (issue #998, and the same rule the engine pass applies).
//! 4. **One live proposal per edge, co-signed — never a silent twin.** The
//!    dedup key `(evidence_hash, instance, to_state)` carries no proposer, so
//!    a second agent pressing the same button matches the first agent's
//!    proposal. That agent must *join* it (through the production accept
//!    path, which re-verifies the seal on this replica before signing), not
//!    skip the mint and cast nothing: at `consensusRule {n: 2}` two humans
//!    pressing the same button would otherwise never reach quorum. Re-pressing
//!    it as the *same* agent stays a no-op — the acting DID is already among
//!    the proposal's votes, and nothing is written.
//! 5. **A lookup failure is an error, never silence.** The engine pass fails
//!    closed on a transient store error because it retries on the next pass.
//!    A user's click has no next pass, so the same silence would report a
//!    lost vote as success.
//!
//! The one residual: the dedup key does not carry `from_state`, so in a flow
//! with two transitions into the same state under identical guards a stranded
//! proposal on the *other* edge can match. It is not ours to co-sign — the
//! fold would never count it from here — so we mint our own and say so in the
//! log. Widening the key is not free: it is shared with the engine pass.

use super::accept::accept_flow_proposal;
use super::atom::TransitionAtom;
use super::pass::{run_flow_consensus_pass, FireOutcome};
use super::FlowInstance;
use crate::agent::AgentContext;
use crate::perspectives::flow_context::FlowInstanceRecord;
use crate::perspectives::flow_context::{
    load_all_flow_instances, load_shacl_flows, reachable_next_states,
};
use crate::perspectives::flow_evaluator::{
    find_live_proposal, recompute_evidence_seal, write_proposal, EvidenceSeal, SatisfiedTransition,
};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::LinkQuery;

/// What one `propose_flow_transition` call did, and where the flow stands
/// after it.
///
/// A bare `Vec<FireOutcome>` could not say: an empty vec meant "queued for
/// other voters", "you already proposed this", and "the instance is stuck"
/// alike. Each field below separates one of those.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ProposeOutcome {
    /// The live proposal this call minted or joined. A client needs it to
    /// render "pending — withdraw?", or to hand to another agent's
    /// `acceptProposal`; it exists at mint time either way.
    pub proposal_uri: String,
    /// `true` when this call wrote the proposal; `false` when it found an
    /// equivalent one already open and joined that.
    pub minted: bool,
    /// `true` when this call recorded a vote for the acting DID — the
    /// proposer's own vote on a mint, an `acceptedBy` on a join. `false`
    /// means the acting DID had already voted: the call changed nothing.
    pub recorded_vote: bool,
    /// Consensus events this call's pass recorded for the first time. Empty
    /// while the edge is still short of quorum — read with `recorded_vote`
    /// to tell "your vote landed, waiting for others" from "nothing to do".
    pub outcomes: Vec<FireOutcome>,
    /// The instance's derived state after the call.
    pub derived_state: String,
    /// `true` when two edges out of `derived_state` both carry quorum. The
    /// flow is irreversibly stalled; a caller must not present this as
    /// "awaiting votes". A contested instance is refused up front, so this
    /// can only be contention that arrived *during* the call.
    pub contested: bool,
}

/// Mint — or co-sign — a manual flow-transition proposal for the acting agent.
///
/// See the module doc for the invariants. Errors when:
/// - the instance or its flow cannot be loaded,
/// - the instance is contested,
/// - `to_state` is not reachable from the derived state,
/// - the target state has a `requires` guard that is not currently satisfied
///   on this replica (an untranslatable guard collapses into the same
///   refusal — it is the disposition a voter would reach too),
/// - a store lookup fails.
pub async fn propose_flow_transition(
    perspective: &mut PerspectiveInstance,
    instance_uri: &str,
    to_state: &str,
    rationale: Option<&str>,
    context: &AgentContext,
) -> anyhow::Result<ProposeOutcome> {
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
    if let Some(contention) = &derived.contested {
        return Err(anyhow::anyhow!(
            "{instance_uri} is contested in `{}` — two edges out of it already carry quorum, \
             so a further proposal cannot resolve it; no proposal written",
            contention.from_state
        ));
    }
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

    // One definition of the seal, shared with the co-sign path. `to_state` is
    // resolved out of `flow.states` in there and out of `reachable_next_states`
    // here, and those cannot disagree: `reachable_next_states` yields only
    // states it found in `flow.states` (`flow_context::render`), so a
    // transition naming a state the flow does not define is unreachable and
    // never gets this far.
    let sealed =
        recompute_evidence_seal(perspective, flow, &record_now, to_state, &acting_did).await?;
    let Some(evidence_hash_val) = sealed.seal.hash() else {
        debug_assert_eq!(sealed.seal, EvidenceSeal::Unmet);
        return Err(anyhow::anyhow!(
            "guard for `{to_state}` is not satisfied on this replica — proposal not written"
        ));
    };
    let evidence = sealed.evidence;
    let evidence_ids: Vec<String> = evidence.iter().map(|e| e.id.clone()).collect();

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

    // `?`, not fail-closed: there is no next pass behind a button.
    let live = find_live_proposal(perspective, &transition).await?;
    let (proposal_uri, minted, recorded_vote, outcomes) = match live {
        Some(uri) => {
            match live_proposal_role(perspective, &uri, instance_uri, &derived.state, &acting_did)
                .await?
            {
                LiveProposalRole::AlreadyVoted => {
                    log::debug!(
                        "propose_flow_transition: {instance_uri} → {to_state} already carries a \
                         vote by {acting_did} on {uri}; writing nothing, running consensus pass"
                    );
                    let outcomes = sweep(perspective, instance_uri, context).await;
                    (uri, false, false, outcomes)
                }
                // The production accept path, so this vote is verified exactly
                // as any other co-sign is: it re-derives the seal on this
                // replica and refuses rather than signing what it cannot
                // reproduce. It runs the consensus pass itself, and that pass
                // is the one that can report a fire — a second sweep here
                // would find every mark already written and return nothing.
                LiveProposalRole::Joinable => {
                    let outcomes = accept_flow_proposal(perspective, &uri, context).await?;
                    (uri, false, true, outcomes)
                }
                LiveProposalRole::OtherEdge(why) => {
                    log::warn!(
                        "propose_flow_transition: {uri} shares the dedup key of \
                         {instance_uri} → {to_state} but {why}; minting our own"
                    );
                    mint(perspective, &transition, &acting_did, rationale, context).await?
                }
            }
        }
        None => mint(perspective, &transition, &acting_did, rationale, context).await?,
    };

    // Re-derive rather than report the pre-call fold: `derived_state` is the
    // answer to "did my click move it?". An error here does NOT unwind the
    // vote — it is written and durable, and retrying this call is a no-op by
    // invariant 4 — it only means the state after it could not be read.
    let after = instance.derive_state(perspective).await.map_err(|e| {
        anyhow::anyhow!(
            "{instance_uri}: the vote is recorded, but the state after it could not be \
             derived ({e:#}); retrying this call is a no-op"
        )
    })?;

    Ok(ProposeOutcome {
        proposal_uri,
        minted,
        recorded_vote,
        outcomes,
        derived_state: after.state,
        contested: after.contested.is_some(),
    })
}

/// Write the proposal and sweep. Split out because two branches mint.
async fn mint(
    perspective: &mut PerspectiveInstance,
    transition: &SatisfiedTransition,
    acting_did: &str,
    rationale: Option<&str>,
    context: &AgentContext,
) -> anyhow::Result<(String, bool, bool, Vec<FireOutcome>)> {
    let uri = write_proposal(perspective, transition, acting_did, rationale, context).await?;
    let outcomes = sweep(perspective, &transition.instance_uri, context).await;
    Ok((uri, true, true, outcomes))
}

async fn sweep(
    perspective: &mut PerspectiveInstance,
    instance_uri: &str,
    context: &AgentContext,
) -> Vec<FireOutcome> {
    let only = [instance_uri.to_string()];
    run_flow_consensus_pass(perspective, None, context, None, Some(&only)).await
}

/// What an already-open proposal carrying our dedup key means for this call.
enum LiveProposalRole {
    /// The acting DID is already among its votes. Nothing to write.
    AlreadyVoted,
    /// Open on this edge and missing our vote — co-sign it.
    Joinable,
    /// Same seal and target state, but not a proposal this call can join.
    /// Carries the reason, for the log.
    OtherEdge(String),
}

/// Classify the live proposal `find_live_proposal` matched.
///
/// Reads it as [`TransitionAtom`] — the same identity-checked view the fold
/// counts — so "already voted" means a vote the fold would count, not a link
/// that merely names our DID.
async fn live_proposal_role(
    perspective: &PerspectiveInstance,
    proposal_uri: &str,
    instance_uri: &str,
    from_state: &str,
    acting_did: &str,
) -> anyhow::Result<LiveProposalRole> {
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(proposal_uri.to_string()),
            ..Default::default()
        })
        .await
        .map_err(|e| anyhow::anyhow!("reading proposal {proposal_uri} failed: {e:#}"))?;
    let atom = match TransitionAtom::from_links(instance_uri, proposal_uri, &links) {
        Ok(atom) => atom,
        Err(reason) => {
            return Ok(LiveProposalRole::OtherEdge(format!(
                "it is not engine-visible ({reason})"
            )))
        }
    };
    if atom.from_state != from_state {
        return Ok(LiveProposalRole::OtherEdge(format!(
            "it leaves `{}`, not `{from_state}`",
            atom.from_state
        )));
    }
    if atom.votes.iter().any(|v| v.did == acting_did) {
        return Ok(LiveProposalRole::AlreadyVoted);
    }
    Ok(LiveProposalRole::Joinable)
}
