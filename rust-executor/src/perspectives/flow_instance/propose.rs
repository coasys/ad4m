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
//! 5. **Into a terminal state, the caller names the run's outputs** as
//!    `(class, id)` pairs, and the proposal signs `outputs_hash` over their
//!    content next to the seal (#1104). Every named output must be an
//!    instance of its class on this replica, checked by the same rule a
//!    voter runs before co-signing (`atom::check_outputs_commitment`). Naming
//!    outputs for a non-terminal state is refused: a run does not end there.
//!    An open proposal on this edge that a voter could sign but that names
//!    *different* outputs is neither joined nor twinned: joining would sign
//!    outputs the caller did not name, and a twin would leave the final edge
//!    with two commitments, which no receipt can verify. The caller is told
//!    to co-sign or reject the open one — advice that only makes sense for a
//!    proposal a voter COULD sign, so one whose commitment fails the
//!    co-signer's own validation is stepped past and minted around instead
//!    (`live_proposal_role`). This sees only proposals the dedup key matches, so a
//!    twin under a different seal is still possible (see
//!    `receipt::final_edge_commitment`).
//! 6. **A lookup failure is an error, never silence.** The engine pass fails
//!    closed on a transient store error because it retries on the next pass.
//!    A user's click has no next pass, so the same silence would report a
//!    lost vote as success.
//!
//! The one residual, and **it is not on this path**: the dedup key does not
//! carry `from_state`, so in a flow with two transitions into the same state
//! under identical guards a stranded proposal on the *other* edge matches it.
//! Here that is handled — the lookup returns every match, this module
//! classifies all of them, co-signs one on its own edge if there is one, and
//! otherwise mints and says so in the log.
//!
//! The engine pass shares the key and cannot do the same: it asks only whether
//! *some* live proposal matched, so in that flow shape a stranded proposal on
//! `A→C` suppresses it ever proposing `B→C`, and the instance stops advancing
//! with a `debug!` line as the only trace. Adding `from_state` to the key is
//! the fix, and it is safe for invariant 4 — the key would still carry no
//! proposer, and two agents pressing one button share a derived state — but it
//! changes what the engine skips, and this PR holds that path
//! behaviour-preserving. Left for the change that owns the engine pass. See
//! [`proposal_already_exists`](crate::perspectives::flow_evaluator).

use super::accept::{accept_flow_proposal, load_outputs};
use super::atom::{
    check_outputs_commitment, normalised_outputs, outputs_hash, OutputRef, OutputsRefusal,
    TransitionAtom,
};
use super::pass::{catch_up_before_voting, run_flow_consensus_pass, FireOutcome};
use super::receipt::is_terminal_state;
use super::FlowInstance;
use crate::agent::AgentContext;
use crate::perspectives::flow_context::FlowInstanceRecord;
use crate::perspectives::flow_context::{
    load_all_flow_instances, load_shacl_flows, reachable_next_states,
};
use crate::perspectives::flow_evaluator::{
    find_live_proposals, recompute_evidence_seal, write_proposal, EvidenceSeal, SatisfiedTransition,
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
/// - `outputs` is non-empty and `to_state` is not terminal,
/// - `to_state` is terminal and an entry of `outputs` is not an instance of
///   its class on this replica,
/// - an open proposal on this edge names different outputs,
/// - a store lookup fails.
///
/// `outputs` is ignored in order and duplicates: the proposal names the
/// sorted, deduplicated refs and commits to their content as this replica
/// reads it now.
pub async fn propose_flow_transition(
    perspective: &mut PerspectiveInstance,
    instance_uri: &str,
    to_state: &str,
    outputs: &[OutputRef],
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

    // The outputs commitment (#1104): only a run that ends here produces
    // anything, and the proposer's own vote passes the same instance check a
    // co-signer runs.
    let terminal = is_terminal_state(flow, to_state);
    let outputs = if terminal {
        let named = normalised_outputs(outputs);
        let loaded = load_outputs(&*perspective, &named).await?;
        if let Some(missing) = named.iter().find(|r| !loaded.contains_key(*r)) {
            return Err(anyhow::anyhow!(
                "a voter would refuse this proposal: {} — no proposal written",
                OutputsRefusal::OutputNotInstance {
                    output: missing.clone()
                }
            ));
        }
        // Hashed over exactly what a co-signer's `load_outputs` reads, so the
        // commitment is the one `check_outputs_commitment` recomputes. Every
        // ref is loaded by now; `filter_map` rather than indexing keeps a
        // future edit above from turning a refusal into a panic.
        let items: Vec<_> = named
            .iter()
            .filter_map(|r| loaded.get(r).cloned())
            .collect();
        Some(items)
    } else if !outputs.is_empty() {
        return Err(anyhow::anyhow!(
            "`{to_state}` is not terminal, so a run does not end there and has no outputs to \
             name — no proposal written"
        ));
    } else {
        None
    };

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
        outputs,
    };
    let committed = transition.outputs.as_deref().map(outputs_hash);

    // `?`, not fail-closed: there is no next pass behind a button.
    //
    // Every candidate, not the first: the dedup key carries no `from_state`, so
    // a proposal on a guard-identical sibling edge shares it and nothing orders
    // the two. Classifying only the first would mint past a joinable proposal
    // sitting behind a foreign one — and mint again on the next press, which is
    // invariant 4 broken in the one shape it exists to cover.
    let live = find_live_proposals(perspective, &transition).await?;
    let mut already_voted = None;
    let mut joinable = None;
    let mut other_outputs = Vec::new();
    for uri in &live {
        match live_proposal_role(
            perspective,
            uri,
            instance_uri,
            &derived.state,
            &acting_did,
            terminal,
            committed.as_deref(),
        )
        .await?
        {
            // Terminal: our vote is already on this edge, so nothing this call
            // could write would add one. Stop — a later candidate can only be
            // a twin, and co-signing it would split the vote.
            LiveProposalRole::AlreadyVoted => {
                already_voted = Some(uri.clone());
                break;
            }
            // First one wins; a second is a twin we must not also sign.
            //
            // Ignoring the rest is safe ONLY because `fold::settle_edge`
            // pools votes across every atom on one `(from_state, to_state)`
            // edge — `quorum_is_counted_across_twin_proposals_on_one_edge`
            // pins it. Our vote on whichever twin we pick therefore counts
            // toward the same quorum as the votes sitting on the others. If
            // that pooling ever goes away this line quietly becomes a
            // vote-splitter, and nothing near it would say so.
            LiveProposalRole::Joinable => {
                joinable.get_or_insert_with(|| uri.clone());
            }
            // On this edge, committed to other outputs. Neither ours to
            // sign nor safe to twin; decided after the loop.
            LiveProposalRole::DifferentOutputs => other_outputs.push(uri.clone()),
            LiveProposalRole::OtherEdge(why) => log::debug!(
                "propose_flow_transition: {uri} shares the dedup key of \
                 {instance_uri} → {to_state} but {why}; not a candidate for this call"
            ),
        }
    }

    let (proposal_uri, minted, recorded_vote, outcomes) = match (already_voted, joinable) {
        // `already_voted` beats an earlier-found `Joinable` — the `break`
        // above argued in one direction, this arm is the same argument in the
        // other. Our vote is already on this edge, and because the fold pools
        // across twins it already counts; co-signing a twin as well would add
        // a second link the fold ignores and a second atom to keep in sync.
        (Some(uri), _) => {
            log::debug!(
                "propose_flow_transition: {instance_uri} → {to_state} already carries a \
                 vote by {acting_did} on {uri}; writing nothing, running consensus pass"
            );
            let outcomes = sweep(perspective, instance_uri, context).await;
            (uri, false, false, outcomes)
        }
        // The production accept path, so this vote is verified exactly as any
        // other co-sign is: it re-derives the seal on this replica and refuses
        // rather than signing what it cannot reproduce. It runs the consensus
        // pass itself, and that pass is the one that can report a fire — a
        // second sweep here would find every mark already written and return
        // nothing.
        (None, Some(uri)) => {
            let outcomes = accept_flow_proposal(perspective, &uri, context).await?;
            (uri, false, true, outcomes)
        }
        // An open proposal on this edge commits to other outputs. A twin
        // would put two outputs commitments on the final edge, and a receipt
        // for such a run is refused (`OutputsCommitmentConflict`), so the
        // disagreement goes back to the caller instead.
        (None, None) if !other_outputs.is_empty() => {
            return Err(anyhow::anyhow!(
                "{instance_uri} → {to_state}: open proposal(s) {} on this edge name different \
                 outputs; co-sign one or reject it — no proposal written",
                other_outputs.join(", ")
            ));
        }
        // Either nothing shares the key, or everything that does belongs to
        // another edge. Both mean this edge has no open proposal to join.
        (None, None) => {
            if !live.is_empty() {
                // Name them. The per-candidate reason is `debug!`, so at
                // `info` this warn is all an operator gets — and "3 of them"
                // without saying which three leaves them nowhere to look.
                log::warn!(
                    "propose_flow_transition: {} proposal(s) share the dedup key of \
                     {instance_uri} → {to_state} but none is on this edge; minting our \
                     own. Candidates: {}",
                    live.len(),
                    live.join(", ")
                );
            }
            mint(perspective, &transition, &acting_did, rationale, context).await?
        }
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
    let mut outcomes = catch_up_before_voting(perspective, &transition.instance_uri, context).await;
    let uri = write_proposal(perspective, transition, acting_did, rationale, context).await?;
    outcomes.extend(sweep(perspective, &transition.instance_uri, context).await);
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
    /// On this edge, VALID (a voter could sign it), but its outputs
    /// commitment is not the one this call would write. Only a terminal
    /// target is classified so — anywhere else `accept` ignores outputs
    /// entirely. Checked before [`Self::AlreadyVoted`], so a re-press
    /// that names new outputs is not reported as a no-op.
    DifferentOutputs,
    /// Same seal and target state, but not a proposal this call can join —
    /// including a terminal proposal no voter could sign. Carries the
    /// reason, for the log.
    OtherEdge(String),
}

/// Classify one of the live proposals `find_live_proposals` matched.
///
/// Reads it as [`TransitionAtom`] — the same identity-checked view the fold
/// counts — so "already voted" means a vote the fold would count, not a link
/// that merely names our DID.
///
/// Into a terminal target (`terminal`), the atom is first validated the way
/// a co-signer would validate it ([`load_outputs`] +
/// [`check_outputs_commitment`]). One that no voter could sign is
/// [`LiveProposalRole::OtherEdge`], NOT [`LiveProposalRole::DifferentOutputs`]:
/// the latter tells the caller to "co-sign one or reject it", and neither is
/// possible — `accept` refuses an invalid commitment, and `reject` only
/// retracts the caller's own links — so classifying it as a rival commitment
/// would let one peer block the manual path into the terminal state for
/// everyone.
async fn live_proposal_role(
    perspective: &PerspectiveInstance,
    proposal_uri: &str,
    instance_uri: &str,
    from_state: &str,
    acting_did: &str,
    terminal: bool,
    committed: Option<&str>,
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
    // Outputs are a terminal-edge concern only: off the final edge `accept`
    // signs regardless of them (`check_outputs_commitment` is a no-op), and
    // our own `committed` is always `None` there, so comparing would refuse
    // to join a proposal every voter accepts over a hash nobody reads.
    if terminal {
        let loaded = match load_outputs(perspective, &atom.outputs).await {
            Ok(loaded) => loaded,
            // The failure is about the FOREIGN atom's outputs (e.g. a class
            // this replica has no shape for); erroring the whole call would
            // hand that proposer the same everyone-is-blocked shape this
            // classification exists to avoid. Worst case of misreading a
            // transient store error here is a twin commitment on the final
            // edge; the fold pools terminal votes per commitment
            // (#1108/#1118), so the twin only splits the votes between two
            // groups — recoverable, unlike the block.
            Err(e) => {
                return Ok(LiveProposalRole::OtherEdge(format!(
                    "a voter could not load its outputs ({e:#})"
                )))
            }
        };
        if let Err(refusal) = check_outputs_commitment(&atom, true, |r| loaded.get(r).cloned()) {
            return Ok(LiveProposalRole::OtherEdge(format!(
                "a voter would refuse it ({refusal})"
            )));
        }
        if atom.outputs_hash.as_deref() != committed {
            return Ok(LiveProposalRole::DifferentOutputs);
        }
    }
    if atom.votes.iter().any(|v| v.did == acting_did) {
        return Ok(LiveProposalRole::AlreadyVoted);
    }
    Ok(LiveProposalRole::Joinable)
}
