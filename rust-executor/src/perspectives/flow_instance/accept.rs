//! The member's vote: this replica co-signing someone's proposal.
//!
//! One rule shapes this file: **a replica may only ever refuse its own
//! action.** So this is where a proposal's evidence seal is re-checked —
//! before we sign, on our own graph — rather than at fire time on whichever
//! replica happened to run the sweep. If the cited content changed since the
//! mint, we decline to co-sign and write nothing; we never delete somebody
//! else's proposal because our copy of the graph is behind.
//!
//! The fold consequently re-runs no guards at all. Every atom it counts
//! carries a seal that each of its voters verified before signing, and
//! history is never re-checked against the live graph — otherwise editing a
//! task cited by a finished transition would unwind the flow that consumed
//! it.
//!
//! At `{n: 1}` the proposer's own mint is the only vote, and the seal was
//! computed by that replica at mint time. A dishonest solo proposer could
//! always have written real evidence and proposed honestly, so the rule
//! already grants them the move; the check that matters is the reviewer's,
//! and it is the one that runs here.

use super::atom::{signed_by, TransitionAtom, ACCEPTED_BY_PREDICATE, FLOW_INSTANCE_PREDICATE};
use super::pass::{run_flow_consensus_pass, FireOutcome};
use super::FlowInstance;
use crate::agent::AgentContext;
use crate::perspectives::flow_context::{
    load_all_flow_instances, load_shacl_flows, FlowInstanceRecord,
};
use crate::perspectives::flow_evaluator::recompute_evidence_hash;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{DecoratedLinkExpression, Link, LinkQuery, LinkStatus};

/// Vote for a proposal as the acting DID, then sweep its instance.
///
/// Refuses — writing nothing — when the proposal is not an identity-checked
/// atom, when it leaves a state the flow is not standing in, or when its
/// evidence seal does not recompute on this replica. Returns whatever
/// settled as a result, which may be nothing: a vote that does not yet reach
/// quorum is a landed vote, not a failure.
pub async fn accept_flow_proposal(
    perspective: &mut PerspectiveInstance,
    proposal_uri: &str,
    context: &AgentContext,
) -> anyhow::Result<Vec<FireOutcome>> {
    let links = proposal_links(perspective, proposal_uri).await?;
    let Some(instance_uri) = links.iter().find_map(|l| {
        (l.data.predicate.as_deref() == Some(FLOW_INSTANCE_PREDICATE))
            .then(|| l.data.target.clone())
    }) else {
        return Err(anyhow::anyhow!(
            "proposal {proposal_uri} carries no {FLOW_INSTANCE_PREDICATE} link — not engine-visible, accept not recorded"
        ));
    };
    // The visibility gate IS the atom check, so what a client may vote on and
    // what the fold may count are one rule. A proposal that fails it is left
    // in place: a mid-sync proposer link may still arrive and complete it.
    let atom = TransitionAtom::from_links(&instance_uri, proposal_uri, &links).map_err(|reason| {
        anyhow::anyhow!(
            "proposal {proposal_uri} is not engine-visible ({reason} — a mid-sync proposal may still complete) — accept not recorded"
        )
    })?;

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
    if atom.from_state != derived.state {
        return Err(anyhow::anyhow!(
            "proposal {proposal_uri} is stale: it leaves `{}` but {instance_uri} is in `{}` — accept not recorded",
            atom.from_state,
            derived.state
        ));
    }

    // The one guard re-run in this engine, and it gates only our own
    // signature. `recompute_evidence_hash` substitutes the PROPOSER's DID
    // because `$did`-templated guards resolved against them at mint time.
    let record_now = FlowInstanceRecord {
        current_state: derived.state.clone(),
        ..record.clone()
    };
    let recomputed = recompute_evidence_hash(
        perspective,
        flow,
        &record_now,
        &atom.to_state,
        &atom.proposer,
    )
    .await?;
    if recomputed.as_deref() != Some(atom.evidence_hash.as_str()) {
        return Err(anyhow::anyhow!(
            "proposal {proposal_uri} cites evidence this replica cannot reproduce — refusing to co-sign; the proposal is left untouched"
        ));
    }

    let did = crate::agent::did_for_context(context)
        .map_err(|e| anyhow::anyhow!("accept_flow_proposal: no acting DID: {e:#}"))?;
    // Authorship-bound through `signed_by`, exactly as the fold counts votes:
    // a link that merely CLAIMS this DID as its author counts for nothing
    // there, so it must not suppress the genuine vote here either. Comparing
    // `l.author` alone would let a peer publish an unverifiable `acceptedBy`
    // in our name and lock us out of every `{n: 2}` edge.
    let already = links.iter().any(|l| {
        l.data.predicate.as_deref() == Some(ACCEPTED_BY_PREDICATE)
            && l.data.target == did
            && signed_by(l, &did)
    });
    if already {
        log::debug!("accept_flow_proposal: {proposal_uri} already accepted by {did}");
    } else {
        perspective
            .add_link(
                Link {
                    source: proposal_uri.to_string(),
                    predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
                    target: did.clone(),
                },
                LinkStatus::Shared,
                None,
                context,
            )
            .await
            .map_err(|e| anyhow::anyhow!("accept_flow_proposal: add_link failed: {e:#}"))?;
    }
    Ok(run_flow_consensus_pass(perspective, None, context, None, Some(&instance_uri)).await)
}

/// Reject a proposal: retract the links on it that this replica signed.
///
/// Invariant: a replica only ever refuses its own action. So we delete only
/// the links this DID actually signed — another agent's links are theirs to
/// retract, and a link that merely *claims* our authorship without a valid
/// signature is not our action either, so it is left alone.
///
/// There is deliberately no "already fired, refuse" guard. A `resolved_as →
/// "fired"` mark is an index any member may write, not authority, and this
/// engine reads no mark to decide anything. Retracting a vote that helped
/// settle an edge therefore does move the flow back — that is the semantics
/// stated in this module's parent doc, not a hole in this function: state is
/// a function of the links present now.
pub async fn reject_flow_proposal(
    perspective: &mut PerspectiveInstance,
    proposal_uri: &str,
    context: &AgentContext,
) -> anyhow::Result<()> {
    use crate::types::LinkExpression;

    let links = proposal_links(perspective, proposal_uri).await?;

    let did = crate::agent::did_for_context(context)
        .map_err(|e| anyhow::anyhow!("reject_flow_proposal: no acting DID: {e:#}"))?;

    let to_remove: Vec<LinkExpression> = links
        .into_iter()
        .filter(|l| signed_by(l, &did))
        .map(LinkExpression::from)
        .collect();

    if to_remove.is_empty() {
        return Err(anyhow::anyhow!(
            "proposal {proposal_uri} carries no link signed by {did} — cannot reject another agent's proposal"
        ));
    }

    perspective
        .remove_links(to_remove, None)
        .await
        .map_err(|e| anyhow::anyhow!("reject_flow_proposal: remove_links failed: {e:#}"))?;
    Ok(())
}

/// Every source-link of a proposal. `Err` when the URI carries none —
/// a typo'd or already-deleted URI must not succeed silently.
async fn proposal_links(
    perspective: &PerspectiveInstance,
    proposal_uri: &str,
) -> anyhow::Result<Vec<DecoratedLinkExpression>> {
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(proposal_uri.to_string()),
            ..Default::default()
        })
        .await
        .map_err(|e| anyhow::anyhow!("proposal lookup on {proposal_uri} failed: {e:#}"))?;
    if links.is_empty() {
        return Err(anyhow::anyhow!(
            "no FlowTransitionProposal at {proposal_uri}"
        ));
    }
    Ok(links)
}
