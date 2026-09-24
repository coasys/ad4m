//! **Produced by flow**: which instances are valid outputs of a flow.
//!
//! [`receipt`](super::receipt) binds a completed run's outputs to a
//! quorum-signed commitment (#1104), and [`verify`](super::verify) re-decides
//! a receipt from its own contents. This module is the consumer seam on top
//! of the two: the *general* question every app-facing surface asks —
//!
//! > *Is this instance a valid output of flow F \[settled into state S\]?*
//!
//! and its enumeration twin, *which instances are*. The role engine's
//! `grantedByFlow` gate (#1076) is one consumer of this question; the
//! `perspective.flowValidOutputs` query and the model-query
//! `where: { producedByFlow }` filter added here are two more. All of them
//! answer it the same way, and that way fails closed.
//!
//! # The contract: what "valid output" means
//!
//! An instance is a valid output of flow F in state S iff **some receipt
//! carried by the perspective**:
//!
//! 1. **verifies** under this replica's own catalogue — the whole of
//!    [`verify_receipt`]: signatures re-checked, DNA hash equal, the carried
//!    read-set re-folded, and the receipt's outputs hashing to the
//!    `outputs_hash` the final edge's quorum signed;
//! 2. was minted for **flow F** (`receipt.flow_uri`), under the DNA this
//!    replica holds for it;
//! 3. settled into **state S** (when the caller names one — otherwise any
//!    terminal state);
//! 4. **names the instance** among its committed outputs
//!    ([`FlowReceipt::outputs`]) — the binding the quorum signed, never the
//!    anyone-writable `granted_by` discovery edge.
//!
//! Every non-`Verified` verdict excludes the receipt — including the
//! [`Undecidable`](super::verify::VerdictKind::Undecidable) kind. That is
//! deliberately stricter than what `Undecidable` *means* ("this replica
//! cannot decide", not "the receipt is bad"): a query that returns instances
//! a payout system acts on must not return one on a receipt nobody could
//! check. Fail closed, exactly as a payout system must (see
//! [`VerdictKind`](super::verify::VerdictKind)).
//!
//! # The live-content check
//!
//! A receipt attests to an output's content **at completion**, and editing
//! the output later does not invalidate the receipt — its preimages are
//! frozen inside it and still hash to the signed commitment (the ratchet;
//! see [`receipt`](super::receipt)). But the perspective-level query and the
//! model-query filter answer about **live instances**, and an instance whose
//! content has moved since the quorum signed is no longer the thing the
//! quorum committed to. So [`flow_valid_outputs`] re-reads each candidate
//! through its class — the same hydration the commitment was taken over —
//! and keeps it only while the content still equals the carried preimage.
//! `content` includes `updatedAt`, so even re-asserting the same value
//! excludes the instance (that is #1104's definition of content, not a
//! choice made here).
//!
//! The **pure** layer ([`valid_outputs`]) deliberately does *not* apply this
//! check — it has no store to read from, and its consumers differ: the role
//! engine dates a grant from the quorum's moment and must keep honouring it
//! after the role instance accrues links, while an app asking "what may I
//! pay out on" must not. Callers with a perspective get the checked answer;
//! callers with only a receipt get the receipt's answer and the committed
//! content to compare themselves.
//!
//! # Discovery is not trust
//!
//! Receipts for flow F are found through F's index — `F -->
//! ad4m://flow/flow_receipt --> receipt` links, written at mint — and each
//! receipt's `ad4m://flow/receipt_content` body. Any member can write either
//! link, and a forged or damaged body proves nothing: everything above
//! re-derives from the receipt's own contents.
//!
//! The read is **scoped to the flow before it is budgeted**, so other flows'
//! receipts (and stray bodies nobody indexed) never spend F's budget — a busy
//! perspective is the normal case, not an attack. And the budget
//! ([`MAX_FLOW_RECEIPTS`]) **refuses** rather than truncates: a read that
//! would exceed it is a [`ReceiptBudgetExceeded`] error on every surface.
//! Truncating would let anyone who writes enough low-sorting candidates
//! evict every genuine receipt and have every surface answer a confident
//! "no valid outputs" (Lal's review of #1127). "I could not read every
//! receipt" is not "there are none" — the same rule that makes an unknown
//! flow an error. A flood under F's index can still make F's question
//! unanswerable, but loudly, and only F's.
//!
//! # What is deliberately NOT here
//!
//! - **A rename of `ad4m://flow/granted_by`.** The edge name is
//!   role-flavoured for a general mechanism, but #1076 (open) follows and
//!   documents that predicate throughout; renaming it underneath that PR
//!   would conflict for no behavioural gain. Raised in the PR instead.
//! - **Automatic minting on completion.** [`mint_flow_receipt`] is an
//!   explicit call; nothing here mints as a side effect of a vote. Whether
//!   the engine should is a design question for the flow plan, not this PR.

use super::atom::OutputRef;
use super::receipt::{
    is_terminal_state, FlowReceipt, FLOW_GRANTED_BY_PREDICATE, FLOW_RECEIPT_CONTENT_PREDICATE,
    FLOW_RECEIPT_PREDICATE,
};
use super::verify::{verify_receipt, ReceiptVerdict};
use super::FlowInstance;
use crate::agent::AgentContext;
use crate::perspectives::flow_context::{load_all_flow_instances, load_shacl_flows};
use crate::perspectives::flow_evaluator::EvidenceItem;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::SHACLFlow;
use crate::types::{Link, LinkQuery, LinkStatus};
use ad4m_client::literal::{Literal, LiteralValue};
use serde::Serialize;
use std::collections::HashMap;

/// One instance a verified receipt speaks for. What the "valid outputs of
/// flow F" query returns, and what the `producedByFlow` model-query filter
/// admits.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ValidOutput {
    /// The instance, as `(className, id)` — the pair the quorum committed to.
    pub output: OutputRef,
    /// The instance's content **as the quorum committed to it**: the
    /// `model_query` hydration at completion. A consumer that pays out on
    /// the instance as it stands now compares this against a fresh read —
    /// [`flow_valid_outputs`] already has.
    pub content: String,
    /// The terminal state the granting run settled into, re-derived by the
    /// verifier (never read off the receipt on trust).
    pub terminal_state: String,
    /// The content-derived URI of the receipt that proves it, for callers
    /// that want to fetch or re-verify the artifact itself.
    pub receipt_uri: String,
}

/// The outputs of `flow_uri` that `receipts` prove — receipt-level, pure.
///
/// Only outputs of receipts that pass [`verify_receipt`] under `catalogue`
/// are returned; a receipt for another flow, into another state than the
/// named one, or with any non-`Verified` verdict (including the
/// `Undecidable` kind — see the module header) contributes nothing. Each
/// receipt stands alone: a forged receipt beside an honest one excludes
/// itself, never its neighbour.
///
/// Deterministic: sorted by `(class, id, receipt_uri)` and deduplicated per
/// `(output, terminal_state, content)`, so two replicas holding the same
/// receipts enumerate the same list.
pub fn valid_outputs(
    catalogue: &HashMap<String, SHACLFlow>,
    flow_uri: &str,
    state: Option<&str>,
    receipts: &[FlowReceipt],
) -> Vec<ValidOutput> {
    let mut out: Vec<ValidOutput> = Vec::new();
    for receipt in receipts {
        // Cheap pre-check, but also a correctness one: a receipt for another
        // flow may legitimately verify, and must still not answer a question
        // about this one.
        if receipt.flow_uri != flow_uri {
            continue;
        }
        let verdict = verify_receipt(catalogue, receipt);
        let ReceiptVerdict::Verified { terminal_state, .. } = &verdict else {
            log::debug!(
                "valid_outputs: a receipt for `{flow_uri}` does not verify and speaks for \
                 nothing here — {verdict}"
            );
            continue;
        };
        if let Some(state) = state {
            if terminal_state != state {
                continue;
            }
        }
        let receipt_uri = match receipt.uri() {
            Ok(uri) => uri,
            Err(e) => {
                // Verified material that cannot name itself would be carried
                // under an unstable identity; exclude it rather than invent one.
                log::warn!("valid_outputs: a verified receipt does not hash: {e:#}");
                continue;
            }
        };
        for item in &receipt.outputs {
            out.push(ValidOutput {
                output: OutputRef::of(item),
                content: item.content.clone(),
                terminal_state: terminal_state.clone(),
                receipt_uri: receipt_uri.clone(),
            });
        }
    }
    out.sort_by(|a, b| {
        (&a.output, &a.terminal_state, &a.content, &a.receipt_uri).cmp(&(
            &b.output,
            &b.terminal_state,
            &b.content,
            &b.receipt_uri,
        ))
    });
    out.dedup_by(|a, b| {
        a.output == b.output && a.terminal_state == b.terminal_state && a.content == b.content
    });
    out
}

/// Is `output` a valid output of `flow_uri` \[in `state`\], per `receipts`?
/// The membership form of [`valid_outputs`] — one definition, so the two can
/// never disagree.
///
/// Takes the whole `(class, id)` ref, never the id alone, for the reason
/// [`FlowReceipt::speaks_for`] does (#1108): the quorum committed to the
/// node's content as read through one class, and the same node read
/// through another is other content it never saw.
pub fn produced_by_flow(
    catalogue: &HashMap<String, SHACLFlow>,
    output: &OutputRef,
    flow_uri: &str,
    state: Option<&str>,
    receipts: &[FlowReceipt],
) -> bool {
    valid_outputs(catalogue, flow_uri, state, receipts)
        .iter()
        .any(|v| &v.output == output)
}

/// Is a committed output named as the class a `model_query` asks about?
///
/// An output is committed *as an instance of a class* (#1104): the same node
/// read through another class is other content, and not what the quorum
/// signed. So the `producedByFlow` filter admits an output only into queries
/// for that class — matched against either spelling a flow's DNA may have
/// used, the query-side class name or the shape's target class.
///
/// This is defense in depth on top of shape conformance (a node that is not
/// an instance of the queried class never matches its SPARQL patterns), and
/// it is the half conformance cannot do: one node can conform to two classes
/// with different content, and a quorum that committed to it as one has said
/// nothing about it as the other.
pub fn output_matches_class(output: &OutputRef, queried_name: &str, target_class: &str) -> bool {
    output.class_name == queried_name || output.class_name == target_class
}

/// How many receipt candidates one flow's read may carry — index entries,
/// and bodies under them. The links are writable by anyone, so without a
/// bound a member could make every `producedByFlow` query parse and verify
/// thousands of junk bodies. Over the budget the read **errors**
/// ([`ReceiptBudgetExceeded`]); it never drops candidates, because a dropped
/// candidate could be the one genuine witness (see the module header).
pub const MAX_FLOW_RECEIPTS: usize = 256;

/// `flow --> receipt`: the per-flow index [`mint_flow_receipt`] writes, so a
/// question about flow F reads only F's receipts. Discovery only, like every
/// receipt link: anyone may write one, and what it points at still has to
/// verify.
pub const FLOW_RECEIPT_INDEX_PREDICATE: &str = "ad4m://flow/flow_receipt";

/// The receipt read for a flow hit [`MAX_FLOW_RECEIPTS`]. Returned as an
/// error, never as a shorter list: "I could not read every receipt" must not
/// read as "there are no valid outputs" — the same argument that makes an
/// unknown flow an error. Every surface refuses on it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReceiptBudgetExceeded {
    /// The flow whose receipts were being read.
    pub flow: String,
    /// A lower bound on the candidates: the count when the read stopped,
    /// always more than `cap`. For the index it is exact; for bodies the
    /// read stops as soon as the running count passes the budget, so it is
    /// "at least this many" — which is all a refusal needs.
    pub found: usize,
    /// [`MAX_FLOW_RECEIPTS`].
    pub cap: usize,
}

impl std::fmt::Display for ReceiptBudgetExceeded {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "flow `{}` has at least {} receipt candidates, over the receipt budget of {}; its valid \
             outputs cannot be decided without reading them all, so none are reported",
            self.flow, self.found, self.cap
        )
    }
}

impl std::error::Error for ReceiptBudgetExceeded {}

/// The receipts filed under `flow_uri`'s index, parsed — every candidate a
/// question about that flow has to consider. Each is a *claim* to be about
/// the flow; verification comes later and is the caller's job.
///
/// Scoped before it is budgeted: only `flow_uri`'s index entries, and only
/// the bodies under them, count toward [`MAX_FLOW_RECEIPTS`]. Over the
/// budget the whole read is a [`ReceiptBudgetExceeded`] error — never a
/// shorter list (see the module header).
///
/// A body that does not parse as a receipt, or parses as another flow's, is
/// warned about and skipped, not an error: it proves nothing for this flow,
/// and failing on it would hand every writer a veto the budget already
/// bounds. Deterministic: entries and bodies are read in sorted order.
pub async fn load_flow_receipts(
    perspective: &PerspectiveInstance,
    flow_uri: &str,
) -> anyhow::Result<Vec<FlowReceipt>> {
    let over_budget = |found: usize| -> anyhow::Error {
        ReceiptBudgetExceeded {
            flow: flow_uri.to_string(),
            found,
            cap: MAX_FLOW_RECEIPTS,
        }
        .into()
    };

    let mut uris: Vec<String> = perspective
        .get_links(&LinkQuery {
            source: Some(flow_uri.to_string()),
            predicate: Some(FLOW_RECEIPT_INDEX_PREDICATE.to_string()),
            ..Default::default()
        })
        .await?
        .into_iter()
        .map(|l| l.data.target)
        .collect();
    uris.sort();
    uris.dedup();
    if uris.len() > MAX_FLOW_RECEIPTS {
        return Err(over_budget(uris.len()));
    }

    let bodies = read_bodies_within_budget(
        &uris,
        |uri| async move {
            Ok(perspective
                .get_links(&LinkQuery {
                    source: Some(uri),
                    predicate: Some(FLOW_RECEIPT_CONTENT_PREDICATE.to_string()),
                    ..Default::default()
                })
                .await?
                .into_iter()
                .map(|l| l.data.target)
                .collect())
        },
        &over_budget,
    )
    .await?;

    let mut receipts = Vec::new();
    for (uri, body) in bodies {
        let parsed = Literal::from_url(body)
            .and_then(|l| l.get())
            .and_then(|v| match v {
                LiteralValue::Json(json) => Ok(serde_json::from_value::<FlowReceipt>(json)?),
                other => Err(anyhow::anyhow!("not a JSON literal: {other:?}")),
            });
        match parsed {
            Ok(receipt) if receipt.flow_uri == flow_uri => receipts.push(receipt),
            Ok(receipt) => log::warn!(
                "load_flow_receipts: `{uri}` is filed under flow `{flow_uri}` but is a receipt \
                 for `{}`; skipped",
                receipt.flow_uri
            ),
            Err(e) => log::warn!(
                "load_flow_receipts: the body under `{uri}` does not read as a FlowReceipt and \
                 proves nothing: {e:#}"
            ),
        }
    }
    Ok(receipts)
}

/// The bodies under each of `uris`, in order, as `(uri, body)` — one
/// `fetch` (a store read) per URI, each URI's bodies sorted and deduplicated.
/// The moment the running count passes [`MAX_FLOW_RECEIPTS`] the read stops
/// with `over_budget`'s error — at worst one read past the budget, so a
/// flood cannot make the reader visit every remaining entry first.
async fn read_bodies_within_budget<F, Fut>(
    uris: &[String],
    mut fetch: F,
    over_budget: &impl Fn(usize) -> anyhow::Error,
) -> anyhow::Result<Vec<(String, String)>>
where
    F: FnMut(String) -> Fut,
    Fut: std::future::Future<Output = anyhow::Result<Vec<String>>>,
{
    let mut bodies: Vec<(String, String)> = Vec::new();
    for uri in uris {
        let mut under = fetch(uri.clone()).await?;
        under.sort();
        under.dedup();
        bodies.extend(under.into_iter().map(|body| (uri.clone(), body)));
        // Inside the loop: the budget bounds the reading, not just the
        // answer — at worst one store read past it, never one per entry.
        if bodies.len() > MAX_FLOW_RECEIPTS {
            return Err(over_budget(bodies.len()));
        }
    }
    Ok(bodies)
}

/// The instances that are, **as they stand**, valid outputs of `flow_uri`
/// \[in `state`\] on this perspective.
///
/// [`valid_outputs`] over the perspective's own catalogue and carried
/// receipts, plus the live-content check the module header describes: each
/// candidate is re-read through its class and kept only while its content
/// still equals what the quorum committed to. An output that cannot be
/// loaded, is no longer an instance of its class, or has been edited since
/// completion is excluded — every failure direction excludes.
///
/// A `flow_uri` this perspective's catalogue does not hold is an **error**,
/// not an empty list: "no such flow here" and "no valid outputs" must not
/// be the same answer (the same three-kind argument
/// [`VerdictKind`](super::verify::VerdictKind) makes).
pub async fn flow_valid_outputs(
    perspective: &PerspectiveInstance,
    flow_uri: &str,
    state: Option<&str>,
) -> anyhow::Result<Vec<ValidOutput>> {
    let catalogue = load_shacl_flows(perspective).await?;
    if !catalogue.contains_key(flow_uri) {
        anyhow::bail!(
            "flow_valid_outputs: flow `{flow_uri}` is not on this perspective — nothing about \
             its outputs can be decided here"
        );
    }
    let receipts = load_flow_receipts(perspective, flow_uri).await?;
    let candidates = valid_outputs(&catalogue, flow_uri, state, &receipts);

    let refs: Vec<OutputRef> = candidates.iter().map(|c| c.output.clone()).collect();
    let live = super::accept::load_outputs(perspective, &refs).await?;
    Ok(candidates
        .into_iter()
        .filter(|c| match live.get(&c.output) {
            Some(now) => {
                let intact = now.content == c.content;
                if !intact {
                    log::debug!(
                        "flow_valid_outputs: `{}` was edited since flow `{flow_uri}` completed; \
                         its live content no longer matches the quorum's commitment, so it is \
                         not returned",
                        c.output.id
                    );
                }
                intact
            }
            None => {
                log::debug!(
                    "flow_valid_outputs: `{}` is not readable as a `{}` any more, so it is not \
                     returned",
                    c.output.id,
                    c.output.class_name
                );
                false
            }
        })
        .collect())
}

/// Re-decide `receipt` under this perspective's own catalogue. The verdict
/// is [`verify_receipt`]'s, three-kinded and all; [`verdict_wire`] is the
/// serialisation the API hands out.
pub async fn verify_flow_receipt(
    perspective: &PerspectiveInstance,
    receipt: &FlowReceipt,
) -> anyhow::Result<ReceiptVerdict> {
    let catalogue = load_shacl_flows(perspective).await?;
    Ok(verify_receipt(&catalogue, receipt))
}

/// The verdict as the API reports it: the three-kind `outcome` a caller must
/// branch on, the human-readable `detail`, and — only when verified — what
/// was verified. The kind travels explicitly so a client cannot reach
/// "reject" through a boolean (the trap [`VerdictKind`](super::verify::VerdictKind)
/// exists to close).
pub fn verdict_wire(verdict: &ReceiptVerdict) -> serde_json::Value {
    use super::verify::VerdictKind;
    let outcome = match verdict.outcome() {
        VerdictKind::Verified => "verified",
        VerdictKind::Rejected => "rejected",
        VerdictKind::Undecidable => "undecidable",
    };
    let mut wire = serde_json::json!({
        "outcome": outcome,
        "detail": verdict.to_string(),
    });
    if let ReceiptVerdict::Verified {
        terminal_state,
        outputs,
        voters,
    } = verdict
    {
        wire["terminalState"] = serde_json::json!(terminal_state);
        wire["outputs"] = serde_json::json!(outputs);
        wire["voters"] = serde_json::json!(voters);
    }
    wire
}

/// Mint the receipt for a completed run and write it to the perspective:
/// the content-addressed receipt node with its body, plus the discovery
/// edges (`instance → receipt`, `output → receipt`, and the flow's own
/// index `flow → receipt` that [`load_flow_receipts`] reads — indexes,
/// never inputs; see [`receipt`](super::receipt)).
///
/// Everything a receipt claims is derived here the way [`FlowReceipt::mint`]
/// demands: the read-set is collected live, the outputs are the ones the
/// final edge's counted atoms name, their content is what `model_query`
/// returns for them **now** — so a run whose outputs were edited since
/// completion refuses to mint, which is #1104's rule, not a limitation.
/// Preimages are collected best-effort: a guard whose evidence no longer
/// reproduces its seal is skipped with a warning (verification requires
/// none), never invented.
///
/// Anyone may mint (the receipt's authority is its contents), and duplicate
/// mints converge on one node by URI.
pub async fn mint_flow_receipt(
    perspective: &mut PerspectiveInstance,
    instance_uri: &str,
    context: &AgentContext,
) -> anyhow::Result<FlowReceipt> {
    use super::receipt::EvidencePreimage;
    use crate::perspectives::flow_evaluator::{recompute_evidence_seal, EvidenceSeal};

    let catalogue = load_shacl_flows(perspective).await?;
    let records = load_all_flow_instances(perspective).await?;
    let record = records
        .iter()
        .find(|r| r.instance_uri == instance_uri)
        .ok_or_else(|| anyhow::anyhow!("no FlowInstance at {instance_uri}"))?;
    let flow = catalogue
        .get(&record.flow_uri)
        .ok_or_else(|| anyhow::anyhow!("flow `{}` is not in the catalogue", record.flow_uri))?;
    let instance = FlowInstance::from_record(record, flow);
    let read_set = instance.read_set(perspective).await?;

    // The outputs the run committed to, read off the counted atoms of the
    // final settled edge. `mint` re-derives the fold and re-checks the hash,
    // so this is a collection step, not a trust step.
    let ingested = read_set.reverified();
    let derived = super::fold_read_set(flow, &ingested)?;
    let final_edge = derived.settled.last().ok_or_else(|| {
        anyhow::anyhow!("{instance_uri} has no settled edge, so there is no completion to mint")
    })?;
    let refs: Vec<OutputRef> = {
        let mut refs: Vec<OutputRef> = ingested
            .atoms()
            .into_iter()
            .filter(|a| final_edge.atom_uris.contains(&a.uri))
            .flat_map(|a| a.outputs)
            .collect();
        refs.sort();
        refs.dedup();
        refs
    };
    let loaded = super::accept::load_outputs(perspective, &refs).await?;
    let outputs: Vec<EvidenceItem> = refs.iter().filter_map(|r| loaded.get(r).cloned()).collect();

    let mut preimages: Vec<EvidencePreimage> = Vec::new();
    for counted in FlowReceipt::counted_seals(flow, &read_set)? {
        let record_now = crate::perspectives::flow_context::FlowInstanceRecord {
            current_state: derived.state.clone(),
            ..record.clone()
        };
        let sealed = recompute_evidence_seal(
            perspective,
            flow,
            &record_now,
            &counted.to_state,
            &counted.proposer,
        )
        .await?;
        match sealed.seal.hash() {
            Some(hash) if hash == counted.seal => preimages.push(EvidencePreimage {
                seal: counted.seal,
                class_names: sealed.class_names,
                items: sealed.evidence,
            }),
            _ => log::warn!(
                "mint_flow_receipt: {instance_uri}: the evidence for `{}`'s seal no longer \
                 reproduces it; the receipt is minted without that preimage (verification \
                 requires none)",
                counted.to_state
            ),
        }
        debug_assert!(matches!(
            sealed.seal,
            EvidenceSeal::Sealed(_) | EvidenceSeal::NoGuard | EvidenceSeal::Unmet
        ));
    }

    let receipt = FlowReceipt::mint(flow, read_set, outputs, preimages)?;
    let uri = receipt.uri()?;
    let body = Literal::from_json(serde_json::to_value(&receipt)?).to_url()?;

    let mut links = vec![
        Link {
            source: uri.clone(),
            predicate: Some(FLOW_RECEIPT_CONTENT_PREDICATE.to_string()),
            target: body,
        },
        Link {
            source: instance_uri.to_string(),
            predicate: Some(FLOW_RECEIPT_PREDICATE.to_string()),
            target: uri.clone(),
        },
        Link {
            source: receipt.flow_uri.clone(),
            predicate: Some(FLOW_RECEIPT_INDEX_PREDICATE.to_string()),
            target: uri.clone(),
        },
    ];
    for output in &receipt.outputs {
        links.push(Link {
            source: output.id.clone(),
            predicate: Some(FLOW_GRANTED_BY_PREDICATE.to_string()),
            target: uri.clone(),
        });
    }
    perspective
        .add_links(links, LinkStatus::Shared, None, context)
        .await?;

    debug_assert!(is_terminal_state(flow, &receipt.terminal_state));
    Ok(receipt)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::flow_instance::atom::fixtures::{
        hash_of, out_item, out_items, out_ref, signed_terminal_proposal, T1,
    };
    use crate::perspectives::flow_instance::{ProposalLinks, ReadSet};

    const INSTANCE: &str = "ad4m://flow/instance/i1";
    const BASE: &str = "ad4m://task/t1";
    const ALICE: &str = "alice";
    const OUTPUT: &str = "ad4m://deliverable/d1";
    const ATTACKER: &str = "ad4m://attacker/node";
    const FLOW: &str = "coasys://DeliveryFlow";

    /// `open → done`, `done` terminal and unguarded, default `{ n: 1 }`.
    fn flow_named(name: &str) -> SHACLFlow {
        serde_json::from_value(serde_json::json!({
            "name": name,
            "namespace": "coasys://",
            "states": [
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0 },
            ],
            "transitions": [
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ],
        }))
        .expect("fixture flow parses")
    }

    fn catalogue(flows: Vec<SHACLFlow>) -> HashMap<String, SHACLFlow> {
        flows.into_iter().map(|f| (f.flow_uri(), f)).collect()
    }

    /// A completed run of `flow` whose final proposal names `ids`, honestly
    /// committed and signed for real, under the content-addressed URI the
    /// fixture computes from those same fields (#1108).
    fn completed(ids: &[&str]) -> ReadSet {
        let (uri, links) = signed_terminal_proposal(
            "p1",
            ALICE,
            "open",
            "done",
            &crate::perspectives::flow_evaluator::evidence_hash(&[], &[]),
            ids,
            &hash_of(ids),
            T1,
        );
        ReadSet {
            instance_uri: INSTANCE.to_string(),
            subject: BASE.to_string(),
            genesis: "open".to_string(),
            proposals: vec![ProposalLinks { uri, links }],
            role_grants: Vec::new(),
        }
    }

    fn mint(flow: &SHACLFlow, ids: &[&str]) -> FlowReceipt {
        FlowReceipt::mint(flow, completed(ids), out_items(ids), Vec::new())
            .expect("the fixture read-set mints")
    }

    fn honest() -> FlowReceipt {
        mint(&flow_named("Delivery"), &[OUTPUT])
    }

    // ---- the contract, positively ----------------------------------------

    /// The control: a verified receipt's committed outputs are the flow's
    /// valid outputs, with the state the verifier re-derived and the
    /// receipt's own content-derived URI.
    ///
    /// Red if `valid_outputs` reads `terminal_state` off the receipt rather
    /// than off the verdict, or reports a URI not derived from the content.
    #[test]
    fn a_verified_receipts_outputs_are_the_flows_valid_outputs() {
        let receipt = honest();
        let cat = catalogue(vec![flow_named("Delivery")]);

        let outputs = valid_outputs(&cat, FLOW, None, std::slice::from_ref(&receipt));
        assert_eq!(
            outputs,
            vec![ValidOutput {
                output: OutputRef::of(&out_item(OUTPUT)),
                content: out_item(OUTPUT).content,
                terminal_state: "done".into(),
                receipt_uri: receipt.uri().expect("uri"),
            }]
        );
        assert!(produced_by_flow(
            &cat,
            &out_ref(OUTPUT),
            FLOW,
            None,
            std::slice::from_ref(&receipt)
        ));
        assert!(produced_by_flow(
            &cat,
            &out_ref(OUTPUT),
            FLOW,
            Some("done"),
            std::slice::from_ref(&receipt)
        ));
        // The same node named through another class is not what the quorum
        // committed to (#1108): membership is by `(class, id)`, never by id.
        assert!(!produced_by_flow(
            &cat,
            &OutputRef {
                class_name: "coasys://Role".into(),
                id: OUTPUT.into(),
            },
            FLOW,
            None,
            std::slice::from_ref(&receipt)
        ));
    }

    /// The run's subject, and any stranger node, are not outputs: only what
    /// the proposer named and the quorum committed to is.
    #[test]
    fn a_node_the_quorum_did_not_commit_to_is_not_an_output() {
        let receipt = honest();
        let cat = catalogue(vec![flow_named("Delivery")]);
        for not_an_output in [BASE, ATTACKER, INSTANCE] {
            assert!(
                !produced_by_flow(
                    &cat,
                    &out_ref(not_an_output),
                    FLOW,
                    None,
                    std::slice::from_ref(&receipt)
                ),
                "`{not_an_output}` must not read as an output"
            );
        }
    }

    // ---- fail closed, per receipt ----------------------------------------

    /// The #1104 re-mint against the enumeration: the honest receipt with
    /// its outputs swapped for the attacker's node. It fails verification
    /// (`OutputsNotCommitted`) and contributes nothing — while the honest
    /// receipt sitting right beside it still answers. Fail-closed is per
    /// receipt, never per set.
    ///
    /// Red if `valid_outputs` reads `receipt.outputs` without verifying, or
    /// drops every receipt once one is bad.
    #[test]
    fn a_forged_re_mint_naming_another_node_contributes_nothing() {
        let honest = honest();
        let mut forged = honest.clone();
        forged.outputs = out_items(&[ATTACKER]);
        let cat = catalogue(vec![flow_named("Delivery")]);

        let receipts = [forged, honest];
        let outputs = valid_outputs(&cat, FLOW, None, &receipts);
        assert!(
            outputs.iter().all(|v| v.output.id != ATTACKER),
            "a forged receipt's outputs must not be listed: {outputs:?}"
        );
        assert!(
            outputs.iter().any(|v| v.output.id == OUTPUT),
            "the honest receipt beside the forgery must still answer: {outputs:?}"
        );
        assert!(!produced_by_flow(
            &cat,
            &out_ref(ATTACKER),
            FLOW,
            None,
            &receipts
        ));
    }

    /// A receipt carrying an output's content other than what the quorum
    /// committed to — the edited-then-re-minted shape — is refused whole.
    ///
    /// Red if the enumeration compares refs instead of content.
    #[test]
    fn a_receipt_carrying_edited_content_contributes_nothing() {
        let mut edited = honest();
        edited.outputs[0].content =
            serde_json::json!({ "id": OUTPUT, "title": "edited" }).to_string();
        let cat = catalogue(vec![flow_named("Delivery")]);

        assert!(
            valid_outputs(&cat, FLOW, None, std::slice::from_ref(&edited)).is_empty(),
            "content the quorum did not sign must not be listed"
        );
    }

    /// `Undecidable` excludes exactly as `Rejected` does. The receipt may
    /// well be fine — an un-synced catalogue is the reader's gap — but a
    /// query a payout system acts on must not return an instance on a
    /// receipt nobody could check.
    ///
    /// Red if the filter is written `!is_rejected()` — the boolean trap
    /// [`VerdictKind`] documents.
    #[test]
    fn an_undecidable_receipt_excludes_its_outputs_rather_than_vouching_for_them() {
        let receipt = honest();

        // FlowUnknown: the reader holds no definition at all.
        assert!(
            valid_outputs(
                &catalogue(Vec::new()),
                FLOW,
                None,
                std::slice::from_ref(&receipt)
            )
            .is_empty(),
            "no catalogue, no answer — and no output"
        );

        // DnaChanged: the reader holds a different definition under the URI.
        let mut other_dna = flow_named("Delivery");
        other_dna.states.push(
            serde_json::from_value(serde_json::json!({ "name": "extra", "value": 2.0 }))
                .expect("state parses"),
        );
        assert!(
            valid_outputs(
                &catalogue(vec![other_dna]),
                FLOW,
                None,
                std::slice::from_ref(&receipt)
            )
            .is_empty(),
            "a receipt minted under other DNA answers nothing here"
        );
    }

    // ---- the flow and state dimensions ------------------------------------

    /// A receipt for another flow — even one that verifies — does not answer
    /// a question about this one; and a state filter admits only runs that
    /// settled into that state.
    ///
    /// Red if the `flow_uri` pre-check or the state comparison is dropped.
    #[test]
    fn a_receipt_answers_only_for_its_own_flow_and_state() {
        let delivery = flow_named("Delivery");
        let other = flow_named("Onboarding");
        let other_uri = other.flow_uri();
        let foreign = mint(&other, &[OUTPUT]);
        let cat = catalogue(vec![delivery, other]);

        assert!(
            valid_outputs(&cat, FLOW, None, std::slice::from_ref(&foreign)).is_empty(),
            "completing Onboarding must not produce Delivery outputs"
        );
        assert_eq!(
            valid_outputs(&cat, &other_uri, None, std::slice::from_ref(&foreign)).len(),
            1,
            "the same receipt answers for its own flow"
        );
        assert!(
            valid_outputs(
                &cat,
                &other_uri,
                Some("open"),
                std::slice::from_ref(&foreign)
            )
            .is_empty(),
            "a state the run did not settle into admits nothing"
        );
    }

    // ---- the class dimension ----------------------------------------------

    /// The `producedByFlow` model-query filter admits an output only into
    /// queries for the class it was committed as — by either spelling — and
    /// a quorum that committed to a node as one class has said nothing about
    /// it as another. Conformance cannot carry this alone: one node can
    /// conform to two classes with different content.
    ///
    /// Red if `output_matches_class` returns true for a foreign class, or
    /// matches only one of the two spellings.
    #[test]
    fn an_output_counts_only_for_the_class_it_was_committed_as() {
        let committed = OutputRef {
            class_name: "coasys://Deliverable".into(),
            id: OUTPUT.into(),
        };
        assert!(output_matches_class(
            &committed,
            "coasys://Deliverable",
            "coasys://Deliverable"
        ));
        assert!(
            output_matches_class(&committed, "Deliverable", "coasys://Deliverable"),
            "the DNA may name the target class where the query names the shape"
        );
        assert!(
            output_matches_class(&committed, "coasys://Deliverable", "we://deliverable"),
            "or the query-side name where the shape's target differs"
        );
        assert!(
            !output_matches_class(&committed, "coasys://Task", "we://task"),
            "a query for another class must not inherit the commitment"
        );
    }

    // ---- the API verdict ---------------------------------------------------

    /// `perspective.verifyFlowReceipt` hands out `verdict_wire`, and a client
    /// branches on its `outcome` string — so each of the three kinds must
    /// reach the wire as itself. A `Rejected` spelled `"verified"` is a
    /// forged receipt vouched for; an `Undecidable` spelled `"rejected"` is
    /// the boolean trap [`VerdictKind`](super::super::verify::VerdictKind)
    /// exists to close, slandering a receipt this replica merely could not
    /// check. What was verified travels only with a verified verdict.
    ///
    /// Red if any arm of the `outcome` mapping names another kind, or if the
    /// verified details leak onto a non-verified verdict.
    #[test]
    fn the_wire_verdict_names_each_kind_as_itself() {
        let cat = catalogue(vec![flow_named("Delivery")]);
        let honest = honest();
        let mut forged = honest.clone();
        forged.outputs = out_items(&[ATTACKER]);

        let verified = verdict_wire(&verify_receipt(&cat, &honest));
        assert_eq!(verified["outcome"], "verified", "{verified}");
        assert_eq!(verified["terminalState"], "done", "{verified}");
        assert!(
            verified["outputs"].as_array().is_some_and(|o| o.len() == 1),
            "{verified}"
        );

        let rejected = verdict_wire(&verify_receipt(&cat, &forged));
        assert_eq!(rejected["outcome"], "rejected", "{rejected}");
        assert!(
            rejected.get("outputs").is_none() && rejected.get("terminalState").is_none(),
            "a rejected verdict carries nothing a client could act on: {rejected}"
        );

        let undecidable = verdict_wire(&verify_receipt(&catalogue(Vec::new()), &honest));
        assert_eq!(undecidable["outcome"], "undecidable", "{undecidable}");
        assert!(
            undecidable.get("outputs").is_none(),
            "an undecidable verdict vouches for nothing either: {undecidable}"
        );
    }

    // ---- the body budget's cost -------------------------------------------

    /// The body budget bounds the **reading**, not just the answer: a flood
    /// of bodies under the index must stop the read as soon as the running
    /// count passes the budget — at worst one store read past it — instead
    /// of paying a read per remaining index entry and holding every body
    /// before refusing (Lal's approval note on #1127). `found` is then "at
    /// least this many", which is all the refusal needs.
    ///
    /// Red if the check sits after the loop: every one of the `MAX` entries
    /// is fetched first.
    #[tokio::test]
    async fn the_body_budget_stops_reading_as_soon_as_it_is_exceeded() {
        let uris: Vec<String> = (0..MAX_FLOW_RECEIPTS)
            .map(|i| format!("ad4m://flow/receipt/{i:04}"))
            .collect();
        let mut fetched = 0usize;
        let err = read_bodies_within_budget(
            &uris,
            |uri| {
                fetched += 1;
                // Every entry carries a full budget's worth of bodies.
                async move {
                    Ok((0..MAX_FLOW_RECEIPTS)
                        .map(|j| format!("{uri}#{j:04}"))
                        .collect())
                }
            },
            &|found| {
                ReceiptBudgetExceeded {
                    flow: FLOW.to_string(),
                    found,
                    cap: MAX_FLOW_RECEIPTS,
                }
                .into()
            },
        )
        .await
        .expect_err("a body flood is over budget");

        assert_eq!(
            fetched, 2,
            "the first read fills the budget exactly, the second passes it — and the read stops there"
        );
        let over = err
            .downcast_ref::<ReceiptBudgetExceeded>()
            .expect("the typed budget error");
        assert_eq!(
            over.found,
            2 * MAX_FLOW_RECEIPTS,
            "at least this many, counted so far"
        );
        assert!(over.found > over.cap);
    }

    // ---- determinism -------------------------------------------------------

    /// Two mints of the same completion collapse to one entry, and the list
    /// is sorted — two replicas enumerating the same receipts agree.
    #[test]
    fn the_enumeration_is_sorted_and_deduplicated() {
        let flow = flow_named("Delivery");
        let cat = catalogue(vec![flow_named("Delivery")]);
        let twin_a = mint(&flow, &[OUTPUT, "ad4m://deliverable/d2"]);
        let twin_b = twin_a.clone();

        let outputs = valid_outputs(&cat, FLOW, None, &[twin_a, twin_b]);
        let ids: Vec<&str> = outputs.iter().map(|v| v.output.id.as_str()).collect();
        assert_eq!(ids, vec![OUTPUT, "ad4m://deliverable/d2"]);
    }
}
