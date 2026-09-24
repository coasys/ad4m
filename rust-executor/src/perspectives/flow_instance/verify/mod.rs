//! The other end of a [receipt](super::receipt): reading one that arrived.
//!
//! [`FlowReceipt::mint`](super::receipt::FlowReceipt::mint) freezes a settled
//! run into a content-addressed artifact. This module is what somebody does
//! with it afterwards — on a different replica, months later, with the minter
//! gone.
//!
//! # What a verified receipt says
//!
//! > *n distinct eligible DIDs each signed a vote naming a proposal URI that
//! > is the content address of this seal and this outputs commitment (and
//! > the instance, the edge and the proposer), and here is the material the
//! > seal was taken over and the outputs the commitment hashes.*
//!
//! Every word of that is cryptographic or re-derivable. A vote is the link
//! `uri --acceptedBy--> did`; its signature covers that link (the URI, the
//! predicate, the voter's DID, the timestamp) and **none of the proposal's
//! fields directly**. The seal and the outputs commitment are signed only
//! by the proposer. What ties them to the vote is the URI: it is the
//! SHA-256 content address of `(instance, from_state, to_state, seal,
//! outputs_hash-or-none)` under the proposer's DID and a proposer-signed
//! nonce ([`atom::proposal_uri`](super::atom::proposal_uri)), recomputed
//! from the proposer-signed fields on every read in
//! `TransitionAtom::from_links` (#1108). A proposal whose fields do not
//! hash to its URI is not an atom, and none of its votes count. So a vote
//! binds the seal and the commitment exactly as far as SHA-256 is
//! collision-resistant. The proposer picks every input, nonce included, so
//! collision resistance is the property that matters, not second-preimage
//! resistance.
//!
//! So: n distinct DIDs signed `acceptedBy` links ([`atom::signed_by`], over
//! verdicts this replica recomputed) on an atom whose URI addresses seal S
//! and outputs hash H; each was eligible under the carried role evidence
//! **as of its own vote's timestamp**; S rehashes from the carried
//! preimage; and the receipt's output preimages (class, id, content) hash
//! to H, checked here, never read on trust (#1104). H is the one commitment
//! every counted atom on the final edge carries. The fold makes that so
//! by pooling terminal-edge votes per commitment, and step 7 below
//! re-checks it.
//!
//! The URI does **not** cover the proposer's `output` links (the list of
//! named refs). It does not need to. A receipt's outputs are checked
//! against H, not against those links, so re-signing them after the votes
//! changes nothing a verifier accepts.
//!
//! The protocol *requires* each of those voters to have recomputed the seal
//! and the outputs commitment against their own graph and refused to sign on
//! mismatch ([`super::accept`] is what does it on an honest client). **The receipt records that
//! requirement; it does not evidence compliance with it.** A voter running
//! modified code signs without recomputing, and nothing about the
//! recomputation is itself signed or carried, so no verifier can tell the two
//! apart. Wording this precisely matters more here than anywhere else: this
//! is the sentence a downstream payout system quotes. (Caught by
//! @lal-bot-coasys reviewing this PR; the earlier phrasing asserted the
//! recomputation as a proven fact.)
//!
//! Not "the author of that flow instance signed it" either — instance
//! authorship is never checked and never carried. Author-signed guard
//! evidence stays deferred on the model-query signature gap
//! (<https://github.com/coasys/ad4m/issues/1046>): hydrated `model_query`
//! results carry no per-link `(author, proof.valid)`, so there is nothing
//! signed to carry yet. The trust root is the voter quorum.
//!
//! [`super::receipt`] carries the full *does NOT prove* table. Two rows are
//! this module's to settle, and it settles them:
//!
//! | Row on [`super::receipt`] | What verification does about it |
//! | --- | --- |
//! | *That signatures were re-verified by the reader* | Settled. [`ReadSet::reverified`](super::ReadSet::reverified) recomputes every carried verdict before the fold, and mint folds through the same call — #1068. |
//! | *That the run is still settled now* | Unchanged, and deliberately. Nothing here is re-queried against a live graph; see § *Nothing is re-queried* below. |
//!
//! Two rows are added by this module rather than settled:
//!
//! | Not proven | Why |
//! | --- | --- |
//! | **That any voter actually recomputed the seal before signing** | Compliance with a protocol obligation, not a property of the artifact — see above. What a signature proves is that the signer signed *that URI*, which addresses that seal and that outputs commitment (#1108). |
//! | **That every counted atom's seal is inspectable** | `mint` accepts an empty `evidence_preimage`, so a receipt may carry none. Verification checks every preimage it *is* given and requires none. Demanding one per counted seal would reject receipts mint produced — the asymmetry that makes receipts fail their own verification. Tightening it belongs on the mint side first. |
//! | **That the named outputs existed, with that content** | A voter loads every named output through its class on its own graph and recomputes the content hash before co-signing ([`check_outputs_commitment`](super::atom::check_outputs_commitment)). Like the seal recompute, that is a protocol obligation the receipt records and cannot evidence, and a pure verifier has no graph to look in. |
//! | **That the outputs still look like that** | A receipt attests to the content at completion. An output edited later does not invalidate it: the carried preimages still hash to the signed commitment. A consumer that cares about the current content compares it itself. |
//!
//! # Order is part of the contract
//!
//! ```text
//!   receipt
//!     │
//!     ▼ 1. is this flow in MY catalogue?          no ──► FlowUnknown
//!     ▼ 2. is my copy the DNA it was minted under? no ──► DnaChanged
//!     ▼ 3. does it bind to anything?               no ──► NoOutputs
//!     ▼ 4. does every carried preimage re-hash?    no ──► SealMismatch
//!     ▼ 5. ReadSet::reverified() ─► fold_read_set        Unfoldable
//!     ▼ 6. uncontested? claimed state? terminal?   no ──► Contested /
//!     │                                                  StateMismatch /
//!     │                                                  NotTerminal
//!     ▼ 7. one outputs_hash on the final edge,     no ──► NoFinalEdge /
//!     │    and hash(receipt.outputs) equals it?          OutputsUncommitted /
//!     │                                                  OutputsCommitmentConflict /
//!     ▼                                                  OutputsNotCommitted
//!    Verified
//! ```
//!
//! Step 7 has to come after the fold, because it reads the fold's final
//! edge. It is what binds `outputs` to signed material (#1104): see
//! [`super::receipt`], § *Outputs are what the quorum committed to*.
//!
//! Steps 1 and 2 come first **on purpose, not for tidiness**. A flow
//! definition is a space's social DNA, and the hash of that DNA is the
//! identity of the social organism. Re-running a quorum's decision under
//! rules it never agreed to is not well-posed: the verifier would be
//! answering a question nobody asked and reporting the answer with the
//! confidence of one that was. So it refuses instead — and refuses with a
//! verdict that *names the reason*, because [`ReceiptVerdict::DnaChanged`]
//! read as "this receipt is bad" would be a slander on a receipt that is
//! merely old.
//!
//! [`ReceiptVerdict::FlowUnknown`] is its own verdict for the same reason. A
//! reader who has never synced the definition has learned nothing about the
//! receipt, and folding it into a failure that reads like evidence would let
//! "I do not have the rules" be reported as "the quorum did not settle this".
//!
//! # Nothing is re-queried
//!
//! `verify_receipt` takes a catalogue and a receipt. It takes no perspective,
//! and there is nowhere for one to enter: the function is pure over its two
//! arguments.
//!
//! That is the whole point of the artifact. The verifier recomputes
//! `evidence_hash` from the preimage **carried in the receipt** and requires
//! it to equal the seal every voter signed. It does not re-run the guard
//! against today's graph. What the receipt shows is what the voters saw — a
//! URI and a content hash inside an [`EvidenceItem`](crate::perspectives::flow_evaluator::EvidenceItem)
//! are a *reference*, so a reader can go look; they are never a replay input,
//! because the thing they reference may be gone, edited, or never have been
//! visible to this replica at all.
//!
//! The consequence worth stating plainly: **a receipt still verifies after
//! the links behind it are retracted.** Deleting a settling vote moves the
//! live flow back — that is the engine's semantics, not a failure mode — and
//! the receipt, which froze the links as they stood, keeps saying what it
//! always said. Receipt and live fold then disagree, deliberately. A verified
//! receipt means *granted at quorum time T*; un-granting is a new signed
//! event, never an invalidation of the receipt.
//!
//! # The binding is the consumer's check, not a verdict
//!
//! Both discovery edges (`instance --ad4m://flow/receipt--> receipt`,
//! `output --ad4m://flow/granted_by--> receipt`) and the per-flow index
//! [`produced`](super::produced) reads are plain multi-edges **anyone may
//! write, onto any node, pointing at any receipt.** A `Verified` verdict says
//! the receipt is a genuine completion of its own flow; it says nothing about
//! which node asked. So every consumer asks the second question itself —
//! does this verified receipt name *my* `(class, id)`? — through
//! [`FlowReceipt::speaks_for`]. There is exactly one such consumer path:
//! [`produced::valid_outputs`](super::produced::valid_outputs), which the
//! `grantedByFlow` role gate ([`grant`](super::grant)) and the app-facing
//! `producedByFlow` surfaces all go through.
//!
//! Either half alone is forgeable: verifying without the binding lets anyone
//! point a genuine receipt at their own node, and binding without verifying
//! lets anyone write a receipt that names whatever they like.
//!
//! # The untrusted boundary is here
//!
//! A receipt is the first thing in this engine that arrives from off-replica,
//! which makes this the seam where a carried `proof.valid` — the *minter's*
//! claim about a link — must stop being inherited. [`ReadSet::reverified`](super::ReadSet::reverified)
//! does that, and both sides of the artifact run it. See its doc for why the
//! filter is spelled `== Some(true)` and never `!= Some(false)`, and for why
//! a broken grant signature collapses an eligibility window rather than
//! widening one.

#[cfg(test)]
mod tests;
pub mod verdict;

use super::atom::{outputs_hash, OutputRef};
use super::fold::Contention;
use super::fold_read_set;
use super::grant::GrantContext;
use super::receipt::{
    final_edge_commitment, flow_dna_hash, is_terminal_state, FlowReceipt, OutputsCommitment,
};
use crate::perspectives::shacl_parser::SHACLFlow;
use std::collections::{BTreeSet, HashMap};
pub use verdict::ReceiptVerdict;

/// Re-decide a receipt from its own contents and the reader's own catalogue.
///
/// **Pure.** No perspective, no store, no clock, no network — see the module
/// header, § *Nothing is re-queried*. The ordering of the checks is part of
/// the contract; it is stated there and pinned by
/// `dna_change_is_reported_before_any_evidence_step`.
///
/// `catalogue` is keyed by `flow_uri`, the shape
/// [`load_shacl_flows`](crate::perspectives::flow_context::load_shacl_flows)
/// returns and [`accept`](super::accept) and [`propose`](super::propose)
/// already take.
///
/// It does **not** say which node the receipt speaks for — see the module
/// header, § *The binding is the consumer's check*.
pub fn verify_receipt(
    catalogue: &HashMap<String, SHACLFlow>,
    receipt: &FlowReceipt,
) -> ReceiptVerdict {
    verify_receipt_within(GrantContext::root(catalogue), receipt)
}

/// [`verify_receipt`] with an explicit depth budget — the entry point for a
/// receipt reached by following a `granted_by` edge out of material already
/// being verified.
///
/// Same checks in the same order; the only difference is that the grant gates
/// inside its fold get whatever budget is left rather than a fresh one. See
/// [`grant`](super::grant) § *What the cap counts* for why that makes
/// verification non-compositional past the cap, and why the direction of that
/// is fail-closed.
pub(crate) fn verify_receipt_within(
    ctx: GrantContext<'_>,
    receipt: &FlowReceipt,
) -> ReceiptVerdict {
    let catalogue = ctx.catalogue();

    // 1. Do I have the rules at all? A reader who has not synced the
    //    definition has learned nothing about the receipt.
    let Some(flow) = catalogue.get(&receipt.flow_uri) else {
        return ReceiptVerdict::FlowUnknown {
            flow_uri: receipt.flow_uri.clone(),
        };
    };

    // 2. Are they the SAME rules? Before any evidence step: re-running a
    //    guard's seal under changed DNA is not a well-posed question.
    let held = match flow_dna_hash(flow) {
        Ok(hash) => hash,
        Err(e) => {
            return ReceiptVerdict::Unfoldable {
                reason: format!(
                    "this replica's copy of `{}` does not hash: {e:#}",
                    flow.name
                ),
            }
        }
    };
    if held != receipt.flow_dna_hash {
        return ReceiptVerdict::DnaChanged {
            claimed: receipt.flow_dna_hash.clone(),
            held,
        };
    }

    // 3. Does it bind to anything? A receipt with no outputs speaks for
    //    nothing, whatever the rest of it proves.
    if receipt.outputs.is_empty() {
        return ReceiptVerdict::NoOutputs;
    }

    // 4. Is the evidence the material the voters sealed? Re-hashed from the
    //    preimage CARRIED HERE, never re-queried: see the module header.
    if let Some(bad) = receipt
        .evidence_preimage
        .iter()
        .find(|p| !p.rehashes_to_seal())
    {
        return ReceiptVerdict::SealMismatch {
            seal: bad.seal.clone(),
        };
    }

    // 5. The same fold, over the same ingest, that `mint` ran — and with the
    //    same remaining grant budget, so a nested receipt gets the same answer
    //    on both sides.
    let ingested = receipt.read_set.reverified();
    let derived = match fold_read_set(flow, &ingested, ctx) {
        Ok(derived) => derived,
        Err(e) => {
            return ReceiptVerdict::Unfoldable {
                reason: format!("{e:#}"),
            }
        }
    };

    // 6. Does the walk say what the receipt says?
    if let Some(Contention {
        from_state,
        candidates,
    }) = derived.contested
    {
        return ReceiptVerdict::Contested {
            from_state,
            candidates: candidates.iter().map(|c| c.to_state.clone()).collect(),
        };
    }
    if derived.state != receipt.terminal_state {
        return ReceiptVerdict::StateMismatch {
            claimed: receipt.terminal_state.clone(),
            derived: derived.state,
        };
    }
    if !is_terminal_state(flow, &derived.state) {
        return ReceiptVerdict::NotTerminal {
            state: derived.state,
        };
    }

    // 7. Does it speak for what the final edge's quorum committed to, and
    //    only that? Read by the same function `mint` checked it with (#1104).
    let claimed_hash = outputs_hash(&receipt.outputs);
    match final_edge_commitment(&derived, &ingested) {
        OutputsCommitment::Committed(committed) if committed == claimed_hash => {}
        OutputsCommitment::Committed(committed) => {
            return ReceiptVerdict::OutputsNotCommitted {
                claimed: receipt.outputs.iter().map(OutputRef::of).collect(),
                claimed_hash,
                committed,
            }
        }
        OutputsCommitment::Uncommitted { proposal_uri } => {
            return ReceiptVerdict::OutputsUncommitted { proposal_uri }
        }
        OutputsCommitment::Conflicting { hashes } => {
            return ReceiptVerdict::OutputsCommitmentConflict { hashes }
        }
        OutputsCommitment::NoFinalEdge => return ReceiptVerdict::NoFinalEdge,
    }
    // Committed implies a final edge, so this `else` is unreachable; it is
    // spelled out rather than unwrapped so that it stays a refusal if that
    // ever changes.
    let Some(last) = derived.settled.last() else {
        return ReceiptVerdict::NoFinalEdge;
    };
    let settled_at = last.settled_at.clone();

    let voters: BTreeSet<String> = derived
        .settled
        .iter()
        .flat_map(|edge| edge.voters.iter().cloned())
        .collect();
    ReceiptVerdict::Verified {
        terminal_state: derived.state,
        settled_at,
        outputs: receipt.outputs.iter().map(OutputRef::of).collect(),
        voters: voters.into_iter().collect(),
    }
}

impl FlowReceipt {
    /// Does this receipt speak for `node`?
    ///
    /// The `output --ad4m://flow/granted_by--> receipt` edge is a plain
    /// multi-edge anyone may write, so following one proves nothing. The
    /// binding that counts is the receipt's own [`outputs`](FlowReceipt),
    /// and this is the check that closes the loop: verify the receipt, then
    /// ask whether it names the node whose edge you followed. Either half
    /// alone is forgeable.
    ///
    /// **This reads the carried list and checks nothing itself.** It means
    /// something only after [`verify_receipt`] returned `Verified`. That step
    /// refuses any receipt whose `outputs` do not hash to the commitment the
    /// final edge's quorum signed ([`ReceiptVerdict::OutputsNotCommitted`],
    /// #1104), so on a verified receipt `true` here means the proposer named
    /// exactly this `(class, id)` as an output and every counted voter signed
    /// a commitment to its content at completion.
    pub fn speaks_for(&self, output: &OutputRef) -> bool {
        // The whole ref, never the id alone: the same node read through
        // another class is other content, and the quorum committed to the
        // content as read through THIS class (#1108 review). An id-only
        // answer would let a #1076 grant check accept a receipt whose
        // voters saw the node through a class that shows almost nothing.
        self.outputs.iter().any(|o| &OutputRef::of(o) == output)
    }
}
