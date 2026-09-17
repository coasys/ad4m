//! The other end of a [receipt](super::receipt): reading one that arrived.
//!
//! [`FlowReceipt::mint`](super::receipt::FlowReceipt::mint) freezes a settled
//! run into a content-addressed artifact. This module is what somebody does
//! with it afterwards — on a different replica, months later, with the minter
//! gone.
//!
//! # What a verified receipt says
//!
//! > *n distinct eligible DIDs each recomputed this seal on their own replica
//! > and refused on mismatch, and here is what they saw.*
//!
//! Not "the author of that flow instance signed it" — instance authorship is
//! never checked and never carried. Author-signed guard evidence stays
//! deferred on the model-query signature gap
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
//! One row is added by this module rather than settled:
//!
//! | Not proven | Why |
//! | --- | --- |
//! | **That every counted atom's seal is inspectable** | `mint` accepts an empty `evidence_preimage`, so a receipt may carry none. Verification checks every preimage it *is* given and requires none. Demanding one per counted seal would reject receipts mint produced — the asymmetry that makes receipts fail their own verification. Tightening it belongs on the mint side first. |
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
//!     ▼                                                  NotTerminal
//!    Verified
//! ```
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
//! # The untrusted boundary is here
//!
//! A receipt is the first thing in this engine that arrives from off-replica,
//! which makes this the seam where a carried `proof.valid` — the *minter's*
//! claim about a link — must stop being inherited. [`ReadSet::reverified`](super::ReadSet::reverified)
//! does that, and both sides of the artifact run it. See its doc for why the
//! filter is spelled `== Some(true)` and never `!= Some(false)`, and for why
//! a broken grant signature collapses an eligibility window rather than
//! widening one.

use super::fold::Contention;
use super::fold_read_set;
use super::receipt::{flow_dna_hash, is_terminal_state, FlowReceipt};
use crate::perspectives::shacl_parser::SHACLFlow;
use std::collections::{BTreeSet, HashMap};

/// What a reader learned from a receipt.
///
/// Every non-[`Verified`](ReceiptVerdict::Verified) variant names its own
/// reason rather than collapsing into one failure, because the reasons are
/// not the same kind of thing: two of them say nothing about the receipt at
/// all (the reader lacks the rules, or holds different ones), and the rest
/// are findings about the carried material.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReceiptVerdict {
    /// The carried material folds, under the reader's own copy of the flow,
    /// to exactly the terminal state the receipt claims.
    Verified {
        /// The state the reader's own fold reached — equal to
        /// `receipt.terminal_state`, re-derived rather than read.
        terminal_state: String,
        /// The nodes this receipt speaks for, as carried. The binding to
        /// check before honouring a `granted_by` edge; see
        /// [`FlowReceipt::speaks_for`].
        outputs: Vec<String>,
        /// The distinct eligible DIDs that made up the quorum on every
        /// settled edge of the walk, sorted. This is the "n distinct DIDs"
        /// in what a receipt claims — the list a reader counts.
        voters: Vec<String>,
    },
    /// The reader's catalogue holds no flow by that URI. **Not a finding
    /// about the receipt**: an un-synced definition is the reader's gap.
    FlowUnknown { flow_uri: String },
    /// The reader holds a definition by that URI whose content hash differs.
    /// **Not a finding about the receipt** either — a receipt minted under
    /// the old DNA *should* stop verifying once the DNA is edited, because
    /// editing it makes the space a different social organism.
    DnaChanged {
        /// `flow_dna_hash` as the receipt carries it.
        claimed: String,
        /// `flow_dna_hash` of the definition the reader holds.
        held: String,
    },
    /// The receipt binds to no node, so no `granted_by` edge could ever be
    /// honoured by it. `mint` refuses to produce one; a receipt that arrived
    /// carrying an empty `outputs` was not made by `mint`.
    NoOutputs,
    /// A carried preimage does not re-hash to the seal it claims, so it is
    /// not the material the voters sealed.
    SealMismatch { seal: String },
    /// The fold refused to derive anything. Fail-closed by construction:
    /// carried role evidence that cannot be resolved aborts the derivation
    /// rather than dropping a candidate, which would de-quorate an edge and
    /// let the walk take a survivor contention would have held.
    Unfoldable { reason: String },
    /// Two declared edges out of the same state both carry quorum. Nothing
    /// that pays out on a completed flow may honour a contested derivation.
    Contested {
        from_state: String,
        candidates: Vec<String>,
    },
    /// The fold reached a different state than the receipt claims.
    StateMismatch { claimed: String, derived: String },
    /// The fold reached the claimed state, but the reader's definition can
    /// still transition out of it — a completion claim for a run that has
    /// not completed.
    NotTerminal { state: String },
}

impl ReceiptVerdict {
    /// Sugar for the one question most callers have. Deliberately not a
    /// `From<ReceiptVerdict> for bool`: a caller that pays out on a receipt
    /// should have to name the verdict it is collapsing.
    pub fn is_verified(&self) -> bool {
        matches!(self, ReceiptVerdict::Verified { .. })
    }
}

impl std::fmt::Display for ReceiptVerdict {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Verified {
                terminal_state,
                voters,
                ..
            } => write!(
                f,
                "verified: settled into `{terminal_state}` by {} distinct eligible DID(s)",
                voters.len()
            ),
            Self::FlowUnknown { flow_uri } => write!(
                f,
                "flow `{flow_uri}` is not in this catalogue, so nothing about this receipt could \
                 be decided — sync the definition and ask again"
            ),
            Self::DnaChanged { claimed, held } => write!(
                f,
                "this receipt was minted under flow DNA `{claimed}`; this replica holds `{held}`. \
                 A run settled under one organism's rules is not re-decidable under another's"
            ),
            Self::NoOutputs => write!(
                f,
                "the receipt binds to no node, so it speaks for nothing and no `granted_by` edge \
                 could be honoured by it"
            ),
            Self::SealMismatch { seal } => write!(
                f,
                "the carried preimage for seal `{seal}` does not re-hash to it, so it is not the \
                 material the voters sealed"
            ),
            Self::Unfoldable { reason } => {
                write!(f, "the carried material does not fold: {reason}")
            }
            Self::Contested {
                from_state,
                candidates,
            } => write!(
                f,
                "the derivation is contested in `{from_state}` ({} settled edges out of it), so \
                 the run has not completed",
                candidates.len()
            ),
            Self::StateMismatch { claimed, derived } => write!(
                f,
                "the receipt claims `{claimed}` but the carried material folds to `{derived}`"
            ),
            Self::NotTerminal { state } => write!(
                f,
                "`{state}` is not terminal in this replica's copy of the flow, so the receipt is \
                 not a completion claim under these rules"
            ),
        }
    }
}

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
pub fn verify_receipt(
    catalogue: &HashMap<String, SHACLFlow>,
    receipt: &FlowReceipt,
) -> ReceiptVerdict {
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

    // 3. Does it bind to anything? `mint` refuses an empty binding; a receipt
    //    that arrived with one did not come from `mint`.
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

    // 5. The same fold, over the same ingest, that `mint` ran.
    let derived = match fold_read_set(flow, &receipt.read_set.reverified()) {
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

    let voters: BTreeSet<String> = derived
        .settled
        .iter()
        .flat_map(|edge| edge.voters.iter().cloned())
        .collect();
    ReceiptVerdict::Verified {
        terminal_state: derived.state,
        outputs: receipt.outputs.clone(),
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
    pub fn speaks_for(&self, node: &str) -> bool {
        self.outputs.iter().any(|o| o == node)
    }
}
