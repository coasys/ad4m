//! The other end of a [receipt](super::receipt): reading one that arrived.
//!
//! [`FlowReceipt::mint`](super::receipt::FlowReceipt::mint) freezes a settled
//! run into a content-addressed artifact. This module is what somebody does
//! with it afterwards — on a different replica, months later, with the minter
//! gone.
//!
//! # What a verified receipt says
//!
//! > *n distinct eligible DIDs each signed a vote on an atom carrying this
//! > seal, and here is the material the seal was taken over.*
//!
//! Every word of that is cryptographic or re-derivable: n distinct DIDs
//! signed `acceptedBy` links on an atom carrying seal S ([`atom::signed_by`],
//! over verdicts this replica recomputed); each was eligible under the
//! carried role evidence **as of its own vote's timestamp**; and S rehashes
//! from the carried preimage.
//!
//! The protocol *requires* each of those voters to have recomputed the seal
//! against their own graph and refused to sign on mismatch ([`super::accept`]
//! is what does it on an honest client). **The receipt records that
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
//! | **That any voter actually recomputed the seal before signing** | Compliance with a protocol obligation, not a property of the artifact — see above. What a signature proves is that the signer signed *that atom, carrying that seal*. |
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

/// A verdict is one of **three** kinds of answer, not two.
///
/// The two-way split is the trap. `!is_verified()` reads "I do not hold the
/// rules" as "the quorum did not settle this" — the reader's own gap reported
/// as a finding against the receipt. The module header warns about exactly
/// that, and a boolean predicate is how a caller walks into it anyway.
///
/// So the split lives in the type and [`ReceiptVerdict::outcome`] matches
/// **exhaustively**: a new variant does not silently join a bucket, it stops
/// compiling until somebody decides which one it belongs to.
///
/// (Raised by @lal-bot-coasys reviewing this PR: three of this module's tests
/// used `is_verified()` as their discriminator, which is the idiom the doc
/// was warning against — so the doc's own warning was untested.)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerdictKind {
    /// The carried material re-derives the claim.
    Verified,
    /// A finding **about the material**: it was checked and it does not hold
    /// up. Evidence against the receipt.
    Rejected,
    /// A finding **about the reader**: this replica cannot decide, because it
    /// does not hold the rules ([`ReceiptVerdict::FlowUnknown`]) or holds
    /// different ones ([`ReceiptVerdict::DnaChanged`]).
    ///
    /// A payout system must refuse to pay on this exactly as it refuses on
    /// [`Rejected`](VerdictKind::Rejected) — and must **not** treat it as
    /// evidence against the receipt or its minter. Sync the definition and
    /// ask again; the answer may well be `Verified`.
    Undecidable,
}

impl ReceiptVerdict {
    /// Which kind of answer this is. The exhaustive match is the point; see
    /// [`VerdictKind`].
    pub fn outcome(&self) -> VerdictKind {
        match self {
            Self::Verified { .. } => VerdictKind::Verified,
            Self::FlowUnknown { .. } | Self::DnaChanged { .. } => VerdictKind::Undecidable,
            Self::NoOutputs
            | Self::SealMismatch { .. }
            | Self::Unfoldable { .. }
            | Self::Contested { .. }
            | Self::StateMismatch { .. }
            | Self::NotTerminal { .. } => VerdictKind::Rejected,
        }
    }

    /// Did the carried material re-derive the claim? Deliberately not a
    /// `From<ReceiptVerdict> for bool`: a caller that pays out on a receipt
    /// should have to name the verdict it is collapsing.
    ///
    /// **`!is_verified()` is not `is_rejected()`** — see [`VerdictKind`].
    pub fn is_verified(&self) -> bool {
        self.outcome() == VerdictKind::Verified
    }

    /// Was the receipt checked and found wanting? False for a receipt this
    /// replica could not check at all.
    pub fn is_rejected(&self) -> bool {
        self.outcome() == VerdictKind::Rejected
    }

    /// Does this answer say more about the reader than about the receipt?
    pub fn is_undecidable(&self) -> bool {
        self.outcome() == VerdictKind::Undecidable
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::flow_instance::atom::fixtures::{
        did_of, signed_link, signed_proposal, signed_vote, T1, T2, T3,
    };
    use crate::perspectives::flow_instance::atom::ACCEPTED_BY_PREDICATE;
    use crate::perspectives::flow_instance::receipt::EvidencePreimage;
    use crate::perspectives::flow_instance::roles::{RoleGrantEvidence, RoleInstanceHistory};
    use crate::perspectives::flow_instance::{ProposalLinks, ReadSet};
    use crate::types::DecoratedLinkExpression;
    use serde_json::{json, Value};

    const INSTANCE: &str = "ad4m://flow/instance/i1";
    const BASE: &str = "ad4m://task/t1";
    const ALICE: &str = "alice";
    const BOB: &str = "bob";
    const REVIEWER: &str = "coasys://Reviewer";
    /// Earlier than any grant link a test writes — the fallback dating a
    /// dropped grant link must *not* be allowed to fall back to.
    const INSTANCE_CREATED: &str = "2025-12-01T00:00:00.000Z";

    // ---- fixtures --------------------------------------------------------

    fn flow_json(states: Value, transitions: Value) -> SHACLFlow {
        serde_json::from_value(json!({
            "name": "Delivery",
            "namespace": "coasys://",
            "states": states,
            "transitions": transitions,
        }))
        .expect("fixture flow parses")
    }

    /// `open → done`, `done` terminal, default `{ n: 1 }` quorum.
    fn two_state_flow() -> SHACLFlow {
        flow_json(
            json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0 },
            ]),
            json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        )
    }

    fn catalogue(flows: Vec<SHACLFlow>) -> HashMap<String, SHACLFlow> {
        flows.into_iter().map(|f| (f.flow_uri(), f)).collect()
    }

    /// One proposal, self-proposed and therefore self-voted. `proposer` is a
    /// persona name; the links carry that persona's real signature.
    fn proposal(uri: &str, proposer: &str, to: &str, at: &str) -> ProposalLinks {
        ProposalLinks {
            uri: uri.to_string(),
            links: signed_proposal(uri, proposer, "open", to, "seal-1", at),
        }
    }

    fn read_set(proposals: Vec<ProposalLinks>, role_grants: Vec<RoleGrantEvidence>) -> ReadSet {
        ReadSet {
            instance_uri: INSTANCE.to_string(),
            subject: BASE.to_string(),
            genesis: "open".to_string(),
            proposals,
            role_grants,
        }
    }

    fn completed() -> ReadSet {
        read_set(vec![proposal("ad4m://p/1", ALICE, "done", T1)], Vec::new())
    }

    fn mint(flow: &SHACLFlow, rs: ReadSet) -> FlowReceipt {
        FlowReceipt::mint(flow, rs, vec![BASE.to_string()], Vec::new())
            .expect("the fixture read-set mints")
    }

    // ---- the happy path, as the control for everything below --------------

    /// The control: honest material, the reader's own copy of the flow, and
    /// the fold re-derives what the receipt claims — including *who* settled
    /// it, which is the "n distinct eligible DIDs" a receipt actually asserts.
    ///
    /// Red if `verify_receipt` reports `terminal_state` from the receipt
    /// rather than from its own fold — e.g. `terminal_state:
    /// receipt.terminal_state.clone()` in the `Verified` arm — because then
    /// `StateMismatch` below could never distinguish the two.
    #[test]
    fn an_honest_receipt_verifies_and_names_the_quorum_that_settled_it() {
        let flow = two_state_flow();
        let receipt = mint(&flow, completed());

        let verdict = verify_receipt(&catalogue(vec![flow]), &receipt);
        assert_eq!(
            verdict,
            ReceiptVerdict::Verified {
                terminal_state: "done".into(),
                outputs: vec![BASE.to_string()],
                voters: vec![did_of(ALICE).to_string()],
            },
            "got: {verdict}"
        );
        assert!(
            receipt.speaks_for(BASE),
            "the binding a `granted_by` edge is checked against"
        );
        assert!(
            !receipt.speaks_for("ad4m://task/somebody-elses"),
            "a receipt speaks only for the nodes it names"
        );
    }

    // ---- (a) the ratchet ---------------------------------------------------

    /// **The whole reason receipts exist.** The links a run settled on can be
    /// retracted — deleting a settling vote moves the live flow back, which is
    /// this engine's semantics and not a failure mode. A receipt froze those
    /// links, so it keeps saying what it always said, and a reader reaches
    /// that answer with no access to the graph the links came from.
    ///
    /// The first assertion is the precondition that gives the second its
    /// meaning: the same fold, over the *live* material, has already moved on.
    ///
    /// Red with `fold_read_set(flow, &ReadSet { proposals: Vec::new(),
    /// ..receipt.read_set.reverified() })` in `verify_receipt` — that is, with
    /// any implementation that treats the carried proposals as a pointer to be
    /// re-fetched rather than as the proof body itself. The receipt would then
    /// fold to `open` and report `StateMismatch`.
    #[test]
    fn a_receipt_still_verifies_after_the_links_behind_it_are_retracted() {
        let flow = two_state_flow();
        let receipt = mint(&flow, completed());

        let after_retraction = read_set(Vec::new(), Vec::new());
        assert_eq!(
            fold_read_set(&flow, &after_retraction.reverified())
                .expect("an empty read-set folds")
                .state,
            "open",
            "precondition: with the settling proposal retracted the LIVE flow stands \
             where it stood before that vote"
        );

        let verdict = verify_receipt(&catalogue(vec![flow]), &receipt);
        assert!(
            verdict.is_verified(),
            "the receipt froze the links; nothing in verification is re-queried — got: {verdict}"
        );
    }

    // ---- (b) changed DNA, refused before the evidence step ------------------

    /// Editing a flow definition edits the identity of the social organism, so
    /// a receipt minted under the old DNA *should* stop verifying — and must
    /// say so in its own words rather than as a finding about the material.
    ///
    /// The second half is the ordering contract: the receipt handed over has
    /// **both** a changed DNA and a broken seal, and `DnaChanged` still wins.
    /// Re-running a guard's seal under rules the quorum never agreed to is not
    /// a well-posed question; a verifier that answered it would report a
    /// confident wrong answer.
    ///
    /// Red if the DNA comparison moves below the seal check (or below the
    /// fold) in `verify_receipt`: the second assertion then reports
    /// `SealMismatch`.
    #[test]
    fn changed_dna_is_reported_in_its_own_words_and_before_any_evidence_step() {
        let with_rule = |n: u32| {
            flow_json(
                json!([
                    { "name": "open", "value": 0.0 },
                    { "name": "done", "value": 1.0, "consensusRule": { "n": n } },
                ]),
                json!([
                    { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
                ]),
            )
        };
        let minted_under = with_rule(1);
        let reader_holds = with_rule(2);
        assert_eq!(
            minted_under.flow_uri(),
            reader_holds.flow_uri(),
            "precondition: same organism URI, different DNA — otherwise this would be \
             FlowUnknown"
        );

        let receipt = mint(&minted_under, completed());
        let reader = catalogue(vec![reader_holds]);

        let verdict = verify_receipt(&reader, &receipt);
        assert!(
            matches!(verdict, ReceiptVerdict::DnaChanged { .. }),
            "a receipt minted under other rules is refused, not folded under the new ones"
        );
        assert_eq!(
            verdict.outcome(),
            VerdictKind::Undecidable,
            "and refused as a statement about THIS replica's rules, not as a finding \
             against the receipt — which is merely old"
        );
        assert!(!verdict.is_rejected());

        // Same receipt, additionally carrying a preimage that does not
        // re-hash. The DNA answer still comes first.
        let mut also_broken = receipt;
        also_broken.evidence_preimage.push(EvidencePreimage {
            seal: "not-the-hash-of-anything".into(),
            class_names: vec![REVIEWER.into()],
            items: Vec::new(),
        });
        let verdict = verify_receipt(&reader, &also_broken);
        assert!(
            matches!(verdict, ReceiptVerdict::DnaChanged { .. }),
            "the DNA check owes its answer before any evidence step — got: {verdict}"
        );
    }

    // ---- (c) an unknown flow is not a finding about the receipt -------------

    /// A reader who has never synced the definition has learned **nothing**
    /// about the receipt. Reporting that as a fold failure would let "I do not
    /// have the rules" be read as "the quorum did not settle this" — the
    /// receipt slandered by the reader's own gap.
    ///
    /// The last assertion is the one that pins that sentence, and it was
    /// missing until @lal-bot-coasys pointed out that this test was a mirror:
    /// naming the variant proves the variant exists, but the slander happens
    /// in the **caller**, and `!is_verified()` was true for `FlowUnknown`
    /// exactly as it is for `SealMismatch`. A caller writing
    /// `if !verdict.is_verified() { reject }` committed the slander with this
    /// test passing — and three tests in this file used that very idiom as
    /// their discriminator.
    ///
    /// Red twice over:
    /// - if the catalogue miss falls into a generic failure, e.g.
    ///   `else { return ReceiptVerdict::Unfoldable { reason: … } }` — the
    ///   `assert_eq` catches it;
    /// - if `FlowUnknown` is classified as a finding about the material, e.g.
    ///   `is_rejected` written as `!self.is_verified()` or `FlowUnknown`
    ///   moved into the `Rejected` arm of `outcome()` — which is the mutation
    ///   the doc above describes and the variant name alone could not catch.
    #[test]
    fn an_unsynced_flow_definition_is_its_own_verdict_not_a_failure() {
        let receipt = mint(&two_state_flow(), completed());

        for (label, reader) in [
            ("an empty catalogue", catalogue(Vec::new())),
            (
                "a catalogue holding only somebody else's flow",
                catalogue(vec![flow_json(
                    json!([{ "name": "start", "value": 0.0 }]),
                    json!([]),
                )
                .tap_rename("Onboarding")]),
            ),
        ] {
            let verdict = verify_receipt(&reader, &receipt);
            assert_eq!(
                verdict,
                ReceiptVerdict::FlowUnknown {
                    flow_uri: "coasys://DeliveryFlow".into()
                },
                "{label}: got {verdict}"
            );
            assert_eq!(
                verdict.outcome(),
                VerdictKind::Undecidable,
                "{label}: an un-synced definition is the reader's gap"
            );
            assert!(
                !verdict.is_verified() && !verdict.is_rejected(),
                "{label}: neither verified NOR rejected — a caller must not be able to \
                 reach `reject` through one predicate"
            );
        }
    }

    // ---- (d) a broken grant signature collapses the window ------------------

    /// A role-gated flow: only a `coasys://Reviewer` may settle `done`.
    fn role_gated_flow() -> SHACLFlow {
        flow_json(
            json!([
                { "name": "open", "value": 0.0 },
                {
                    "name": "done", "value": 1.0,
                    "consensusRule": {
                        "n": 1,
                        "fromRole": { "className": REVIEWER, "didProperty": "agent" },
                    },
                },
            ]),
            json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        )
    }

    /// `r0 --agent--> did`, the assignment link that dates a grant.
    /// `valid` is honoured cryptographically; `claims_valid` is what the
    /// carried read-set *says* about it.
    fn grant_link(who: &str, valid: bool, claims_valid: Option<bool>) -> DecoratedLinkExpression {
        signed_link("r0", "agent", did_of(who), "admin", valid, claims_valid, T2)
    }

    fn reviewer_evidence(grant: DecoratedLinkExpression) -> RoleGrantEvidence {
        RoleGrantEvidence {
            to_state: "done".into(),
            role_class: REVIEWER.into(),
            did: did_of(ALICE).into(),
            instances: vec![RoleInstanceHistory {
                instance_id: "r0".into(),
                grant_links: vec![grant],
                revocation_links: Vec::new(),
                // Earlier than the assignment link — the widening the
                // suppression rule exists to prevent.
                asserted_instance_timestamp: Some(INSTANCE_CREATED.into()),
            }],
        }
    }

    /// The fail-open direction #1063 is open on, closed at the ingest.
    ///
    /// Grant links are the one kind nothing downstream signature-checks, so a
    /// forged one has to be dropped here. Dropping alone **inverts**: with no
    /// grant link left, `RoleGrantEvidence::resolve` falls back to
    /// `asserted_instance_timestamp` — the instance's own creation, earlier
    /// than any assignment — and the forgery buys a *wider* window than the
    /// genuine link it replaced. So dropping also drops the fallback, leaving
    /// `resolve` to fail closed and abort the derivation.
    ///
    /// The receipt is minted from honest material and the grant link swapped
    /// afterwards, because that is the shape of the threat: the artifact
    /// arrives from elsewhere, already carrying what its sender chose.
    ///
    /// Red under either half of the fix:
    /// - drop `asserted_instance_timestamp: None` from `reverified_history`
    ///   (keep `history.asserted_instance_timestamp.clone()`) — the window
    ///   widens to `INSTANCE_CREATED`, the vote at `T3` becomes eligible and
    ///   the tampered receipt reports `Verified`;
    /// - write the grant filter as `.filter(|l| l.proof.valid != Some(false))`
    ///   over the *carried* links instead of `reverified_link` + `link_counts`
    ///   — the forgery's own `"valid": true` is inherited, the link survives,
    ///   and the tampered receipt reports `Verified`.
    #[test]
    fn a_forged_grant_link_collapses_the_eligibility_window_instead_of_widening_it() {
        let flow = role_gated_flow();
        let honest = read_set(
            // Vote at T3, grant at T2: eligible as of its own timestamp.
            vec![proposal("ad4m://p/1", ALICE, "done", T3)],
            vec![reviewer_evidence(grant_link(ALICE, true, None))],
        );
        let receipt = mint(&flow, honest);
        let reader = catalogue(vec![flow]);
        assert!(
            verify_receipt(&reader, &receipt).is_verified(),
            "precondition: with the genuine assignment link this receipt verifies"
        );

        // What arrives: the same receipt, its assignment link replaced by one
        // signed with somebody else's key and still claiming to be valid.
        let mut tampered = receipt;
        tampered.read_set.role_grants =
            vec![reviewer_evidence(grant_link(ALICE, false, Some(true)))];

        let verdict = verify_receipt(&reader, &tampered);
        assert!(
            verdict.is_rejected(),
            "a forged assignment link must never buy eligibility, and this is a finding \
             about the MATERIAL rather than about the reader — got: {verdict}"
        );
        let ReceiptVerdict::Unfoldable { reason } = &verdict else {
            panic!(
                "the window must COLLAPSE — an unresolvable candidate aborts the derivation \
                 rather than de-quorating one edge — got: {verdict}"
            );
        };
        assert!(
            reason.contains("cannot be placed in time"),
            "the refusal must name the fail-closed grant dating, got: {reason}"
        );
    }

    // ---- the ingest seam itself (#1068) ------------------------------------

    /// `proof.valid` is a per-replica read view over a signature. On a value
    /// that arrived from elsewhere it is the *sender's* claim, and the ingest
    /// replaces it with an answer this replica computed — in **both**
    /// directions, which is the half a "drop what claims to be invalid" filter
    /// would miss.
    ///
    /// Red if `ReadSet::reverified` clones without calling `verify_signature`,
    /// and red in the second assertion if it merely *filters* on the carried
    /// verdict instead of recomputing it.
    #[test]
    fn the_ingest_recomputes_every_carried_verdict_rather_than_inheriting_it() {
        let forged_but_claims_valid = signed_link(
            "ad4m://p/1",
            ACCEPTED_BY_PREDICATE,
            did_of(BOB),
            BOB,
            false,
            Some(true),
            T1,
        );
        let genuine_but_claims_nothing = {
            let mut l = signed_vote("ad4m://p/1", BOB, T1);
            l.proof.valid = None;
            l.proof.invalid = None;
            l
        };

        let ingested = read_set(
            vec![ProposalLinks {
                uri: "ad4m://p/1".into(),
                links: vec![forged_but_claims_valid, genuine_but_claims_nothing],
            }],
            Vec::new(),
        )
        .reverified();

        assert_eq!(
            ingested.proposals[0].links[0].proof.valid,
            Some(false),
            "a forged link's own `\"valid\": true` is the sender's word, not a fact"
        );
        assert_eq!(
            ingested.proposals[0].links[1].proof.valid,
            Some(true),
            "and a genuine link the sender never evaluated is not thereby worthless"
        );
    }

    /// The vote half of #1068, end to end: a co-signature that claims to be
    /// valid and is not must not reach quorum.
    ///
    /// `{ n: 2 }` makes Bob's vote load-bearing — Alice alone cannot settle
    /// `done` — so inheriting the forgery is the difference between a verified
    /// receipt and a refused one.
    ///
    /// Red with `fold_read_set(flow, &receipt.read_set)` in `verify_receipt`
    /// — i.e. folding the carried value rather than the re-verified one: Bob's
    /// forged `"valid": true` is inherited, quorum is reached, and the
    /// tampered receipt verifies.
    #[test]
    fn a_forged_co_signature_that_claims_to_be_valid_does_not_reach_quorum() {
        let flow = flow_json(
            json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0, "consensusRule": { "n": 2 } },
            ]),
            json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        );
        let with_bobs_vote = |vote: DecoratedLinkExpression| {
            let mut links = signed_proposal("ad4m://p/1", ALICE, "open", "done", "seal-1", T1);
            links.push(vote);
            read_set(
                vec![ProposalLinks {
                    uri: "ad4m://p/1".into(),
                    links,
                }],
                Vec::new(),
            )
        };

        let receipt = mint(&flow, with_bobs_vote(signed_vote("ad4m://p/1", BOB, T2)));
        let reader = catalogue(vec![flow]);
        assert!(
            verify_receipt(&reader, &receipt).is_verified(),
            "precondition: two genuine signatures settle a `{{n: 2}}` edge"
        );

        let mut tampered = receipt;
        tampered.read_set = with_bobs_vote(signed_link(
            "ad4m://p/1",
            ACCEPTED_BY_PREDICATE,
            did_of(BOB),
            BOB,
            false,
            Some(true),
            T2,
        ));

        // And the other side of the same coin: a minter handed that material
        // cannot produce the receipt in the first place. See
        // `mint_refuses_material_a_verifier_would_refuse` below.
        assert_eq!(
            verify_receipt(&reader, &tampered),
            ReceiptVerdict::StateMismatch {
                claimed: "done".into(),
                derived: "open".into(),
            },
            "a forged co-signature counts for nobody, so the edge never settles"
        );
    }

    /// **Marvin's constraint, made falsifiable.** Mint and verify must fold
    /// the same material; if only verify re-verifies, the two sides fold
    /// different inputs by construction. That divergence is invisible on the
    /// happy path — both sides agree on honest material — and surfaces only as
    /// a receipt that minted cleanly on one replica and fails on another,
    /// after the artifact is durable and the minter is gone.
    ///
    /// So the property is stated from the mint side: material a verifier would
    /// refuse must not mint. Bob's co-signature claims `"valid": true` and is
    /// signed with somebody else's key, `{ n: 2 }` makes it load-bearing, and
    /// so the fold stays in `open` — which the flow can still leave, so there
    /// is no completion to claim.
    ///
    /// Red with `fold_read_set(flow, &read_set)` in `FlowReceipt::mint`: the
    /// forged verdict is inherited, quorum is reached, and `mint` produces a
    /// receipt that `a_forged_co_signature_that_claims_to_be_valid_does_not_
    /// reach_quorum` shows a verifier rejects.
    #[test]
    fn mint_refuses_material_a_verifier_would_refuse() {
        let flow = flow_json(
            json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0, "consensusRule": { "n": 2 } },
            ]),
            json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        );
        let mut links = signed_proposal("ad4m://p/1", ALICE, "open", "done", "seal-1", T1);
        links.push(signed_link(
            "ad4m://p/1",
            ACCEPTED_BY_PREDICATE,
            did_of(BOB),
            BOB,
            false,
            Some(true),
            T2,
        ));
        let forged = read_set(
            vec![ProposalLinks {
                uri: "ad4m://p/1".into(),
                links,
            }],
            Vec::new(),
        );

        let err = FlowReceipt::mint(&flow, forged, vec![BASE.to_string()], Vec::new())
            .expect_err("a quorum resting on a forged signature is not a quorum");
        assert!(
            format!("{err:#}").contains("can still transition out"),
            "the fold must stay in `open` rather than counting the forgery, got: {err:#}"
        );
    }

    // ---- the remaining refusals --------------------------------------------

    /// The seal is re-hashed from the preimage **carried in the receipt** —
    /// never re-queried against a live graph. A preimage that does not
    /// reproduce its seal is not the material the voters sealed, whatever the
    /// graph says today.
    ///
    /// Red if the seal check is dropped from `verify_receipt`, which would
    /// make the carried preimage decorative.
    #[test]
    fn a_preimage_that_does_not_rehash_to_its_seal_is_refused() {
        let flow = two_state_flow();
        let mut receipt = mint(&flow, completed());
        receipt.evidence_preimage.push(EvidencePreimage {
            seal: "a-seal-nothing-here-hashes-to".into(),
            class_names: vec![REVIEWER.into()],
            items: Vec::new(),
        });

        assert_eq!(
            verify_receipt(&catalogue(vec![flow]), &receipt),
            ReceiptVerdict::SealMismatch {
                seal: "a-seal-nothing-here-hashes-to".into()
            }
        );
    }

    /// `mint` refuses an empty binding, so a receipt that arrived with one was
    /// not made by `mint` — and a verifier that accepted it would hand out a
    /// `Verified` that no `granted_by` edge could ever be checked against.
    ///
    /// Red without the `outputs.is_empty()` check in `verify_receipt`: the
    /// receipt folds perfectly well, so it would verify.
    #[test]
    fn a_receipt_that_binds_to_nothing_is_refused() {
        let flow = two_state_flow();
        let mut receipt = mint(&flow, completed());
        receipt.outputs.clear();

        assert_eq!(
            verify_receipt(&catalogue(vec![flow]), &receipt),
            ReceiptVerdict::NoOutputs
        );
    }

    /// A receipt is a completion claim. The reader's own copy of the flow
    /// decides what terminal means — add an edge out of `done` and the same
    /// carried material no longer describes a completed run.
    ///
    /// Reached only through a hand-built receipt: `mint` derives the state and
    /// refuses a non-terminal one, and the DNA hash would otherwise catch the
    /// edit first. That is the point — this arm exists for material that did
    /// not come from `mint`.
    ///
    /// Red without the `is_terminal_state` check in `verify_receipt`.
    #[test]
    fn a_state_the_readers_flow_can_leave_is_not_a_completion() {
        let minted_under = two_state_flow();
        let receipt = mint(&minted_under, completed());

        let reopenable = flow_json(
            json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0 },
            ]),
            json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
                { "action_name": "Reopen", "from_state": "done", "to_state": "open", "actions": [] },
            ]),
        );
        // Re-stamp the claim so the DNA check passes and this arm is reached.
        let mut arrived = receipt;
        arrived.flow_dna_hash = flow_dna_hash(&reopenable).expect("hash");

        assert_eq!(
            verify_receipt(&catalogue(vec![reopenable]), &arrived),
            ReceiptVerdict::NotTerminal {
                state: "done".into()
            }
        );
    }

    /// Two declared edges out of `open` both carry quorum. Nothing that pays
    /// out on a completed flow may honour a contested derivation — `mint`
    /// refuses to produce one, and a verifier refuses to accept one.
    ///
    /// Red without the `contested` arm in `verify_receipt`: the walk stops in
    /// `open`, so it would degrade to `StateMismatch` — a verdict that reads
    /// as "not settled yet" for a run that can never settle.
    #[test]
    fn a_contested_derivation_is_refused_as_contested() {
        let flow = flow_json(
            json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0 },
                { "name": "rejected", "value": 1.0 },
            ]),
            json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
                { "action_name": "Reject", "from_state": "open", "to_state": "rejected", "actions": [] },
            ]),
        );
        let receipt = mint(&flow, completed());

        let mut arrived = receipt;
        arrived.flow_dna_hash = flow_dna_hash(&flow).expect("hash");
        arrived.read_set = read_set(
            vec![
                proposal("ad4m://p/1", ALICE, "done", T1),
                proposal("ad4m://p/2", BOB, "rejected", T2),
            ],
            Vec::new(),
        );

        let verdict = verify_receipt(&catalogue(vec![flow]), &arrived);
        let ReceiptVerdict::Contested {
            from_state,
            candidates,
        } = &verdict
        else {
            panic!("a contested derivation has not completed — got: {verdict}");
        };
        assert_eq!(from_state, "open");
        assert_eq!(candidates.len(), 2, "both settled edges are named");
    }

    /// Helper for the FlowUnknown fixture: a second flow whose URI differs.
    trait Rename {
        fn tap_rename(self, name: &str) -> Self;
    }
    impl Rename for SHACLFlow {
        fn tap_rename(mut self, name: &str) -> Self {
            self.name = name.to_string();
            self
        }
    }
}
