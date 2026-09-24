//! What a reader learned from a receipt: the vocabulary a consumer
//! matches on, kept apart from the algorithm that produces it.

use crate::perspectives::flow_instance::atom::OutputRef;
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
        /// The outputs this receipt speaks for, as `(class, id)`: the refs of
        /// `receipt.outputs`, whose content hashes to the `outputs_hash` the
        /// final edge's quorum signed (any difference is
        /// [`ReceiptVerdict::OutputsNotCommitted`]). The binding to check
        /// before honouring a `granted_by` edge; see
        /// [`FlowReceipt::speaks_for`](crate::perspectives::flow_instance::receipt::FlowReceipt::speaks_for).
        outputs: Vec<OutputRef>,
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
    /// The receipt names no output, so it speaks for nothing and no
    /// `granted_by` edge can be honoured by it. `mint` refuses to produce
    /// such a receipt.
    NoOutputs,
    /// The walk reached the terminal state without settling any edge, so no
    /// quorum committed to anything.
    NoFinalEdge,
    /// An atom counted on the final edge carries no `outputs_hash`: its
    /// voters agreed to no outputs, so nothing can be bound to the run.
    OutputsUncommitted { proposal_uri: String },
    /// Atoms counted on the final edge carry different `outputs_hash`
    /// values (sorted). Each voter agreed only to their own atom's outputs,
    /// so no set was agreed by the whole quorum. Strict: there is no
    /// intersection.
    OutputsCommitmentConflict { hashes: Vec<String> },
    /// The receipt's output preimages do not hash to the `outputs_hash` the
    /// final edge's quorum signed. This is the #1104 re-mint (genuine signed
    /// material, and outputs the minter chose), and also a receipt carrying
    /// an output's content other than what the quorum committed to.
    OutputsNotCommitted {
        /// The refs of `receipt.outputs`.
        claimed: Vec<OutputRef>,
        /// [`outputs_hash`](crate::perspectives::flow_instance::atom::outputs_hash) of `receipt.outputs`, content included.
        claimed_hash: String,
        /// The `outputs_hash` on the final edge's counted atoms.
        committed: String,
    },
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
            | Self::NoFinalEdge
            | Self::OutputsUncommitted { .. }
            | Self::OutputsCommitmentConflict { .. }
            | Self::OutputsNotCommitted { .. }
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
                "the receipt names no output, so it speaks for nothing and no `granted_by` edge \
                 could be honoured by it"
            ),
            Self::NoFinalEdge => write!(
                f,
                "the walk settled no edge, so no quorum committed to any output"
            ),
            Self::OutputsUncommitted { proposal_uri } => write!(
                f,
                "proposal {proposal_uri}, counted on the final edge, carries no outputs \
                 commitment, so its voters agreed to no outputs"
            ),
            Self::OutputsCommitmentConflict { hashes } => write!(
                f,
                "the atoms counted on the final edge commit to different outputs {hashes:?}, so \
                 no set of outputs was agreed by the whole quorum"
            ),
            Self::OutputsNotCommitted {
                claimed,
                claimed_hash,
                committed,
            } => write!(
                f,
                "the receipt claims outputs {claimed:?} (hash `{claimed_hash}`), but the final \
                 edge's quorum committed to `{committed}`"
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
