//! The read-set: everything the engine read to decide one flow's state, as
//! a plain value, and the ingest seam (#1068) that re-verifies one that
//! arrived from elsewhere. [`fold_read_set`] is pure over it.

use super::atom::{marked_fired, TransitionAtom};
use super::fold::{fold, rule_for, DerivedState, ResolvedRule, VouchedAtom};
use super::roles::{self, eligible_votes, RoleGrant, RoleGrantEvidence};
use crate::perspectives::flow_context::FlowInstanceRecord;
use crate::perspectives::flow_evaluator::requires_query_input;
use crate::perspectives::flow_spawn::initial_state_of;
use crate::perspectives::shacl_parser::SHACLFlow;
use crate::types::{DecoratedLinkExpression, LinkExpression};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
/// One proposal as the store returned it: every verified link on it, from
/// every author, each carrying its own signature verdict and status
/// (`atom::load_proposal_links`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProposalLinks {
    pub uri: String,
    pub links: Vec<DecoratedLinkExpression>,
}

/// Everything the engine read to decide one flow's state, as a plain value.
///
/// Serialisable on purpose: this is what a minted token carries as its
/// backing, and [`fold_read_set`] over it reproduces the verdict without a
/// perspective.
///
/// Both halves are the same kind of thing: **signed links**, carried raw.
///
/// - `proposals` — every verified link of every proposal, from every author,
///   each with its own signature verdict.
/// - `role_grants` — the links that date each voter's grant and the
///   revocation tombstones that end it; tombstones are carried *before* any
///   authority filter.
///
/// No derived value travels. Grant windows, revocation times and the
/// authority rule are all recomputed by the reader
/// ([`RoleGrantEvidence::resolve`]), so a verdict's chronology — who was
/// granted when, revoked when, by whom — is re-derived from author-signed
/// material rather than asserted by whoever minted the read-set. Carrying an
/// asserted field *beside* the links was considered and rejected: a second,
/// weaker trust path is one a verifier can silently fall back to.
///
/// Exactly three residues stay asserted, and each is named where it lives:
/// that a matched instance really satisfied the role query
/// (`model_query` hydration witnesses no link — the model-query-signatures
/// gap); that a carried grant link sits on one of the rule's DID fields (the
/// reader re-checks its instance, target, author and signature, but a field
/// name maps to a predicate only through the class shape, which a pure
/// reader does not hold); and **completeness** — a minter can withhold a
/// tombstone it dislikes, which absence of a link can never disprove.
///
/// # Reading one that arrived from elsewhere
///
/// `proof.valid` on a carried link is the *minter's* claim about that link, so
/// nothing a reader decides may rest on it unchecked. The two halves of the
/// read-set are at different stages of honouring that:
///
/// - **Role evidence cannot carry a verdict at all.** It holds plain
///   [`LinkExpression`](crate::types::LinkExpression) — no `proof.valid`, no
///   `status` — and `revocation_link_counts_for_did` computes the verdict
///   from the signature on every call, inside
///   [`roles::RoleGrantEvidence::resolve`], the only path from carried
///   evidence to a window. There is no version of this call that skips the
///   check, and no field a forger could set instead (r4076927995).
/// - **Proposals and votes read the carried verdict** inside the fold, via
///   `atom::signed_by`. What makes that safe is [`ReadSet::reverified`]: the
///   ingest seam of <https://github.com/coasys/ad4m/issues/1068>, which
///   replaces every carried verdict with one this replica computed. Both
///   [`FlowReceipt::mint`](super::receipt::FlowReceipt::mint) and
///   [`verify_receipt`](super::verify::verify_receipt) fold through it, so the two
///   sides fold the same material. It remains an obligation on any *future*
///   caller that folds a read-set which arrived from elsewhere — #1074 is the
///   follow-up that makes the obligation unrepresentable by carrying the
///   links in wire form.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ReadSet {
    pub instance_uri: String,
    /// The run's base expression — the thing the flow is *about*
    /// ([`FlowInstance::subject`](super::FlowInstance::subject)). On-perspective this was ambient in
    /// [`FlowInstance::as_record`](super::FlowInstance::as_record); a read-set that travels must carry it,
    /// because translating a role query for the authority check substitutes
    /// `$flow.base` from it.
    pub subject: String,
    /// The state the walk starts from — the flow definition's first state.
    pub genesis: String,
    pub proposals: Vec<ProposalLinks>,
    /// The links behind each `(gated target state, voter)` pair's membership
    /// (#1027), unfiltered by authority. [`fold_read_set`] resolves these to
    /// [`RoleGrant`] windows and gates every vote as of its own timestamp, so
    /// a verifier re-runs both the resolution and the decision itself.
    pub role_grants: Vec<RoleGrantEvidence>,
}

impl ReadSet {
    /// The proposals that are atoms. A proposal that fails the identity
    /// checks is logged and dropped — never deleted, because a missing
    /// proposer link may still be in flight.
    pub fn atoms(&self) -> Vec<TransitionAtom> {
        self.proposals
            .iter()
            .filter_map(|p| {
                TransitionAtom::from_links(&self.instance_uri, &p.uri, &p.links)
                    .map_err(|reason| {
                        log::debug!(
                            "flow instance {}: proposal {} is not an atom — {reason}",
                            self.instance_uri,
                            p.uri
                        )
                    })
                    .ok()
            })
            .collect()
    }

    /// The flat record the role translator takes, rebuilt from carried
    /// fields alone — which is why [`ReadSet::subject`] exists. Must agree
    /// with [`FlowInstance::as_record`](super::FlowInstance::as_record) field for field: the same role query
    /// has to translate identically on and off a perspective, or a verifier
    /// would apply a different authority rule than the minter did.
    pub fn as_record(&self, flow: &SHACLFlow) -> FlowInstanceRecord {
        FlowInstanceRecord {
            flow_uri: flow.flow_uri(),
            instance_uri: self.instance_uri.clone(),
            subject: self.subject.clone(),
            current_state: self.genesis.clone(),
            created_at: None,
        }
    }

    /// Proposal URIs already carrying **this replica's** `Local`
    /// `resolved_as → "fired"` mark. Bookkeeping for [`pass`](super::pass), never an
    /// input to the fold; a peer's `Shared` mark is not counted.
    pub fn marked_proposals(&self) -> HashSet<String> {
        self.proposals
            .iter()
            .filter(|p| marked_fired(&p.links))
            .map(|p| p.uri.clone())
            .collect()
    }

    /// The same read-set with **every carried link's per-replica read view
    /// replaced by this replica's own** — the ingest seam of #1068.
    ///
    /// Two fields on a carried link are read views rather than properties of
    /// the link, so on a read-set that arrived from elsewhere both are the
    /// *sender's* claim:
    ///
    /// - `proof.valid` — **recomputed**. A sender who writes `"valid": true`
    ///   onto an unsigned vote gets it counted toward quorum by any reader
    ///   that folds the value as handed over. The claim is replaced by an
    ///   answer this replica computed from the signature.
    /// - `status` — **cleared**, because locality is not a signable property
    ///   and so cannot be recomputed at all. See [`reverified_link`] for why
    ///   `None` is the honest answer and what invariant that places on
    ///   locality gates.
    ///
    /// This function is the one place either claim is answered.
    ///
    /// Pure — [`DecoratedLinkExpression::verify_signature`] is SHA256 plus an
    /// Ed25519 check against the author's own `did:key`: no store, no clock,
    /// no network.
    ///
    /// # Both sides run it, and that is the contract
    ///
    /// [`FlowReceipt::mint`](super::receipt::FlowReceipt::mint) and
    /// [`verify_receipt`](super::verify::verify_receipt) both fold
    /// `read_set.reverified()`, never the raw value. A verify-only rule would
    /// have the two sides fold different inputs by construction: invisible on
    /// the happy path, and surfacing only as a receipt that minted cleanly on
    /// one replica and fails on another — after the artifact is durable and
    /// the minter is gone. On the minting replica the links came from the
    /// local store and already carry a locally computed verdict, so the
    /// signature half is a no-op there; it costs one signature check per
    /// carried link.
    ///
    /// The `status` half is **not** a no-op on the minting replica — it
    /// discards real local marks. That is intended and is what keeps the two
    /// sides folding the same input: a fold that read `status` would give the
    /// minter an answer no verifier could ever reproduce, since the verifier
    /// has no locality to read. Nothing in [`fold_read_set`] reads `status`;
    /// the one gate that does, [`atom::marked_fired`](super::atom::marked_fired), is bookkeeping for
    /// [`pass`](super::pass) and is deliberately reached through the **raw** read-set, not
    /// through this one.
    ///
    /// # The three halves are treated differently, and each for a reason
    ///
    /// - **Proposal and vote links are re-decorated, not dropped.**
    ///   [`atom::signed_by`](super::atom::signed_by) already demands `proof.valid == Some(true)`, so a
    ///   link whose signature does not check out counts for nobody once the
    ///   verdict is honest. Dropping would be the same answer with less to
    ///   look at — and a proposal carries links from *every* author, so a
    ///   third party who writes a garbage link onto someone else's proposal
    ///   must not be able to make the whole receipt unverifiable.
    /// - **Role evidence only loses its locality claim.** Dating links and
    ///   tombstones are plain [`LinkExpression`] — no verdict field a sender
    ///   could set — and [`RoleGrantEvidence::resolve`] computes the verdict
    ///   from the signature on every call
    ///   ([`GrantDating`](super::roles::dating::GrantDating),
    ///   [`revocation_link_counts_for_did`](crate::perspectives::flow_evaluator)),
    ///   so that call is the unskippable check.
    pub fn reverified(&self) -> ReadSet {
        ReadSet {
            instance_uri: self.instance_uri.clone(),
            subject: self.subject.clone(),
            genesis: self.genesis.clone(),
            proposals: self
                .proposals
                .iter()
                .map(|p| ProposalLinks {
                    uri: p.uri.clone(),
                    links: p.links.iter().map(reverified_link).collect(),
                })
                .collect(),
            role_grants: self
                .role_grants
                .iter()
                .map(|evidence| RoleGrantEvidence {
                    to_state: evidence.to_state.clone(),
                    role_class: evidence.role_class.clone(),
                    did: evidence.did.clone(),
                    instances: evidence.instances.iter().map(reverified_history).collect(),
                })
                .collect(),
        }
    }
}

/// One carried link with its signature verdict recomputed from the signature
/// and its locality claim discarded.
///
/// Never reads the carried `proof.valid`; see [`ReadSet::reverified`].
///
/// # Why `status` is cleared rather than recomputed
///
/// `proof.valid` and `status` are both per-replica read views rather than
/// properties of the link, so a carried value of either is the *sender's*
/// claim. They part company on what can be done about it. A signature is a
/// property of the link's own bytes, so the claim can be **replaced** with an
/// answer computed here. Locality cannot: `Local` means "this link is in my
/// store and was never published", a fact about storage that nothing in the
/// link is or could be signed over. There is no computation that recovers it
/// from a value in flight.
///
/// So the only truthful carried locality is *unknown*, and `None` is how this
/// type says that. Keeping the sender's `Some(Local)` would let a peer assert
/// "this is your own local mark" about a link this replica has never stored —
/// the same shape of forgery as `"valid": true` on an unsigned vote, and the
/// reason this seam has to answer both.
///
/// **Invariant this depends on: every locality gate tests `== Some(Local)`,
/// never `!= Some(Shared)`.** The two agree on `Some(Local)` and `Some(Shared)`
/// and differ exactly on `None`, where the second form reads *unknown* as
/// *local* — a fail-open wearing the shape of a fail-closed, the same trap
/// [`link_counts`] documents one field over. Today's gates
/// ([`atom::marked_fired`](super::atom::marked_fired), [`pass::local_cached_state`](super::pass)) both use the
/// safe form.
///
/// This is only reachable because this PR made [`ReadSet`] travel; before
/// that, every link in one had come off the local store.
fn reverified_link(link: &DecoratedLinkExpression) -> DecoratedLinkExpression {
    let mut link = link.clone();
    link.verify_signature();
    link.status = None;
    link
}

/// Does a re-decorated link count?
///
/// **`== Some(true)`, never `!= Some(false)`.** The two forms differ on
/// `None`, which means *never evaluated* — precisely the state this gate
/// exists to reject. `!= Some(false)` is a fail-open wearing the shape of a
/// fail-closed: it admits an unevaluated link, and on a value that arrived
/// over the wire `None` is one field a sender omits.
fn link_counts(link: &DecoratedLinkExpression) -> bool {
    link.proof.valid == Some(true)
}

/// One role instance's carried history, stripped of its locality claims.
///
/// Nothing is dropped here: [`RoleGrantEvidence::resolve`] recomputes every
/// signature and applies every filter itself, and a link it rejects can only
/// leave a grant undated, never dated earlier.
///
/// A carried `produced_at` passes through untouched: it is a date, not a
/// link, and there is nothing here to re-check it against. What it is worth
/// to a reader of a serialised read-set is stated in
/// [`grant`](super::grant) § *What a receipt of a gated flow proves*.
fn reverified_history(history: &roles::RoleInstanceHistory) -> roles::RoleInstanceHistory {
    let delocalized_all = |links: &[LinkExpression]| links.iter().map(delocalized).collect();
    roles::RoleInstanceHistory {
        instance_id: history.instance_id.clone(),
        grant_links: delocalized_all(&history.grant_links),
        grantees_own_links: delocalized_all(&history.grantees_own_links),
        revocation_links: delocalized_all(&history.revocation_links),
        produced_at: history.produced_at.clone(),
    }
}

/// The `status` half of [`reverified_link`], for the role-evidence halves of
/// the read-set: those carry plain [`LinkExpression`] with no verdict field
/// to recompute (the type refuses to carry one — see
/// [`roles::RoleInstanceHistory`]), but locality is still a carried claim and
/// is discarded for exactly the reasons [`reverified_link`] gives.
fn delocalized(link: &LinkExpression) -> LinkExpression {
    let mut link = link.clone();
    link.status = None;
    link
}

/// The state of a flow, re-derived from a read-set. **Pure** — no store
/// access, no role queries, no clock.
///
/// Two steps, both re-runnable by anyone holding the value:
///
/// 1. [`role_grant_views`] resolves the carried links into [`RoleGrant`]
///    windows, applying the authority rule from *the flow definition passed
///    in here* rather than any rule the minter applied.
/// 2. [`eligible_votes`] gates each atom's votes as of their own timestamps
///    against those windows, and the fold walks the pre-filtered atoms.
///
/// The fold itself does no role work; every eligibility decision is visible
/// in the read-set before it runs.
///
/// Before either step: **the walk starts where this flow definition starts,
/// never where the read-set says it does.** `genesis` on a carried read-set
/// is the minter's word, and folding from it would honour a walk planted at
/// (or one edge short of) a terminal state — every quorum before the planted
/// genesis simply skipped, up to a "completion" with an empty voter list
/// (r4077689141). Every honest producer ([`FlowInstance::read_set`](super::FlowInstance::read_set)) writes
/// [`initial_state_of`] here, so the only read-sets this refuses are ones no
/// honest producer built. The check lives at this seam rather than in
/// [`FlowReceipt::mint`](super::receipt::FlowReceipt::mint) and
/// [`verify_receipt`](super::verify::verify_receipt) separately so that it also
/// holds for [`counted_seals`](super::receipt::FlowReceipt::counted_seals) and any
/// future caller, rather than resting on each call site (the #1078 lesson).
///
/// This is the function an off-perspective verifier re-runs over a minted
/// token's proof to reach the same verdict independently — after
/// re-decorating the carried links' signatures, per [`ReadSet`].
pub fn fold_read_set(flow: &SHACLFlow, read_set: &ReadSet) -> anyhow::Result<DerivedState> {
    let Some(initial) = initial_state_of(flow) else {
        anyhow::bail!(
            "fold_read_set: flow `{}` has no states, so {} has no genesis to fold from",
            flow.flow_uri(),
            read_set.instance_uri
        );
    };
    if read_set.genesis != initial {
        anyhow::bail!(
            "fold_read_set: {} claims genesis `{}`, but flow `{}` starts in `{}`; a walk \
             folded from a carried genesis would skip every quorum before it, so it is \
             refused",
            read_set.instance_uri,
            read_set.genesis,
            flow.flow_uri(),
            initial
        );
    }
    let grants = role_grant_views(flow, read_set)?;
    let vouched: Vec<VouchedAtom> = read_set
        .atoms()
        .into_iter()
        .map(|atom| {
            // An unreadable rule leaves no vote eligible. `settle_edge`
            // refuses the edge anyway; emptying the set here means the
            // refusal also holds for anything reading `eligible_votes`
            // directly, rather than resting on one call site (#1078).
            let eligible_votes = match rule_for(flow, &atom.to_state) {
                ResolvedRule::Rule(rule) => eligible_votes(&atom, &rule, &grants),
                ResolvedRule::Refused => Vec::new(),
            };
            VouchedAtom {
                eligible_votes,
                atom,
            }
        })
        .collect();
    Ok(fold(&read_set.genesis, flow, &vouched))
}

/// Resolve every carried [`RoleGrantEvidence`] into the [`RoleGrant`] view
/// the gate consumes. Pure, and **fail-closed for the whole fold**: evidence
/// that cannot be resolved — an untranslatable role query, an instance no
/// carried link can place in time — aborts the derivation. The caller
/// abandons the read exactly as it does when `read_set` itself fails.
///
/// It used to drop the candidate and fold on, reasoning that a candidate with
/// no view contributes no eligible votes ("no grant, no vote") and so a
/// dropped candidate can only ever *narrow* eligibility. That is true of
/// eligibility and false of the **outcome**, because the outcome is decided by
/// vote counts: [`fold::Contention`](super::fold) only fires when two edges out of
/// the same state are both quorate. De-quorate one of them by dropping a
/// candidate and the walk stops contending and TAKES the survivor — an edge
/// fires that the same read-set with the same rules would never have derived.
/// Fail-closed for eligibility, fail-OPEN for the transition. One unresolved
/// candidate must not be able to pick a winner.
///
/// Evidence for a state whose rule carries no `fromRole` is dropped: an
/// ungated edge admits every vote regardless, and resolving it would only
/// invite a reader to think the gate meant something.
///
/// The `producedByFlow` gate, like the authority rule, is read from **the
/// flow definition passed in here** and never from the carried evidence: a
/// minter who could name the granting flow would be naming the rule its own
/// receipt is judged by.
fn role_grant_views(flow: &SHACLFlow, read_set: &ReadSet) -> anyhow::Result<Vec<RoleGrant>> {
    let record = read_set.as_record(flow);
    let mut grants = Vec::with_capacity(read_set.role_grants.len());
    for evidence in &read_set.role_grants {
        // A refused rule is not the fail-open drop warned about above: it bars
        // the edge for every voter (`fold_read_set` empties `eligible_votes`),
        // so evidence targeting that state cannot sway any outcome.
        let ResolvedRule::Rule(rule) = rule_for(flow, &evidence.to_state) else {
            continue;
        };
        let Some(role) = rule.from_role.as_ref() else {
            continue;
        };
        let view = requires_query_input(role, &record, &evidence.did)
            .and_then(|input| evidence.resolve(&input, role))
            .map_err(|e| {
                e.context(format!(
                    "flow instance {}: role evidence for `{}` on `{}` does not resolve, so no \
                     verdict can be derived from this read-set",
                    read_set.instance_uri, evidence.did, evidence.to_state
                ))
            })?;
        grants.push(view);
    }
    Ok(grants)
}
