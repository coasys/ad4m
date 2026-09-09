//! What counts as a proposal, and who counts as having signed it.
//!
//! Nothing here decides anything about a flow's state — this module only
//! answers "is this pile of links a proposal, and whose words are in it?".
//! The answer is a [`TransitionAtom`]: a proposal every one of whose fields
//! was read **only from links the proposer themselves signed**. A reader who
//! trusts that sentence never has to open this file; [`fold`](super::fold)
//! is where the decisions live.
//!
//! **A [`TransitionAtom`] carries no eligibility verdict.** It records every
//! vote that carries a valid self-signature — it does not know, and does not
//! ask, whether those voters were eligible to vote under the rule's `fromRole`
//! gate. That check happens in [`super::roles::resolve_role_grants`], called
//! from [`super::FlowInstance::read_set`], and the result lands in
//! [`super::ReadSet::role_grants`]. [`Vote`] is the struct that carries a
//! timestamp; it is not an eligibility claim.  Reviewers who expect to find
//! the role gate inside `TransitionAtom` or `from_links` will not find it —
//! and that is by design: keeping the two concerns separate is what lets the
//! fold be pure.
//!
//! Two rules do all the work:
//!
//! - **Identity is a signature check.** [`signed_by`] is the only place in
//!   the flow engine that compares authors, and it requires the stored
//!   signature verdict to be valid as well. A synced link may *claim* any
//!   author; the executor records `proof.valid = Some(false)` when the bytes
//!   do not verify, and such a link counts for nobody.
//! - **No last-write-wins.** [`unique_field`] reads a field only from the
//!   proposer's own links and rejects the atom outright when the proposer
//!   published two different values. Anyone else's link on that predicate is
//!   invisible, so a peer cannot re-point someone else's proposal by
//!   appending a later value — the trick model hydration would fall for.

use crate::perspectives::flow_classes::FLOW_TRANSITION_PROPOSAL_CLASS;
use crate::perspectives::model_query::utils::parse_literal_value;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{DecoratedLinkExpression, LinkQuery};
use serde::{Deserialize, Serialize};

// ---------------------------------------------------------------------------
// The link vocabulary of a flow proposal. One home, because the flow engine
// is the only thing that reads it.
// ---------------------------------------------------------------------------

/// Proposal → the `FlowInstance` URI it proposes a transition for.
pub const FLOW_INSTANCE_PREDICATE: &str = "ad4m://flow/instance";
/// Proposal → the state the transition leaves. `literal:string:`-encoded.
pub const FROM_STATE_PREDICATE: &str = "ad4m://flow/from_state";
/// Proposal → the state the transition enters. `literal:string:`-encoded.
pub const TO_STATE_PREDICATE: &str = "ad4m://flow/to_state";
/// Proposal → the proposer's DID. Raw (DIDs are URIs, not literals).
pub const PROPOSER_PREDICATE: &str = "ad4m://flow/proposer";
/// Proposal → the evidence seal computed at mint. `literal:string:`-encoded.
pub const EVIDENCE_HASHES_PREDICATE: &str = "ad4m://flow/evidence_hashes";
/// Proposal → a voting DID. A vote counts only when the link's author IS the
/// DID it names, with a valid signature (see [`valid_votes`]).
pub const ACCEPTED_BY_PREDICATE: &str = "ad4m://acceptedBy";
/// Proposal → bookkeeping about how the engine already recorded it.
/// **Never an input to the fold** — see [`marked_fired`].
pub const RESOLVED_AS_PREDICATE: &str = "ad4m://flow/resolved_as";
/// The only [`RESOLVED_AS_PREDICATE`] value this engine writes.
pub const FIRED_MARK: &str = "fired";

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// One signed vote: who cast it, and the timestamp of the link they signed.
///
/// The timestamp is inside the signature, so an agent can back-date their
/// own vote and nobody else's — the same power a proposer already has over
/// their own proposal.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Vote {
    pub did: String,
    pub at: String,
}

/// One identity-checked, sealed proposal together with its valid votes.
///
/// Every field came from a link the proposer signed, so a third party cannot
/// change what an atom says without forging the proposer's key.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TransitionAtom {
    pub uri: String,
    pub from_state: String,
    pub to_state: String,
    pub proposer: String,
    /// Earliest timestamp among the **proposer's own** signed links on this
    /// proposal. Third-party links are excluded so a peer cannot re-order
    /// someone else's atom by back-dating a link onto it.
    pub proposed_at: String,
    /// Non-empty by construction ([`AtomRejection::EmptySeal`] otherwise).
    /// The fold never re-runs the guard behind it; every voter checked the
    /// seal on their own replica before signing (`super::accept`).
    pub evidence_hash: String,
    /// The proposer's own vote plus every self-authored `acceptedBy`,
    /// one per DID, earliest first.
    pub votes: Vec<Vote>,
}

/// Why a proposal is not an atom. Logged and tested; never acted on by
/// deleting — this engine deletes nothing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AtomRejection {
    /// No `proposer → d` link signed by `d` itself.
    NoValidProposerLink,
    /// Two DIDs each self-claim to be the proposer.
    AmbiguousProposer,
    /// The proposer signed no link on this predicate.
    MissingField(&'static str),
    /// The proposer signed two distinct values for this predicate.
    AmbiguousField(&'static str),
    /// The proposer's `flow/instance` link names a different instance.
    WrongInstance,
    /// The proposer sealed the proposal with an empty evidence hash, which
    /// no verifier can reproduce.
    EmptySeal,
}

impl std::fmt::Display for AtomRejection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoValidProposerLink => write!(
                f,
                "no `{PROPOSER_PREDICATE}` link signed by the DID it names"
            ),
            Self::AmbiguousProposer => write!(f, "two DIDs each self-claim to be the proposer"),
            Self::MissingField(p) => write!(f, "the proposer signed no `{p}` link"),
            Self::AmbiguousField(p) => write!(f, "the proposer signed two distinct `{p}` values"),
            Self::WrongInstance => write!(
                f,
                "the proposer's `{FLOW_INSTANCE_PREDICATE}` link names a different flow instance"
            ),
            Self::EmptySeal => write!(f, "the evidence seal is empty"),
        }
    }
}

// ---------------------------------------------------------------------------
// Identity primitives (pure)
// ---------------------------------------------------------------------------

/// The one identity check in the flow engine: this link was authored by
/// `did` **and** its stored signature verdict is valid.
pub fn signed_by(link: &DecoratedLinkExpression, did: &str) -> bool {
    link.author == did && link.proof.valid == Some(true)
}

/// The value a link carries, `literal:string:` decoded. URI targets (DIDs,
/// instance URIs) pass through unchanged.
fn field_value(target: &str) -> String {
    match parse_literal_value(target) {
        serde_json::Value::String(s) => s,
        other => other.to_string(),
    }
}

fn links_on<'l>(
    links: &'l [DecoratedLinkExpression],
    predicate: &'l str,
) -> impl Iterator<Item = &'l DecoratedLinkExpression> + 'l {
    links
        .iter()
        .filter(move |l| l.data.predicate.as_deref() == Some(predicate))
}

/// The unique DID `d` carrying a `proposer → d` link signed by `d` itself.
///
/// Self-authorship is the whole point: a proposal that merely *claims* a
/// proposer is a proposal anyone could have written in that DID's name.
pub fn self_authored_proposer(links: &[DecoratedLinkExpression]) -> Result<String, AtomRejection> {
    let mut dids: Vec<String> = links_on(links, PROPOSER_PREDICATE)
        .filter(|l| signed_by(l, &l.data.target))
        .map(|l| l.data.target.clone())
        .collect();
    dids.sort();
    dids.dedup();
    match dids.len() {
        0 => Err(AtomRejection::NoValidProposerLink),
        1 => Ok(dids.remove(0)),
        _ => Err(AtomRejection::AmbiguousProposer),
    }
}

/// The single value `author` published on `predicate`.
///
/// Links by anyone else are invisible here — that is what makes an atom's
/// content unforgeable by a third party. Two distinct proposer-signed values
/// are a rejection, never "pick the latest".
pub fn unique_field(
    links: &[DecoratedLinkExpression],
    predicate: &'static str,
    author: &str,
) -> Result<String, AtomRejection> {
    let mut values: Vec<String> = links_on(links, predicate)
        .filter(|l| signed_by(l, author))
        .map(|l| field_value(&l.data.target))
        .collect();
    values.sort();
    values.dedup();
    match values.len() {
        0 => Err(AtomRejection::MissingField(predicate)),
        1 => Ok(values.remove(0)),
        _ => Err(AtomRejection::AmbiguousField(predicate)),
    }
}

/// Every vote on this proposal: the proposer's own, cast at `proposed_at`,
/// plus each `acceptedBy → a` link signed by `a`. One vote per DID, the
/// earliest kept, sorted by `(at, did)`.
///
/// A vote is an authorship claim, not a data claim — otherwise one agent
/// could write `acceptedBy → did:key:X` links for DIDs it does not control
/// and clear any `{n}` quorum alone.
pub fn valid_votes(
    links: &[DecoratedLinkExpression],
    proposer: &str,
    proposed_at: &str,
) -> Vec<Vote> {
    let mut votes: Vec<Vote> = std::iter::once(Vote {
        did: proposer.to_string(),
        at: proposed_at.to_string(),
    })
    .chain(
        links_on(links, ACCEPTED_BY_PREDICATE)
            .filter(|l| signed_by(l, &l.data.target))
            .map(|l| Vote {
                did: l.data.target.clone(),
                at: l.timestamp.clone(),
            }),
    )
    .collect();
    // Keep one vote per DID, the earliest — grouping by DID first, because
    // `dedup_by` only ever drops neighbours.
    votes.sort_by(|a, b| (&a.did, &a.at).cmp(&(&b.did, &b.at)));
    votes.dedup_by(|later, kept| later.did == kept.did);
    votes.sort_by(|a, b| (&a.at, &a.did).cmp(&(&b.at, &b.did)));
    votes
}

/// Whether some agent whose signature verifies marked this proposal fired.
///
/// **Bookkeeping only.** The fold never reads it: it exists so a UI can list
/// history and so the consensus pass knows which edges it has already
/// recorded. A forged mark therefore moves nothing in either direction — it
/// cannot fabricate history, and it cannot hide a proposal from the fold.
pub fn marked_fired(links: &[DecoratedLinkExpression]) -> bool {
    links_on(links, RESOLVED_AS_PREDICATE)
        .any(|l| l.proof.valid == Some(true) && field_value(&l.data.target) == FIRED_MARK)
}

impl TransitionAtom {
    /// Build an atom from one proposal's raw links, or say why it is not one.
    /// `links` is every link with the proposal as source, from every author —
    /// the filtering is the point of this function.
    pub fn from_links(
        instance_uri: &str,
        uri: &str,
        links: &[DecoratedLinkExpression],
    ) -> Result<TransitionAtom, AtomRejection> {
        let proposer = self_authored_proposer(links)?;
        if unique_field(links, FLOW_INSTANCE_PREDICATE, &proposer)? != instance_uri {
            return Err(AtomRejection::WrongInstance);
        }
        let evidence_hash = unique_field(links, EVIDENCE_HASHES_PREDICATE, &proposer)?;
        if evidence_hash.is_empty() {
            return Err(AtomRejection::EmptySeal);
        }
        let proposed_at = earliest_proposer_timestamp(links, &proposer);
        Ok(TransitionAtom {
            uri: uri.to_string(),
            from_state: unique_field(links, FROM_STATE_PREDICATE, &proposer)?,
            to_state: unique_field(links, TO_STATE_PREDICATE, &proposer)?,
            votes: valid_votes(links, &proposer, &proposed_at),
            proposed_at,
            proposer,
            evidence_hash,
        })
    }
}

/// Earliest timestamp among the proposer's own signed links. Never empty in
/// practice: the proposer link that named them is one of these.
fn earliest_proposer_timestamp(links: &[DecoratedLinkExpression], proposer: &str) -> String {
    links
        .iter()
        .filter(|l| signed_by(l, proposer))
        .map(|l| l.timestamp.clone())
        .min()
        .unwrap_or_default()
}

/// Enumerate one instance's proposals and read each one's links.
///
/// **The dividing principle.** The halves split exactly where hydration starts
/// destroying what the caller needs. Half 1 only has to know that a proposal
/// *exists* and belongs to this instance — a fact the model layer preserves
/// exactly, so discovery belongs there. Half 2 has to know *who wrote each
/// individual field, and whether that link's signature verified* — per-link
/// facts hydration collapses away, so field-reading has to stay on raw links.
///
/// **Half 1 — class query (this PR's change):** a `model_query` over the
/// hard-wired `FlowTransitionProposal` subject class, filtered by
/// `flowInstance == instance_uri`, yields the URIs of every proposal that
/// belongs to this instance. Using the class query rather than a raw
/// `get_links` scopes the result to properly-typed proposals and reuses the
/// machinery already validated by `flow_evaluator`'s `run_query`.
///
/// **Half 2 — raw `get_links` per proposal (must stay raw):** model_query
/// hydration collapses each instance to a single `author` field (the earliest
/// author across all links, `model_query/hydration.rs:171-183,350`) and
/// carries no per-link signature verdict. The identity checks below need
/// exactly those two fields on every individual link: `signed_by` requires
/// `proof.valid == Some(true)`, and `unique_field` reads only links where
/// `l.author == proposer`. Hydrating the proposals would silently break both
/// checks — a third-party forgery that model_query would collapse into the
/// proposer's own hydrated value would pass undetected. This half cannot go
/// away until the fold is rewritten to work on hydrated instances rather than
/// raw link-level proofs.
pub async fn load_proposal_links(
    perspective: &PerspectiveInstance,
    instance_uri: &str,
) -> anyhow::Result<Vec<(String, Vec<DecoratedLinkExpression>)>> {
    if instance_uri.is_empty() {
        return Err(anyhow::anyhow!(
            "load_proposal_links: instance_uri must not be empty"
        ));
    }

    // Half 1: discover which FlowTransitionProposal instances belong to this
    // flow instance via the subject-class query layer.
    let query_json = serde_json::json!({ "where": { "flowInstance": instance_uri } }).to_string();
    let raw = perspective
        .model_query(FLOW_TRANSITION_PROPOSAL_CLASS, &query_json)
        .await
        .map_err(|e| anyhow::anyhow!("load_proposal_links: model_query failed: {e:#}"))?;
    let result: serde_json::Value = serde_json::from_str(&raw).map_err(|e| {
        anyhow::anyhow!("load_proposal_links: model_query returned invalid JSON: {e:#}")
    })?;
    let mut uris: Vec<String> = result
        .get("instances")
        .and_then(serde_json::Value::as_array)
        .ok_or_else(|| {
            anyhow::anyhow!("load_proposal_links: model_query returned no `instances` array")
        })?
        .iter()
        .filter_map(|inst| {
            inst.get("id")
                .and_then(serde_json::Value::as_str)
                .map(str::to_string)
        })
        .collect();
    uris.sort();
    uris.dedup();

    // Half 2: raw get_links per proposal — see doc comment for why this half
    // must stay raw rather than using model_query hydration.
    let mut out = Vec::with_capacity(uris.len());
    for uri in uris {
        let links = perspective
            .get_links(&LinkQuery {
                source: Some(uri.clone()),
                ..Default::default()
            })
            .await
            .map_err(|e| anyhow::anyhow!("load_proposal_links: get_links({uri}) failed: {e:#}"))?;
        out.push((uri, links));
    }
    Ok(out)
}

#[cfg(test)]
pub(super) mod fixtures {
    use super::*;
    use crate::types::{DecoratedExpressionProof, Link};

    pub const INSTANCE: &str = "ad4m://flow/instance/i1";
    pub const PROPOSAL: &str = "ad4m://flow/proposal/p1";
    pub const ALICE: &str = "did:key:alice";
    pub const BOB: &str = "did:key:bob";
    pub const MALLORY: &str = "did:key:mallory";
    pub const T1: &str = "2026-01-01T00:00:00.000Z";
    pub const T2: &str = "2026-01-02T00:00:00.000Z";
    pub const T3: &str = "2026-01-03T00:00:00.000Z";

    pub fn literal(s: &str) -> String {
        format!("literal:string:{}", urlencoding::encode(s))
    }

    /// One link as `get_links` returns it. Author, signature verdict and
    /// timestamp are all inputs the checks read, so every fixture states
    /// them explicitly.
    pub fn link(
        predicate: &str,
        target: &str,
        author: &str,
        valid: bool,
        timestamp: &str,
    ) -> DecoratedLinkExpression {
        DecoratedLinkExpression {
            author: author.to_string(),
            timestamp: timestamp.to_string(),
            data: Link {
                source: PROPOSAL.to_string(),
                predicate: Some(predicate.to_string()),
                target: target.to_string(),
            },
            proof: DecoratedExpressionProof {
                key: format!("{author}#key"),
                signature: "sig".to_string(),
                valid: Some(valid),
                invalid: Some(!valid),
            },
            status: None,
        }
    }

    /// The five links `write_flow_transition_proposal` emits, all signed by
    /// the proposer.
    pub fn honest_proposal(
        proposer: &str,
        from: &str,
        to: &str,
        seal: &str,
        at: &str,
    ) -> Vec<DecoratedLinkExpression> {
        vec![
            link(PROPOSER_PREDICATE, proposer, proposer, true, at),
            link(FLOW_INSTANCE_PREDICATE, INSTANCE, proposer, true, at),
            link(FROM_STATE_PREDICATE, &literal(from), proposer, true, at),
            link(TO_STATE_PREDICATE, &literal(to), proposer, true, at),
            link(
                EVIDENCE_HASHES_PREDICATE,
                &literal(seal),
                proposer,
                true,
                at,
            ),
        ]
    }

    pub fn atom_of(links: &[DecoratedLinkExpression]) -> Result<TransitionAtom, AtomRejection> {
        TransitionAtom::from_links(INSTANCE, PROPOSAL, links)
    }
}

#[cfg(test)]
mod tests {
    use super::fixtures::*;
    use super::*;

    #[test]
    fn an_engine_minted_proposal_is_an_atom_and_its_proposer_is_its_first_voter() {
        let atom = atom_of(&honest_proposal(ALICE, "review", "approved", "h1", T1))
            .expect("engine-minted proposal must be an atom");
        assert_eq!(atom.proposer, ALICE);
        assert_eq!(atom.from_state, "review");
        assert_eq!(atom.to_state, "approved");
        assert_eq!(atom.evidence_hash, "h1");
        assert_eq!(atom.proposed_at, T1);
        assert_eq!(
            atom.votes,
            vec![Vote {
                did: ALICE.into(),
                at: T1.into()
            }],
            "minting is voting: the proposer votes at the moment they propose"
        );
    }

    /// Test 1. The `"test-signature"` shape: a link that claims a DID over
    /// bytes that do not verify. Identity is a signature check, so such a
    /// link is neither a proposer claim nor a vote — an `author ==`
    /// comparison would accept both.
    #[test]
    fn an_invalid_signature_is_not_a_vote_or_a_proposer() {
        let mut unsigned_proposer = honest_proposal(ALICE, "review", "approved", "h1", T1);
        unsigned_proposer[0] = link(PROPOSER_PREDICATE, ALICE, ALICE, false, T1);
        assert_eq!(
            atom_of(&unsigned_proposer),
            Err(AtomRejection::NoValidProposerLink),
            "a proposer link whose signature does not verify is not that DID's proposal"
        );

        let mut forged_votes = honest_proposal(ALICE, "review", "approved", "h1", T1);
        // (a) self-named, signature invalid — the synced-garbage shape.
        forged_votes.push(link(ACCEPTED_BY_PREDICATE, BOB, BOB, false, T2));
        // (b) validly signed, but by someone other than the DID it names.
        forged_votes.push(link(
            ACCEPTED_BY_PREDICATE,
            "did:key:carol",
            MALLORY,
            true,
            T2,
        ));
        assert_eq!(
            atom_of(&forged_votes).expect("atom").votes,
            vec![Vote {
                did: ALICE.into(),
                at: T1.into()
            }],
            "neither shape may add a voter"
        );
    }

    /// Test 2. Mallory appends a LATER `to_state` to Alice's proposal: model
    /// hydration is last-timestamp-wins across authors, so a hydrating
    /// engine would read Mallory's value while still attributing the
    /// proposal to Alice. Only Alice's own links are visible here — and when
    /// *Alice* publishes two values, the atom is rejected rather than
    /// resolved, because "pick the latest" is the rule this module removes.
    #[test]
    fn a_foreign_later_link_cannot_repoint_a_proposal() {
        let mut foreign = honest_proposal(ALICE, "review", "approved", "h1", T1);
        foreign.push(link(
            TO_STATE_PREDICATE,
            &literal("rejected"),
            MALLORY,
            true,
            T3,
        ));
        assert_eq!(
            atom_of(&foreign)
                .expect("a foreign link must not invalidate the atom")
                .to_state,
            "approved",
            "only the proposer's own value may be read"
        );

        let mut ambiguous = honest_proposal(ALICE, "review", "approved", "h1", T1);
        ambiguous.push(link(
            TO_STATE_PREDICATE,
            &literal("rejected"),
            ALICE,
            true,
            T3,
        ));
        assert_eq!(
            atom_of(&ambiguous),
            Err(AtomRejection::AmbiguousField(TO_STATE_PREDICATE)),
            "two proposer values is a rejection, never last-write-wins"
        );
    }

    /// Test 3. `proposed_at` is the fold's tie-break key and a vote's default
    /// timestamp, so a third party must not be able to move it: Mallory adds
    /// an earlier-stamped link to Alice's proposal and it changes nothing.
    #[test]
    fn a_foreign_backdated_link_cannot_reorder_a_proposal() {
        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T2);
        links.push(link(
            ACCEPTED_BY_PREDICATE,
            MALLORY,
            MALLORY,
            true,
            "2020-01-01T00:00:00.000Z",
        ));
        let atom = atom_of(&links).expect("atom");
        assert_eq!(
            atom.proposed_at, T2,
            "only the proposer's links date an atom"
        );
        assert_eq!(
            atom.votes,
            vec![
                Vote {
                    did: MALLORY.into(),
                    at: "2020-01-01T00:00:00.000Z".into()
                },
                Vote {
                    did: ALICE.into(),
                    at: T2.into()
                },
            ],
            "Mallory may back-date their OWN vote, and only their own"
        );
    }

    /// One DID votes once, however many `acceptedBy` links they sign; the
    /// earliest is the one that counts, so a re-vote cannot move a settle
    /// time later.
    #[test]
    fn one_did_votes_once_and_the_earliest_vote_is_the_one_kept() {
        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        links.push(link(ACCEPTED_BY_PREDICATE, BOB, BOB, true, T3));
        links.push(link(ACCEPTED_BY_PREDICATE, BOB, BOB, true, T2));
        // Alice re-votes explicitly, later than her own mint.
        links.push(link(ACCEPTED_BY_PREDICATE, ALICE, ALICE, true, T3));
        assert_eq!(
            atom_of(&links).expect("atom").votes,
            vec![
                Vote {
                    did: ALICE.into(),
                    at: T1.into()
                },
                Vote {
                    did: BOB.into(),
                    at: T2.into()
                },
            ]
        );
    }

    /// The rejections that keep unverifiable proposals out of the fold. An
    /// empty seal is the sharpest: it would otherwise count toward quorum
    /// with no evidence at all.
    #[test]
    fn a_proposal_missing_its_own_words_is_rejected_by_name() {
        let mut without_to_state = honest_proposal(ALICE, "review", "approved", "h1", T1);
        without_to_state.retain(|l| l.data.predicate.as_deref() != Some(TO_STATE_PREDICATE));
        assert_eq!(
            atom_of(&without_to_state),
            Err(AtomRejection::MissingField(TO_STATE_PREDICATE))
        );

        assert_eq!(
            atom_of(&honest_proposal(ALICE, "review", "approved", "", T1)),
            Err(AtomRejection::EmptySeal)
        );

        let mut wrong_instance = honest_proposal(ALICE, "review", "approved", "h1", T1);
        wrong_instance[1] = link(
            FLOW_INSTANCE_PREDICATE,
            "ad4m://flow/instance/other",
            ALICE,
            true,
            T1,
        );
        assert_eq!(atom_of(&wrong_instance), Err(AtomRejection::WrongInstance));

        let mut two_proposers = honest_proposal(ALICE, "review", "approved", "h1", T1);
        two_proposers.push(link(PROPOSER_PREDICATE, MALLORY, MALLORY, true, T3));
        assert_eq!(
            atom_of(&two_proposers),
            Err(AtomRejection::AmbiguousProposer)
        );
    }

    /// The mark is read from any author with a valid signature, because it is
    /// an index rather than an authority — and it never reaches the fold.
    #[test]
    fn a_fired_mark_is_readable_but_needs_a_valid_signature() {
        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        assert!(!marked_fired(&links));
        links.push(link(
            RESOLVED_AS_PREDICATE,
            &literal(FIRED_MARK),
            MALLORY,
            false,
            T3,
        ));
        assert!(!marked_fired(&links), "an unverifiable mark is not a mark");
        links.push(link(
            RESOLVED_AS_PREDICATE,
            &literal(FIRED_MARK),
            MALLORY,
            true,
            T3,
        ));
        assert!(marked_fired(&links));
    }
}
