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
//! Three rules do all the work:
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
//! - **The URI is the fields.** A vote signs nothing but the proposal URI,
//!   so the URI is the content address of every field above
//!   ([`proposal_uri`], recomputed in [`TransitionAtom::from_links`],
//!   #1108). Without it the *proposer* could do what the second rule stops
//!   a peer from doing: retract and re-sign `outputs_hash` or the seal
//!   under the voted URI after the co-signs landed.

pub mod outputs;
pub mod uri;

pub use outputs::{
    check_outputs_commitment, normalised_outputs, outputs_hash, OutputRef, OutputsRefusal,
};
pub use uri::proposal_uri;

use super::time::parse_link_timestamp;
use crate::perspectives::flow_classes::FLOW_TRANSITION_PROPOSAL_CLASS;
use crate::perspectives::model_query::utils::parse_literal_value;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{DecoratedLinkExpression, LinkQuery, LinkStatus};
use outputs::named_outputs;
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
/// Proposal → [`outputs_hash`] over the **content** of the instances the
/// proposer names as the run's outputs. Written only on a proposal **into a
/// terminal state**, next to the evidence seal. `literal:string:`-encoded.
/// Every voter loads each named instance on its own replica and recomputes it
/// before co-signing (`super::accept`), and a receipt's output preimages must
/// hash to it (`super::verify`, #1104).
pub const OUTPUTS_HASH_PREDICATE: &str = "ad4m://flow/outputs_hash";
/// Proposal → one instance the proposer names as an output of the run, as an
/// [`OutputRef`]. One link per output. The target is a `literal:string:` whose
/// text is the canonical JSON `{"className":"…","id":"…"}`
/// ([`OutputRef::encode`]); a pair, not a bare id, because an output is an
/// instance **of a class** and its content is read through that class's
/// shape. Only the proposer's own signed links count, like every other field
/// of an atom, and one that does not parse rejects the atom
/// ([`AtomRejection::MalformedOutput`]).
pub const OUTPUT_PREDICATE: &str = "ad4m://flow/output";
/// Domain tag of [`outputs_hash`]. Versioned: v1 hashed ids only (#1108 v2),
/// v2 hashes instance content.
pub const OUTPUTS_HASH_TAG: &str = "ad4m-flow-outputs/v2";
/// Proposal → the proposer's uniqueness salt for this proposal's
/// content-addressed URI. `literal:string:`-encoded. Any string the proposer
/// can defend as unique (the engine writes a UUID); it exists so one
/// proposer can open two proposals whose other fields agree (a re-propose
/// after a retraction, a deliberate twin). Signed by the proposer like every
/// other field, and part of the [`proposal_uri`] preimage.
pub const PROPOSAL_NONCE_PREDICATE: &str = "ad4m://flow/nonce";
/// Domain tag of a proposal's content-addressed URI ([`proposal_uri`]).
pub const PROPOSAL_URI_TAG: &str = "ad4m-flow-proposal-uri/v1";
/// Proposal → a voting DID. A vote counts only when the link's author IS the
/// DID it names, with a valid signature (see [`valid_votes`]).
pub const ACCEPTED_BY_PREDICATE: &str = "ad4m://acceptedBy";
/// Proposal → bookkeeping about how the engine already recorded it.
/// **Never an input to the fold** — see [`marked_fired`].
pub const RESOLVED_AS_PREDICATE: &str = "ad4m://flow/resolved_as";
/// The only [`RESOLVED_AS_PREDICATE`] value this engine writes.
pub const FIRED_MARK: &str = "fired";
/// Tombstone: role instance → revoked DID. Written instead of deleting the
/// grant; the link's author-asserted timestamp is the revocation time. Source =
/// role-instance URI, target = the DID — literal-encoded or raw, the reader
/// accepts both (`flow_evaluator::target_names_did`). Authority mirrors the
/// grant's own rule (`roles::revocation_authorised`): whoever the role
/// query's `author` condition accepts as granter may revoke, which makes
/// admin-gated roles admin-revoked, `$did` roles self-revoked, and
/// open-authorship roles revocable by anyone. Stays in the graph forever so
/// newcomers can reconstruct the full role-membership history.
pub const ROLE_GRANT_REVOKED_PREDICATE: &str = "ad4m://flow/role_grant_revoked";

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
    /// The proposer's [`OUTPUTS_HASH_PREDICATE`] value. `None` when the
    /// proposer signed none, which is correct for a proposal into a
    /// non-terminal state and a refusal reason for one into a terminal state
    /// ([`check_outputs_commitment`]).
    pub outputs_hash: Option<String>,
    /// The instances the proposer named with [`OUTPUT_PREDICATE`], sorted and
    /// deduplicated. What a voter loads, re-hashes and checks against
    /// `outputs_hash`; not read by the fold or by `verify_receipt`.
    pub outputs: Vec<OutputRef>,
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
    /// None of the proposer's own signed links carries an RFC 3339-parseable
    /// timestamp, so the proposal cannot be placed in time (#1000). Fail
    /// closed: a proposal that cannot be dated must not sort anywhere.
    NoParseableTimestamp,
    /// One of the proposer's own [`OUTPUT_PREDICATE`] links is not an
    /// encoded [`OutputRef`]. Rejecting the atom, rather than skipping the
    /// link, keeps a voter from co-signing outputs it could not read.
    MalformedOutput(String),
    /// The proposal's URI is not the content address of its own
    /// proposer-signed fields ([`proposal_uri`]). Either a field was re-signed
    /// after the URI was fixed — the post-co-sign swap this check exists to
    /// refuse — or the proposal predates content-addressed URIs (a pre-#1108
    /// UUID URI), which is rejected the same way: a vote on such a URI covers
    /// nothing.
    UriMismatch {
        /// [`proposal_uri`] over the fields as the proposer currently signs
        /// them.
        expected: String,
    },
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
            Self::NoParseableTimestamp => write!(
                f,
                "none of the proposer's own links carries an RFC 3339-parseable timestamp"
            ),
            Self::MalformedOutput(text) => write!(
                f,
                "the proposer's `{OUTPUT_PREDICATE}` value `{text}` is not a \
                 {{\"className\", \"id\"}} pair"
            ),
            Self::UriMismatch { expected } => write!(
                f,
                "its URI is not the content address of its proposer-signed fields \
                 (they address `{expected}`), so a vote on it covers nothing"
            ),
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
/// earliest kept, ordered by parsed instant (`did`, then the original
/// string, break the remaining ties).
///
/// Ordering compares **parsed instants**, never timestamp strings: the
/// timestamp is client-asserted and clients disagree on RFC 3339 flavour,
/// so string order is client-library order inside a sub-second collision
/// (#1000, and [`super::time`]). A vote whose timestamp does not parse is
/// dropped loudly — it cannot be placed in time, and it must not become
/// "earliest" by being garbage.
///
/// A vote is an authorship claim, not a data claim — otherwise one agent
/// could write `acceptedBy → did:key:X` links for DIDs it does not control
/// and clear any `{n}` quorum alone.
pub fn valid_votes(
    links: &[DecoratedLinkExpression],
    proposer: &str,
    proposed_at: &str,
) -> Vec<Vote> {
    let mut votes: Vec<(chrono::DateTime<chrono::Utc>, Vote)> = std::iter::once(Vote {
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
    .filter_map(|v| match parse_link_timestamp(&v.at) {
        Some(instant) => Some((instant, v)),
        None => {
            log::warn!(
                "flow: dropping vote by `{}` — timestamp `{}` is not RFC 3339, so it cannot be ordered",
                v.did,
                v.at
            );
            None
        }
    })
    .collect();
    // Keep one vote per DID, the earliest — grouping by DID first, because
    // `dedup_by` only ever drops neighbours.
    votes.sort_by(|(a_at, a), (b_at, b)| (&a.did, a_at, &a.at).cmp(&(&b.did, b_at, &b.at)));
    votes.dedup_by(|(_, later), (_, kept)| later.did == kept.did);
    votes.sort_by(|(a_at, a), (b_at, b)| (a_at, &a.did, &a.at).cmp(&(b_at, &b.did, &b.at)));
    votes.into_iter().map(|(_, v)| v).collect()
}

/// Whether **this replica** marked this proposal fired: a `Local`
/// `resolved_as → "fired"` link. Marks are per-replica bookkeeping (#987), so
/// a peer's mark is not a mark here — a forged one cannot mute this replica's
/// once-only [`FireOutcome`](super::pass::FireOutcome).
///
/// # "This replica's" is only true of links this replica read
///
/// That guarantee used to rest on a peer's mark arriving `Shared`, which held
/// while every [`ReadSet`](super::ReadSet) was built from the local store. A
/// read-set now travels, and `status` is not signed, so on a *carried*
/// read-set `Some(Local)` means only "whoever sent this claimed `Local`" —
/// not an answer about this replica at all.
///
/// The gap is closed one layer up rather than here:
/// [`reverified_link`](super::read_set::reverified_link) clears `status` on every
/// carried link, so a re-verified read-set answers `false` for every
/// proposal. That is the truthful answer — this replica has marked nothing it
/// never read — and it is why this function must keep testing
/// `== Some(Local)` rather than `!= Some(Shared)`, which would read the
/// cleared value as a mark and hand the sender the forgery back.
///
/// Callers therefore get a meaningful answer only from a locally read
/// read-set, which is the only place [`pass`](super::pass) uses it.
///
/// **Bookkeeping only.** The fold never reads it: it exists so a UI can list
/// history and so the consensus pass knows which edges it has already
/// recorded. A forged mark therefore moves nothing in either direction — it
/// cannot fabricate history, and it cannot hide a proposal from the fold.
pub fn marked_fired(links: &[DecoratedLinkExpression]) -> bool {
    links_on(links, RESOLVED_AS_PREDICATE)
        .any(|l| l.status == Some(LinkStatus::Local) && field_value(&l.data.target) == FIRED_MARK)
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
        // Optional, because only a proposal into a terminal state carries
        // one. Two distinct proposer values are still a rejection: "pick one"
        // would let the proposer show different voters different outputs.
        let outputs_hash = match unique_field(links, OUTPUTS_HASH_PREDICATE, &proposer) {
            Ok(hash) => Some(hash),
            Err(AtomRejection::MissingField(_)) => None,
            Err(other) => return Err(other),
        };
        let from_state = unique_field(links, FROM_STATE_PREDICATE, &proposer)?;
        let to_state = unique_field(links, TO_STATE_PREDICATE, &proposer)?;
        // The vote-covers-fields invariant (#1108). A vote is
        // `uri --acceptedBy--> did` and signs nothing but the URI, so the
        // URI must be the content address of every field read above — or
        // the proposer could re-sign `outputs_hash` (or the seal) under the
        // voted URI after the co-signs landed, and the swapped value would
        // read as quorum-agreed. Recomputed here, on every read, from the
        // proposer's own signed fields; a mismatch — including every
        // pre-#1108 random-UUID proposal — is not an atom, so no vote on it
        // is ever counted.
        let nonce = unique_field(links, PROPOSAL_NONCE_PREDICATE, &proposer)?;
        let expected = proposal_uri(
            instance_uri,
            &from_state,
            &to_state,
            &evidence_hash,
            outputs_hash.as_deref(),
            &proposer,
            &nonce,
        );
        if uri != expected {
            return Err(AtomRejection::UriMismatch { expected });
        }
        let proposed_at = earliest_proposer_timestamp(links, &proposer)
            .ok_or(AtomRejection::NoParseableTimestamp)?;
        Ok(TransitionAtom {
            uri: uri.to_string(),
            from_state,
            to_state,
            votes: valid_votes(links, &proposer, &proposed_at),
            outputs: named_outputs(links, &proposer)?,
            outputs_hash,
            proposed_at,
            proposer,
            evidence_hash,
        })
    }
}

/// Earliest timestamp among the proposer's own signed links, by parsed
/// instant — string `min()` would pick by client format inside a sub-second
/// collision (#1000). Links whose timestamp does not parse are skipped;
/// `None` means no proposer link could be placed in time at all, which
/// [`TransitionAtom::from_links`] turns into
/// [`AtomRejection::NoParseableTimestamp`].
fn earliest_proposer_timestamp(
    links: &[DecoratedLinkExpression],
    proposer: &str,
) -> Option<String> {
    links
        .iter()
        .filter(|l| signed_by(l, proposer))
        .filter_map(|l| parse_link_timestamp(&l.timestamp).map(|dt| (dt, l.timestamp.clone())))
        .min()
        .map(|(_, ts)| ts)
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
/// **Half 1 — class query (changed in #990, `cda1d95ea`):** a `model_query` over the
/// hard-wired `FlowTransitionProposal` subject class, filtered by
/// `flowInstance == instance_uri`, yields the URIs of every proposal that
/// belongs to this instance. Using the class query rather than a raw
/// `get_links` scopes the result to properly-typed proposals and reuses the
/// machinery already validated by `flow_evaluator`'s `run_query`.
///
/// This narrows the *pointer* set, not the atom set. Class conformance emits a
/// triple per required property, so a half-written proposal carrying only
/// `flowInstance` is no longer discovered — but [`TransitionAtom::from_links`]
/// already rejected that shape with `MissingField`, so the fold never saw it
/// either way. The `where` is exists-style (`?source <ad4m://flow/instance>
/// …`) and this function reads only `instances[].id`, never the hydrated
/// `flowInstance` value, so a third-party re-point cannot drop a real proposal
/// from discovery — that is the same attack half 2 refuses to hydrate.
///
/// **Half 2 — raw `get_links` per proposal (must stay raw):** model_query
/// hydration collapses each instance to a single `author` field (the earliest
/// author across all links, `model_query/hydration.rs:171-183,350`) and
/// carries no per-link signature verdict — its hydrated record is
/// `(predicate, target, author, timestamp)`, and `proof.valid` is never in it.
/// Both identity checks below run through [`signed_by`], which needs both
/// dropped fields at once: `l.author == did` **and** `proof.valid ==
/// Some(true)`. Worse than lossy, hydrating would *invert* [`unique_field`]:
/// scalar properties last-write-win on timestamp with no author filter, so a
/// later third-party `to_state` becomes the hydrated value while `author`
/// stays the earliest DID — the forgery would be served back as the
/// proposer's own word. This half cannot go away until instances carry
/// per-property `(author, proof.valid)`.
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
mod tests {
    use super::super::test_support::*;
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

    /// A proposal none of whose proposer links can be placed in time must
    /// not enter the fold at all — an undatable atom would otherwise sort
    /// arbitrarily against every dated one (#1000).
    #[test]
    fn a_proposal_with_no_parseable_timestamp_is_rejected_by_name() {
        let links = honest_proposal(ALICE, "review", "approved", "h1", "not-a-timestamp");
        assert_eq!(
            atom_of(&links),
            Err(AtomRejection::NoParseableTimestamp),
            "an undatable proposal is refused, not sorted"
        );
    }

    /// A mark is this replica's own bookkeeping, so only a `Local` link is
    /// one. A peer's mark arrives `Shared` — however well signed — and must
    /// not count, or a forged mark could mute this replica's once-only
    /// `FireOutcome` (#987). The mark never reaches the fold either way.
    #[test]
    fn only_a_local_fired_mark_is_a_mark() {
        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        assert!(!marked_fired(&links));

        let mut peer_mark = link(
            RESOLVED_AS_PREDICATE,
            &literal(FIRED_MARK),
            MALLORY,
            true,
            T3,
        );
        peer_mark.status = Some(LinkStatus::Shared);
        links.push(peer_mark);
        assert!(
            !marked_fired(&links),
            "a peer's shared mark is not this replica's mark"
        );

        let mut own_mark = link(RESOLVED_AS_PREDICATE, &literal(FIRED_MARK), ALICE, true, T3);
        own_mark.status = Some(LinkStatus::Local);
        links.push(own_mark);
        assert!(marked_fired(&links), "our own local mark is");

        let mut other_value = link(RESOLVED_AS_PREDICATE, &literal("rejected"), ALICE, true, T3);
        other_value.status = Some(LinkStatus::Local);
        assert!(
            !marked_fired(&[other_value]),
            "only the `fired` value marks a proposal fired"
        );
    }

    /// `only_a_local_fired_mark_is_a_mark` rests on a sentence that stopped
    /// being true when this PR made `ReadSet` travel: "a peer's mark arrives
    /// `Shared`". It arrives however the sender wrote it. `status` is not
    /// signed and locality is not recomputable — it is a fact about *whose
    /// store a link sits in* — so on a carried read-set `Some(Local)` says
    /// only "the sender claimed `Local`".
    ///
    /// The seam answers it the one way it can: `reverified_link` clears
    /// `status`, so a carried mark is not this replica's mark no matter what
    /// it asserts. Note the fixture forges nothing else — the mark is validly
    /// signed by Mallory and survives the signature half of `reverified`
    /// untouched. Only the locality claim is dropped.
    ///
    /// Killing mutation: delete `link.status = None;` from `reverified_link`.
    /// Every other test in the crate stays green — `marked_proposals` has one
    /// production caller (`pass`, on this replica's own store), so this is
    /// latent rather than live, and latent is exactly what an assertion is
    /// for. Also red if `marked_fired` is rewritten to `!= Some(Shared)`,
    /// which reads the cleared value as a mark and hands the claim back.
    #[test]
    fn a_carried_local_mark_is_not_this_replicas_mark() {
        use super::super::{ProposalLinks, ReadSet};

        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        let mut forged_mark = link(
            RESOLVED_AS_PREDICATE,
            &literal(FIRED_MARK),
            MALLORY,
            true,
            T3,
        );
        forged_mark.status = Some(LinkStatus::Local);
        links.push(forged_mark);

        let arrived = ReadSet {
            instance_uri: "flow://instance".into(),
            subject: "subject://base".into(),
            genesis: "review".into(),
            proposals: vec![ProposalLinks {
                uri: "proposal://p1".into(),
                links,
            }],
            role_grants: vec![],
        };

        assert!(
            arrived.marked_proposals().contains("proposal://p1"),
            "precondition: read as handed over, the sender's claim IS taken as \
             our mark — this is the forgery the seam exists to answer"
        );
        assert!(
            arrived.reverified().marked_proposals().is_empty(),
            "a carried `Local` mark must not count as this replica's: we have \
             marked nothing we never read"
        );
    }
}
