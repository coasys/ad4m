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
use crate::perspectives::flow_classes::{
    FLOW_TRANSITION_PROPOSAL_CLASS, PROPOSAL_EVIDENCE_PREDICATE,
};
use crate::perspectives::model_query::utils::parse_literal_value;
use crate::perspectives::model_query::LINKS_KEY;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{DecoratedLinkExpression, LinkStatus};
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
    /// Build an atom from one proposal's links, or say why it is not one.
    /// `links` are the proposal's links from every author, one per link with
    /// its own author and verdict, never a hydrated instance — the filtering
    /// is the point of this function.
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

/// Every predicate a proposal's links are read on: each property of the
/// `FlowTransitionProposal` class, plus the two links the class does not
/// declare, the votes ([`ACCEPTED_BY_PREDICATE`]) and this replica's fired mark
/// ([`RESOLVED_AS_PREDICATE`]).
///
/// The fields an atom reads are all here, and so is every other predicate the
/// engine writes on a proposal, because [`TransitionAtom::proposed_at`] is the
/// earliest of the proposer's own links on *any* predicate. A link on a
/// predicate outside this list is not read. Only the proposer's own links can
/// date a proposal, so the one thing such a link could have moved is a
/// timestamp the proposer already sets. `proposal_link_predicates_cover_the_class`
/// ties the list to the class's SDNA.
pub const PROPOSAL_LINK_PREDICATES: [&str; 13] = [
    FLOW_INSTANCE_PREDICATE,
    FROM_STATE_PREDICATE,
    TO_STATE_PREDICATE,
    PROPOSER_PREDICATE,
    PROPOSAL_EVIDENCE_PREDICATE,
    EVIDENCE_HASHES_PREDICATE,
    OUTPUT_PREDICATE,
    OUTPUTS_HASH_PREDICATE,
    PROPOSAL_NONCE_PREDICATE,
    "ad4m://flow/run_uri",
    "ad4m://flow/rationale",
    ACCEPTED_BY_PREDICATE,
    RESOLVED_AS_PREDICATE,
];

/// Enumerate one instance's proposals and read each one's links, in one
/// `model_query`.
///
/// The query selects the `FlowTransitionProposal` instances whose
/// `flowInstance` is `instance_uri` and asks for their links on
/// [`PROPOSAL_LINK_PREDICATES`] (`links`, #1117). It reads only each
/// instance's `id` and its `__links` rows, never a hydrated value.
///
/// **Why the rows and not the instance.** Hydration folds a proposal into one
/// object: a scalar is last-write-wins across authors, and `author` is the
/// author of the earliest link. Read that way, a later third-party `to_state`
/// would be the value while the proposal is still attributed to its proposer
/// (#1046 §1). The atom rules need each link's own author and signature
/// verdict, which is what a `__links` row is: a
/// [`DecoratedLinkExpression`], every author's links kept apart, each with the
/// verdict the store computed from its signature and its `status`.
///
/// **What the query withholds.** A link whose signature does not verify is
/// neither in a row nor able to select a proposal (the `model_query` default,
/// #1113, #1120). [`signed_by`] ignores such a link anyway, so no atom changes,
/// but a read-set no longer carries forged material for every receipt reader
/// to re-verify.
///
/// A requested key missing from `__links`, or a row that is not a link, is an
/// `Err`, never "no links": an empty `acceptedBy` would silently drop votes.
pub async fn load_proposal_links(
    perspective: &PerspectiveInstance,
    instance_uri: &str,
) -> anyhow::Result<Vec<(String, Vec<DecoratedLinkExpression>)>> {
    if instance_uri.is_empty() {
        return Err(anyhow::anyhow!(
            "load_proposal_links: instance_uri must not be empty"
        ));
    }
    let query_json = serde_json::json!({
        "where": { "flowInstance": instance_uri },
        "links": PROPOSAL_LINK_PREDICATES,
    })
    .to_string();
    let raw = perspective
        .model_query(FLOW_TRANSITION_PROPOSAL_CLASS, &query_json)
        .await
        .map_err(|e| anyhow::anyhow!("load_proposal_links: model_query failed: {e:#}"))?;
    let result: serde_json::Value = serde_json::from_str(&raw).map_err(|e| {
        anyhow::anyhow!("load_proposal_links: model_query returned invalid JSON: {e:#}")
    })?;
    proposal_links_of(&result)
}

/// Each instance's `id` and its links on [`PROPOSAL_LINK_PREDICATES`], from
/// one `model_query` result, sorted by URI.
fn proposal_links_of(
    result: &serde_json::Value,
) -> anyhow::Result<Vec<(String, Vec<DecoratedLinkExpression>)>> {
    let instances = result
        .get("instances")
        .and_then(serde_json::Value::as_array)
        .ok_or_else(|| {
            anyhow::anyhow!("load_proposal_links: model_query returned no `instances` array")
        })?;
    let mut out: Vec<(String, Vec<DecoratedLinkExpression>)> = Vec::new();
    for instance in instances {
        let Some(uri) = instance.get("id").and_then(serde_json::Value::as_str) else {
            continue;
        };
        let mut links = Vec::new();
        for predicate in PROPOSAL_LINK_PREDICATES {
            let rows = instance
                .get(LINKS_KEY)
                .and_then(|l| l.get(predicate))
                .and_then(serde_json::Value::as_array)
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "load_proposal_links: {uri} has no `{LINKS_KEY}.{predicate}` rows; \
                         reading that as \"no links\" could drop votes"
                    )
                })?;
            for row in rows {
                links.push(serde_json::from_value(row.clone()).map_err(|e| {
                    anyhow::anyhow!(
                        "load_proposal_links: a `{predicate}` row of {uri} is not a link ({e}): {row}"
                    )
                })?);
            }
        }
        out.push((uri.to_string(), links));
    }
    out.sort_by(|a, b| a.0.cmp(&b.0));
    out.dedup_by(|a, b| a.0 == b.0);
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::super::test_support::*;
    use super::*;
    /// Every property path of the class, and the two links it does not
    /// declare, is read. A path missing here would be a proposer link the
    /// read drops, and `proposed_at` is the earliest of those.
    #[test]
    fn proposal_link_predicates_cover_the_class() {
        let sdna: serde_json::Value =
            serde_json::from_str(crate::perspectives::flow_classes::FLOW_TRANSITION_PROPOSAL_SDNA)
                .expect("the class SDNA parses");
        let mut expected: Vec<&str> = sdna["properties"]
            .as_array()
            .expect("properties")
            .iter()
            .map(|p| p["path"].as_str().expect("path"))
            .chain([ACCEPTED_BY_PREDICATE, RESOLVED_AS_PREDICATE])
            .collect();
        expected.sort();
        let mut read = PROPOSAL_LINK_PREDICATES.to_vec();
        read.sort();
        assert_eq!(read, expected);
    }

    fn query_result(links: serde_json::Value) -> serde_json::Value {
        serde_json::json!({ "instances": [{ "id": "proposal://p1", "__links": links }] })
    }

    fn all_keys_empty() -> serde_json::Map<String, serde_json::Value> {
        PROPOSAL_LINK_PREDICATES
            .iter()
            .map(|p| (p.to_string(), serde_json::json!([])))
            .collect()
    }

    /// A requested key missing from `__links` is an error, not "no links":
    /// no `acceptedBy` rows read as none would drop every vote. A row that is
    /// not a link is an error too. The rows of every key are one list.
    #[test]
    fn proposal_rows_missing_or_malformed_are_errors_not_no_links() {
        let mut missing = all_keys_empty();
        missing.remove(ACCEPTED_BY_PREDICATE);
        let err = proposal_links_of(&query_result(missing.into()))
            .expect_err("a missing key is an error");
        assert!(
            err.to_string().contains(ACCEPTED_BY_PREDICATE),
            "names the key: {err}"
        );

        let mut malformed = all_keys_empty();
        malformed.insert(
            TO_STATE_PREDICATE.to_string(),
            serde_json::json!([{ "author": ALICE }]),
        );
        let err = proposal_links_of(&query_result(malformed.into()))
            .expect_err("a row that is not a link is an error");
        assert!(err.to_string().contains("not a link"), "{err}");

        let honest = honest_proposal(ALICE, "review", "approved", "h1", T1);
        let mut rows = all_keys_empty();
        for l in &honest {
            rows[l.data.predicate.as_deref().unwrap()]
                .as_array_mut()
                .unwrap()
                .push(serde_json::to_value(l).unwrap());
        }
        let read = proposal_links_of(&query_result(rows.into())).expect("rows parse");
        assert_eq!(read.len(), 1);
        assert_eq!(read[0].0, "proposal://p1");
        assert_eq!(read[0].1.len(), honest.len());
        assert_eq!(
            atom_of(&read[0].1),
            atom_of(&honest),
            "the rows read back as the same atom"
        );
    }

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
