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

use super::time::parse_link_timestamp;
use crate::perspectives::flow_classes::FLOW_TRANSITION_PROPOSAL_CLASS;
use crate::perspectives::flow_evaluator::EvidenceItem;
use crate::perspectives::model_query::utils::parse_literal_value;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{DecoratedLinkExpression, LinkQuery, LinkStatus};
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

/// One output of a run: an instance, and the class it is an instance of.
///
/// The class is part of the name because an output's content is what
/// `model_query` returns for that class's shape, and the same node read
/// through another class is other content. Serialised camelCase
/// (`{"className", "id"}`), the shape the TS client sends.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct OutputRef {
    pub class_name: String,
    pub id: String,
}

impl OutputRef {
    /// The text an [`OUTPUT_PREDICATE`] link carries: canonical JSON, keys
    /// sorted, so one ref has one encoding.
    pub fn encode(&self) -> String {
        crate::perspectives::flow_evaluator::canonical_json(&serde_json::json!({
            "className": self.class_name,
            "id": self.id,
        }))
    }

    /// Inverse of [`encode`](Self::encode). A JSON object with exactly these
    /// two string fields parses; nothing else does.
    pub fn decode(text: &str) -> Option<Self> {
        serde_json::from_str(text).ok()
    }

    /// The ref an output preimage names.
    pub fn of(item: &EvidenceItem) -> Self {
        Self {
            class_name: item.class_name.clone(),
            id: item.id.clone(),
        }
    }
}

/// The one definition of a run's outputs commitment: the
/// [`evidence_hash`](crate::perspectives::flow_evaluator::evidence_hash)
/// framing over each output's `(class, id, canonical content)`, under
/// [`OUTPUTS_HASH_TAG`] so it can never be read as an evidence seal
/// ([`tagged_items_hash`](crate::perspectives::flow_evaluator::tagged_items_hash)).
///
/// `items` are what `model_query` returns for each named output, the same
/// shape the evidence seal hashes, so **editing an output changes the hash**.
/// Order does not change the result. Duplicates do: every writer hashes one
/// item per [`normalised_outputs`] ref, and a receipt carrying a duplicate is
/// refused rather than normalised.
pub fn outputs_hash(items: &[EvidenceItem]) -> String {
    crate::perspectives::flow_evaluator::tagged_items_hash(OUTPUTS_HASH_TAG, items)
}

/// **The one producer of a proposal's URI**: `ad4m://flow/proposal/<hash>`,
/// where the hash is the [`content_address`](crate::perspectives::content_address::content_address)
/// of everything a co-signer's vote must cover.
///
/// A vote is `proposal_uri --acceptedBy--> voter` — it signs the URI and
/// nothing else. With a random URI that covered nothing, and the final-edge
/// proposer could re-sign `outputs_hash` (or the seal) *under the same URI*
/// after the votes landed, walking a swapped output into a verified receipt
/// (#1108 review, @lal-bot-coasys). Deriving the URI from the fields closes
/// it: a proposal whose fields do not hash to its own URI is not an atom
/// ([`AtomRejection::UriMismatch`]), so every vote the fold counts covers
/// the instance, the edge, the seal and the outputs commitment.
///
/// The preimage is positional under [`PROPOSAL_URI_TAG`]:
/// `(instance, from_state, to_state, evidence_hash, outputs_hash-or-none)`,
/// with the proposer as the address's author and the proposer-signed
/// [`PROPOSAL_NONCE_PREDICATE`] value as its salt. `outputs_hash` stays an
/// `Option` — absent (a non-terminal proposal) is not the empty string.
///
/// Every producer routes through here — the engine pass and the manual
/// propose path via `flow_classes::write_flow_transition_proposal`, and the
/// test fixtures — so there is exactly one definition of "the URI for these
/// fields".
pub fn proposal_uri(
    instance_uri: &str,
    from_state: &str,
    to_state: &str,
    evidence_hash: &str,
    outputs_hash: Option<&str>,
    proposer: &str,
    nonce: &str,
) -> String {
    let hash = crate::perspectives::content_address::content_address(
        PROPOSAL_URI_TAG,
        proposer,
        &[
            Some(instance_uri),
            Some(from_state),
            Some(to_state),
            Some(evidence_hash),
            outputs_hash,
        ],
        nonce,
    );
    crate::perspectives::flow_classes::flow_transition_proposal_uri(&hash)
}

/// `refs` sorted and deduplicated: the form a proposal names.
pub fn normalised_outputs(refs: &[OutputRef]) -> Vec<OutputRef> {
    let mut refs = refs.to_vec();
    refs.sort();
    refs.dedup();
    refs
}

/// Why a voter refuses to co-sign a proposal into a terminal state. Each
/// reason is its own variant so a client and a test can tell them apart.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OutputsRefusal {
    /// The proposal enters a terminal state but its proposer signed no
    /// [`OUTPUTS_HASH_PREDICATE`]. Nothing would bind a receipt for the run
    /// to any output, so co-signing it would complete a run no receipt can
    /// speak for.
    Uncommitted,
    /// A named output is not an instance of the class it is named as, on
    /// this replica.
    OutputNotInstance { output: OutputRef },
    /// The proposer's signed `outputs_hash` is not the hash of the named
    /// outputs' content as this replica reads it: an output was edited since
    /// the proposal, this replica has not synced an edit yet, or the proposer
    /// committed to something else.
    HashMismatch {
        /// `outputs_hash` as the proposer signed it.
        committed: String,
        /// [`outputs_hash`] of the named outputs' content on this replica.
        recomputed: String,
    },
}

impl std::fmt::Display for OutputsRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Uncommitted => write!(
                f,
                "it enters a terminal state but carries no `{OUTPUTS_HASH_PREDICATE}`, so no \
                 receipt could bind the run to any output"
            ),
            Self::OutputNotInstance { output } => write!(
                f,
                "it names `{}` as an output of class `{}`, and this replica has no such \
                 instance of that class",
                output.id, output.class_name
            ),
            Self::HashMismatch {
                committed,
                recomputed,
            } => write!(
                f,
                "its `{OUTPUTS_HASH_PREDICATE}` is `{committed}`, but the outputs it names hash \
                 to `{recomputed}` on this replica: their content is not what the proposer \
                 committed to"
            ),
        }
    }
}

/// The voter's outputs check, pure. Run by `super::accept` before co-signing
/// and by `super::propose` before the proposer's own vote, so both sides of a
/// vote apply one rule.
///
/// Only a proposal into a terminal state is checked (`terminal`); anywhere
/// else a run does not end and there is nothing to bind. `content` answers
/// what this replica's `model_query` returns for a named output, `None` when
/// it is not an instance of that class. Checks in order: a commitment is
/// present, every named output is an instance of its class, and the
/// commitment is the hash of their content.
pub fn check_outputs_commitment(
    atom: &TransitionAtom,
    terminal: bool,
    content: impl Fn(&OutputRef) -> Option<EvidenceItem>,
) -> Result<(), OutputsRefusal> {
    if !terminal {
        return Ok(());
    }
    let Some(committed) = &atom.outputs_hash else {
        return Err(OutputsRefusal::Uncommitted);
    };
    let mut items = Vec::with_capacity(atom.outputs.len());
    for output in &atom.outputs {
        match content(output) {
            Some(item) => items.push(item),
            None => {
                return Err(OutputsRefusal::OutputNotInstance {
                    output: output.clone(),
                })
            }
        }
    }
    let recomputed = outputs_hash(&items);
    if &recomputed != committed {
        return Err(OutputsRefusal::HashMismatch {
            committed: committed.clone(),
            recomputed,
        });
    }
    Ok(())
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
/// [`reverified_link`](super::reverified_link) clears `status` on every
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

/// The outputs `proposer` named with [`OUTPUT_PREDICATE`], sorted and
/// deduplicated. A third party's `output` link is invisible here, exactly as
/// in [`unique_field`], so nobody can add an output to someone else's
/// proposal. One of the proposer's own links that does not decode rejects
/// the atom ([`AtomRejection::MalformedOutput`]).
fn named_outputs(
    links: &[DecoratedLinkExpression],
    proposer: &str,
) -> Result<Vec<OutputRef>, AtomRejection> {
    let refs = links_on(links, OUTPUT_PREDICATE)
        .filter(|l| signed_by(l, proposer))
        .map(|l| {
            let text = field_value(&l.data.target);
            OutputRef::decode(&text).ok_or(AtomRejection::MalformedOutput(text))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(normalised_outputs(&refs))
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
pub(super) mod fixtures {
    use super::*;
    use crate::types::{DecoratedExpressionProof, Link};

    pub const INSTANCE: &str = "ad4m://flow/instance/i1";
    pub const PROPOSAL: &str = "ad4m://flow/proposal/p1";
    /// The nonce every claimed-verdict fixture proposal salts its URI with.
    /// Tests that need twins pass their own distinct nonces instead.
    pub const NONCE: &str = "fixture-nonce";
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

    /// The six links `write_flow_transition_proposal` emits, all signed by
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
            link(
                PROPOSAL_NONCE_PREDICATE,
                &literal(NONCE),
                proposer,
                true,
                at,
            ),
        ]
    }

    /// The URI these links address: recomputed from the proposer-signed
    /// fields exactly as [`TransitionAtom::from_links`] recomputes it, so a
    /// field-focused test exercises field logic rather than tripping the URI
    /// check with a stale fixture constant. Falls back to [`PROPOSAL`] when
    /// a field is missing or ambiguous — `from_links` rejects those shapes
    /// before it reaches the URI either way. Tests **of** the URI check call
    /// `from_links` with a deliberately different URI instead.
    pub fn addressed_uri(links: &[DecoratedLinkExpression]) -> String {
        let Ok(proposer) = self_authored_proposer(links) else {
            return PROPOSAL.to_string();
        };
        let field = |p: &'static str| unique_field(links, p, &proposer).ok();
        let (Some(from), Some(to), Some(seal), Some(nonce)) = (
            field(FROM_STATE_PREDICATE),
            field(TO_STATE_PREDICATE),
            field(EVIDENCE_HASHES_PREDICATE),
            field(PROPOSAL_NONCE_PREDICATE),
        ) else {
            return PROPOSAL.to_string();
        };
        let outputs_hash = field(OUTPUTS_HASH_PREDICATE);
        proposal_uri(
            INSTANCE,
            &from,
            &to,
            &seal,
            outputs_hash.as_deref(),
            &proposer,
            &nonce,
        )
    }

    pub fn atom_of(links: &[DecoratedLinkExpression]) -> Result<TransitionAtom, AtomRejection> {
        TransitionAtom::from_links(INSTANCE, &addressed_uri(links), links)
    }

    // -----------------------------------------------------------------------
    // Cryptographically honest fixtures
    //
    // The builders above state a signature verdict; these ones *earn* it.
    // Both kinds are needed and neither replaces the other:
    //
    // - `link` / `honest_proposal` exercise the checks that read a carried
    //   verdict (`signed_by`, `from_links`), where the verdict is the input
    //   under test and inventing a keypair would only obscure it;
    // - `signed_*` exercise anything downstream of
    //   [`ReadSet::reverified`](super::super::ReadSet::reverified), which
    //   recomputes the verdict from the signature — there a fixture that
    //   merely *claims* `valid: true` is exactly the minter's word the
    //   reader no longer takes, so it has to sign for real.
    // -----------------------------------------------------------------------

    use crate::agent::signatures::TestSigner;
    use crate::types::{Link as CoreLink, LinkExpression, LinkStatus as CoreLinkStatus};
    use std::collections::HashMap;
    use std::sync::{LazyLock, Mutex};

    /// A named persona holding a **real** Ed25519 keypair. Leaked on first use
    /// so the DIDs read like the `&'static str` constants they stand beside.
    /// One keypair per persona per process.
    pub fn persona(name: &str) -> &'static TestSigner {
        static SIGNERS: LazyLock<Mutex<HashMap<String, &'static TestSigner>>> =
            LazyLock::new(|| Mutex::new(HashMap::new()));
        *SIGNERS
            .lock()
            .expect("persona registry")
            .entry(name.to_string())
            .or_insert_with(|| Box::leak(Box::new(TestSigner::generate())))
    }

    /// `persona(name).did`, for fixtures that need the identity rather than
    /// the key.
    pub fn did_of(name: &str) -> &'static str {
        &persona(name).did
    }

    /// One link as `get_links` returns it, with its `valid` flag honoured
    /// **cryptographically**: a valid link is signed by `author_name`'s own
    /// key over its own data and timestamp, and a forged one carries a
    /// signature from a key that is not theirs.
    ///
    /// `proof.valid` is still pre-set to match, because that is what a store
    /// hands back — but every reader downstream of the ingest recomputes it,
    /// so a fixture whose claim and signature disagree gets ruled on by the
    /// signature. That disagreement is itself a fixture: pass
    /// `claims_valid = Some(true)` with `valid = false` to build the forgery
    /// a dishonest minter would carry.
    pub fn signed_link(
        source: &str,
        predicate: &str,
        target: &str,
        author_name: &str,
        valid: bool,
        claims_valid: Option<bool>,
        timestamp: &str,
    ) -> DecoratedLinkExpression {
        let at = chrono::DateTime::parse_from_rfc3339(timestamp)
            .unwrap_or_else(|e| panic!("fixture timestamp `{timestamp}`: {e}"))
            .with_timezone(&chrono::Utc);
        let author = persona(author_name);
        let signing_key = if valid { author } else { persona("forger") };
        let mut expr = signing_key.sign_at(
            CoreLink {
                source: source.to_string(),
                predicate: Some(predicate.to_string()),
                target: target.to_string(),
            }
            .normalize(),
            at,
        );
        expr.author = author.did.clone();
        expr.proof.key = author.key_id.clone();
        let mut link =
            DecoratedLinkExpression::from((LinkExpression::from(expr), CoreLinkStatus::Shared));
        let claimed = claims_valid.unwrap_or(valid);
        link.proof.valid = Some(claimed);
        link.proof.invalid = Some(!claimed);
        link
    }

    /// The six links `write_flow_transition_proposal` emits, all genuinely
    /// signed by the proposer, sourced at the proposal's **content-addressed
    /// URI** — computed here from the same fields, so the fixture and the
    /// production writer agree by construction. `nonce` distinguishes twins;
    /// the old fixture URI strings ("ad4m://p/1") make fine nonces. Returns
    /// the URI alongside the links because everything downstream (votes, the
    /// read-set) names the proposal by it.
    pub fn signed_proposal(
        nonce: &str,
        proposer_name: &str,
        from: &str,
        to: &str,
        seal: &str,
        at: &str,
    ) -> (String, Vec<DecoratedLinkExpression>) {
        let uri = proposal_uri(INSTANCE, from, to, seal, None, did_of(proposer_name), nonce);
        let signed = |predicate: &str, target: &str| {
            signed_link(&uri, predicate, target, proposer_name, true, None, at)
        };
        let links = vec![
            signed(PROPOSER_PREDICATE, did_of(proposer_name)),
            signed(FLOW_INSTANCE_PREDICATE, INSTANCE),
            signed(FROM_STATE_PREDICATE, &literal(from)),
            signed(TO_STATE_PREDICATE, &literal(to)),
            signed(EVIDENCE_HASHES_PREDICATE, &literal(seal)),
            signed(PROPOSAL_NONCE_PREDICATE, &literal(nonce)),
        ];
        (uri, links)
    }

    /// [`signed_proposal`] into a terminal state: the six links plus one
    /// signed `output` link per id in `outputs` (as [`out_ref`]) and a signed
    /// `outputs_hash` equal to `committed` — which is part of the URI
    /// preimage, so the returned URI covers it. Pass [`hash_of`]`(outputs)`
    /// for an honest proposal; anything else builds a proposal whose
    /// commitment does not match its named outputs.
    #[allow(clippy::too_many_arguments)]
    pub fn signed_terminal_proposal(
        nonce: &str,
        proposer_name: &str,
        from: &str,
        to: &str,
        seal: &str,
        outputs: &[&str],
        committed: &str,
        at: &str,
    ) -> (String, Vec<DecoratedLinkExpression>) {
        let uri = proposal_uri(
            INSTANCE,
            from,
            to,
            seal,
            Some(committed),
            did_of(proposer_name),
            nonce,
        );
        let links = signed_terminal_links_at(
            &uri,
            proposer_name,
            from,
            to,
            seal,
            outputs,
            committed,
            nonce,
            at,
        );
        (uri, links)
    }

    /// The links of [`signed_terminal_proposal`] sourced at an **arbitrary**
    /// `uri` — the raw material of the post-co-sign swap: a proposer who
    /// re-signs `outputs_hash` (or the seal) under a URI that was addressed
    /// for other fields produces exactly this shape, genuinely signed.
    /// Honest fixtures use [`signed_terminal_proposal`], which computes the
    /// URI these links actually address.
    #[allow(clippy::too_many_arguments)]
    pub fn signed_terminal_links_at(
        uri: &str,
        proposer_name: &str,
        from: &str,
        to: &str,
        seal: &str,
        outputs: &[&str],
        committed: &str,
        nonce: &str,
        at: &str,
    ) -> Vec<DecoratedLinkExpression> {
        let signed = |predicate: &str, target: &str| {
            signed_link(uri, predicate, target, proposer_name, true, None, at)
        };
        let mut links = vec![
            signed(PROPOSER_PREDICATE, did_of(proposer_name)),
            signed(FLOW_INSTANCE_PREDICATE, INSTANCE),
            signed(FROM_STATE_PREDICATE, &literal(from)),
            signed(TO_STATE_PREDICATE, &literal(to)),
            signed(EVIDENCE_HASHES_PREDICATE, &literal(seal)),
            signed(PROPOSAL_NONCE_PREDICATE, &literal(nonce)),
        ];
        links.extend(
            outputs
                .iter()
                .map(|id| signed(OUTPUT_PREDICATE, &literal(&out_ref(id).encode()))),
        );
        links.push(signed(OUTPUTS_HASH_PREDICATE, &literal(committed)));
        links
    }

    /// The class every fixture output is an instance of.
    pub const OUT_CLASS: &str = "coasys://Deliverable";

    /// `id` as an output of [`OUT_CLASS`].
    pub fn out_ref(id: &str) -> OutputRef {
        OutputRef {
            class_name: OUT_CLASS.to_string(),
            id: id.to_string(),
        }
    }

    /// `id`'s content as the fixture graph holds it: a function of the id, so
    /// a proposer and a receipt built apart agree on it.
    pub fn out_item(id: &str) -> EvidenceItem {
        EvidenceItem {
            id: id.to_string(),
            class_name: OUT_CLASS.to_string(),
            content: serde_json::json!({ "id": id, "title": format!("the content of {id}") })
                .to_string(),
        }
    }

    /// [`out_item`] for each id.
    pub fn out_items(ids: &[&str]) -> Vec<EvidenceItem> {
        ids.iter().map(|id| out_item(id)).collect()
    }

    /// [`outputs_hash`] over [`out_items`], for fixtures.
    pub fn hash_of(ids: &[&str]) -> String {
        outputs_hash(&out_items(ids))
    }

    /// A genuinely signed `proposal --acceptedBy--> voter` co-signature.
    pub fn signed_vote(proposal_uri: &str, voter_name: &str, at: &str) -> DecoratedLinkExpression {
        signed_link(
            proposal_uri,
            ACCEPTED_BY_PREDICATE,
            did_of(voter_name),
            voter_name,
            true,
            None,
            at,
        )
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

    // ---- the outputs commitment (#1104) --------------------------------------

    const D1: &str = "ad4m://deliverable/d1";
    const D2: &str = "ad4m://deliverable/d2";
    const ATTACKER: &str = "ad4m://attacker/node";

    fn with_outputs(outputs: &[&str], committed: &str) -> Vec<DecoratedLinkExpression> {
        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        links.extend(outputs.iter().map(|id| {
            link(
                OUTPUT_PREDICATE,
                &literal(&out_ref(id).encode()),
                ALICE,
                true,
                T1,
            )
        }));
        links.push(link(
            OUTPUTS_HASH_PREDICATE,
            &literal(committed),
            ALICE,
            true,
            T1,
        ));
        links
    }

    /// What this replica's `model_query` returns for a ref: the fixture
    /// content, for the ids in `present` only.
    fn graph_with<'a>(present: &'a [&'a str]) -> impl Fn(&OutputRef) -> Option<EvidenceItem> + 'a {
        move |r: &OutputRef| {
            (r.class_name == OUT_CLASS && present.contains(&r.id.as_str())).then(|| out_item(&r.id))
        }
    }

    /// The hash is over each output's `(class, id, content)`, in any order.
    /// Editing an output's content changes it, so does reading the same id
    /// as another class, and so does the item set. It is never the evidence
    /// seal over the same items.
    ///
    /// Red if `outputs_hash` hashes ids only, drops the class or the
    /// content, is order-sensitive, or loses its domain tag.
    #[test]
    fn outputs_hash_is_over_class_id_and_content() {
        assert_eq!(hash_of(&[D2, D1]), hash_of(&[D1, D2]), "order-independent");
        assert_ne!(hash_of(&[D1]), hash_of(&[D1, D2]));
        assert_ne!(hash_of(&[D1]), hash_of(&[ATTACKER]));
        assert_ne!(hash_of(&[]), hash_of(&[D1]));

        let mut edited = out_item(D1);
        edited.content = serde_json::json!({ "id": D1, "title": "edited" }).to_string();
        assert_ne!(outputs_hash(&[edited]), hash_of(&[D1]), "content-sensitive");

        let mut other_class = out_item(D1);
        other_class.class_name = "coasys://Other".to_string();
        assert_ne!(
            outputs_hash(&[other_class]),
            hash_of(&[D1]),
            "class-sensitive"
        );

        let mut reordered_keys = out_item(D1);
        reordered_keys.content = format!(r#"{{"title":"the content of {D1}","id":"{D1}"}}"#);
        assert_eq!(
            outputs_hash(&[reordered_keys]),
            hash_of(&[D1]),
            "canonical: key order is not content"
        );

        assert_ne!(
            hash_of(&[D1]),
            crate::perspectives::flow_evaluator::evidence_hash(&[], &out_items(&[D1])),
            "an outputs hash is never an evidence seal over the same items"
        );
    }

    /// An [`OutputRef`] round-trips through its link encoding, and the
    /// encoding is canonical. Anything that is not exactly a
    /// `{className, id}` object does not decode.
    #[test]
    fn an_output_ref_encodes_canonically_and_decodes_strictly() {
        let r = out_ref(D1);
        assert_eq!(
            r.encode(),
            format!(r#"{{"className":"{OUT_CLASS}","id":"{D1}"}}"#)
        );
        assert_eq!(OutputRef::decode(&r.encode()), Some(r));
        assert_eq!(OutputRef::decode(D1), None, "a bare id is not a ref");
        assert_eq!(OutputRef::decode(r#"{"id":"x"}"#), None);
        assert_eq!(
            OutputRef::decode(r#"{"className":"c","id":"x","extra":1}"#),
            None
        );
    }

    /// The atom reads the proposer's named outputs and commitment, and only
    /// the proposer's: Mallory's `output` link on Alice's proposal names
    /// nothing. Two distinct commitments by Alice reject the atom, the same
    /// way two `to_state` values do, and so does an `output` link of hers
    /// that is not a ref.
    ///
    /// Red if `named_outputs` drops its `signed_by` filter (Mallory's node
    /// becomes an output), skips an undecodable link instead of rejecting,
    /// or if an ambiguous `outputs_hash` is read as `None`.
    #[test]
    fn an_atom_reads_only_the_proposers_named_outputs_and_commitment() {
        let mut links = with_outputs(&[D2, D1], &hash_of(&[D1, D2]));
        links.push(link(
            OUTPUT_PREDICATE,
            &literal(&out_ref(ATTACKER).encode()),
            MALLORY,
            true,
            T2,
        ));
        let atom = atom_of(&links).expect("atom");
        assert_eq!(atom.outputs, vec![out_ref(D1), out_ref(D2)]);
        assert_eq!(atom.outputs_hash, Some(hash_of(&[D1, D2])));

        let plain = atom_of(&honest_proposal(ALICE, "review", "approved", "h1", T1)).expect("atom");
        assert_eq!(plain.outputs_hash, None, "no commitment is not a rejection");
        assert!(plain.outputs.is_empty());

        let mut bare = with_outputs(&[D1], &hash_of(&[D1]));
        bare.push(link(OUTPUT_PREDICATE, D2, ALICE, true, T1));
        assert_eq!(
            atom_of(&bare),
            Err(AtomRejection::MalformedOutput(D2.to_string()))
        );

        links.push(link(
            OUTPUTS_HASH_PREDICATE,
            &literal(&hash_of(&[ATTACKER])),
            ALICE,
            true,
            T2,
        ));
        assert_eq!(
            atom_of(&links),
            Err(AtomRejection::AmbiguousField(OUTPUTS_HASH_PREDICATE))
        );
    }

    /// **Required test (b).** The proposer names d1 and signs a hash over d1
    /// and the attacker's node. A voter loads d1, recomputes, and refuses.
    ///
    /// Red if `check_outputs_commitment` skips the recompute, or compares the
    /// commitment against itself.
    #[test]
    fn a_voter_refuses_an_outputs_hash_that_does_not_match_the_named_outputs() {
        let atom = atom_of(&with_outputs(&[D1], &hash_of(&[D1, ATTACKER]))).expect("atom");
        assert_eq!(
            check_outputs_commitment(&atom, true, graph_with(&[D1, ATTACKER])),
            Err(OutputsRefusal::HashMismatch {
                committed: hash_of(&[D1, ATTACKER]),
                recomputed: hash_of(&[D1]),
            })
        );
        let honest = atom_of(&with_outputs(&[D1], &hash_of(&[D1]))).expect("atom");
        assert_eq!(
            check_outputs_commitment(&honest, true, graph_with(&[D1])),
            Ok(()),
            "control: the matching commitment passes"
        );
    }

    /// The ids and the commitment match, but this replica reads d1 with other
    /// content than the proposer hashed: edited since the proposal. Refused
    /// as a hash mismatch, not as a missing instance.
    ///
    /// Red if the hash is recomputed from the named refs instead of the
    /// content this replica loaded.
    #[test]
    fn a_voter_refuses_an_output_whose_content_changed_since_the_proposal() {
        let atom = atom_of(&with_outputs(&[D1], &hash_of(&[D1]))).expect("atom");
        let mut edited = out_item(D1);
        edited.content = serde_json::json!({ "id": D1, "title": "edited" }).to_string();
        let edited_graph = |r: &OutputRef| (r == &out_ref(D1)).then(|| edited.clone());
        assert_eq!(
            check_outputs_commitment(&atom, true, edited_graph),
            Err(OutputsRefusal::HashMismatch {
                committed: hash_of(&[D1]),
                recomputed: outputs_hash(&[edited.clone()]),
            })
        );
    }

    /// **Required test (c).** The commitment matches, but one named output is
    /// not an instance of its class on this replica. The voter refuses and
    /// names it, before any hash is compared.
    ///
    /// Red if `check_outputs_commitment` skips the instance check.
    #[test]
    fn a_voter_refuses_a_named_output_that_is_not_an_instance() {
        let atom = atom_of(&with_outputs(&[D1, D2], &hash_of(&[D1, D2]))).expect("atom");
        assert_eq!(
            check_outputs_commitment(&atom, true, graph_with(&[D1])),
            Err(OutputsRefusal::OutputNotInstance {
                output: out_ref(D2)
            })
        );
    }

    /// A terminal proposal with no commitment is refused as `Uncommitted`.
    /// A non-terminal proposal is not checked at all: a run that does not end
    /// there has nothing to bind.
    ///
    /// Red if a missing commitment is treated as the empty set (it would pass
    /// as "no outputs"), or if non-terminal proposals are checked.
    #[test]
    fn only_a_terminal_proposal_must_commit_to_its_outputs() {
        let plain = atom_of(&honest_proposal(ALICE, "review", "approved", "h1", T1)).expect("atom");
        assert_eq!(
            check_outputs_commitment(&plain, true, graph_with(&[])),
            Err(OutputsRefusal::Uncommitted)
        );
        let unloaded = atom_of(&with_outputs(&[D1], &hash_of(&[D2]))).expect("atom");
        assert_eq!(
            check_outputs_commitment(&unloaded, false, graph_with(&[])),
            Ok(())
        );
    }

    // ---- the content-addressed URI (#1108) ------------------------------------

    /// **The URI is the fields, or the proposal is not an atom.** A vote is
    /// `uri --acceptedBy--> did`, so a URI that does not commit to the
    /// proposer-signed fields lets the proposer swap them after the votes
    /// land. Three shapes must reject: a URI addressed for other fields (the
    /// swap), a legacy random-UUID URI (a vote on it covers nothing), and a
    /// proposal with no signed nonce at all (nothing to recompute from).
    ///
    /// Red while `from_links` skips the recompute: every shape here parses.
    #[test]
    fn an_atom_whose_uri_is_not_its_fields_content_address_is_rejected() {
        let links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        let expected = addressed_uri(&links);
        assert!(
            atom_of(&links).is_ok(),
            "control: under its own content address the proposal is an atom"
        );

        // The swap / legacy shape: same signed fields, other URI.
        assert_eq!(
            TransitionAtom::from_links(INSTANCE, PROPOSAL, &links),
            Err(AtomRejection::UriMismatch {
                expected: expected.clone()
            }),
            "a URI the fields do not address is rejected, and the rejection \
             names what they do address"
        );
        assert_eq!(
            TransitionAtom::from_links(
                INSTANCE,
                "ad4m://flow/proposal/8f0e1a44-3d3c-4e0a-9c9c-3f5a1b2c3d4e",
                &links
            ),
            Err(AtomRejection::UriMismatch { expected }),
            "a pre-#1108 UUID URI is the same rejection — a vote on it covers nothing"
        );

        // No nonce: nothing to recompute the address from. `MissingField`,
        // not `UriMismatch` — the reader must not invent a salt.
        let mut without_nonce = honest_proposal(ALICE, "review", "approved", "h1", T1);
        without_nonce.retain(|l| l.data.predicate.as_deref() != Some(PROPOSAL_NONCE_PREDICATE));
        assert_eq!(
            atom_of(&without_nonce),
            Err(AtomRejection::MissingField(PROPOSAL_NONCE_PREDICATE))
        );
    }

    /// The address covers every field a vote must cover: change one — the
    /// outputs commitment, the seal, the edge, the nonce — and the URI that
    /// was right before is wrong now. This is the property the swap tests in
    /// `verify` exercise end to end; here it is pinned per field.
    ///
    /// Red while `from_links` skips the recompute, and red for the mutations
    /// that drop `outputs_hash` or the seal from the URI preimage.
    #[test]
    fn re_signing_any_addressed_field_under_the_old_uri_rejects_the_atom() {
        let original = with_outputs(&[D1], &hash_of(&[D1]));
        let uri = addressed_uri(&original);
        assert!(
            TransitionAtom::from_links(INSTANCE, &uri, &original).is_ok(),
            "control: the unswapped fields address `{uri}`"
        );

        // Replace one proposer-signed field wholesale — the withheld-and-
        // replaced shape, where the original links never reach the reader.
        let resigned = |predicate: &'static str, value: &str| {
            let mut links = original.clone();
            links.retain(|l| l.data.predicate.as_deref() != Some(predicate));
            links.push(link(predicate, &literal(value), ALICE, true, T2));
            TransitionAtom::from_links(INSTANCE, &uri, &links)
        };

        let swapped_outputs = resigned(OUTPUTS_HASH_PREDICATE, &hash_of(&[ATTACKER]));
        assert!(
            matches!(swapped_outputs, Err(AtomRejection::UriMismatch { .. })),
            "a re-signed outputs commitment no longer addresses the voted URI: {swapped_outputs:?}"
        );
        let swapped_seal = resigned(EVIDENCE_HASHES_PREDICATE, "h2-reframed-evidence");
        assert!(
            matches!(swapped_seal, Err(AtomRejection::UriMismatch { .. })),
            "a re-signed evidence seal no longer addresses the voted URI: {swapped_seal:?}"
        );
        let swapped_edge = resigned(TO_STATE_PREDICATE, "rejected");
        assert!(
            matches!(swapped_edge, Err(AtomRejection::UriMismatch { .. })),
            "a re-signed target state no longer addresses the voted URI: {swapped_edge:?}"
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
