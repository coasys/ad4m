//! A **flow receipt**: the completion claim a finished run leaves behind.
//!
//! # What a receipt says
//!
//! > *Under the social DNA of this space, this quorum settled this run into
//! > this terminal state, and here is the signed material — refold it
//! > yourself.*
//!
//! Nothing in it is a verdict. The terminal state is not asserted by whoever
//! minted it: [`FlowReceipt::mint`] derives it by running
//! [`fold_read_set`](super::fold_read_set) itself and refuses to build a
//! receipt for any other state. Everything a reader needs to reach the same
//! answer travels inside — and only that:
//!
//! ```text
//!   FlowReceipt
//!   ├── flow_uri          ──┐ the reference: WHICH social organism's rules
//!   ├── flow_dna_hash     ──┘ this run was settled under. Detection only;
//!   │                         a hash cannot reconstruct a definition, so
//!   │                         nothing here can be replayed against old rules.
//!   ├── terminal_state      derived by the fold at mint, never asserted
//!   ├── outputs             (class, id, content) per output; must hash to
//!   │                       the final edge's signed outputs_hash
//!   ├── read_set            the proof body — signed links, carried raw
//!   └── evidence_preimage   what each counted atom's seal was taken over
//! ```
//!
//! The definition itself is deliberately **not** carried. Flows and subject
//! classes are a space's social DNA; the hash of that DNA is the identity of
//! the social organism. Edit the DNA and it is a different space, so a
//! receipt minted under the old DNA *should* stop verifying in the new one.
//! A verifier folds against the definition its **own** catalogue holds, and
//! the minter contributes no rule text whatsoever — which is why there is no
//! version of "a dishonest minter embeds a weakened rule around real votes".
//!
//! `flow_dna_hash` is minter-asserted and that is acceptable: a false hash
//! can suppress or falsely trigger the *mismatch signal*, never manufacture a
//! verification, because the verifier still refolds the real signed votes
//! under the definition it holds. Making the hash quorum-signed (inside the
//! proposal seal) is named, deferred hardening.
//!
//! # Outputs are what the quorum committed to
//!
//! `outputs` is the one field where "a false value cannot manufacture a
//! verification" would NOT hold: it is exactly what a downstream consumer pays
//! out on (<https://github.com/coasys/ad4m/issues/1104>). So it is bound to
//! voter-signed material.
//!
//! An output is an instance **of a class**, and what a consumer pays out on is
//! that instance as it stood, not just its id: any of its properties can
//! change while the id stays the same. So a proposal into a terminal state
//! names the run's outputs as `(class, id)` pairs and carries
//! `outputs_hash`, the evidence-seal framing over each output's
//! `(class, id, canonical content)` under its own domain tag
//! ([`outputs_hash`](super::atom::outputs_hash)), as a signed field next to
//! the evidence seal. Every voter loads each named output through its class
//! on its own replica, refuses one that is not an instance of it, and
//! recomputes the hash over what it read before co-signing
//! ([`check_outputs_commitment`](super::atom::check_outputs_commitment)).
//!
//! "Content" is what `model_query` returns for the instance through its
//! class, the same hydration the evidence seal hashes: every scalar
//! property, every relation and collection **as the ids it points at** (not
//! the related instances' own content), property getters, and the synthetic
//! `createdAt` / `updatedAt` / `author` / `timestamp` read off the shape's
//! links. Re-asserting a value therefore changes the hash too: it moves
//! `updatedAt`.
//!
//! [`final_edge_commitment`] reads the commitment off the fold's last settled
//! edge (the one into the terminal state). Every atom the fold counted there
//! must carry an `outputs_hash`, and they must all carry the same one. The
//! receipt carries each output's preimage (an
//! [`EvidenceItem`]: class, id, content). `mint` refuses outputs that do not
//! hash to the commitment, and `verify_receipt` re-hashes `receipt.outputs`
//! and refuses any receipt whose hash differs
//! (`ReceiptVerdict::OutputsNotCommitted`). A re-mint of somebody else's run
//! can therefore only name what that run's quorum committed to, with the
//! content it committed to.
//!
//! **A receipt attests to the content at completion.** Editing an output
//! later does not invalidate a receipt already minted: its preimages are
//! frozen inside it, and they still hash to the signed commitment. It does
//! mean a receipt can no longer be minted from the live graph, because the
//! live content no longer hashes to the commitment; a minter has to hold the
//! content as it stood at completion.
//!
//! Outputs do not depend on the terminal state's `requires`. A guarded
//! terminal state and an unguarded one bind outputs the same way.
//!
//! The receipt carries the preimages, not just the hash, because a verifier
//! needs them to re-hash, and a reader needs the `(class, id)` pairs: to
//! answer "does this receipt speak for node X", and to list every output of
//! a flow.
//!
//! # What is NOT here
//!
//! - **The verifier.** [`verify_receipt`](super::verify::verify_receipt) and
//!   [`ReceiptVerdict`](super::verify::ReceiptVerdict) live in
//!   [`super::verify`]. Mint-time validation is written so that what it
//!   produces is exactly what that verifier accepts: an asymmetric rule would
//!   mint receipts that fail their own verification. The two sides share
//!   [`ReadSet::reverified`](super::ReadSet::reverified) — the ingest seam —
//!   and both fold [`fold_read_set`](super::fold_read_set) over its result.
//! - **Revocation.** Completion is a ratchet. Retracting a settling vote
//!   moves the *flow* back by design, and the receipt freezes the links as
//!   they stood — receipt and live fold then disagree, deliberately. A
//!   verified receipt means "granted at quorum time T"; un-granting is a new
//!   signed event (a tombstone on the output), never an invalidation of the
//!   receipt. Until output tombstones ship, a grant a receipt carries is
//!   **irrevocable by design**.
//!
//! # Anyone may mint
//!
//! A receipt's authority comes from its contents, not its minter, so minting
//! needs no coordination and duplicate mints are benign. The URI is derived
//! from the content ([`FlowReceipt::uri`]), so two replicas that mint the
//! same material converge on one node rather than racing.
//!
//! Twin mints collapse only when the content is byte-identical, and a carried
//! link's `proof.valid` is a per-replica verdict two replicas can legitimately
//! differ on (one has verified the signature, the other has not yet). Such
//! twins sit side by side instead of collapsing. Harmless — both verify — but
//! worth knowing before anyone counts receipts.
//!
//! # Discovery edges are not trusted
//!
//! Two plain multi-edges exist so a reader can *find* a receipt:
//! `instance --ad4m://flow/receipt--> receipt` and
//! `output --ad4m://flow/granted_by--> receipt`. Anyone can link any node to
//! any receipt and it forges nothing: the binding that counts is inside the
//! receipt's own content ([`FlowReceipt::outputs`]), and a verifier checks
//! that the receipt names the node whose edge it followed. The edges have
//! exactly the status `resolved_as` marks have — an index, never an input.

use super::atom::{outputs_hash, OutputRef, OUTPUTS_HASH_PREDICATE};
use super::fold::DerivedState;
use super::{fold_read_set, ReadSet};
use crate::perspectives::flow_evaluator::{canonical_json, evidence_hash, EvidenceItem};
use crate::perspectives::shacl_parser::SHACLFlow;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::collections::BTreeSet;

/// `instance --> receipt`. Discovery only; see the module header.
pub const FLOW_RECEIPT_PREDICATE: &str = "ad4m://flow/receipt";

/// `output --> receipt`. Discovery only; the binding lives in
/// [`FlowReceipt::outputs`].
pub const FLOW_GRANTED_BY_PREDICATE: &str = "ad4m://flow/granted_by";

/// `receipt --> literal:json:{…}`. The receipt's body, as one link off its
/// own content-derived URI.
pub const FLOW_RECEIPT_CONTENT_PREDICATE: &str = "ad4m://flow/receipt_content";

/// Receipt URIs are `ad4m://flow/receipt/{sha256 of the content}`.
pub const RECEIPT_URI_PREFIX: &str = "ad4m://flow/receipt/";

/// Hard cap on a serialised receipt. A completion whose receipt exceeds it is
/// logged and **not minted**: there is no spill/fetch protocol, and a receipt
/// that cannot be carried whole is not a receipt. Expected size is a few KB —
/// grant links are typically one per `(instance, DID)`, tombstones none or
/// one, and role evidence covers only DIDs that actually voted on gated
/// target states.
pub const MAX_RECEIPT_BYTES: usize = 256 * 1024;

/// What one counted atom's `evidence_hash` was taken over.
///
/// Guard evidence never enters the read-set — it reaches a proposal only as
/// the seal every voter independently recomputed on their own replica before
/// co-signing. Carrying the preimage turns "this hash matches nothing I can
/// see" into an inspectable object whose recomputed hash must equal that
/// seal.
///
/// # Why `class_names` is a field
///
/// [`evidence_hash`] frames the guard's class names into the digest *before*
/// the items, and the class names are not recoverable from the items: a
/// negative guard (`count: { max: 0 }`) is satisfied by **zero** matches, so
/// it contributes a class name and no item at all. A preimage carrying only
/// the items could not be re-hashed, and the verifier's seal check would be
/// unrunnable. (The design table lists the preimage as `(seal, items)`; this
/// is the one place this implementation adds a field to it, and the reason.)
///
/// Signed *evidence-author* links are a different thing and deliberately not
/// here: hydrated `model_query` results carry no per-link `(author,
/// proof.valid)`, so there is nothing signed to carry yet. The trust root of
/// a receipt is the voter quorum, not the evidence authors — the receipt's
/// claim is "n distinct DIDs each independently verified this guard held, and
/// here is what they saw".
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EvidencePreimage {
    /// The atom's `evidence_hash`, exactly as the proposer sealed it.
    pub seal: String,
    /// The guard's class names, in the order [`evidence_hash`] frames them.
    pub class_names: Vec<String>,
    /// Every instance the guard matched, with the `model_query` JSON the
    /// proposer saw.
    pub items: Vec<EvidenceItem>,
}

impl EvidencePreimage {
    /// Does this preimage hash to the seal it claims? Pure. The verifier's
    /// seal check; also run at mint, so a preimage that could not be
    /// reproduced never reaches a receipt.
    pub fn rehashes_to_seal(&self) -> bool {
        evidence_hash(&self.class_names, &self.items) == self.seal
    }
}

/// A completion claim, addressed to the future.
///
/// Built only by [`FlowReceipt::mint`], which derives every claim it makes
/// rather than accepting one. See the module header for the shape and for
/// what is deliberately absent.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FlowReceipt {
    /// `SHACLFlow::flow_uri()` — the role some organism has.
    pub flow_uri: String,
    /// [`flow_dna_hash`] of the definition as it stood at mint — *which*
    /// organism. Detection, never a replay input.
    pub flow_dna_hash: String,
    /// The state the fold reached at mint. Derived, never asserted, and
    /// always terminal ([`is_terminal_state`]): a receipt is a completion
    /// claim, not a per-edge event.
    pub terminal_state: String,
    /// The outputs this receipt speaks for, each with its content as the
    /// final edge's quorum committed to it: the binding a verifier checks
    /// before honouring a `granted_by` edge. Never empty; `mint` writes it
    /// sorted by `(class, id)`, one entry per output.
    ///
    /// **Bound to signed material.** Its
    /// [`outputs_hash`](super::atom::outputs_hash) must equal the
    /// `outputs_hash` every counted atom on the final edge carries.
    /// `verify_receipt` checks that and refuses any difference. See the
    /// module header, § *Outputs are what the quorum committed to*.
    pub outputs: Vec<EvidenceItem>,
    /// The proof body: signed links, carried raw, exactly as the fold
    /// received them.
    pub read_set: ReadSet,
    /// One entry per distinct seal among the counted atoms.
    pub evidence_preimage: Vec<EvidencePreimage>,
}

/// Content hash of a flow definition — the name of the social organism.
///
/// `hex(SHA256(canonical_json(flow)))` with `states` and `transitions` sorted
/// first. The sort is required, not tidiness: a `SHACLFlow` is reconstructed
/// from graph links on every read, and link enumeration order is a store
/// artefact two replicas may disagree on. An order-sensitive hash would make
/// the same DNA hash differently on different replicas, and every receipt
/// would look minted under foreign DNA.
///
/// States sort by `name` and transitions by `(from_state, to_state)`, each
/// with the element's own canonical JSON as the final tiebreaker — two
/// transitions may legitimately share `(from_state, to_state)` and differ in
/// `action_name`, and a partial sort key would leave their relative order to
/// the store again.
///
/// Everything else is scalars, `Option`s and author-ordered lists inside
/// states (`requires`) whose order the DNA author controls and which
/// round-trips deterministically: author-ordered content is identity-bearing
/// and stays unsorted.
pub fn flow_dna_hash(flow: &SHACLFlow) -> anyhow::Result<String> {
    let mut value = serde_json::to_value(flow)?;
    if let Some(states) = value.get_mut("states").and_then(Value::as_array_mut) {
        states.sort_by_cached_key(|s| (string_at(s, "name"), canonical_json(s)));
    }
    if let Some(transitions) = value.get_mut("transitions").and_then(Value::as_array_mut) {
        transitions.sort_by_cached_key(|t| {
            (
                string_at(t, "from_state"),
                string_at(t, "to_state"),
                canonical_json(t),
            )
        });
    }
    Ok(hex::encode(Sha256::digest(
        canonical_json(&value).as_bytes(),
    )))
}

fn string_at(value: &Value, key: &str) -> String {
    value
        .get(key)
        .and_then(Value::as_str)
        .unwrap_or_default()
        .to_string()
}

/// Has this state no outgoing transitions? Terminal states are where a run
/// ends, and only a run that ended gets a receipt.
///
/// Read off `transitions` directly rather than through
/// `flow_context::render::reachable_next_states`, which additionally requires
/// the target state to exist in `states`: a transition pointing at a state
/// the definition forgot to declare is a broken flow, and treating its source
/// as terminal would mint a completion claim for a run that is merely stuck.
pub fn is_terminal_state(flow: &SHACLFlow, state: &str) -> bool {
    !flow.transitions.iter().any(|t| t.from_state == state)
}

impl FlowReceipt {
    /// Mint a receipt for a run the fold says has completed. **Pure** — no
    /// store, no clock; the caller collects the preimages (the one step that
    /// needs a perspective) and writes the result.
    ///
    /// The terminal state is a *return* of this function, not a parameter:
    /// `mint` folds the read-set itself under `flow`, so no caller can claim a
    /// state the carried material does not reach. `outputs` is a parameter:
    /// each output's `(class, id)` and the content `model_query` returned for
    /// it when the run completed. It is checked against the commitment the
    /// final edge's quorum signed ([`final_edge_commitment`]), so no caller
    /// can name an output, or content, that quorum did not commit to. An
    /// output edited since completion no longer hashes to it, so a minter
    /// needs the content as it stood then. `mint` writes the outputs sorted
    /// by `(class, id)`.
    ///
    /// Refuses, rather than minting something that would fail its own
    /// verification:
    ///
    /// - `outputs` is empty — a receipt with no binding speaks for nothing,
    ///   and the `granted_by` check a verifier runs could never pass;
    /// - a preimage does not re-hash to the seal it claims;
    /// - the fold does not reach a state with no outgoing transitions — a
    ///   receipt is a completion claim, and an intermediate edge is not one;
    /// - the fold is **contested** — two edges out of the same state both
    ///   carry quorum, and anything that pays out on a completed flow must
    ///   refuse a contested derivation;
    /// - a counted atom on the final edge carries no `outputs_hash`, or two
    ///   of them carry different ones;
    /// - `outputs` names one output twice with different content;
    /// - `outputs` does not hash to the final edge's `outputs_hash`;
    /// - the serialised receipt exceeds [`MAX_RECEIPT_BYTES`].
    pub fn mint(
        flow: &SHACLFlow,
        read_set: ReadSet,
        mut outputs: Vec<EvidenceItem>,
        evidence_preimage: Vec<EvidencePreimage>,
    ) -> anyhow::Result<FlowReceipt> {
        outputs.sort_by(|a, b| {
            (&a.class_name, &a.id, &a.content).cmp(&(&b.class_name, &b.id, &b.content))
        });
        outputs.dedup();
        if let Some(pair) = outputs
            .windows(2)
            .find(|w| OutputRef::of(&w[0]) == OutputRef::of(&w[1]))
        {
            anyhow::bail!(
                "FlowReceipt::mint: {}: output `{}` of class `{}` is given twice with different \
                 content; a receipt carries one content per output",
                read_set.instance_uri,
                pair[0].id,
                pair[0].class_name
            );
        }
        if outputs.is_empty() {
            anyhow::bail!(
                "FlowReceipt::mint: {} has no outputs to speak for; a receipt with no binding \
                 could never be honoured",
                read_set.instance_uri
            );
        }
        if let Some(bad) = evidence_preimage.iter().find(|p| !p.rehashes_to_seal()) {
            anyhow::bail!(
                "FlowReceipt::mint: {}: the carried preimage for seal `{}` does not re-hash to \
                 it, so a verifier would reject this receipt; refusing to mint it",
                read_set.instance_uri,
                bad.seal
            );
        }

        // Through the ingest seam, exactly as `verify_receipt` does. Minting
        // on the raw value and verifying on the re-verified one would fold
        // different inputs by construction — see `ReadSet::reverified`.
        let ingested = read_set.reverified();
        let derived = fold_read_set(flow, &ingested)?;
        if let Some(contested) = &derived.contested {
            anyhow::bail!(
                "FlowReceipt::mint: {} is contested in `{}` ({} settled edges out of it), so it \
                 has not completed and nothing may pay out on it",
                read_set.instance_uri,
                contested.from_state,
                contested.candidates.len()
            );
        }
        if !is_terminal_state(flow, &derived.state) {
            anyhow::bail!(
                "FlowReceipt::mint: {} folds to `{}`, which flow `{}` can still transition out \
                 of; a receipt is a completion claim, not a per-edge event",
                read_set.instance_uri,
                derived.state,
                flow.flow_uri()
            );
        }
        match final_edge_commitment(&derived, &ingested) {
            OutputsCommitment::Committed(committed) if committed == outputs_hash(&outputs) => {}
            OutputsCommitment::Committed(committed) => anyhow::bail!(
                "FlowReceipt::mint: {}: outputs {:?} hash to `{}`, but the final edge's quorum \
                 committed to `{committed}`; a receipt may only carry the outputs, and the \
                 content, that quorum agreed to",
                read_set.instance_uri,
                outputs.iter().map(OutputRef::of).collect::<Vec<_>>(),
                outputs_hash(&outputs)
            ),
            other => anyhow::bail!("FlowReceipt::mint: {}: {other}", read_set.instance_uri),
        }

        let receipt = FlowReceipt {
            flow_uri: flow.flow_uri(),
            flow_dna_hash: flow_dna_hash(flow)?,
            terminal_state: derived.state,
            outputs,
            read_set,
            evidence_preimage,
        };
        let size = receipt.body()?.len();
        if size > MAX_RECEIPT_BYTES {
            anyhow::bail!(
                "FlowReceipt::mint: {} serialises to {size} bytes, over the {MAX_RECEIPT_BYTES} \
                 byte cap; not minted (there is no spill protocol)",
                receipt.read_set.instance_uri
            );
        }
        Ok(receipt)
    }

    /// The receipt as it is written to the graph.
    pub fn body(&self) -> anyhow::Result<String> {
        Ok(serde_json::to_string(self)?)
    }

    /// SHA256 over the canonical form of the whole receipt — key order
    /// independent, so two replicas serialising the same material agree.
    pub fn content_hash(&self) -> anyhow::Result<String> {
        let value = serde_json::to_value(self)?;
        Ok(hex::encode(Sha256::digest(
            canonical_json(&value).as_bytes(),
        )))
    }

    /// This receipt's node URI, derived from its content. Two replicas
    /// minting the same completion write the same node instead of racing;
    /// a receipt whose content is altered is a different node, so the URI can
    /// never name material other than its own.
    pub fn uri(&self) -> anyhow::Result<String> {
        Ok(format!("{RECEIPT_URI_PREFIX}{}", self.content_hash()?))
    }

    /// The distinct seals among the atoms the fold counted, in the order the
    /// settled edges were walked. What [`EvidencePreimage`] entries a mint
    /// has to collect.
    ///
    /// Exactly the counted atoms, nothing speculative: it folds first and
    /// keeps only atoms named in `settled[].atom_uris` — the proposals that
    /// contributed the counted votes.
    ///
    /// # The dedupe key is `seal`, and that is a contract
    ///
    /// The key is `seal` while the retained payload is `to_state` and
    /// `proposer`, so two counted atoms sharing a seal collapse to one entry
    /// and the survivor's `to_state` / `proposer` stand in for the dropped
    /// one's. Atoms *can* share a seal — two states with identical `requires`
    /// matching identical instances is the guard-identical sibling-edge shape
    /// from #1062 — so this is a live case, not a theoretical one.
    ///
    /// It is sound only because those two fields exist **solely to re-derive
    /// the seal** (see [`CountedAtom`]): anything that re-derives to the same
    /// seal is interchangeable for that purpose, so which representative
    /// survives cannot matter.
    ///
    /// That makes the invariant load-bearing: **add a field to
    /// [`CountedAtom`] that a verifier consumes and that is not hashed into
    /// the seal, and this dedupe silently drops the variant that differs.**
    /// Such a field needs either the seal widened to cover it or the dedupe
    /// key widened to include it — not a third `CountedAtom` member.
    pub fn counted_seals(flow: &SHACLFlow, read_set: &ReadSet) -> anyhow::Result<Vec<CountedAtom>> {
        // Same ingest as `mint` and `verify_receipt`: the atoms this walks
        // must be the atoms that walk counted.
        let read_set = read_set.reverified();
        let derived = fold_read_set(flow, &read_set)?;
        let counted: std::collections::BTreeSet<&str> = derived
            .settled
            .iter()
            .flat_map(|edge| edge.atom_uris.iter().map(String::as_str))
            .collect();
        let mut out: Vec<CountedAtom> = Vec::new();
        for atom in read_set.atoms() {
            if !counted.contains(atom.uri.as_str()) {
                continue;
            }
            if out.iter().any(|c| c.seal == atom.evidence_hash) {
                continue;
            }
            out.push(CountedAtom {
                proposal_uri: atom.uri,
                to_state: atom.to_state,
                proposer: atom.proposer,
                seal: atom.evidence_hash,
            });
        }
        Ok(out)
    }
}

/// One atom whose votes the fold counted, reduced to what re-deriving its
/// seal needs: the guard belongs to `to_state`, and `$did`-substituted guards
/// resolved against the **proposer** at mint time, so re-running the seal has
/// to substitute that same identity.
///
/// "Reduced to what re-deriving its seal needs" is a contract, not a
/// description — [`FlowReceipt::counted_seals`] dedupes on `seal` alone and
/// relies on every other field here being redundant given it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CountedAtom {
    pub proposal_uri: String,
    pub to_state: String,
    pub proposer: String,
    pub seal: String,
}

/// What the final edge's quorum committed to as the run's outputs. See
/// [`final_edge_commitment`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OutputsCommitment {
    /// Every atom counted on the final edge carries this `outputs_hash`.
    Committed(String),
    /// A counted atom on the final edge carries no `outputs_hash`, so its
    /// voters agreed to no outputs at all.
    Uncommitted { proposal_uri: String },
    /// Counted atoms on the final edge carry different `outputs_hash`
    /// values, sorted. Quorum belongs to the edge, so no one hash was agreed
    /// by the whole quorum.
    Conflicting { hashes: Vec<String> },
    /// The walk settled no edge, so there is no final edge to read.
    NoFinalEdge,
}

impl std::fmt::Display for OutputsCommitment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Committed(hash) => write!(f, "the final edge commits to outputs `{hash}`"),
            Self::Uncommitted { proposal_uri } => write!(
                f,
                "proposal {proposal_uri}, counted on the final edge, carries no \
                 `{OUTPUTS_HASH_PREDICATE}`, so its voters agreed to no outputs"
            ),
            Self::Conflicting { hashes } => write!(
                f,
                "the atoms counted on the final edge commit to different outputs {hashes:?}, so \
                 no set of outputs was agreed by the whole quorum"
            ),
            Self::NoFinalEdge => write!(
                f,
                "the walk settled no edge, so nothing committed to outputs"
            ),
        }
    }
}

/// **The one reading of a run's outputs commitment.** `mint` and
/// `verify_receipt` both call it, so the two sides cannot disagree on what a
/// receipt may speak for.
///
/// `derived` must be the fold of `ingested`, the re-verified read-set. Takes
/// the last settled edge (the one into the terminal state), and the
/// `outputs_hash` on each atom the fold counted there.
///
/// # Strict across twins
///
/// Quorum belongs to an edge, not to a proposal (see [`super::fold`]), so the
/// counted votes into the terminal state can sit on twin atoms. When those
/// twins commit to different outputs, each voter agreed only to their own
/// atom's set, and no set was agreed by the whole quorum. That is
/// [`OutputsCommitment::Conflicting`] and a receipt is refused; there is no
/// intersection or union.
///
/// `propose` refuses to join or twin an open proposal **with the same seal**
/// that names different outputs. It cannot see a twin under a different seal
/// (the cited evidence changed between the two mints, so the dedup key
/// differs), and neither can the engine pass. So a run can still end with
/// conflicting commitments on its final edge, and then it gets no receipt.
pub fn final_edge_commitment(derived: &DerivedState, ingested: &ReadSet) -> OutputsCommitment {
    let Some(final_edge) = derived.settled.last() else {
        return OutputsCommitment::NoFinalEdge;
    };
    let mut hashes: BTreeSet<String> = BTreeSet::new();
    for atom in ingested.atoms() {
        if !final_edge.atom_uris.contains(&atom.uri) {
            continue;
        }
        match atom.outputs_hash {
            Some(hash) => {
                hashes.insert(hash);
            }
            None => {
                return OutputsCommitment::Uncommitted {
                    proposal_uri: atom.uri,
                }
            }
        }
    }
    let mut hashes: Vec<String> = hashes.into_iter().collect();
    match hashes.len() {
        1 => OutputsCommitment::Committed(hashes.remove(0)),
        // `atom_uris` names at least one atom on a settled edge; zero would
        // mean the read-set lost an atom the fold counted. Fail closed.
        0 => OutputsCommitment::NoFinalEdge,
        _ => OutputsCommitment::Conflicting { hashes },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::flow_instance::atom::fixtures::{
        did_of, hash_of, out_item, out_items, signed_proposal, signed_terminal_proposal, T1, T2,
    };
    use crate::perspectives::flow_instance::ProposalLinks;

    /// Persona names rather than DIDs: every proposal below is now signed for
    /// real, because `mint` folds through
    /// [`ReadSet::reverified`](crate::perspectives::flow_instance::ReadSet::reverified)
    /// and a `did:key:alice` placeholder signs nothing.
    const ALICE: &str = "alice";
    const BOB: &str = "bob";

    const INSTANCE: &str = "ad4m://flow/instance/i1";
    const BASE: &str = "ad4m://task/t1";
    const DELIVERABLE: &str = "coasys://Deliverable";
    /// The node the honest run's final proposal names as its output.
    const OUTPUT: &str = "ad4m://deliverable/d1";
    /// A node the honest run never committed to.
    const ATTACKER: &str = "ad4m://attacker/node";

    /// `open → done`, `done` terminal. `extra` is spliced into the states and
    /// transitions so one fixture covers the branch and multi-hop shapes.
    fn flow_json(states: Value, transitions: Value) -> SHACLFlow {
        serde_json::from_value(serde_json::json!({
            "name": "Delivery",
            "namespace": "coasys://",
            "states": states,
            "transitions": transitions,
        }))
        .expect("fixture flow parses")
    }

    /// `open → done`, `done` terminal and guarded. The guard is incidental to
    /// the outputs since #1104; `unguarded_flow` is the same without it.
    fn two_state_flow() -> SHACLFlow {
        flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0, "requires": [{ "className": DELIVERABLE }] },
            ]),
            serde_json::json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        )
    }

    /// One proposal, self-proposed and therefore self-voted: under the
    /// default `{ n: 1 }` rule that is a settled edge. `proposer` is a
    /// persona *name*; the links are signed with that persona's real key.
    fn proposal(
        uri: &str,
        proposer: &str,
        from: &str,
        to: &str,
        seal: &str,
        at: &str,
    ) -> ProposalLinks {
        ProposalLinks {
            uri: uri.to_string(),
            links: signed_proposal(uri, proposer, from, to, seal, at),
        }
    }

    /// A proposal into a terminal state: signed like [`proposal`], and
    /// additionally naming `outputs` and signing `committed` as their
    /// `outputs_hash`. Honest when `committed == hash_of(outputs)`.
    #[allow(clippy::too_many_arguments)]
    fn committing(
        uri: &str,
        proposer: &str,
        from: &str,
        to: &str,
        seal: &str,
        outputs: &[&str],
        committed: &str,
        at: &str,
    ) -> ProposalLinks {
        ProposalLinks {
            uri: uri.to_string(),
            links: signed_terminal_proposal(uri, proposer, from, to, seal, outputs, committed, at),
        }
    }

    /// Alice's honest final proposal `open → done`, committing to [`OUTPUT`].
    fn final_proposal(at: &str) -> ProposalLinks {
        committing(
            "ad4m://p/1",
            ALICE,
            "open",
            "done",
            &delivered().seal,
            &[OUTPUT],
            &hash_of(&[OUTPUT]),
            at,
        )
    }

    /// Each id's preimage as the fixture graph holds it ([`out_items`]).
    fn outs(ids: &[&str]) -> Vec<EvidenceItem> {
        out_items(ids)
    }

    fn read_set(genesis: &str, proposals: Vec<ProposalLinks>) -> ReadSet {
        ReadSet {
            instance_uri: INSTANCE.to_string(),
            subject: BASE.to_string(),
            genesis: genesis.to_string(),
            proposals,
            role_grants: Vec::new(),
        }
    }

    fn completed() -> ReadSet {
        read_set("open", vec![final_proposal(T1)])
    }

    /// What `done`'s guard matched on the honest run: one deliverable.
    fn delivered() -> EvidencePreimage {
        deliverables(&[OUTPUT])
    }

    fn deliverables(ids: &[&str]) -> EvidencePreimage {
        preimage(
            &[DELIVERABLE],
            ids.iter()
                .map(|id| item(id, DELIVERABLE, &format!("{{\"id\":\"{id}\"}}")))
                .collect(),
        )
    }

    fn preimage(class_names: &[&str], items: Vec<EvidenceItem>) -> EvidencePreimage {
        let class_names: Vec<String> = class_names.iter().map(|s| s.to_string()).collect();
        EvidencePreimage {
            seal: evidence_hash(&class_names, &items),
            class_names,
            items,
        }
    }

    fn item(id: &str, class_name: &str, content: &str) -> EvidenceItem {
        EvidenceItem {
            id: id.to_string(),
            class_name: class_name.to_string(),
            content: content.to_string(),
        }
    }

    // ---- flow_dna_hash --------------------------------------------------

    /// A `SHACLFlow` is reconstructed from graph links on every read, and two
    /// replicas may enumerate those links in different orders. If that order
    /// reached the hash, the same DNA would hash differently per replica and
    /// every receipt would look minted under foreign DNA.
    ///
    /// Red without the two `sort_by_cached_key` calls in `flow_dna_hash`.
    #[test]
    fn dna_hash_ignores_the_order_states_and_transitions_arrive_in() {
        let states = |a: Value, b: Value| serde_json::json!([a, b]);
        let open = serde_json::json!({ "name": "open", "value": 0.0 });
        let done = serde_json::json!({ "name": "done", "value": 1.0 });
        let finish = serde_json::json!({ "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] });
        let reopen = serde_json::json!({ "action_name": "Reopen", "from_state": "done", "to_state": "open", "actions": [] });

        let one = flow_json(
            states(open.clone(), done.clone()),
            serde_json::json!([finish.clone(), reopen.clone()]),
        );
        let other = flow_json(states(done, open), serde_json::json!([reopen, finish]));

        assert_eq!(
            flow_dna_hash(&one).expect("hash"),
            flow_dna_hash(&other).expect("hash"),
            "the same DNA read in a different link order must have the same name"
        );
    }

    /// Two transitions may legitimately share `(from_state, to_state)` and
    /// differ in `action_name`. Sorting on the endpoints alone leaves their
    /// relative order to the store, because a stable sort keeps ties as they
    /// came in.
    ///
    /// Red with `canonical_json(t)` dropped from the transition sort key.
    #[test]
    fn dna_hash_breaks_ties_between_transitions_with_the_same_endpoints() {
        let ship = serde_json::json!({ "action_name": "Ship", "from_state": "open", "to_state": "done", "actions": [] });
        let cancel = serde_json::json!({ "action_name": "Cancel", "from_state": "open", "to_state": "done", "actions": [] });
        let states = serde_json::json!([
            { "name": "open", "value": 0.0 },
            { "name": "done", "value": 1.0 },
        ]);

        let one = flow_json(
            states.clone(),
            serde_json::json!([ship.clone(), cancel.clone()]),
        );
        let other = flow_json(states, serde_json::json!([cancel, ship]));

        assert_eq!(
            flow_dna_hash(&one).expect("hash"),
            flow_dna_hash(&other).expect("hash"),
            "two transitions with the same endpoints must not leave the hash to link order"
        );
    }

    /// Editing the DNA is editing the space's identity — the hash has to move
    /// with it, or the `DnaChanged` signal the verifier owes a reader could
    /// never fire.
    ///
    /// Red if `flow_dna_hash` digests only the flow's identity rather than its
    /// content — e.g. `Sha256::digest(flow.flow_uri())` instead of the
    /// canonical JSON of the whole (sorted) definition.
    #[test]
    fn dna_hash_changes_when_the_quorum_rule_changes() {
        let with_rule = |n: u32| {
            flow_json(
                serde_json::json!([
                    { "name": "open", "value": 0.0 },
                    { "name": "done", "value": 1.0, "consensusRule": { "n": n } },
                ]),
                serde_json::json!([
                    { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
                ]),
            )
        };
        assert_ne!(
            flow_dna_hash(&with_rule(1)).expect("hash"),
            flow_dna_hash(&with_rule(2)).expect("hash"),
            "a different quorum rule is different social DNA"
        );
    }

    // ---- the seal preimage ----------------------------------------------

    /// `evidence_hash` frames the guard's class names into the digest before
    /// the items, and a negative guard (`count: { max: 0 }`) is satisfied by
    /// zero matches — so a class name can be part of a seal with no item
    /// behind it, and a preimage carrying only items could not be re-hashed.
    /// This is why `EvidencePreimage` carries `class_names`.
    ///
    /// Red if `rehashes_to_seal` hashes `&[]` instead of `self.class_names`.
    #[test]
    fn a_negative_guards_class_name_is_part_of_the_seal() {
        let negative = preimage(&["coasys://Blocker"], Vec::new());
        assert!(
            negative.rehashes_to_seal(),
            "a satisfied negative guard's seal must be reproducible from its class name alone"
        );
        assert_ne!(
            negative.seal,
            evidence_hash(&[], &[]),
            "a guard that matched nothing is not the same as no guard at all"
        );
    }

    /// Red if `mint` skips the `rehashes_to_seal` check: the receipt would
    /// mint and then fail its own verification.
    #[test]
    fn mint_refuses_a_preimage_that_does_not_hash_to_its_seal() {
        let mut tampered = preimage(
            &["coasys://Agreement"],
            vec![item("a1", "coasys://Agreement", "{\"id\":\"a1\"}")],
        );
        tampered.items[0].content = "{\"id\":\"a1\",\"approved\":false}".to_string();

        let err = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            outs(&[OUTPUT]),
            vec![delivered(), tampered],
        )
        .expect_err("a preimage that does not re-hash to its seal must not mint");
        assert!(
            format!("{err:#}").contains("does not re-hash"),
            "the error must name the seal mismatch, got: {err:#}"
        );
    }

    // ---- mint's refusals -------------------------------------------------

    /// The happy path — and the only test that can catch `mint` *asserting* a
    /// state instead of folding for one, since every other mint test asserts a
    /// refusal. "Derived, never asserted" is this module's whole claim.
    ///
    /// Red with `terminal_state: read_set.genesis.clone()` in place of
    /// `derived.state`.
    #[test]
    fn mint_derives_the_terminal_state_from_the_carried_material() {
        let receipt = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            outs(&[OUTPUT]),
            vec![delivered()],
        )
        .expect("a settled run into a terminal state mints");
        assert_eq!(receipt.terminal_state, "done");
        assert_eq!(receipt.flow_uri, "coasys://DeliveryFlow");
        assert_eq!(receipt.outputs, outs(&[OUTPUT]));
    }

    /// A receipt is a completion claim, not a per-edge event. `open → doing`
    /// settles, but `doing` can still transition out.
    ///
    /// Red without the `is_terminal_state` guard in `mint`.
    #[test]
    fn mint_refuses_a_run_that_has_not_reached_a_terminal_state() {
        let flow = flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "doing", "value": 0.5 },
                { "name": "done", "value": 1.0 },
            ]),
            serde_json::json!([
                { "action_name": "Start", "from_state": "open", "to_state": "doing", "actions": [] },
                { "action_name": "Finish", "from_state": "doing", "to_state": "done", "actions": [] },
            ]),
        );
        let half_way = read_set(
            "open",
            vec![proposal("ad4m://p/1", ALICE, "open", "doing", "seal-1", T1)],
        );

        let err = FlowReceipt::mint(&flow, half_way, outs(&[OUTPUT]), Vec::new())
            .expect_err("an intermediate state is not a completion");
        assert!(
            format!("{err:#}").contains("can still transition out"),
            "the error must say why `doing` is not terminal, got: {err:#}"
        );
    }

    /// Two edges out of `open` both carry quorum: the run is irreversibly
    /// stalled, and anything that pays out on a completed flow must refuse it.
    ///
    /// Red without the `contested` guard in `mint`.
    #[test]
    fn mint_refuses_a_contested_run() {
        let flow = flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0 },
                { "name": "rejected", "value": 1.0 },
            ]),
            serde_json::json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
                { "action_name": "Reject", "from_state": "open", "to_state": "rejected", "actions": [] },
            ]),
        );
        let both = read_set(
            "open",
            vec![
                proposal("ad4m://p/1", ALICE, "open", "done", "seal-1", T1),
                proposal("ad4m://p/2", BOB, "open", "rejected", "seal-2", T2),
            ],
        );

        let err = FlowReceipt::mint(&flow, both, outs(&[OUTPUT]), Vec::new())
            .expect_err("a contested derivation has not completed");
        assert!(
            format!("{err:#}").contains("contested"),
            "the error must name the contention, got: {err:#}"
        );
    }

    /// A receipt with no outputs speaks for nothing, and the `granted_by`
    /// check a verifier runs could never pass. Refused before anything else,
    /// whatever the run committed to.
    ///
    /// Red without the empty check at the top of `mint`.
    #[test]
    fn mint_refuses_a_receipt_with_no_outputs() {
        let empty_commitment = read_set(
            "open",
            vec![committing(
                "ad4m://p/1",
                ALICE,
                "open",
                "done",
                &delivered().seal,
                &[],
                &hash_of(&[]),
                T1,
            )],
        );
        let err = FlowReceipt::mint(&two_state_flow(), empty_commitment, Vec::new(), Vec::new())
            .expect_err("a receipt with no outputs speaks for nothing");
        assert!(
            format!("{err:#}").contains("no outputs to speak for"),
            "the error must name the missing binding, got: {err:#}"
        );
    }

    /// Outputs no longer come from `requires`: an unguarded terminal state
    /// binds outputs exactly like a guarded one, and no preimage is needed
    /// because its seal is over an empty bag.
    ///
    /// Red if `mint` refuses an unguarded terminal state, e.g. if the pre-
    /// rework `requires` check comes back.
    #[test]
    fn mint_binds_outputs_on_a_terminal_state_with_no_requires() {
        let unguarded = flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0 },
            ]),
            serde_json::json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        );
        let rs = read_set(
            "open",
            vec![committing(
                "ad4m://p/1",
                ALICE,
                "open",
                "done",
                &evidence_hash(&[], &[]),
                &[OUTPUT],
                &hash_of(&[OUTPUT]),
                T1,
            )],
        );
        let receipt =
            FlowReceipt::mint(&unguarded, rs, outs(&[OUTPUT]), Vec::new()).expect("mints");
        assert_eq!(receipt.outputs, outs(&[OUTPUT]));
    }

    /// `mint` writes the outputs sorted and deduplicated, whatever order the
    /// caller listed them in, so two mints of one run carry one list.
    ///
    /// Red if `mint` stores `outputs` as passed.
    #[test]
    fn mint_writes_the_outputs_sorted_and_deduplicated() {
        let three = [OUTPUT, "ad4m://deliverable/d2", "ad4m://deliverable/d3"];
        let rs = read_set(
            "open",
            vec![committing(
                "ad4m://p/1",
                ALICE,
                "open",
                "done",
                &delivered().seal,
                &three,
                &hash_of(&three),
                T1,
            )],
        );
        let receipt = FlowReceipt::mint(
            &two_state_flow(),
            rs,
            outs(&[
                "ad4m://deliverable/d3",
                OUTPUT,
                "ad4m://deliverable/d2",
                OUTPUT,
            ]),
            vec![delivered()],
        )
        .expect("mints");
        assert_eq!(receipt.outputs, outs(&three));
    }

    /// The #1104 re-mint, from the mint side: the run's material is public,
    /// but a mint naming a node the quorum did not commit to is refused.
    ///
    /// Red if `mint` skips the comparison with the final edge's commitment.
    #[test]
    fn mint_refuses_outputs_the_final_edge_did_not_commit_to() {
        let err = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            outs(&[ATTACKER]),
            vec![delivered()],
        )
        .expect_err("a re-mint naming another node must not mint");
        assert!(
            format!("{err:#}").contains(&format!(
                "but the final edge's quorum committed to `{}`",
                hash_of(&[OUTPUT])
            )),
            "the error must name the commitment, got: {err:#}"
        );
    }

    /// The quorum committed to d1 as it stood. A mint carrying d1 with other
    /// content (edited since completion, or made up) is refused, although it
    /// names the same output.
    ///
    /// Red if `mint` hashes the refs instead of the carried content.
    #[test]
    fn mint_refuses_an_output_whose_content_is_not_what_was_committed() {
        let mut edited = out_item(OUTPUT);
        edited.content = serde_json::json!({ "id": OUTPUT, "title": "edited" }).to_string();
        let err = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            vec![edited],
            vec![delivered()],
        )
        .expect_err("edited content must not mint");
        assert!(
            format!("{err:#}").contains(&format!(
                "but the final edge's quorum committed to `{}`",
                hash_of(&[OUTPUT])
            )),
            "the error must name the commitment, got: {err:#}"
        );
    }

    /// One output given twice with two contents is ambiguous: `mint` refuses
    /// rather than picking one, before it compares anything.
    ///
    /// Red if `mint` deduplicates by `(class, id)` and keeps either content.
    #[test]
    fn mint_refuses_one_output_given_twice_with_different_content() {
        let mut edited = out_item(OUTPUT);
        edited.content = serde_json::json!({ "id": OUTPUT, "title": "edited" }).to_string();
        let err = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            vec![out_item(OUTPUT), edited],
            vec![delivered()],
        )
        .expect_err("two contents for one output must not mint");
        assert!(
            format!("{err:#}").contains("is given twice with different content"),
            "got: {err:#}"
        );
    }

    /// Only the edge into the terminal state commits to outputs. The earlier
    /// edge's proposal carries no `outputs_hash` at all, which is correct
    /// for a non-terminal state and must not block the mint.
    ///
    /// Red with `derived.settled.first()` in place of `.last()` in
    /// `final_edge_commitment`: the earlier atom reads as `Uncommitted`.
    #[test]
    fn mint_reads_the_commitment_off_the_final_edge_not_an_earlier_one() {
        let flow = flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "doing", "value": 0.5 },
                { "name": "done", "value": 1.0 },
            ]),
            serde_json::json!([
                { "action_name": "Start", "from_state": "open", "to_state": "doing", "actions": [] },
                { "action_name": "Finish", "from_state": "doing", "to_state": "done", "actions": [] },
            ]),
        );
        let rs = read_set(
            "open",
            vec![
                proposal("ad4m://p/1", ALICE, "open", "doing", "seal-1", T1),
                committing(
                    "ad4m://p/2",
                    BOB,
                    "doing",
                    "done",
                    "seal-2",
                    &[OUTPUT],
                    &hash_of(&[OUTPUT]),
                    T2,
                ),
            ],
        );
        let receipt = FlowReceipt::mint(&flow, rs, outs(&[OUTPUT]), Vec::new()).expect("mints");
        assert_eq!(receipt.outputs, outs(&[OUTPUT]));
    }

    /// A final-edge proposal with no commitment binds nothing, so a receipt
    /// for its run cannot be minted. Its voters agreed to no outputs.
    ///
    /// Red if `final_edge_commitment` skips an atom with no `outputs_hash`
    /// instead of reporting it.
    #[test]
    fn mint_refuses_a_final_edge_that_committed_to_no_outputs() {
        let rs = read_set(
            "open",
            vec![proposal(
                "ad4m://p/1",
                ALICE,
                "open",
                "done",
                &delivered().seal,
                T1,
            )],
        );
        assert_eq!(
            final_edge_commitment(
                &fold_read_set(&two_state_flow(), &rs.reverified()).expect("folds"),
                &rs.reverified()
            ),
            OutputsCommitment::Uncommitted {
                proposal_uri: "ad4m://p/1".into()
            }
        );
        let err = FlowReceipt::mint(&two_state_flow(), rs, outs(&[OUTPUT]), vec![delivered()])
            .expect_err("no commitment, no receipt");
        assert!(
            format!("{err:#}").contains("carries no `ad4m://flow/outputs_hash`"),
            "got: {err:#}"
        );
    }

    /// Twin atoms on the final edge that commit to different outputs: no one
    /// set was agreed by the whole quorum, and `mint` refuses whichever set
    /// the caller names.
    ///
    /// Red if `final_edge_commitment` reads only the first counted atom's
    /// hash.
    #[test]
    fn mint_refuses_twin_final_edge_atoms_with_different_commitments() {
        let flow = flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0, "consensusRule": { "n": 2 } },
            ]),
            serde_json::json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        );
        let seal = evidence_hash(&[], &[]);
        let rs = read_set(
            "open",
            vec![
                committing(
                    "ad4m://p/1",
                    ALICE,
                    "open",
                    "done",
                    &seal,
                    &[OUTPUT],
                    &hash_of(&[OUTPUT]),
                    T1,
                ),
                committing(
                    "ad4m://p/2",
                    BOB,
                    "open",
                    "done",
                    &seal,
                    &[ATTACKER],
                    &hash_of(&[ATTACKER]),
                    T2,
                ),
            ],
        );
        for named in [[OUTPUT], [ATTACKER]] {
            let err = FlowReceipt::mint(&flow, rs.clone(), outs(&named), Vec::new())
                .expect_err("conflicting commitments bind nothing");
            assert!(
                format!("{err:#}").contains("commit to different outputs"),
                "naming {named:?}, got: {err:#}"
            );
        }
    }

    /// A caller can hand `mint` a read-set whose `genesis` points anywhere,
    /// and the fold starts walking wherever it points. Planted at `done`, an
    /// empty read-set "completes" with zero votes behind it (r4077689141).
    ///
    /// Red without the genesis check in `fold_read_set`.
    #[test]
    fn mint_refuses_a_genesis_that_is_not_the_flows_initial_state() {
        let err = FlowReceipt::mint(
            &two_state_flow(),
            read_set("done", Vec::new()),
            outs(&[OUTPUT]),
            vec![delivered()],
        )
        .expect_err("a walk that starts at the finish line is not a completion");
        assert!(
            format!("{err:#}").contains("genesis"),
            "the error must name the planted genesis, got: {err:#}"
        );
    }

    /// Over the cap there is no spill protocol, so the completion is logged
    /// and not minted rather than written half-carried.
    ///
    /// Red without the `MAX_RECEIPT_BYTES` check in `mint`.
    #[test]
    fn mint_refuses_a_receipt_over_the_size_cap() {
        let bulky = preimage(
            &["coasys://Transcript"],
            vec![item(
                "t1",
                "coasys://Transcript",
                &"x".repeat(MAX_RECEIPT_BYTES + 1),
            )],
        );
        let err = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            outs(&[OUTPUT]),
            vec![delivered(), bulky],
        )
        .expect_err("a receipt over the cap is not minted");
        assert!(
            format!("{err:#}").contains("byte cap"),
            "the error must name the cap, got: {err:#}"
        );
    }

    // ---- identity and transport ------------------------------------------

    /// Two replicas minting the same completion must write one node, and a
    /// receipt whose material differs must be a different node — otherwise a
    /// URI could name material other than its own.
    ///
    /// Red if `uri()` returns anything not derived from the content.
    #[test]
    fn the_receipt_uri_is_derived_from_its_content() {
        let flow = two_state_flow();
        let mint = |rs: ReadSet| {
            FlowReceipt::mint(&flow, rs, outs(&[OUTPUT]), vec![delivered()])
                .expect("mints")
                .uri()
                .expect("uri")
        };
        let twin = mint(completed());
        let same = mint(completed());
        let later = mint(read_set("open", vec![final_proposal(T2)]));

        assert_eq!(twin, same, "the same material must collapse onto one node");
        assert_ne!(
            twin, later,
            "a different vote timestamp is different material and a different node"
        );
        assert!(twin.starts_with(RECEIPT_URI_PREFIX), "got {twin}");
    }

    /// A receipt only does its job off-perspective, so the body written to
    /// the graph has to read back identical — including the carried links and
    /// the evidence preimage.
    ///
    /// Red if `body()` silently drops a field on the way out — e.g. clearing
    /// `evidence_preimage` before serialising, which compiles fine and leaves
    /// a receipt that travelled unable to prove its own seals. Also red (as a
    /// compile error) if any field drops its `Deserialize`, e.g.
    /// `EvidenceItem`.
    #[test]
    fn a_receipt_round_trips_through_its_stored_body() {
        let receipt = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            outs(&[OUTPUT]),
            vec![delivered()],
        )
        .expect("mints");

        let read_back: FlowReceipt =
            serde_json::from_str(&receipt.body().expect("body")).expect("a stored receipt parses");
        assert_eq!(read_back, receipt);
        assert_eq!(
            read_back.uri().expect("uri"),
            receipt.uri().expect("uri"),
            "a receipt that travelled must still name itself the same"
        );
    }

    // ---- which atoms a mint has to collect evidence for -------------------

    /// Only atoms the fold counted are part of the claim. A proposal for an
    /// edge the flow never declared contributes no seal, and two atoms
    /// sharing a seal need it carried once.
    ///
    /// Red if `counted_seals` returns every atom in the read-set.
    #[test]
    fn counted_seals_covers_the_settled_atoms_once_each() {
        let flow = flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0, "consensusRule": { "n": 2 } },
                { "name": "rejected", "value": 1.0 },
            ]),
            // `open → rejected` is deliberately NOT declared: an undeclared
            // edge never settles, however many votes it collects.
            serde_json::json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        );
        // Twin proposals on one edge: `{n: 2}` counts distinct voters across
        // them, so both are counted and they share one seal.
        let rs = read_set(
            "open",
            vec![
                proposal("ad4m://p/1", ALICE, "open", "done", "seal-1", T1),
                proposal("ad4m://p/2", BOB, "open", "done", "seal-1", T2),
                proposal("ad4m://p/3", ALICE, "open", "rejected", "seal-never", T2),
            ],
        );

        let counted = FlowReceipt::counted_seals(&flow, &rs).expect("folds");
        let seals: Vec<&str> = counted.iter().map(|c| c.seal.as_str()).collect();
        assert_eq!(
            seals,
            vec!["seal-1"],
            "one entry per distinct counted seal, and nothing for an edge the walk never took"
        );
        assert_eq!(counted[0].to_state, "done");
        assert_eq!(counted[0].proposer, did_of(ALICE));
    }
}
