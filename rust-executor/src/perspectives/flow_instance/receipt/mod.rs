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

pub mod commitment;
pub mod dna;
pub mod mint;
#[cfg(test)]
mod test_support;

pub use commitment::{final_edge_commitment, OutputsCommitment};
pub use dna::{flow_dna_hash, is_terminal_state};

use super::ReadSet;
use crate::perspectives::flow_evaluator::{canonical_json, evidence_hash, EvidenceItem};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

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

impl FlowReceipt {
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
}

#[cfg(test)]
mod tests {
    use super::test_support::*;
    use super::*;
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
            GrantContext::empty(),
        )
        .expect_err("a preimage that does not re-hash to its seal must not mint");
        assert!(
            format!("{err:#}").contains("does not re-hash"),
            "the error must name the seal mismatch, got: {err:#}"
        );
    }

    /// Two replicas minting the same completion must write one node, and a
    /// receipt whose material differs must be a different node — otherwise a
    /// URI could name material other than its own.
    ///
    /// Red if `uri()` returns anything not derived from the content.
    #[test]
    fn the_receipt_uri_is_derived_from_its_content() {
        let flow = two_state_flow();
        let mint = |rs: ReadSet| {
            FlowReceipt::mint(
                &flow,
                rs,
                outs(&[OUTPUT]),
                vec![delivered()],
                GrantContext::empty(),
            )
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
            GrantContext::empty(),
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
}
