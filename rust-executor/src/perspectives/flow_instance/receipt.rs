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
//!   ├── outputs             the binding: which nodes this receipt speaks for
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
//! # What is NOT here
//!
//! - **The verifier.** `verify_receipt` and `ReceiptVerdict` are the next PR.
//!   This module mints, and mint-time validation is written so that what it
//!   produces is exactly what that verifier will accept: an asymmetric rule
//!   would mint receipts that fail their own verification.
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

use super::{fold_read_set, ReadSet};
use crate::perspectives::flow_evaluator::{canonical_json, evidence_hash, EvidenceItem};
use crate::perspectives::shacl_parser::SHACLFlow;
use serde::{Deserialize, Serialize};
use serde_json::Value;
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
    /// The nodes this receipt speaks for — the binding a verifier checks
    /// before honouring a `granted_by` edge. Never empty.
    pub outputs: Vec<String>,
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
    /// `mint` folds the read-set itself under `flow`, so no caller can claim
    /// a state the carried material does not reach.
    ///
    /// Refuses, rather than minting something that would fail its own
    /// verification:
    ///
    /// - the fold does not reach a state with no outgoing transitions — a
    ///   receipt is a completion claim, and an intermediate edge is not one;
    /// - the fold is **contested** — two edges out of the same state both
    ///   carry quorum, and anything that pays out on a completed flow must
    ///   refuse a contested derivation;
    /// - `outputs` is empty — a receipt with no binding speaks for nothing,
    ///   and the `granted_by` check a verifier runs could never pass;
    /// - a preimage does not re-hash to the seal it claims;
    /// - the serialised receipt exceeds [`MAX_RECEIPT_BYTES`].
    pub fn mint(
        flow: &SHACLFlow,
        read_set: ReadSet,
        outputs: Vec<String>,
        evidence_preimage: Vec<EvidencePreimage>,
    ) -> anyhow::Result<FlowReceipt> {
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

        let derived = fold_read_set(flow, &read_set)?;
        if let Some(contested) = derived.contested {
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
    pub fn counted_seals(flow: &SHACLFlow, read_set: &ReadSet) -> anyhow::Result<Vec<CountedAtom>> {
        let derived = fold_read_set(flow, read_set)?;
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CountedAtom {
    pub proposal_uri: String,
    pub to_state: String,
    pub proposer: String,
    pub seal: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::flow_instance::atom::fixtures::{honest_proposal, ALICE, BOB, T1, T2};
    use crate::perspectives::flow_instance::ProposalLinks;

    const INSTANCE: &str = "ad4m://flow/instance/i1";
    const BASE: &str = "ad4m://task/t1";

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

    fn two_state_flow() -> SHACLFlow {
        flow_json(
            serde_json::json!([
                { "name": "open", "value": 0.0 },
                { "name": "done", "value": 1.0 },
            ]),
            serde_json::json!([
                { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
            ]),
        )
    }

    /// One proposal, self-proposed and therefore self-voted: under the
    /// default `{ n: 1 }` rule that is a settled edge.
    fn proposal(uri: &str, proposer: &str, from: &str, to: &str, seal: &str, at: &str) -> ProposalLinks {
        ProposalLinks {
            uri: uri.to_string(),
            links: honest_proposal(proposer, from, to, seal, at),
        }
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
        read_set(
            "open",
            vec![proposal("ad4m://p/1", ALICE, "open", "done", "seal-1", T1)],
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

        let one = flow_json(states.clone(), serde_json::json!([ship.clone(), cancel.clone()]));
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
    /// Red if `flow_dna_hash` returns anything constant.
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
        let mut tampered = preimage(&["coasys://Agreement"], vec![item("a1", "coasys://Agreement", "{\"id\":\"a1\"}")]);
        tampered.items[0].content = "{\"id\":\"a1\",\"approved\":false}".to_string();

        let err = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            vec![BASE.to_string()],
            vec![tampered],
        )
        .expect_err("a preimage that does not re-hash to its seal must not mint");
        assert!(
            format!("{err:#}").contains("does not re-hash"),
            "the error must name the seal mismatch, got: {err:#}"
        );
    }

    // ---- mint's refusals -------------------------------------------------

    #[test]
    fn mint_derives_the_terminal_state_from_the_carried_material() {
        let receipt = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            vec![BASE.to_string()],
            Vec::new(),
        )
        .expect("a settled run into a terminal state mints");
        assert_eq!(receipt.terminal_state, "done");
        assert_eq!(receipt.flow_uri, "coasys://DeliveryFlow");
        assert_eq!(receipt.outputs, vec![BASE.to_string()]);
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

        let err = FlowReceipt::mint(&flow, half_way, vec![BASE.to_string()], Vec::new())
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

        let err = FlowReceipt::mint(&flow, both, vec![BASE.to_string()], Vec::new())
            .expect_err("a contested derivation has not completed");
        assert!(
            format!("{err:#}").contains("contested"),
            "the error must name the contention, got: {err:#}"
        );
    }

    /// The `granted_by` edge is untrusted; the binding a verifier honours is
    /// the receipt's own output list. An empty one can never be honoured.
    ///
    /// Red without the `outputs.is_empty()` guard in `mint`.
    #[test]
    fn mint_refuses_a_receipt_that_binds_to_nothing() {
        let err = FlowReceipt::mint(&two_state_flow(), completed(), Vec::new(), Vec::new())
            .expect_err("a receipt with no outputs speaks for nothing");
        assert!(
            format!("{err:#}").contains("no outputs"),
            "the error must name the missing binding, got: {err:#}"
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
            vec![BASE.to_string()],
            vec![bulky],
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
            FlowReceipt::mint(&flow, rs, vec![BASE.to_string()], Vec::new())
                .expect("mints")
                .uri()
                .expect("uri")
        };
        let twin = mint(completed());
        let same = mint(completed());
        let later = mint(read_set(
            "open",
            vec![proposal("ad4m://p/1", ALICE, "open", "done", "seal-1", T2)],
        ));

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
    /// Red if any field drops its `Deserialize` (e.g. `EvidenceItem`).
    #[test]
    fn a_receipt_round_trips_through_its_stored_body() {
        let receipt = FlowReceipt::mint(
            &two_state_flow(),
            completed(),
            vec![BASE.to_string()],
            vec![preimage(
                &["coasys://Agreement"],
                vec![item("a1", "coasys://Agreement", "{\"id\":\"a1\"}")],
            )],
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
        assert_eq!(counted[0].proposer, ALICE);
    }
}
