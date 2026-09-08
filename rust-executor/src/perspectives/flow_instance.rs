//! A running flow's state, **derived** rather than read.
//!
//! # Why this module exists
//!
//! A flow's state used to be whatever the `ad4m://flow/current_state` link
//! said. Any neighbourhood member can write that link
//! (`perspective_instance::diff_from_link_language` persists inbound diffs
//! without an ownership rule), so the whole consensus ceremony — proposer
//! signature, co-signatures, evidence seal, quorum, declared edge —
//! protected only the *write* of the state, never the *read*. One forged
//! link moved a flow, deleted an honest frontier as "superseded", or
//! suppressed every mint.
//!
//! Here the state is a **fold**: start at the flow definition's initial
//! state and replay the transition atoms this replica can re-verify
//! itself. `currentState` stays on the graph as a write-through cache that
//! no engine path reads (see [`FlowInstanceRecord::cached_state`]).
//!
//! # What a replica re-verifies on every read
//!
//! Every field of an atom is read only from links **authored by the
//! proposer with a valid signature**, and only when the proposer authored
//! exactly one distinct value for it. That closes two forgeries that an
//! author-string check alone lets through:
//!
//! - **Signature validity.** Every synced link carries a stored
//!   `proof.valid` verdict (`sparql_store`), and comparing `author` alone
//!   accepts a link that claims Alice's DID over garbage bytes.
//!   [`signed_by`] is the one identity primitive; nothing else in the flow
//!   engine compares authors.
//! - **Field override.** Model hydration is last-timestamp-wins across
//!   *all* authors, so a peer could append a later `to_state` to someone
//!   else's honest proposal and have the engine read the peer's value
//!   while still attributing the proposal to its author. [`unique_field`]
//!   ignores every link the proposer did not author.
//!
//! # What this does NOT close
//!
//! Any member can remove any link, including links they did not author
//! (`perspective_instance::remove_links` matches on
//! `(source, predicate, target, author, timestamp)` with no ownership
//! rule). Deleting a fired atom removes a step from every replica's fold.
//! No fold can defend against the disappearance of its own inputs; that is
//! a link-language property and is tracked as an accepted gap (see the
//! `#[ignore]`d characterisation test in `flow_instance_e2e`).

use crate::perspectives::flow_consensus::resolve_role_dids;
use crate::perspectives::flow_context::{load_all_flow_instances, FlowInstanceRecord};
use crate::perspectives::flow_spawn::initial_state_of;
use crate::perspectives::model_query::utils::parse_literal_value;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::{ConsensusRule, SHACLFlow};
use crate::types::{DecoratedLinkExpression, LinkQuery};
use std::collections::{BTreeMap, BTreeSet, HashMap};

// ---------------------------------------------------------------------------
// The link vocabulary of a flow proposal. One home, because this module is
// the only place that reads it.
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
/// Proposal → an accepting DID. A vote counts only when the link's author
/// IS the DID it names (see [`valid_acceptors`]).
pub const ACCEPTED_BY_PREDICATE: &str = "ad4m://acceptedBy";
/// Proposal → how a firing consumed it. Absence = live.
pub const RESOLVED_AS_PREDICATE: &str = "ad4m://flow/resolved_as";
/// The only [`RESOLVED_AS_PREDICATE`] value the engine writes.
pub const FIRED_MARK: &str = "fired";

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// A running flow: its identity row plus the definition it runs.
///
/// Borrows the definition from the caller's catalogue — flow definitions
/// are read once per pass and shared across every instance of that flow.
#[derive(Debug)]
pub struct FlowInstance<'a> {
    pub uri: String,
    pub subject: String,
    pub flow: &'a SHACLFlow,
    /// The on-graph `currentState` link. Compared against the fold to warn
    /// about tampering; never an input to any decision.
    pub cached_state: Option<String>,
}

/// One identity-checked, sealed proposal together with its valid votes.
///
/// Every field here came from a link the proposer authored with a valid
/// signature, so a third party cannot change what an atom says without
/// forging the proposer's key.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TransitionAtom {
    pub uri: String,
    pub from_state: String,
    pub to_state: String,
    pub proposer: String,
    /// Earliest timestamp among the **proposer's own** links on this
    /// proposal. Third-party links are excluded so a peer cannot re-order
    /// someone else's atom by back-dating a link onto it.
    pub proposed_at: String,
    /// Non-empty by construction (an empty seal is [`AtomRejection::EmptySeal`]).
    pub evidence_hash: String,
    /// Self-authored, signature-valid `acceptedBy` DIDs, sorted and deduped.
    pub acceptors: Vec<String>,
    /// Whether a `resolved_as → "fired"` link exists, from any author. An
    /// index hint only: it decides which proposals the fold *considers* as
    /// history, and every one of them is then re-verified from scratch.
    pub marked_fired: bool,
}

impl TransitionAtom {
    /// Every DID vouching for this atom: proposer + acceptors.
    pub fn qualifying_dids(&self) -> impl Iterator<Item = &String> {
        std::iter::once(&self.proposer).chain(self.acceptors.iter())
    }
}

/// Why a proposal is not an atom. Logged and tested; never acted on by
/// deleting, except for [`AtomRejection::EmptySeal`] (see
/// [`AtomBag::unsealed_live`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AtomRejection {
    /// No `proposer → d` link signed by `d` itself.
    NoValidProposerLink,
    /// Two DIDs each self-claim to be the proposer.
    AmbiguousProposer,
    /// The proposer authored no valid link on this predicate.
    MissingField(&'static str),
    /// The proposer authored two distinct values for this predicate.
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
                "no `{PROPOSER_PREDICATE}` link authored, with a valid signature, by the DID it names"
            ),
            Self::AmbiguousProposer => write!(f, "two DIDs each self-claim to be the proposer"),
            Self::MissingField(p) => write!(f, "the proposer authored no valid `{p}` link"),
            Self::AmbiguousField(p) => {
                write!(f, "the proposer authored two distinct `{p}` values")
            }
            Self::WrongInstance => write!(
                f,
                "the proposer's `{FLOW_INSTANCE_PREDICATE}` link names a different flow instance"
            ),
            Self::EmptySeal => write!(f, "the evidence seal is empty"),
        }
    }
}

/// A proposal the identity checks turned away, with enough context for the
/// pass to decide whether it is cleanable noise.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RejectedProposal {
    pub uri: String,
    pub marked_fired: bool,
    pub reason: AtomRejection,
}

/// One atom with its eligible voters already resolved. Role resolution
/// needs the store; the fold is pure, so the caller resolves first.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VouchedAtom {
    pub atom: TransitionAtom,
    /// `{proposer} ∪ acceptors`, gated by the rule's `fromRole`. Sorted.
    pub eligible: Vec<String>,
}

/// One consensus event the fold accepted: the atoms that together reached
/// quorum on one declared edge.
///
/// A shell holds *atoms*, plural, because quorum is a property of the
/// bucket, not of a single proposal: two replicas each minting their own
/// proposal for the same edge is how an `{n: 2}` flow reaches consensus
/// without a human accept, and the firing pass marks all of them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Shell {
    pub from_state: String,
    pub to_state: String,
    pub atoms: Vec<TransitionAtom>,
    /// Union of the contributing atoms' eligible voters. Sorted, deduped.
    pub eligible_voters: Vec<String>,
}

/// The result of the fold: the authoritative state of a flow instance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DerivedState {
    pub state: String,
    /// The accepted chain, in the order it was replayed.
    pub shells: Vec<Shell>,
    /// `Some(cached == state)` when the instance carries a cache link.
    /// `None` from the pure [`fold`], which has no cache to compare with.
    pub cache_agrees: Option<bool>,
}

/// Every proposal on one instance, sorted into atoms and rejections. One
/// load; the fold, the frontier and the superseded set are views on it.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct AtomBag {
    pub atoms: Vec<TransitionAtom>,
    pub rejected: Vec<RejectedProposal>,
}

impl AtomBag {
    /// Atoms the graph marks as fired — the fold's candidate history.
    pub fn marked(&self) -> impl Iterator<Item = &TransitionAtom> {
        self.atoms.iter().filter(|a| a.marked_fired)
    }

    /// Unmarked atoms that leave `state` — the consensus pass's input.
    pub fn frontier(&self, state: &str) -> Vec<TransitionAtom> {
        self.atoms
            .iter()
            .filter(|a| !a.marked_fired && a.from_state == state)
            .cloned()
            .collect()
    }

    /// Unmarked atoms that leave some *other* state: the flow moved on
    /// under them, so the pass deletes them (auto-invalidation trigger a).
    pub fn superseded(&self, state: &str) -> Vec<&TransitionAtom> {
        self.atoms
            .iter()
            .filter(|a| !a.marked_fired && a.from_state != state)
            .collect()
    }

    /// Live proposals whose only defect is an empty seal. Unverifiable and
    /// countable-with-zero-evidence if anything ever counted them, so the
    /// pass deletes them exactly as it did before the fold existed.
    /// Marked ones are left alone: history is never deleted.
    pub fn unsealed_live(&self) -> Vec<&str> {
        self.rejected
            .iter()
            .filter(|r| !r.marked_fired && r.reason == AtomRejection::EmptySeal)
            .map(|r| r.uri.as_str())
            .collect()
    }
}

// ---------------------------------------------------------------------------
// Identity primitives (pure)
// ---------------------------------------------------------------------------

/// The one identity check in the flow engine: this link was authored by
/// `did` **and** its signature verifies.
///
/// The validity half is what stops a peer from syncing in a link that
/// claims `author: did:key:alice` over arbitrary bytes; the executor
/// stores such a link with `proof.valid = Some(false)` and it must not
/// count as Alice for anything.
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
/// content unforgeable by a third party. Two distinct proposer-authored
/// values are a rejection, never a "pick the latest": last-write-wins is
/// exactly the rule that let a peer re-point someone else's proposal.
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

/// The DIDs that voted for this proposal: `acceptedBy → a` links signed by
/// `a`. Sorted and deduped.
///
/// A vote is an authorship claim, not a data claim — otherwise one agent
/// could write `acceptedBy → did:key:X` links for DIDs it does not control
/// and clear an `{n}` quorum alone.
pub fn valid_acceptors(links: &[DecoratedLinkExpression]) -> Vec<String> {
    let mut dids: Vec<String> = links_on(links, ACCEPTED_BY_PREDICATE)
        .filter(|l| signed_by(l, &l.data.target))
        .map(|l| l.data.target.clone())
        .collect();
    dids.sort();
    dids.dedup();
    dids
}

/// Whether some agent whose signature verifies marked this proposal as
/// fired.
///
/// The mark is an **index, not a grant of authority**: every other check on
/// the atom still runs, so a mark cannot make an unqualified proposal fold.
/// What it can do is bring a *qualified* proposal into history earlier than
/// the live pass would have — that pass re-checks the cited evidence
/// (Clock A) before firing, and the fold does not. Requiring the mark's
/// signature to verify keeps out marks fabricated in another agent's name,
/// but any member can still write one under their own DID, so the window
/// stands: mint, edit the cited evidence, mark before the pass runs.
///
/// That is the same class as the deletion gap — an index any member may
/// write — and closing it needs a rule about who may write the mark, which
/// is a design question beyond this slice.
pub fn marked_fired(links: &[DecoratedLinkExpression]) -> bool {
    links_on(links, RESOLVED_AS_PREDICATE)
        .any(|l| l.proof.valid == Some(true) && field_value(&l.data.target) == FIRED_MARK)
}

impl TransitionAtom {
    /// Build an atom from one proposal's raw links, or say why it is not
    /// one. `links` is every link with the proposal as source, from every
    /// author — the filtering is the point of this function.
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
        Ok(TransitionAtom {
            uri: uri.to_string(),
            from_state: unique_field(links, FROM_STATE_PREDICATE, &proposer)?,
            to_state: unique_field(links, TO_STATE_PREDICATE, &proposer)?,
            proposed_at: earliest_proposer_timestamp(links, &proposer),
            proposer,
            evidence_hash,
            acceptors: valid_acceptors(links),
            marked_fired: marked_fired(links),
        })
    }
}

/// Earliest timestamp among the proposer's own valid links. Never empty in
/// practice: the proposer link that named them is one of these.
fn earliest_proposer_timestamp(links: &[DecoratedLinkExpression], proposer: &str) -> String {
    links
        .iter()
        .filter(|l| signed_by(l, proposer))
        .map(|l| l.timestamp.clone())
        .min()
        .unwrap_or_default()
}

// ---------------------------------------------------------------------------
// Checking one edge
// ---------------------------------------------------------------------------

/// Whether the definition declares `from → to`. Only the declared graph
/// may fire, however a proposal arrived.
pub fn declares_edge(flow: &SHACLFlow, from: &str, to: &str) -> bool {
    flow.transitions
        .iter()
        .any(|t| t.from_state == from && t.to_state == to)
}

/// The rule governing a transition INTO `to_state`: the target state's own
/// `consensusRule` wins, else the flow-level one, else `{ n: 1 }`.
pub fn rule_for(flow: &SHACLFlow, to_state: &str) -> ConsensusRule {
    effective_consensus_rule(flow, to_state)
        .cloned()
        .unwrap_or(ConsensusRule {
            n: 1,
            from_role: None,
        })
}

/// The declared rule, without the default. Separate from [`rule_for`]
/// because the live-firing path passes `Option` on to `aggregate_flow_votes`,
/// which owns the same default and validates `n`.
pub fn effective_consensus_rule<'a>(
    flow: &'a SHACLFlow,
    to_state: &str,
) -> Option<&'a ConsensusRule> {
    flow.states
        .iter()
        .find(|s| s.name == to_state)
        .and_then(|s| s.consensus_rule.as_ref())
        .or(flow.consensus_rule.as_ref())
}

/// Whether enough eligible DIDs vouched for a transition.
///
/// `n = 0` is a misconfigured rule and never satisfies quorum: "everyone
/// passes" is not a safe reading of a threshold nobody set. The live path
/// rejects such a rule loudly (`aggregate_flow_votes`); the fold, which
/// must never fail a whole read over one bad definition, fails closed.
pub fn quorum_met(rule: &ConsensusRule, eligible_count: usize) -> bool {
    rule.n > 0 && eligible_count as u32 >= rule.n
}

/// `{proposer} ∪ acceptors`, gated by the rule's `fromRole`.
///
/// `Err` on a store error or an undeterminable role query — the caller
/// must skip the read rather than fold on a wrong eligible set.
pub async fn eligible_voters(
    perspective: &PerspectiveInstance,
    instance: &FlowInstance<'_>,
    atom: &TransitionAtom,
    rule: &ConsensusRule,
) -> anyhow::Result<Vec<String>> {
    let mut candidates: Vec<String> = atom.qualifying_dids().cloned().collect();
    candidates.sort();
    candidates.dedup();
    let Some(role) = rule.from_role.as_ref() else {
        return Ok(candidates);
    };
    let allowed = resolve_role_dids(perspective, role, &instance.as_record(), &candidates).await?;
    candidates.retain(|d| allowed.contains(d));
    Ok(candidates)
}

// ---------------------------------------------------------------------------
// The fold
// ---------------------------------------------------------------------------

/// Votes accumulated for one candidate edge out of the state the fold is
/// currently standing on.
#[derive(Default)]
struct EdgeVotes {
    atoms: Vec<TransitionAtom>,
    voters: BTreeSet<String>,
}

/// Replay the history atoms into the state they leave the flow in.
///
/// Walks the atoms in `(proposed_at, to_state, uri)` order — deterministic
/// whatever order the store returned them in — and accumulates votes per
/// candidate edge out of the current state. When a declared edge reaches
/// its quorum the flow moves, that bucket becomes a [`Shell`], and the
/// accumulator resets. Atoms that do not leave the current state are
/// skipped: they belong to a branch that never fired or to a visit the
/// fold has already passed.
///
/// The quorum rule is the same one the live firing path applies, so a fold
/// over what the engine fired reproduces exactly the states it walked
/// through — including cycles, where an edge fires once per visit.
pub fn fold(genesis: &str, flow: &SHACLFlow, atoms: Vec<VouchedAtom>) -> DerivedState {
    let mut ordered = atoms;
    ordered.sort_by(|a, b| {
        (&a.atom.proposed_at, &a.atom.to_state, &a.atom.uri).cmp(&(
            &b.atom.proposed_at,
            &b.atom.to_state,
            &b.atom.uri,
        ))
    });

    let mut state = genesis.to_string();
    let mut shells: Vec<Shell> = Vec::new();
    let mut pending: BTreeMap<String, EdgeVotes> = BTreeMap::new();

    for vouched in ordered {
        let to_state = vouched.atom.to_state.clone();
        if vouched.atom.from_state != state || !declares_edge(flow, &state, &to_state) {
            continue;
        }
        let quorum_reached = {
            let votes = pending.entry(to_state.clone()).or_default();
            votes.atoms.push(vouched.atom);
            votes.voters.extend(vouched.eligible);
            quorum_met(&rule_for(flow, &to_state), votes.voters.len())
        };
        if !quorum_reached {
            continue;
        }
        let votes = pending.remove(&to_state).expect("inserted just above");
        shells.push(Shell {
            from_state: state,
            to_state: to_state.clone(),
            atoms: votes.atoms,
            eligible_voters: votes.voters.into_iter().collect(),
        });
        state = to_state;
        pending.clear();
    }

    DerivedState {
        state,
        shells,
        cache_agrees: None,
    }
}

// ---------------------------------------------------------------------------
// Loading
// ---------------------------------------------------------------------------

impl<'a> FlowInstance<'a> {
    /// Pair one already-loaded instance row with its definition.
    pub fn from_record(record: &FlowInstanceRecord, flow: &'a SHACLFlow) -> Self {
        FlowInstance {
            uri: record.instance_uri.clone(),
            subject: record.subject.clone(),
            flow,
            cached_state: record.cached_state.clone(),
        }
    }

    /// Load one instance by URI and pair it with its definition from the
    /// caller's catalogue. `None` when either is absent.
    ///
    /// Enumerates instances and filters, because `model_query` has no
    /// by-id filter; the read surfaces that will call this per instance
    /// (slice 2) should hold the catalogue and the row they already have
    /// and use [`Self::from_record`] instead.
    // Dead until slice 2 moves MCP / the prompt block / the TS wrapper onto
    // the fold; the pass has its rows already and uses `from_record`.
    #[allow(dead_code)]
    pub async fn load(
        perspective: &PerspectiveInstance,
        instance_uri: &str,
        flows_by_uri: &'a HashMap<String, SHACLFlow>,
    ) -> anyhow::Result<Option<FlowInstance<'a>>> {
        let records = load_all_flow_instances(perspective).await?;
        let Some(record) = records.iter().find(|r| r.instance_uri == instance_uri) else {
            return Ok(None);
        };
        Ok(flows_by_uri
            .get(&record.flow_uri)
            .map(|flow| FlowInstance::from_record(record, flow)))
    }

    /// The state a fresh instance of this flow begins in. `None` only for
    /// a zero-state flow, which never has an instance.
    pub fn genesis(&self) -> Option<String> {
        initial_state_of(self.flow)
    }

    /// The flat row the guard and role translators take (`$flow.base`,
    /// `$flow.instance`). Neither reads a state, so the row carries the
    /// genesis state purely to stay a valid `FlowInstanceRecord`.
    pub fn as_record(&self) -> FlowInstanceRecord {
        FlowInstanceRecord {
            flow_uri: self.flow.flow_uri(),
            instance_uri: self.uri.clone(),
            subject: self.subject.clone(),
            state: self.genesis().unwrap_or_default(),
            cached_state: self.cached_state.clone(),
            created_at: None,
        }
    }

    /// Every proposal pointing at this instance, identity-checked.
    pub async fn load_atoms(&self, perspective: &PerspectiveInstance) -> anyhow::Result<AtomBag> {
        let mut bag = AtomBag::default();
        for (uri, links) in load_proposal_links(perspective, &self.uri).await? {
            match TransitionAtom::from_links(&self.uri, &uri, &links) {
                Ok(atom) => bag.atoms.push(atom),
                Err(reason) => {
                    log::debug!(
                        "flow instance {}: proposal {uri} is not an atom — {reason}",
                        self.uri
                    );
                    bag.rejected.push(RejectedProposal {
                        uri,
                        marked_fired: marked_fired(&links),
                        reason,
                    });
                }
            }
        }
        Ok(bag)
    }

    /// The state of this flow: the fold over its re-verified history.
    ///
    /// The only function the rest of the engine may call for "what state
    /// is this flow in". `Err` on a store error during role resolution —
    /// callers must then neither fire nor delete (fail closed).
    // Called by tests and (slice 2) by the read surfaces; the pass needs
    // the bag as well, so it calls `derive_state_from` directly.
    #[allow(dead_code)]
    pub async fn derive_state(
        &self,
        perspective: &PerspectiveInstance,
    ) -> anyhow::Result<DerivedState> {
        let bag = self.load_atoms(perspective).await?;
        self.derive_state_from(perspective, &bag).await
    }

    /// [`Self::derive_state`] over an already-loaded bag, so a caller that
    /// also needs the frontier pays for one load, not two.
    pub async fn derive_state_from(
        &self,
        perspective: &PerspectiveInstance,
        bag: &AtomBag,
    ) -> anyhow::Result<DerivedState> {
        let genesis = self.genesis().ok_or_else(|| {
            anyhow::anyhow!(
                "flow `{}` has no states, so instance {} has no genesis to fold from",
                self.flow.name,
                self.uri
            )
        })?;

        let mut history = Vec::new();
        for atom in bag.marked() {
            let rule = rule_for(self.flow, &atom.to_state);
            history.push(VouchedAtom {
                eligible: eligible_voters(perspective, self, atom, &rule).await?,
                atom: atom.clone(),
            });
        }

        let mut derived = fold(&genesis, self.flow, history);
        derived.cache_agrees = self.cached_state.as_ref().map(|c| *c == derived.state);
        if derived.cache_agrees == Some(false) {
            // Not an error: the cache is written through by the firing
            // replica and a peer may overwrite it at any time. Disagreement
            // means the link is stale or forged, and either way the fold
            // wins — but an operator should see it.
            log::warn!(
                "flow instance {}: cached currentState `{}` disagrees with the derived state `{}`; ignoring the cache",
                self.uri,
                self.cached_state.as_deref().unwrap_or_default(),
                derived.state,
            );
        }
        Ok(derived)
    }
}

/// Enumerate this instance's proposals and read each one's links.
///
/// One `get_links` to find the proposals, then one per proposal for all of
/// its links from every author — the identity checks need the third-party
/// links precisely so they can ignore them.
pub async fn load_proposal_links(
    perspective: &PerspectiveInstance,
    instance_uri: &str,
) -> anyhow::Result<Vec<(String, Vec<DecoratedLinkExpression>)>> {
    if instance_uri.is_empty() {
        return Err(anyhow::anyhow!(
            "load_proposal_links: instance_uri must not be empty"
        ));
    }
    let pointers = perspective
        .get_links(&LinkQuery {
            predicate: Some(FLOW_INSTANCE_PREDICATE.to_string()),
            target: Some(instance_uri.to_string()),
            ..Default::default()
        })
        .await
        .map_err(|e| anyhow::anyhow!("load_proposal_links: proposal lookup failed: {e:#}"))?;

    let mut uris: Vec<String> = pointers.into_iter().map(|l| l.data.source).collect();
    uris.sort();
    uris.dedup();

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
    use super::*;
    use crate::types::{DecoratedExpressionProof, Link};

    const INSTANCE: &str = "ad4m://flow/instance/i1";
    const ALICE: &str = "did:key:alice";
    const BOB: &str = "did:key:bob";
    const MALLORY: &str = "did:key:mallory";
    const T1: &str = "2026-01-01T00:00:00.000Z";
    const T2: &str = "2026-01-02T00:00:00.000Z";
    const T3: &str = "2026-01-03T00:00:00.000Z";

    fn literal(s: &str) -> String {
        format!("literal:string:{}", urlencoding::encode(s))
    }

    /// One link as `get_links` returns it: author, signature verdict and
    /// timestamp are all inputs the checks read, so every fixture states
    /// them explicitly.
    fn link(
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
                source: "ad4m://flow/proposal/p1".to_string(),
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

    /// The five links `write_flow_transition_proposal` emits, all authored
    /// by the proposer with valid signatures.
    fn honest_proposal(
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

    fn atom_of(links: &[DecoratedLinkExpression]) -> Result<TransitionAtom, AtomRejection> {
        TransitionAtom::from_links(INSTANCE, "ad4m://flow/proposal/p1", links)
    }

    // ---- identity and field checks ---------------------------------------

    #[test]
    fn honest_proposal_is_an_atom() {
        let atom = atom_of(&honest_proposal(ALICE, "review", "approved", "h1", T1))
            .expect("engine-minted proposal must be an atom");
        assert_eq!(atom.proposer, ALICE);
        assert_eq!(atom.from_state, "review");
        assert_eq!(atom.to_state, "approved");
        assert_eq!(atom.evidence_hash, "h1");
        assert_eq!(atom.proposed_at, T1);
        assert!(atom.acceptors.is_empty());
        assert!(!atom.marked_fired);
    }

    /// The field-override attack: Mallory appends a LATER `to_state` to
    /// Alice's honest proposal. Model hydration is last-write-wins across
    /// authors, so the pre-fold engine read Mallory's value while still
    /// attributing the proposal to Alice.
    #[test]
    fn foreign_author_field_override_is_ignored() {
        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        links.push(link(
            TO_STATE_PREDICATE,
            &literal("rejected"),
            MALLORY,
            true,
            T3,
        ));
        let atom = atom_of(&links).expect("a foreign link must not invalidate Alice's proposal");
        assert_eq!(atom.to_state, "approved", "only Alice's value may be read");
    }

    /// Same attack, from the proposer's own key: two distinct self-authored
    /// values are ambiguous, and "pick the latest" is the rule this whole
    /// module exists to remove — so the atom is rejected, not resolved.
    #[test]
    fn ambiguous_proposer_field_rejects_atom() {
        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        links.push(link(
            TO_STATE_PREDICATE,
            &literal("rejected"),
            ALICE,
            true,
            T3,
        ));
        assert_eq!(
            atom_of(&links),
            Err(AtomRejection::AmbiguousField(TO_STATE_PREDICATE))
        );
    }

    /// A synced link may claim any author; the executor stores the
    /// signature verdict alongside it. A proposer link whose signature does
    /// not verify is not that DID's proposal.
    #[test]
    fn invalid_signature_proposer_is_not_an_atom() {
        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        links[0] = link(PROPOSER_PREDICATE, ALICE, ALICE, false, T1);
        assert_eq!(atom_of(&links), Err(AtomRejection::NoValidProposerLink));
    }

    #[test]
    fn two_self_claimed_proposers_reject_the_atom() {
        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        links.push(link(PROPOSER_PREDICATE, MALLORY, MALLORY, true, T3));
        assert_eq!(atom_of(&links), Err(AtomRejection::AmbiguousProposer));
    }

    /// A vote is an authorship claim. Both shapes that break it — a link
    /// naming a DID it was not authored by, and a self-named link whose
    /// signature does not verify — must count for nothing.
    #[test]
    fn only_self_authored_valid_votes_count() {
        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        links.push(link(ACCEPTED_BY_PREDICATE, BOB, BOB, true, T2));
        links.push(link(
            ACCEPTED_BY_PREDICATE,
            "did:key:carol",
            MALLORY,
            true,
            T2,
        ));
        links.push(link(
            ACCEPTED_BY_PREDICATE,
            "did:key:dave",
            "did:key:dave",
            false,
            T2,
        ));
        let atom = atom_of(&links).expect("atom");
        assert_eq!(
            atom.acceptors,
            vec![BOB.to_string()],
            "forged and unverifiable votes must not count"
        );
    }

    #[test]
    fn wrong_instance_link_is_not_an_atom() {
        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        links[1] = link(
            FLOW_INSTANCE_PREDICATE,
            "ad4m://flow/instance/other",
            ALICE,
            true,
            T1,
        );
        assert_eq!(atom_of(&links), Err(AtomRejection::WrongInstance));
    }

    #[test]
    fn missing_and_empty_fields_are_named_in_the_rejection() {
        let mut without_to_state = honest_proposal(ALICE, "review", "approved", "h1", T1);
        without_to_state.retain(|l| l.data.predicate.as_deref() != Some(TO_STATE_PREDICATE));
        assert_eq!(
            atom_of(&without_to_state),
            Err(AtomRejection::MissingField(TO_STATE_PREDICATE))
        );

        let unsealed = honest_proposal(ALICE, "review", "approved", "", T1);
        assert_eq!(atom_of(&unsealed), Err(AtomRejection::EmptySeal));
    }

    /// Ordering must not be re-writable by a third party: Mallory adds an
    /// EARLIER-timestamped link to Alice's proposal to win the
    /// earliest-first tie-break. `proposed_at` only sees Alice's links.
    #[test]
    fn proposed_at_ignores_foreign_backdated_links() {
        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T2);
        links.push(link(
            ACCEPTED_BY_PREDICATE,
            MALLORY,
            MALLORY,
            true,
            "2020-01-01T00:00:00.000Z",
        ));
        assert_eq!(atom_of(&links).expect("atom").proposed_at, T2);
    }

    #[test]
    fn a_fired_mark_from_any_author_is_visible_as_history() {
        let mut links = honest_proposal(ALICE, "review", "approved", "h1", T1);
        links.push(link(
            RESOLVED_AS_PREDICATE,
            &literal(FIRED_MARK),
            MALLORY,
            true,
            T3,
        ));
        assert!(
            atom_of(&links).expect("atom").marked_fired,
            "the mark is an index the fold re-verifies, so it is read from any author"
        );
    }

    // ---- the fold --------------------------------------------------------

    /// `review ⇄ changes_requested → approved`, `{n}` configurable on the
    /// `approved` state so quorum cases stay in one fixture.
    fn review_flow(approved_n: Option<u32>) -> SHACLFlow {
        let mut approved = serde_json::json!({ "name": "approved", "value": 1.0 });
        if let Some(n) = approved_n {
            approved["consensusRule"] = serde_json::json!({ "n": n });
        }
        serde_json::from_value(serde_json::json!({
            "name": "Review",
            "namespace": "review://",
            "states": [
                { "name": "review", "value": 0.0 },
                { "name": "changes_requested", "value": 0.5 },
                approved,
            ],
            "transitions": [
                { "action_name": "Request", "from_state": "review", "to_state": "changes_requested", "actions": [] },
                { "action_name": "Resubmit", "from_state": "changes_requested", "to_state": "review", "actions": [] },
                { "action_name": "Approve", "from_state": "review", "to_state": "approved", "actions": [] },
            ],
        }))
        .expect("fixture flow parses")
    }

    fn vouched(uri: &str, from: &str, to: &str, at: &str, voters: &[&str]) -> VouchedAtom {
        VouchedAtom {
            atom: TransitionAtom {
                uri: uri.to_string(),
                from_state: from.to_string(),
                to_state: to.to_string(),
                proposer: voters.first().copied().unwrap_or(ALICE).to_string(),
                proposed_at: at.to_string(),
                evidence_hash: "h".to_string(),
                acceptors: Vec::new(),
                marked_fired: true,
            },
            eligible: voters.iter().map(|d| d.to_string()).collect(),
        }
    }

    #[test]
    fn genesis_is_the_state_when_there_is_no_history() {
        let derived = fold("review", &review_flow(None), Vec::new());
        assert_eq!(derived.state, "review");
        assert!(derived.shells.is_empty());
        assert_eq!(derived.cache_agrees, None, "the pure fold sees no cache");
    }

    #[test]
    fn two_shells_are_replayed_in_order() {
        let derived = fold(
            "review",
            &review_flow(None),
            vec![
                vouched("p2", "changes_requested", "review", T2, &[ALICE]),
                vouched("p1", "review", "changes_requested", T1, &[ALICE]),
            ],
        );
        assert_eq!(derived.state, "review");
        let walked: Vec<(&str, &str)> = derived
            .shells
            .iter()
            .map(|s| (s.from_state.as_str(), s.to_state.as_str()))
            .collect();
        assert_eq!(
            walked,
            vec![
                ("review", "changes_requested"),
                ("changes_requested", "review")
            ]
        );
    }

    /// A cycle consumes one atom per visit: the atom that fired the first
    /// `review → changes_requested` must not fire it again when the flow
    /// comes back to `review`.
    #[test]
    fn cycle_consumes_one_atom_per_hop() {
        let derived = fold(
            "review",
            &review_flow(None),
            vec![
                vouched("p1", "review", "changes_requested", T1, &[ALICE]),
                vouched("p2", "changes_requested", "review", T2, &[ALICE]),
            ],
        );
        assert_eq!(derived.state, "review");
        assert_eq!(derived.shells.len(), 2, "the first atom must not be reused");
    }

    /// The same edge legitimately firing twice: the second visit's atom was
    /// minted after the first visit's, so replay order separates them.
    #[test]
    fn an_edge_can_fire_once_per_visit() {
        let derived = fold(
            "review",
            &review_flow(None),
            vec![
                vouched("p1", "review", "changes_requested", T1, &[ALICE]),
                vouched("p2", "changes_requested", "review", T2, &[ALICE]),
                vouched("p3", "review", "changes_requested", T3, &[BOB]),
            ],
        );
        assert_eq!(derived.state, "changes_requested");
        assert_eq!(derived.shells.len(), 3);
    }

    /// Quorum is a property of the bucket, not of one proposal: two
    /// replicas each minting their own proposal for the same edge is how
    /// an `{n: 2}` flow reaches consensus without a human accept, and the
    /// firing pass marks both. A per-atom fold would regress such a flow
    /// to its genesis state.
    #[test]
    fn two_single_voter_atoms_on_one_edge_reach_quorum_together() {
        let flow = review_flow(Some(2));
        let alone = fold(
            "review",
            &flow,
            vec![vouched("p1", "review", "approved", T1, &[ALICE])],
        );
        assert_eq!(alone.state, "review", "1 < n = 2 must not advance");

        let together = fold(
            "review",
            &flow,
            vec![
                vouched("p1", "review", "approved", T1, &[ALICE]),
                vouched("p2", "review", "approved", T2, &[BOB]),
            ],
        );
        assert_eq!(together.state, "approved");
        assert_eq!(together.shells.len(), 1, "one consensus event, two atoms");
        assert_eq!(together.shells[0].atoms.len(), 2);
        assert_eq!(
            together.shells[0].eligible_voters,
            vec![ALICE.to_string(), BOB.to_string()]
        );
    }

    /// The same DID twice is one voter — otherwise a single agent clears
    /// any `{n}` by minting `n` proposals.
    #[test]
    fn one_did_cannot_reach_a_two_signer_quorum_alone() {
        let derived = fold(
            "review",
            &review_flow(Some(2)),
            vec![
                vouched("p1", "review", "approved", T1, &[ALICE]),
                vouched("p2", "review", "approved", T2, &[ALICE]),
            ],
        );
        assert_eq!(derived.state, "review");
    }

    /// A marked atom the fold cannot vouch for is not history — this is
    /// what makes a forged `resolved_as` mark worthless.
    #[test]
    fn unvouched_undeclared_and_misrouted_atoms_are_skipped() {
        let flow = review_flow(None);
        let cases: [(&str, VouchedAtom); 3] = [
            (
                "no eligible voter (e.g. every voter failed the fromRole gate)",
                vouched("p1", "review", "approved", T1, &[]),
            ),
            (
                "the definition declares no such edge",
                vouched("p2", "review", "review", T1, &[ALICE]),
            ),
            (
                "leaves a state the flow is not in",
                vouched("p3", "approved", "review", T1, &[ALICE]),
            ),
        ];
        for (name, atom) in cases {
            let derived = fold("review", &flow, vec![atom]);
            assert_eq!(derived.state, "review", "{name}");
            assert!(derived.shells.is_empty(), "{name}");
        }
    }

    /// A rule of `{n: 0}` is misconfigured; "nobody set a threshold" must
    /// not read as "everybody passes".
    #[test]
    fn a_zero_threshold_rule_never_reaches_quorum() {
        let derived = fold(
            "review",
            &review_flow(Some(0)),
            vec![vouched("p1", "review", "approved", T1, &[ALICE, BOB])],
        );
        assert_eq!(derived.state, "review");
    }

    /// Two replicas that hold the same links must derive the same state,
    /// whatever order their stores return them in.
    #[test]
    fn fold_is_deterministic_under_input_order() {
        let flow = review_flow(Some(2));
        let atoms = vec![
            vouched("p1", "review", "changes_requested", T1, &[ALICE]),
            vouched("p2", "changes_requested", "review", T2, &[BOB]),
            vouched("p3", "review", "approved", T3, &[ALICE]),
            vouched("p4", "review", "approved", T3, &[BOB]),
        ];
        let expected = fold("review", &flow, atoms.clone());
        assert_eq!(expected.state, "approved");

        for rotation in 1..atoms.len() {
            let mut shuffled = atoms.clone();
            shuffled.rotate_left(rotation);
            assert_eq!(
                fold("review", &flow, shuffled),
                expected,
                "rotation {rotation} derived a different state"
            );
        }
    }

    // ---- the bag ---------------------------------------------------------

    fn bag() -> AtomBag {
        let mut bag = AtomBag {
            atoms: vec![
                atom_of(&honest_proposal(ALICE, "review", "approved", "h1", T1)).expect("atom"),
                atom_of(&honest_proposal(
                    BOB,
                    "changes_requested",
                    "review",
                    "h2",
                    T2,
                ))
                .expect("atom"),
            ],
            rejected: vec![
                RejectedProposal {
                    uri: "ad4m://flow/proposal/unsealed".into(),
                    marked_fired: false,
                    reason: AtomRejection::EmptySeal,
                },
                RejectedProposal {
                    uri: "ad4m://flow/proposal/history-unsealed".into(),
                    marked_fired: true,
                    reason: AtomRejection::EmptySeal,
                },
                RejectedProposal {
                    uri: "ad4m://flow/proposal/anonymous".into(),
                    marked_fired: false,
                    reason: AtomRejection::NoValidProposerLink,
                },
            ],
        };
        bag.atoms[1].marked_fired = true;
        bag
    }

    #[test]
    fn the_bag_partitions_on_the_derived_state_not_on_the_cache() {
        let bag = bag();
        assert_eq!(
            bag.frontier("review")
                .iter()
                .map(|a| a.to_state.as_str())
                .collect::<Vec<_>>(),
            vec!["approved"]
        );
        assert!(bag.frontier("approved").is_empty());
        assert!(
            bag.superseded("review").is_empty(),
            "a marked atom is history, never superseded noise"
        );
        assert_eq!(bag.superseded("approved").len(), 1);
        assert_eq!(bag.marked().count(), 1);
    }

    /// Only unsealed LIVE proposals are cleanable. An identity-unverified
    /// proposal may still be mid-sync, and a marked one is history.
    #[test]
    fn only_live_unsealed_proposals_are_cleanable() {
        assert_eq!(bag().unsealed_live(), vec!["ad4m://flow/proposal/unsealed"]);
    }
}
