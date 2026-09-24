//! A running flow's state, **derived rather than read**.
//!
//! # The model, in one paragraph
//!
//! The state of a flow is not stored anywhere. It is computed, every time,
//! from the signed links that exist on the graph right now: collect every
//! transition proposal, keep only the facts each agent signed for themselves,
//! pool the votes per declared edge, and walk from the flow's starting state
//! taking whichever edge reached quorum first. That walk's endpoint is the
//! state. Nothing else moves it — not the `currentState` link (a cache this
//! engine writes and never reads), not a `resolved_as → "fired"` mark (an
//! index for UIs), not anyone's unsigned claim. And because state is a
//! function of the links present *now*, deleting a link recomputes the state
//! without it: remove a settled vote and the flow stands where it stood
//! before that vote. That is the semantics, not a failure mode.
//!
//! # Pipeline
//!
//! ```text
//! links in perspective
//!   │
//!   ▼ load_proposal_links  (atom.rs)
//!      ├─ Half 1: model_query(FlowTransitionProposal, where flowInstance=uri)
//!      │          → proposal URIs scoped to this instance
//!      └─ Half 2: get_links(source=uri) per proposal (raw; signature verdicts intact)
//!   │
//!   ▼ TransitionAtom::from_links  (atom.rs)
//!      Answers: "is this a proposal, and whose words are in it?"
//!      Checks: proposer self-signed; all fields from proposer only; non-empty seal.
//!      Carries NO eligibility verdict — that is decided in read_set, never here.
//!   │
//!   ▼ FlowInstance::read_set  ← ONLY store access in a state read
//!      ├─ proposals (TransitionAtoms from above)
//!      └─ resolve_role_grants per gated target state  (roles.rs)
//!         Reads each candidate's role instances and collects the links behind
//!         them — grant links and signed tombstones, authority NOT applied.
//!         Decides nothing: not about a vote, not about a window.
//!   │
//!   ▼ fold_read_set
//!      ├─ RoleGrantEvidence::resolve per candidate (roles.rs, pure): links →
//!      │  RoleGrant windows, applying the authority rule from the definition
//!      │  passed in. Unresolvable evidence aborts the whole derivation —
//!      │  dropping the candidate would de-quorate an edge and let the walk
//!      │  take a survivor contention would have held.
//!      └─ eligible_votes per atom (roles.rs, pure): each vote is gated
//!      AS OF ITS OWN TIMESTAMP against the windows (#1027) → VouchedAtom
//!      with pre-filtered votes. No store. No role queries.
//!   │
//!   ▼ fold  (fold.rs — pure, no I/O)
//!      Walks from genesis taking the earliest-settled declared edge per state.
//!      "Earliest" is each voter's own claimed clock. Can back-dating a vote
//!      manufacture a quorum, or win a race it should have lost? No to the
//!      first; the second is refused rather than decided, where the race is
//!      irreversible. See fold.rs § Ordering and time — the properties and the
//!      RESIDUAL, each with a code pointer.
//!   │
//!   ▼ DerivedState { state, settled, contested }
//! ```
//!
//! # Where to read
//!
//! - [`fold`] — the algorithm, and the engine's entire belief system. Thirty
//!   of its lines are the whole decision procedure.
//! - [`atom`] — what counts as a proposal and as a vote, and why a foreign
//!   link is invisible.
//!
//! Everything else is plumbing whose contract is stated on its module doc,
//! and none of it makes a decision: [`roles`] resolves who may vote,
//! [`pass`] writes the cache and the marks, [`accept`] casts this replica's
//! own vote — re-verifying the proposal's evidence seal against our own graph
//! before signing, and sweeping the instance afterwards so the new vote is
//! folded — and [`trigger`] runs that same sweep when a peer's flow links
//! sync in, since the cache and the marks are this replica's own and nobody
//! else can update them. Nothing in them can move a state the fold did not
//! derive.
//!
//! # The read-set is the proof
//!
//! [`FlowInstance::read_set`] does all the I/O and produces a [`ReadSet`]:
//! the raw signed links of every proposal, plus the raw signed links behind
//! every voter's role membership. No verdict travels — the windows and the
//! authority filter are recomputed from those links by whoever reads them.
//! [`fold_read_set`] is pure over that value, so
//! when a completed flow mints a Synergy token later, the token's backing is
//! `serde_json` of the read-set the fold already received — and an
//! off-perspective verifier can re-run the identical fold over it and reach
//! the same verdict. That is what makes it a proof rather than an assertion,
//! and it is why the fold stays pure and synchronous.

pub mod accept;
pub mod atom;
pub mod fold;
pub mod grant;
#[cfg(test)]
mod ordering_tests;
pub mod pass;
pub mod produced;
pub mod propose;
pub mod read_set;
pub mod receipt;
pub mod roles;
#[cfg(test)]
pub(crate) mod test_support;
pub mod time;
pub mod trigger;
pub mod verify;

pub(crate) use pass::local_cached_state;
pub use read_set::{fold_read_set, ProposalLinks, ReadSet};

use crate::perspectives::flow_context::FlowInstanceRecord;
use crate::perspectives::flow_spawn::initial_state_of;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::SHACLFlow;
use fold::{rule_for, Contention, DerivedState, ResolvedRule};
use grant::GrantContext;
use roles::resolve_role_grants;
use std::collections::{BTreeSet, HashMap};

/// A running flow: its identity instance plus the definition it runs. Built once
/// per read; borrows the definition from the caller's catalogue.
///
/// The two URIs point at different things and are easy to swap by eye:
/// `uri` is the flow run, `subject` is the thing it runs *about*.
#[derive(Debug)]
pub struct FlowInstance<'a> {
    /// This run's own identity — `ad4m://flow/instance/{id}`, the
    /// `FlowInstanceRecord::instance_uri`. Every proposal in the read-set
    /// points here through `flowInstance`.
    pub uri: String,
    /// The base expression this run is bound to — the task, message or
    /// whatever else the flow is about (`ad4m://task/foo`), *not* this run.
    ///
    /// In flow-definition terms this is the run's **input**: it is the
    /// expression whose subject classes auto-spawn matched against the
    /// definition's `inputTypes` (`flow_spawn::spawn_candidates`).
    ///
    /// Called `subject` rather than `baseExpression` only because that name
    /// collided with an `Ad4mModel` synthetic field (`e6362e5ca`); the
    /// template language still spells it `$flow.base`
    /// (`flow_context::render::FLOW_BASE_TOKEN`).
    pub subject: String,
    pub flow: &'a SHACLFlow,
}

impl<'a> FlowInstance<'a> {
    /// Pair an already-loaded flow instance with its definition from the caller's
    /// catalogue.
    pub fn from_record(record: &FlowInstanceRecord, flow: &'a SHACLFlow) -> Self {
        FlowInstance {
            uri: record.instance_uri.clone(),
            subject: record.subject.clone(),
            flow,
        }
    }

    /// The state a fresh instance of this flow begins in; `None` only for a
    /// zero-state flow, which never has an instance.
    pub fn genesis(&self) -> Option<String> {
        initial_state_of(self.flow)
    }

    /// The flat record the guard and role translators take (`$flow.base`,
    /// `$flow.instance`). Neither reads a state, so the record carries genesis
    /// purely to stay a valid [`FlowInstanceRecord`].
    pub fn as_record(&self) -> FlowInstanceRecord {
        FlowInstanceRecord {
            flow_uri: self.flow.flow_uri(),
            instance_uri: self.uri.clone(),
            subject: self.subject.clone(),
            current_state: self.genesis().unwrap_or_default(),
            created_at: None,
        }
    }

    /// All I/O for a state read. Returns the proposal links of every
    /// [`TransitionAtom`] on this instance, plus one [`RoleGrantEvidence`] —
    /// the candidate's role instances with the grant and tombstone links
    /// behind them — per `(target_state, candidate_DID)` pair where the
    /// rule's `fromRole` gates that state. That is three classes of store
    /// query and no others; the fold that follows is pure over this value.
    ///
    /// Fails closed: `Err` propagates on any store error **and** on any role
    /// query that cannot discriminate between DIDs. The caller must abandon the
    /// read and leave the state unknown, rather than fold over an incomplete
    /// eligible set and derive a wrong answer.
    pub async fn read_set(&self, perspective: &PerspectiveInstance) -> anyhow::Result<ReadSet> {
        let genesis = self.genesis().ok_or_else(|| {
            anyhow::anyhow!(
                "flow `{}` has no states, so instance {} has no genesis to fold from",
                self.flow.name,
                self.uri
            )
        })?;
        let proposals: Vec<ProposalLinks> = atom::load_proposal_links(perspective, &self.uri)
            .await?
            .into_iter()
            .map(|(uri, links)| ProposalLinks { uri, links })
            .collect();

        let mut read_set = ReadSet {
            instance_uri: self.uri.clone(),
            subject: self.subject.clone(),
            genesis,
            proposals,
            role_grants: Vec::new(),
        };

        // One role resolution per gated target state, over the union of the
        // DIDs that voted for it — the fold only ever intersects the role set
        // with DIDs that actually voted.
        let record = self.as_record();
        let atoms = read_set.atoms();
        let targets: BTreeSet<&str> = atoms.iter().map(|a| a.to_state.as_str()).collect();
        for to_state in targets {
            // No role resolution for a refused target: the edge cannot settle
            // whoever voted, so resolving grants for it would be I/O whose
            // result nothing reads. Not a fail-open — the refusal is
            // `settle_edge`'s, and `fold_read_set` empties the eligible set.
            let ResolvedRule::Rule(rule) = rule_for(self.flow, to_state) else {
                continue;
            };
            let Some(role) = rule.from_role.as_ref() else {
                continue;
            };
            let candidates: BTreeSet<String> = atoms
                .iter()
                .filter(|a| a.to_state == to_state)
                .flat_map(|a| a.votes.iter().map(|v| v.did.clone()))
                .collect();
            let candidates: Vec<String> = candidates.into_iter().collect();
            read_set.role_grants.extend(
                resolve_role_grants(perspective, to_state, role, &record, &candidates).await?,
            );
        }
        Ok(read_set)
    }

    /// The authoritative state of this flow: calls `read_set` (all I/O), then
    /// `fold_read_set` (pure). The single entry point the rest of the engine
    /// uses for "what state is this flow in" — nothing else is authoritative.
    ///
    /// `catalogue` is the caller's whole flow catalogue, not just this flow:
    /// a `grantedByFlow` gate verifies receipts for *other* flows, and it
    /// verifies them against the reader's own definitions. Every caller
    /// already holds one — they had to look `self.flow` up in it.
    pub async fn derive_state(
        &self,
        perspective: &PerspectiveInstance,
        catalogue: &HashMap<String, SHACLFlow>,
    ) -> anyhow::Result<DerivedState> {
        fold_read_set(
            self.flow,
            &self.read_set(perspective).await?,
            GrantContext::root(catalogue),
        )
    }
}

/// A `FlowInstanceRecord` together with the derivation's contention verdict.
///
/// Returned by [`derive_states`] so consumers can honour the invariant:
/// *anything that pays out on a completed flow must refuse a derivation with
/// `contested.is_some()`*. Contention is a per-fold ephemeral — it is NOT
/// stored on `FlowInstanceRecord` to prevent stale persisted values.
#[derive(Debug, Clone)]
pub struct DerivedFlow {
    pub record: FlowInstanceRecord,
    /// `Some` when two edges out of the current state both carry quorum; the
    /// flow is irreversibly stalled and consumers must not propose into it or
    /// present it as "awaiting votes". `None` means an ordinary settled or
    /// waiting state.
    pub contested: Option<Contention>,
}

/// Derive the current state of every record in one pass, replacing each
/// `currentState` cache with the fold's answer so downstream readers act on
/// the live derived state.
///
/// Records are dropped — never acted on — when their flow is absent from the
/// catalogue or when `derive_state` returns `Err` (store error, undeterminable
/// role query, or zero-state flow). A pass that cannot derive a state must not
/// guess it.
pub async fn derive_states(
    perspective: &PerspectiveInstance,
    records: &[FlowInstanceRecord],
    flows_by_uri: &HashMap<String, SHACLFlow>,
) -> Vec<DerivedFlow> {
    let mut out = Vec::with_capacity(records.len());
    for record in records {
        let Some(flow) = flows_by_uri.get(&record.flow_uri) else {
            continue;
        };
        match FlowInstance::from_record(record, flow)
            .derive_state(perspective, flows_by_uri)
            .await
        {
            Ok(derived) => out.push(DerivedFlow {
                record: FlowInstanceRecord {
                    current_state: derived.state,
                    ..record.clone()
                },
                contested: derived.contested,
            }),
            Err(e) => log::warn!(
                "derive_states: skipping {} — its state could not be derived: {e:#}",
                record.instance_uri
            ),
        }
    }
    out
}
