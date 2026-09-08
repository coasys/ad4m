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
//! own vote. Nothing in them can move a state the fold did not derive.
//!
//! # The read-set is the proof
//!
//! [`FlowInstance::read_set`] does all the I/O and produces a [`ReadSet`]:
//! the raw signed links of every proposal, plus the role verdicts that
//! decided who was eligible. [`fold_read_set`] is pure over that value, so
//! when a completed flow mints a Synergy token later, the token's backing is
//! `serde_json` of the read-set the fold already received — and an
//! off-perspective verifier can re-run the identical fold over it and reach
//! the same verdict. That is what makes it a proof rather than an assertion,
//! and it is why the fold stays pure and synchronous.

pub mod accept;
pub mod atom;
pub mod fold;
pub mod pass;
pub mod roles;

use crate::perspectives::flow_context::FlowInstanceRecord;
use crate::perspectives::flow_spawn::initial_state_of;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::SHACLFlow;
use crate::types::DecoratedLinkExpression;
use atom::{marked_fired, TransitionAtom};
use fold::{fold, rule_for, DerivedState, VouchedAtom};
use roles::{eligible_votes, resolve_role_grants, RoleGrant};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, HashMap, HashSet};

/// A running flow: its identity row plus the definition it runs. Built once
/// per read; borrows the definition from the caller's catalogue.
#[derive(Debug)]
pub struct FlowInstance<'a> {
    pub uri: String,
    pub subject: String,
    pub flow: &'a SHACLFlow,
}

/// One proposal exactly as the store returned it: every link on it, from
/// every author, each carrying its own signature verdict.
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
/// The two halves are not worth the same, and the difference matters to
/// anything that settles value on this:
///
/// - `proposals` are **proof**. Every link carries its own author and
///   signature verdict, so a verifier re-runs the identity checks itself
///   instead of believing us.
/// - `role_grants` are an **audit record**, not proof — see the field.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ReadSet {
    pub instance_uri: String,
    /// The state the walk starts from — the flow definition's first state.
    pub genesis: String,
    pub proposals: Vec<ProposalLinks>,
    /// Which voters this replica held eligible, and the role rows it says it
    /// read. `eligible` is a verdict WE computed: the rows are cited by ID
    /// and not carried here, so a verifier that folds this set has trusted
    /// the minter about role membership rather than checked it. Present
    /// because roles are re-derived live and a token's backing must at least
    /// record what its verdict rested on. Making this half re-verifiable
    /// needs the signed role rows themselves — the vote-time snapshot — which
    /// is platform work this engine does not do yet.
    pub role_grants: Vec<RoleGrant>,
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

    /// Proposal URIs already carrying a `resolved_as → "fired"` mark.
    /// Bookkeeping for [`pass`], never an input to the fold.
    pub fn marked_proposals(&self) -> HashSet<String> {
        self.proposals
            .iter()
            .filter(|p| marked_fired(&p.links))
            .map(|p| p.uri.clone())
            .collect()
    }
}

/// The state of a flow, re-derived from a read-set. **Pure** — this is the
/// function an off-perspective verifier re-runs over a minted token's proof.
pub fn fold_read_set(flow: &SHACLFlow, read_set: &ReadSet) -> DerivedState {
    let vouched: Vec<VouchedAtom> = read_set
        .atoms()
        .into_iter()
        .map(|atom| {
            let rule = rule_for(flow, &atom.to_state);
            VouchedAtom {
                eligible_votes: eligible_votes(&atom, &rule, &read_set.role_grants),
                atom,
            }
        })
        .collect();
    fold(&read_set.genesis, flow, &vouched)
}

impl<'a> FlowInstance<'a> {
    /// Pair an already-loaded row with its definition from the caller's
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

    /// The flat row the guard and role translators take (`$flow.base`,
    /// `$flow.instance`). Neither reads a state, so the row carries genesis
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

    /// Read everything the fold needs, and nothing else: this instance's
    /// proposal links, and one `fromRole` verdict per (target state, voter)
    /// the rules actually gate.
    ///
    /// All of the engine's I/O for a state read happens here. `Err` on a
    /// store error or an undeterminable role query — the caller must then
    /// abandon the read rather than act on a wrong eligible set.
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
            let rule = rule_for(self.flow, to_state);
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

    /// The state of this flow: read, then fold. The only function the rest of
    /// the engine may call for "what state is this flow in".
    pub async fn derive_state(
        &self,
        perspective: &PerspectiveInstance,
    ) -> anyhow::Result<DerivedState> {
        Ok(fold_read_set(self.flow, &self.read_set(perspective).await?))
    }
}

/// Replace each record's `currentState` cache with the fold's answer, so
/// every downstream reader in one pass acts on the derived state.
///
/// Records whose flow is not in the catalogue, or whose read fails, are
/// dropped — a pass must not act on an instance it could not derive.
pub async fn derive_states(
    perspective: &PerspectiveInstance,
    records: &[FlowInstanceRecord],
    flows_by_uri: &HashMap<String, SHACLFlow>,
) -> Vec<FlowInstanceRecord> {
    let mut out = Vec::with_capacity(records.len());
    for record in records {
        let Some(flow) = flows_by_uri.get(&record.flow_uri) else {
            continue;
        };
        match FlowInstance::from_record(record, flow)
            .derive_state(perspective)
            .await
        {
            Ok(derived) => out.push(FlowInstanceRecord {
                current_state: derived.state,
                ..record.clone()
            }),
            Err(e) => log::warn!(
                "derive_states: skipping {} — its state could not be derived: {e:#}",
                record.instance_uri
            ),
        }
    }
    out
}
