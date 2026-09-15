//! Sync-triggered re-derivation: when a peer's links arrive, run the pass.
//!
//! The cache and the marks are `Local` (see [`pass`](super::pass)), so
//! nothing a peer writes can update them on this replica — only this
//! replica's own pass can. Before this module that pass ran after this
//! replica's own vote ([`accept`](super::accept)) or mint only; a replica
//! that merely *watched* a flow settle would never have recorded it.
//!
//! [`FlowTouch::of_diff`] picks the flow-relevant links out of an inbound
//! diff and [`PerspectiveInstance::schedule_flow_consensus_pass`] coalesces
//! bursts into one debounced pass, scoped to the instances those links
//! touched. At most one pass is queued per perspective at a time: a diff
//! that lands while one is waiting is folded into it, and the pass reads the
//! store only after the wait, so it sees every link that queued it.
//!
//! **What triggers.** Proposal links (the atom fields and `flow/instance`),
//! votes (`acceptedBy`) and `FlowInstance` rows (`flow/flow_uri`,
//! `flow/base`) — additions and removals alike, since deleting a vote
//! regresses the state and the cache must follow. **What does not.** Chat,
//! tasks, anything outside the flow vocabulary; and a peer's legacy `Shared`
//! cache or mark, which this replica neither reads nor mirrors. Role rows
//! (`fromRole`) are not in the vocabulary either — they are whatever class
//! the flow author chose — so a role change is folded on the next
//! flow-relevant diff or local vote rather than the moment it lands.

use super::atom::{
    ACCEPTED_BY_PREDICATE, EVIDENCE_HASHES_PREDICATE, FLOW_INSTANCE_PREDICATE,
    FROM_STATE_PREDICATE, PROPOSER_PREDICATE, TO_STATE_PREDICATE,
};
use super::pass::run_flow_consensus_pass;
use crate::agent::AgentContext;
use crate::perspectives::flow_classes::{FLOW_BASE_PREDICATE, FLOW_URI_PREDICATE};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{DecoratedPerspectiveDiff, LinkQuery};
use std::collections::HashSet;
use std::sync::atomic::Ordering;
use std::time::Duration;

/// How long a queued pass waits for the rest of a sync burst before it
/// reads. Sync delivers a proposal's five links, or a batch of votes, as
/// separate diffs in quick succession; one pass over all of them is the
/// same result for a fraction of the work.
pub const FLOW_PASS_DEBOUNCE: Duration = Duration::from_millis(300);

/// The flow-relevant subjects of one inbound diff. Empty for the common
/// case — a diff with nothing from the flow vocabulary in it.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct FlowTouch {
    /// `FlowInstance` URIs the diff names directly: the row's own links, or
    /// a proposal's `flow/instance` link.
    pub instances: HashSet<String>,
    /// Proposal URIs whose instance the diff does not name (a vote, a field
    /// link). Resolved against the store when the pass runs.
    pub proposals: HashSet<String>,
}

impl FlowTouch {
    pub fn of_diff(diff: &DecoratedPerspectiveDiff) -> Self {
        let mut touch = FlowTouch::default();
        for link in diff.additions.iter().chain(diff.removals.iter()) {
            match link.data.predicate.as_deref().unwrap_or("") {
                FLOW_INSTANCE_PREDICATE => {
                    touch.instances.insert(link.data.target.clone());
                }
                FLOW_URI_PREDICATE | FLOW_BASE_PREDICATE => {
                    touch.instances.insert(link.data.source.clone());
                }
                FROM_STATE_PREDICATE
                | TO_STATE_PREDICATE
                | PROPOSER_PREDICATE
                | EVIDENCE_HASHES_PREDICATE
                | ACCEPTED_BY_PREDICATE => {
                    touch.proposals.insert(link.data.source.clone());
                }
                _ => {}
            }
        }
        touch
    }

    pub fn is_empty(&self) -> bool {
        self.instances.is_empty() && self.proposals.is_empty()
    }

    fn absorb(&mut self, other: FlowTouch) {
        self.instances.extend(other.instances);
        self.proposals.extend(other.proposals);
    }
}

/// Per-perspective coalescing state behind
/// [`PerspectiveInstance::schedule_flow_consensus_pass`]. Pure, so the
/// "one pass queued at a time" rule is testable without a runtime.
#[derive(Debug, Default)]
pub struct FlowPassQueue {
    pending: FlowTouch,
    scheduled: bool,
}

impl FlowPassQueue {
    /// Fold `touch` into the pending pass. `true` when the caller must
    /// spawn that pass — i.e. none was queued yet.
    pub fn enqueue(&mut self, touch: FlowTouch) -> bool {
        self.pending.absorb(touch);
        !std::mem::replace(&mut self.scheduled, true)
    }

    /// Hand the pending work to the pass that is about to run, and open
    /// the queue for the next one. Anything that arrives from here on
    /// spawns a fresh pass, so nothing lands in a gap.
    pub fn take(&mut self) -> FlowTouch {
        self.scheduled = false;
        std::mem::take(&mut self.pending)
    }
}

impl PerspectiveInstance {
    /// Queue a consensus pass over the flow instances `diff` touches, if
    /// any. Called from `diff_from_link_language` once the links are
    /// persisted; returns immediately. See the module doc for what counts
    /// as a touch and how bursts coalesce.
    pub(crate) fn schedule_flow_consensus_pass(&self, diff: &DecoratedPerspectiveDiff) {
        let touch = FlowTouch::of_diff(diff);
        if touch.is_empty() {
            return;
        }
        let spawn = self
            .flow_pass_queue
            .lock()
            .expect("flow pass queue poisoned")
            .enqueue(touch);
        if !spawn {
            // A pass is already waiting; it reads after the debounce, so it
            // will see these links too.
            return;
        }
        let mut this = self.clone();
        tokio::spawn(async move {
            tokio::time::sleep(FLOW_PASS_DEBOUNCE).await;
            let touch = this
                .flow_pass_queue
                .lock()
                .expect("flow pass queue poisoned")
                .take();
            if this.is_teardown.load(Ordering::Acquire) {
                return;
            }
            this.run_sync_triggered_flow_pass(touch).await;
        });
    }

    /// Resolve the touched proposals to their instances and sweep. Runs as
    /// the main agent: the pass writes `Local` links only, so the signing
    /// identity is bookkeeping rather than authority — the same context the
    /// auto-processor's main loop uses.
    async fn run_sync_triggered_flow_pass(&mut self, touch: FlowTouch) {
        let mut instances = touch.instances;
        for proposal in &touch.proposals {
            let query = LinkQuery {
                source: Some(proposal.clone()),
                predicate: Some(FLOW_INSTANCE_PREDICATE.to_string()),
                ..Default::default()
            };
            match self.get_links(&query).await {
                // A proposal whose `flow/instance` link has not arrived is
                // not an atom yet; that link's own arrival triggers the pass
                // that folds it.
                Ok(links) => instances.extend(links.into_iter().map(|l| l.data.target)),
                Err(e) => log::warn!(
                    "sync-triggered flow pass: resolving {proposal} to its instance failed: {e:#}"
                ),
            }
        }
        if instances.is_empty() {
            return;
        }
        let mut instances: Vec<String> = instances.into_iter().collect();
        instances.sort();
        log::debug!("sync-triggered flow pass on {}: {instances:?}", self.uuid);
        let context = AgentContext::main_agent();
        let outcomes = run_flow_consensus_pass(self, None, &context, None, Some(&instances)).await;
        for outcome in &outcomes {
            log::info!(
                "🔥 flow settled (synced in): {} {} → {} (by {:?})",
                outcome.instance_uri,
                outcome.from_state,
                outcome.to_state,
                outcome.voters
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link, LinkStatus};

    fn link(source: &str, predicate: &str, target: &str) -> DecoratedLinkExpression {
        DecoratedLinkExpression {
            author: "did:key:peer".into(),
            timestamp: String::new(),
            data: Link {
                source: source.into(),
                predicate: Some(predicate.into()),
                target: target.into(),
            },
            proof: DecoratedExpressionProof {
                key: String::new(),
                signature: String::new(),
                valid: Some(true),
                invalid: Some(false),
            },
            status: Some(LinkStatus::Shared),
        }
    }

    fn set(items: &[&str]) -> HashSet<String> {
        items.iter().map(|s| s.to_string()).collect()
    }

    /// The common case is the quiet one: application data must not queue
    /// a pass, or every chat message would re-derive every flow.
    #[test]
    fn application_data_is_not_a_touch() {
        let diff = DecoratedPerspectiveDiff::from_additions(vec![
            link("flux://message/1", "flux://body", "literal:string:hi"),
            link("ns://task/1", "rdf://type", "ns://Task"),
            // A peer's legacy cache or mark: neither read nor mirrored.
            link(
                "ad4m://flow/instance/i",
                "ad4m://flow/current_state",
                "literal:string:scoped",
            ),
            link(
                "ad4m://flow/proposal/p",
                "ad4m://flow/resolved_as",
                "literal:string:fired",
            ),
        ]);
        assert!(FlowTouch::of_diff(&diff).is_empty());
    }

    /// Instance rows and a proposal's `flow/instance` link name the instance
    /// outright; votes and field links name only the proposal, which the
    /// pass resolves. Removals count the same as additions.
    #[test]
    fn flow_links_name_their_instance_or_their_proposal() {
        let diff = DecoratedPerspectiveDiff {
            additions: vec![
                link(
                    "ad4m://flow/instance/a",
                    FLOW_URI_PREDICATE,
                    "delivery://DeliveryFlow",
                ),
                link(
                    "ad4m://flow/proposal/p1",
                    FLOW_INSTANCE_PREDICATE,
                    "ad4m://flow/instance/b",
                ),
                link(
                    "ad4m://flow/proposal/p2",
                    ACCEPTED_BY_PREDICATE,
                    "did:key:bob",
                ),
            ],
            removals: vec![link(
                "ad4m://flow/proposal/p3",
                TO_STATE_PREDICATE,
                "literal:string:scoped",
            )],
        };
        assert_eq!(
            FlowTouch::of_diff(&diff),
            FlowTouch {
                instances: set(&["ad4m://flow/instance/a", "ad4m://flow/instance/b"]),
                proposals: set(&["ad4m://flow/proposal/p2", "ad4m://flow/proposal/p3"]),
            }
        );
    }

    /// One pass queued at a time: the first touch spawns, later ones merge
    /// into it, `take` hands everything to the running pass and re-arms.
    #[test]
    fn a_queue_holds_one_pass_and_merges_bursts_into_it() {
        let mut queue = FlowPassQueue::default();
        let first = FlowTouch {
            instances: set(&["ad4m://flow/instance/a"]),
            proposals: HashSet::new(),
        };
        let second = FlowTouch {
            instances: HashSet::new(),
            proposals: set(&["ad4m://flow/proposal/p"]),
        };
        assert!(queue.enqueue(first.clone()), "the first touch spawns");
        assert!(!queue.enqueue(second.clone()), "the second rides along");

        let taken = queue.take();
        assert_eq!(taken.instances, first.instances);
        assert_eq!(taken.proposals, second.proposals);

        assert!(
            queue.enqueue(first),
            "after take, the next touch spawns again — nothing lands in a gap"
        );
        assert!(queue.take().proposals.is_empty(), "take drains");
    }
}
