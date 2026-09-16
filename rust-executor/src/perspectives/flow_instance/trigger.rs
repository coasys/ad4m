//! Sync-triggered re-derivation: when a peer's links arrive, run the pass.
//!
//! The cache and the marks are `Local` (see [`pass`](super::pass)), so
//! nothing a peer writes can update them on this replica — only this
//! replica's own pass can. `Local` is also per agent on a multi-user node,
//! so one sync burst sweeps once per agent that keeps bookkeeping here; see
//! [`flow_pass_agents`]. Before this module that pass ran after this
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
//! votes (`acceptedBy`) and `FlowInstance` instances (`flow/flow_uri`,
//! `flow/base`) — additions and removals alike, since deleting a vote
//! regresses the state and the cache must follow. A role revocation
//! tombstone (`flow/role_grant_revoked`, #1027) triggers too, but names a
//! role instance rather than a flow instance, and any flow instance's gate may
//! read it — so it sweeps every flow instance. **What does not.** Chat, tasks,
//! anything outside the flow vocabulary; and a peer's legacy `Shared` cache
//! or mark, which this replica neither reads nor mirrors. Role instances
//! themselves (`fromRole` grants) are not in the vocabulary — they are
//! whatever class the flow author chose — so a new grant is folded on the
//! next flow-relevant diff or local vote rather than the moment it lands.

use super::atom::{
    ACCEPTED_BY_PREDICATE, EVIDENCE_HASHES_PREDICATE, FLOW_INSTANCE_PREDICATE,
    FROM_STATE_PREDICATE, PROPOSER_PREDICATE, ROLE_GRANT_REVOKED_PREDICATE, TO_STATE_PREDICATE,
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
    /// `FlowInstance` URIs the diff names directly: the flow instance's own links, or
    /// a proposal's `flow/instance` link.
    pub instances: HashSet<String>,
    /// Proposal URIs whose instance the diff does not name (a vote, a field
    /// link). Resolved against the store when the pass runs.
    pub proposals: HashSet<String>,
    /// The diff carried a role revocation tombstone. It names a role instance,
    /// not a flow instance, and a role instance can gate any flow instance's votes — so the
    /// pass sweeps every instance in the perspective.
    pub every_instance: bool,
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
                ROLE_GRANT_REVOKED_PREDICATE => {
                    touch.every_instance = true;
                }
                _ => {}
            }
        }
        touch
    }

    pub fn is_empty(&self) -> bool {
        self.instances.is_empty() && self.proposals.is_empty() && !self.every_instance
    }

    fn absorb(&mut self, other: FlowTouch) {
        self.instances.extend(other.instances);
        self.proposals.extend(other.proposals);
        self.every_instance |= other.every_instance;
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

    /// Resolve the touched proposals to their instances, then sweep **once
    /// per agent that keeps bookkeeping here** — see
    /// [`flow_pass_agents`].
    ///
    /// Resolution happens once and outside that loop: which instances a diff
    /// touched is a question about `Shared` links, so it has the same answer
    /// for every agent. Only the recording is per agent.
    async fn run_sync_triggered_flow_pass(&mut self, touch: FlowTouch) {
        let instance_filter: Option<Vec<String>> = if touch.every_instance {
            log::debug!(
                "sync-triggered flow pass on {}: a role revocation synced in; sweeping every instance",
                self.uuid
            );
            None
        } else {
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
            Some(instances)
        };

        for context in self.flow_pass_contexts().await {
            let outcomes =
                run_flow_consensus_pass(self, None, &context, None, instance_filter.as_deref())
                    .await;
            log_synced_outcomes(&context, &outcomes);
        }
    }

    /// The agents [`flow_pass_agents`] names, with the managed users read
    /// from the user DB and narrowed to those that can access this
    /// perspective — a user who cannot see it has no bookkeeping to keep
    /// here, and skipping them is what bounds the cost on a node with many
    /// tenants.
    ///
    /// `UserInfo` carries the DID, so this uses
    /// [`can_access_perspective_with_did`](crate::helpers::can_access_perspective_with_did)
    /// and never takes the wallet mutex.
    ///
    /// That is deliberately the *same* predicate `perspectives_ws` uses to
    /// decide whether a managed user may read this perspective at all, so
    /// bookkeeping is kept for exactly the agents that can read it. One
    /// consequence worth knowing, because it is quiet: that predicate is
    /// `is_owned_by`, which is **false for an unowned perspective** — a
    /// perspective with no owners gets the main-agent pass only. That is
    /// consistent (the WS API denies managed users there too, so they have
    /// no cache to keep), and the `debug!` below names the count so the
    /// "nobody swept" case is visible rather than inferred.
    ///
    /// The single-user short-circuit here is an optimisation (skip the DB
    /// read); the rule itself lives in `flow_pass_agents`, which is where it
    /// is tested.
    async fn flow_pass_contexts(&self) -> Vec<AgentContext> {
        let multi_user = crate::user_management::is_multi_user_enabled();
        if !multi_user {
            return flow_pass_agents(false, Vec::new());
        }
        let handle = self.persisted.lock().await.clone();
        let emails: Vec<String> =
            match crate::db::Ad4mDb::with_global_instance(|db| db.list_users()) {
                Ok(users) => {
                    let total = users.len();
                    let emails: Vec<String> = users
                        .into_iter()
                        .filter(|u| {
                            crate::helpers::can_access_perspective_with_did(
                                &Some(u.did.clone()),
                                &handle,
                            )
                        })
                        .map(|u| u.username)
                        .collect();
                    log::debug!(
                        "sync-triggered flow pass on {}: sweeping for the main agent and {} of {} \
                     managed user(s) with access",
                        self.uuid,
                        emails.len(),
                        total
                    );
                    emails
                }
                Err(e) => {
                    // Fail closed on the *extra* agents, not on the pass: the
                    // main agent still sweeps, so the host's own view stays
                    // correct and the managed users heal on the next touch.
                    log::warn!(
                        "sync-triggered flow pass on {}: could not list users ({e}); \
                     running the main-agent pass only",
                        self.uuid
                    );
                    Vec::new()
                }
            };
        flow_pass_agents(true, emails)
    }
}

/// Every agent whose `Local` bookkeeping a flow pass on this perspective
/// must advance: the main agent always, then each managed user.
///
/// **Why more than one.** The derived-state cache and the fired marks are
/// `Local` links, and managed users get their own local link space (the
/// per-user `Local` work in flight). A single main-agent pass would leave
/// every managed user's bookkeeping un-advanced: they would read a stale
/// `currentState`, and their first later pass would report a settle that
/// happened long ago as new — the `first_pass_here` catch-up in
/// [`pass`](super::pass) suppresses the flood only once.
///
/// **Why all managed users and not only the online ones.** Unlike the
/// auto-processor — where an offline user simply misses an LLM pass another
/// loop will redo — a missed flow pass leaves a value the user *reads*
/// stale, and nothing re-triggers it: passes run on a link touch, a local
/// vote, or an interpretation run, so an absent user's cache would stay
/// wrong until the next unrelated link arrives in that flow. The pass is a
/// fold over the links present now, with no LLM call, so running it for a
/// quiet user costs store reads and nothing else.
///
/// Single-user mode returns exactly `[main_agent]`, so nothing changes
/// there. Managed users are deduplicated and ordered, so a pass sweeps in
/// the same order every time.
///
/// Cost note: each context currently re-reads the shared links too
/// (`load_shacl_flows`, `load_flow_instances`, the read set). Only the
/// cache and mark reads are genuinely per agent. Splitting
/// `run_flow_consensus_pass` into derive-once / record-per-agent is the
/// optimisation to make when the per-user `Local` links land, and is left
/// until then so this change stays reviewable against that PR.
pub fn flow_pass_agents(multi_user: bool, managed_user_emails: Vec<String>) -> Vec<AgentContext> {
    let mut contexts = vec![AgentContext::main_agent()];
    if !multi_user {
        return contexts;
    }
    let mut emails: Vec<String> = managed_user_emails;
    emails.sort();
    emails.dedup();
    contexts.extend(emails.into_iter().map(AgentContext::for_user_email));
    contexts
}

fn log_synced_outcomes(context: &AgentContext, outcomes: &[super::pass::FireOutcome]) {
    let agent = match &context.user_email {
        Some(email) => email.as_str(),
        None => "main agent",
    };
    for outcome in outcomes {
        log::info!(
            "🔥 flow settled (synced in, for {agent}): {} {} → {} (by {:?})",
            outcome.instance_uri,
            outcome.from_state,
            outcome.to_state,
            outcome.voters
        );
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

    /// Flow instances and a proposal's `flow/instance` link name the instance
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
                every_instance: false,
            }
        );
    }

    /// A role revocation names a role instance, not a flow instance, and any
    /// flow instance's gate may read it — so it touches every flow instance (#1027).
    #[test]
    fn a_role_revocation_touches_every_instance() {
        let diff = DecoratedPerspectiveDiff::from_additions(vec![link(
            "ns://reviewer/1",
            ROLE_GRANT_REVOKED_PREDICATE,
            "literal:string:did%3Akey%3Abob",
        )]);
        let touch = FlowTouch::of_diff(&diff);
        assert!(touch.every_instance && !touch.is_empty());
        assert!(touch.instances.is_empty() && touch.proposals.is_empty());
    }

    /// One pass queued at a time: the first touch spawns, later ones merge
    /// into it, `take` hands everything to the running pass and re-arms.
    #[test]
    fn a_queue_holds_one_pass_and_merges_bursts_into_it() {
        let mut queue = FlowPassQueue::default();
        let first = FlowTouch {
            instances: set(&["ad4m://flow/instance/a"]),
            proposals: HashSet::new(),
            every_instance: false,
        };
        let second = FlowTouch {
            instances: HashSet::new(),
            proposals: set(&["ad4m://flow/proposal/p"]),
            every_instance: true,
        };
        assert!(queue.enqueue(first.clone()), "the first touch spawns");
        assert!(!queue.enqueue(second.clone()), "the second rides along");

        let taken = queue.take();
        assert_eq!(taken.instances, first.instances);
        assert_eq!(taken.proposals, second.proposals);
        assert!(
            taken.every_instance,
            "a sweep-everything touch survives the merge"
        );

        assert!(
            queue.enqueue(first),
            "after take, the next touch spawns again — nothing lands in a gap"
        );
        assert!(queue.take().proposals.is_empty(), "take drains");
    }

    // ---- flow_pass_agents ---------------------------------------------

    fn emails(contexts: &[AgentContext]) -> Vec<Option<&str>> {
        contexts
            .iter()
            .map(|c| c.user_email.as_deref())
            .collect::<Vec<_>>()
    }

    #[test]
    fn single_user_mode_sweeps_as_the_main_agent_only() {
        // Even with users in the DB — a node that had multi-user switched
        // off still has the rows — single-user mode must not fan out.
        let contexts = flow_pass_agents(false, vec!["a@example.com".into()]);
        assert_eq!(emails(&contexts), vec![None]);
        assert!(contexts[0].is_main_agent);
    }

    #[test]
    fn multi_user_sweeps_for_the_main_agent_and_every_managed_user() {
        let contexts = flow_pass_agents(true, vec!["b@example.com".into(), "a@example.com".into()]);
        assert_eq!(
            emails(&contexts),
            vec![None, Some("a@example.com"), Some("b@example.com")],
            "main agent first, then managed users in a stable order"
        );
        assert!(
            contexts[1..].iter().all(|c| !c.is_main_agent),
            "a managed user's pass must sign as that user, not as the host"
        );
    }

    #[test]
    fn multi_user_with_no_managed_users_still_sweeps_for_the_main_agent() {
        // The DB read failing, or a node before its first user signs up:
        // the host's own view must still be maintained.
        assert_eq!(emails(&flow_pass_agents(true, Vec::new())), vec![None]);
    }

    #[test]
    fn a_duplicated_user_is_swept_once() {
        let contexts = flow_pass_agents(true, vec!["a@example.com".into(), "a@example.com".into()]);
        assert_eq!(emails(&contexts), vec![None, Some("a@example.com")]);
    }
}
