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

    /// The agents [`flow_pass_agents`] names, with this perspective's owner
    /// DIDs read from its handle and resolved against the node's agents.
    ///
    /// Two reads, both cheap and neither touching the wallet mutex: the
    /// handle's `owners`, and — only when an owner is not the main agent —
    /// the user DB, whose `UserInfo` carries the DID this maps by.
    ///
    /// The user DB is read only in multi-user mode, because that is the only
    /// mode in which an owner can be anyone but the main agent. That is an
    /// optimisation, not a second rule: the rule is `flow_pass_agents`'s, and
    /// an owner whose DID resolves to nobody is skipped there either way.
    ///
    /// Signing keys are **not** pre-checked. A user whose key is not in the
    /// wallet makes that user's write fail with the pass's existing
    /// "recording … rolled back (re-runs next pass)" warning — loud, and
    /// harmless because the derivation is a pure fold that repeats. The
    /// alternative, `WalletBackend::key_exists`, falls back to a blocking
    /// HTTP call on `SharedWallet` — the very backend a multi-user node
    /// runs — so pre-checking would trade a rare loud failure for a wallet
    /// round-trip per user per pass. Note also that nothing here calls
    /// `ensure_user_key`: a missing key must never be answered by minting a
    /// fresh DID underneath an existing user.
    async fn flow_pass_contexts(&self) -> Vec<AgentContext> {
        let owners = self.persisted.lock().await.get_owners();
        if owners.is_empty() {
            return flow_pass_agents(&[], None, &[]);
        }

        let main_did = crate::agent::AgentService::with_global_instance(|a| a.did.clone());
        let managed_users: Vec<(String, String)> =
            if crate::user_management::is_multi_user_enabled() {
                match crate::db::Ad4mDb::with_global_instance(|db| db.list_users()) {
                    Ok(users) => users.into_iter().map(|u| (u.did, u.username)).collect(),
                    Err(e) => {
                        // The main agent still resolves from `owners` alone, so
                        // the host's own view stays correct; the managed users
                        // heal on the next touch.
                        log::warn!(
                            "sync-triggered flow pass on {}: could not list users ({e}); \
                             resolving owners against the main agent only",
                            self.uuid
                        );
                        Vec::new()
                    }
                }
            } else {
                Vec::new()
            };

        let contexts = flow_pass_agents(&owners, main_did.as_deref(), &managed_users);
        log::debug!(
            "sync-triggered flow pass on {}: {} of {} owner(s) are agents on this node",
            self.uuid,
            contexts.len(),
            owners.len()
        );
        contexts
    }
}

/// Every agent whose `Local` bookkeeping a flow pass on this perspective
/// must advance: **this perspective's owners, and nobody else.**
///
/// `owners` is the handle's owner DID list, `main_agent_did` this node's own
/// DID, and `managed_users` the `(did, email)` of the node's managed users.
/// An owner DID is the main agent's, or a managed user's, or neither — and
/// *neither* is skipped: it names an agent this node does not act for, whose
/// bookkeeping is kept on its own node.
///
/// **Why more than one.** The derived-state cache and the fired marks are
/// `Local` links, and managed users get their own local link space (the
/// per-user `Local` work in flight). A single main-agent pass would leave
/// every managed user's bookkeeping un-advanced: they would read a stale
/// `currentState`, and their first later pass would report a settle that
/// happened long ago as new — the `first_pass_here` catch-up in
/// [`pass`](super::pass) suppresses the flood only once.
///
/// **Why the main agent is not unconditional.** A hosted node holds
/// perspectives it is not an owner of: a neighbourhood one managed user
/// joined is that user's, and the main agent cannot even read it
/// (`check_main_agent_access` is `is_owned_by` once a perspective has
/// owners, which is why `perspectives_ws` denies it). Sweeping as the main
/// agent there would write `Local` links for an agent that has nothing to
/// read them.
///
/// **Why an unowned perspective is still the main agent's.** `owners` is
/// populated on neighbourhood publish and join only (`neighbourhoods.rs`),
/// so a plain local perspective has none — and `check_main_agent_access`
/// grants the main agent an unowned perspective for exactly that reason.
/// Empty `owners` therefore means `[main_agent]`, which is also every
/// single-user node's normal case.
///
/// **Why all owners and not only the online ones.** Unlike the
/// auto-processor — where an offline user simply misses an LLM pass another
/// loop will redo — a missed flow pass leaves a value the user *reads*
/// stale, and nothing re-triggers it: passes run on a link touch, a local
/// vote, or an interpretation run, so an absent user's cache would stay
/// wrong until the next unrelated link arrives in that flow. The pass is a
/// fold over the links present now, with no LLM call, so running it for a
/// quiet owner costs store reads and nothing else.
///
/// Owners keep their listed order and are deduplicated, so a pass sweeps in
/// the same order every time.
///
/// Cost note: each context currently re-reads the shared links too
/// (`load_shacl_flows`, `load_flow_instances`, the read set). Only the
/// cache and mark reads are genuinely per agent. Splitting
/// `run_flow_consensus_pass` into derive-once / record-per-agent is the
/// optimisation to make when the per-user `Local` links land, and is left
/// until then so this change stays reviewable against that PR.
pub fn flow_pass_agents(
    owners: &[String],
    main_agent_did: Option<&str>,
    managed_users: &[(String, String)],
) -> Vec<AgentContext> {
    if owners.is_empty() {
        return vec![AgentContext::main_agent()];
    }

    let mut contexts: Vec<AgentContext> = Vec::new();
    for owner in owners {
        let context = if Some(owner.as_str()) == main_agent_did {
            AgentContext::main_agent()
        } else {
            match managed_users
                .iter()
                .find(|(did, _)| did == owner)
                .map(|(_, email)| email.clone())
            {
                Some(email) => AgentContext::for_user_email(email),
                // An owner this node does not act for: a peer that joined the
                // neighbourhood from its own executor, or a user that has since
                // been removed. Its bookkeeping is not ours to advance.
                None => continue,
            }
        };
        if !contexts
            .iter()
            .any(|c| c.user_email == context.user_email && c.is_main_agent == context.is_main_agent)
        {
            contexts.push(context);
        }
    }
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
                ..Default::default()
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

    const MAIN: &str = "did:key:zMain";
    const ALICE: &str = "did:key:zAlice";
    const BOB: &str = "did:key:zBob";

    fn users() -> Vec<(String, String)> {
        vec![
            (ALICE.into(), "a@example.com".into()),
            (BOB.into(), "b@example.com".into()),
        ]
    }

    #[test]
    fn an_unowned_perspective_is_the_main_agents() {
        // `owners` is populated on neighbourhood publish/join only, so a plain
        // local perspective has none — and that is the single-user norm. Users
        // in the DB must not fan a pass out over a perspective nobody claims.
        let contexts = flow_pass_agents(&[], Some(MAIN), &users());
        assert_eq!(emails(&contexts), vec![None]);
        assert!(contexts[0].is_main_agent);
    }

    #[test]
    fn owners_are_swept_in_their_listed_order() {
        let owners = vec![BOB.to_string(), MAIN.to_string(), ALICE.to_string()];
        let contexts = flow_pass_agents(&owners, Some(MAIN), &users());
        assert_eq!(
            emails(&contexts),
            vec![Some("b@example.com"), None, Some("a@example.com")],
            "one context per owner, in the handle's order"
        );
        assert!(
            contexts[1].is_main_agent,
            "the main agent's own DID resolves to the main-agent context"
        );
        assert!(
            !contexts[0].is_main_agent && !contexts[2].is_main_agent,
            "a managed user's pass must sign as that user, not as the host"
        );
    }

    #[test]
    fn a_perspective_the_main_agent_does_not_own_is_not_swept_for_it() {
        // The hosted case: one managed user joined a neighbourhood. The main
        // agent cannot read it (`perspectives_ws` denies it), so writing its
        // `Local` bookkeeping there would be bookkeeping nobody reads.
        let contexts = flow_pass_agents(&[ALICE.to_string()], Some(MAIN), &users());
        assert_eq!(emails(&contexts), vec![Some("a@example.com")]);
    }

    #[test]
    fn an_owner_this_node_does_not_act_for_is_skipped() {
        // A peer that joined from its own executor, or a removed user: its
        // bookkeeping lives on its own node. Skipped, never silently swept as
        // the main agent — that would sign one agent's cache with another's key.
        let owners = vec!["did:key:zRemotePeer".to_string(), MAIN.to_string()];
        assert_eq!(
            emails(&flow_pass_agents(&owners, Some(MAIN), &users())),
            vec![None]
        );

        // And with no resolvable owner at all, nothing here has bookkeeping to
        // keep: no pass, rather than a main-agent fallback.
        assert!(
            flow_pass_agents(&["did:key:zRemotePeer".to_string()], Some(MAIN), &users()).is_empty()
        );
    }

    #[test]
    fn a_duplicated_owner_is_swept_once() {
        let owners = vec![ALICE.to_string(), ALICE.to_string(), MAIN.to_string()];
        assert_eq!(
            emails(&flow_pass_agents(&owners, Some(MAIN), &users())),
            vec![Some("a@example.com"), None]
        );
    }

    #[test]
    fn without_a_main_agent_did_only_managed_owners_resolve() {
        // The user DB read failed, or the agent is not yet initialised: the
        // owners that still resolve are swept, and no context is invented.
        let owners = vec![MAIN.to_string(), ALICE.to_string()];
        assert_eq!(
            emails(&flow_pass_agents(&owners, None, &users())),
            vec![Some("a@example.com")]
        );
        assert!(flow_pass_agents(&owners, Some(MAIN), &[]).len() == 1);
    }
}
