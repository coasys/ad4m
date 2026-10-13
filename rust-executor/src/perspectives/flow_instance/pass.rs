//! The background sweep: derive every instance in scope, then record what
//! the fold already decided.
//!
//! This module observes; it does not decide. It records exactly two things,
//! per agent, in [`FlowReplicaState`] — the executor's own tables, not the
//! perspective (#1424):
//!
//! - **State** — what this agent's pass last derived, so a reader without a
//!   fold (a UI through `perspective.flowStates`, a prompt block) sees the
//!   fold's answer. The fold never takes it as input; this pass heals it
//!   whenever it lags.
//! - **Fired marks** — the proposals whose votes settled an edge. A history
//!   index for UIs (`perspective.flowFired`), and this pass's "have I already
//!   recorded this consensus event?" test, so [`FireOutcome`]s are emitted
//!   once per event per replica-agent. Never an input to the fold.
//!
//! When either moves, the pass publishes a `flow-state-changed` event to that
//! agent's sockets.
//!
//! Before #1424 both were `Local` links (#987), which made them part of the
//! graph every other agent on the node shares. Out of the graph they are
//! exactly what they say — what *this* agent's replica derived — and no
//! query, sync or co-owner can see or forge them.
//!
//! **It deletes nothing.** A replica's view is partial by construction: a
//! proposal can arrive before the evidence it cites, and the settling links
//! of an earlier transition can arrive after a later one. Deleting on the
//! strength of a partial view destroys honest history that sync was about to
//! vindicate, so the right response to a disagreement is to wait for links,
//! never to remove them. Superseded and unverifiable proposals therefore
//! linger; explicit rejection is a human act and lives elsewhere.
//!
//! ## Terms
//!
//! - **Replica** — one agent's executor holding its own local copy of the
//!   shared perspective; every neighbourhood member (human client or bot) is
//!   one. Replicas receive links in whatever order sync delivers them, so
//!   each derives state from a view that may still be missing links others
//!   already hold — which is why the code says "new to *this replica*",
//!   never just "new". On a multi-user node each owner agent keeps its own
//!   rows, so "replica" here means replica-agent.
//!
//! ## Catch-up
//!
//! Because the marks are per replica, a replica that joins a flow with
//! history would, on its first pass, find every settled edge unmarked and
//! report each one as new. It must not: those events happened before this
//! replica was watching. So the **first** pass this agent runs over an
//! instance — no row for it yet ([`FlowReplicaState::has_derived`]) — is a
//! silent catch-up: it marks what has settled and writes the state, and
//! emits nothing. From then on the instance has a row, so every later pass
//! reports normally. The invariant: **no event flood on join; no missed
//! events for edges that settle after catch-up.**
//!
//! The agent that mints an instance seeds its row at the mint (Rust:
//! `mint_flow_instance`; a client's `FlowInstance.start`: the local-write
//! hook in `perspective_instance`), so its own first pass is not a catch-up
//! and its first settle is reported. An upgrade from the `Local`-link
//! bookkeeping starts with empty tables, so every instance's first pass
//! after it is a catch-up too: nothing old is re-reported.

use super::replica_state::{FlowReplicaState, FlowStateChangedEvent, PriorRow};
use super::{fold_read_set, FlowInstance};
use crate::agent::AgentContext;
use crate::perspectives::flow_context::{
    load_all_flow_instances, load_flow_instances, load_shacl_flows, retain_selected_flows,
    scope_subject,
};
use crate::perspectives::model_query::types::Scope;
use crate::perspectives::perspective_instance::PerspectiveInstance;

/// One consensus event this replica recorded for the first time: the atoms
/// that settled an edge, now marked, with the cache advanced to match.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, ts_rs::TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct FireOutcome {
    pub instance_uri: String,
    pub from_state: String,
    pub to_state: String,
    /// The distinct DIDs whose votes made up the quorum.
    pub voters: Vec<String>,
    pub contributing_proposal_uris: Vec<String>,
}

/// Catch `context`'s agent up on `instance_uri` — silently, as every first
/// pass is — when it has never derived that instance here. Called right
/// before the agent writes a vote of its own (`accept`, `propose`).
///
/// Without it, an agent whose first contact with an instance is its own vote
/// — a co-owner on the same node pressing a button on someone else's run —
/// would have that vote's settle swallowed: the pass after the vote is its
/// first, so it is a catch-up, and the agent would be told nothing fired.
/// Catching up first records the history quietly, and the pass after the
/// vote then reports exactly what the vote settled. Failures are logged and
/// ignored: the worst case is the old behaviour, a silent first pass.
pub(crate) async fn catch_up_before_own_vote(
    perspective: &mut PerspectiveInstance,
    context: &AgentContext,
    instance_uri: &str,
) {
    let has_row = crate::agent::did_for_context(context)
        .and_then(|did| FlowReplicaState::has_derived(&perspective.uuid, &did, instance_uri));
    match has_row {
        Ok(true) => {}
        Ok(false) => {
            let only = [instance_uri.to_string()];
            let reported =
                run_flow_consensus_pass(perspective, None, context, None, Some(&only)).await;
            debug_assert!(reported.is_empty(), "a first pass reports nothing");
        }
        Err(e) => log::warn!(
            "catch_up_before_own_vote: reading the bookkeeping of {instance_uri} failed; \
             the vote's own pass may catch up silently: {e:#}"
        ),
    }
}

/// After `writer`'s own pass: the same pass for every other agent this
/// node keeps bookkeeping for on the perspective — its owners here, as a
/// sync-triggered pass sweeps them (`flow_pass_contexts`).
///
/// A write made on this node never syncs back to it, so nothing else would
/// run their pass: a co-owner's rows would lag a vote cast here until
/// something else touched the instance, and that co-owner's next vote would
/// report this one's settle as its own. Before #1424 this did not arise,
/// because every agent on the node shared the one `Local` cache.
///
/// Their outcomes reach them as their own `flow-state-changed` events; the
/// caller still returns the writer's.
pub(crate) async fn run_flow_pass_for_co_owners(
    perspective: &mut PerspectiveInstance,
    writer: &AgentContext,
    flow_filter: Option<&[String]>,
    instance_filter: Option<&[String]>,
) {
    for context in perspective.flow_pass_contexts().await {
        if same_agent(&context, writer) {
            continue;
        }
        let outcomes =
            run_flow_consensus_pass(perspective, None, &context, flow_filter, instance_filter)
                .await;
        for outcome in &outcomes {
            log::debug!(
                "flow settled for co-owner {:?}: {} {} → {}",
                context.user_email,
                outcome.instance_uri,
                outcome.from_state,
                outcome.to_state
            );
        }
    }
}

fn same_agent(a: &AgentContext, b: &AgentContext) -> bool {
    if a.is_main_agent || b.is_main_agent {
        a.is_main_agent == b.is_main_agent
    } else {
        a.user_email == b.user_email
    }
}

/// One sweep over the `FlowInstance`s in scope, as `context`'s agent: derive
/// each, and where that agent's recorded state or marks lag the fold, record
/// them in one transaction and report what is new.
///
/// Never fails: any error is logged and skips the narrowest safe unit
/// (instance < pass). `instance_filter` narrows the sweep to the named
/// instances, so a single accept — or a sync burst that touched two flows —
/// does not re-derive every flow on the perspective.
pub async fn run_flow_consensus_pass(
    perspective: &mut PerspectiveInstance,
    scope: Option<&Scope>,
    context: &AgentContext,
    flow_filter: Option<&[String]>,
    instance_filter: Option<&[String]>,
) -> Vec<FireOutcome> {
    let loaded = async {
        let agent_did = crate::agent::did_for_context(context)?;
        let mut flows_by_uri = load_shacl_flows(perspective).await?;
        retain_selected_flows(&mut flows_by_uri, flow_filter);
        let records = match scope {
            Some(s) => load_flow_instances(perspective, &[scope_subject(s).to_string()]).await?,
            None => load_all_flow_instances(perspective).await?,
        };
        anyhow::Ok((agent_did, flows_by_uri, records))
    }
    .await;
    let (agent_did, flows_by_uri, mut records) = match loaded {
        Ok(l) => l,
        Err(e) => {
            log::warn!("run_flow_consensus_pass: load failed: {e:#}");
            return Vec::new();
        }
    };
    if let Some(only) = instance_filter {
        records.retain(|r| only.contains(&r.instance_uri));
    }
    records.sort_by(|a, b| a.instance_uri.cmp(&b.instance_uri));
    let uuid = perspective.uuid.clone();

    let mut outcomes = Vec::new();
    for record in &records {
        let Some(flow) = flows_by_uri.get(&record.flow_uri) else {
            continue;
        };
        let instance = FlowInstance::from_record(record, flow);
        let read_set = match instance.read_set(perspective).await {
            Ok(rs) => rs,
            Err(e) => {
                log::warn!(
                    "run_flow_consensus_pass: reading {} failed; skipping instance this pass: {e:#}",
                    record.instance_uri
                );
                continue;
            }
        };
        // Same treatment as a failed read: unresolvable role evidence means
        // this instance has no derivable verdict this pass. Folding on with a
        // candidate's grants missing could fire an edge that contention would
        // otherwise have held — see `role_grant_views`.
        //
        // The raw read-set, not `reverified()` — and this is the ONE production
        // fold that does not re-verify. The invariant everywhere else is "no
        // fold consumes a verdict it did not compute"; the exception is sound
        // here only because `read_set` was just read from *this replica's own*
        // store, so every `proof.valid` and every `status` on it is already
        // this replica's own conclusion. There is nothing to re-derive, and
        // `status` in particular could not be re-derived at all — locality is
        // a fact about whose store a link sits in, not a property of its bytes.
        //
        // That reason lives outside this function, which is why it is written
        // here: if any sync path ever writes a *sender's* `proof.valid` into
        // the store, this call becomes the hole in the invariant and nothing
        // near it would say so. Anything that arrived from elsewhere must be
        // folded through `ReadSet::reverified` instead — see `verify.rs`
        // and <https://github.com/coasys/ad4m/issues/1068>.
        let derived = match fold_read_set(flow, &read_set) {
            Ok(d) => d,
            Err(e) => {
                log::warn!(
                    "run_flow_consensus_pass: folding {} failed; skipping instance this pass: {e:#}",
                    record.instance_uri
                );
                continue;
            }
        };

        // Only the settled atoms can be new, so only they are looked up.
        let settled_atoms: Vec<String> = derived
            .settled
            .iter()
            .flat_map(|edge| edge.atom_uris.iter().cloned())
            .collect();
        let bookkeeping = FlowReplicaState::fired_among(&uuid, &agent_did, &settled_atoms)
            .and_then(|marked| {
                let state =
                    FlowReplicaState::current_state(&uuid, &agent_did, &record.instance_uri)?;
                let has_row = state.is_some()
                    || FlowReplicaState::has_derived(&uuid, &agent_did, &record.instance_uri)?;
                Ok((marked, state, has_row))
            });
        let (already_marked, recorded_state, has_row) = match bookkeeping {
            Ok(b) => b,
            Err(e) => {
                log::warn!(
                    "run_flow_consensus_pass: reading the bookkeeping of {} failed; skipping instance this pass: {e:#}",
                    record.instance_uri
                );
                continue;
            }
        };

        // Catch-up (module doc): no row = never derived nor minted here by
        // this agent. Marks are per replica-agent, so on a join every settled
        // edge is unmarked, and reporting them all would be a flood of events
        // that happened before this replica watched. The pass still marks them
        // and ALWAYS writes the state — that row is what makes the next pass
        // an ordinary one, so an edge settling afterwards is not missed.
        // Invariant: no event flood on join; no missed events for edges that
        // settle after catch-up.
        let first_pass_here = !has_row;

        // An edge is new to this replica when some atom that settled it is
        // not yet marked. The mark is bookkeeping, so this comparison can
        // never change the state — only how much of it we still have to note.
        let mut to_mark: Vec<String> = Vec::new();
        let mut fresh: Vec<FireOutcome> = Vec::new();
        for edge in &derived.settled {
            let unmarked: Vec<String> = edge
                .atom_uris
                .iter()
                .filter(|uri| !already_marked.contains(*uri))
                .cloned()
                .collect();
            if unmarked.is_empty() {
                continue;
            }
            to_mark.extend(unmarked);
            fresh.push(FireOutcome {
                instance_uri: record.instance_uri.clone(),
                from_state: edge.from_state.clone(),
                to_state: edge.to_state.clone(),
                voters: edge.voters.clone(),
                contributing_proposal_uris: edge.atom_uris.clone(),
            });
        }
        let stale_state = recorded_state.as_deref() != Some(derived.state.as_str());
        if !stale_state && to_mark.is_empty() {
            continue;
        }
        if stale_state && recorded_state.is_some() {
            log::debug!(
                "run_flow_consensus_pass: healing {} — recorded `{}`, derived `{}`",
                record.instance_uri,
                recorded_state.as_deref().unwrap_or_default(),
                derived.state
            );
        }
        let recorded = match FlowReplicaState::record_pass(
            &uuid,
            &agent_did,
            &record.instance_uri,
            stale_state.then_some(derived.state.as_str()),
            &to_mark,
        ) {
            Ok(recorded) => recorded,
            Err(e) => {
                log::warn!(
                    "run_flow_consensus_pass: recording {} failed (re-runs next pass): {e:#}",
                    record.instance_uri
                );
                continue;
            }
        };
        // The reads above ran outside the transaction, so another pass for
        // this agent may have recorded in between. The transaction's own
        // answers decide: a row it found means this was no first pass, and
        // an edge is this pass's to report only if it inserted a mark of it.
        let first_pass_here = first_pass_here && recorded.prior == PriorRow::Absent;
        fresh.retain(|outcome| {
            outcome
                .contributing_proposal_uris
                .iter()
                .any(|uri| recorded.newly_marked.contains(uri))
        });
        if first_pass_here {
            log::debug!(
                "run_flow_consensus_pass: first derivation of {} for {agent_did} on this replica — caught up silently at `{}` ({} settled edge(s) marked, none reported)",
                record.instance_uri,
                derived.state,
                fresh.len()
            );
            continue;
        }
        // `from` is what this agent last recorded; a row seeded at mint has
        // none yet, and a minted instance stands at its flow's first state.
        let from = match recorded.prior {
            PriorRow::Derived(s) => s,
            PriorRow::Minted | PriorRow::Absent => read_set.genesis.clone(),
        };
        if from != derived.state || !recorded.newly_marked.is_empty() {
            FlowReplicaState::publish_state_changed(&FlowStateChangedEvent {
                perspective_uuid: uuid.clone(),
                agent_did: agent_did.clone(),
                instance: record.instance_uri.clone(),
                from,
                to: derived.state.clone(),
                fired_proposals: recorded.newly_marked,
            })
            .await;
        }
        outcomes.extend(fresh);
    }
    outcomes
}
