//! This replica's flow bookkeeping, per agent, **outside the perspective**
//! (#1424).
//!
//! The consensus pass keeps two things that are not inputs to the fold:
//!
//! - **State** — what this agent's pass last derived for an instance, so a
//!   reader without a fold (a UI, a prompt block) can show it.
//! - **Fired marks** — the proposals whose votes settled an edge, as this
//!   agent's pass recorded them. They are a history index, and they are how
//!   the pass tells a new consensus event from one it has already reported,
//!   so a [`FireOutcome`](super::pass::FireOutcome) is emitted once per event
//!   per replica-agent.
//!
//! Both live in two executor SQLite tables (`flow_replica_state`,
//! `flow_fired_mark` in [`Ad4mDb`]). They are not links, so they never sync,
//! no model query sees them, and no other agent on the same executor can read
//! them through the graph. Rows are keyed by `(perspective, agent_did, …)`
//! because the pass runs once per owner agent on a multi-user executor, and are
//! deleted with the perspective.
//!
//! Before #1424 both were `Local` links (#987). Those links, where they still
//! exist, are dead data: nothing writes or reads them any more. After an
//! upgrade the tables start empty, so the first pass over each instance is
//! the silent catch-up of [`pass`](super::pass) — no flood of old events.
//!
//! Clients read this through `perspective.flowStates` / `perspective.flowFired`
//! and the `flow-state-changed` event, all scoped to the caller's own agent.

use crate::db::{Ad4mDb, FlowReplicaStateRow};
use crate::events::{Audience, Event, Topic};
use crate::pubsub::get_global_pubsub;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicBool, Ordering};

/// One row of this agent's bookkeeping, as `perspective.flowStates` returns
/// it. An instance with no entry is one this agent has neither derived nor
/// minted here: a client must not guess its state.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, ts_rs::TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct FlowStateEntry {
    pub instance: String,
    /// The state this agent's pass last derived. `None` for a row seeded at
    /// mint and not derived yet: the instance stands at its flow's first
    /// state, which only the flow definition names.
    pub current_state: Option<String>,
    /// RFC 3339 time of the pass that last wrote this state; `None` with it.
    pub derived_at: Option<String>,
}

/// An instance's derived state moved, or an edge settled, on one agent's
/// replica. Published once per change by that agent's pass, and delivered
/// only to that agent's sockets.
///
/// A catch-up pass (this agent's first over an instance here) publishes one
/// too, with `caught_up` set: it reports no settles — whatever settled
/// before this replica watched is not news — but it is the first state this
/// agent's clients can show, and the only signal they get of it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, ts_rs::TS)]
#[serde(rename_all = "camelCase")]
pub struct FlowStateChangedEvent {
    pub perspective_uuid: String,
    /// The agent whose replica derived this: the event's only recipient,
    /// when it owns the perspective.
    pub agent_did: String,
    pub instance: String,
    /// The state this agent had recorded before; for a seeded row, the
    /// flow's first state. `None` exactly when `caught_up`: this agent had no
    /// row, so its clients had no state to show (`currentStateName` is
    /// `undefined`), and naming the first state would claim a move nobody saw.
    pub from: Option<String>,
    pub to: String,
    /// Proposals this pass marked fired: the atoms of every edge that settled
    /// since the previous pass. Empty when the state moved without a new
    /// settle, e.g. back, after a vote was withdrawn — and on a catch-up,
    /// whose marks record history rather than report it.
    pub fired_proposals: Vec<String>,
    /// This was the agent's first pass over the instance here.
    pub caught_up: bool,
}

/// What a recorded pass found in the table before writing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PriorRow {
    /// Nothing: this agent never derived nor minted the instance here.
    Absent,
    /// Seeded at mint, not derived yet.
    Minted,
    Derived(String),
}

/// Who receives a `flow-state-changed`: the agent whose bookkeeping moved
/// (`agent_did`), when it is one of the perspective's `owners`
/// (`perspectives::event_recipients`); else nobody. Bookkeeping is per agent,
/// so a co-owner, or another user of the same executor, never gets it.
pub(crate) fn state_changed_audience(owners: &[String], agent_did: &str) -> Audience {
    let owner = owners.iter().find(|o| *o == agent_did).cloned();
    Audience::Dids(owner.into_iter().collect())
}

/// The `flow-state-changed` event for `event`: to [`state_changed_audience`],
/// needing the query capability on the perspective (`perspective.flowStates`
/// reads the same rows).
pub(crate) fn state_changed_event(owners: &[String], event: &FlowStateChangedEvent) -> Event {
    Event::new(
        Topic::FlowStateChanged,
        state_changed_audience(owners, &event.agent_did),
        event,
    )
}

impl From<Option<FlowReplicaStateRow>> for PriorRow {
    fn from(row: Option<FlowReplicaStateRow>) -> Self {
        match row {
            None => PriorRow::Absent,
            Some(FlowReplicaStateRow {
                current_state: None,
                ..
            }) => PriorRow::Minted,
            Some(FlowReplicaStateRow {
                current_state: Some(s),
                ..
            }) => PriorRow::Derived(s),
        }
    }
}

/// What [`FlowReplicaState::record_pass`] did: the row it found, and the
/// marks it inserted. The transaction that inserts a mark is the one that
/// reports its edge, so each edge is reported once even if two passes for
/// one agent overlap. Votes and background sweeps do not overlap (they share
/// the perspective's `flow_pass_lock`, so a vote's own pass is the one that
/// reports its settle); this covers passes run outside it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PassRecord {
    pub prior: PriorRow,
    pub newly_marked: Vec<String>,
}

/// Thin service over [`Ad4mDb`]'s two flow tables. Stateless: every call
/// names the perspective and the agent.
pub struct FlowReplicaState;

impl FlowReplicaState {
    /// Derived state per instance for `agent_did`; `instances` narrows the
    /// read. Seeded-but-underived rows are left out.
    pub fn states(
        perspective: &str,
        agent_did: &str,
        instances: Option<&[String]>,
    ) -> anyhow::Result<HashMap<String, String>> {
        Ok(Self::rows(perspective, agent_did, instances)?
            .into_iter()
            .filter_map(|r| r.current_state.map(|s| (r.instance, s)))
            .collect())
    }

    /// Every row in the RPC's shape, ordered by instance — seeded ones too,
    /// with no state, so a client can tell them from instances with no row.
    pub fn entries(
        perspective: &str,
        agent_did: &str,
        instances: Option<&[String]>,
    ) -> anyhow::Result<Vec<FlowStateEntry>> {
        Ok(Self::rows(perspective, agent_did, instances)?
            .into_iter()
            .map(|r| FlowStateEntry {
                instance: r.instance,
                derived_at: r.current_state.as_ref().and(r.derived_at),
                current_state: r.current_state,
            })
            .collect())
    }

    /// What this agent has recorded for one instance — one primary-key read
    /// that answers both "is there a row?" and "what state?", which is what
    /// the pass needs per instance.
    pub fn recorded(
        perspective: &str,
        agent_did: &str,
        instance: &str,
    ) -> anyhow::Result<PriorRow> {
        Ad4mDb::with_global_instance(|db| {
            db.get_flow_replica_state(perspective, agent_did, instance)
        })
        .map(PriorRow::from)
        .map_err(|e| anyhow::anyhow!("reading the flow state of {instance}: {e}"))
    }

    /// This agent's derived state for one instance, if it has one.
    pub fn current_state(
        perspective: &str,
        agent_did: &str,
        instance: &str,
    ) -> anyhow::Result<Option<String>> {
        Ok(match Self::recorded(perspective, agent_did, instance)? {
            PriorRow::Derived(state) => Some(state),
            PriorRow::Minted | PriorRow::Absent => None,
        })
    }

    /// Whether this agent has a row for the instance — derived here, or
    /// minted here. `false` is exactly the catch-up case of
    /// [`pass`](super::pass).
    pub fn has_derived(perspective: &str, agent_did: &str, instance: &str) -> anyhow::Result<bool> {
        Ok(Self::recorded(perspective, agent_did, instance)? != PriorRow::Absent)
    }

    /// Note that `agent_did` minted `instance` on this replica, so its first
    /// pass reports what settles rather than catching up silently. A no-op
    /// when a row exists, or once the perspective is torn down (see
    /// [`record_pass`](Self::record_pass)). Returns whether one was inserted.
    pub fn seed_minted(
        perspective: &str,
        agent_did: &str,
        instance: &str,
        torn_down: &AtomicBool,
    ) -> anyhow::Result<bool> {
        Ad4mDb::with_global_instance(|db| {
            if torn_down.load(Ordering::Acquire) {
                return Ok(false);
            }
            db.seed_flow_replica_state(perspective, agent_did, instance)
        })
        .map_err(|e| anyhow::anyhow!("seeding flow state of {instance}: {e}"))
    }

    /// Write `state` for the instance. Returns whether it changed; `false`
    /// once the perspective is torn down, when nothing is written.
    pub fn set_current_state(
        perspective: &str,
        agent_did: &str,
        instance: &str,
        state: &str,
        torn_down: &AtomicBool,
    ) -> anyhow::Result<bool> {
        Ok(Self::record_pass(
            perspective,
            agent_did,
            instance,
            Some(state),
            &[],
            torn_down,
        )?
        .is_some_and(|before| before.prior != PriorRow::Derived(state.to_string())))
    }

    /// One pass's write: the state (when `Some`) and the new fired marks,
    /// in one transaction. Returns what the row held before and which of
    /// `fired` this call was the first to mark.
    ///
    /// `None`, writing nothing, once `torn_down` — the perspective's
    /// `is_teardown` — is set. `perspectives::remove_perspective` sets it
    /// before it deletes the perspective's rows, and both the delete and
    /// this check run under the one DB lock. So a pass still running when
    /// the perspective goes either writes before the delete, which removes
    /// the row, or finds the flag set: it cannot leave a row behind.
    pub fn record_pass(
        perspective: &str,
        agent_did: &str,
        instance: &str,
        state: Option<&str>,
        fired: &[String],
        torn_down: &AtomicBool,
    ) -> anyhow::Result<Option<PassRecord>> {
        if state == Some("") {
            return Err(anyhow::anyhow!(
                "an empty state would read as `not yet derived`; refusing to record it for {instance}"
            ));
        }
        let at = chrono::Utc::now().to_rfc3339();
        Ad4mDb::with_global_instance(|db| {
            if torn_down.load(Ordering::Acquire) {
                return Ok(None);
            }
            db.record_flow_replica_pass(perspective, agent_did, instance, state, fired, &at)
                .map(Some)
        })
        .map(|recorded| {
            recorded.map(|(before, newly_marked)| PassRecord {
                prior: PriorRow::from(before),
                newly_marked,
            })
        })
        .map_err(|e| anyhow::anyhow!("recording the flow pass over {instance}: {e}"))
    }

    pub fn is_fired(perspective: &str, agent_did: &str, proposal: &str) -> anyhow::Result<bool> {
        Ok(!Self::fired_among(perspective, agent_did, &[proposal.to_string()])?.is_empty())
    }

    /// The subset of `proposals` this agent has marked fired.
    pub fn fired_among(
        perspective: &str,
        agent_did: &str,
        proposals: &[String],
    ) -> anyhow::Result<HashSet<String>> {
        Ad4mDb::with_global_instance(|db| {
            db.get_flow_fired_among(perspective, agent_did, proposals)
        })
        .map_err(|e| anyhow::anyhow!("reading fired marks: {e}"))
    }

    /// Mark `proposals` (all of `instance`) fired without touching the state.
    /// The pass never calls this: it records marks and state together in
    /// [`record_pass`](Self::record_pass), one transaction.
    #[cfg(test)]
    pub fn mark_fired(
        perspective: &str,
        agent_did: &str,
        instance: &str,
        proposals: &[String],
    ) -> anyhow::Result<()> {
        let live = AtomicBool::new(false);
        Self::record_pass(perspective, agent_did, instance, None, proposals, &live).map(|_| ())
    }

    /// Every proposal of `instance` this agent has marked fired, sorted.
    pub fn fired_for_instance(
        perspective: &str,
        agent_did: &str,
        instance: &str,
    ) -> anyhow::Result<Vec<String>> {
        Ad4mDb::with_global_instance(|db| {
            db.get_flow_fired_for_instance(perspective, agent_did, instance)
        })
        .map_err(|e| anyhow::anyhow!("reading fired marks of {instance}: {e}"))
    }

    /// Drop every row of the perspective, for every agent. Production removal
    /// goes through `Ad4mDb::remove_perspective`, which does the same in the
    /// same call that drops the perspective's links.
    #[cfg(test)]
    pub fn remove_perspective(perspective: &str) -> anyhow::Result<()> {
        Ad4mDb::with_global_instance(|db| db.remove_flow_replica_rows(perspective))
            .map_err(|e| anyhow::anyhow!("removing flow bookkeeping of {perspective}: {e}"))
    }

    /// Publish `event` as `flow-state-changed` to [`state_changed_audience`]: the agent
    /// whose bookkeeping moved, if it is one of the perspective's owners, read now.
    pub async fn publish_state_changed(
        perspective: &crate::perspectives::perspective_instance::PerspectiveInstance,
        event: &FlowStateChangedEvent,
    ) {
        let owners = crate::perspectives::event_recipients(&*perspective.persisted.lock().await);
        get_global_pubsub()
            .await
            .publish_event(state_changed_event(&owners, event));
    }

    fn rows(
        perspective: &str,
        agent_did: &str,
        instances: Option<&[String]>,
    ) -> anyhow::Result<Vec<FlowReplicaStateRow>> {
        Ad4mDb::with_global_instance(|db| {
            db.get_flow_replica_states(perspective, agent_did, instances)
        })
        .map_err(|e| anyhow::anyhow!("reading flow states: {e}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation_test_support::ensure_db_init;

    /// The teardown flag of a perspective that is not being removed.
    static LIVE: AtomicBool = AtomicBool::new(false);

    const ALICE: &str = "did:key:alice";
    const BOB: &str = "did:key:bob";

    fn fresh() -> String {
        ensure_db_init();
        uuid::Uuid::new_v4().to_string()
    }

    #[test]
    fn an_unknown_instance_has_no_row_and_no_state() {
        let p = fresh();
        assert!(!FlowReplicaState::has_derived(&p, ALICE, "i").unwrap());
        assert_eq!(
            FlowReplicaState::current_state(&p, ALICE, "i").unwrap(),
            None
        );
        assert!(FlowReplicaState::states(&p, ALICE, None)
            .unwrap()
            .is_empty());
    }

    #[test]
    fn set_current_state_reports_whether_it_changed() {
        let p = fresh();
        assert!(FlowReplicaState::set_current_state(&p, ALICE, "i", "open", &LIVE).unwrap());
        assert!(!FlowReplicaState::set_current_state(&p, ALICE, "i", "open", &LIVE).unwrap());
        assert!(FlowReplicaState::set_current_state(&p, ALICE, "i", "done", &LIVE).unwrap());
        assert_eq!(
            FlowReplicaState::current_state(&p, ALICE, "i")
                .unwrap()
                .as_deref(),
            Some("done")
        );
    }

    #[test]
    fn an_empty_state_is_refused() {
        let p = fresh();
        assert!(FlowReplicaState::set_current_state(&p, ALICE, "i", "", &LIVE).is_err());
        assert!(!FlowReplicaState::has_derived(&p, ALICE, "i").unwrap());
    }

    /// The key carries the agent: two agents on one executor never see each
    /// other's state or marks.
    #[test]
    fn rows_are_per_agent() {
        let p = fresh();
        FlowReplicaState::set_current_state(&p, ALICE, "i", "done", &LIVE).unwrap();
        FlowReplicaState::mark_fired(&p, ALICE, "i", &["p1".into()]).unwrap();
        assert!(!FlowReplicaState::has_derived(&p, BOB, "i").unwrap());
        assert!(!FlowReplicaState::is_fired(&p, BOB, "p1").unwrap());
        assert!(FlowReplicaState::states(&p, BOB, None).unwrap().is_empty());
        assert!(FlowReplicaState::fired_for_instance(&p, BOB, "i")
            .unwrap()
            .is_empty());
    }

    #[test]
    fn rows_are_per_perspective_and_removed_with_it() {
        let p = fresh();
        let q = fresh();
        FlowReplicaState::set_current_state(&p, ALICE, "i", "done", &LIVE).unwrap();
        FlowReplicaState::mark_fired(&p, ALICE, "i", &["p1".into()]).unwrap();
        FlowReplicaState::set_current_state(&q, ALICE, "i", "open", &LIVE).unwrap();

        FlowReplicaState::remove_perspective(&p).unwrap();
        assert!(!FlowReplicaState::has_derived(&p, ALICE, "i").unwrap());
        assert!(!FlowReplicaState::is_fired(&p, ALICE, "p1").unwrap());
        assert_eq!(
            FlowReplicaState::current_state(&q, ALICE, "i")
                .unwrap()
                .as_deref(),
            Some("open"),
            "another perspective's rows survive"
        );
    }

    /// A seeded row counts for the catch-up test but has no state to show.
    #[test]
    fn a_minted_row_has_derived_but_no_state() {
        let p = fresh();
        assert!(FlowReplicaState::seed_minted(&p, ALICE, "i", &LIVE).unwrap());
        assert!(!FlowReplicaState::seed_minted(&p, ALICE, "i", &LIVE).unwrap());
        assert!(FlowReplicaState::has_derived(&p, ALICE, "i").unwrap());
        assert_eq!(
            FlowReplicaState::current_state(&p, ALICE, "i").unwrap(),
            None
        );
        // On the wire a seeded row is listed with no state, so a client can
        // tell "minted here, at the first state" from "never seen here".
        assert_eq!(
            serde_json::to_value(FlowReplicaState::entries(&p, ALICE, None).unwrap()).unwrap(),
            serde_json::json!([{ "instance": "i", "currentState": null, "derivedAt": null }])
        );
        assert_eq!(
            serde_json::to_value(FlowReplicaState::entries(&p, BOB, None).unwrap()).unwrap(),
            serde_json::json!([]),
            "and an agent with no row gets nothing"
        );
        assert_eq!(
            FlowReplicaState::record_pass(&p, ALICE, "i", Some("open"), &[], &LIVE)
                .unwrap()
                .expect("not torn down")
                .prior,
            PriorRow::Minted
        );
    }

    /// Two passes that both found an edge unmarked: only the first to record
    /// it gets the mark back as new, so only that one reports the edge.
    #[test]
    fn a_mark_is_new_to_exactly_one_pass() {
        let p = fresh();
        let marks = ["p1".to_string(), "p2".to_string()];
        let first = FlowReplicaState::record_pass(&p, ALICE, "i", Some("done"), &marks, &LIVE)
            .unwrap()
            .expect("not torn down");
        let second = FlowReplicaState::record_pass(&p, ALICE, "i", Some("done"), &marks, &LIVE)
            .unwrap()
            .expect("not torn down");
        assert_eq!(first.newly_marked, marks.to_vec());
        assert!(second.newly_marked.is_empty());
        assert_eq!(second.prior, PriorRow::Derived("done".into()));
        // Per agent: Bob's first record of the same marks is new to him.
        let bob = FlowReplicaState::record_pass(&p, BOB, "i", Some("done"), &marks, &LIVE)
            .unwrap()
            .expect("not torn down");
        assert_eq!(bob.newly_marked, marks.to_vec());
    }

    /// Seeding never clobbers a derived state.
    #[test]
    fn seeding_an_existing_row_is_a_no_op() {
        let p = fresh();
        FlowReplicaState::set_current_state(&p, ALICE, "i", "done", &LIVE).unwrap();
        assert!(!FlowReplicaState::seed_minted(&p, ALICE, "i", &LIVE).unwrap());
        assert_eq!(
            FlowReplicaState::current_state(&p, ALICE, "i")
                .unwrap()
                .as_deref(),
            Some("done")
        );
    }

    #[test]
    fn record_pass_writes_state_and_marks_and_returns_the_prior_row() {
        let p = fresh();
        let first = FlowReplicaState::record_pass(
            &p,
            ALICE,
            "i",
            Some("scoped"),
            &["p1".into(), "p2".into()],
            &LIVE,
        )
        .unwrap()
        .expect("not torn down");
        assert_eq!(first.prior, PriorRow::Absent);
        assert_eq!(first.newly_marked, vec!["p1".to_string(), "p2".into()]);
        let second =
            FlowReplicaState::record_pass(&p, ALICE, "i", Some("done"), &["p3".into()], &LIVE)
                .unwrap()
                .expect("not torn down");
        assert_eq!(second.prior, PriorRow::Derived("scoped".into()));
        assert_eq!(second.newly_marked, vec!["p3".to_string()]);
        assert_eq!(
            FlowReplicaState::fired_for_instance(&p, ALICE, "i").unwrap(),
            vec!["p1".to_string(), "p2".into(), "p3".into()]
        );
        assert_eq!(
            FlowReplicaState::fired_among(&p, ALICE, &["p2".into(), "p9".into()]).unwrap(),
            HashSet::from(["p2".to_string()])
        );
        let entries = FlowReplicaState::entries(&p, ALICE, None).unwrap();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].current_state.as_deref(), Some("done"));
        let derived_at = entries[0]
            .derived_at
            .as_deref()
            .expect("a derived row has a time");
        assert!(
            chrono::DateTime::parse_from_rfc3339(derived_at).is_ok(),
            "derivedAt is RFC 3339: {derived_at}"
        );
    }

    #[test]
    fn marks_alone_leave_the_state_alone() {
        let p = fresh();
        FlowReplicaState::set_current_state(&p, ALICE, "i", "open", &LIVE).unwrap();
        FlowReplicaState::mark_fired(&p, ALICE, "i", &["p1".into()]).unwrap();
        FlowReplicaState::mark_fired(&p, ALICE, "i", &["p1".into()]).unwrap();
        assert_eq!(
            FlowReplicaState::current_state(&p, ALICE, "i")
                .unwrap()
                .as_deref(),
            Some("open")
        );
        assert_eq!(
            FlowReplicaState::fired_for_instance(&p, ALICE, "i").unwrap(),
            vec!["p1".to_string()]
        );
    }

    #[test]
    fn states_narrow_to_the_named_instances() {
        let p = fresh();
        for (i, s) in [("a", "x"), ("b", "y"), ("c", "z")] {
            FlowReplicaState::set_current_state(&p, ALICE, i, s, &LIVE).unwrap();
        }
        let only =
            FlowReplicaState::states(&p, ALICE, Some(&["a".into(), "c".into(), "zz".into()]))
                .unwrap();
        assert_eq!(
            only,
            HashMap::from([("a".to_string(), "x".to_string()), ("c".into(), "z".into())])
        );
    }

    /// Once the perspective is torn down, no write lands: not a pass's, not
    /// a mint's, not a seed. `perspectives::remove_perspective` deletes the
    /// rows after setting the flag, so a write that came later would leave
    /// rows behind for good (Marvin, #1425).
    #[test]
    fn nothing_is_written_once_the_perspective_is_torn_down() {
        let p = fresh();
        let torn_down = AtomicBool::new(true);
        assert_eq!(
            FlowReplicaState::record_pass(&p, ALICE, "i", Some("done"), &["p1".into()], &torn_down)
                .unwrap(),
            None
        );
        assert!(!FlowReplicaState::set_current_state(&p, ALICE, "i", "done", &torn_down).unwrap());
        assert!(!FlowReplicaState::seed_minted(&p, ALICE, "i", &torn_down).unwrap());
        assert!(!FlowReplicaState::has_derived(&p, ALICE, "i").unwrap());
        assert!(!FlowReplicaState::is_fired(&p, ALICE, "p1").unwrap());
    }

    /// Deleting a perspective through the DB's own path drops the rows too.
    #[test]
    fn db_remove_perspective_drops_flow_rows() {
        let p = fresh();
        FlowReplicaState::set_current_state(&p, ALICE, "i", "done", &LIVE).unwrap();
        FlowReplicaState::mark_fired(&p, ALICE, "i", &["p1".into()]).unwrap();
        Ad4mDb::with_global_instance(|db| db.remove_perspective(&p)).unwrap();
        assert!(!FlowReplicaState::has_derived(&p, ALICE, "i").unwrap());
        assert!(!FlowReplicaState::is_fired(&p, ALICE, "p1").unwrap());
    }
}
