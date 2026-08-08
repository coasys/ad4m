//! Observability signal-set for [`super::watcher::run_one_pass`] — an event
//! stream + a queryable snapshot registry.
//!
//! Two sinks, deliberately separated so callers can use either alone:
//! * [`publish_event`] pushes a JSON-serialized [`AutoProcessorEvent`] onto
//!   the global pub-sub topic [`crate::pubsub::AUTO_PROCESSOR_STATE_CHANGE_TOPIC`]
//!   (analogous to `PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC`), for streaming
//!   consumers.
//! * [`SignalRegistry::record`] folds an event into a compact per-
//!   `(processor_id, partition)` [`ProcessorSnapshot`] a UI can render
//!   without replaying the stream since executor boot.
//!
//! [`emit`] convenience-combines both.
//!
//! Wildcard/partitioned processors (spec §Q2) key events by
//! `(processor_id, partition)`; single-partition processors pass
//! `partition = None`. The pair is the natural aggregation grain so per-
//! partition dashboards, filters, and per-partition-claim reasoning (spec
//! build-list item 2) can consume the same feed.
//!
//! This module is intentionally decoupled from
//! [`super::watcher::run_one_pass`]: wiring the emit calls into the exit
//! paths is a separate step so that follow-up lands without touching the
//! signal shape itself.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::sync::Mutex;

use crate::pubsub::{get_global_pubsub, AUTO_PROCESSOR_STATE_CHANGE_TOPIC};

/// Unix wall-clock in milliseconds. Callers should pass the same `now_ms`
/// they already thread through [`super::watcher::run_one_pass`] so the
/// signal timeline and the pass timeline line up.
pub type UnixMillis = i64;

/// Rolled-up terminal state of the last observed pass for a given
/// `(processor_id, partition)` key.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ProcessorState {
    /// Never observed (no event recorded yet).
    Idle,
    /// [`AutoProcessorEvent::Started`] recorded with no matching terminal
    /// event yet.
    Running,
    /// Last event was [`AutoProcessorEvent::Finished`].
    Finished,
    /// Last event was [`AutoProcessorEvent::Skipped`].
    Skipped,
    /// Last event was [`AutoProcessorEvent::Error`].
    Errored,
}

impl Default for ProcessorState {
    fn default() -> Self {
        ProcessorState::Idle
    }
}

/// One point-in-time event about a pass. Serialized to JSON on
/// [`AUTO_PROCESSOR_STATE_CHANGE_TOPIC`].
///
/// Variants mirror the exit paths of [`super::watcher::run_one_pass`]:
/// * `Started` on pass entry (after telepresence + claim win, before the
///   LLM round-trip).
/// * `Finished` on the [`super::watcher::PassOutcome::Won`] path.
/// * `Skipped` on `BackedOff`, `NotCandidate`, `ShapesMissing`, or
///   `EmptyTranscript` — anything that legitimately declined to do the LLM
///   round-trip. `reason` carries a short discriminant so a UI can distinguish
///   "someone else has it" from "no data yet".
/// * `Error` on any `Err(_)` return.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum AutoProcessorEvent {
    Started {
        processor_id: String,
        partition: Option<String>,
        batch_size: usize,
        at_ms: UnixMillis,
    },
    Finished {
        processor_id: String,
        partition: Option<String>,
        output_count: usize,
        at_ms: UnixMillis,
    },
    Skipped {
        processor_id: String,
        partition: Option<String>,
        reason: String,
        at_ms: UnixMillis,
    },
    Error {
        processor_id: String,
        partition: Option<String>,
        message: String,
        at_ms: UnixMillis,
    },
}

impl AutoProcessorEvent {
    pub fn processor_id(&self) -> &str {
        match self {
            Self::Started { processor_id, .. }
            | Self::Finished { processor_id, .. }
            | Self::Skipped { processor_id, .. }
            | Self::Error { processor_id, .. } => processor_id,
        }
    }

    pub fn partition(&self) -> Option<&str> {
        match self {
            Self::Started { partition, .. }
            | Self::Finished { partition, .. }
            | Self::Skipped { partition, .. }
            | Self::Error { partition, .. } => partition.as_deref(),
        }
    }

    pub fn at_ms(&self) -> UnixMillis {
        match self {
            Self::Started { at_ms, .. }
            | Self::Finished { at_ms, .. }
            | Self::Skipped { at_ms, .. }
            | Self::Error { at_ms, .. } => *at_ms,
        }
    }
}

/// Snapshot of the last-known state per `(processor_id, partition)`. Cheap to
/// serialize; a UI subscribes to the stream for real-time updates and can
/// call [`SignalRegistry::snapshot_all`] once on load to fill its table.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct ProcessorSnapshot {
    pub processor_id: String,
    pub partition: Option<String>,
    pub state: ProcessorState,
    /// `batch_size` from the last observed `Started`. Cleared to 0 by any
    /// terminal event.
    pub pending_count: usize,
    /// `output_count` from the last observed `Finished`; retained across
    /// later `Started` events so a UI can show "23 last run".
    pub last_output_count: Option<usize>,
    /// `at_ms` of the last observed `Finished`; retained across later events.
    pub last_finished_at: Option<UnixMillis>,
    /// `reason` from the most recent `Skipped`.
    pub last_skip_reason: Option<String>,
    /// `message` from the most recent `Error`.
    pub last_error: Option<String>,
    /// `at_ms` of the most recent event of any type.
    pub last_event_at: Option<UnixMillis>,
}

/// Fold [`AutoProcessorEvent`]s into per-`(processor_id, partition)`
/// snapshots. `BTreeMap` (not `HashMap`) so [`SignalRegistry::snapshot_all`]
/// returns entries in a deterministic order — useful for UI table stability
/// and test assertions.
#[derive(Debug, Default)]
pub struct SignalRegistry {
    snapshots: Mutex<BTreeMap<(String, Option<String>), ProcessorSnapshot>>,
}

impl SignalRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Fold `event` into the snapshot for its `(processor_id, partition)`
    /// key. Creates the entry on first observation.
    pub fn record(&self, event: &AutoProcessorEvent) {
        let key = (
            event.processor_id().to_string(),
            event.partition().map(String::from),
        );
        let mut map = self.snapshots.lock().unwrap();
        let entry = map.entry(key.clone()).or_insert_with(|| ProcessorSnapshot {
            processor_id: key.0.clone(),
            partition: key.1.clone(),
            ..Default::default()
        });
        entry.last_event_at = Some(event.at_ms());
        match event {
            AutoProcessorEvent::Started { batch_size, .. } => {
                entry.state = ProcessorState::Running;
                entry.pending_count = *batch_size;
            }
            AutoProcessorEvent::Finished {
                output_count,
                at_ms,
                ..
            } => {
                entry.state = ProcessorState::Finished;
                entry.pending_count = 0;
                entry.last_output_count = Some(*output_count);
                entry.last_finished_at = Some(*at_ms);
            }
            AutoProcessorEvent::Skipped { reason, .. } => {
                entry.state = ProcessorState::Skipped;
                entry.pending_count = 0;
                entry.last_skip_reason = Some(reason.clone());
            }
            AutoProcessorEvent::Error { message, .. } => {
                entry.state = ProcessorState::Errored;
                entry.pending_count = 0;
                entry.last_error = Some(message.clone());
            }
        }
    }

    /// Snapshot for one `(processor_id, partition)` key, if any events have
    /// been recorded for it.
    pub fn get(&self, processor_id: &str, partition: Option<&str>) -> Option<ProcessorSnapshot> {
        let map = self.snapshots.lock().unwrap();
        map.get(&(processor_id.to_string(), partition.map(String::from)))
            .cloned()
    }

    /// All snapshots in deterministic `(processor_id, partition)` order.
    pub fn snapshot_all(&self) -> Vec<ProcessorSnapshot> {
        let map = self.snapshots.lock().unwrap();
        map.values().cloned().collect()
    }

    /// Drop every recorded snapshot. Intended for test setup only; production
    /// code should treat the registry as an append-only view.
    pub fn clear(&self) {
        self.snapshots.lock().unwrap().clear();
    }
}

/// Publish `event` on [`AUTO_PROCESSOR_STATE_CHANGE_TOPIC`]. Serialization
/// errors are logged and swallowed: this is an observability side-channel,
/// not a correctness path — a dropped signal must never fail a pass.
pub async fn publish_event(event: &AutoProcessorEvent) {
    match serde_json::to_string(event) {
        Ok(payload) => {
            get_global_pubsub()
                .await
                .publish(&AUTO_PROCESSOR_STATE_CHANGE_TOPIC, &payload)
                .await;
        }
        Err(err) => {
            log::warn!("auto_processor::signals::publish_event: serialize failed: {err:#}");
        }
    }
}

/// Convenience: record on `registry` and publish on the pub-sub topic in one
/// call. Callers that only need one sink can use [`SignalRegistry::record`]
/// or [`publish_event`] directly.
pub async fn emit(registry: &SignalRegistry, event: AutoProcessorEvent) {
    registry.record(&event);
    publish_event(&event).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    fn started(pid: &str, partition: Option<&str>, batch: usize, at: i64) -> AutoProcessorEvent {
        AutoProcessorEvent::Started {
            processor_id: pid.to_string(),
            partition: partition.map(String::from),
            batch_size: batch,
            at_ms: at,
        }
    }

    fn finished(pid: &str, partition: Option<&str>, out: usize, at: i64) -> AutoProcessorEvent {
        AutoProcessorEvent::Finished {
            processor_id: pid.to_string(),
            partition: partition.map(String::from),
            output_count: out,
            at_ms: at,
        }
    }

    fn skipped(pid: &str, partition: Option<&str>, reason: &str, at: i64) -> AutoProcessorEvent {
        AutoProcessorEvent::Skipped {
            processor_id: pid.to_string(),
            partition: partition.map(String::from),
            reason: reason.to_string(),
            at_ms: at,
        }
    }

    fn error(pid: &str, partition: Option<&str>, msg: &str, at: i64) -> AutoProcessorEvent {
        AutoProcessorEvent::Error {
            processor_id: pid.to_string(),
            partition: partition.map(String::from),
            message: msg.to_string(),
            at_ms: at,
        }
    }

    #[test]
    fn started_sets_running_and_pending_count() {
        let reg = SignalRegistry::new();
        reg.record(&started("proc-a", None, 7, 1000));
        let snap = reg.get("proc-a", None).unwrap();
        assert_eq!(snap.state, ProcessorState::Running);
        assert_eq!(snap.pending_count, 7);
        assert_eq!(snap.last_event_at, Some(1000));
        assert_eq!(snap.last_output_count, None);
        assert_eq!(snap.last_finished_at, None);
    }

    #[test]
    fn finished_after_started_clears_pending_and_records_output() {
        let reg = SignalRegistry::new();
        reg.record(&started("proc-a", None, 3, 1000));
        reg.record(&finished("proc-a", None, 3, 1500));
        let snap = reg.get("proc-a", None).unwrap();
        assert_eq!(snap.state, ProcessorState::Finished);
        assert_eq!(snap.pending_count, 0);
        assert_eq!(snap.last_output_count, Some(3));
        assert_eq!(snap.last_finished_at, Some(1500));
        assert_eq!(snap.last_event_at, Some(1500));
    }

    #[test]
    fn skipped_carries_reason_and_clears_pending() {
        let reg = SignalRegistry::new();
        reg.record(&started("proc-a", None, 4, 1000));
        reg.record(&skipped("proc-a", None, "backed_off", 1200));
        let snap = reg.get("proc-a", None).unwrap();
        assert_eq!(snap.state, ProcessorState::Skipped);
        assert_eq!(snap.pending_count, 0);
        assert_eq!(snap.last_skip_reason.as_deref(), Some("backed_off"));
        assert_eq!(snap.last_event_at, Some(1200));
        // A prior successful run's output count must persist across a later Skip
        // — otherwise a single "empty scope" skip would blank the UI's rolling
        // "23 last run" indicator.
        assert_eq!(snap.last_output_count, None);
    }

    #[test]
    fn error_carries_message_and_clears_pending() {
        let reg = SignalRegistry::new();
        reg.record(&started("proc-a", None, 4, 1000));
        reg.record(&error("proc-a", None, "sparql: parse error", 1300));
        let snap = reg.get("proc-a", None).unwrap();
        assert_eq!(snap.state, ProcessorState::Errored);
        assert_eq!(snap.pending_count, 0);
        assert_eq!(snap.last_error.as_deref(), Some("sparql: parse error"));
        assert_eq!(snap.last_event_at, Some(1300));
    }

    #[test]
    fn finish_output_count_persists_across_later_skip() {
        let reg = SignalRegistry::new();
        reg.record(&started("proc-a", None, 5, 1000));
        reg.record(&finished("proc-a", None, 5, 1500));
        reg.record(&skipped("proc-a", None, "empty_transcript", 2000));
        let snap = reg.get("proc-a", None).unwrap();
        assert_eq!(snap.state, ProcessorState::Skipped);
        // Rolling "last successful output" must survive an unrelated later Skip;
        // otherwise a UI would flip from "23 last run" to blank on the next
        // no-op cycle.
        assert_eq!(snap.last_output_count, Some(5));
        assert_eq!(snap.last_finished_at, Some(1500));
        assert_eq!(snap.last_skip_reason.as_deref(), Some("empty_transcript"));
        assert_eq!(snap.last_event_at, Some(2000));
    }

    #[test]
    fn separate_partitions_get_separate_snapshots() {
        let reg = SignalRegistry::new();
        reg.record(&started("proc-a", Some("soa://payments"), 2, 1000));
        reg.record(&started("proc-a", Some("soa://onboarding"), 5, 1010));
        reg.record(&finished("proc-a", Some("soa://payments"), 2, 1200));

        let payments = reg.get("proc-a", Some("soa://payments")).unwrap();
        let onboarding = reg.get("proc-a", Some("soa://onboarding")).unwrap();
        assert_eq!(payments.state, ProcessorState::Finished);
        assert_eq!(payments.last_output_count, Some(2));
        assert_eq!(onboarding.state, ProcessorState::Running);
        assert_eq!(onboarding.pending_count, 5);
        // Distinct keys are truly disjoint — a Finished on one partition must not
        // mutate another partition's rolling snapshot.
        assert_eq!(onboarding.last_output_count, None);
    }

    #[test]
    fn partition_none_and_some_are_distinct_keys() {
        let reg = SignalRegistry::new();
        reg.record(&started("proc-a", None, 3, 1000));
        reg.record(&started("proc-a", Some("part-1"), 9, 1010));
        assert_eq!(reg.get("proc-a", None).unwrap().pending_count, 3);
        assert_eq!(reg.get("proc-a", Some("part-1")).unwrap().pending_count, 9);
    }

    #[test]
    fn snapshot_all_returns_deterministic_order() {
        let reg = SignalRegistry::new();
        // Insert in scrambled order to prove BTreeMap ordering, not insertion.
        reg.record(&started("proc-b", None, 1, 1000));
        reg.record(&started("proc-a", Some("z"), 1, 1000));
        reg.record(&started("proc-a", None, 1, 1000));
        reg.record(&started("proc-a", Some("a"), 1, 1000));
        let all = reg.snapshot_all();
        let keys: Vec<(String, Option<String>)> = all
            .into_iter()
            .map(|s| (s.processor_id, s.partition))
            .collect();
        assert_eq!(
            keys,
            vec![
                ("proc-a".to_string(), None),
                ("proc-a".to_string(), Some("a".to_string())),
                ("proc-a".to_string(), Some("z".to_string())),
                ("proc-b".to_string(), None),
            ]
        );
    }

    #[test]
    fn event_helpers_return_expected_fields() {
        let ev = started("proc-a", Some("p"), 3, 42);
        assert_eq!(ev.processor_id(), "proc-a");
        assert_eq!(ev.partition(), Some("p"));
        assert_eq!(ev.at_ms(), 42);

        let ev = finished("proc-a", None, 5, 43);
        assert_eq!(ev.partition(), None);
        assert_eq!(ev.at_ms(), 43);
    }

    #[test]
    fn event_serialization_roundtrip() {
        // JSON shape is the wire contract: consumers on the pub-sub topic
        // deserialize by these exact tag values and field names.
        for original in [
            started("proc-a", None, 3, 100),
            started("proc-a", Some("p1"), 5, 101),
            finished("proc-a", Some("p1"), 5, 102),
            skipped("proc-a", None, "backed_off", 103),
            error("proc-a", Some("p1"), "boom", 104),
        ] {
            let json = serde_json::to_string(&original).unwrap();
            let parsed: AutoProcessorEvent = serde_json::from_str(&json).unwrap();
            assert_eq!(parsed, original);
        }
    }

    #[test]
    fn event_json_tag_is_snake_case_type_field() {
        // Lock down the wire shape so accidental #[serde(tag)] renames or
        // enum-name changes are caught at unit-test time, not on a UI
        // consumer crashing in prod.
        let json = serde_json::to_string(&started("proc-a", None, 1, 1)).unwrap();
        assert!(
            json.contains("\"type\":\"started\""),
            "expected snake_case 'started' tag, got {json}"
        );
        let json = serde_json::to_string(&finished("proc-a", None, 1, 1)).unwrap();
        assert!(json.contains("\"type\":\"finished\""), "got {json}");
        let json = serde_json::to_string(&skipped("proc-a", None, "x", 1)).unwrap();
        assert!(json.contains("\"type\":\"skipped\""), "got {json}");
        let json = serde_json::to_string(&error("proc-a", None, "x", 1)).unwrap();
        assert!(json.contains("\"type\":\"error\""), "got {json}");
    }

    #[tokio::test]
    async fn emit_publishes_to_pubsub_topic_and_records_in_registry() {
        // End-to-end: subscribe on the same topic emit() targets, fire the
        // event, assert both sinks observed it. Uses the process-global
        // pubsub singleton (same instance as production callers).
        let reg = SignalRegistry::new();
        let mut rx = get_global_pubsub()
            .await
            .subscribe(&AUTO_PROCESSOR_STATE_CHANGE_TOPIC)
            .await;
        let event = finished("proc-emit-test", Some("part-x"), 7, 999);
        emit(&reg, event.clone()).await;

        let msg = tokio::time::timeout(std::time::Duration::from_secs(1), rx.recv())
            .await
            .expect("pubsub receive timed out")
            .expect("pubsub channel closed");
        let parsed: AutoProcessorEvent = serde_json::from_str(&msg).unwrap();
        assert_eq!(parsed, event);

        let snap = reg.get("proc-emit-test", Some("part-x")).unwrap();
        assert_eq!(snap.state, ProcessorState::Finished);
        assert_eq!(snap.last_output_count, Some(7));
    }
}
