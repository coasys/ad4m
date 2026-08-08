//! Wire-up sketch for the observability signal-set (spec build-list item 3,
//! `planning/neighbourhood-auto-processing-spec.md` §6).
//!
//! Spike-flavored: [`super::watcher::run_one_pass`] does NOT yet emit
//! [`super::signals::AutoProcessorEvent`]s. Before we touch that function (it
//! lives on real #885's review surface once #883 clears), we lock the emit
//! CONTRACT here: for each of the 5 [`super::watcher::PassOutcome`] variants
//! (plus the anyhow `Err(_)` path) we spell out the exact event sequence a
//! wired-up pass would produce, then assert both sinks (registry snapshot +
//! [`crate::pubsub::AUTO_PROCESSOR_STATE_CHANGE_TOPIC`] subscriber wire shape)
//! observe it correctly.
//!
//! Contract (locked in by these tests):
//!
//! | PassOutcome variant | Emit sequence                                              | reason string          |
//! |---------------------|------------------------------------------------------------|------------------------|
//! | `NotCandidate`      | `Skipped` only (no `Started` — never won a claim)          | `"not_candidate"`      |
//! | `BackedOff`         | `Skipped` only (no `Started` — never won a claim)          | `"backed_off"`         |
//! | `ShapesMissing`     | `Started` → `Skipped` (won claim, then bailed pre-LLM)     | `"shapes_missing"`     |
//! | `EmptyTranscript`   | `Started` → `Skipped` (won claim, then no rows to interpret)| `"empty_transcript"`  |
//! | `Won`               | `Started` → `Finished`                                     | n/a                    |
//! | `Err(_)` post-claim | `Started` → `Error`                                        | n/a (uses `message`)   |
//! | `Err(_)` pre-claim  | `Error` only (no `Started` — never won a claim)            | n/a (uses `message`)   |
//!
//! Rule of thumb: **`Started` fires iff the claim was won.** All terminal
//! events (`Finished` / `Skipped` / `Error`) fire regardless. This lets a UI
//! distinguish:
//!  * pre-claim skips ("someone else has it" — `NotCandidate` / `BackedOff`)
//!  * post-claim skips ("we reserved but had nothing to do" — `ShapesMissing`
//!    / `EmptyTranscript`)
//!
//! from the same pub-sub feed by looking at whether a `Started` preceded the
//! terminal event on the same `(processor_id, partition)` key.
//!
//! Zero touch to `watcher.rs`. Real wire-up is a follow-up PR that inserts
//! `emit(&reg, Started { … }).await` after the `try_claim` success return, and
//! matches on [`super::watcher::PassOutcome`] before the final `Ok(_)` return
//! to emit the correct terminal event. This file is what that PR's tests will
//! look like once `run_one_pass` is actually calling `emit`.

#![cfg(test)]

use super::signals::{emit, AutoProcessorEvent, ProcessorSnapshot, ProcessorState, SignalRegistry};
use crate::pubsub::{get_global_pubsub, AUTO_PROCESSOR_STATE_CHANGE_TOPIC};

/// Test PID unique per test to avoid registry cross-talk under
/// `--test-threads=1`. (Registry is per-test-local, but the global pub-sub
/// topic is process-wide — namespacing by PID keeps subscriber reads
/// deterministic even if a stray event from an adjacent test leaks in.)
fn pid(t: &str) -> String {
    format!("wire-sketch-{t}")
}

/// Sink pair used by every test: local registry + a subscription on the
/// production pub-sub topic. Mirrors what a real subscriber would set up.
struct Sinks {
    reg: SignalRegistry,
    rx: tokio::sync::broadcast::Receiver<String>,
}

impl Sinks {
    async fn open() -> Self {
        let rx = get_global_pubsub()
            .await
            .subscribe(&AUTO_PROCESSOR_STATE_CHANGE_TOPIC)
            .await;
        Sinks {
            reg: SignalRegistry::new(),
            rx,
        }
    }

    /// Consume events off the pub-sub topic until we've collected `n` events
    /// tagged with `processor_id`. Filters out cross-talk (events from
    /// unrelated concurrent tests / earlier residual events) so this test is
    /// order-robust under `--test-threads=1` on a busy topic.
    async fn recv_for_pid(&mut self, processor_id: &str, n: usize) -> Vec<AutoProcessorEvent> {
        let mut out = Vec::with_capacity(n);
        let deadline = std::time::Duration::from_secs(2);
        while out.len() < n {
            let msg = tokio::time::timeout(deadline, self.rx.recv())
                .await
                .expect("pubsub receive timed out")
                .expect("pubsub channel closed");
            let ev: AutoProcessorEvent = serde_json::from_str(&msg)
                .expect("pub-sub payload must be a serialized AutoProcessorEvent");
            if ev.processor_id() == processor_id {
                out.push(ev);
            }
        }
        out
    }

    fn expect_snapshot(&self, processor_id: &str, partition: Option<&str>) -> ProcessorSnapshot {
        self.reg
            .get(processor_id, partition)
            .unwrap_or_else(|| panic!("no snapshot for ({processor_id}, {partition:?})"))
    }
}

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

// ---------------------------------------------------------------------------
// Reason-string wire contract — the four `Skipped` variants map 1:1 to
// PassOutcome variant names in snake_case. UI code filters/renders by these
// exact strings, so we lock them in as consts here (the real wire-up in
// run_one_pass will import these).
// ---------------------------------------------------------------------------

pub const REASON_NOT_CANDIDATE: &str = "not_candidate";
pub const REASON_BACKED_OFF: &str = "backed_off";
pub const REASON_SHAPES_MISSING: &str = "shapes_missing";
pub const REASON_EMPTY_TRANSCRIPT: &str = "empty_transcript";

#[test]
fn reason_strings_match_pass_outcome_variant_names_in_snake_case() {
    // Guards against a rename: PassOutcome variants (CamelCase) must snake-case
    // to these exact reason strings. If someone renames PassOutcome::BackedOff
    // to PassOutcome::PeerBusy, this test still passes (assertions are pure
    // string) — so the *real* guard is grep-ability of these consts from
    // PassOutcome sites in watcher.rs. Documenting the contract in one place.
    assert_eq!(REASON_NOT_CANDIDATE, "not_candidate");
    assert_eq!(REASON_BACKED_OFF, "backed_off");
    assert_eq!(REASON_SHAPES_MISSING, "shapes_missing");
    assert_eq!(REASON_EMPTY_TRANSCRIPT, "empty_transcript");
}

#[tokio::test]
async fn not_candidate_emits_skipped_only_no_started() {
    let mut sinks = Sinks::open().await;
    let p = pid("not-cand");
    emit(&sinks.reg, skipped(&p, None, REASON_NOT_CANDIDATE, 1_000)).await;

    // Wire: exactly one event on the topic, tagged skipped/not_candidate.
    let events = sinks.recv_for_pid(&p, 1).await;
    assert_eq!(events, vec![skipped(&p, None, REASON_NOT_CANDIDATE, 1_000)]);

    // Snapshot: Skipped state, pending_count zero (never observed a Started
    // batch_size), last_skip_reason set, last_output_count stays None (never
    // completed a pass).
    let snap = sinks.expect_snapshot(&p, None);
    assert_eq!(snap.state, ProcessorState::Skipped);
    assert_eq!(snap.pending_count, 0);
    assert_eq!(snap.last_skip_reason.as_deref(), Some(REASON_NOT_CANDIDATE));
    assert_eq!(snap.last_output_count, None);
    assert_eq!(snap.last_event_at, Some(1_000));
}

#[tokio::test]
async fn backed_off_emits_skipped_only_no_started() {
    let mut sinks = Sinks::open().await;
    let p = pid("backed-off");
    emit(&sinks.reg, skipped(&p, None, REASON_BACKED_OFF, 2_000)).await;

    let events = sinks.recv_for_pid(&p, 1).await;
    assert_eq!(events, vec![skipped(&p, None, REASON_BACKED_OFF, 2_000)]);

    let snap = sinks.expect_snapshot(&p, None);
    assert_eq!(snap.state, ProcessorState::Skipped);
    assert_eq!(snap.pending_count, 0);
    assert_eq!(snap.last_skip_reason.as_deref(), Some(REASON_BACKED_OFF));
}

#[tokio::test]
async fn shapes_missing_emits_started_then_skipped_post_claim() {
    let mut sinks = Sinks::open().await;
    let p = pid("shapes-missing");

    // Post-claim skip: Started fires first (we reserved batch of 5), then
    // Skipped with reason "shapes_missing" fires when the SDNA didn't
    // resolve.
    emit(&sinks.reg, started(&p, None, 5, 3_000)).await;
    emit(&sinks.reg, skipped(&p, None, REASON_SHAPES_MISSING, 3_100)).await;

    let events = sinks.recv_for_pid(&p, 2).await;
    assert_eq!(
        events,
        vec![
            started(&p, None, 5, 3_000),
            skipped(&p, None, REASON_SHAPES_MISSING, 3_100),
        ],
        "post-claim skip must fire Started FIRST then Skipped — this lets a UI \
         subscriber tell a shapes-missing skip apart from a pre-claim not-candidate skip"
    );

    let snap = sinks.expect_snapshot(&p, None);
    assert_eq!(snap.state, ProcessorState::Skipped);
    assert_eq!(
        snap.pending_count, 0,
        "terminal event must clear pending_count even though Started set it to 5"
    );
    assert_eq!(
        snap.last_skip_reason.as_deref(),
        Some(REASON_SHAPES_MISSING)
    );
    assert_eq!(snap.last_event_at, Some(3_100));
}

#[tokio::test]
async fn empty_transcript_emits_started_then_skipped_post_claim() {
    let mut sinks = Sinks::open().await;
    let p = pid("empty-transcript");

    emit(&sinks.reg, started(&p, None, 3, 4_000)).await;
    emit(
        &sinks.reg,
        skipped(&p, None, REASON_EMPTY_TRANSCRIPT, 4_050),
    )
    .await;

    let events = sinks.recv_for_pid(&p, 2).await;
    assert_eq!(
        events,
        vec![
            started(&p, None, 3, 4_000),
            skipped(&p, None, REASON_EMPTY_TRANSCRIPT, 4_050),
        ]
    );

    let snap = sinks.expect_snapshot(&p, None);
    assert_eq!(snap.state, ProcessorState::Skipped);
    assert_eq!(snap.pending_count, 0);
    assert_eq!(
        snap.last_skip_reason.as_deref(),
        Some(REASON_EMPTY_TRANSCRIPT)
    );
}

#[tokio::test]
async fn won_emits_started_then_finished_with_output_count() {
    let mut sinks = Sinks::open().await;
    let p = pid("won");

    emit(&sinks.reg, started(&p, None, 7, 5_000)).await;
    // output_count = bases.len() from PassOutcome::Won { bases }
    emit(&sinks.reg, finished(&p, None, 4, 5_800)).await;

    let events = sinks.recv_for_pid(&p, 2).await;
    assert_eq!(
        events,
        vec![started(&p, None, 7, 5_000), finished(&p, None, 4, 5_800),]
    );

    let snap = sinks.expect_snapshot(&p, None);
    assert_eq!(snap.state, ProcessorState::Finished);
    assert_eq!(snap.pending_count, 0);
    assert_eq!(
        snap.last_output_count,
        Some(4),
        "output_count must reflect bases.len() from PassOutcome::Won"
    );
    assert_eq!(snap.last_finished_at, Some(5_800));
    assert_eq!(snap.last_event_at, Some(5_800));
}

#[tokio::test]
async fn post_claim_error_emits_started_then_error() {
    let mut sinks = Sinks::open().await;
    let p = pid("post-claim-err");

    // Mid-pass failure (e.g. run_interpretation_with_strategy_and_model errored
    // after we won the claim + loaded shapes): Started already fired, so the
    // terminal Error event completes the sequence.
    emit(&sinks.reg, started(&p, None, 2, 6_000)).await;
    emit(&sinks.reg, error(&p, None, "llm: timeout after 30s", 6_500)).await;

    let events = sinks.recv_for_pid(&p, 2).await;
    assert_eq!(
        events,
        vec![
            started(&p, None, 2, 6_000),
            error(&p, None, "llm: timeout after 30s", 6_500),
        ]
    );

    let snap = sinks.expect_snapshot(&p, None);
    assert_eq!(snap.state, ProcessorState::Errored);
    assert_eq!(snap.pending_count, 0);
    assert_eq!(snap.last_error.as_deref(), Some("llm: timeout after 30s"));
}

#[tokio::test]
async fn pre_claim_error_emits_error_only_no_started() {
    let mut sinks = Sinks::open().await;
    let p = pid("pre-claim-err");

    // try_claim itself errored (rare: SPARQL store hiccup, corrupt claim
    // link). No Started was emitted because we never crossed the claim-win
    // boundary. UI sees a bare Error and knows this was a pre-work failure.
    emit(
        &sinks.reg,
        error(&p, None, "try_claim: sparql store unavailable", 7_000),
    )
    .await;

    let events = sinks.recv_for_pid(&p, 1).await;
    assert_eq!(
        events,
        vec![error(
            &p,
            None,
            "try_claim: sparql store unavailable",
            7_000
        )]
    );

    let snap = sinks.expect_snapshot(&p, None);
    assert_eq!(snap.state, ProcessorState::Errored);
    assert_eq!(snap.pending_count, 0);
    assert_eq!(
        snap.last_error.as_deref(),
        Some("try_claim: sparql store unavailable")
    );
}

#[tokio::test]
async fn partitioned_pass_carries_partition_through_full_sequence() {
    // Wildcard/partitioned processor (spec Q2 + build-list item 1): each
    // `(processor_id, partition)` key is its own event stream and its own
    // registry snapshot. This test drives two partitions in parallel through
    // Won and Skipped respectively; assert both snapshots are correct and
    // disjoint, and both event sequences reach a subscriber tagged by
    // partition.
    let mut sinks = Sinks::open().await;
    let p = pid("wildcard");
    let payments = Some("soa://subgroup/payments");
    let onboarding = Some("soa://subgroup/onboarding");

    emit(&sinks.reg, started(&p, payments, 4, 8_000)).await;
    emit(&sinks.reg, started(&p, onboarding, 6, 8_010)).await;
    emit(&sinks.reg, finished(&p, payments, 3, 8_500)).await;
    emit(
        &sinks.reg,
        skipped(&p, onboarding, REASON_EMPTY_TRANSCRIPT, 8_600),
    )
    .await;

    let events = sinks.recv_for_pid(&p, 4).await;
    assert_eq!(events.len(), 4);
    // All 4 events carry a partition (never None on the wildcard path).
    for ev in &events {
        assert!(
            ev.partition().is_some(),
            "wildcard pass must carry Some(partition) on every event; got {ev:?}"
        );
    }

    let pay_snap = sinks.expect_snapshot(&p, payments);
    assert_eq!(pay_snap.state, ProcessorState::Finished);
    assert_eq!(pay_snap.last_output_count, Some(3));

    let onb_snap = sinks.expect_snapshot(&p, onboarding);
    assert_eq!(onb_snap.state, ProcessorState::Skipped);
    assert_eq!(
        onb_snap.last_skip_reason.as_deref(),
        Some(REASON_EMPTY_TRANSCRIPT)
    );
    assert_eq!(
        onb_snap.last_output_count, None,
        "onboarding partition never completed a pass; last_output_count stays None"
    );
}

#[tokio::test]
async fn snapshot_all_returns_both_partitions_in_deterministic_order() {
    // A UI that opens the executor connection and calls snapshot_all() to
    // fill its initial table needs deterministic ordering (BTreeMap by
    // `(processor_id, partition)`) — this test drives that path.
    let mut sinks = Sinks::open().await;
    let p = pid("snapshot-order");

    emit(&sinks.reg, started(&p, Some("z-partition"), 1, 9_000)).await;
    emit(&sinks.reg, started(&p, Some("a-partition"), 1, 9_001)).await;
    emit(&sinks.reg, started(&p, None, 1, 9_002)).await;

    // Drain topic (order-robust filter does the work); we don't assert on
    // topic order here since a real subscriber would consume incrementally.
    let _ = sinks.recv_for_pid(&p, 3).await;

    let all = sinks.reg.snapshot_all();
    let keys: Vec<(String, Option<String>)> = all
        .into_iter()
        .filter(|s| s.processor_id == p)
        .map(|s| (s.processor_id, s.partition))
        .collect();
    assert_eq!(
        keys,
        vec![
            (p.clone(), None),
            (p.clone(), Some("a-partition".to_string())),
            (p.clone(), Some("z-partition".to_string())),
        ],
        "snapshot_all must return (pid, partition) keys in BTreeMap order — \
         None first, then partitions lexicographically — so a UI table renders \
         stably across reloads"
    );
}
