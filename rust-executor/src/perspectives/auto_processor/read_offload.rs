//! Move one auto-processor tick's reads off the Tokio worker.
//!
//! Config, cursor and transcript acquisition, and the filter that follows
//! them, run on a blocking-pool thread. That thread drives the async reads
//! with [`Handle::block_on`] and holds one permit of [`READ_SLOTS`] until it
//! returns. Claim waits, events, the model check and writes stay on the
//! calling task. Dropping the tick cancels the parent token; each stage's
//! gate watches that token, so the store read ends too. A stage's deadline
//! starts when that stage opens, after a read slot is held. Queue time and
//! earlier stages do not spend it.
//!
//! The cap is two prepares at once, and never more than
//! [`std::thread::available_parallelism`]. Two is a conservative starting
//! cap, not a measured optimum.

use std::sync::Arc;
use std::time::Duration;

use tokio::sync::Semaphore;
use tokio_util::sync::CancellationToken;

use crate::perspectives::interpretation::gather_transcript_sparql_stopped;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::sparql_store::{BoundStop, EvalGate, EvalStop, InlineQuery};

use super::config::load_processors_stopped;
use super::cursor::{
    load_processed_sources_stopped, processed_ids_in_window, turn_in_source_window,
};
use super::delta_read::{delta_query, DeltaRead};
use super::watcher::{CachedTurn, PendingTurn, TickOutcome, WatcherState};

/// The pre-fix path. Committed `false`: prepare runs on a blocking worker.
/// A red run of the offload tests sets this to `true` and does not commit
/// that. `true` runs the reads on the calling Tokio worker, under a stop
/// that never fires.
fn reads_on_runtime() -> bool {
    false
}

/// How many prepares may read at once. See the module docs.
fn read_concurrency() -> usize {
    std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1)
        .min(2)
}

static READ_SLOT_COUNT: std::sync::LazyLock<usize> = std::sync::LazyLock::new(read_concurrency);

static READ_SLOTS: std::sync::LazyLock<Arc<Semaphore>> =
    std::sync::LazyLock::new(|| Arc::new(Semaphore::new(*READ_SLOT_COUNT)));

#[cfg(test)]
pub(crate) fn read_slot_count() -> usize {
    *READ_SLOT_COUNT
}

#[cfg(test)]
pub(crate) fn available_permits() -> usize {
    READ_SLOTS.available_permits()
}

/// The deadline for one prepare stage. Tests that have not set a global
/// config still get the executor default of 30s. A per-stage test override
/// replaces this for that stage only. The budget starts when the stage
/// opens, after a read slot is held.
fn auto_processor_query_deadline() -> Duration {
    let guard = crate::config::GLOBAL_AD4M_CONFIG
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let secs = guard
        .as_ref()
        .and_then(|cfg| cfg.query_deadline_secs)
        .filter(|secs| *secs > 0)
        .unwrap_or(30);
    Duration::from_secs(secs)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Stage {
    Config,
    Cursor,
    Source,
}

fn stage_deadline(stage: Stage) -> Duration {
    #[cfg(test)]
    if let Some(deadline) = probe::deadline_for(stage) {
        return deadline;
    }
    let _ = stage;
    auto_processor_query_deadline()
}

/// One stage's gate. The bound stop drops before the gate, so a stage ending
/// does not look stopped to the next stage.
struct StageOpen {
    bound: Option<BoundStop>,
    gate: Option<EvalGate>,
    stop: EvalStop,
}

impl StageOpen {
    fn open(parent: &CancellationToken, stage: Stage) -> Option<Self> {
        if reads_on_runtime() {
            let stop = EvalStop::none();
            let bound = BoundStop::bind(&stop);
            return Some(Self {
                bound: Some(bound),
                gate: None,
                stop,
            });
        }
        if parent.is_cancelled() {
            return None;
        }
        let gate = EvalGate::request(stage_deadline(stage), Some(parent.clone()));
        let stop = gate.stop();
        let bound = BoundStop::bind(&stop);
        Some(Self {
            bound: Some(bound),
            gate: Some(gate),
            stop,
        })
    }

    fn stop(&self) -> &EvalStop {
        &self.stop
    }
}

impl Drop for StageOpen {
    fn drop(&mut self) {
        // Unbind first. The gate's drop then cancels its token.
        drop(self.bound.take());
        drop(self.gate.take());
    }
}

pub(crate) enum PrepareOutcome {
    /// The tick ends here. Do not drain.
    Finished(TickOutcome),
    /// Reads are in `watcher`. The caller checks the model and drains.
    Drain(Arc<Vec<super::config::AutoProcessorConfig>>),
}

struct CancelOnDrop(CancellationToken);

impl Drop for CancelOnDrop {
    fn drop(&mut self) {
        self.0.cancel();
    }
}

/// The watcher for one prepare. The job writes it back into this lease, even
/// after the awaiting future has been dropped. [`prepare_reads`] copies it
/// into the caller's `&mut` when that future completes or is dropped before
/// the job has taken it.
#[derive(Clone)]
pub(crate) struct PrepareLease {
    slot: Arc<std::sync::Mutex<Option<WatcherState>>>,
}

impl PrepareLease {
    pub(crate) fn from_watcher(watcher: WatcherState) -> Self {
        Self {
            slot: Arc::new(std::sync::Mutex::new(Some(watcher))),
        }
    }

    pub(crate) fn clone_watcher(&self) -> Option<WatcherState> {
        self.slot.lock().unwrap_or_else(|e| e.into_inner()).clone()
    }

    fn take(&self) -> Option<WatcherState> {
        self.slot.lock().unwrap_or_else(|e| e.into_inner()).take()
    }
}

struct Restore<'a> {
    lease: PrepareLease,
    watcher: &'a mut WatcherState,
}

impl Drop for Restore<'_> {
    fn drop(&mut self) {
        if let Some(watcher) = self.lease.take() {
            *self.watcher = watcher;
        }
    }
}

/// Run the read half of one tick. The `&mut` is restored when this future
/// completes, and when it is dropped before the job takes the watcher. A
/// caller that drops the future and keeps the watcher uses [`PrepareLease`].
pub(crate) async fn prepare_reads(
    perspective: &PerspectiveInstance,
    watcher: &mut WatcherState,
    now_ms: i64,
) -> PrepareOutcome {
    if reads_on_runtime() {
        let parent = CancellationToken::new();
        return prepare_inner(perspective, watcher, now_ms, &parent).await;
    }

    let lease = PrepareLease::from_watcher(std::mem::take(watcher));
    let _restore = Restore {
        lease: lease.clone(),
        watcher,
    };
    prepare_leased(perspective, &lease, now_ms).await
}

/// [`prepare_reads`] for a caller that owns the lease past this future.
pub(crate) async fn prepare_leased(
    perspective: &PerspectiveInstance,
    lease: &PrepareLease,
    now_ms: i64,
) -> PrepareOutcome {
    if reads_on_runtime() {
        let parent = CancellationToken::new();
        let mut watcher = match lease.take() {
            Some(watcher) => watcher,
            None => return PrepareOutcome::Finished(TickOutcome::Pending),
        };
        let outcome = prepare_inner(perspective, &mut watcher, now_ms, &parent).await;
        *lease.slot.lock().unwrap_or_else(|e| e.into_inner()) = Some(watcher);
        return outcome;
    }

    let parent = CancellationToken::new();
    let _cancel = CancelOnDrop(parent.clone());
    // The permit comes before any gate. A tick queued behind a full cap must
    // not arm a watchdog, and must not spend its deadline while it waits.
    let permit = READ_SLOTS
        .clone()
        .acquire_owned()
        .await
        .expect("the auto-processor read semaphore is never closed");
    let runtime = tokio::runtime::Handle::current();
    let perspective = perspective.clone();
    let parent_for_job = parent.clone();
    let slot_for_job = lease.slot.clone();

    let join = tokio::task::spawn_blocking(move || {
        let _permit = permit;
        let _inline = InlineQuery::enter();
        let mut watcher = match slot_for_job
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .take()
        {
            Some(watcher) => watcher,
            None => return PrepareOutcome::Finished(TickOutcome::Pending),
        };
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            runtime.block_on(prepare_inner(
                &perspective,
                &mut watcher,
                now_ms,
                &parent_for_job,
            ))
        }));
        // The lease the caller still holds, not a slot private to this job.
        *slot_for_job.lock().unwrap_or_else(|e| e.into_inner()) = Some(watcher);
        match result {
            Ok(outcome) => outcome,
            Err(payload) => {
                log::error!(
                    "auto_processor [{}]: read prepare panicked: {}",
                    perspective.uuid,
                    panic_message(&payload)
                );
                PrepareOutcome::Finished(TickOutcome::Pending)
            }
        }
    });

    match join.await {
        Ok(outcome) => outcome,
        Err(err) => {
            log::error!("auto_processor read prepare task failed: {err}");
            PrepareOutcome::Finished(TickOutcome::Pending)
        }
    }
}

fn panic_message(payload: &(dyn std::any::Any + Send)) -> String {
    if let Some(message) = payload.downcast_ref::<&str>() {
        (*message).to_string()
    } else if let Some(message) = payload.downcast_ref::<String>() {
        message.clone()
    } else {
        "auto-processor prepare panicked".to_string()
    }
}

fn job_cancelled(parent: &CancellationToken) -> bool {
    parent.is_cancelled()
}

async fn prepare_inner(
    perspective: &PerspectiveInstance,
    watcher: &mut WatcherState,
    now_ms: i64,
    parent: &CancellationToken,
) -> PrepareOutcome {
    let uuid = perspective.uuid.clone();
    if job_cancelled(parent) {
        return PrepareOutcome::Finished(TickOutcome::Pending);
    }
    // Read before any query, so a write during one makes the next tick read again.
    let generation = perspective.sparql_store.write_generation();

    let configs = match watcher.reads.configs(generation) {
        Some(configs) => configs,
        None => {
            let Some(stage) = StageOpen::open(parent, Stage::Config) else {
                return PrepareOutcome::Finished(TickOutcome::Pending);
            };
            match load_processors_stopped(perspective, stage.stop()).await {
                Ok(configs) => watcher.reads.set_configs(generation, configs),
                Err(err) => {
                    log::warn!("auto_processor_tick [{uuid}]: load_processors failed: {err:#}");
                    if job_cancelled(parent) {
                        return PrepareOutcome::Finished(TickOutcome::Pending);
                    }
                    return PrepareOutcome::Finished(if watcher.has_pending() {
                        TickOutcome::Pending
                    } else {
                        TickOutcome::Idle
                    });
                }
            }
        }
    };
    if configs.is_empty() {
        return PrepareOutcome::Finished(TickOutcome::Idle);
    }

    for cfg in configs.iter() {
        if job_cancelled(parent) {
            return PrepareOutcome::Finished(TickOutcome::Pending);
        }
        let processed_rows = match watcher.reads.processed(&cfg.processor_id, generation) {
            Some(rows) => rows,
            None => {
                let Some(stage) = StageOpen::open(parent, Stage::Cursor) else {
                    return PrepareOutcome::Finished(TickOutcome::Pending);
                };
                match load_processed_sources_stopped(perspective, &cfg.processor_id, stage.stop())
                    .await
                {
                    Ok(rows) => watcher
                        .reads
                        .set_processed(&cfg.processor_id, generation, rows),
                    Err(err) => {
                        log::warn!(
                            "auto_processor `{}` [{uuid}]: load_processed_source_ids failed: {err:#}",
                            cfg.processor_id
                        );
                        if job_cancelled(parent) {
                            return PrepareOutcome::Finished(TickOutcome::Pending);
                        }
                        // A cursor failure skips this config before `retain_pending`.
                        continue;
                    }
                }
            }
        };
        let processed = processed_ids_in_window(&processed_rows, now_ms, cfg.source_window_ms);
        let queueable = |id: &str, timestamp: &str| {
            cfg.source_window_ms
                .is_none_or(|window| turn_in_source_window(timestamp, now_ms, window))
                && !processed.contains(id)
        };
        watcher.retain_pending(&cfg.processor_id, |turn| {
            queueable(&turn.id, &turn.timestamp)
        });
        if job_cancelled(parent) {
            return PrepareOutcome::Finished(TickOutcome::Pending);
        }

        let kept = watcher.reads.transcript(cfg).filter(|(_, turns)| {
            !turns.iter().any(|turn| {
                matches!(turn, CachedTurn::Consumed { .. })
                    && queueable(turn.id(), turn.timestamp())
            })
        });
        let delta = match &kept {
            Some((kept_at, _)) if *kept_at == generation => None,
            Some((kept_at, _)) => perspective
                .sparql_store
                .additions_since(*kept_at, generation)
                .map(|added| delta_query(&cfg.source_scope_query, &added))
                .filter(|delta| *delta != DeltaRead::Full),
            None => None,
        };

        // The record loop stays under this gate on a cache hit, so a long
        // record can still be cancelled.
        let Some(stage) = StageOpen::open(parent, Stage::Source) else {
            return PrepareOutcome::Finished(TickOutcome::Pending);
        };
        let transcript = match (kept, delta) {
            (Some((kept_at, turns)), _) if kept_at == generation => turns,
            (kept, Some(delta)) => {
                drop(kept);
                let gained = match delta {
                    DeltaRead::Query(query) => {
                        gather_transcript_sparql_stopped(perspective, &query, stage.stop()).await
                    }
                    _ => Ok(Vec::new()),
                };
                let gained = match gained {
                    Ok(turns) => turns,
                    Err(err) => {
                        log::warn!(
                            "auto_processor `{}` [{uuid}]: reading the turns added since the last \
                             tick failed: {err:#}",
                            cfg.processor_id
                        );
                        if job_cancelled(parent) {
                            return PrepareOutcome::Finished(TickOutcome::Pending);
                        }
                        continue;
                    }
                };
                let Some(gained) = hash_turns(&gained, parent, stage.stop()) else {
                    if job_cancelled(parent) {
                        return PrepareOutcome::Finished(TickOutcome::Pending);
                    }
                    log::warn!(
                        "auto_processor `{}` [{uuid}]: stopped while hashing the turns added \
                         since the last tick",
                        cfg.processor_id
                    );
                    continue;
                };
                match watcher
                    .reads
                    .merge_transcript(cfg, generation, gained, queueable)
                {
                    Ok(turns) => turns,
                    Err(()) => {
                        if job_cancelled(parent) {
                            return PrepareOutcome::Finished(TickOutcome::Pending);
                        }
                        log::warn!(
                            "auto_processor `{}` [{uuid}]: stopped while merging the transcript",
                            cfg.processor_id
                        );
                        continue;
                    }
                }
            }
            _ => {
                let gained = match gather_transcript_sparql_stopped(
                    perspective,
                    &cfg.source_scope_query,
                    stage.stop(),
                )
                .await
                {
                    Ok(turns) => turns,
                    Err(err) => {
                        log::warn!(
                            "auto_processor `{}` [{uuid}]: gather_transcript_sparql failed: {err:#}",
                            cfg.processor_id
                        );
                        if job_cancelled(parent) {
                            return PrepareOutcome::Finished(TickOutcome::Pending);
                        }
                        continue;
                    }
                };
                let Some(gained) = hash_turns(&gained, parent, stage.stop()) else {
                    if job_cancelled(parent) {
                        return PrepareOutcome::Finished(TickOutcome::Pending);
                    }
                    log::warn!(
                        "auto_processor `{}` [{uuid}]: stopped while hashing the transcript",
                        cfg.processor_id
                    );
                    continue;
                };
                match watcher
                    .reads
                    .set_transcript(cfg, generation, gained, queueable)
                {
                    Ok(turns) => turns,
                    Err(()) => {
                        if job_cancelled(parent) {
                            return PrepareOutcome::Finished(TickOutcome::Pending);
                        }
                        log::warn!(
                            "auto_processor `{}` [{uuid}]: stopped while keeping the transcript",
                            cfg.processor_id
                        );
                        continue;
                    }
                }
            }
        };

        for (i, turn) in transcript.iter().enumerate() {
            if i % 32 == 0 && (job_cancelled(parent) || stage.stop().is_stopped()) {
                // Partial records stay. The next tick records the rest from
                // the kept transcript. Do not drain this one.
                return PrepareOutcome::Finished(TickOutcome::Pending);
            }
            let CachedTurn::Full(pending) = turn else {
                continue;
            };
            if !queueable(&pending.id, &pending.timestamp)
                || watcher.is_deferred(&cfg.processor_id, &pending.id, now_ms)
            {
                continue;
            }
            watcher.record_item(&cfg.processor_id, pending.clone(), now_ms);
        }
    }

    let had_pending = watcher.has_pending_for(configs.iter().map(|cfg| cfg.processor_id.as_str()));
    if !had_pending {
        return PrepareOutcome::Finished(TickOutcome::Idle);
    }
    if job_cancelled(parent) {
        return PrepareOutcome::Finished(TickOutcome::Pending);
    }
    PrepareOutcome::Drain(configs)
}

fn hash_turns(
    turns: &[crate::perspectives::interpretation::TranscriptTurn],
    parent: &CancellationToken,
    stop: &EvalStop,
) -> Option<Vec<PendingTurn>> {
    let mut out = Vec::with_capacity(turns.len());
    for (i, turn) in turns.iter().enumerate() {
        if i % 32 == 0 && (job_cancelled(parent) || stop.is_stopped()) {
            return None;
        }
        out.push(PendingTurn::from_transcript(turn));
    }
    Some(out)
}

#[cfg(test)]
pub(crate) use probe::probe_query;

#[cfg(test)]
pub(crate) mod probe {
    use super::Stage;
    use crate::perspectives::sparql_store::EvalStop;
    use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
    use std::sync::Mutex;
    use std::thread::ThreadId;
    use std::time::{Duration, Instant};

    static PROBE_ON: AtomicBool = AtomicBool::new(false);
    static PASS: AtomicBool = AtomicBool::new(false);
    static RELEASE: AtomicBool = AtomicBool::new(false);
    static CURRENT: AtomicUsize = AtomicUsize::new(0);
    static PEAK: AtomicUsize = AtomicUsize::new(0);
    static ENTERED: AtomicU64 = AtomicU64::new(0);
    static STOPPING: AtomicBool = AtomicBool::new(false);
    static STOPPING_WHILE_RUNNING: AtomicBool = AtomicBool::new(false);
    static SAW_STOP: AtomicBool = AtomicBool::new(false);
    static STOP_WAS_DEADLINE: AtomicBool = AtomicBool::new(false);

    static HOLD_START: Mutex<Option<Instant>> = Mutex::new(None);
    static HOLD_END: Mutex<Option<Instant>> = Mutex::new(None);
    static QUERY_THREAD: Mutex<Option<ThreadId>> = Mutex::new(None);

    #[derive(Clone, Copy, Debug)]
    pub(crate) enum Park {
        All,
        Only(Stage),
    }

    /// `Timed` stays for the duration, and returns the stop error if the stop
    /// fires first. `UntilRelease` stays until [`set_pass`] or the guard drops,
    /// including after the stop has fired, so the permit stays held.
    #[derive(Clone, Copy, Debug)]
    pub(crate) enum Hold {
        Timed(Duration),
        UntilRelease { limit: Duration },
    }

    struct Spec {
        park: Park,
        hold: Hold,
        panic_stage: Option<Stage>,
        deadline: Option<(Stage, Duration)>,
        /// The first parked query of each stage holds. Later queries of that
        /// stage return at once, so a per-stage budget can still finish.
        hold_once: bool,
    }

    static SPEC: Mutex<Option<Spec>> = Mutex::new(None);
    /// When set, the next query whose text contains this needle cancels `stop`.
    /// Active even while the hold probe is off, so a shape-load test can fire
    /// one optional read without parking the rest.
    static CANCEL_NEEDLE: Mutex<Option<String>> = Mutex::new(None);

    pub(crate) struct CancelOnQuery {
        prev: Option<String>,
    }

    impl Drop for CancelOnQuery {
        fn drop(&mut self) {
            *CANCEL_NEEDLE.lock().unwrap_or_else(|e| e.into_inner()) = self.prev.take();
        }
    }

    /// Cancel the bound stop when a query contains `needle`.
    pub(crate) fn cancel_on_query(needle: &str) -> CancelOnQuery {
        let mut slot = CANCEL_NEEDLE.lock().unwrap_or_else(|e| e.into_inner());
        let prev = slot.clone();
        *slot = Some(needle.to_string());
        CancelOnQuery { prev }
    }

    fn cancel_if_needle(stop: &EvalStop, query: &str) -> bool {
        let needle = CANCEL_NEEDLE
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .clone();
        let Some(needle) = needle else {
            return false;
        };
        if !query.contains(&needle) {
            return false;
        }
        stop.cancel_for_test();
        true
    }
    /// Stages that have already taken their one hold. Reset by [`install`].
    static HELD_ONCE: Mutex<[bool; 3]> = Mutex::new([false; 3]);

    fn stage_slot(stage: Stage) -> usize {
        match stage {
            Stage::Config => 0,
            Stage::Cursor => 1,
            Stage::Source => 2,
        }
    }

    pub(crate) struct Guard;

    impl Drop for Guard {
        fn drop(&mut self) {
            RELEASE.store(true, Ordering::Release);
            PASS.store(true, Ordering::Release);
            PROBE_ON.store(false, Ordering::Release);
        }
    }

    fn install_spec(spec: Spec) -> Guard {
        CURRENT.store(0, Ordering::Release);
        PEAK.store(0, Ordering::Release);
        ENTERED.store(0, Ordering::Release);
        STOPPING.store(false, Ordering::Release);
        STOPPING_WHILE_RUNNING.store(false, Ordering::Release);
        SAW_STOP.store(false, Ordering::Release);
        STOP_WAS_DEADLINE.store(false, Ordering::Release);
        PASS.store(false, Ordering::Release);
        RELEASE.store(false, Ordering::Release);
        *HOLD_START.lock().unwrap_or_else(|e| e.into_inner()) = None;
        *HOLD_END.lock().unwrap_or_else(|e| e.into_inner()) = None;
        *QUERY_THREAD.lock().unwrap_or_else(|e| e.into_inner()) = None;
        *HELD_ONCE.lock().unwrap_or_else(|e| e.into_inner()) = [false; 3];
        *SPEC.lock().unwrap_or_else(|e| e.into_inner()) = Some(spec);
        PROBE_ON.store(true, Ordering::Release);
        Guard
    }

    pub(crate) fn install(
        park: Park,
        hold: Hold,
        panic_stage: Option<Stage>,
        deadline: Option<(Stage, Duration)>,
    ) -> Guard {
        install_spec(Spec {
            park,
            hold,
            panic_stage,
            deadline,
            hold_once: false,
        })
    }

    /// Park the first query of each stage. Later queries of that stage return.
    pub(crate) fn install_holding_each_stage_once(
        hold: Hold,
        deadline: Option<(Stage, Duration)>,
    ) -> Guard {
        install_spec(Spec {
            park: Park::All,
            hold,
            panic_stage: None,
            deadline,
            hold_once: true,
        })
    }

    pub(crate) fn set_pass() {
        PASS.store(true, Ordering::Release);
    }

    pub(crate) fn current() -> usize {
        CURRENT.load(Ordering::Acquire)
    }

    pub(crate) fn peak() -> usize {
        PEAK.load(Ordering::Acquire)
    }

    pub(crate) fn entered() -> u64 {
        ENTERED.load(Ordering::Acquire)
    }

    pub(crate) fn stopping() -> bool {
        STOPPING.load(Ordering::Acquire)
    }

    pub(crate) fn stopping_while_running() -> bool {
        STOPPING_WHILE_RUNNING.load(Ordering::Acquire)
    }

    pub(crate) fn saw_stop() -> bool {
        SAW_STOP.load(Ordering::Acquire)
    }

    pub(crate) fn stop_was_deadline() -> bool {
        STOP_WAS_DEADLINE.load(Ordering::Acquire)
    }

    pub(crate) fn hold_start() -> Option<Instant> {
        *HOLD_START.lock().unwrap_or_else(|e| e.into_inner())
    }

    pub(crate) fn hold_end() -> Option<Instant> {
        *HOLD_END.lock().unwrap_or_else(|e| e.into_inner())
    }

    pub(crate) fn query_thread() -> Option<ThreadId> {
        *QUERY_THREAD.lock().unwrap_or_else(|e| e.into_inner())
    }

    pub(crate) fn deadline_for(stage: Stage) -> Option<Duration> {
        if !PROBE_ON.load(Ordering::Acquire) {
            return None;
        }
        SPEC.lock()
            .unwrap_or_else(|e| e.into_inner())
            .as_ref()
            .and_then(|spec| spec.deadline)
            .filter(|(wanted, _)| *wanted == stage)
            .map(|(_, deadline)| deadline)
    }

    pub(crate) fn classify(query: &str) -> Stage {
        if query.contains("?speaker") {
            Stage::Source
        } else if query.contains("interp/run_id") || query.contains("interp/sources") {
            Stage::Cursor
        } else {
            Stage::Config
        }
    }

    fn note_stop(stop: &EvalStop) {
        if !stop.is_stopped() {
            return;
        }
        SAW_STOP.store(true, Ordering::Release);
        STOPPING.store(true, Ordering::Release);
        if crate::perspectives::sparql_store::eval_count::running() > 0 {
            STOPPING_WHILE_RUNNING.store(true, Ordering::Release);
        }
        if stop
            .stopped_err("query")
            .is_some_and(|err| err.to_string().contains("deadline"))
        {
            STOP_WAS_DEADLINE.store(true, Ordering::Release);
        }
    }

    fn enter_hold() {
        let now = CURRENT.fetch_add(1, Ordering::AcqRel) + 1;
        PEAK.fetch_max(now, Ordering::AcqRel);
        ENTERED.fetch_add(1, Ordering::AcqRel);
        let mut start = HOLD_START.lock().unwrap_or_else(|e| e.into_inner());
        if start.is_none() {
            *start = Some(Instant::now());
        }
        *QUERY_THREAD.lock().unwrap_or_else(|e| e.into_inner()) = Some(std::thread::current().id());
    }

    fn leave_hold() {
        CURRENT.fetch_sub(1, Ordering::AcqRel);
        *HOLD_END.lock().unwrap_or_else(|e| e.into_inner()) = Some(Instant::now());
    }

    /// Called from `query_internal` after the running-eval count goes up.
    /// A returned error fails that query. Off, this is one relaxed load.
    pub(crate) fn probe_query(
        stop: &EvalStop,
        query: &str,
    ) -> Result<(), deno_core::anyhow::Error> {
        if cancel_if_needle(stop, query) {
            return Err(stop
                .stopped_err("query")
                .unwrap_or_else(|| deno_core::anyhow::anyhow!("query cancelled")));
        }
        if !PROBE_ON.load(Ordering::Relaxed) {
            return Ok(());
        }
        if PASS.load(Ordering::Acquire) {
            ENTERED.fetch_add(1, Ordering::AcqRel);
            return Ok(());
        }
        let stage = classify(query);
        let spec = SPEC
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .as_ref()
            .map(|spec| (spec.park, spec.hold, spec.panic_stage, spec.hold_once));
        let Some((park, hold, panic_stage, hold_once)) = spec else {
            return Ok(());
        };
        if panic_stage == Some(stage) {
            panic!("auto-processor read probe");
        }
        let parked = match park {
            Park::All => true,
            Park::Only(wanted) => wanted == stage,
        };
        if !parked {
            return Ok(());
        }
        if hold_once {
            let mut held = HELD_ONCE.lock().unwrap_or_else(|e| e.into_inner());
            let slot = stage_slot(stage);
            if held[slot] {
                return Ok(());
            }
            held[slot] = true;
        }

        enter_hold();
        let started = Instant::now();
        let result = loop {
            note_stop(stop);
            let release = RELEASE.load(Ordering::Acquire);
            let pass = PASS.load(Ordering::Acquire);
            if stop.is_stopped() && (matches!(hold, Hold::Timed(_)) || pass || release) {
                break Err(stop
                    .stopped_err("query")
                    .unwrap_or_else(|| deno_core::anyhow::anyhow!("query cancelled")));
            }
            match hold {
                Hold::Timed(limit) if started.elapsed() >= limit || release => break Ok(()),
                Hold::UntilRelease { limit } if pass || release || started.elapsed() >= limit => {
                    if stop.is_stopped() {
                        break Err(stop
                            .stopped_err("query")
                            .unwrap_or_else(|| deno_core::anyhow::anyhow!("query cancelled")));
                    }
                    break Ok(());
                }
                _ => std::thread::sleep(Duration::from_millis(5)),
            }
        };
        leave_hold();
        result
    }
}
