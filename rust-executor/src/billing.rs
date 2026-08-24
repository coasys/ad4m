use crate::db::Ad4mDb;
use crate::pubsub::mark_credits_dirty;

#[derive(Debug, thiserror::Error)]
pub enum BillingError {
    #[error("Insufficient compute credits")]
    InsufficientCredits,
    #[error("User not found: {0}")]
    UserNotFound(String),
    #[error("{0}")]
    Other(#[from] anyhow::Error),
}

// ---------------------------------------------------------------------------
// Test-only observability: per-thread call counter + optional forced result.
//
// Purpose: give behavioural tests a way to assert "handler X billed exactly
// once with operation Y and amount Z" without a full DB fixture, and to
// inject BillingError variants so ?-propagation can be exercised.
//
// Design: thread_local! so tests running on the same test binary but
// different threads do not clobber each other's counters. Cargo test runs
// tests on separate threads by default; the AI service also spawns runtime
// threads, so we only record calls that happen on the test's own thread
// (which is the correct scope for handler-level billing).
//
// Production path: zero cost. All the recording lives behind
// #[cfg(test)] and does not compile into release binaries.
// ---------------------------------------------------------------------------
#[cfg(test)]
pub mod test_seam {
    use super::BillingError;
    use std::cell::RefCell;

    #[derive(Debug, Clone, PartialEq)]
    pub struct BillCall {
        pub email: String,
        pub amount: f64,
        pub operation: String,
        pub summary: Option<String>,
    }

    thread_local! {
        pub(super) static CALLS: RefCell<Vec<BillCall>> = const { RefCell::new(Vec::new()) };
        pub(super) static FORCED_RESULT: RefCell<Option<ForcedResult>> = const { RefCell::new(None) };
    }

    #[derive(Clone, Copy)]
    pub enum ForcedResult {
        InsufficientCredits,
        UserNotFound,
        OtherError,
        Success, // still short-circuits the DB path (useful when no DB is initialised)
    }

    /// Reset the counter and clear any forced result. Call at the start of each test.
    pub fn reset() {
        CALLS.with(|c| c.borrow_mut().clear());
        FORCED_RESULT.with(|f| *f.borrow_mut() = None);
    }

    /// Snapshot the recorded calls on the current thread.
    pub fn calls() -> Vec<BillCall> {
        CALLS.with(|c| c.borrow().clone())
    }

    /// Count how many `bill_compute` calls have been recorded on the current thread.
    pub fn call_count() -> usize {
        CALLS.with(|c| c.borrow().len())
    }

    /// Force the next (and subsequent) `bill_compute` calls to short-circuit
    /// with the given result instead of hitting the DB. Cleared by `reset()`.
    pub fn force_result(r: ForcedResult) {
        FORCED_RESULT.with(|f| *f.borrow_mut() = Some(r));
    }

    pub(super) fn record(email: &str, amount: f64, operation: &str, summary: Option<&str>) {
        CALLS.with(|c| {
            c.borrow_mut().push(BillCall {
                email: email.to_string(),
                amount,
                operation: operation.to_string(),
                summary: summary.map(|s| s.to_string()),
            });
        });
    }

    pub(super) fn take_forced_result() -> Option<Result<(), BillingError>> {
        FORCED_RESULT.with(|f| {
            f.borrow().map(|r| match r {
                ForcedResult::InsufficientCredits => Err(BillingError::InsufficientCredits),
                ForcedResult::UserNotFound => Err(BillingError::UserNotFound("test".into())),
                ForcedResult::OtherError => Err(BillingError::Other(anyhow::anyhow!("forced"))),
                ForcedResult::Success => Ok(()),
            })
        })
    }
}

const DEFAULT_LINK_WRITE_RATE: f64 = 0.25; // credits per link write

/// Look up the link write rate from the host_rates DB table, falling back to the default.
pub fn get_link_write_rate() -> f64 {
    Ad4mDb::with_global_instance(|db| db.get_host_rate("link write"))
        .ok()
        .flatten()
        .unwrap_or(DEFAULT_LINK_WRITE_RATE)
}

/// Bill a single AI compute operation using the shared `host_rates` table.
///
/// This is the ONE call site every AI billing path (WS-RPC prompt/embed,
/// /v1 chat/embeddings, whisper transcription worker) should go through.
/// It matches the pre-existing transcription-worker pattern:
///
///   1. Look up the model's `name` (falling back to `model_id` when the
///      model row isn't in the DB, matching the transcription worker's
///      behaviour under uninitialised state).
///   2. Look up `host_rates.price_in_hot` keyed by that name;
///      absent → rate = 0.0 (billing effectively no-ops for models
///      the operator hasn't priced yet).
///   3. cost = unit_count × rate.
///   4. Call `bill_compute(email, cost, operation, Some(summary))`.
///   5. `InsufficientCredits` is logged as `warn!` and returned as an
///      error so the caller can decide whether to still deliver the
///      compute; other errors are logged and swallowed (do not fail the
///      user's request on a bookkeeping hiccup).
///
/// Consistency with the WS-RPC transcription pattern (ai_service::
/// open_transcription_stream) is deliberate. Do not inline a
/// different formula per surface — route through this helper.
pub fn bill_ai_operation(
    email: &str,
    model_id: &str,
    operation: &str,
    unit_count: usize,
    unit_label: &str,
) -> Result<(), BillingError> {
    if unit_count == 0 {
        return Ok(());
    }

    let rate_key = Ad4mDb::with_global_instance(|db| db.get_model(model_id.to_string()))
        .ok()
        .flatten()
        .map(|m| m.name)
        .unwrap_or_else(|| model_id.to_string());

    let rate = Ad4mDb::with_global_instance(|db| db.get_host_rate(&rate_key))
        .ok()
        .flatten()
        .unwrap_or(0.0);

    let cost = unit_count as f64 * rate;
    if cost <= 0.0 {
        // Rate = 0 (operator hasn't priced this model) or arithmetic
        // underflow. Log at debug and skip — no reason to hit the
        // DB transaction for a zero-cost debit.
        log::debug!(
            "bill_ai_operation: skipping zero-cost debit for op={} model={} units={} rate={}",
            operation,
            rate_key,
            unit_count,
            rate
        );
        return Ok(());
    }

    let summary = format!(
        "{unit_count} {unit_label} (model: {rate_key})",
        unit_count = unit_count,
        unit_label = unit_label,
        rate_key = rate_key
    );

    match bill_compute(email, cost, operation, Some(&summary)) {
        Ok(()) => Ok(()),
        Err(BillingError::InsufficientCredits) => {
            log::warn!(
                "{}: insufficient credits for user {} — delivering already-computed result",
                operation,
                email
            );
            Err(BillingError::InsufficientCredits)
        }
        Err(e) => {
            log::warn!("{} billing failed: {:?}", operation, e);
            Err(e)
        }
    }
}

/// Read-only credit check. Returns Ok(()) if the user can afford compute.
/// Used as a pre-check before link operations; the actual deduction happens
/// after the operation via bill_compute with the exact cost.
/// No-ops (allows) if free hosting is enabled or user has free access.
pub fn check_compute_credits(email: &str) -> Result<(), anyhow::Error> {
    let global_free =
        Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true);
    if global_free {
        return Ok(());
    }
    let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))?;
    if free {
        return Ok(());
    }
    let credits = Ad4mDb::with_global_instance(|db| db.get_user_credits(email))?;
    if credits <= 0.0 {
        return Err(anyhow::anyhow!("Insufficient compute credits"));
    }
    Ok(())
}

/// Deduct credits and log a compute event for the given user email.
/// No-ops if free hosting is enabled globally or the user has free access.
///
/// This is the shared billing function callable from anywhere in the executor
/// (e.g. AI service async tasks, perspective instance methods) without needing
/// a REST RequestContext or auth token.
pub fn bill_compute(
    email: &str,
    amount: f64,
    operation: &str,
    summary: Option<&str>,
) -> Result<(), BillingError> {
    // Test observability: record every call BEFORE any early return (free-hosting,
    // free-access, DB error), so tests can assert on call count regardless of
    // whether the underlying DB is initialised.
    #[cfg(test)]
    {
        test_seam::record(email, amount, operation, summary);
        if let Some(forced) = test_seam::take_forced_result() {
            return forced;
        }
    }

    let global_free = Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled())?;
    if global_free {
        return Ok(());
    }
    let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))?;
    if free {
        return Ok(());
    }
    let result = Ad4mDb::with_global_instance(|db| {
        db.deduct_credits_and_log(email, amount, operation, summary)
    });

    match result {
        Ok((row_id, credits_after)) => {
            crate::pubsub::push_compute_log_entry(
                row_id,
                email,
                operation,
                summary,
                amount,
                credits_after,
            );
            mark_credits_dirty(email);
            Ok(())
        }
        Err(e) => {
            let msg = e.to_string();
            if msg.contains("Insufficient compute credits") {
                Err(BillingError::InsufficientCredits)
            } else if msg.starts_with("User not found") {
                Err(BillingError::UserNotFound(email.to_string()))
            } else {
                Err(BillingError::Other(e))
            }
        }
    }
}
