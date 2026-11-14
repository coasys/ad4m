//! The bot activity log: every write and join a bot made and every call refused, with the
//! reason the bot gave. Bounded twice: [`KEEP_PER_BOT`] entries per bot (the oldest go on
//! insert) and [`MAX_AGE`] (an hourly sweep). Revoking a bot keeps its log.

use std::time::Duration;

use crate::db::{BotActivityRow, BotRow};
use crate::db_backend::db_backend;

use super::{now, Reach, Refusal};
use crate::api::ws_handler::WsRpcError;

/// The most entries kept per bot.
pub const KEEP_PER_BOT: usize = 10_000;
/// How long an entry is kept, in seconds (30 days).
pub const MAX_AGE: u64 = 30 * 24 * 3600;
/// How often the sweep runs.
pub const SWEEP_INTERVAL: Duration = Duration::from_secs(3600);
/// What the attempt entry of an admitted call says while it runs.
pub const STARTED: &str = "started";
/// What the entry of a call that ran and answered says.
pub const ALLOWED: &str = "allowed";
/// The prefix of the entry of a call the bot check let through but that then failed (its
/// guard or its handler answered an error): `failed:<status>`.
pub const FAILED: &str = "failed";
/// What the entry of a call that never answered says: its dispatch was cancelled or
/// panicked, so its work may be partly applied. `interrupted` is indeterminate, not
/// rolled back.
pub const INTERRUPTED: &str = "interrupted";

/// The outcome of a call the bot check let through, by what it answered.
pub fn answered(answer: Result<(), &WsRpcError>) -> String {
    match answer {
        Ok(()) => ALLOWED.to_string(),
        Err(e) => format!("{FAILED}:{}", e.code),
    }
}

/// The outcome of the bot check alone: a refusal's code. An admitted call's outcome is known
/// only once it ran ([`Pending::finish`]) or was dropped ([`Pending`]'s `Drop`).
pub fn refused(refusal: &Refusal) -> String {
    refusal.code.to_string()
}

/// A call the bot check let through. [`Pending::new`] writes its attempt entry (`started`,
/// its id the call's operation id) before the call runs; [`Pending::finish`] settles the
/// same entry with the call's answer. Dropped without `finish` — a cancelled dispatch, or
/// a panicking handler unwinding through it — the entry settles as [`INTERRUPTED`]: part of
/// the call's work may have applied.
pub struct Pending {
    id: i64,
    finished: bool,
}

impl Pending {
    /// Writes the attempt entry and returns the call's guard. Fails when the log cannot be
    /// written: the call must then not run at all — a bot's write or join never runs
    /// unlogged (fail closed, `503` [`super::LOG_UNAVAILABLE`]).
    pub fn new(
        bot: &BotRow,
        method: &str,
        reach: Reach,
        reason: Option<&str>,
    ) -> Result<Self, WsRpcError> {
        let entry = entry(bot, method, &reach, reason, STARTED, now());
        let id = write(&entry).map_err(|e| {
            ::log::warn!("bots: logging the start of {method} of {}: {e}", bot.did);
            WsRpcError {
                code: 503,
                message: format!(
                    "{}: the activity log could not be written, so the call does not run",
                    super::LOG_UNAVAILABLE
                ),
            }
        })?;
        Ok(Pending {
            id,
            finished: false,
        })
    }

    /// Settles the entry with the call's answer: [`ALLOWED`], or `failed:<status>`.
    pub fn finish(mut self, answer: Result<(), &WsRpcError>) {
        self.finished = true;
        self.settle(&answered(answer));
    }

    /// The outcome on the call's entry. A log that cannot be written does not change the
    /// call's answer; the failure goes to the executor's log.
    fn settle(&self, outcome: &str) {
        if let Err(e) = db_backend().set_bot_activity_outcome(self.id, outcome) {
            ::log::warn!(
                "bots: settling activity entry {} to {outcome}: {e}",
                self.id
            );
        }
    }
}

impl Drop for Pending {
    /// A `Pending` dropped without [`Pending::finish`] settles its entry [`INTERRUPTED`]:
    /// the dispatch future holding it was dropped (cancelled) or unwound (a panic).
    fn drop(&mut self) {
        if !self.finished {
            self.settle(INTERRUPTED);
        }
    }
}

/// The entry of a call of `bot` to `method`.
pub fn entry(
    bot: &BotRow,
    method: &str,
    reach: &Reach,
    reason: Option<&str>,
    outcome: &str,
    at: u64,
) -> BotActivityRow {
    let (action, space) = match reach {
        Reach::Action { action, space } => (
            Some(action.as_str().to_string()),
            space.as_ref().map(|s| s.text().to_string()),
        ),
        Reach::Own | Reach::Outside => (None, None),
    };
    BotActivityRow {
        id: 0,
        bot_did: bot.did.clone(),
        at,
        method: method.to_string(),
        action,
        space,
        reason: reason.map(super::cut_reason),
        outcome: outcome.to_string(),
    }
}

/// Adds the entry of a call to the log. A log that cannot be written does not stop the call:
/// the failure goes to the executor's log.
pub fn record(bot: &BotRow, method: &str, reach: &Reach, reason: Option<&str>, outcome: &str) {
    let entry = entry(bot, method, reach, reason, outcome, now());
    if let Err(e) = write(&entry) {
        ::log::warn!("bots: logging {method} of {}: {e}", bot.did);
    }
}

/// Writes `entry`; the entry's id. A refused call still logs its refusal entry ([`record`]);
/// an admitted call's attempt entry must succeed or the call does not run ([`Pending::new`]).
fn write(entry: &BotActivityRow) -> crate::db::Ad4mDbResult<i64> {
    #[cfg(test)]
    if FAIL_WRITES.load(std::sync::atomic::Ordering::SeqCst) {
        return Err(rusqlite::Error::SqliteFailure(
            rusqlite::ffi::Error::new(rusqlite::ffi::SQLITE_CANTOPEN),
            Some("writes fail by test hook".to_string()),
        )
        .into());
    }
    db_backend().add_bot_activity(entry, KEEP_PER_BOT)
}

#[cfg(test)]
static FAIL_WRITES: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Makes every activity write fail, for the fail-closed test ([`Pending::new`]).
#[cfg(test)]
pub(crate) fn set_fail_writes(fail: bool) {
    FAIL_WRITES.store(fail, std::sync::atomic::Ordering::SeqCst);
}

/// Removes the entries older than [`MAX_AGE`]. Returns how many.
pub fn sweep(now: u64) -> Result<usize, String> {
    db_backend()
        .sweep_bot_activity(now.saturating_sub(MAX_AGE))
        .map_err(|e| e.to_string())
}

/// Sweeps the log every [`SWEEP_INTERVAL`], for as long as the executor runs.
pub async fn run_sweep() {
    let mut interval = tokio::time::interval(SWEEP_INTERVAL);
    loop {
        interval.tick().await;
        match sweep(now()) {
            Ok(0) => {}
            Ok(removed) => ::log::info!("bots: swept {removed} old activity entries"),
            Err(e) => ::log::warn!("bots: sweeping the activity log: {e}"),
        }
    }
}
