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
/// What the entry of a call that ran and answered says.
pub const ALLOWED: &str = "allowed";
/// The prefix of the entry of a call the bot check let through but that then failed (its
/// guard or its handler answered an error): `failed:<status>`.
pub const FAILED: &str = "failed";

/// The outcome of a call the bot check let through, by what it answered.
pub fn answered(answer: Result<(), &WsRpcError>) -> String {
    match answer {
        Ok(()) => ALLOWED.to_string(),
        Err(e) => format!("{FAILED}:{}", e.code),
    }
}

/// The outcome of the bot check alone: a refusal's code. An admitted call's outcome is known
/// only once it ran ([`Pending::finish`]).
pub fn refused(refusal: &Refusal) -> String {
    refusal.code.to_string()
}

/// A call the bot check let through, logged once the dispatcher knows its answer.
pub struct Pending {
    bot: BotRow,
    method: String,
    reach: Reach,
    reason: Option<String>,
}

impl Pending {
    pub fn new(bot: &BotRow, method: &str, reach: Reach, reason: Option<&str>) -> Self {
        Pending {
            bot: bot.clone(),
            method: method.to_string(),
            reach,
            reason: reason.map(str::to_string),
        }
    }

    /// Logs the call with its answer: [`ALLOWED`], or `failed:<status>`.
    pub fn finish(self, answer: Result<(), &WsRpcError>) {
        let outcome = answered(answer);
        record(
            &self.bot,
            &self.method,
            &self.reach,
            self.reason.as_deref(),
            &outcome,
        );
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
    if let Err(e) = db_backend().add_bot_activity(&entry, KEEP_PER_BOT) {
        ::log::warn!("bots: logging {method} of {}: {e}", bot.did);
    }
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
