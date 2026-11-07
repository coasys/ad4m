//! The bot activity log: every write and join a bot made and every call refused, with the
//! reason the bot gave. Bounded twice: [`KEEP_PER_BOT`] entries per bot (the oldest go on
//! insert) and [`MAX_AGE`] (an hourly sweep). Revoking a bot keeps its log.

use std::time::Duration;

use crate::db::{BotActivityRow, BotRow};
use crate::db_backend::db_backend;

use super::{now, Reach, Refusal};

/// The most entries kept per bot.
pub const KEEP_PER_BOT: usize = 10_000;
/// How long an entry is kept, in seconds (30 days).
pub const MAX_AGE: u64 = 30 * 24 * 3600;
/// How often the sweep runs.
pub const SWEEP_INTERVAL: Duration = Duration::from_secs(3600);
/// What an allowed call's entry says.
pub const ALLOWED: &str = "allowed";

/// The entry of a call of `bot` to `method`.
pub fn entry(
    bot: &BotRow,
    method: &str,
    reach: &Reach,
    reason: Option<&str>,
    decided: &Result<(), Refusal>,
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
        outcome: match decided {
            Ok(()) => ALLOWED.to_string(),
            Err(refusal) => refusal.code.to_string(),
        },
    }
}

/// Adds the entry of a call to the log. A log that cannot be written does not stop the call:
/// the failure goes to the executor's log.
pub fn record(
    bot: &BotRow,
    method: &str,
    reach: &Reach,
    reason: Option<&str>,
    decided: &Result<(), Refusal>,
) {
    let entry = entry(bot, method, reach, reason, decided, now());
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
