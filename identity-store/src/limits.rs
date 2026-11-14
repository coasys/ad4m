//! The identity bucket: a token bucket per identity over its writes (offer opens, inbox
//! writes, record writes, request posts). `IDENTITY_BURST` writes pass at once, then one per
//! `IDENTITY_REFILL_S` seconds; past it the store answers `429 {scope:"identity"}` with
//! `Retry-After`. Reads and waits never count against an identity.
//!
//! A write takes its token inside its own transaction, after every other check passed, so a
//! refused or failed write costs nothing.

use rusqlite::{params, Connection, OptionalExtension};

use crate::db::{from_sql, to_sql};
use crate::error::ApiError;
use crate::Settings;

/// Takes one write of `scid` from its bucket at `now`, or refuses with the seconds until the
/// next token.
pub(crate) fn take_identity_write(
    conn: &Connection,
    settings: &Settings,
    scid: &str,
    now: u64,
) -> Result<(), ApiError> {
    let bucket = format!("identity:{scid}");
    let burst = u64::from(settings.identity_burst);
    let refill = settings.identity_refill_s.max(1);
    let row: Option<(i64, i64)> = conn
        .query_row(
            "SELECT tokens, refilled_at FROM buckets WHERE bucket = ?1",
            params![bucket],
            |r| Ok((r.get(0)?, r.get(1)?)),
        )
        .optional()?;
    let (tokens, refilled_at) = match row {
        Some((tokens, at)) => {
            let (tokens, at) = (from_sql(tokens), from_sql(at).min(now));
            let earned = (now - at) / refill;
            let tokens = (tokens + earned).min(burst);
            // A full bucket starts its next refill now; otherwise the partial period carries.
            let at = if tokens == burst {
                now
            } else {
                at + earned * refill
            };
            (tokens, at)
        }
        None => (burst, now),
    };
    if tokens == 0 {
        let retry = (refilled_at + refill).saturating_sub(now).max(1);
        return Err(ApiError::rate_limited("identity", retry));
    }
    conn.execute(
        "INSERT INTO buckets (bucket, tokens, refilled_at) VALUES (?1, ?2, ?3)
         ON CONFLICT(bucket) DO UPDATE SET tokens = excluded.tokens, refilled_at = excluded.refilled_at",
        params![bucket, to_sql(tokens - 1)?, to_sql(refilled_at)?],
    )?;
    Ok(())
}
