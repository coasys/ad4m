//! The users' session cutoffs (`users.sessions_valid_after`): see `agent::revoked`.

use super::{Ad4mDb, Ad4mDbResult};
use rusqlite::params;

impl Ad4mDb {
    /// Sets the time (Unix seconds) at or before which the user's tokens are revoked.
    /// `false` when no user has this email.
    pub fn set_user_sessions_valid_after(&self, email: &str, cutoff: u64) -> Ad4mDbResult<bool> {
        let updated = self.conn.execute(
            "UPDATE users SET sessions_valid_after = ?1 WHERE username = ?2",
            params![i64::try_from(cutoff).unwrap_or(i64::MAX), email],
        )?;
        Ok(updated > 0)
    }

    /// Every user with a session cutoff, with the cutoff.
    pub fn user_session_cutoffs(&self) -> Ad4mDbResult<Vec<(String, u64)>> {
        let mut stmt = self.conn.prepare(
            "SELECT username, sessions_valid_after FROM users WHERE sessions_valid_after IS NOT NULL",
        )?;
        let rows = stmt.query_map([], |row| {
            let cutoff: i64 = row.get(1)?;
            Ok((row.get(0)?, u64::try_from(cutoff).unwrap_or(0)))
        })?;
        Ok(rows.collect::<Result<_, _>>()?)
    }
}
