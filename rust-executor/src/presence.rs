//! Managed-user presence derived from `users.last_seen` (#1070).
//!
//! One module owns the whole contract so its two sides cannot drift apart:
//! the online window, the write-throttle derived from it, the writer's
//! decision ([`last_seen_write_due`], used by
//! `agent::capabilities::track_last_seen`) and the readers' predicate
//! ([`is_online`], used by the auto-processor supervisor through
//! `select_online_managed_users` and by `PerspectiveInstance::online_agents`
//! for author election). The readers take no window argument, so no call site
//! can pass a different one.
//!
//! ```text
//!  authenticated request ──► last_seen_write_due? ──► users.last_seen
//!                                                          │
//!         supervisor (spawn/reap loops) ◄── is_online ◄────┤
//!         online_agents (elect_author)  ◄── is_online ◄────┘
//! ```
//!
//! An active user's `last_seen` is at most `throttle + request gap` old, so
//! they stay online as long as they send a request at least every
//! `MANAGED_USER_ONLINE_WINDOW_S - LAST_SEEN_WRITE_THROTTLE_S` seconds.

/// Seconds after their last recorded activity at which a managed user counts
/// as offline. The policy value; everything else here derives from it.
pub const MANAGED_USER_ONLINE_WINDOW_S: i64 = 600;

/// Minimum interval (seconds) between two `users.last_seen` writes for the same
/// user. A third of the window, never set on its own: a throttle equal to the
/// window made the writer refresh `last_seen` only once the reader had already
/// called the user offline, so every active user was reaped once per window
/// (#1070). A third leaves two thirds (400 s) for the gap between requests.
pub const LAST_SEEN_WRITE_THROTTLE_S: i64 = MANAGED_USER_ONLINE_WINDOW_S / 3;

/// A stored `last_seen` further than this in the future is treated as stale and
/// rewritten; anything closer is accepted as clock skew.
pub const LAST_SEEN_CLOCK_SKEW_S: i64 = 60;

/// Whether a request at `now` must write `users.last_seen`, given the stored
/// value: never seen, implausibly far in the future, or older than the
/// throttle.
pub fn last_seen_write_due(last_seen: Option<i64>, now: i64) -> bool {
    match last_seen {
        None => true,
        Some(ls) if ls > now + LAST_SEEN_CLOCK_SKEW_S => true,
        Some(ls) => ls < now.saturating_sub(LAST_SEEN_WRITE_THROTTLE_S),
    }
}

/// Whether a managed user last seen at `last_seen` is online at `now`.
/// `None` (never seen since boot) is offline; a future value is clamped to
/// `now`, so a fast client clock is online, not rejected. The boundary is
/// inclusive: `last_seen == now - MANAGED_USER_ONLINE_WINDOW_S` is online.
pub fn is_online(last_seen: Option<i64>, now: i64) -> bool {
    let Some(ls) = last_seen else {
        return false;
    };
    ls.min(now) >= now.saturating_sub(MANAGED_USER_ONLINE_WINDOW_S)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::auto_processor::watcher::select_online_managed_users;

    /// The throttle leaves room for the gap between an active user's requests:
    /// at least two throttles of slack, so the two constants can never again
    /// be complements.
    #[test]
    fn last_seen_write_throttle_leaves_margin_inside_online_window() {
        // Runtime bindings, not a `const` assert: a regression should fail
        // this test by name rather than stop the crate from compiling.
        let throttle = LAST_SEEN_WRITE_THROTTLE_S;
        let window = MANAGED_USER_ONLINE_WINDOW_S;
        assert!(throttle > 0);
        assert!(
            window - throttle >= 2 * throttle,
            "`last_seen` is only rewritten once it is {throttle}s old, so a {window}s window \
             leaves only {}s for the gap between an active user's requests (#1070)",
            window - throttle
        );
    }

    #[test]
    fn last_seen_write_due_boundaries() {
        let now = 1_000_000_i64;
        let t = LAST_SEEN_WRITE_THROTTLE_S;
        assert!(last_seen_write_due(None, now));
        assert!(!last_seen_write_due(Some(now), now));
        assert!(!last_seen_write_due(Some(now - t), now));
        assert!(last_seen_write_due(Some(now - t - 1), now));
        assert!(!last_seen_write_due(
            Some(now + LAST_SEEN_CLOCK_SKEW_S),
            now
        ));
        assert!(last_seen_write_due(
            Some(now + LAST_SEEN_CLOCK_SKEW_S + 1),
            now
        ));
    }

    #[test]
    fn is_online_boundaries() {
        let now = 1_000_000_i64;
        let w = MANAGED_USER_ONLINE_WINDOW_S;
        assert!(!is_online(None, now));
        assert!(is_online(Some(now), now));
        assert!(is_online(Some(now - w), now), "the boundary is inclusive");
        assert!(!is_online(Some(now - w - 1), now));
        assert!(is_online(Some(now + 3_600), now), "future clamps to now");
    }

    /// Both readers of `users.last_seen`: the supervisor's filter and
    /// `online_agents`' predicate.
    fn online_for_both_readers(last_seen: Option<i64>, now: i64) -> (bool, bool) {
        let supervisor =
            !select_online_managed_users(vec![("u@x".to_string(), last_seen)], now).is_empty();
        (supervisor, is_online(last_seen, now))
    }

    /// Behavioural pin for #1070. A user whose requests are never more than
    /// `gap` seconds apart must never be classified offline by either reader.
    ///
    /// The schedule is the worst case for the writer: after each write at `w`
    /// the user sends a request every second up to `w + throttle` (none of
    /// them due), then goes quiet for `gap`. Just before the next request
    /// `last_seen` is `throttle + gap` old, the oldest it can get. Both readers
    /// are checked every second before that second's request lands, the order
    /// in which the flap happened. `MAX_ACTIVE_GAP_S` is the promise made in
    /// the module docs, so at `gap == 400` `last_seen` sits exactly on
    /// `now - window`.
    #[test]
    fn continuously_active_user_is_never_classified_offline() {
        // A fixed literal, not derived from the constants under test.
        const MAX_ACTIVE_GAP_S: i64 = 400;
        const START: i64 = 1_000_000;
        let run_s = 4 * MANAGED_USER_ONLINE_WINDOW_S;

        for gap in 1..=MAX_ACTIVE_GAP_S {
            let mut last_seen: Option<i64> = None;
            let mut next_request = START;
            let mut last_request = START;
            for now in START..START + run_s {
                if now > START {
                    let (supervisor, election) = online_for_both_readers(last_seen, now);
                    assert!(
                        supervisor && election,
                        "user never more than {gap}s between requests classified offline at \
                         t+{}s (last_seen {}s old; supervisor online: {supervisor}, \
                         election online: {election})",
                        now - START,
                        now - last_seen.unwrap_or(START)
                    );
                }
                if now == next_request {
                    last_request = now;
                    if last_seen_write_due(last_seen, now) {
                        last_seen = Some(now);
                    }
                    let written = last_seen.expect("first request always writes");
                    next_request = if now < written + LAST_SEEN_WRITE_THROTTLE_S {
                        now + 1
                    } else {
                        written + LAST_SEEN_WRITE_THROTTLE_S + gap
                    };
                }
            }
            let reaped_by = last_request + MANAGED_USER_ONLINE_WINDOW_S + 1;
            assert_eq!(
                online_for_both_readers(last_seen, reaped_by),
                (false, false),
                "user who stopped at t+{}s still online at t+{}s",
                last_request - START,
                reaped_by - START
            );
        }
    }
}
