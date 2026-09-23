//! Comparing client-asserted link timestamps by instant, not by string.
//!
//! Every ordering decision in this engine reads a link `timestamp` — a field
//! the *client* wrote. The two clients in this repo alone do not agree on
//! format: Rust's `to_rfc3339()` writes `…T21:35:00+00:00`, TS's
//! `toISOString()` writes `…T21:35:00.000Z`. Both name the same instant, and
//! they do not sort the same as strings — so any string comparison lets a
//! sub-second race be decided by which library wrote the vote (#1000).
//!
//! The timestamp is inside the signed envelope, so it cannot be canonicalised
//! at ingest without invalidating the signature. The fix is therefore to
//! parse at the point of comparison: every sort/min/max over timestamps in
//! this module tree goes through [`parse_link_timestamp`] and compares the
//! parsed instant, keeping the original string for storage and read-sets.
//!
//! **Unparseable timestamps fail closed.** A timestamp that does not parse
//! cannot be placed in time, and "cannot be placed in time" must never
//! resolve to "earliest" (or a garbage timestamp would win every race).
//! Concretely: an unparseable vote timestamp drops that vote (loudly, via
//! `warn!`), an atom none of whose proposer links carry a parseable
//! timestamp is rejected ([`super::atom::AtomRejection::NoParseableTimestamp`]),
//! and a role window that cannot be placed in time never opens
//! ([`super::roles::RoleGrantWindow::open_at`]).

use chrono::{DateTime, Utc};

/// The instant a link timestamp names, or `None` for anything that is not
/// RFC 3339. Offsets are honoured (`…02:00:00+02:00` == `…00:00:00Z`).
pub fn parse_link_timestamp(ts: &str) -> Option<DateTime<Utc>> {
    DateTime::parse_from_rfc3339(ts)
        .ok()
        .map(|dt| dt.with_timezone(&Utc))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The exact hazard class from #1000: pairs whose string order and
    /// instant order disagree. Whole-second differences are immune (digits
    /// dominate); the danger is confined to sub-second collisions and mixed
    /// offsets.
    #[test]
    fn string_order_and_instant_order_disagree_on_the_issue_1000_pairs() {
        // Same second: 'Z' (0x5A) sorts after '.' (0x2E), so the EARLIER
        // instant is the LATER string.
        let (a, b) = ("2026-01-01T00:00:00Z", "2026-01-01T00:00:00.100Z");
        assert!(a > b, "string order");
        assert!(
            parse_link_timestamp(a).unwrap() < parse_link_timestamp(b).unwrap(),
            "instant order"
        );

        // Mixed offsets: the '+02:00' form names the earlier instant but
        // sorts after every same-day 'T00:…' string.
        let (c, d) = ("2026-01-01T02:00:00+02:00", "2026-01-01T00:00:00.100Z");
        assert!(c > d, "string order");
        assert!(
            parse_link_timestamp(c).unwrap() < parse_link_timestamp(d).unwrap(),
            "instant order"
        );
    }

    #[test]
    fn garbage_and_empty_do_not_parse() {
        assert_eq!(parse_link_timestamp(""), None);
        assert_eq!(parse_link_timestamp("not-a-timestamp"), None);
        assert_eq!(parse_link_timestamp("2026-01-01"), None); // date, no time
    }
}
