//! A grant as a set of time windows, and eligibility as of a timestamp.

use crate::perspectives::flow_evaluator::{cardinality_satisfied, RoleRevocation};
use crate::perspectives::flow_instance::time::parse_link_timestamp;
use crate::perspectives::shacl_parser::ModelQueryCount;
use serde::{Deserialize, Serialize};
/// One matched role instance's history for one DID: when it started to count and
/// every authorised tombstone that ended it.
///
/// A **computed view**, never a carried value. It is what
/// [`RoleGrantEvidence::resolve`] derives from the signed links in the
/// read-set, so the chronology a verdict rests on is re-derived by whoever
/// reads it rather than asserted by whoever wrote it. `Serialize` stays only
/// because surfaces render this shape; nothing in the read-set holds one.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RoleGrantWindow {
    /// The instance the role query matched for this DID.
    pub instance_id: String,
    /// When the instance started to count — see the module doc.
    pub granted_at: String,
    /// The authorised, signed tombstones on this instance naming this DID,
    /// earliest first. Usually none or one.
    pub revocations: Vec<RoleRevocation>,
}

impl RoleGrantWindow {
    /// The moment the instance stopped counting, if it has: the earliest
    /// revocation **by parsed instant** — string `min()` would pick by
    /// client format inside a sub-second collision (#1000).
    ///
    /// Fail-closed like [`Self::open_at`]: a revocation that cannot be
    /// placed in time sorts *first*, never last. `open_at` treats such a
    /// tombstone as closing the window outright, so any surface rendering
    /// this answer must show that tombstone — a later, parseable one
    /// presenting itself as the earliest (or, with no other revocations,
    /// the instance reading as "not revoked") would be the "revocation ignored"
    /// direction this module cannot afford.
    pub fn revoked_at(&self) -> Option<&str> {
        self.revocations
            .iter()
            .min_by_key(|r| {
                let parsed = parse_link_timestamp(&r.at);
                (
                    parsed.is_some(),
                    parsed.unwrap_or(chrono::DateTime::<chrono::Utc>::MIN_UTC),
                    r.at.as_str(),
                )
            })
            .map(|r| r.at.as_str())
    }

    /// Whether the instance counted for a vote cast at `at`:
    /// `granted_at <= at < revoked_at`, compared as parsed instants —
    /// timestamps are client-asserted and clients disagree on RFC 3339
    /// flavour, so string comparison would gate same-second votes by client
    /// library (#1000).
    ///
    /// **Anything that cannot be placed in time fails closed.** An
    /// unparseable `at` or `granted_at` means the window never opens for
    /// that question; an unparseable revocation timestamp closes the window
    /// outright — an authorised tombstone exists, so the safe reading is
    /// "revoked", never "revocation ignored". A revocation stamped with the
    /// vote's own timestamp already gates it.
    pub fn open_at(&self, at: &str) -> bool {
        let (Some(at), Some(granted)) = (
            parse_link_timestamp(at),
            parse_link_timestamp(&self.granted_at),
        ) else {
            return false;
        };
        if granted > at {
            return false;
        }
        let mut revoked: Option<chrono::DateTime<chrono::Utc>> = None;
        for r in &self.revocations {
            match parse_link_timestamp(&r.at) {
                None => return false,
                Some(instant) => revoked = Some(revoked.map_or(instant, |m| m.min(instant))),
            }
        }
        revoked.is_none_or(|revoked| at < revoked)
    }
}

/// The role instances behind one `(target state, DID)` pair, with their history.
///
/// A **computed view** over [`RoleGrantEvidence`], produced by
/// [`RoleGrantEvidence::resolve`] and consumed by [`eligible_votes`]. It is
/// what makes a verdict readable after the fact — "Bob counted toward
/// `approved` at 10:02 because instance r1 said he was a Reviewer from 09:00
/// and its tombstone is from 11:00" — but it is not what travels: the
/// read-set carries the links those sentences were derived from, so a reader
/// re-derives the sentence instead of believing it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RoleGrant {
    /// The state whose rule asked the question — rules are per target state,
    /// so the same DID can be eligible for one edge and not another.
    pub to_state: String,
    /// The role query's class, for readability of a serialised read-set.
    pub role_class: String,
    pub did: String,
    /// IDs of the instances the role query matched for this DID (`windows`, by
    /// ID).
    pub instances: Vec<String>,
    /// One history per matched instance, sorted by `(granted_at, instance_id)`.
    pub windows: Vec<RoleGrantWindow>,
}

impl RoleGrant {
    /// How many of the matched instances counted at `at`.
    pub fn instances_open_at(&self, at: &str) -> usize {
        self.windows.iter().filter(|w| w.open_at(at)).count()
    }

    /// Whether this DID satisfied the rule's `count` at `at` — the number of
    /// instances open then, under the same cardinality rule a `requires` guard
    /// uses (`None` = at least one).
    pub fn eligible_at(&self, at: &str, count: Option<&ModelQueryCount>) -> bool {
        cardinality_satisfied(count, self.instances_open_at(at))
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_support::*;
    use super::*;
    /// `revoked_at` must share `open_at`'s fail direction: a revocation that
    /// cannot be placed in time closes the window (`open_at`), so it is also
    /// the one `revoked_at` surfaces — never out-ranked by a later,
    /// parseable tombstone, and never dropped into "not revoked".
    #[test]
    fn an_unparseable_revocation_is_surfaced_never_ignored() {
        let w = RoleGrantWindow {
            instance_id: "r0".into(),
            granted_at: T0.into(),
            revocations: vec![
                revocation(ADMIN(), "not-a-timestamp"),
                revocation(ADMIN(), T2),
            ],
        };
        assert_eq!(w.revoked_at(), Some("not-a-timestamp"));
        assert!(!w.open_at(NOW), "unparseable revocation closes the window");

        let only_garbage = RoleGrantWindow {
            instance_id: "r0".into(),
            granted_at: T0.into(),
            revocations: vec![revocation(ADMIN(), "not-a-timestamp")],
        };
        assert_eq!(only_garbage.revoked_at(), Some("not-a-timestamp"));
    }
}
