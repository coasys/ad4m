//! The verdict of a link and of a key. The link judgement ([`LinkVerdict`], [`LinkFacts`],
//! [`judge`]) lives in the identity core, one copy for the executor, the link server and the
//! link language (`ad4m_identity_core::judgement`); this module re-exports it and keeps what
//! only the executor needs: when a new judgement replaces a stored verdict ([`replaces`]), a
//! key's verdict at the head of its log ([`key_verdict`]) and the cache's feed of key verdict
//! changes ([`VerdictChange`]).

use ad4m_identity_core::KeyState;

pub use ad4m_identity_core::judgement::{judge, InvalidReason, LinkFacts, LinkVerdict, SignerLog};

/// Whether a new judgement `new` of a stored link replaces its stored verdict `stored`, on a
/// write of the same proof and on a re-check.
///
/// A stored `Valid` is never replaced by `CantCheck`. `CantCheck` only says the executor cannot
/// tell right now (the signer's log is not in the key map: before the held logs load, after an
/// eviction, after a log reset), while a stored `Valid` is the one order the executor knows: the
/// link counted before any revocation the key map holds. That order is read from the stored
/// verdict (`entered_after_revoke`), so writing `CantCheck` over it would lose it, and a later
/// revocation would flag the link for good. A stored `Valid` still moves to `Invalid` or
/// `Flagged`, which the log decides.
///
/// No other stored verdict needs this: `Invalid` and `Flagged` are computed again from the log,
/// the signature and the clock alone, never from the stored verdict, so a link that passes
/// through `CantCheck` comes back to them on the re-check that runs once the log is known.
pub fn replaces(stored: Option<&LinkVerdict>, new: &LinkVerdict) -> bool {
    !matches!(
        (stored, new),
        (Some(LinkVerdict::Valid), LinkVerdict::CantCheck)
    )
}

/// A key's verdict at the head of its log, for the sessions that use the key and for the
/// re-check of links.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KeyVerdict {
    Valid,
    Revoked,
    Expired,
    NotInLog,
}

/// The cache learnt that the verdict of `key` (a signing multikey) in the log of `scid` changed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VerdictChange {
    pub key: String,
    pub scid: String,
    pub verdict: KeyVerdict,
}

/// The verdict of `key` (a signing multikey, device or master) at the trusted seq `trusted` of
/// the log `state`, by the clock `now`.
pub fn key_verdict(state: &KeyState, trusted: u64, key: &str, now: u64) -> KeyVerdict {
    let Some(record) = state.record(key).filter(|r| r.added <= trusted) else {
        return KeyVerdict::NotInLog;
    };
    if record.revoked.is_some_and(|r| r <= trusted) {
        return KeyVerdict::Revoked;
    }
    if state.key_valid_at_time(&record.id, trusted, now) {
        KeyVerdict::Valid
    } else {
        KeyVerdict::Expired
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::identity::cache::tests::TestLog;

    const NOW: u64 = 1_000;

    /// A log whose `d1` was added at 0 and, when `revoked`, revoked at that seq.
    fn log_with(revoked: Option<u64>) -> (KeyState, String) {
        let mut state = TestLog::new(41).chain().state().clone();
        let d1 = state.keys.get_mut("d1").expect("d1");
        d1.revoked = revoked;
        let device = d1.sign.clone();
        (state, device)
    }

    #[test]
    fn only_a_stored_valid_is_kept_over_cant_check() {
        let all = [
            LinkVerdict::Valid,
            LinkVerdict::CantCheck,
            LinkVerdict::Flagged,
            LinkVerdict::Invalid(InvalidReason::Revoked),
        ];
        for new in &all {
            assert!(replaces(None, new));
            for stored in &all {
                let kept = *stored == LinkVerdict::Valid && *new == LinkVerdict::CantCheck;
                assert_eq!(replaces(Some(stored), new), !kept, "{stored:?} -> {new:?}");
            }
        }
    }

    #[test]
    fn key_verdicts_read_the_log_up_to_its_trusted_seq() {
        let (state, device) = log_with(Some(1));
        assert_eq!(key_verdict(&state, 0, &device, NOW), KeyVerdict::Valid);
        assert_eq!(key_verdict(&state, 1, &device, NOW), KeyVerdict::Revoked);
        assert_eq!(
            key_verdict(&state, 1, "z6MkUnknown", NOW),
            KeyVerdict::NotInLog
        );
    }
}
