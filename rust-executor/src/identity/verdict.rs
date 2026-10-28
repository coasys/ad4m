//! The verdict of a link: does it count, and where is it shown. A pure function of the link's
//! signature, its anchor, what the signer's log says up to its trusted seq, and where the link
//! entered the space's ordered history. No I/O: the caller reads the log (the key map in
//! `cache.rs`) and the order.
//!
//! | Verdict | Logic reads (values, counts, votes, roles, ordering) | Display reads |
//! |---|---|---|
//! | `Valid` | count | show |
//! | `Invalid(reason)` | exclude | hide |
//! | `CantCheck` | exclude until a re-check | show, with the verdict |
//! | `Flagged` | exclude | show, with the verdict |

use ad4m_identity_core::KeyState;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

/// Whether a link counts.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(tag = "kind", content = "reason", rename_all = "camelCase")]
#[ts(export)]
pub enum LinkVerdict {
    /// Every check holds.
    Valid,
    /// A check fails for good.
    Invalid(InvalidReason),
    /// The signer's log is not known up to the anchor yet (the store did not answer, or its
    /// trusted head is below the anchor). A re-check runs when the log is known.
    CantCheck,
    /// The signer's key was valid at the anchor, but the link entered the space's ordered
    /// history after the key's `revoke` entered it.
    Flagged,
}

/// Why a link is `Invalid`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum InvalidReason {
    /// The signature does not hold under the signer's key.
    BadSignature,
    /// The link has an anchor, but no log lists the signer, or the log the key index names
    /// does not list it.
    NotListed,
    /// The signer is a master key. A master signs log events, never content.
    MasterKey,
    /// The anchor is before the seq that added the key.
    NotYetValid,
    /// The key was revoked at or before the anchor.
    Revoked,
    /// A `KeyLife::Until` key or a bot scope is past its `expires` by the executor's clock.
    Expired,
}

impl InvalidReason {
    fn name(self) -> &'static str {
        match self {
            InvalidReason::BadSignature => "badSignature",
            InvalidReason::NotListed => "notListed",
            InvalidReason::MasterKey => "masterKey",
            InvalidReason::NotYetValid => "notYetValid",
            InvalidReason::Revoked => "revoked",
            InvalidReason::Expired => "expired",
        }
    }

    fn from_name(name: &str) -> Option<InvalidReason> {
        [
            InvalidReason::BadSignature,
            InvalidReason::NotListed,
            InvalidReason::MasterKey,
            InvalidReason::NotYetValid,
            InvalidReason::Revoked,
            InvalidReason::Expired,
        ]
        .into_iter()
        .find(|r| r.name() == name)
    }
}

impl LinkVerdict {
    /// Logic reads count the link.
    pub fn counts(&self) -> bool {
        matches!(self, LinkVerdict::Valid)
    }

    /// Display reads show the link (all but `Invalid`).
    pub fn shown(&self) -> bool {
        !matches!(self, LinkVerdict::Invalid(_))
    }

    /// The verdict as the link store keeps it: `valid`, `invalid:<reason>`, `cantCheck`,
    /// `flagged`.
    pub fn stored(&self) -> String {
        match self {
            LinkVerdict::Valid => "valid".into(),
            LinkVerdict::Invalid(reason) => format!("invalid:{}", reason.name()),
            LinkVerdict::CantCheck => "cantCheck".into(),
            LinkVerdict::Flagged => "flagged".into(),
        }
    }

    /// The verdict from its stored form; `None` for a form [`LinkVerdict::stored`] never writes.
    pub fn from_stored(s: &str) -> Option<LinkVerdict> {
        match s {
            "valid" => Some(LinkVerdict::Valid),
            "cantCheck" => Some(LinkVerdict::CantCheck),
            "flagged" => Some(LinkVerdict::Flagged),
            _ => s
                .strip_prefix("invalid:")
                .and_then(InvalidReason::from_name)
                .map(LinkVerdict::Invalid),
        }
    }
}

/// What the signer's log says, as the key map holds it.
#[derive(Clone, Copy, Debug)]
pub enum SignerLog<'a> {
    /// A log lists the signer. `trusted`: the seq of its accepted, store-signed head. Nothing
    /// past it counts.
    Listed { state: &'a KeyState, trusted: u64 },
    /// No log lists the signer: the key index does not know it, or the log it names does not
    /// list it.
    NoLog,
    /// Nothing is known: the lookup failed or did not run.
    Unknown,
}

/// The inputs of [`judge`].
#[derive(Clone, Copy, Debug)]
pub struct LinkFacts<'a> {
    /// The signature holds under the signer's `did:key` (the old signed bytes without an
    /// anchor, `coasys:link` over `{data, timestamp, anchor}` with one).
    pub signature_holds: bool,
    /// The signer's signing multikey.
    pub signer: &'a str,
    /// The seq of the signer's log that the link names; `None` for a link without one.
    pub anchor: Option<u64>,
    pub log: SignerLog<'a>,
    /// Where the link entered the space's ordered history, compared with the `revoke` of its
    /// key: `Some(true)` after it, `Some(false)` before it, `None` when the space gives no
    /// order (a private perspective, or a link language that records no revocations).
    pub entered_after_revoke: Option<bool>,
    /// The executor's clock (unix seconds), for `KeyLife::Until` keys and bot scopes only.
    pub now: u64,
}

/// The verdict of a link.
///
/// 1. A signature that does not hold: `Invalid(BadSignature)`.
/// 2. No anchor and no log that lists the signer (a device-only key, or nothing known):
///    `Valid`, as before key logs.
/// 3. No anchor, and a log lists the signer: judged as anchored at the seq that added the key.
/// 4. With an anchor: no log lists the signer, `Invalid(NotListed)`; nothing known, or the
///    anchor past the trusted seq, `CantCheck`; a master key, `Invalid(MasterKey)`; the key not
///    added yet at the anchor, `Invalid(NotYetValid)`; revoked at or before it,
///    `Invalid(Revoked)`; past an expiry, `Invalid(Expired)`.
/// 5. Revoked after the anchor (at or below the trusted seq): `Flagged` when the link entered
///    the space's history after the revocation did, else `Valid`.
pub fn judge(facts: &LinkFacts) -> LinkVerdict {
    use LinkVerdict::*;
    if !facts.signature_holds {
        return Invalid(InvalidReason::BadSignature);
    }
    let unlisted = |anchored_verdict: LinkVerdict| match facts.anchor {
        None => Valid,
        Some(_) => anchored_verdict,
    };
    let (state, trusted) = match facts.log {
        SignerLog::Listed { state, trusted } => (state, trusted),
        SignerLog::NoLog => return unlisted(Invalid(InvalidReason::NotListed)),
        SignerLog::Unknown => return unlisted(CantCheck),
    };
    let signer = facts.signer.to_string();
    let Some(id) = state.device_key_id_of(&signer) else {
        if state.key_id_of(&signer).is_some() {
            return Invalid(InvalidReason::MasterKey);
        }
        return unlisted(Invalid(InvalidReason::NotListed));
    };
    let Some(record) = state.keys.get(id) else {
        return unlisted(Invalid(InvalidReason::NotListed));
    };
    if record.added > trusted {
        // Listed only past the trusted seq: no store-signed head proves the listing yet.
        return unlisted(CantCheck);
    }
    let at = facts.anchor.unwrap_or(record.added);
    if at > trusted {
        return CantCheck;
    }
    if at < record.added {
        return Invalid(InvalidReason::NotYetValid);
    }
    let revoked = record.revoked.filter(|r| *r <= trusted);
    if revoked.is_some_and(|r| r <= at) {
        return Invalid(InvalidReason::Revoked);
    }
    if !state.key_valid_at_time(id, at, facts.now) {
        return Invalid(InvalidReason::Expired);
    }
    if revoked.is_some() && facts.entered_after_revoke == Some(true) {
        return Flagged;
    }
    Valid
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
    use ad4m_identity_core::{BotScope, KeyRecord};

    const NOW: u64 = 1_000;

    fn facts<'a>(log: SignerLog<'a>, signer: &'a str, anchor: Option<u64>) -> LinkFacts<'a> {
        LinkFacts {
            signature_holds: true,
            signer,
            anchor,
            log,
            entered_after_revoke: None,
            now: NOW,
        }
    }

    /// A log whose `d1` was added at 0 and, when `revoked`, revoked at that seq.
    fn log_with(revoked: Option<u64>) -> (KeyState, String, String) {
        let mut state = TestLog::new(41).chain().state().clone();
        let d1 = state.keys.get_mut("d1").expect("d1");
        d1.revoked = revoked;
        let device = d1.sign.clone();
        let master = state.masters[0].sign.clone();
        (state, device, master)
    }

    #[test]
    fn a_bad_signature_is_invalid_whatever_the_log() {
        let (state, device, _) = log_with(None);
        for log in [
            SignerLog::Listed {
                state: &state,
                trusted: 0,
            },
            SignerLog::NoLog,
            SignerLog::Unknown,
        ] {
            for anchor in [None, Some(0)] {
                let mut f = facts(log, &device, anchor);
                f.signature_holds = false;
                assert_eq!(judge(&f), LinkVerdict::Invalid(InvalidReason::BadSignature));
            }
        }
    }

    #[test]
    fn an_unanchored_link_without_a_log_is_valid_as_before() {
        assert_eq!(
            judge(&facts(SignerLog::NoLog, "z6MkAny", None)),
            LinkVerdict::Valid
        );
        assert_eq!(
            judge(&facts(SignerLog::Unknown, "z6MkAny", None)),
            LinkVerdict::Valid
        );
    }

    #[test]
    fn an_anchored_link_needs_a_log_that_lists_its_signer() {
        assert_eq!(
            judge(&facts(SignerLog::NoLog, "z6MkAny", Some(0))),
            LinkVerdict::Invalid(InvalidReason::NotListed)
        );
        assert_eq!(
            judge(&facts(SignerLog::Unknown, "z6MkAny", Some(0))),
            LinkVerdict::CantCheck
        );
        let (state, _, _) = log_with(None);
        let listed = SignerLog::Listed {
            state: &state,
            trusted: 0,
        };
        assert_eq!(
            judge(&facts(listed, "z6MkSomeoneElse", Some(0))),
            LinkVerdict::Invalid(InvalidReason::NotListed)
        );
    }

    #[test]
    fn a_master_key_never_signs_a_link() {
        let (state, _, master) = log_with(None);
        let listed = SignerLog::Listed {
            state: &state,
            trusted: 0,
        };
        for anchor in [None, Some(0)] {
            assert_eq!(
                judge(&facts(listed, &master, anchor)),
                LinkVerdict::Invalid(InvalidReason::MasterKey)
            );
        }
    }

    #[test]
    fn an_anchor_past_the_trusted_seq_cant_be_checked_yet() {
        let (state, device, _) = log_with(None);
        let listed = SignerLog::Listed {
            state: &state,
            trusted: 0,
        };
        assert_eq!(
            judge(&facts(listed, &device, Some(1))),
            LinkVerdict::CantCheck
        );
        assert_eq!(judge(&facts(listed, &device, Some(0))), LinkVerdict::Valid);
    }

    #[test]
    fn a_key_revoked_at_or_before_the_anchor_is_invalid() {
        let (state, device, _) = log_with(Some(1));
        let listed = SignerLog::Listed {
            state: &state,
            trusted: 2,
        };
        for anchor in [1, 2] {
            assert_eq!(
                judge(&facts(listed, &device, Some(anchor))),
                LinkVerdict::Invalid(InvalidReason::Revoked)
            );
        }
    }

    #[test]
    fn a_revocation_past_the_trusted_seq_does_not_count_yet() {
        let (state, device, _) = log_with(Some(1));
        let listed = SignerLog::Listed {
            state: &state,
            trusted: 0,
        };
        let mut f = facts(listed, &device, Some(0));
        f.entered_after_revoke = Some(true);
        assert_eq!(judge(&f), LinkVerdict::Valid);
    }

    #[test]
    fn a_link_signed_before_a_revocation_is_judged_by_the_space_order() {
        let (state, device, _) = log_with(Some(1));
        let listed = SignerLog::Listed {
            state: &state,
            trusted: 1,
        };
        let mut f = facts(listed, &device, Some(0));
        assert_eq!(judge(&f), LinkVerdict::Valid, "no order: valid");
        f.entered_after_revoke = Some(false);
        assert_eq!(judge(&f), LinkVerdict::Valid, "entered before the revoke");
        f.entered_after_revoke = Some(true);
        assert_eq!(judge(&f), LinkVerdict::Flagged, "entered after the revoke");
    }

    #[test]
    fn a_key_added_after_the_anchor_is_not_yet_valid() {
        let (mut state, device, _) = log_with(None);
        state.keys.get_mut("d1").expect("d1").added = 2;
        let listed = SignerLog::Listed {
            state: &state,
            trusted: 3,
        };
        assert_eq!(
            judge(&facts(listed, &device, Some(1))),
            LinkVerdict::Invalid(InvalidReason::NotYetValid)
        );
    }

    #[test]
    fn a_key_listed_only_past_the_trusted_seq_does_not_speak_yet() {
        let (mut state, device, _) = log_with(None);
        state.keys.get_mut("d1").expect("d1").added = 2;
        let listed = SignerLog::Listed {
            state: &state,
            trusted: 1,
        };
        assert_eq!(judge(&facts(listed, &device, None)), LinkVerdict::Valid);
        assert_eq!(
            judge(&facts(listed, &device, Some(1))),
            LinkVerdict::CantCheck
        );
    }

    #[test]
    fn an_expired_key_is_invalid_by_the_executor_clock() {
        let (mut state, device, _) = log_with(None);
        state.keys.get_mut("d1").expect("d1").expires = Some(NOW);
        let listed = SignerLog::Listed {
            state: &state,
            trusted: 0,
        };
        assert_eq!(
            judge(&facts(listed, &device, Some(0))),
            LinkVerdict::Invalid(InvalidReason::Expired)
        );
    }

    #[test]
    fn a_bot_key_counts_within_its_scope() {
        let (mut state, device, _) = log_with(None);
        let d1: &mut KeyRecord = state.keys.get_mut("d1").expect("d1");
        d1.scopes = vec![(
            0,
            BotScope {
                can: vec!["write".into()],
                expires: NOW + 1,
            },
        )];
        let listed = SignerLog::Listed {
            state: &state,
            trusted: 0,
        };
        assert_eq!(judge(&facts(listed, &device, Some(0))), LinkVerdict::Valid);
        let mut late = facts(listed, &device, Some(0));
        late.now = NOW + 1;
        assert_eq!(judge(&late), LinkVerdict::Invalid(InvalidReason::Expired));
    }

    /// A key that a log listed after it signed (a guest key claimed as `d1`): judged as
    /// anchored at the seq that added it.
    #[test]
    fn an_unanchored_link_by_a_listed_key_is_judged_at_the_seq_that_added_it() {
        let (state, device, _) = log_with(Some(1));
        let listed = SignerLog::Listed {
            state: &state,
            trusted: 1,
        };
        let mut f = facts(listed, &device, None);
        assert_eq!(judge(&f), LinkVerdict::Valid, "added at 0, revoked at 1");
        f.entered_after_revoke = Some(true);
        assert_eq!(judge(&f), LinkVerdict::Flagged);
    }

    #[test]
    fn stored_verdicts_read_back() {
        let all = [
            LinkVerdict::Valid,
            LinkVerdict::CantCheck,
            LinkVerdict::Flagged,
            LinkVerdict::Invalid(InvalidReason::BadSignature),
            LinkVerdict::Invalid(InvalidReason::NotListed),
            LinkVerdict::Invalid(InvalidReason::MasterKey),
            LinkVerdict::Invalid(InvalidReason::NotYetValid),
            LinkVerdict::Invalid(InvalidReason::Revoked),
            LinkVerdict::Invalid(InvalidReason::Expired),
        ];
        for v in all {
            assert_eq!(LinkVerdict::from_stored(&v.stored()), Some(v.clone()));
        }
        assert_eq!(LinkVerdict::from_stored("true"), None);
        assert_eq!(LinkVerdict::from_stored("invalid:other"), None);
    }

    #[test]
    fn logic_reads_count_only_valid_and_display_hides_only_invalid() {
        assert!(LinkVerdict::Valid.counts() && LinkVerdict::Valid.shown());
        for v in [LinkVerdict::CantCheck, LinkVerdict::Flagged] {
            assert!(!v.counts() && v.shown());
        }
        let invalid = LinkVerdict::Invalid(InvalidReason::Revoked);
        assert!(!invalid.counts() && !invalid.shown());
    }

    #[test]
    fn key_verdicts_read_the_log_up_to_its_trusted_seq() {
        let (state, device, _) = log_with(Some(1));
        assert_eq!(key_verdict(&state, 0, &device, NOW), KeyVerdict::Valid);
        assert_eq!(key_verdict(&state, 1, &device, NOW), KeyVerdict::Revoked);
        assert_eq!(
            key_verdict(&state, 1, "z6MkUnknown", NOW),
            KeyVerdict::NotInLog
        );
    }
}
