//! The link judgement (`judgement::judge`) and the bytes a link signs, against
//! `vectors/link-signing-bytes.json`.
#![cfg(feature = "build")]

mod common;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

use ad4m_identity_core::judgement::*;
use ad4m_identity_core::{BotScope, KeyRecord, KeyState};
use common::*;
use ed25519_dalek::Signer;
use serde_json::Value;

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
    let mut state = Log::new().chain.state().clone();
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
    assert_eq!(
        judge(&f),
        LinkVerdict::Flagged,
        "no order: the holder of the revoked key could have signed it today"
    );
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

/// A link is a write: a bot key whose scope at the anchor lacks `write` authors nothing,
/// anchored or not, whatever else its scope allows.
#[test]
fn a_bot_key_without_write_in_its_scope_authors_no_link() {
    for can in [vec!["read"], vec!["join"], vec!["read", "join"], vec![]] {
        let (mut state, device, _) = log_with(None);
        state.keys.get_mut("d1").expect("d1").scopes = vec![(
            0,
            BotScope {
                can: can.iter().map(|a| a.to_string()).collect(),
                expires: NOW + 1,
            },
        )];
        let listed = SignerLog::Listed {
            state: &state,
            trusted: 0,
        };
        for anchor in [Some(0), None] {
            assert_eq!(
                judge(&facts(listed, &device, anchor)),
                LinkVerdict::Invalid(InvalidReason::NotInScope),
                "can {can:?}, anchor {anchor:?}"
            );
        }
    }
}

/// The scope in force at the anchor decides: a `scope` that adds `write` later makes a link
/// anchored after it count, and one anchored before it still does not.
#[test]
fn the_scope_at_the_anchor_decides_whether_a_bot_may_write() {
    let (mut state, device, _) = log_with(None);
    state.keys.get_mut("d1").expect("d1").scopes = vec![
        (
            0,
            BotScope {
                can: vec!["read".into()],
                expires: NOW + 1,
            },
        ),
        (
            1,
            BotScope {
                can: vec!["read".into(), "write".into()],
                expires: NOW + 1,
            },
        ),
    ];
    let listed = SignerLog::Listed {
        state: &state,
        trusted: 1,
    };
    assert_eq!(
        judge(&facts(listed, &device, Some(0))),
        LinkVerdict::Invalid(InvalidReason::NotInScope)
    );
    assert_eq!(judge(&facts(listed, &device, Some(1))), LinkVerdict::Valid);
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
    assert_eq!(
        judge(&f),
        LinkVerdict::Flagged,
        "added at 0, revoked at 1, no order"
    );
    f.entered_after_revoke = Some(false);
    assert_eq!(judge(&f), LinkVerdict::Valid, "entered before the revoke");
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
        LinkVerdict::Invalid(InvalidReason::NotInScope),
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

/// The verdict's JSON is the SDK's `LinkVerdict`: `{kind}`, and `{kind, reason}` for `invalid`.
#[test]
fn the_verdict_json_is_kind_and_reason() {
    assert_eq!(
        serde_json::to_value(LinkVerdict::Invalid(InvalidReason::NotInScope)).unwrap(),
        serde_json::json!({"kind": "invalid", "reason": "notInScope"})
    );
    assert_eq!(
        serde_json::to_value(LinkVerdict::CantCheck).unwrap(),
        serde_json::json!({"kind": "cantCheck"})
    );
}

fn signing_vectors() -> Vec<Value> {
    let file: Value =
        serde_json::from_str(include_str!("../vectors/link-signing-bytes.json")).unwrap();
    file["links"].as_array().unwrap().clone()
}

#[test]
fn the_signed_bytes_of_a_link_are_the_vectors() {
    let links = signing_vectors();
    assert_eq!(links.len(), 5);
    assert_eq!(
        links.iter().filter(|l| l.get("anchor").is_some()).count(),
        2,
        "with and without anchors"
    );
    for case in links {
        let timestamp = case["timestamp"].as_str().unwrap();
        let bytes = match case["anchor"].as_u64() {
            Some(anchor) => anchored_signing_bytes(&case["data"], timestamp, anchor).unwrap(),
            None => hash_data_and_timestamp(&case["data"], timestamp)
                .unwrap()
                .to_vec(),
        };
        assert_eq!(hex(&bytes), case["bytes"].as_str().unwrap(), "{case}");
    }
}

/// The old bytes read the timestamp as a date: every form of one instant signs the same bytes.
#[test]
fn the_unanchored_bytes_read_the_timestamp_as_a_date() {
    let data = serde_json::json!({"predicate": null, "source": "a", "target": "b"});
    let one = hash_data_and_timestamp(&data, "2026-11-14T10:00:00.000Z").unwrap();
    for same in [
        "2026-11-14T10:00:00Z",
        "2026-11-14T12:00:00+02:00",
        "2026-11-14T10:00:00.000400Z",
    ] {
        assert_eq!(hash_data_and_timestamp(&data, same).unwrap(), one, "{same}");
    }
    assert!(matches!(
        hash_data_and_timestamp(&data, "not a date"),
        Err(LinkBytesError::Timestamp(_))
    ));
}

#[test]
fn an_anchored_signature_holds_only_over_its_own_bytes() {
    let key = dev(1);
    let data = serde_json::json!({"predicate": "p", "source": "a", "target": "b"});
    let timestamp = "2026-11-14T10:00:00.000Z";
    let bytes = anchored_signing_bytes(&data, timestamp, 3).unwrap();
    let signature = key.sign(&bytes).to_bytes();
    let signer = mk(&key);
    assert!(anchored_signature_holds(
        &data, timestamp, 3, &signer, &signature
    ));
    assert!(!anchored_signature_holds(
        &data, timestamp, 4, &signer, &signature
    ));
    assert!(!anchored_signature_holds(
        &data,
        "2026-11-14T10:00:00Z",
        3,
        &signer,
        &signature
    ));
    assert!(!anchored_signature_holds(
        &data,
        timestamp,
        3,
        &mk(&dev(2)),
        &signature
    ));
}
