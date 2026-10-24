//! `fold` refuses each crafted case (the rules of section 5 of the format spec), and the typed
//! and JSON paths agree. Wrong `seq` and `prev` are the module's `ChainError`; the rest are
//! `ChainError::Policy(FoldError)`.
#![cfg(feature = "build")]

mod common;

use ad4m_identity_core::{
    error_code, event_hash, fold, fold_json, import, ChainError, DeviceKey, Event, FoldError,
    FormatError, ImportOutcome, KelPolicy, KeyLife,
};
use common::*;
use serde_json::json;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

fn refuse(name: &str) -> ChainError<FoldError> {
    let case = refusal_cases()
        .into_iter()
        .find(|c| c.name == name)
        .unwrap_or_else(|| panic!("no case {name}"));
    let err = fold_json(&case.events)
        .err()
        .unwrap_or_else(|| panic!("{name} folds"));
    assert_eq!(pinned(&error_code(&err)), case.error, "{name}");
    err
}

fn format_of(err: ChainError<FoldError>) -> FormatError {
    match err {
        ChainError::Policy(FoldError::Format(f)) => f,
        other => panic!("not a format error: {other:?}"),
    }
}

fn policy_of(err: ChainError<FoldError>) -> FoldError {
    match err {
        ChainError::Policy(f) => f,
        other => panic!("not a policy error: {other:?}"),
    }
}

#[test]
fn unknown_op() {
    assert_eq!(
        format_of(refuse("unknown op")),
        FormatError::UnknownOp("merge".into())
    );
}

#[test]
fn unknown_field() {
    assert_eq!(
        format_of(refuse("unknown field")),
        FormatError::UnknownField("name".into())
    );
}

#[test]
fn unknown_device_key_field() {
    assert_eq!(
        format_of(refuse("unknown device-key field")),
        FormatError::UnknownField("model".into())
    );
}

#[test]
fn float() {
    assert!(matches!(format_of(refuse("float")), FormatError::Float(_)));
}

#[test]
fn big_integer() {
    assert!(matches!(
        format_of(refuse("big integer")),
        FormatError::BigInteger(_)
    ));
}

#[test]
fn group_kind() {
    assert_eq!(
        format_of(refuse("group")),
        FormatError::UnsupportedKind("group".into())
    );
}

#[test]
fn other_format() {
    assert_eq!(
        format_of(refuse("other format")),
        FormatError::Version("coasys/2".into())
    );
}

#[test]
fn empty_proof_array() {
    assert!(matches!(
        format_of(refuse("empty proof array")),
        FormatError::NotCanonical(_)
    ));
}

#[test]
fn can_without_bot_mark() {
    assert_eq!(
        policy_of(refuse("can without bot mark")),
        FoldError::CanWithoutBotMark
    );
}

#[test]
fn wrong_prev_is_the_modules() {
    assert!(matches!(refuse("wrong prev"), ChainError::Prev { .. }));
}

#[test]
fn wrong_seq_is_the_modules() {
    assert_eq!(refuse("wrong seq"), ChainError::Seq { want: 1, got: 2 });
}

#[test]
fn other_scid_is_wrong_chain() {
    assert!(matches!(
        refuse("other scid"),
        ChainError::WrongChain { .. }
    ));
}

#[test]
fn scid_not_derived() {
    assert!(matches!(
        format_of(refuse("scid not derived")),
        FormatError::Scid { .. }
    ));
}

#[test]
fn first_event_not_an_incept() {
    assert_eq!(
        format_of(refuse("first event not an incept")),
        FormatError::NotIncept
    );
}

#[test]
fn incept_after_genesis() {
    assert_eq!(
        format_of(refuse("incept after #0")),
        FormatError::InceptAfterGenesis
    );
}

#[test]
fn out_of_order_key_id() {
    assert_eq!(
        policy_of(refuse("out-of-order key id")),
        FoldError::KeyIdOrder {
            want: "d1".into(),
            got: "d2".into()
        }
    );
    // In an add too: the next id after d1 is d2.
    let log = Log::new();
    let mut event =
        ad4m_identity_core::build::add_unsigned(&log.chain, device(3, KeyLife::Standing), "o", "c")
            .unwrap();
    sign(&mut event, "m0", &master(0));
    assert_eq!(
        log.try_push(&event),
        Err(ChainError::Policy(FoldError::KeyIdOrder {
            want: "d2".into(),
            got: "d3".into()
        }))
    );
}

#[test]
fn duplicate_key() {
    assert_eq!(
        policy_of(refuse("duplicate key")),
        FoldError::DuplicateKey(mk(&master(0)))
    );
    // A revoked key cannot come back either.
    let mut log = Log::new();
    log.revoke(&["d1"]);
    let mut event = ad4m_identity_core::build::add_unsigned(
        &log.chain,
        DeviceKey {
            id: "d2".into(),
            ..device(1, KeyLife::Standing)
        },
        "o",
        "c",
    )
    .unwrap();
    sign(&mut event, "m0", &master(0));
    assert_eq!(
        log.try_push(&event),
        Err(ChainError::Policy(FoldError::DuplicateKey(mk(&dev(1)))))
    );
}

#[test]
fn bot_in_incept() {
    assert_eq!(policy_of(refuse("bot in incept")), FoldError::BotInIncept);
}

#[test]
fn add_with_two_keys() {
    assert_eq!(policy_of(refuse("add with two keys")), FoldError::AddNotOne);
}

#[test]
fn bot_with_no_action() {
    assert_eq!(
        policy_of(refuse("bot with no action")),
        FoldError::EmptyScope
    );
}

#[test]
fn bot_with_an_unknown_action() {
    assert_eq!(
        format_of(refuse("bot with an unknown action")),
        FormatError::UnknownAction("admin".into())
    );
}

#[test]
fn unsupported_key_types() {
    assert_eq!(
        policy_of(refuse("X25519 key as a signing key")),
        FoldError::UnsupportedKeyType
    );
    assert_eq!(
        policy_of(refuse("P-256 passkey key")),
        FoldError::UnsupportedKeyType
    );
}

#[test]
fn incept_without_the_devices_proof() {
    assert_eq!(
        policy_of(refuse("incept without the device's proof")),
        FoldError::MissingProof("d1".into())
    );
}

#[test]
fn add_signed_by_a_device_key() {
    assert_eq!(
        policy_of(refuse("add signed by a device key")),
        FoldError::MissingProof("m0".into())
    );
}

#[test]
fn forged_master_proof() {
    assert_eq!(
        policy_of(refuse("add with a forged master proof")),
        FoldError::BadProof("m0".into())
    );
}

#[test]
fn proof_in_another_domain() {
    assert_eq!(
        policy_of(refuse("proof in another domain")),
        FoldError::BadProof("m0".into())
    );
}

#[test]
fn extra_proof_by_a_device_key() {
    assert_eq!(
        policy_of(refuse("an extra proof by a device key")),
        FoldError::BadProof("d1".into())
    );
}

#[test]
fn revoke_of_an_unknown_key() {
    assert_eq!(
        policy_of(refuse("revoke of an unknown key")),
        FoldError::UnknownKey("d9".into())
    );
}

#[test]
fn revoke_twice() {
    assert_eq!(
        policy_of(refuse("revoke twice")),
        FoldError::RevokedKey("d1".into())
    );
}

#[test]
fn scope_on_a_key_without_the_bot_mark() {
    assert_eq!(
        policy_of(refuse("scope on a key without the bot mark")),
        FoldError::NotABot("d1".into())
    );
}

#[test]
fn rotate_with_a_wrong_salt() {
    assert_eq!(
        policy_of(refuse("rotate with a wrong salt")),
        FoldError::NextMismatch
    );
}

#[test]
fn event_after_the_end() {
    assert_eq!(policy_of(refuse("event after the end")), FoldError::Ended);
}

#[test]
fn every_case_has_a_test_above() {
    // Keeps the list and the tests in step: each case is refused by name in this file.
    let source = include_str!("fold.rs");
    for case in refusal_cases() {
        assert!(
            source.contains(&format!("refuse(\"{}\")", case.name)),
            "no test for {}",
            case.name
        );
    }
}

#[test]
fn empty_log() {
    assert_eq!(fold(&[]).unwrap_err(), ChainError::Empty);
}

#[test]
fn typed_big_integer_has_no_canonical_form() {
    // A typed event cannot carry a float, but it can carry a u64 above 2^53 − 1: the module
    // refuses it before the policy runs.
    let mut event = incept_event(vec![device(1, KeyLife::Standing)]);
    if let ad4m_identity_core::Op::Incept { add, .. } = &mut event.op {
        add[0].life = KeyLife::Until(u64::MAX);
    }
    assert!(event_hash(&event).is_err());
    assert!(ad4m_identity_core::derive_scid(&event).is_err());
    assert!(matches!(fold(&[event]), Err(ChainError::Malformed(_))));
    assert!(ad4m_identity_core::build::incept_unsigned(
        &mk(&master(0)),
        next(1),
        vec![device(1, KeyLife::Until(1 << 53))]
    )
    .is_err());
}

#[test]
fn serde_parsing_is_strict() {
    let log = Log::new();
    let mut value = json(&log.events[0]);
    value["extra"] = json!(1);
    assert!(serde_json::from_value::<Event>(value).is_err());
    let text = serde_json::to_string(&log.events[0]).unwrap();
    let back: Event = serde_json::from_str(&text).unwrap();
    assert_eq!(back, log.events[0]);
    // A rotate leaves out an empty revoke, and refuses an explicit one.
    let mut log = Log::new();
    log.rotate(&[], false);
    let mut rotate = json(&log.events[1]);
    assert!(rotate.get("revoke").is_none());
    rotate["revoke"] = json!([]);
    assert!(matches!(
        Event::from_json(&rotate),
        Err(FoldError::Format(FormatError::NotCanonical(_)))
    ));
}

#[test]
fn device_key_round_trips_its_three_flat_forms() {
    let forms = [
        (
            KeyLife::Standing,
            json!({"id": "d1", "sign": mk(&dev(1)), "enc": enc("d1")}),
        ),
        (
            KeyLife::Until(1_800_000_000),
            json!({"id": "d1", "sign": mk(&dev(1)), "enc": enc("d1"), "expires": 1_800_000_000u64}),
        ),
        (
            bot(&["read", "join"], 1_700_000_000),
            json!({"id": "d1", "sign": mk(&dev(1)), "enc": enc("d1"), "kind": "bot",
                   "can": ["read", "join"], "expires": 1_700_000_000u64}),
        ),
    ];
    for (life, wire) in forms {
        let key = device(1, life);
        assert_eq!(serde_json::to_value(&key).unwrap(), wire);
        assert_eq!(serde_json::from_value::<DeviceKey>(wire).unwrap(), key);
    }
    let can_no_kind = json!({"id": "d1", "sign": mk(&dev(1)), "enc": enc("d1"),
                             "can": ["read"], "expires": 5});
    assert!(serde_json::from_value::<DeviceKey>(can_no_kind).is_err());
    let bot_no_expiry = json!({"id": "d1", "sign": mk(&dev(1)), "enc": enc("d1"),
                               "kind": "bot", "can": ["read"]});
    assert!(serde_json::from_value::<DeviceKey>(bot_no_expiry).is_err());
}

#[test]
fn incept_with_an_until_key_passes() {
    let log = Log::incept(vec![
        device(1, KeyLife::Standing),
        device(2, KeyLife::Until(1_800_000_000)),
    ]);
    assert_eq!(log.chain.state().keys["d2"].expires, Some(1_800_000_000));
}

#[test]
fn append_one_by_one_equals_fold() {
    let log = vector_log();
    let folded = fold(&log.events).unwrap();
    let mut chain = fold(&log.events[..1]).unwrap();
    for event in &log.events[1..] {
        chain.append(&KelPolicy, event).unwrap();
    }
    assert_eq!(chain.state(), folded.state());
    assert_eq!(chain.head(), folded.head());
    assert_eq!(chain.head(), log.chain.head());
    // And JSON and typed folds agree.
    let from_json = fold_json(&log.json()).unwrap();
    assert_eq!(from_json.state(), folded.state());
    assert_eq!(from_json.hashes(), folded.hashes());
}

#[test]
fn import_appends_detects_gaps_and_forks() {
    let log = vector_log();
    let mut chain = fold(&log.events[..2]).unwrap();
    assert_eq!(
        import(&mut chain, &log.events[4..]).unwrap(),
        ImportOutcome::Gap { have: 1, first: 4 }
    );
    let mut shuffled: Vec<Event> = log.events[2..].to_vec();
    shuffled.reverse();
    match import(&mut chain, &shuffled).unwrap() {
        ImportOutcome::Appended { head, count } => {
            assert_eq!(head, log.chain.head());
            assert_eq!(count, 5);
        }
        other => panic!("{other:?}"),
    }
    assert_eq!(
        import(&mut chain, &log.events).unwrap(),
        ImportOutcome::AlreadyHave
    );
    // A second valid event at seq 1 is a fork, and nothing changes.
    let mut short = Log::new();
    let base = short.chain.clone();
    short.add(KeyLife::Standing);
    let mut other = base.clone();
    let rival = {
        let mut l = Log {
            events: vec![short.events[0].clone()],
            chain: base,
            n: 0,
        };
        l.revoke(&["d1"]);
        l.events[1].clone()
    };
    other.append(&KelPolicy, &rival).unwrap();
    let before = short.chain.head();
    match import(&mut short.chain, &[rival]).unwrap() {
        ImportOutcome::Fork(fork) => {
            assert_eq!(fork.seq, 1);
            assert_eq!(fork.ours, before.hash);
            assert_eq!(&fork.theirs, other.hash_at(1).unwrap());
        }
        other => panic!("{other:?}"),
    }
    assert_eq!(short.chain.head(), before);
}
