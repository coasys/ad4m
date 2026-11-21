//! `unlink` and the `since` of `revoke` (format spec rules K16 and K17): who may sign an
//! unlink, which keys it names, how it reads in the key state, and the strict JSON forms.
#![cfg(feature = "build")]

mod common;

use ad4m_identity_core::build::{self, BuildError};
use ad4m_identity_core::{ChainError, Event, FoldError, KeyLife, Op};
use common::*;
use serde_json::json;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

/// A log with devices d1 and d2 (seq 1 after the add).
fn two_devices() -> Log {
    let mut log = Log::new();
    log.add(KeyLife::Standing);
    log
}

fn refused(log: &Log, event: &Event) -> FoldError {
    match log.try_push(event) {
        Err(ChainError::Policy(e)) => e,
        other => panic!("expected a policy refusal, got {other:?}"),
    }
}

#[test]
fn the_master_unlinks_a_device_and_the_key_is_valid_only_below_the_unlink() {
    let mut log = two_devices();
    log.unlink("d1");
    let state = log.chain.state();
    assert_eq!(state.keys["d1"].unlinked, Some(2));
    assert_eq!(state.keys["d1"].revoked, None);
    assert!(state.key_valid_at("d1", 1));
    assert!(!state.key_valid_at("d1", 2));
    assert!(!state.key_valid_at("d1", 9));
    // The other device and the master are untouched.
    assert!(state.key_valid_at("d2", 9));
    assert!(state.current_master().is_some());
    // An unlinked key is no longer a sealing target.
    assert!(state
        .encryption_keys_at(2)
        .iter()
        .all(|(id, _)| *id != "d1"));
    assert!(state
        .encryption_keys_at(1)
        .iter()
        .any(|(id, _)| *id == "d1"));
}

#[test]
fn a_device_unlinks_itself_with_its_own_signature() {
    let mut log = two_devices();
    let event = build::unlink_self(&log.chain, &"d2".to_string(), &dev(2)).unwrap();
    assert_eq!(event.proof.len(), 1);
    log.push(event);
    assert_eq!(log.chain.state().keys["d2"].unlinked, Some(2));
}

#[test]
fn a_third_device_cannot_unlink_another_without_the_master() {
    let mut log = two_devices();
    log.add(KeyLife::Standing);
    // d3 signs the unlink of d1.
    let mut by_d3 = build::unlink_unsigned(&log.chain, &"d1".to_string()).unwrap();
    sign(&mut by_d3, "d3", &dev(3));
    assert_eq!(
        refused(&log, &by_d3),
        FoldError::MissingProof("m0".into()),
        "only the master or the key itself signs"
    );
    // d1's own proof on the unlink of d2 is no better.
    let mut by_d1 = build::unlink_unsigned(&log.chain, &"d2".to_string()).unwrap();
    sign(&mut by_d1, "d1", &dev(1));
    assert!(matches!(refused(&log, &by_d1), FoldError::MissingProof(_)));
    // And the builder refuses a signer that is not the key.
    assert_eq!(
        build::unlink_self(&log.chain, &"d1".to_string(), &dev(3)).unwrap_err(),
        BuildError::NotTheKey
    );
}

#[test]
fn one_signer_only() {
    let log = two_devices();
    let mut both = build::unlink_unsigned(&log.chain, &"d1".to_string()).unwrap();
    sign(&mut both, "d1", &dev(1));
    sign(&mut both, "m0", &master(0));
    assert_eq!(refused(&log, &both), FoldError::BadProof("m0".into()));
    // A forged proof under d1's id.
    let mut forged = build::unlink_unsigned(&log.chain, &"d1".to_string()).unwrap();
    sign(&mut forged, "d1", &dev(7));
    assert_eq!(refused(&log, &forged), FoldError::BadProof("d1".into()));
}

#[test]
fn an_unlink_names_one_active_device_key() {
    let mut log = two_devices();
    // A master, an unknown key.
    for id in ["m0", "d9"] {
        let event = build::unlink(&log.chain, &master(0), &id.to_string()).unwrap();
        assert_eq!(refused(&log, &event), FoldError::UnknownKey(id.into()));
    }
    // Twice.
    log.unlink("d1");
    let again = build::unlink(&log.chain, &master(0), &"d1".to_string()).unwrap();
    assert_eq!(refused(&log, &again), FoldError::UnlinkedKey("d1".into()));
    // After a revoke.
    log.revoke(&["d2"]);
    let late = build::unlink(&log.chain, &master(0), &"d2".to_string()).unwrap();
    assert_eq!(refused(&log, &late), FoldError::RevokedKey("d2".into()));
    // A bot key is a key like the others.
    let bot_id = log.add(bot(&["read"], 2_000_000_000));
    log.unlink(&bot_id);
    assert!(log.chain.state().keys[&bot_id].unlinked.is_some());
}

#[test]
fn a_revoke_may_follow_an_unlink_once() {
    let mut log = two_devices();
    log.unlink("d1");
    log.revoke(&["d1"]);
    let record = &log.chain.state().keys["d1"];
    assert_eq!((record.unlinked, record.revoked), (Some(2), Some(3)));
    // The key stops at the unlink, not later.
    assert!(!log.chain.state().key_valid_at("d1", 2));
    assert!(log.chain.state().key_valid_at("d1", 1));
    let twice = build::revoke(&log.chain, &master(0), &["d1".to_string()], None).unwrap();
    assert_eq!(refused(&log, &twice), FoldError::RevokedKey("d1".into()));
}

#[test]
fn a_rotate_may_revoke_an_unlinked_key() {
    let mut log = two_devices();
    log.unlink("d1");
    log.rotate(&["d1"], false);
    assert_eq!(log.chain.state().keys["d1"].revoked, Some(3));
}

#[test]
fn a_scope_of_an_unlinked_bot_is_refused() {
    let mut log = Log::new();
    let id = log.add(bot(&["read"], 2_000_000_000));
    log.unlink(&id);
    let scope = build::scope(
        &log.chain,
        &master(0),
        &id,
        &ad4m_identity_core::BotScope {
            can: vec!["write".into()],
            expires: 2_100_000_000,
        },
    )
    .unwrap();
    assert_eq!(refused(&log, &scope), FoldError::UnlinkedKey(id));
}

#[test]
fn an_unlink_needs_a_live_log() {
    let mut log = Log::new();
    log.rotate(&[], true);
    assert_eq!(
        build::unlink_unsigned(&log.chain, &"d1".to_string()).unwrap_err(),
        BuildError::Ended
    );
}

#[test]
fn unlink_and_since_have_one_json_form() {
    let mut log = two_devices();
    let with_since = build::revoke(
        &log.chain,
        &master(0),
        &["d1".to_string()],
        Some(1_700_000_000_000),
    )
    .unwrap();
    let value = json(&with_since);
    assert_eq!(value["since"], json!(1_700_000_000_000u64));
    assert_eq!(Event::from_json(&value).unwrap(), with_since);
    log.push(with_since);
    assert_eq!(
        log.chain.state().keys["d1"].revoked_since,
        Some(1_700_000_000_000)
    );

    let plain = build::revoke(&log.chain, &master(0), &["d2".to_string()], None).unwrap();
    assert!(
        json(&plain).get("since").is_none(),
        "an absent since is left out"
    );

    let unlink = build::unlink(&log.chain, &master(0), &"d2".to_string()).unwrap();
    let value = json(&unlink);
    assert_eq!(value["op"], "unlink");
    assert_eq!(value["unlink"], "d2");
    assert_eq!(Event::from_json(&value).unwrap(), unlink);
    assert!(matches!(unlink.op, Op::Unlink { .. }));
}

#[test]
fn since_and_unlink_parse_strictly() {
    let log = two_devices();
    let revoke = json(&build::revoke(&log.chain, &master(0), &["d1".to_string()], None).unwrap());
    let unlink = json(&build::unlink(&log.chain, &master(0), &"d1".to_string()).unwrap());
    let edit = |base: &serde_json::Value, f: &dyn Fn(&mut serde_json::Value)| {
        let mut v = base.clone();
        f(&mut v);
        Event::from_json(&v).unwrap_err()
    };
    // `since` must be an integer: not null, a string, a float, 2^53.
    for bad in [
        json!(null),
        json!("1"),
        json!(1.5),
        json!(9_007_199_254_740_992u64),
    ] {
        edit(&revoke, &|v| v["since"] = bad.clone());
    }
    // A revoke does not take `unlink`, an unlink does not take `since` or a list.
    edit(&revoke, &|v| v["unlink"] = json!("d1"));
    edit(&unlink, &|v| v["since"] = json!(1));
    edit(&unlink, &|v| v["unlink"] = json!(["d1"]));
    edit(&unlink, &|v| {
        v.as_object_mut().unwrap().remove("unlink");
    });
}
