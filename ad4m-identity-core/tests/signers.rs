//! Who signs (AR-01 rule 4, R7, R8): `add`, `revoke` and `scope` by the current master only;
//! `rotate` by the revealed key with the committed salt; master *n* fails after the rotation.
#![cfg(feature = "build")]

mod common;

use ad4m_identity_core::build::{self, BuildError};
use ad4m_identity_core::{BotScope, ChainError, Event, FoldError, KeyLife};
use common::*;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

/// A log at master m1 (one rotation) with a standing device d1 and a bot d2.
fn rotated() -> Log {
    let mut log = Log::new();
    log.add(bot(&["read"], 2_000_000_000));
    log.rotate(&[], false);
    log
}

/// The three master-signed events after the head of `log`, unsigned.
fn master_events(log: &Log) -> Vec<Event> {
    let id = log.chain.state().next_device_id();
    let n: u32 = id[1..].parse().unwrap();
    vec![
        build::add_unsigned(&log.chain, device(n, KeyLife::Standing), "o", "c").unwrap(),
        build::revoke_unsigned(&log.chain, &["d1".to_string()]).unwrap(),
        build::scope_unsigned(
            &log.chain,
            &"d2".to_string(),
            &BotScope {
                can: vec!["write".into()],
                expires: 2_100_000_000,
            },
        )
        .unwrap(),
    ]
}

#[test]
fn current_master_passes() {
    let log = rotated();
    for mut event in master_events(&log) {
        sign(&mut event, "m1", &master(1));
        log.try_push(&event).unwrap();
    }
}

#[test]
fn device_key_fails() {
    let log = rotated();
    for mut event in master_events(&log) {
        // As itself: the master's proof is missing.
        let mut as_device = event.clone();
        sign(&mut as_device, "d1", &dev(1));
        assert_eq!(
            log.try_push(&as_device),
            Err(ChainError::Policy(FoldError::MissingProof("m1".into())))
        );
        // Posing as the master: the proof does not verify.
        sign(&mut event, "m1", &dev(1));
        assert_eq!(
            log.try_push(&event),
            Err(ChainError::Policy(FoldError::BadProof("m1".into())))
        );
    }
}

#[test]
fn earlier_master_fails() {
    let log = rotated();
    for event in master_events(&log) {
        let mut as_m0 = event.clone();
        sign(&mut as_m0, "m0", &master(0));
        assert_eq!(
            log.try_push(&as_m0),
            Err(ChainError::Policy(FoldError::MissingProof("m1".into())))
        );
        let mut posing = event;
        sign(&mut posing, "m1", &master(0));
        assert_eq!(
            log.try_push(&posing),
            Err(ChainError::Policy(FoldError::BadProof("m1".into())))
        );
    }
    // And the builder will not sign with it.
    assert_eq!(
        build::revoke(&log.chain, &master(0), &["d1".to_string()]).unwrap_err(),
        BuildError::NotCurrentMaster
    );
}

#[test]
fn rotate_needs_the_revealed_key_and_its_salt() {
    let log = Log::new();
    // Wrong salt.
    let mut wrong =
        build::rotate_unsigned(&log.chain, &mk(&master(1)), &salt(9), Some(next(2)), &[]).unwrap();
    sign(&mut wrong, "m1", &master(1));
    assert_eq!(
        log.try_push(&wrong),
        Err(ChainError::Policy(FoldError::NextMismatch))
    );
    // A key that was never committed.
    let mut other =
        build::rotate_unsigned(&log.chain, &mk(&master(5)), &salt(1), Some(next(2)), &[]).unwrap();
    sign(&mut other, "m1", &master(5));
    assert_eq!(
        log.try_push(&other),
        Err(ChainError::Policy(FoldError::NextMismatch))
    );
    // The right key and salt, signed by the current master instead of the revealed key.
    let mut by_m0 =
        build::rotate_unsigned(&log.chain, &mk(&master(1)), &salt(1), Some(next(2)), &[]).unwrap();
    sign(&mut by_m0, "m0", &master(0));
    assert_eq!(
        log.try_push(&by_m0),
        Err(ChainError::Policy(FoldError::MissingProof("m1".into())))
    );
    // Signed by a device key as m1.
    let mut by_device =
        build::rotate_unsigned(&log.chain, &mk(&master(1)), &salt(1), Some(next(2)), &[]).unwrap();
    sign(&mut by_device, "m1", &dev(1));
    assert_eq!(
        log.try_push(&by_device),
        Err(ChainError::Policy(FoldError::BadProof("m1".into())))
    );
    // The builder refuses a key and salt that do not match.
    assert_eq!(
        build::rotate(&log.chain, &master(1), &salt(2), Some(next(2)), &[]).unwrap_err(),
        BuildError::NextMismatch
    );
    // The right one passes.
    log.try_push(&log.rotate_event(&[], false)).unwrap();
}

#[test]
fn master_n_fails_from_the_rotation_on() {
    let mut log = Log::new();
    log.add(KeyLife::Standing); // seq 1
    log.rotate(&[], false); // seq 2: m0 → m1
    let state = log.chain.state();
    assert!(state.key_valid_at("m0", 1));
    assert!(!state.key_valid_at("m0", 2));
    assert!(!state.key_valid_at(&mk(&master(0)), 2));
    assert!(state.key_valid_at("m1", 2));
    assert!(!state.key_valid_at("m1", 1));
    assert_eq!(state.current_master().unwrap().id, "m1");
    assert_eq!(state.masters[0].revoked, Some(2));
}

#[test]
fn rotate_with_revoke_ends_those_keys() {
    let mut log = Log::incept(vec![
        device(1, KeyLife::Standing),
        device(2, KeyLife::Standing),
    ]);
    log.rotate(&["d1"], false);
    let state = log.chain.state();
    assert!(!state.key_valid_at("d1", 1));
    assert!(state.key_valid_at("d1", 0));
    assert!(state.key_valid_at("d2", 1));
    // A master id is not a device key a rotate may revoke.
    let mut bad = build::rotate_unsigned(
        &log.chain,
        &mk(&master(2)),
        &salt(2),
        Some(next(3)),
        &["m0".to_string()],
    )
    .unwrap();
    sign(&mut bad, "m2", &master(2));
    assert_eq!(
        log.try_push(&bad),
        Err(ChainError::Policy(FoldError::UnknownKey("m0".into())))
    );
}

#[test]
fn a_revealed_key_already_in_the_log_fails() {
    // A commitment to a device key that is already in the log.
    let mut event = build::incept_unsigned(
        &mk(&master(0)),
        ad4m_identity_core::commitment(&mk(&dev(1)), &salt(1)),
        vec![device(1, KeyLife::Standing)],
    )
    .unwrap();
    sign(&mut event, "m0", &master(0));
    sign(&mut event, "d1", &dev(1));
    let chain = ad4m_identity_core::fold(std::slice::from_ref(&event)).unwrap();
    let log = Log {
        events: vec![event],
        chain,
        n: 0,
    };
    let mut rotate =
        build::rotate_unsigned(&log.chain, &mk(&dev(1)), &salt(1), Some(next(2)), &[]).unwrap();
    sign(&mut rotate, "m1", &dev(1));
    assert_eq!(
        log.try_push(&rotate),
        Err(ChainError::Policy(FoldError::DuplicateKey(mk(&dev(1)))))
    );
}

#[test]
fn signing_input_and_attach_match_the_signer_form() {
    let log = Log::new();
    let mut by_hand = build::revoke_unsigned(&log.chain, &["d1".to_string()]).unwrap();
    let input = build::signing_input(&by_hand, "m0").unwrap();
    let signature = ed25519_dalek::Signer::sign(&master(0), &input).to_bytes();
    build::attach_proof(&mut by_hand, "m0", &signature).unwrap();
    let by_signer = build::revoke(&log.chain, &master(0), &["d1".to_string()]).unwrap();
    assert_eq!(by_hand, by_signer);
    assert_eq!(
        build::attach_proof(&mut by_hand, "m0", &signature).unwrap_err(),
        BuildError::AlreadySigned("m0".into())
    );
    log.try_push(&by_signer).unwrap();
}
