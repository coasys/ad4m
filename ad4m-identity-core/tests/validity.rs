//! Key validity over seqs and time (section 6 of the format spec).
#![cfg(feature = "build")]

mod common;

use ad4m_identity_core::build;
use ad4m_identity_core::{BotScope, ChainError, FoldError, KeyLife};
use common::*;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

#[test]
fn nothing_applies_after_an_ending_rotate() {
    let mut log = Log::new();
    log.add(bot(&["read"], 2_000_000_000)); // d2, seq 1
    log.rotate(&[], true); // seq 2: m1, end
    let state = log.chain.state();
    assert!(state.ended);
    assert_eq!(state.next, None);
    assert!(state.current_master().is_none());
    for key in ["m0", "m1", "d1", "d2"] {
        assert!(!state.key_valid_at(key, 2), "{key} at the end");
        assert!(!state.key_valid_at(key, 3), "{key} after the end");
    }
    assert!(state.key_valid_at("d1", 1));
    assert!(state.key_valid_at("m0", 1));
    // Every op after the end is refused, whoever signs.
    let head = log.chain.head();
    let ops = [
        ad4m_identity_core::Op::Revoke {
            revoke: vec!["d1".into()],
        },
        ad4m_identity_core::Op::Add {
            add: vec![device(3, KeyLife::Standing)],
            offer: "o".into(),
            co_sig: "c".into(),
        },
        ad4m_identity_core::Op::Scope {
            key: "d2".into(),
            can: vec!["read".into()],
            expires: 3_000_000_000,
        },
        ad4m_identity_core::Op::Rotate {
            master: mk(&master(2)),
            salt: salt(2),
            next: Some(next(3)),
            revoke: vec![],
        },
    ];
    for op in ops {
        let mut event = ad4m_identity_core::Event {
            format: ad4m_identity_core::FORMAT.into(),
            scid: log.chain.id().clone(),
            seq: head.seq + 1,
            prev: Some(head.hash.clone()),
            op,
            proof: vec![],
        };
        sign(&mut event, "m1", &master(1));
        assert_eq!(
            log.try_push(&event),
            Err(ChainError::Policy(FoldError::Ended))
        );
    }
}

#[test]
fn revoked_key_valid_before_and_invalid_from_its_revocation() {
    let mut log = Log::new();
    log.add(KeyLife::Standing); // d2 at 1
    log.add(KeyLife::Standing); // d3 at 2
    log.revoke(&["d2", "d3"]); // at 3
    let state = log.chain.state();
    assert!(!state.key_valid_at("d2", 0));
    assert!(state.key_valid_at("d2", 1));
    assert!(state.key_valid_at("d2", 2));
    assert!(!state.key_valid_at("d2", 3));
    assert!(!state.key_valid_at(&mk(&dev(3)), 3));
    assert!(state.key_valid_at(&mk(&dev(3)), 2));
    assert!(!state.key_valid_at("d9", 1));
    assert_eq!(
        state.key_id_of(&mk(&dev(2))).map(String::as_str),
        Some("d2")
    );
    assert_eq!(state.encryption_keys_at(3).len(), 1);
    assert_eq!(state.encryption_keys_at(2).len(), 3);
}

#[test]
fn bot_key_lapses_and_comes_back_with_a_later_scope() {
    let mut log = Log::new();
    let id = log.add(bot(&["read", "write"], 1_000)); // seq 1
    let state = log.chain.state().clone();
    assert!(state.key_valid_at_time(&id, 1, 999));
    assert!(!state.key_valid_at_time(&id, 1, 1_000), "at expires");
    assert!(!state.key_valid_at_time(&id, 1, 5_000), "after expires");
    assert!(state.key_valid_at(&id, 1), "key_valid_at reads no expiry");
    assert_eq!(
        state.bot_scope_at(&id, 1),
        Some(&BotScope {
            can: vec!["read".into(), "write".into()],
            expires: 1_000
        })
    );
    assert_eq!(state.bot_scope_at("d1", 1), None);

    log.scope(&id, &["read"], 10_000); // seq 2
    let state = log.chain.state();
    assert!(state.key_valid_at_time(&id, 2, 5_000), "valid again");
    assert!(
        !state.key_valid_at_time(&id, 1, 5_000),
        "the old scope still holds at seq 1"
    );
    assert!(!state.key_valid_at_time(&id, 2, 10_000));
    assert_eq!(state.bot_scope_at(&id, 2).unwrap().can, vec!["read"]);
    assert_eq!(state.bot_scope_at(&id, 1).unwrap().expires, 1_000);

    log.revoke(&[&id]); // seq 3: retires the bot
    let state = log.chain.state();
    assert!(!state.key_valid_at_time(&id, 3, 5_000));
    let scope = build::scope(
        &log.chain,
        &master(0),
        &id,
        &BotScope {
            can: vec!["read".into()],
            expires: 99_999,
        },
    )
    .unwrap();
    assert_eq!(
        log.try_push(&scope),
        Err(ChainError::Policy(FoldError::RevokedKey(id)))
    );
}

#[test]
fn until_key_valid_before_expires_and_never_rescoped() {
    let mut log = Log::incept(vec![
        device(1, KeyLife::Standing),
        device(2, KeyLife::Until(1_000)),
    ]);
    let state = log.chain.state();
    assert!(state.key_valid_at_time("d2", 0, 999));
    assert!(!state.key_valid_at_time("d2", 0, 1_000));
    assert!(!state.key_valid_at_time("d2", 0, 1_001));
    assert!(
        state.key_valid_at_time("d1", 0, u64::MAX),
        "a standing key has no expiry"
    );
    assert_eq!(state.bot_scope_at("d2", 0), None);
    // An Until key is never extended by scope.
    let scope = build::scope(
        &log.chain,
        &master(0),
        &"d2".to_string(),
        &BotScope {
            can: vec!["read".into()],
            expires: 9_999,
        },
    )
    .unwrap();
    assert_eq!(
        log.try_push(&scope),
        Err(ChainError::Policy(FoldError::NotABot("d2".into())))
    );
    // An add may list an Until key too.
    let id = log.add(KeyLife::Until(5_000));
    assert!(log.chain.state().key_valid_at_time(&id, 1, 4_999));
    assert!(!log.chain.state().key_valid_at_time(&id, 1, 5_000));
}

#[test]
fn masters_have_no_expiry() {
    let mut log = Log::new();
    log.rotate(&[], false);
    let state = log.chain.state();
    assert!(state.key_valid_at_time("m1", 1, u64::MAX));
    assert!(!state.key_valid_at_time("m0", 1, 0));
    assert!(state.record("m1").unwrap().enc.is_none());
}
