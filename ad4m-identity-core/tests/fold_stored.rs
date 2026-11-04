//! `fold_stored`, the replay of a host's own stored, already-verified log: the same chain as
//! `fold` for every log `fold` accepts, and the only checks it leaves out are the proofs'
//! signatures and the duplicate-key scan (rule K8).
#![cfg(feature = "build")]

mod common;

use ad4m_hash_chain::{encode_proof_value, ChainError};
use ad4m_identity_core::build::Signer;
use ad4m_identity_core::{fold, fold_stored, Event, FoldError, KelChain, KeyLife};
use common::*;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

fn same_chain(a: &KelChain, b: &KelChain) -> bool {
    a.id() == b.id() && a.hashes() == b.hashes() && a.state() == b.state()
}

/// A log with every op and key life, two rotations and the end: incept with two keys, bot and
/// standing adds, scopes (one naming an action again), revocations, rotations that revoke.
fn varied_log() -> Log {
    let mut log = Log::incept(vec![
        device(1, KeyLife::Standing),
        device(2, KeyLife::Standing),
    ]);
    let bot_a = log.add(bot(&["read", "write"], 1_750_000_000));
    log.scope(&bot_a, &["read"], 1_800_000_000);
    log.add(KeyLife::Standing);
    log.revoke(&["d2"]);
    log.rotate(&["d1"], false);
    log.scope(&bot_a, &["join", "read", "join"], 1_900_000_000);
    let bot_b = log.add(bot(&["join"], 1_760_000_000));
    log.revoke(&[bot_a.as_str()]);
    log.rotate(&[], false);
    log.scope(&bot_b, &["read", "write", "join"], 1_950_000_000);
    log.revoke(&["d4"]);
    log.rotate(&[], true);
    log
}

#[test]
fn a_stored_fold_gives_the_chain_a_full_fold_gives_at_every_seq() {
    let log = varied_log();
    assert_eq!(log.events.len(), 13);
    assert!(log.chain.state().ended);
    for end in 1..=log.events.len() {
        let events = &log.events[..end];
        let full = fold(events).unwrap();
        let stored = fold_stored(events).unwrap();
        assert!(
            same_chain(&full, &stored),
            "the folds differ at seq {}",
            end - 1
        );
    }
    assert!(same_chain(&fold_stored(&log.events).unwrap(), &log.chain));
}

#[test]
fn a_stored_fold_refuses_what_a_full_fold_refuses_but_a_bad_signature_or_a_reused_key() {
    // Every crafted refusal: `fold_stored` gives the same answer as `fold`, except where `fold`
    // refuses a proof whose signature alone fails (`BadProof`) or a key the log already lists
    // (`DuplicateKey`).
    let mut skipped = 0;
    for case in refusal_cases() {
        let Ok(events) = case
            .events
            .iter()
            .map(Event::from_json)
            .collect::<Result<Vec<_>, _>>()
        else {
            continue; // refused on parsing, before either fold
        };
        let full = fold(&events);
        let stored = fold_stored(&events);
        match (&full, &stored) {
            (Ok(a), Ok(b)) => assert!(same_chain(a, b), "{}", case.name),
            (Err(a), Err(b)) if a == b => {}
            // Past a check it leaves out, the stored fold goes on: it may take the log, or stop
            // at a later rule.
            (Err(ChainError::Policy(FoldError::BadProof(_) | FoldError::DuplicateKey(_))), _) => {
                skipped += 1
            }
            _ => panic!(
                "{}: fold {:?}, fold_stored {:?}",
                case.name,
                full.err(),
                stored.err()
            ),
        }
    }
    // At least a wrong signature and a duplicate key.
    assert!(skipped >= 2, "{skipped}");
}

#[test]
fn a_stored_fold_takes_a_signature_a_full_fold_refuses_and_still_needs_the_proof() {
    let log = varied_log();
    // Every proof of every event signed over other bytes: the format holds, no signature does.
    let wrong = encode_proof_value(&Signer::sign(&sk("mallory"), b"not this event"));
    let mut forged = log.events.clone();
    for event in &mut forged {
        for proof in &mut event.proof {
            proof.proof_value = wrong.clone();
        }
    }
    assert!(matches!(
        fold(&forged).unwrap_err(),
        ChainError::Policy(FoldError::BadProof(_))
    ));
    assert!(same_chain(&fold_stored(&forged).unwrap(), &log.chain));
    // Which proofs an event carries is still checked: an event with none, two by its signer, one
    // for another purpose, or one by a key that does not sign it, is refused as `fold` does.
    let at = 4; // the revocation of d2, signed by m0
    let mut variants = Vec::new();
    let mut none = log.events[..=at].to_vec();
    none[at].proof.clear();
    variants.push(none);
    let mut twice = log.events[..=at].to_vec();
    let again = twice[at].proof[0].clone();
    twice[at].proof.push(again);
    variants.push(twice);
    let mut purpose = log.events[..=at].to_vec();
    purpose[at].proof[0].proof_purpose = "authentication".into();
    variants.push(purpose);
    let mut extra = log.events[..=at].to_vec();
    sign(&mut extra[at], "d1", &dev(1));
    variants.push(extra);
    for events in &variants {
        let full = fold(events).unwrap_err();
        assert!(matches!(
            full,
            ChainError::Policy(FoldError::MissingProof(_) | FoldError::BadProof(_))
        ));
        assert_eq!(fold_stored(events).unwrap_err(), full);
    }
}
