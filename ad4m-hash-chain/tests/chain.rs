//! Order and links, tampering, forks and out-of-order import (build plan 5.18.9).
#![cfg(not(target_arch = "wasm32"))]

mod common;

use ad4m_hash_chain::{
    compare, entry_hash, entry_hash_json, fold, import, verify_proof, CanonError, Chain,
    ChainError, Compare, ForkEvidence, ImportOutcome, ProofError,
};
use common::*;
use serde_json::json;

fn snapshot(chain: &TestChain) -> serde_json::Value {
    chain_json(chain)
}

// ------------------------------------------------------------- order and links

#[test]
fn fold_of_a_valid_chain_gives_head_hashes_and_state() {
    let entries = make_chain("c", 1, 5);
    let chain = fold(&TestPolicy, &entries).unwrap();
    assert_eq!(chain.id(), "c");
    assert_eq!(chain.head().seq, 4);
    assert_eq!(chain.head().hash, entry_hash(&entries[4]).unwrap());
    for (i, e) in entries.iter().enumerate() {
        assert_eq!(chain.hash_at(i as u64), Some(&entry_hash(e).unwrap()));
    }
    assert_eq!(chain.hash_at(5), None);
    assert_eq!(chain.state().count, 4);
    assert_eq!(chain.state().key, multikey(&signing_key(1)));
}

#[test]
fn empty_fold_is_empty() {
    assert_eq!(fold(&TestPolicy, &[]).unwrap_err(), ChainError::Empty);
}

#[test]
fn wrong_seq_refused_and_chain_unchanged() {
    let entries = make_chain("c", 1, 3);
    let mut chain = fold(&TestPolicy, &entries[..2]).unwrap();
    let before = snapshot(&chain);
    let mut skip = entries[2].clone();
    skip.seq = 3;
    let skip = sign(skip, &signing_key(1), DOMAIN);
    assert_eq!(
        chain.append(&TestPolicy, &skip).unwrap_err(),
        ChainError::Seq { want: 2, got: 3 }
    );
    // An entry we already hold is also a wrong seq for append.
    assert_eq!(
        chain.append(&TestPolicy, &entries[1]).unwrap_err(),
        ChainError::Seq { want: 2, got: 1 }
    );
    assert_eq!(snapshot(&chain), before);
    chain.append(&TestPolicy, &entries[2]).unwrap();
}

#[test]
fn wrong_prev_refused_and_chain_unchanged() {
    let entries = make_chain("c", 1, 3);
    let mut chain = fold(&TestPolicy, &entries[..2]).unwrap();
    let before = snapshot(&chain);
    let mut bad = entries[2].clone();
    bad.prev = Some(entry_hash(&entries[0]).unwrap());
    let bad = sign(bad, &signing_key(1), DOMAIN);
    assert_eq!(
        chain.append(&TestPolicy, &bad).unwrap_err(),
        ChainError::Prev {
            want: entry_hash(&entries[1]).unwrap(),
            got: Some(entry_hash(&entries[0]).unwrap())
        }
    );
    let mut none = entries[2].clone();
    none.prev = None;
    assert!(matches!(
        chain.append(&TestPolicy, &none).unwrap_err(),
        ChainError::Prev { got: None, .. }
    ));
    assert_eq!(snapshot(&chain), before);
}

#[test]
fn entry_of_another_chain_refused() {
    let entries = make_chain("c", 1, 2);
    let mut chain = fold(&TestPolicy, &entries[..1]).unwrap();
    let mut other = entries[1].clone();
    other.chain = "d".into();
    let other = sign(other, &signing_key(1), DOMAIN);
    assert_eq!(
        chain.append(&TestPolicy, &other).unwrap_err(),
        ChainError::WrongChain {
            want: "c".into(),
            got: "d".into()
        }
    );
    assert_eq!(chain.head().seq, 0);
}

#[test]
fn genesis_needs_seq_zero_and_null_prev() {
    let entries = make_chain("c", 1, 2);
    assert_eq!(
        Chain::genesis(&TestPolicy, &entries[1]).unwrap_err(),
        ChainError::Seq { want: 0, got: 1 }
    );
    let mut e0 = entries[0].clone();
    e0.prev = Some(entry_hash(&entries[1]).unwrap());
    let e0 = sign(e0, &signing_key(1), DOMAIN);
    assert!(matches!(
        Chain::genesis(&TestPolicy, &e0).unwrap_err(),
        ChainError::Malformed(_)
    ));
}

#[test]
fn prev_that_is_not_a_sha256_multihash_is_malformed() {
    let entries = make_chain("c", 1, 2);
    let mut chain = fold(&TestPolicy, &entries[..1]).unwrap();
    let mut bad = entries[1].clone();
    bad.prev = Some("not-a-hash".into());
    assert!(matches!(
        chain.append(&TestPolicy, &bad).unwrap_err(),
        ChainError::Malformed(_)
    ));
}

#[test]
fn policy_failure_changes_nothing() {
    let entries = make_chain("c", 1, 3);
    let mut chain = fold(&TestPolicy, &entries[..2]).unwrap();
    let before = snapshot(&chain);
    // Rotate in a valid entry, then a refused one: the state stays at the valid one.
    let mut unsigned = entries[2].clone();
    unsigned.key = Some(multikey(&signing_key(2)));
    unsigned.proof.clear();
    assert_eq!(
        policy_err(&chain.append(&TestPolicy, &unsigned).unwrap_err()),
        Some(&TestError::MissingProof)
    );
    assert_eq!(snapshot(&chain), before);
}

#[test]
fn policy_hooks_follow_the_key_named_in_entry_zero_and_rotations() {
    let k1 = signing_key(1);
    let k2 = signing_key(2);
    let e0 = genesis_entry("c", &k1);
    let e1 = next_entry(&e0, "rotate", &k1, Some(&k2));
    let mut chain = fold(&TestPolicy, &[e0, e1.clone()]).unwrap();
    assert_eq!(chain.state().key, multikey(&k2));
    // The old key no longer signs.
    let old = next_entry(&e1, "old key", &k1, None);
    assert!(matches!(
        policy_err(&chain.append(&TestPolicy, &old).unwrap_err()),
        Some(TestError::BadProof(ProofError::Signature))
    ));
    let new = next_entry(&e1, "new key", &k2, None);
    chain.append(&TestPolicy, &new).unwrap();
    assert_eq!(chain.state().count, 2);
}

#[test]
fn entry_with_float_or_unsafe_integer_is_malformed() {
    #[derive(Clone, serde::Serialize, serde::Deserialize)]
    struct Loose {
        seq: u64,
        prev: Option<String>,
        x: f64,
    }
    impl ad4m_hash_chain::Entry for Loose {
        fn seq(&self) -> u64 {
            self.seq
        }
        fn prev(&self) -> Option<&String> {
            self.prev.as_ref()
        }
        fn proofs(&self) -> &[ad4m_hash_chain::Proof] {
            &[]
        }
    }
    let float = Loose {
        seq: 0,
        prev: None,
        x: 0.5,
    };
    assert!(matches!(entry_hash(&float), Err(CanonError::Float(_))));
    let mut big = make_chain("c", 1, 1)[0].clone();
    big.seq = 1 << 53;
    assert!(matches!(entry_hash(&big), Err(CanonError::Integer(_))));
    let proof = ad4m_hash_chain::Proof::unsigned("test:c#k", "assertionMethod", DOMAIN);
    assert!(matches!(
        ad4m_hash_chain::signing_input(&float, &proof),
        Err(CanonError::Float(_))
    ));
    assert!(matches!(
        ad4m_hash_chain::signing_input(&big, &proof),
        Err(CanonError::Integer(_))
    ));
    assert!(matches!(
        Chain::genesis(&TestPolicy, &big).unwrap_err(),
        ChainError::Malformed(_)
    ));
}

// ------------------------------------------------------------------ tampering

#[test]
fn one_changed_byte_in_a_body_breaks_the_next_entry_prev() {
    let k = signing_key(1);
    let entries = make_chain("c", 1, 3);
    let mut changed = entries[1].clone();
    let mut bytes = changed.body.clone().into_bytes();
    bytes[0] ^= 0x01;
    changed.body = String::from_utf8(bytes).unwrap();
    assert_ne!(
        entry_hash(&changed).unwrap(),
        entry_hash(&entries[1]).unwrap()
    );

    // Unsigned again: its own proof fails.
    let err = fold(
        &TestPolicy,
        &[entries[0].clone(), changed.clone(), entries[2].clone()],
    )
    .unwrap_err();
    assert!(matches!(
        policy_err(&err),
        Some(TestError::BadProof(ProofError::Signature))
    ));

    // Re-signed by the key: the next entry's link fails.
    let resigned = sign(changed, &k, DOMAIN);
    let err = fold(
        &TestPolicy,
        &[entries[0].clone(), resigned.clone(), entries[2].clone()],
    )
    .unwrap_err();
    assert_eq!(
        err,
        ChainError::Prev {
            want: entry_hash(&resigned).unwrap(),
            got: Some(entry_hash(&entries[1]).unwrap())
        }
    );
}

#[test]
fn changed_proof_value_fails_the_policy_check() {
    let entries = make_chain("c", 1, 2);
    let mut chain = fold(&TestPolicy, &entries[..1]).unwrap();
    let mut bad = entries[1].clone();
    let mut sig = ad4m_hash_chain::decode_proof_value(&bad.proof[0].proof_value).unwrap();
    sig[0] ^= 0x80;
    bad.proof[0].proof_value = ad4m_hash_chain::encode_proof_value(&sig);
    // The hash does not change: the proof sits outside it.
    assert_eq!(entry_hash(&bad).unwrap(), entry_hash(&entries[1]).unwrap());
    assert!(matches!(
        policy_err(&chain.append(&TestPolicy, &bad).unwrap_err()),
        Some(TestError::BadProof(ProofError::Signature))
    ));
}

#[test]
fn proof_and_member_order_do_not_change_the_hash_or_the_proof() {
    let entries = make_chain("c", 1, 2);
    let e = &entries[1];
    let p = &e.proof[0];
    // The same entry as text, with members and proof members in another order.
    let text = format!(
        r#"{{"proof":[{{"proofValue":"{}","domain":"{}","proofPurpose":"{}","verificationMethod":"{}","cryptosuite":"{}","type":"{}"}}],"body":"{}","prev":"{}","seq":1,"chain":"c"}}"#,
        p.proof_value,
        p.domain,
        p.proof_purpose,
        p.verification_method,
        p.cryptosuite,
        p.r#type,
        e.body,
        e.prev.as_ref().unwrap()
    );
    let value: serde_json::Value = serde_json::from_str(&text).unwrap();
    assert_eq!(entry_hash_json(&value).unwrap(), entry_hash(e).unwrap());
    let parsed: TestEntry = serde_json::from_str(&text).unwrap();
    assert_eq!(&parsed, e);
    verify_proof(
        &parsed,
        &parsed.proof[0],
        &multikey(&signing_key(1)),
        DOMAIN,
    )
    .unwrap();
    ad4m_hash_chain::verify_proof_json(&value, p, &multikey(&signing_key(1)), DOMAIN).unwrap();
}

#[test]
fn a_proof_under_another_domain_fails() {
    let k = signing_key(1);
    let entries = make_chain("c", 1, 2);
    let other = sign(entries[1].clone(), &k, "other:domain");
    assert_eq!(
        verify_proof(&other, &other.proof[0], &multikey(&k), DOMAIN),
        Err(ProofError::Domain {
            want: DOMAIN.into(),
            got: "other:domain".into()
        })
    );
    // Relabelling the domain breaks the signature, because the domain is signed.
    let mut relabelled = other.clone();
    relabelled.proof[0].domain = DOMAIN.into();
    assert_eq!(
        verify_proof(&relabelled, &relabelled.proof[0], &multikey(&k), DOMAIN),
        Err(ProofError::Signature)
    );
    let mut chain = fold(&TestPolicy, &entries[..1]).unwrap();
    assert!(matches!(
        policy_err(&chain.append(&TestPolicy, &other).unwrap_err()),
        Some(TestError::BadProof(ProofError::Domain { .. }))
    ));
}

#[test]
fn proof_refuses_unknown_members() {
    let entries = make_chain("c", 1, 1);
    let mut v = serde_json::to_value(&entries[0].proof[0]).unwrap();
    v["created"] = json!("2026-10-24T00:00:00Z");
    assert!(serde_json::from_value::<ad4m_hash_chain::Proof>(v).is_err());
}

// ---------------------------------------------------------------------- forks

fn forked() -> (Vec<TestEntry>, TestEntry) {
    let k = signing_key(1);
    let entries = make_chain("c", 1, 4);
    let theirs = next_entry(&entries[1], "their entry 2", &k, None);
    (entries, theirs)
}

#[test]
fn two_valid_entries_at_one_seq_give_fork_evidence_and_nothing_appends() {
    let k = signing_key(1);
    let (entries, theirs) = forked();
    let their_next = next_entry(&theirs, "their entry 3", &k, None);
    // Both branches are valid on their own.
    let ours = fold(&TestPolicy, &entries).unwrap();
    let branch = fold(
        &TestPolicy,
        &[
            entries[0].clone(),
            entries[1].clone(),
            theirs.clone(),
            their_next.clone(),
        ],
    )
    .unwrap();
    let evidence = ForkEvidence {
        chain: "c".into(),
        seq: 2,
        ours: entry_hash(&entries[2]).unwrap(),
        theirs: entry_hash(&theirs).unwrap(),
    };
    assert_eq!(
        compare(ours.hashes(), branch.hashes(), 0, &"c".to_string()),
        Compare::Fork(evidence.clone())
    );

    let mut chain = fold(&TestPolicy, &entries[..3]).unwrap();
    let before = snapshot(&chain);
    let outcome = import(
        &mut chain,
        &TestPolicy,
        &[theirs.clone(), their_next, entries[3].clone()],
    )
    .unwrap();
    assert_eq!(outcome, ImportOutcome::Fork(evidence));
    assert_eq!(snapshot(&chain), before);
}

#[test]
fn a_shorter_peer_chain_is_we_extend_and_never_truncates() {
    let entries = make_chain("c", 1, 5);
    let mut chain = fold(&TestPolicy, &entries).unwrap();
    let peer = fold(&TestPolicy, &entries[..3]).unwrap();
    assert_eq!(
        compare(chain.hashes(), peer.hashes(), 0, &"c".to_string()),
        Compare::WeExtend { from: 3 }
    );
    assert_eq!(
        compare(peer.hashes(), chain.hashes(), 0, &"c".to_string()),
        Compare::TheyExtend { from: 3 }
    );
    assert_eq!(
        compare(
            &chain.hashes()[2..],
            &peer.hashes()[2..],
            2,
            &"c".to_string()
        ),
        Compare::WeExtend { from: 3 }
    );
    let before = snapshot(&chain);
    assert_eq!(
        import(&mut chain, &TestPolicy, &entries[..3]).unwrap(),
        ImportOutcome::AlreadyHave
    );
    assert_eq!(snapshot(&chain), before);
    assert_eq!(chain.head().seq, 4);
}

#[test]
fn compare_same_and_fork_offsets() {
    let h: Vec<String> = make_chain("c", 1, 3)
        .iter()
        .map(|e| entry_hash(e).unwrap())
        .collect();
    let c = "c".to_string();
    assert_eq!(compare(&h, &h, 0, &c), Compare::Same);
    assert_eq!(compare(&[], &[], 7, &c), Compare::Same);
    match compare(&h[1..], &[h[1].clone(), h[0].clone()], 1, &c) {
        Compare::Fork(f) => {
            assert_eq!(f.seq, 2);
            assert_eq!(f.ours, h[2]);
            assert_eq!(f.theirs, h[0]);
        }
        other => panic!("{other:?}"),
    }
}

// --------------------------------------------------------------- out of order

#[test]
fn a_shuffled_batch_imports_as_the_sorted_batch() {
    let entries = make_chain("c", 1, 8);
    let mut sorted_chain = fold(&TestPolicy, &entries[..2]).unwrap();
    let mut shuffled_chain = sorted_chain.clone();
    let sorted = import(&mut sorted_chain, &TestPolicy, &entries[2..]).unwrap();
    let mut shuffled = entries[2..].to_vec();
    shuffled.reverse();
    shuffled.swap(1, 4);
    let got = import(&mut shuffled_chain, &TestPolicy, &shuffled).unwrap();
    assert_eq!(got, sorted);
    assert_eq!(
        got,
        ImportOutcome::Appended {
            head: sorted_chain.head(),
            count: 6
        }
    );
    assert_eq!(snapshot(&shuffled_chain), snapshot(&sorted_chain));
    assert_eq!(
        snapshot(&sorted_chain),
        snapshot(&fold(&TestPolicy, &entries).unwrap())
    );
}

#[test]
fn a_batch_past_our_head_is_a_gap_until_the_missing_entries_arrive() {
    let entries = make_chain("c", 1, 6);
    let mut chain = fold(&TestPolicy, &entries[..2]).unwrap();
    let before = snapshot(&chain);
    assert_eq!(
        import(&mut chain, &TestPolicy, &entries[4..]).unwrap(),
        ImportOutcome::Gap { have: 1, first: 4 }
    );
    assert_eq!(snapshot(&chain), before);
    assert_eq!(
        import(&mut chain, &TestPolicy, &entries[2..4]).unwrap(),
        ImportOutcome::Appended {
            head: ad4m_hash_chain::Head {
                seq: 3,
                hash: entry_hash(&entries[3]).unwrap()
            },
            count: 2
        }
    );
    assert!(matches!(
        import(&mut chain, &TestPolicy, &entries[4..]).unwrap(),
        ImportOutcome::Appended { count: 2, .. }
    ));
    assert_eq!(chain.head().seq, 5);
}

#[test]
fn a_repeated_batch_is_already_have() {
    let entries = make_chain("c", 1, 4);
    let mut chain = fold(&TestPolicy, &entries[..1]).unwrap();
    assert!(matches!(
        import(&mut chain, &TestPolicy, &entries).unwrap(),
        ImportOutcome::Appended { count: 3, .. }
    ));
    assert_eq!(
        import(&mut chain, &TestPolicy, &entries).unwrap(),
        ImportOutcome::AlreadyHave
    );
    // Duplicates within one batch count once.
    let mut doubled = entries.clone();
    doubled.extend(entries.clone());
    assert_eq!(
        import(&mut chain, &TestPolicy, &doubled).unwrap(),
        ImportOutcome::AlreadyHave
    );
    assert_eq!(
        import(&mut chain, &TestPolicy, &[]).unwrap(),
        ImportOutcome::AlreadyHave
    );
}

#[test]
fn import_changes_nothing_when_a_later_entry_fails() {
    let entries = make_chain("c", 1, 5);
    let mut chain = fold(&TestPolicy, &entries[..2]).unwrap();
    let before = snapshot(&chain);
    let mut batch = entries[2..].to_vec();
    batch[2].proof.clear();
    assert_eq!(
        policy_err(&import(&mut chain, &TestPolicy, &batch).unwrap_err()),
        Some(&TestError::MissingProof)
    );
    assert_eq!(snapshot(&chain), before);
    // A hole inside the batch is a seq error, and also changes nothing.
    let holed = vec![entries[2].clone(), entries[4].clone()];
    assert_eq!(
        import(&mut chain, &TestPolicy, &holed).unwrap_err(),
        ChainError::Seq { want: 3, got: 4 }
    );
    assert_eq!(snapshot(&chain), before);
}

#[test]
fn import_refuses_another_chain_before_comparing_hashes() {
    let entries = make_chain("c", 1, 3);
    let other = make_chain("d", 1, 3);
    let mut chain = fold(&TestPolicy, &entries).unwrap();
    assert_eq!(
        import(&mut chain, &TestPolicy, &other[..1]).unwrap_err(),
        ChainError::WrongChain {
            want: "c".into(),
            got: "d".into()
        }
    );
}

#[test]
fn chain_error_codes_have_the_typescript_shape() {
    let e: ChainError<TestError> = ChainError::Prev {
        want: "Qm1".into(),
        got: None,
    };
    assert_eq!(
        e.code(),
        json!({"code": "prev", "want": "Qm1", "got": null})
    );
    let e: ChainError<TestError> = ChainError::Policy(TestError::NoKey);
    assert_eq!(e.code(), json!({"code": "policy", "reason": "NoKey"}));
    assert_eq!(e.to_string(), "NoKey");
    let outcome = ImportOutcome::Gap { have: 1, first: 4 };
    assert_eq!(
        serde_json::to_value(&outcome).unwrap(),
        json!({"kind": "gap", "have": 1, "first": 4})
    );
}
