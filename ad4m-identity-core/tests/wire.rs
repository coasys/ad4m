//! A signed link on the wire (`wire`): its hash, its signature, its judgement into the
//! `JudgedLink` the membership replay takes, the trusted seq of a store-signed head, and the
//! `coasys:room-key` signature of an X25519 key. The link server runs these through the wasm
//! exports; `tests/wasm_exports.rs` checks the exports give the same answers.
#![cfg(feature = "build")]

mod common;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

use ad4m_hash_chain::{encode_proof_value, sha256};
use ad4m_identity_core::judgement::{InvalidReason, LinkVerdict, SignerLog};
use ad4m_identity_core::wire::*;
use ad4m_identity_core::{HeadStatement, KeyLife};
use common::*;
use ed25519_dalek::{Signer, SigningKey};
use serde_json::json;

const NOW_MS: u64 = 1_700_000_000_000;
const TIMESTAMP: &str = "2026-11-15T10:00:00.000Z";

fn did_key(key: &SigningKey) -> String {
    format!("did:key:{}", mk(key))
}

/// A membership link signed by `key` the old way (no anchor): `sha256(json(data) ‖ timestamp)`.
fn signed(key: &SigningKey, source: &str, predicate: &str, target: &str) -> WireLink {
    let data = json!({"predicate": predicate, "source": source, "target": target});
    let bytes = ad4m_identity_core::judgement::hash_data_and_timestamp(&data, TIMESTAMP).unwrap();
    let signature = key.sign(&bytes).to_bytes();
    serde_json::from_value(json!({
        "author": did_key(key),
        "timestamp": TIMESTAMP,
        "data": {"source": source, "predicate": predicate, "target": target},
        "proof": {"key": format!("{}#primary", did_key(key)), "signature": hex(&signature)},
    }))
    .unwrap()
}

/// The same link signed with an anchor: `coasys:link` over `{data, timestamp, anchor}`.
fn anchored(
    key: &SigningKey,
    source: &str,
    predicate: &str,
    target: &str,
    anchor: u64,
) -> WireLink {
    let data = json!({"predicate": predicate, "source": source, "target": target});
    let bytes =
        ad4m_identity_core::judgement::anchored_signing_bytes(&data, TIMESTAMP, anchor).unwrap();
    let signature = key.sign(&bytes).to_bytes();
    serde_json::from_value(json!({
        "author": did_key(key),
        "timestamp": TIMESTAMP,
        "data": {"source": source, "predicate": predicate, "target": target},
        "proof": {"key": did_key(key), "signature": hex(&signature), "anchor": anchor},
    }))
    .unwrap()
}

const MEMBER: &str = "coasys://membership/member";
const SPACE: &str = "coasys://membership/space";

#[test]
fn the_hash_is_sha256_of_author_source_predicate_target_and_timestamp() {
    let link = signed(&dev(1), "did:scid:coasys:1:QmA", MEMBER, SPACE);
    let mut input = Vec::new();
    for part in [
        link.author.as_str(),
        "did:scid:coasys:1:QmA",
        MEMBER,
        SPACE,
        TIMESTAMP,
    ] {
        input.extend_from_slice(part.as_bytes());
    }
    assert_eq!(wire_link_hash(&link), sha256(&input));
}

#[test]
fn a_link_signed_by_a_device_key_of_a_log_counts_for_its_person() {
    let log = Log::new();
    let state = log.chain.state();
    let link = signed(&dev(1), &state.did, MEMBER, SPACE);
    assert!(wire_signature_holds(&link));
    let judged = judge_wire_link(&link, listed(state, log.seq()), NOW_MS).unwrap();
    assert_eq!(judged.verdict, LinkVerdict::Valid);
    assert_eq!(judged.person.as_deref(), Some(state.did.as_str()));
    assert!(!judged.by_bot);
    assert_eq!(judged.hash, wire_link_hash(&link));
    assert_eq!(judged.author_key, link.author);
    assert_eq!(judged.timestamp, 1_794_736_800_000);
    assert_eq!(
        (
            judged.source.as_str(),
            judged.predicate.as_str(),
            judged.target.as_str()
        ),
        (state.did.as_str(), MEMBER, SPACE)
    );
}

fn listed(state: &ad4m_identity_core::KeyState, trusted: u64) -> SignerLog<'_> {
    SignerLog::Listed { state, trusted }
}

#[test]
fn an_anchored_link_counts_for_its_person() {
    let log = Log::new();
    let state = log.chain.state();
    let link = anchored(&dev(1), &state.did, MEMBER, SPACE, 0);
    assert!(wire_signature_holds(&link));
    let judged = judge_wire_link(&link, listed(state, 0), NOW_MS).unwrap();
    assert_eq!(judged.verdict, LinkVerdict::Valid);
    assert_eq!(judged.person.as_deref(), Some(state.did.as_str()));
}

#[test]
fn a_bot_key_counts_for_its_owner_marked_as_a_bot() {
    let mut log = Log::new();
    log.add(bot(&["write"], 4_000_000_000)); // d2
    let state = log.chain.state();
    let link = signed(&dev(2), "did:key:zBot", MEMBER, SPACE);
    let judged = judge_wire_link(&link, listed(state, log.seq()), NOW_MS).unwrap();
    assert_eq!(judged.verdict, LinkVerdict::Valid);
    assert_eq!(judged.person.as_deref(), Some(state.did.as_str()));
    assert!(judged.by_bot);
}

#[test]
fn a_device_only_key_counts_for_no_person() {
    let link = signed(&sk("device only"), "did:key:zSomeone", MEMBER, SPACE);
    let judged = judge_wire_link(&link, SignerLog::NoLog, NOW_MS).unwrap();
    assert_eq!(judged.verdict, LinkVerdict::Valid);
    assert_eq!(judged.person, None);
    assert!(!judged.by_bot);
}

#[test]
fn a_signature_that_does_not_hold_is_invalid_and_speaks_for_no_one() {
    let log = Log::new();
    let state = log.chain.state();
    let mut link = signed(&dev(1), &state.did, MEMBER, SPACE);
    link.data.target = "invite://other".into();
    assert!(!wire_signature_holds(&link));
    let judged = judge_wire_link(&link, listed(state, log.seq()), NOW_MS).unwrap();
    assert_eq!(
        judged.verdict,
        LinkVerdict::Invalid(InvalidReason::BadSignature)
    );
    assert_eq!(judged.person, None);
}

#[test]
fn a_key_revoked_in_the_trusted_log_does_not_count_for_a_link_arriving_now() {
    let mut log = Log::incept(vec![
        device(1, KeyLife::Standing),
        device(2, KeyLife::Standing),
    ]);
    log.revoke(&["d2"]);
    let state = log.chain.state();
    let link = signed(&dev(2), &state.did, MEMBER, SPACE);
    let judged = judge_wire_link(&link, listed(state, log.seq()), NOW_MS).unwrap();
    assert!(!judged.verdict.counts(), "{:?}", judged.verdict);
    assert_eq!(judged.person, None);
    // Before the revoke is trusted, the key still counts.
    let judged = judge_wire_link(&link, listed(state, 0), NOW_MS).unwrap();
    assert_eq!(judged.verdict, LinkVerdict::Valid);
}

#[test]
fn a_link_with_an_unreadable_timestamp_or_signature_is_malformed() {
    let mut link = signed(&dev(1), "a", MEMBER, SPACE);
    link.timestamp = "yesterday".into();
    assert!(judge_wire_link(&link, SignerLog::NoLog, NOW_MS).is_err());
    let mut link = signed(&dev(1), "a", MEMBER, SPACE);
    link.proof.signature = "not hex".into();
    assert!(!wire_signature_holds(&link));
}

#[test]
fn an_anchor_reads_as_a_number_or_a_decimal_string() {
    let link = anchored(&dev(1), "a", MEMBER, SPACE, 7);
    let mut value = serde_json::to_value(&link).unwrap();
    assert_eq!(value["proof"]["anchor"], json!(7));
    value["proof"]["anchor"] = json!("7");
    let again: WireLink = serde_json::from_value(value.clone()).unwrap();
    assert_eq!(again.proof.anchor, Some(7));
    value["proof"]["anchor"] = json!("-1");
    assert!(serde_json::from_value::<WireLink>(value).is_err());
}

#[test]
fn a_standing_device_key_speaks_for_its_person_and_a_revoked_one_for_no_one() {
    let mut log = Log::incept(vec![
        device(1, KeyLife::Standing),
        device(2, KeyLife::Standing),
    ]);
    log.add(bot(&["write"], 1_800_000_000)); // d3, expires in unix seconds
    log.revoke(&["d2"]);
    let state = log.chain.state();
    let did = state.did.clone();
    let person = |key: &SigningKey, trusted: u64, now_ms: u64| {
        author_of(state, &mk(key), trusted, now_ms).map(|a| (a.person, a.by_bot))
    };
    assert_eq!(
        person(&dev(1), log.seq(), NOW_MS),
        Some((did.clone(), false))
    );
    // Revoked at the trusted seq: no one. Before the revoke is trusted: still the person.
    assert_eq!(person(&dev(2), log.seq(), NOW_MS), None);
    assert_eq!(
        person(&dev(2), log.seq() - 1, NOW_MS),
        Some((did.clone(), false))
    );
    // A bot key speaks for its owner until its scope expires.
    assert_eq!(
        person(&dev(3), log.seq(), NOW_MS),
        Some((did.clone(), true))
    );
    assert_eq!(person(&dev(3), log.seq(), 1_800_000_000_000), None);
    // Added past the trusted seq, a master key, a key the log does not list: no one.
    assert_eq!(person(&dev(3), 0, NOW_MS), None);
    assert_eq!(person(&master(1), log.seq(), NOW_MS), None);
    assert_eq!(person(&sk("stranger"), log.seq(), NOW_MS), None);
}

fn store_key() -> SigningKey {
    sk("store")
}

fn signed_head(scid: &str, seq: u64, hash: &str, store: &SigningKey) -> SignedHead {
    let store_did = did_key(store);
    let statement = HeadStatement {
        scid,
        seq,
        hash,
        time: 1_700_000_000,
        store: &store_did,
    };
    let sig = store.sign(&statement.signing_bytes().unwrap()).to_bytes();
    SignedHead {
        scid: scid.into(),
        seq,
        hash: hash.into(),
        time: 1_700_000_000,
        store: store_did,
        sig: encode_proof_value(&sig),
    }
}

#[test]
fn a_head_signed_by_a_trusted_store_key_gives_its_seq() {
    let mut log = Log::new();
    log.add(KeyLife::Standing);
    let scid = log.chain.id().clone();
    let trusted = vec![mk(&store_key())];
    let at = |seq: u64| log.chain.hash_at(seq).unwrap().clone();

    let head = signed_head(&scid, 0, &at(0), &store_key());
    assert_eq!(trusted_seq(&log.chain, &head, &trusted), Some(0));
    let head = signed_head(&scid, 1, &at(1), &store_key());
    assert_eq!(trusted_seq(&log.chain, &head, &trusted), Some(1));

    // Another store's key, another log's scid, a hash the log does not hold at the seq, a seq
    // past the log, a signature over other bytes: none is trusted.
    let other = signed_head(&scid, 1, &at(1), &sk("other store"));
    assert_eq!(trusted_seq(&log.chain, &other, &trusted), None);
    let elsewhere = signed_head("QmElsewhere", 1, &at(1), &store_key());
    assert_eq!(trusted_seq(&log.chain, &elsewhere, &trusted), None);
    let wrong_hash = signed_head(&scid, 1, &at(0), &store_key());
    assert_eq!(trusted_seq(&log.chain, &wrong_hash, &trusted), None);
    let past = signed_head(&scid, 2, &at(1), &store_key());
    assert_eq!(trusted_seq(&log.chain, &past, &trusted), None);
    let mut forged = signed_head(&scid, 1, &at(1), &store_key());
    forged.time += 1;
    assert_eq!(trusted_seq(&log.chain, &forged, &trusted), None);
}

#[test]
fn an_x25519_key_signed_under_coasys_room_key_verifies_for_its_did_only() {
    let key = dev(1);
    let did = did_key(&key);
    let x25519 = "11".repeat(32);
    let bytes = room_key_signing_bytes(&did, &x25519).unwrap();
    assert_eq!(
        String::from_utf8(bytes.clone()).unwrap(),
        format!(r#"{{"did":"{did}","domain":"coasys:room-key","x25519PublicKey":"{x25519}"}}"#)
    );
    let signature = hex(&key.sign(&bytes).to_bytes());
    assert!(verify_room_key(&did, &x25519, &signature));

    let other = did_key(&dev(2));
    assert!(!verify_room_key(&other, &x25519, &signature));
    assert!(!verify_room_key(&did, &"22".repeat(32), &signature));
    assert!(!verify_room_key(&did, &x25519, "zz"));
    // Only 32 bytes of lowercase hex is an X25519 key; only a did:key signs.
    assert!(room_key_signing_bytes(&did, "11").is_err());
    assert!(room_key_signing_bytes(&did, &"AA".repeat(32)).is_err());
    assert!(room_key_signing_bytes("did:scid:coasys:1:QmA", &x25519).is_err());
}
