//! The wasm-bindgen exports of `wire` (wasm32 + features `build`, `wasm`) give what the native
//! functions give: `KelHandle.trustedSeq`, `KelHandle.judgeLink`, `KelHandle.authorOf`,
//! `judgeLinkWithoutLog`, `wireLinkHash`, `roomKeySigningBytes` and `verifyRoomKey`. The link
//! server runs them (`link-server/src/identity.ts`); `tests/wire.rs` pins the native answers.
#![cfg(all(target_arch = "wasm32", feature = "wasm", feature = "build"))]

mod common;

use ad4m_hash_chain::encode_proof_value;
use ad4m_identity_core::judgement::SignerLog;
use ad4m_identity_core::wasm::{
    judge_link_without_log, room_key_signing_bytes_js, verify_room_key_js, wire_link_hash_js,
    KelChainJs,
};
use ad4m_identity_core::wire::{self, SignedHead, WireLink};
use ad4m_identity_core::{HeadStatement, KeyLife};
use common::*;
use ed25519_dalek::{Signer, SigningKey};
use serde_json::{json, Value};
use wasm_bindgen::JsValue;
use wasm_bindgen_test::wasm_bindgen_test;

const NOW_MS: u64 = 1_700_000_000_000;
const TIMESTAMP: &str = "2026-11-15T10:00:00.000Z";
const MEMBER: &str = "coasys://membership/member";

fn js(value: &Value) -> JsValue {
    js_sys::JSON::parse(&value.to_string()).unwrap()
}

fn rust(value: &JsValue) -> Value {
    serde_json::from_str(&js_sys::JSON::stringify(value).unwrap().as_string().unwrap()).unwrap()
}

fn did_key(key: &SigningKey) -> String {
    format!("did:key:{}", mk(key))
}

fn signed(key: &SigningKey, source: &str) -> WireLink {
    let data =
        json!({"predicate": MEMBER, "source": source, "target": "coasys://membership/space"});
    let bytes = ad4m_identity_core::judgement::hash_data_and_timestamp(&data, TIMESTAMP).unwrap();
    serde_json::from_value(json!({
        "author": did_key(key),
        "timestamp": TIMESTAMP,
        "data": {"source": source, "predicate": MEMBER, "target": "coasys://membership/space"},
        "proof": {"key": did_key(key), "signature": hex(&key.sign(&bytes).to_bytes())},
    }))
    .unwrap()
}

fn signed_head(scid: &str, seq: u64, hash: &str) -> SignedHead {
    let store = sk("store");
    let store_did = did_key(&store);
    let statement = HeadStatement {
        scid,
        seq,
        hash,
        time: 1_700_000_000,
        store: &store_did,
    };
    SignedHead {
        scid: scid.into(),
        seq,
        hash: hash.into(),
        time: 1_700_000_000,
        store: store_did.clone(),
        sig: encode_proof_value(&store.sign(&statement.signing_bytes().unwrap()).to_bytes()),
    }
}

fn the_log() -> Log {
    let mut log = Log::incept(vec![
        device(1, KeyLife::Standing),
        device(2, KeyLife::Standing),
    ]);
    log.add(bot(&["write"], 1_800_000_000)); // d3
    log.revoke(&["d2"]);
    log
}

#[wasm_bindgen_test]
fn trusted_seq_export_matches_the_native_answer() {
    let log = the_log();
    let handle = KelChainJs::fold(js(&json!(log.json()))).unwrap();
    let scid = log.chain.id().clone();
    let trusted = vec![mk(&sk("store"))];
    for seq in 0..=log.seq() {
        let head = signed_head(&scid, seq, log.chain.hash_at(seq).unwrap());
        let got = handle
            .trusted_seq(js(&json!(head)), trusted.clone())
            .unwrap();
        assert_eq!(got, Some(seq as f64));
        assert_eq!(wire::trusted_seq(&log.chain, &head, &trusted), Some(seq));
    }
    let wrong = signed_head(&scid, 1, log.chain.hash_at(0).unwrap());
    assert_eq!(
        handle
            .trusted_seq(js(&json!(wrong)), trusted.clone())
            .unwrap(),
        None
    );
    let untrusted = signed_head(&scid, 1, log.chain.hash_at(1).unwrap());
    assert_eq!(
        handle
            .trusted_seq(js(&json!(untrusted)), vec![mk(&sk("other"))])
            .unwrap(),
        None
    );
    let thrown = rust(&handle.trusted_seq(js(&json!({})), trusted).err().unwrap());
    assert_eq!(thrown["code"], "malformed");
}

#[wasm_bindgen_test]
fn judge_link_export_matches_the_native_judgement() {
    let log = the_log();
    let handle = KelChainJs::fold(js(&json!(log.json()))).unwrap();
    let state = log.chain.state();
    for (key, trusted) in [
        (dev(1), log.seq()),
        (dev(2), log.seq()),
        (dev(2), log.seq() - 1),
        (dev(3), log.seq()),
        (sk("stranger"), log.seq()),
    ] {
        let link = signed(&key, &state.did);
        let text = serde_json::to_string(&link).unwrap();
        let got: Value = serde_json::from_str(
            &handle
                .judge_link(&text, trusted as f64, NOW_MS as f64)
                .unwrap(),
        )
        .unwrap();
        let native =
            wire::judge_wire_link(&link, SignerLog::Listed { state, trusted }, NOW_MS).unwrap();
        assert_eq!(got, serde_json::to_value(&native).unwrap());
    }
    let device_only = signed(&sk("device only"), "did:key:zSomeone");
    let text = serde_json::to_string(&device_only).unwrap();
    let got: Value =
        serde_json::from_str(&judge_link_without_log(&text, NOW_MS as f64).unwrap()).unwrap();
    let native = wire::judge_wire_link(&device_only, SignerLog::NoLog, NOW_MS).unwrap();
    assert_eq!(got, serde_json::to_value(&native).unwrap());
    assert_eq!(
        wire_link_hash_js(&text).unwrap(),
        hex(&wire::wire_link_hash(&device_only))
    );

    let thrown = rust(&handle.judge_link("{}", 0.0, NOW_MS as f64).err().unwrap());
    assert_eq!(thrown["code"], "malformed");
    let thrown = rust(&handle.judge_link(&text, -1.0, NOW_MS as f64).err().unwrap());
    assert_eq!(thrown["code"], "malformed");
    let mut bad_time = device_only.clone();
    bad_time.timestamp = "yesterday".into();
    let bad_time = serde_json::to_string(&bad_time).unwrap();
    let thrown = rust(
        &judge_link_without_log(&bad_time, NOW_MS as f64)
            .err()
            .unwrap(),
    );
    assert_eq!(thrown["code"], "malformed");
}

#[wasm_bindgen_test]
fn author_of_export_matches_the_native_answer() {
    let log = the_log();
    let handle = KelChainJs::fold(js(&json!(log.json()))).unwrap();
    let state = log.chain.state();
    for (key, trusted) in [
        (dev(1), log.seq()),
        (dev(2), log.seq()),
        (dev(2), log.seq() - 1),
        (dev(3), log.seq()),
        (master(1), log.seq()),
    ] {
        let got = handle
            .author_of(&mk(&key), trusted as f64, NOW_MS as f64)
            .unwrap();
        let native = wire::author_of(state, &mk(&key), trusted, NOW_MS);
        assert_eq!(rust(&got), json!(native));
    }
}

#[wasm_bindgen_test]
fn room_key_exports_match_the_native_answers() {
    let key = dev(1);
    let did = did_key(&key);
    let x25519 = "11".repeat(32);
    let bytes = room_key_signing_bytes_js(&did, &x25519).unwrap();
    assert_eq!(bytes, wire::room_key_signing_bytes(&did, &x25519).unwrap());
    let signature = hex(&key.sign(&bytes).to_bytes());
    assert!(verify_room_key_js(&did, &x25519, &signature));
    assert!(!verify_room_key_js(&did_key(&dev(2)), &x25519, &signature));
    let thrown = rust(&room_key_signing_bytes_js(&did, "11").err().unwrap());
    assert_eq!(thrown["code"], "malformed");
}
