//! The wasm-bindgen exports of the new device's side of linking (`device`) give the answers of the
//! shared vectors (`devices.json`, `link.json`), and a device that holds only bytes to sign links
//! end to end (wasm32 + features `wasm`, `build`).
#![cfg(all(target_arch = "wasm32", feature = "wasm", feature = "build"))]

use ad4m_identity_core::build::Signer;
use ad4m_identity_core::wasm::*;
use ed25519_dalek::SigningKey;
use serde_json::{json, Value};
use wasm_bindgen::JsValue;
use wasm_bindgen_test::wasm_bindgen_test;

fn js(value: &Value) -> JsValue {
    js_sys::JSON::parse(&value.to_string()).unwrap()
}

fn rust(value: &JsValue) -> Value {
    serde_json::from_str(&js_sys::JSON::stringify(value).unwrap().as_string().unwrap()).unwrap()
}

fn vector(text: &str) -> Value {
    serde_json::from_str(text).unwrap()
}

fn devices() -> Value {
    vector(include_str!("../vectors/devices.json"))
}

fn link() -> Value {
    vector(include_str!("../vectors/link.json"))
}

fn sign(key: &SigningKey, bytes: &[u8]) -> String {
    proof_value(&Signer::sign(key, bytes)).unwrap()
}

#[wasm_bindgen_test]
fn the_pre_join_read_matches_the_vector() {
    let d = devices();
    let address = &d["preAddress"];
    assert_eq!(
        pre_inbox_address(
            address["sign"].as_str().unwrap(),
            address["r"].as_str().unwrap()
        )
        .unwrap(),
        address["address"].as_str().unwrap()
    );
    let auth = &d["preAuth"];
    let mut value = auth["value"].clone();
    value.as_object_mut().unwrap().remove("sig");
    assert_eq!(
        pre_inbox_bytes(js(&value)).unwrap(),
        auth["signingBytes"].as_str().unwrap().as_bytes()
    );
    let sig = auth["value"]["sig"].as_str().unwrap();
    let header = pre_inbox_header(js(&value), sig).unwrap();
    let decoded = {
        use base64::Engine;
        let bytes = base64::engine::general_purpose::URL_SAFE_NO_PAD
            .decode(header)
            .unwrap();
        serde_json::from_slice::<Value>(&bytes).unwrap()
    };
    assert_eq!(decoded, auth["value"]);
    // Another signature does not give a header.
    let other = sign(&SigningKey::from_bytes(&[3; 32]), b"x");
    assert!(pre_inbox_header(js(&value), &other).is_err());
}

#[wasm_bindgen_test]
fn the_claim_and_the_match_code_match_the_vector() {
    let l = link();
    let claim_v = &l["claim"];
    let signed = &l["signedOffer"];
    let ticket = json!({
        "store": "https://store.ad4m.dev",
        "scid": signed["offer"]["scid"],
        "offerId": claim_v["offerId"],
        "s": claim_v["s"],
        "signedOffer": signed,
    });
    let device_key = claim_v["deviceKey"].as_str().unwrap();
    let enc_key = claim_v["encKey"].as_str().unwrap();
    assert_eq!(
        claim_bytes(js(&ticket), device_key, enc_key, true).unwrap(),
        l["claimSigningBytes"].as_str().unwrap().as_bytes()
    );
    let got = rust(
        &claim(
            js(&ticket),
            device_key,
            enc_key,
            true,
            claim_v["hint"].as_str().map(str::to_string),
            claim_v["coSig"].as_str().unwrap(),
        )
        .unwrap(),
    );
    assert_eq!(&got, claim_v);
    // A co-signature that is not the device's is refused.
    let wrong = sign(&SigningKey::from_bytes(&[3; 32]), b"x");
    let err = rust(&claim(js(&ticket), device_key, enc_key, true, None, &wrong).unwrap_err());
    assert_eq!(err["code"], "link");

    let m = &l["match"];
    assert_eq!(
        match_code(
            m["scid"].as_str().unwrap(),
            m["offerId"].as_str().unwrap(),
            m["deviceKey"].as_str().unwrap()
        ),
        m["code"].as_str().unwrap()
    );
}

#[wasm_bindgen_test]
fn the_record_vector_opens_through_the_log_and_the_receipt() {
    let d = devices();
    let chain = KelChainJs::fold(js(&d["log"])).unwrap();
    let r = &d["record"];
    let secret = hex_bytes(d["seal"]["recipientSecret"].as_str().unwrap());
    let enc = EncKeyJs::from_secret(&secret).unwrap();
    assert_eq!(enc.multikey(), d["seal"]["to"].as_str().unwrap());
    let stores = vec![r["receipt"]["value"]["store"].as_str().unwrap().to_string()];
    let record = rust(
        &open_record(
            js(&r["sealed"]),
            js(&r["receipt"]["value"]),
            &chain,
            stores.clone(),
            "d1",
            &enc,
        )
        .unwrap(),
    );
    assert_eq!(
        record,
        serde_json::from_str::<Value>(r["plaintext"].as_str().unwrap()).unwrap()
    );
    // No device of the record runs an executor at an address.
    assert_eq!(rust(&picker_devices(js(&record)).unwrap()), json!([]));
    // A store that is not trusted, or another device's key, is refused.
    let err = rust(
        &open_record(
            js(&r["sealed"]),
            js(&r["receipt"]["value"]),
            &chain,
            vec![],
            "d1",
            &enc,
        )
        .unwrap_err(),
    );
    assert_eq!(err["code"], "record");
    let other = EncKeyJs::from_secret(&[7; 32]).unwrap();
    assert!(open_record(
        js(&r["sealed"]),
        js(&r["receipt"]["value"]),
        &chain,
        stores,
        "d1",
        &other
    )
    .is_err());
}

fn hex_bytes(text: &str) -> Vec<u8> {
    (0..text.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&text[i..i + 2], 16).unwrap())
        .collect()
}

#[wasm_bindgen_test]
fn the_log_names_a_device_by_its_signing_key() {
    let d = devices();
    let chain = KelChainJs::fold(js(&d["log"])).unwrap();
    let state = rust(&chain.state().unwrap());
    let sign = state["keys"]["d1"]["sign"].as_str().unwrap();
    assert_eq!(chain.device_key_id(sign), Some("d1".to_string()));
    assert_eq!(chain.device_key_id("z6MkNotAKey"), None);
}

#[wasm_bindgen_test]
fn statements_requests_and_keys_have_their_bytes() {
    let statement = json!({"v": 1, "purpose": "login", "challenge": "c"});
    let bytes = statement_bytes(js(&statement)).unwrap();
    let key = SigningKey::from_bytes(&[9; 32]);
    let signed = rust(&signed_statement(js(&statement), &sign(&key, &bytes)).unwrap());
    assert_eq!(signed["statement"], statement);
    assert!(signed_statement(js(&statement), "nope").is_err());

    assert_eq!(body_hash(b""), body_hash(b""));
    assert_ne!(body_hash(b""), body_hash(b"x"));
    assert_eq!(
        proof_value(&[0; 12])
            .err()
            .map(|e| rust(&e)["code"].clone()),
        Some(json!("malformed"))
    );

    let raw = key.verifying_key().to_bytes();
    assert_eq!(
        ed25519_multikey(&raw).unwrap(),
        ad4m_hash_chain::encode_ed25519_multikey(&raw)
    );
    assert!(x25519_multikey(&[1; 5]).is_err());
}

#[wasm_bindgen_test]
fn a_new_key_makes_a_device_string_the_holder_parses() {
    let enc = EncKeyJs::generate();
    assert_eq!(
        EncKeyJs::from_secret(&enc.secret()).unwrap().multikey(),
        enc.multikey()
    );
    let sign =
        ed25519_multikey(&SigningKey::from_bytes(&[9; 32]).verifying_key().to_bytes()).unwrap();
    let text = device_string(
        &sign,
        &enc.multikey(),
        &new_r(),
        1_700_000_300.0,
        None,
        "https://store.ad4m.dev",
    )
    .unwrap();
    assert!(text.starts_with("coasys:device:1:"));
    assert!(device_string("nope", &enc.multikey(), &new_r(), 1.0, None, "x").is_err());
}
