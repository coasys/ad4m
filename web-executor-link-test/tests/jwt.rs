//! The executor's JWTs verify on wasm32. The wasm32 build uses jsonwebtoken 10, which by
//! default rejects a token whose `aud` is not expected; native's jsonwebtoken 8 checks `aud`
//! only against an expected audience. Every token the executor signs carries an `aud`.
#![cfg(target_arch = "wasm32")]

use jsonwebtoken::{encode, Algorithm, EncodingKey, Header};
use rust_executor::agent::capabilities::{
    decode_jwt, read_stored_claims, signing_key_name, Claims,
};
use rust_executor::config::{set_global_config, Ad4mConfig};
use rust_executor::wallet::{try_init_wallet_backend, wallet_backend, LocalWallet, WalletBackend};
use std::sync::Arc;
use wasm_bindgen_test::wasm_bindgen_test;

/// The wallet's signing key, created once for the test binary.
fn signing_secret() -> Vec<u8> {
    // Not `Ad4mConfig::default()`: its `prepare` looks for a home directory, which wasm32
    // has none of (the boot in S9 builds its config the same way).
    set_global_config(Ad4mConfig::unprepared());
    let local = Arc::new(LocalWallet::new());
    local
        .generate_keypair(&signing_key_name())
        .expect("generate the signing key");
    let _ = try_init_wallet_backend(local as Arc<dyn WalletBackend>);
    wallet_backend()
        .get_secret_key(&signing_key_name())
        .expect("the signing key exists")
}

fn token(claims: &Claims, header: Header, secret: &[u8]) -> String {
    encode(&header, claims, &EncodingKey::from_secret(secret)).expect("encode")
}

#[wasm_bindgen_test]
fn a_token_with_an_audience_round_trips() {
    let secret = signing_secret();
    let claims = Claims::new("did:key:issuer".into(), "an app".into(), 3600, None);
    let jwt = token(&claims, Header::default(), &secret);

    let decoded = decode_jwt(jwt.clone()).expect("decode_jwt accepts the executor's token");
    assert_eq!(decoded.nonce(), claims.nonce());

    let stored = read_stored_claims(&jwt).expect("read_stored_claims reads the executor's token");
    assert_eq!(stored.nonce(), claims.nonce());
}

#[wasm_bindgen_test]
fn a_token_signed_with_another_key_is_refused() {
    let _ = signing_secret();
    let claims = Claims::new("did:key:issuer".into(), "an app".into(), 3600, None);
    let jwt = token(&claims, Header::default(), b"not the executor's key");
    assert!(decode_jwt(jwt).is_err());
}

#[wasm_bindgen_test]
fn a_token_with_another_algorithm_is_refused() {
    let secret = signing_secret();
    let claims = Claims::new("did:key:issuer".into(), "an app".into(), 3600, None);
    let jwt = token(&claims, Header::new(Algorithm::HS384), &secret);
    assert!(decode_jwt(jwt).is_err());
}

#[wasm_bindgen_test]
fn an_expired_token_is_refused_but_still_read() {
    let secret = signing_secret();
    let claims = Claims::new("did:key:issuer".into(), "an app".into(), 3600, None);
    // Claims keeps `exp` private: move it an hour into the past, beyond the 60 s leeway.
    let mut value = serde_json::to_value(&claims).expect("claims serialize");
    let exp = value["exp"].as_u64().expect("exp is a number");
    value["exp"] = serde_json::json!(exp - 2 * 3600);
    let jwt = encode(
        &Header::default(),
        &value,
        &EncodingKey::from_secret(&secret),
    )
    .expect("encode");

    assert!(decode_jwt(jwt.clone()).is_err());
    // Revocation reads an expired token too: read_stored_claims skips the exp check.
    let stored = read_stored_claims(&jwt).expect("read_stored_claims reads an expired token");
    assert_eq!(stored.nonce(), claims.nonce());
}

#[wasm_bindgen_test]
fn garbage_is_neither_decoded_nor_read() {
    let _ = signing_secret();
    assert!(decode_jwt("not-a-jwt".to_string()).is_err());
    assert!(read_stored_claims("not-a-jwt").is_none());
}
