//! The new device's side of linking (`device`): the steps a browser takes with a signing key it
//! cannot export, checked against the same fixtures as the inbox, link and record tests.
#![cfg(feature = "build")]

mod common;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

use ad4m_hash_chain::sha256;
use ad4m_identity_core::build::Signer;
use ad4m_identity_core::device::{self, EncKey};
use ad4m_identity_core::inbox::*;
use ad4m_identity_core::link::OfferKind;
use ad4m_identity_core::record::{sign_receipt, PrivateRecord};
use common::*;
use ed25519_dalek::SigningKey;
use serde_json::{json, Value};

/// The device key of the link: `d4`, not yet in the log. Its X25519 key is `enc_secret(4)`.
fn browser() -> SigningKey {
    dev(4)
}

fn enc() -> EncKey {
    EncKey::from_secret(enc_secret(4).to_bytes())
}

fn sign_text(bytes: &[u8]) -> String {
    device::proof_value(&Signer::sign(&browser(), bytes)).unwrap()
}

/// A ticket for `d4` as the holder (master 0, `d4`'s offer bound to it) sealed it to the
/// pre-join inbox, and the address it was sealed at.
fn sealed_ticket(log: &Log) -> (String, String) {
    let signed = signed_offer(
        log,
        ad4m_identity_core::link::Offer {
            device_key: Some(mk(&browser())),
            ..link_offer(log, OfferKind::Device, None)
        },
    );
    let body = InboxBody::OfferTicket(Box::new(OfferTicket {
        store: "https://store.ad4m.dev".into(),
        scid: log.chain.state().scid.clone(),
        offer_id: signed.offer.offer_id.clone(),
        s: bs58::encode(link_secret()).into_string(),
        signed_offer: signed,
    }));
    let payload = sign_payload(
        mk(&browser()),
        body,
        LINK_NOW,
        signed_by(log),
        &master(log.n),
    )
    .unwrap();
    let address = device::pre_address(&mk(&browser()), &device_r()).unwrap();
    let sealed = seal(
        &enc().multikey(),
        &InboxAddress::Pre(address.clone()),
        &payload,
        &mut TestRng::new("device"),
    )
    .unwrap();
    (address, sealed)
}

#[test]
fn the_device_string_round_trips_and_refuses_a_bad_field() {
    let text = device::device_string(
        &mk(&browser()),
        &enc().multikey(),
        &device_r(),
        LINK_NOW + 300,
        Some(&"h".repeat(100)),
        "https://store.ad4m.dev",
    )
    .unwrap();
    let parsed = ad4m_identity_core::link::DeviceString::parse(&text).unwrap();
    assert_eq!(parsed.sign, mk(&browser()));
    assert_eq!(parsed.hint.unwrap().chars().count(), 64);

    let bad = device::device_string("nope", &enc().multikey(), &device_r(), 1, None, "x");
    assert_eq!(bad.unwrap_err().code, "link");
}

#[test]
fn a_new_device_takes_every_link_step_with_the_bytes_it_signs() {
    let log = devices_log();
    let scid = log.chain.state().scid.clone();
    let me = mk(&browser());
    let (address, sealed) = sealed_ticket(&log);

    // The pre-join read: the bytes to sign, then the header.
    let auth = json!({
        "address": address, "sign": me, "r": device_r(), "store": store_did(),
        "method": "GET", "path": format!("/v1/inbox/pre/{address}"),
        "nonce": bs58::encode(&sha256(b"nonce")[..16]).into_string(),
        "expires": LINK_NOW + 60,
    });
    let sig = sign_text(&device::pre_auth_bytes(&auth).unwrap());
    let header = device::pre_auth_header(&auth, &sig).unwrap();
    assert!(!header.contains('=') && !header.contains('+'));
    // A signature by another key, or for another address, is refused.
    let other = device::proof_value(&Signer::sign(
        &dev(5),
        &device::pre_auth_bytes(&auth).unwrap(),
    ))
    .unwrap();
    assert_eq!(
        device::pre_auth_header(&auth, &other).unwrap_err().code,
        "inbox"
    );

    // Open, fetch the log, check, claim.
    let payload = device::open_message(&enc(), &address, &sealed).unwrap();
    assert_eq!(device::ticket_scid(&payload), Some(scid.as_str()));
    let Accept::ClaimOffer(ticket) =
        device::check_message(&payload, &me, &log.chain, LINK_NOW).unwrap()
    else {
        panic!("a ticket asks for a claim");
    };
    let co_sig = sign_text(&device::claim_bytes(&ticket, &me, &enc().multikey(), false).unwrap());
    let claim = device::claim(
        &ticket,
        &me,
        &enc().multikey(),
        false,
        Some("Chrome"),
        &co_sig,
    )
    .unwrap();
    assert_eq!(claim.offer_id, link_offer_id());
    ad4m_identity_core::link::verify_claim(&scid, &claim).unwrap();
    assert_eq!(
        device::match_code(&scid, &ticket.offer_id, &me),
        ad4m_identity_core::link::match_code(&scid, &ticket.offer_id, &me)
    );
}

#[test]
fn a_step_refuses_what_does_not_hold() {
    let log = devices_log();
    let me = mk(&browser());
    let (address, sealed) = sealed_ticket(&log);

    // Another X25519 key, or another address, does not open the message.
    let wrong = EncKey::from_secret(enc_secret(5).to_bytes());
    assert_eq!(
        device::open_message(&wrong, &address, &sealed)
            .unwrap_err()
            .reason,
        "open"
    );
    let other_address =
        device::pre_address(&me, &bs58::encode(&sha256(b"r2")[..16]).into_string()).unwrap();
    assert!(device::open_message(&enc(), &other_address, &sealed).is_err());

    // A ticket checked by another device is not for it.
    let payload = device::open_message(&enc(), &address, &sealed).unwrap();
    assert!(device::check_message(&payload, &mk(&dev(5)), &log.chain, LINK_NOW).is_err());

    // A co-signature of another key, or over another claim, is refused.
    let Accept::ClaimOffer(ticket) =
        device::check_message(&payload, &me, &log.chain, LINK_NOW).unwrap()
    else {
        panic!();
    };
    let bytes = device::claim_bytes(&ticket, &me, &enc().multikey(), false).unwrap();
    let forged = device::proof_value(&Signer::sign(&dev(5), &bytes)).unwrap();
    assert_eq!(
        device::claim(&ticket, &me, &enc().multikey(), false, None, &forged)
            .unwrap_err()
            .code,
        "link"
    );
    let good = sign_text(&bytes);
    assert!(device::claim(&ticket, &me, &enc().multikey(), true, None, &good).is_err());
    assert_eq!(
        device::proof_value(&[0u8; 12]).unwrap_err().code,
        "malformed"
    );
}

#[test]
fn a_store_request_and_a_statement_are_signed_over_their_bytes() {
    let log = devices_log();
    let head = log.chain.head();
    let req = json!({
        "did": log.chain.state().did, "store": store_did(), "keyId": "d1",
        "kelSeq": head.seq, "kelHash": head.hash,
        "method": "GET", "path": "/v1/private/x",
        "bodyHash": device::body_hash(b""),
        "nonce": bs58::encode(&sha256(b"n")[..16]).into_string(),
        "expires": LINK_NOW + 60,
    });
    let sig = sign_text(&device::request_bytes(&req).unwrap());
    let header = device::request_header(&req, &sig).unwrap();
    assert!(!header.is_empty());
    assert!(device::request_bytes(&json!({"nope": 1})).is_err());

    let statement = json!({"v": 1, "purpose": "login", "challenge": "c"});
    let bytes = device::statement_bytes(&statement).unwrap();
    assert_eq!(
        bytes,
        ad4m_identity_core::domain::signing_bytes(
            ad4m_identity_core::domain::SESSION_DOMAIN,
            &statement
        )
        .unwrap()
    );
    let signed = device::signed_statement(&statement, &sign_text(&bytes)).unwrap();
    assert_eq!(signed["statement"], statement);
    assert!(device::signed_statement(&statement, "nope").is_err());
    assert!(device::statement_bytes(&Value::Null).is_err());
}

#[test]
fn the_record_opens_with_the_receipt_and_lists_the_devices_that_answer() {
    let log = devices_log();
    let store = sk("the identity store");
    let sealed = ad4m_identity_core::record::seal_record(
        &log.chain,
        1,
        &{
            let mut r: PrivateRecord = record_fixture();
            let d1 = r.devices.get_mut("d1").unwrap();
            d1.address = Some("wss://laptop.example".into());
            r
        },
        None,
        &dev(1),
        &mut TestRng::new("record"),
    )
    .unwrap();
    let receipt = sign_receipt(log.chain.id(), &sealed, log.chain.head().seq, &store).unwrap();
    let stores = vec![format!("did:key:{}", mk(&store))];
    let key = EncKey::from_secret(enc_secret(2).to_bytes());
    let record = device::open_record(&sealed, &receipt, &log.chain, &stores, "d2", &key).unwrap();
    let picked = device::picker_devices(&record);
    assert_eq!(picked.len(), 1);
    assert_eq!(picked[0].key_id, "d1");
    assert_eq!(picked[0].address, "wss://laptop.example");

    // A receipt from a store the device does not trust, or the wrong key, is refused.
    assert!(device::open_record(&sealed, &receipt, &log.chain, &[], "d2", &key).is_err());
    let wrong = EncKey::from_secret(enc_secret(1).to_bytes());
    assert!(device::open_record(&sealed, &receipt, &log.chain, &stores, "d2", &wrong).is_err());
}
