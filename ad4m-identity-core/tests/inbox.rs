//! The device inbox: addresses, HPKE seals, payloads and the device's checks
//! (`inbox::check_payload`), and the pre-join read.
#![cfg(feature = "build")]

mod common;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

use ad4m_identity_core::inbox::*;
use ad4m_identity_core::link::{offer_commit, LinkError, Offer, OfferKind, SignedBy, OFFER_TTL_S};
use ad4m_identity_core::x25519_multikey_of;
use common::*;

fn sealed_to_d1(log: &Log, payload: &InboxPayload) -> String {
    seal(
        &x25519_multikey_of(&enc_secret(1)),
        &joined(log, 1),
        payload,
        &mut TestRng::new("test"),
    )
    .unwrap()
}

#[test]
fn a_sealed_message_opens_with_the_device_key_at_its_address() {
    let log = devices_log();
    let payload = master_payload(&log, 1, 0);
    let sealed = sealed_to_d1(&log, &payload);
    assert_eq!(open(&enc_secret(1), &joined(&log, 1), &sealed), Ok(payload));
}

#[test]
fn another_key_or_another_address_does_not_open_it() {
    let log = devices_log();
    let sealed = sealed_to_d1(&log, &master_payload(&log, 1, 0));
    assert_eq!(
        open(&enc_secret(2), &joined(&log, 1), &sealed),
        Err(InboxError::Open)
    );
    assert_eq!(
        open(&enc_secret(1), &joined(&log, 2), &sealed),
        Err(InboxError::Open)
    );
    let pre = InboxAddress::Pre(pre_address(&mk(&dev(1)), &device_r()).unwrap());
    assert_eq!(open(&enc_secret(1), &pre, &sealed), Err(InboxError::Open));
}

#[test]
fn a_tampered_message_does_not_open() {
    let log = devices_log();
    let sealed = sealed_to_d1(&log, &master_payload(&log, 1, 0));
    for i in [0, sealed.len() / 2, sealed.len() - 2] {
        let mut bytes = sealed.clone().into_bytes();
        bytes[i] = if bytes[i] == b'A' { b'B' } else { b'A' };
        let tampered = String::from_utf8(bytes).unwrap();
        assert_eq!(
            open(&enc_secret(1), &joined(&log, 1), &tampered),
            Err(InboxError::Open),
            "byte {i}"
        );
    }
    assert_eq!(
        open(
            &enc_secret(1),
            &joined(&log, 1),
            &"A".repeat(MAX_SEALED_BYTES + 1)
        ),
        Err(InboxError::Open)
    );
}

#[test]
fn the_current_master_is_kept() {
    let log = devices_log();
    let p = master_payload(&log, 1, 0);
    assert_eq!(
        check_payload(&p, &mk(&dev(1)), &log.chain, LINK_NOW),
        Ok(Accept::KeepMaster { index: 0 })
    );
}

#[test]
fn a_payload_for_another_device_is_refused() {
    let log = devices_log();
    let p = master_payload(&log, 1, 0);
    assert_eq!(
        check_payload(&p, &mk(&dev(2)), &log.chain, LINK_NOW),
        Err(InboxError::Recipient)
    );
}

#[test]
fn a_payload_signed_by_a_device_key_is_refused() {
    let log = devices_log();
    let body = master_payload(&log, 1, 0).body;
    let by = SignedBy {
        key_id: "d2".into(),
        ..signed_by(&log)
    };
    let p = sign_payload(mk(&dev(1)), body, LINK_NOW, by, &dev(2)).unwrap();
    assert_eq!(
        check_payload(&p, &mk(&dev(1)), &log.chain, LINK_NOW),
        Err(InboxError::Signer)
    );
}

#[test]
fn a_payload_at_a_position_the_log_does_not_hold_is_refused() {
    let log = devices_log();
    let body = master_payload(&log, 1, 0).body;
    for by in [
        SignedBy {
            kel_hash: log.chain.hash_at(0).unwrap().clone(),
            ..signed_by(&log)
        },
        SignedBy {
            kel_seq: 9,
            ..signed_by(&log)
        },
    ] {
        let p = sign_payload(mk(&dev(1)), body.clone(), LINK_NOW, by, &master(0)).unwrap();
        assert_eq!(
            check_payload(&p, &mk(&dev(1)), &log.chain, LINK_NOW),
            Err(InboxError::Signer)
        );
    }
}

#[test]
fn a_changed_body_or_a_foreign_signature_is_refused() {
    let log = devices_log();
    let mut p = master_payload(&log, 1, 0);
    p.sent_at += 1;
    assert_eq!(
        check_payload(&p, &mk(&dev(1)), &log.chain, LINK_NOW),
        Err(InboxError::Signature)
    );
    let body = master_payload(&log, 1, 0).body;
    let p = sign_payload(mk(&dev(1)), body, LINK_NOW, signed_by(&log), &master(1)).unwrap();
    assert_eq!(
        check_payload(&p, &mk(&dev(1)), &log.chain, LINK_NOW),
        Err(InboxError::Signature)
    );
}

#[test]
fn a_master_secret_for_another_index_is_refused() {
    let log = devices_log();
    let body = InboxBody::Master {
        did: log.chain.state().did.clone(),
        index: 0,
        secret: bs58::encode(master(1).to_bytes()).into_string(),
    };
    let p = sign_payload(mk(&dev(1)), body, LINK_NOW, signed_by(&log), &master(0)).unwrap();
    assert_eq!(
        check_payload(&p, &mk(&dev(1)), &log.chain, LINK_NOW),
        Err(InboxError::Master(0))
    );
    let body = InboxBody::Master {
        did: log.chain.state().did.clone(),
        index: 5,
        secret: bs58::encode(master(5).to_bytes()).into_string(),
    };
    let p = sign_payload(mk(&dev(1)), body, LINK_NOW, signed_by(&log), &master(0)).unwrap();
    assert_eq!(
        check_payload(&p, &mk(&dev(1)), &log.chain, LINK_NOW),
        Err(InboxError::Master(5))
    );
}

#[test]
fn a_master_of_another_identity_is_refused() {
    let log = devices_log();
    let body = InboxBody::Master {
        did: "did:scid:coasys:1:QmOther".into(),
        index: 0,
        secret: bs58::encode(master(0).to_bytes()).into_string(),
    };
    let p = sign_payload(mk(&dev(1)), body, LINK_NOW, signed_by(&log), &master(0)).unwrap();
    assert_eq!(
        check_payload(&p, &mk(&dev(1)), &log.chain, LINK_NOW),
        Err(InboxError::Master(0))
    );
}

#[test]
fn debug_output_never_shows_a_master_secret() {
    let log = devices_log();
    let p = master_payload(&log, 1, 0);
    let InboxBody::Master { secret, .. } = &p.body else {
        panic!("a master payload");
    };
    assert!(!format!("{p:?}").contains(secret.as_str()));
}

#[test]
fn a_replaced_master_is_stale() {
    let mut log = devices_log();
    let old = master_payload(&log, 1, 0);
    log.rotate(&[], false);
    // Signed by m0 at a position it held: valid, but master 0 no longer counts.
    assert_eq!(
        check_payload(&old, &mk(&dev(1)), &log.chain, LINK_NOW),
        Ok(Accept::Stale)
    );
    let current = master_payload(&log, 1, 1);
    assert_eq!(
        check_payload(&current, &mk(&dev(1)), &log.chain, LINK_NOW),
        Ok(Accept::KeepMaster { index: 1 })
    );
}

fn bound_offer(log: &Log, bound: Option<String>) -> Offer {
    Offer {
        device_key: bound,
        ..link_offer(log, OfferKind::Device, None)
    }
}

fn ticket(log: &Log, offer: Offer, s: [u8; 32]) -> InboxPayload {
    let signed = signed_offer(log, offer);
    let body = InboxBody::OfferTicket(Box::new(OfferTicket {
        store: "https://store.coasys.org".into(),
        scid: log.chain.state().scid.clone(),
        offer_id: signed.offer.offer_id.clone(),
        s: bs58::encode(s).into_string(),
        signed_offer: signed,
    }));
    sign_payload(mk(&dev(4)), body, LINK_NOW, signed_by(log), &master(0)).unwrap()
}

#[test]
fn a_ticket_for_a_bound_offer_is_claimed() {
    let log = devices_log();
    let p = ticket(&log, bound_offer(&log, Some(mk(&dev(4)))), link_secret());
    let InboxBody::OfferTicket(t) = &p.body else {
        unreachable!()
    };
    assert_eq!(
        check_payload(&p, &mk(&dev(4)), &log.chain, LINK_NOW),
        Ok(Accept::ClaimOffer(t.clone()))
    );
    assert_eq!(t.offer_string().offer_id, link_offer_id());
}

#[test]
fn a_ticket_bound_to_another_key_or_unbound_is_refused() {
    let log = devices_log();
    for bound in [None, Some(mk(&dev(5)))] {
        let p = ticket(&log, bound_offer(&log, bound), link_secret());
        assert_eq!(
            check_payload(&p, &mk(&dev(4)), &log.chain, LINK_NOW),
            Err(InboxError::Ticket(LinkError::Bound))
        );
    }
}

#[test]
fn a_ticket_with_the_wrong_secret_or_an_expired_offer_is_refused() {
    let log = devices_log();
    let p = ticket(&log, bound_offer(&log, Some(mk(&dev(4)))), [9; 32]);
    assert_eq!(
        check_payload(&p, &mk(&dev(4)), &log.chain, LINK_NOW),
        Err(InboxError::Ticket(LinkError::Secret))
    );
    let p = ticket(&log, bound_offer(&log, Some(mk(&dev(4)))), link_secret());
    assert_eq!(
        check_payload(&p, &mk(&dev(4)), &log.chain, LINK_NOW + OFFER_TTL_S),
        Err(InboxError::Ticket(LinkError::Expired))
    );
}

#[test]
fn a_ticket_whose_offer_is_another_offer_is_refused() {
    let log = devices_log();
    let mut p = ticket(&log, bound_offer(&log, Some(mk(&dev(4)))), link_secret());
    let InboxBody::OfferTicket(t) = &mut p.body else {
        unreachable!()
    };
    t.offer_id = bs58::encode([1u8; 16]).into_string();
    let p = sign_payload(p.to, p.body, p.sent_at, p.by, &master(0)).unwrap();
    assert!(matches!(
        check_payload(&p, &mk(&dev(4)), &log.chain, LINK_NOW),
        Err(InboxError::Ticket(LinkError::Malformed(_)))
    ));
    assert_eq!(
        offer_commit(&link_secret()),
        link_offer(&log, OfferKind::Device, None).commit
    );
}

#[test]
fn a_payload_with_an_unknown_field_or_type_does_not_parse() {
    let log = devices_log();
    let mut value = serde_json::to_value(master_payload(&log, 1, 0)).unwrap();
    value["extra"] = 1.into();
    assert!(serde_json::from_value::<InboxPayload>(value).is_err());
    let mut value = serde_json::to_value(master_payload(&log, 1, 0)).unwrap();
    value["body"]["type"] = "pause".into();
    assert!(serde_json::from_value::<InboxPayload>(value).is_err());
    let mut value = serde_json::to_value(master_payload(&log, 1, 0)).unwrap();
    value["body"]["extra"] = 1.into();
    assert!(serde_json::from_value::<InboxPayload>(value).is_err());
    let p = ticket(&log, bound_offer(&log, Some(mk(&dev(4)))), link_secret());
    let mut value = serde_json::to_value(&p).unwrap();
    value["body"]["extra"] = 1.into();
    assert!(serde_json::from_value::<InboxPayload>(value).is_err());
    let value = serde_json::to_value(master_payload(&log, 1, 0)).unwrap();
    assert_eq!(value["body"]["type"], "master");
    let p = ticket(&log, bound_offer(&log, Some(mk(&dev(4)))), link_secret());
    let value = serde_json::to_value(&p).unwrap();
    assert_eq!(value["body"]["type"], "offer-ticket");
    assert_eq!(serde_json::from_value::<InboxPayload>(value).unwrap(), p);
}

#[test]
fn the_pre_address_needs_a_signing_key_and_a_16_byte_r() {
    let a = pre_address(&mk(&dev(4)), &device_r()).unwrap();
    assert_eq!(a, pre_address(&mk(&dev(4)), &device_r()).unwrap());
    assert_ne!(a, pre_address(&mk(&dev(5)), &device_r()).unwrap());
    assert!(pre_address(&enc("d4"), &device_r()).is_err());
    assert!(pre_address(&mk(&dev(4)), &bs58::encode([1u8; 15]).into_string()).is_err());
    assert_eq!(
        InboxAddress::Pre(a.clone()).text(),
        format!("{PRE_PREFIX}{a}")
    );
}

#[test]
fn a_pre_join_read_verifies_only_for_its_address_and_signature() {
    let auth = sign_pre_auth(
        &device_r(),
        &store_did(),
        "GET",
        "/v1/inbox/pre/x",
        "n",
        LINK_NOW,
        &dev(4),
    )
    .unwrap();
    assert_eq!(verify_pre_auth(&auth), Ok(()));
    let mut elsewhere = auth.clone();
    elsewhere.store = format!("did:key:{}", mk(&sk("another store")));
    assert_eq!(verify_pre_auth(&elsewhere), Err(InboxError::Signature));
    let mut moved = auth.clone();
    moved.address = pre_address(&mk(&dev(5)), &device_r()).unwrap();
    assert!(matches!(
        verify_pre_auth(&moved),
        Err(InboxError::Malformed(_))
    ));
    let mut other = auth.clone();
    other.method = "DELETE".into();
    assert_eq!(verify_pre_auth(&other), Err(InboxError::Signature));
}

#[test]
fn the_store_takes_only_base64url_up_to_8_kib() {
    let log = devices_log();
    let sealed = sealed_to_d1(&log, &master_payload(&log, 1, 0));
    assert_eq!(check_sealed(&sealed), Ok(()));
    assert!(check_sealed("not base64url!").is_err());
    assert!(check_sealed("").is_err());
    assert!(check_sealed(&"A".repeat(MAX_SEALED_BYTES + 1)).is_err());
}
