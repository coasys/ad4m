//! Offers, claims, offer strings and the core's part of the store's claim check
//! (`link::check_add`).
#![cfg(feature = "build")]

mod common;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

use ad4m_identity_core::build::{self, BuildError};
use ad4m_identity_core::link::*;
use ad4m_identity_core::{BotScope, KeyLife, Op};
use common::*;

fn bot_scope() -> BotScope {
    BotScope {
        can: vec!["read".into(), "write".into()],
        expires: 1_800_000_000,
    }
}

#[test]
fn a_signed_offer_at_the_head_verifies() {
    let log = Log::new();
    let offer = signed_offer(&log, link_offer(&log, OfferKind::Device, None));
    assert_eq!(verify_offer(&offer, &log.chain, LINK_NOW), Ok(()));
}

#[test]
fn an_offer_signed_by_a_device_key_is_refused() {
    let log = Log::new();
    let offer = sign_offer(
        link_offer(&log, OfferKind::Device, None),
        signed_by(&log),
        &dev(1),
    )
    .unwrap();
    assert_eq!(
        verify_offer(&offer, &log.chain, LINK_NOW),
        Err(LinkError::Signature)
    );
    let mut as_device = offer.clone();
    as_device.by.key_id = "d1".into();
    assert_eq!(
        verify_offer(&as_device, &log.chain, LINK_NOW),
        Err(LinkError::Signer)
    );
}

#[test]
fn an_offer_by_the_old_master_after_a_rotation_is_refused() {
    let mut log = Log::new();
    let old = signed_offer(&log, link_offer(&log, OfferKind::Device, None));
    log.rotate(&[], false);
    assert_eq!(
        verify_offer(&old, &log.chain, LINK_NOW),
        Err(LinkError::Signer)
    );
    let fresh = signed_offer(&log, link_offer(&log, OfferKind::Device, None));
    assert_eq!(verify_offer(&fresh, &log.chain, LINK_NOW), Ok(()));
}

#[test]
fn an_offer_at_a_moved_head_is_refused_as_moved() {
    let mut log = Log::new();
    let offer = signed_offer(&log, link_offer(&log, OfferKind::Device, None));
    log.revoke(&["d1"]);
    assert_eq!(
        verify_offer(&offer, &log.chain, LINK_NOW),
        Err(LinkError::Moved)
    );
}

#[test]
fn an_offer_expiring_more_than_300_s_ahead_or_in_the_past_is_refused() {
    let log = Log::new();
    let mut long = link_offer(&log, OfferKind::Device, None);
    long.expires = LINK_NOW + OFFER_TTL_S + 1;
    assert_eq!(
        verify_offer(&signed_offer(&log, long), &log.chain, LINK_NOW),
        Err(LinkError::TooLong)
    );
    let offer = signed_offer(&log, link_offer(&log, OfferKind::Device, None));
    assert_eq!(
        verify_offer(&offer, &log.chain, LINK_NOW + OFFER_TTL_S),
        Err(LinkError::Expired)
    );
}

#[test]
fn an_offer_signature_does_not_cover_another_offer() {
    let log = Log::new();
    let mut offer = signed_offer(&log, link_offer(&log, OfferKind::Device, None));
    offer.offer.kind = OfferKind::Node;
    assert_eq!(
        verify_offer(&offer, &log.chain, LINK_NOW),
        Err(LinkError::Signature)
    );
}

#[test]
fn a_bot_offer_carries_its_scope_and_no_other_offer_does() {
    let log = Log::new();
    let bot_without = signed_offer(&log, link_offer(&log, OfferKind::Bot, None));
    assert!(matches!(
        verify_offer(&bot_without, &log.chain, LINK_NOW),
        Err(LinkError::Malformed(_))
    ));
    let device_with = signed_offer(&log, link_offer(&log, OfferKind::Device, Some(bot_scope())));
    assert!(matches!(
        verify_offer(&device_with, &log.chain, LINK_NOW),
        Err(LinkError::Malformed(_))
    ));
    let bot = signed_offer(&log, link_offer(&log, OfferKind::Bot, Some(bot_scope())));
    assert_eq!(verify_offer(&bot, &log.chain, LINK_NOW), Ok(()));
}

#[test]
fn a_claim_verifies_and_its_cosig_covers_enc_key_and_can_keep_master() {
    let log = Log::new();
    let scid = log.chain.state().scid.clone();
    let claim = link_claim(&log, 2, true);
    assert_eq!(verify_claim(&scid, &claim), Ok(()));

    let mut flipped = claim.clone();
    flipped.can_keep_master = false;
    assert_eq!(verify_claim(&scid, &flipped), Err(LinkError::CoSig));

    let mut swapped = claim.clone();
    swapped.enc_key = enc("someone else");
    assert_eq!(verify_claim(&scid, &swapped), Err(LinkError::CoSig));

    assert_eq!(verify_claim("QmOther", &claim), Err(LinkError::CoSig));
}

#[test]
fn a_claim_with_the_wrong_secret_fails_the_commit() {
    let log = Log::new();
    let offer = link_offer(&log, OfferKind::Device, None);
    let claim = link_claim(&log, 2, false);
    assert!(check_secret(&offer, &claim));
    let mut wrong = claim;
    wrong.s = bs58::encode([7u8; 32]).into_string();
    assert!(!check_secret(&offer, &wrong));
}

#[test]
fn a_claim_with_a_long_hint_or_bad_keys_is_malformed() {
    let log = Log::new();
    let scid = log.chain.state().scid.clone();
    let mut long_hint = link_claim(&log, 2, false);
    long_hint.hint = Some("x".repeat(65));
    assert!(matches!(
        verify_claim(&scid, &long_hint),
        Err(LinkError::Malformed(_))
    ));
    let mut enc_as_sign = link_claim(&log, 2, false);
    enc_as_sign.enc_key = enc_as_sign.device_key.clone();
    assert!(matches!(
        verify_claim(&scid, &enc_as_sign),
        Err(LinkError::Malformed(_))
    ));
}

#[test]
fn the_match_code_is_the_same_on_both_sides_and_names_the_key() {
    let log = Log::new();
    let scid = log.chain.state().scid.clone();
    let claim = link_claim(&log, 2, false);
    let holder = match_code(&scid, &link_offer_id(), &claim.device_key);
    let device = match_code(&scid, &claim.offer_id, &mk(&dev(2)));
    assert_eq!(holder, device);
    assert_ne!(holder, match_code(&scid, &link_offer_id(), &mk(&dev(3))));
}

fn claimed(log: &Log, n: u32) -> ClaimView {
    claim_view(&link_claim(log, n, false), LINK_NOW + 5)
}

#[test]
fn add_claimed_spends_the_claim_and_folds() {
    let mut log = Log::new();
    let offer = signed_offer(&log, link_offer(&log, OfferKind::Device, None));
    let view = claimed(&log, 2);
    let add = build::add_claimed(&log.chain, &master(0), &offer.offer, &view, None).unwrap();
    assert_eq!(
        check_add(&offer, &view, log.chain.state(), &add, LINK_NOW),
        Ok(())
    );
    let Op::Add { add: keys, .. } = &add.op else {
        panic!("not an add")
    };
    assert_eq!(keys[0].life, KeyLife::Standing);
    assert_eq!(keys[0].id, "d2");
    log.push(add);
    assert!(log.chain.state().key_valid_at(&mk(&dev(2)), 1));
}

#[test]
fn add_claimed_copies_a_bot_offers_scope_and_sets_an_until_expiry() {
    let log = Log::new();
    let bot = signed_offer(&log, link_offer(&log, OfferKind::Bot, Some(bot_scope())));
    let view = claimed(&log, 2);
    let add = build::add_claimed(&log.chain, &master(0), &bot.offer, &view, None).unwrap();
    let Op::Add { add: keys, .. } = &add.op else {
        panic!("not an add")
    };
    assert_eq!(keys[0].life, KeyLife::Bot(bot_scope()));
    assert_eq!(
        build::add_claimed(&log.chain, &master(0), &bot.offer, &view, Some(5)),
        Err(BuildError::OfferShape)
    );

    let device = signed_offer(&log, link_offer(&log, OfferKind::Device, None));
    let add = build::add_claimed(
        &log.chain,
        &master(0),
        &device.offer,
        &view,
        Some(1_750_000_000),
    )
    .unwrap();
    let Op::Add { add: keys, .. } = &add.op else {
        panic!("not an add")
    };
    assert_eq!(keys[0].life, KeyLife::Until(1_750_000_000));
}

#[test]
fn check_add_refuses_each_condition_alone() {
    let log = Log::new();
    let offer = signed_offer(&log, link_offer(&log, OfferKind::Device, None));
    let view = claimed(&log, 2);
    let good = build::add_claimed(&log.chain, &master(0), &offer.offer, &view, None).unwrap();
    let state = log.chain.state();
    let claim_err = |r: Result<(), LinkError>| matches!(r, Err(LinkError::Claim(_)));

    // 2. The offer's signer is no longer the current master.
    let mut rotated = Log::new();
    let old_offer = signed_offer(&rotated, link_offer(&rotated, OfferKind::Device, None));
    rotated.rotate(&[], false);
    let after =
        build::add_claimed(&rotated.chain, &master(1), &old_offer.offer, &view, None).unwrap();
    assert!(claim_err(check_add(
        &old_offer,
        &view,
        rotated.chain.state(),
        &after,
        LINK_NOW
    )));

    // 3. Past expires plus the confirm grace.
    let late = offer.offer.expires + CONFIRM_GRACE_S;
    assert!(claim_err(check_add(&offer, &view, state, &good, late)));
    assert_eq!(check_add(&offer, &view, state, &good, late - 1), Ok(()));

    // 4. The key, its encryption key or the co-signature differ from the claim's.
    let other = claimed(&log, 3);
    let other_add = build::add_claimed(&log.chain, &master(0), &offer.offer, &other, None).unwrap();
    assert!(claim_err(check_add(
        &offer, &view, state, &other_add, LINK_NOW
    )));
    let mut other_enc = view.clone();
    other_enc.enc_key = enc("other");
    let enc_add =
        build::add_claimed(&log.chain, &master(0), &offer.offer, &other_enc, None).unwrap();
    assert!(claim_err(check_add(
        &offer, &view, state, &enc_add, LINK_NOW
    )));
    let mut other_sig = view.clone();
    other_sig.co_sig = "z1".into();
    let sig_add =
        build::add_claimed(&log.chain, &master(0), &offer.offer, &other_sig, None).unwrap();
    assert!(claim_err(check_add(
        &offer, &view, state, &sig_add, LINK_NOW
    )));

    // 5. A bot offer with an add without the mark, and a device offer with a bot key.
    let bot = signed_offer(&log, link_offer(&log, OfferKind::Bot, Some(bot_scope())));
    assert!(claim_err(check_add(&bot, &view, state, &good, LINK_NOW)));
    let bot_add = build::add_claimed(&log.chain, &master(0), &bot.offer, &view, None).unwrap();
    assert!(claim_err(check_add(
        &offer, &view, state, &bot_add, LINK_NOW
    )));
    let mut other_scope = bot.clone();
    other_scope.offer.bot = Some(BotScope {
        can: vec!["read".into()],
        expires: 1_800_000_000,
    });
    assert!(claim_err(check_add(
        &other_scope,
        &view,
        state,
        &bot_add,
        LINK_NOW
    )));

    // The add names another offer.
    let mut renamed = offer.clone();
    renamed.offer.offer_id = bs58::encode([9u8; 16]).into_string();
    assert!(claim_err(check_add(
        &renamed, &view, state, &good, LINK_NOW
    )));
}

fn offer_string() -> OfferString {
    OfferString {
        store: "https://store.coasys.org".into(),
        scid: Log::new().chain.state().scid.clone(),
        offer_id: link_offer_id(),
        s: bs58::encode(link_secret()).into_string(),
    }
}

#[test]
fn the_offer_string_round_trips_bare_and_as_a_link() {
    let string = offer_string();
    assert!(string.encode().starts_with("coasys:offer:1:"));
    assert!(string
        .link()
        .starts_with("https://coasys.id/o#coasys:offer:1:"));
    assert_eq!(OfferString::parse(&string.encode()).unwrap(), string);
    assert_eq!(OfferString::parse_link(&string.link()).unwrap(), string);
    assert_eq!(OfferString::parse_link(&string.encode()).unwrap(), string);
}

#[test]
fn the_offer_string_parser_refuses_other_prefixes_short_codes_and_extra_fields() {
    let string = offer_string();
    let body = string.encode()["coasys:offer:1:".len()..].to_string();
    assert!(OfferString::parse(&format!("coasys:offer:2:{body}")).is_err());
    assert!(OfferString::parse(&format!("coasys:device:1:{body}")).is_err());
    assert!(OfferString::parse("R7KQ2M").is_err());
    assert!(OfferString::parse_link("R7K Q2M").is_err());

    let mut value = serde_json::to_value(&string).unwrap();
    value["kind"] = "device".into();
    let text = ad4m_hash_chain::canonical_json(&value).unwrap();
    let extra = format!("coasys:offer:1:{}", bs58::encode(text).into_string());
    assert!(OfferString::parse(&extra).is_err());

    let mut short = string.clone();
    short.s = bs58::encode([1u8; 16]).into_string();
    assert!(OfferString::parse(&short.encode()).is_err());
}

#[test]
fn an_offer_link_with_the_string_in_a_query_or_path_is_refused() {
    let string = offer_string().encode();
    assert!(OfferString::parse_link(&format!("https://coasys.id/o?o={string}")).is_err());
    assert!(OfferString::parse_link(&format!("https://coasys.id/o?x=1#{string}")).is_err());
    assert!(OfferString::parse_link(&format!("https://coasys.id/{string}#{string}")).is_err());
}

#[test]
fn the_device_string_round_trips_and_refuses_extra_fields() {
    let device = DeviceString {
        sign: mk(&dev(2)),
        enc: enc("d2"),
        r: bs58::encode([3u8; 16]).into_string(),
        exp: LINK_NOW + 300,
        hint: None,
        store: "https://store.coasys.org".into(),
    };
    assert_eq!(DeviceString::parse(&device.encode()).unwrap(), device);
    assert!(OfferString::parse(&device.encode()).is_err());
    let mut value = serde_json::to_value(&device).unwrap();
    value["s"] = "secret".into();
    let text = ad4m_hash_chain::canonical_json(&value).unwrap();
    assert!(DeviceString::parse(&format!(
        "coasys:device:1:{}",
        bs58::encode(text).into_string()
    ))
    .is_err());
}

#[test]
fn offer_states_name_themselves_on_the_wire() {
    let view = claimed(&Log::new(), 2);
    let claimed_state = OfferState::Claimed { claim: view };
    let json = serde_json::to_value(&claimed_state).unwrap();
    assert_eq!(json["state"], "claimed");
    assert_eq!(json["claim"]["deviceKey"], mk(&dev(2)));
    assert_eq!(
        serde_json::to_value(OfferState::Spent { seq: 4 }).unwrap(),
        serde_json::json!({"state": "spent", "seq": 4})
    );
    for state in [
        OfferState::Open,
        OfferState::Expired,
        OfferState::Cancelled,
        OfferState::Contested,
        OfferState::Locked,
        OfferState::Superseded,
    ] {
        assert_eq!(serde_json::to_value(&state).unwrap()["state"], state.name());
    }
    assert!(claimed_state.is_live() && OfferState::Open.is_live() && !OfferState::Locked.is_live());
}
