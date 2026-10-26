//! The DID document of a log (section 9 of the format spec): which keys it lists at a seq,
//! in which relationships, with which extension properties, and its JSON-LD form.
#![cfg(feature = "build")]

mod common;

use ad4m_identity_core::document::{COASYS_CONTEXT, DID_CONTEXT, MULTIKEY, MULTIKEY_CONTEXT};
use ad4m_identity_core::{
    did_document, BotScope, DidDocument, DocumentError, DocumentMetadata, KeyLife,
};
use common::*;
use serde_json::{json, Value};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

fn doc(log: &Log, version: Option<u64>) -> (DidDocument, DocumentMetadata) {
    did_document(&log.chain, version, &[]).unwrap()
}

fn ids(list: &[String]) -> Vec<&str> {
    list.iter().map(String::as_str).collect()
}

fn method_ids(doc: &DidDocument) -> Vec<&str> {
    doc.verification_method
        .iter()
        .map(|m| m.id.as_str())
        .collect()
}

fn method<'a>(doc: &'a DidDocument, id: &str) -> &'a ad4m_identity_core::VerificationMethod {
    doc.verification_method
        .iter()
        .find(|m| m.id == id)
        .unwrap_or_else(|| panic!("no method {id}"))
}

/// incept d1 (seq 0), add d2 (seq 1), add bot d3 (seq 2), revoke d2 (seq 3).
fn revoked_log() -> Log {
    let mut log = Log::new();
    log.add(KeyLife::Standing);
    log.add(bot(&["read", "write"], 1_750_000_000));
    log.revoke(&["d2"]);
    log
}

#[test]
fn the_head_document_lists_live_keys_and_keeps_bots_out_of_assertions() {
    let log = revoked_log();
    let (doc, meta) = doc(&log, None);
    let did = &log.chain.state().did;
    assert_eq!(&doc.id, did);
    assert_eq!(&doc.controller, did);
    // A bot has no inbox, so its X25519 key gets no method and no `keyAgreement`.
    assert_eq!(method_ids(&doc), ["#d1", "#d1-enc", "#d3"]);
    assert_eq!(ids(&doc.authentication), ["#d1"]);
    assert_eq!(ids(&doc.assertion_method), ["#d1"]);
    assert_eq!(ids(&doc.key_agreement), ["#d1-enc"]);
    assert_eq!(ids(&doc.capability_invocation), ["#d3"]);
    for m in &doc.verification_method {
        assert_eq!(m.type_, MULTIKEY);
        assert_eq!(&m.controller, did);
    }
    assert_eq!(method(&doc, "#d1").public_key_multibase, mk(&dev(1)));
    assert_eq!(method(&doc, "#d1-enc").public_key_multibase, enc("d1"));
    assert_eq!(
        method(&doc, "#d3").bot,
        Some(BotScope {
            can: vec!["read".into(), "write".into()],
            expires: 1_750_000_000
        })
    );
    assert_eq!(method(&doc, "#d1").bot, None);
    assert_eq!(
        meta,
        DocumentMetadata {
            version_id: 3,
            next_version_id: None,
            deactivated: false
        }
    );
}

#[test]
fn an_earlier_version_lists_the_keys_valid_then() {
    let log = revoked_log();
    let (doc, meta) = doc(&log, Some(1));
    assert_eq!(method_ids(&doc), ["#d1", "#d1-enc", "#d2", "#d2-enc"]);
    assert_eq!(ids(&doc.authentication), ["#d1", "#d2"]);
    assert_eq!(ids(&doc.assertion_method), ["#d1", "#d2"]);
    assert!(doc.capability_invocation.is_empty());
    assert_eq!(meta.version_id, 1);
    assert_eq!(meta.next_version_id, Some(2));
    // Version 0 holds only the incepted key; the head equals the last version.
    assert_eq!(method_ids(&self::doc(&log, Some(0)).0), ["#d1", "#d1-enc"]);
    assert_eq!(self::doc(&log, Some(3)), self::doc(&log, None));
}

#[test]
fn an_until_key_carries_its_expiry_and_signs_as_the_person() {
    let mut log = Log::new();
    log.add(KeyLife::Until(1_800_000_000));
    let (doc, _) = doc(&log, None);
    assert_eq!(method(&doc, "#d2").expires, Some(1_800_000_000));
    assert_eq!(method(&doc, "#d2-enc").expires, None);
    assert_eq!(method(&doc, "#d1").expires, None);
    assert_eq!(ids(&doc.authentication), ["#d1", "#d2"]);
    assert_eq!(ids(&doc.assertion_method), ["#d1", "#d2"]);
    // The expiry is a property of the method, not a filter: the document takes no time.
    let value = serde_json::to_value(&doc).unwrap();
    assert_eq!(
        value["verificationMethod"][2]["coasys:expires"],
        1_800_000_000
    );
}

#[test]
fn the_bot_scope_is_the_one_in_force_at_the_version() {
    let mut log = revoked_log();
    log.scope("d3", &["read"], 1_900_000_000); // seq 4
    let before = doc(&log, Some(3)).0;
    let after = doc(&log, Some(4)).0;
    assert_eq!(
        method(&before, "#d3").bot.as_ref().unwrap().expires,
        1_750_000_000
    );
    let scope = method(&after, "#d3").bot.clone().unwrap();
    assert_eq!(scope.can, ["read"]);
    assert_eq!(scope.expires, 1_900_000_000);
    assert_eq!(
        serde_json::to_value(method(&after, "#d3")).unwrap()["coasys:bot"],
        json!({"can": ["read"], "expires": 1_900_000_000u64})
    );
}

#[test]
fn masters_never_appear_and_a_rotate_changes_no_method() {
    let mut log = revoked_log();
    log.rotate(&[], false); // seq 4: m1
    let before = doc(&log, Some(3)).0;
    let after = doc(&log, Some(4)).0;
    assert_eq!(before.verification_method, after.verification_method);
    assert_eq!(before.authentication, after.authentication);
    assert_eq!(before.key_agreement, after.key_agreement);
    assert_eq!(before.capability_invocation, after.capability_invocation);
    let masters = [mk(&master(0)), mk(&master(1))];
    for d in [&before, &after] {
        assert!(d
            .verification_method
            .iter()
            .all(|m| !m.id.starts_with("#m")));
        assert!(d
            .verification_method
            .iter()
            .all(|m| !masters.contains(&m.public_key_multibase)));
    }
}

#[test]
fn a_rotate_that_revokes_drops_the_keys_from_its_seq() {
    let mut log = revoked_log();
    log.rotate(&["d1"], false); // seq 4
    assert_eq!(method_ids(&doc(&log, Some(4)).0), ["#d3"]);
    assert!(doc(&log, Some(4)).0.authentication.is_empty());
    assert_eq!(ids(&doc(&log, Some(3)).0.authentication), ["#d1"]);
}

#[test]
fn an_ended_log_is_deactivated_with_no_methods() {
    let mut log = revoked_log();
    log.rotate(&[], true); // seq 4: end
    let (head, meta) = doc(&log, None);
    assert!(head.verification_method.is_empty());
    assert!(head.authentication.is_empty());
    assert!(head.assertion_method.is_empty());
    assert!(head.key_agreement.is_empty());
    assert!(head.capability_invocation.is_empty());
    assert_eq!(
        meta,
        DocumentMetadata {
            version_id: 4,
            next_version_id: None,
            deactivated: true
        }
    );
    // An earlier version still shows its keys; the DID itself stays deactivated.
    let (earlier, meta) = doc(&log, Some(3));
    assert_eq!(method_ids(&earlier), ["#d1", "#d1-enc", "#d3"]);
    assert!(meta.deactivated);
    assert_eq!(meta.next_version_id, Some(4));
    // The relationships are written even when empty.
    let value = serde_json::to_value(&head).unwrap();
    for member in [
        "verificationMethod",
        "authentication",
        "assertionMethod",
        "keyAgreement",
        "capabilityInvocation",
    ] {
        assert_eq!(value[member], json!([]), "{member}");
    }
}

#[test]
fn a_version_past_the_head_is_refused() {
    let log = revoked_log();
    let err = did_document(&log.chain, Some(4), &[]).unwrap_err();
    assert_eq!(
        err,
        DocumentError::VersionPastHead {
            version: 4,
            head: 3
        }
    );
    assert_eq!(err.name(), "VersionPastHead");
    assert!(did_document(&log.chain, Some(u64::MAX), &[]).is_err());
}

#[test]
fn keys_are_listed_in_order_of_addition_past_nine() {
    let mut log = Log::new();
    for _ in 0..10 {
        log.add(KeyLife::Standing); // d2 … d11
    }
    let (doc, _) = doc(&log, None);
    let expected: Vec<String> = (1..=11).map(|n| format!("#d{n}")).collect();
    assert_eq!(doc.authentication, expected);
}

#[test]
fn the_json_ld_form_has_a_fixed_member_order() {
    let log = revoked_log();
    let did = log.chain.state().did.clone();
    let aka = vec![format!("{did}?src=store.example")];
    let (doc, meta) = did_document(&log.chain, None, &aka).unwrap();
    let text = serde_json::to_string(&doc).unwrap();
    let order = [
        "\"@context\"",
        "\"id\"",
        "\"controller\"",
        "\"verificationMethod\"",
        "\"authentication\"",
        "\"assertionMethod\"",
        "\"keyAgreement\"",
        "\"capabilityInvocation\"",
        "\"alsoKnownAs\"",
    ];
    let positions: Vec<usize> = order.iter().map(|k| text.find(k).unwrap()).collect();
    assert!(positions.windows(2).all(|w| w[0] < w[1]), "{text}");
    let value: Value = serde_json::from_str(&text).unwrap();
    assert_eq!(
        value["@context"],
        json!([DID_CONTEXT, MULTIKEY_CONTEXT, COASYS_CONTEXT])
    );
    assert_eq!(value["alsoKnownAs"], json!(aka));
    let first = serde_json::to_string(&doc.verification_method[0]).unwrap();
    assert_eq!(
        first,
        format!(
            "{{\"id\":\"#d1\",\"type\":\"Multikey\",\"controller\":\"{did}\",\"publicKeyMultibase\":\"{}\"}}",
            mk(&dev(1))
        )
    );
    // No alsoKnownAs given: the member is left out.
    let bare = serde_json::to_value(did_document(&log.chain, None, &[]).unwrap().0).unwrap();
    assert!(bare.get("alsoKnownAs").is_none());
    // Metadata: DID Core's string version ids.
    assert_eq!(
        serde_json::to_value(&meta).unwrap(),
        json!({"versionId": "3", "deactivated": false})
    );
    let (_, earlier) = did_document(&log.chain, Some(1), &[]).unwrap();
    assert_eq!(
        serde_json::to_string(&earlier).unwrap(),
        r#"{"versionId":"1","nextVersionId":"2","deactivated":false}"#
    );
}
