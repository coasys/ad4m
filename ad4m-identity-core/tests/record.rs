//! The private details record: sealed to every device key without the bot mark and to the
//! recovery key, opened by each, refused by the rest; its JSON; the store's check.
#![cfg(feature = "build")]

mod common;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

use ad4m_identity_core::record::*;
use ad4m_identity_core::{x25519_multikey_of, KelChain, KeyLife, X25519Secret};
use common::*;
use ed25519_dalek::SigningKey;
use serde_json::json;

/// The store's head key in these tests.
fn store_key() -> SigningKey {
    sk("the identity store")
}

/// The store keys a reader trusts for signed heads.
fn stores() -> Vec<String> {
    vec![mk(&store_key())]
}

/// The store's receipt for `s`, accepted when the log was `chain` (at its head).
fn receipt(s: &SealedRecord, chain: &KelChain) -> RecordReceipt {
    sign_receipt(chain.id(), s, chain.head().seq, &store_key()).unwrap()
}

/// [`open_record`] of `s` as a reader fetches it right after the store accepted it: with the
/// store's receipt at `chain`'s head.
fn open(
    s: &SealedRecord,
    chain: &KelChain,
    entry: &str,
    secret: &X25519Secret,
) -> Result<PrivateRecord, RecordError> {
    open_record(s, &receipt(s, chain), chain, &stores(), entry, secret)
}

/// A record of `version` signed by d1.
fn sealed(log: &Log, version: u64, recovery: bool) -> SealedRecord {
    sealed_by(log, version, recovery, &dev(1))
}

/// A record of `version` signed by `signer`, with the fixture recovery key attested by the
/// log's current master when `recovery`.
fn sealed_by(log: &Log, version: u64, recovery: bool, signer: &SigningKey) -> SealedRecord {
    let recovery = recovery.then(|| attest(log, &x25519_multikey_of(&recovery_secret_fixture())));
    seal_record(
        &log.chain,
        version,
        &record_fixture(),
        recovery,
        signer,
        &mut TestRng::new(&format!("record {version}")),
    )
    .unwrap()
}

/// Signs `s` again with `key` after a test changed it, as a writer of the identity would.
fn resign(s: &mut SealedRecord, log: &Log, key: &SigningKey) {
    let bytes = envelope_signing_bytes(&scid(log), s).unwrap();
    s.sig =
        ad4m_hash_chain::encode_proof_value(&ad4m_identity_core::build::Signer::sign(key, &bytes));
}

/// `key` attested as the recovery key by the log's current master.
fn attest(log: &Log, key: &str) -> Recovery {
    attest_recovery_key(&log.chain, key.to_string(), &master(log.n)).unwrap()
}

fn scid(log: &Log) -> String {
    log.chain.state().scid.clone()
}

#[test]
fn the_record_is_wrapped_to_every_device_key_and_the_recovery_key_and_not_to_a_bot() {
    let log = devices_log();
    let s = sealed(&log, 1, true);
    let entries: Vec<&str> = s.wrapped.keys().map(String::as_str).collect();
    assert_eq!(entries, ["d1", "d2", "recovery"]);
    for (entry, secret) in [
        ("d1", enc_secret(1)),
        ("d2", enc_secret(2)),
        ("recovery", recovery_secret_fixture()),
    ] {
        assert_eq!(
            open(&s, &log.chain, entry, &secret),
            Ok(record_fixture()),
            "{entry}"
        );
    }
    // The bot key d3 has no entry, and its secret opens no other entry.
    assert_eq!(
        open(&s, &log.chain, "d3", &enc_secret(3)),
        Err(RecordError::NoEntry("d3".into()))
    );
    assert_eq!(
        open(&s, &log.chain, "d1", &enc_secret(3)),
        Err(RecordError::Open)
    );
    assert_eq!(
        check_sealed(&s, &log.chain),
        Ok(Writer::Device("d1".into()))
    );
}

/// A reader that holds its `enc` secret elsewhere (an executor's wallet) unwraps the record key
/// there and decrypts with the key alone; the key of one version opens no other, and the
/// envelope is still verified against the log first: a key does not make an unsigned or
/// re-signed-by-no-one envelope count.
#[test]
fn the_record_key_unwrapped_alone_decrypts_its_version_only() {
    let log = devices_log();
    let v1 = sealed(&log, 1, false);
    let key = unwrap_key(&v1.wrapped["d2"], &scid(&log), &enc_secret(2)).unwrap();
    assert_eq!(decrypt_record(&v1, &log.chain, &key), Ok(record_fixture()));
    assert_eq!(
        unwrap_key(&v1.wrapped["d2"], &scid(&log), &enc_secret(1)).map(|k| *k),
        Err(RecordError::Open)
    );
    let v2 = sealed(&log, 2, false);
    assert_eq!(
        decrypt_record(&v2, &log.chain, &key),
        Err(RecordError::Open)
    );
    let mut unsigned = v1.clone();
    unsigned.sig = String::new();
    assert!(decrypt_record(&unsigned, &log.chain, &key).is_err());
    let mut renamed = v1.clone();
    renamed.signer = "d2".into();
    assert!(decrypt_record(&renamed, &log.chain, &key).is_err());
}

/// A writer whose key signs elsewhere (an app whose device key is in its executor's wallet)
/// seals without the signature, naming its key; the signature added later makes the same
/// envelope a device's write. Only the named key may sign it, and only a writer of the log
/// may be named.
#[test]
fn an_envelope_sealed_unsigned_counts_once_its_named_key_signs_it() {
    let log = devices_log();
    let mut s = seal_unsigned(
        &log.chain,
        1,
        &record_fixture(),
        None,
        "d1".into(),
        &mut TestRng::new("unsigned"),
    )
    .unwrap();
    assert!(s.sig.is_empty());
    assert!(check_sealed(&s, &log.chain).is_err());
    assert!(matches!(
        sign_envelope(&log.chain, &mut s.clone(), &dev(2)),
        Err(RecordError::Signer(_))
    ));
    sign_envelope(&log.chain, &mut s, &dev(1)).unwrap();
    assert_eq!(
        check_sealed(&s, &log.chain),
        Ok(Writer::Device("d1".into()))
    );
    assert_eq!(
        open(&s, &log.chain, "d2", &enc_secret(2)),
        Ok(record_fixture())
    );
    assert!(seal_unsigned(
        &log.chain,
        1,
        &record_fixture(),
        None,
        "d9".into(),
        &mut TestRng::new("unsigned"),
    )
    .is_err());
}

#[test]
fn a_revoked_device_cannot_open_the_next_version() {
    let mut log = devices_log();
    let v1 = sealed(&log, 1, true);
    log.revoke(&["d2"]);
    let v2 = sealed(&log, 2, true);
    let entries: Vec<&str> = v2.wrapped.keys().map(String::as_str).collect();
    assert_eq!(entries, ["d1", "recovery"]);
    // d2's wrap of version 1 holds the old record key, not the new one.
    let mut spliced = v2.clone();
    spliced
        .wrapped
        .insert("d2".into(), v1.wrapped["d2"].clone());
    resign(&mut spliced, &log, &dev(1));
    assert_eq!(
        open(&spliced, &log.chain, "d2", &enc_secret(2)),
        Err(RecordError::Open)
    );
    assert_eq!(
        open(&v2, &log.chain, "d1", &enc_secret(1)),
        Ok(record_fixture())
    );
}

#[test]
fn every_version_has_a_fresh_record_key_and_nonce() {
    let log = devices_log();
    let (a, b) = (sealed(&log, 1, false), sealed(&log, 2, false));
    assert_ne!(a.nonce, b.nonce);
    assert_ne!(a.wrapped["d1"], b.wrapped["d1"]);
}

#[test]
fn a_ciphertext_moved_to_another_version_identity_or_recovery_key_does_not_open() {
    let log = devices_log();
    // Even signed again by a writer of the identity, the AAD binds the ciphertext.
    let mut s = sealed(&log, 1, false);
    s.version = 2;
    resign(&mut s, &log, &dev(1));
    assert_eq!(
        open(&s, &log.chain, "d1", &enc_secret(1)),
        Err(RecordError::Open)
    );
    let mut s = sealed(&log, 1, true);
    let other = attest(&log, &x25519_multikey_of(&enc_secret(9)));
    s.recovery_key = Some(other.key);
    s.recovery_attestation = Some(other.attestation);
    resign(&mut s, &log, &dev(1));
    assert_eq!(
        open(&s, &log.chain, "d1", &enc_secret(1)),
        Err(RecordError::Open)
    );
    // Another identity whose d1 is the same key: the signature names the scid.
    let s = sealed(&log, 1, false);
    let other = Log::new();
    assert_eq!(
        open(&s, &other.chain, "d1", &enc_secret(1)),
        Err(RecordError::Signature)
    );
}

#[test]
fn a_version_past_the_safe_integers_is_refused_not_a_panic() {
    let log = devices_log();
    let mut s = sealed(&log, 1, false);
    let accepted = receipt(&s, &log.chain);
    s.version = 1 << 53;
    assert!(matches!(
        open_record(&s, &accepted, &log.chain, &stores(), "d1", &enc_secret(1)),
        Err(RecordError::Malformed(_))
    ));
    assert!(matches!(
        RecordReceipt::unsigned(&scid(&log), &s, 1, "did:key:z"),
        Err(RecordError::Malformed(_))
    ));
    assert!(matches!(
        seal_record(
            &log.chain,
            1 << 53,
            &record_fixture(),
            None,
            &dev(1),
            &mut TestRng::new("big")
        ),
        Err(RecordError::Malformed(_))
    ));
}

#[test]
fn a_tampered_ciphertext_does_not_open() {
    let log = devices_log();
    let mut s = sealed(&log, 1, false);
    let mut bytes = s.ciphertext.into_bytes();
    bytes[3] = if bytes[3] == b'A' { b'B' } else { b'A' };
    s.ciphertext = String::from_utf8(bytes).unwrap();
    assert_eq!(
        open(&s, &log.chain, "d1", &enc_secret(1)),
        Err(RecordError::Signature)
    );
    resign(&mut s, &log, &dev(1));
    assert_eq!(
        open(&s, &log.chain, "d1", &enc_secret(1)),
        Err(RecordError::Open)
    );
}

#[test]
fn a_record_without_a_recovery_key_has_no_recovery_entry() {
    let log = devices_log();
    let s = sealed(&log, 1, false);
    assert_eq!(s.recovery_key, None);
    assert!(!s.wrapped.contains_key(RECOVERY_ENTRY));
    let value = serde_json::to_value(&s).unwrap();
    assert!(value.get("recoveryKey").is_none());
    assert_eq!(serde_json::from_value::<SealedRecord>(value).unwrap(), s);
    // A later write that sets the recovery key wraps to it.
    let s = sealed(&log, 2, true);
    assert!(s.wrapped.contains_key(RECOVERY_ENTRY));
}

#[test]
fn the_store_refuses_a_record_not_wrapped_to_exactly_the_logs_readers() {
    let mut log = devices_log();
    let v1 = sealed(&log, 1, true);
    // A key added since: d4 is missing.
    log.add_key(sealable_device(4, KeyLife::Standing));
    assert!(matches!(
        check_sealed(&v1, &log.chain),
        Err(RecordError::Wrapped(_))
    ));
    // A revoked key still wrapped.
    let mut log = devices_log();
    log.revoke(&["d2"]);
    assert!(matches!(
        check_sealed(&v1, &log.chain),
        Err(RecordError::Wrapped(_))
    ));
    // A bot key wrapped.
    let log = devices_log();
    let mut with_bot = sealed(&log, 1, true);
    with_bot
        .wrapped
        .insert("d3".into(), with_bot.wrapped["d1"].clone());
    resign(&mut with_bot, &log, &dev(1));
    assert!(matches!(
        check_sealed(&with_bot, &log.chain),
        Err(RecordError::Wrapped(_))
    ));
    // A recovery entry without a recovery key, and the reverse.
    let mut no_key = sealed(&log, 1, true);
    no_key.recovery_key = None;
    no_key.recovery_attestation = None;
    resign(&mut no_key, &log, &dev(1));
    assert!(matches!(
        check_sealed(&no_key, &log.chain),
        Err(RecordError::Wrapped(_))
    ));
    let mut no_entry = sealed(&log, 1, true);
    no_entry.wrapped.remove(RECOVERY_ENTRY);
    resign(&mut no_entry, &log, &dev(1));
    assert!(matches!(
        check_sealed(&no_entry, &log.chain),
        Err(RecordError::Wrapped(_))
    ));
}

#[test]
fn the_store_refuses_a_malformed_record() {
    let log = devices_log();
    let good = sealed(&log, 1, true);
    let mut bad = good.clone();
    bad.nonce = "short".into();
    assert!(matches!(
        check_sealed(&bad, &log.chain),
        Err(RecordError::Malformed(_))
    ));
    let mut bad = good.clone();
    bad.ciphertext = "not base64!".into();
    assert!(matches!(
        check_sealed(&bad, &log.chain),
        Err(RecordError::Malformed(_))
    ));
    let mut bad = good.clone();
    bad.recovery_key = Some(mk(&dev(1)));
    assert!(matches!(
        check_sealed(&bad, &log.chain),
        Err(RecordError::Malformed(_))
    ));
    let mut bad = good;
    bad.version = 1 << 53;
    assert!(matches!(
        check_sealed(&bad, &log.chain),
        Err(RecordError::Malformed(_))
    ));
}

// ----- the writer's signature -----

#[test]
fn a_forged_envelope_is_refused_by_the_reader_and_the_store() {
    let log = devices_log();
    let good = sealed(&log, 1, true);
    // No signature.
    let mut forged = good.clone();
    forged.sig = String::new();
    assert_eq!(
        open(&forged, &log.chain, "d1", &enc_secret(1)),
        Err(RecordError::Signature)
    );
    assert_eq!(
        check_sealed(&forged, &log.chain),
        Err(RecordError::Signature)
    );
    // Signed by a key outside the log, naming d1.
    let mut forged = good.clone();
    resign(&mut forged, &log, &sk("a hostile store"));
    assert_eq!(
        open(&forged, &log.chain, "d1", &enc_secret(1)),
        Err(RecordError::Signature)
    );
    assert_eq!(
        check_sealed(&forged, &log.chain),
        Err(RecordError::Signature)
    );
    // Naming a key the log does not hold.
    let mut forged = good.clone();
    forged.signer = "d9".into();
    assert_eq!(
        open(&forged, &log.chain, "d1", &enc_secret(1)),
        Err(RecordError::Signer("d9".into()))
    );
    assert_eq!(
        check_sealed(&forged, &log.chain),
        Err(RecordError::Signer("d9".into()))
    );
}

#[test]
fn every_field_of_the_envelope_is_signed() {
    let log = devices_log();
    let good = sealed(&log, 1, true);
    type Change = Box<dyn Fn(&mut SealedRecord)>;
    let changes: Vec<(&str, Change)> = vec![
        ("version", Box::new(|s| s.version = 2)),
        (
            "wrapped",
            Box::new(|s| {
                let d2 = s.wrapped["d2"].clone();
                s.wrapped.insert("d1".into(), d2);
            }),
        ),
        (
            "recoveryKey",
            Box::new(|s| s.recovery_key = Some(x25519_multikey_of(&enc_secret(9)))),
        ),
        (
            "recoveryAttestation",
            Box::new(|s| s.recovery_attestation.as_mut().unwrap().sig.push('1')),
        ),
        (
            "nonce",
            Box::new(|s| s.nonce = bs58::encode([7u8; 24]).into_string()),
        ),
        ("ciphertext", Box::new(|s| s.ciphertext.push('A'))),
        ("signer", Box::new(|s| s.signer = "d2".into())),
    ];
    for (field, change) in changes {
        let mut s = good.clone();
        change(&mut s);
        assert_eq!(
            open(&s, &log.chain, "d2", &enc_secret(2)),
            Err(RecordError::Signature),
            "{field}"
        );
        assert_eq!(
            check_sealed(&s, &log.chain),
            Err(RecordError::Signature),
            "{field}"
        );
    }
}

#[test]
fn a_swapped_recovery_key_is_refused() {
    // A store swaps in its own recovery key, keeping the writer's signature over the original.
    let log = devices_log();
    let mut s = sealed(&log, 1, true);
    let store_key = X25519Secret::from([7u8; 32]);
    s.recovery_key = Some(x25519_multikey_of(&store_key));
    assert_eq!(
        open(&s, &log.chain, "d1", &enc_secret(1)),
        Err(RecordError::Signature)
    );
    assert_eq!(check_sealed(&s, &log.chain), Err(RecordError::Signature));
}

#[test]
fn a_revoked_or_bot_device_cannot_sign_a_record() {
    let mut log = devices_log();
    let by_d2 = sealed_by(&log, 1, false, &dev(2));
    let accepted = receipt(&by_d2, &log.chain);
    assert_eq!(
        open(&by_d2, &log.chain, "d1", &enc_secret(1)),
        Ok(record_fixture())
    );
    log.revoke(&["d2"]);
    // The envelope d2 wrote before its revocation still opens for d1 with the store's receipt
    // from before the revocation; a receipt naming a seq at or after it does not.
    assert_eq!(
        open_record(
            &by_d2,
            &accepted,
            &log.chain,
            &stores(),
            "d1",
            &enc_secret(1)
        ),
        Ok(record_fixture())
    );
    assert_eq!(
        open(&by_d2, &log.chain, "d1", &enc_secret(1)),
        Err(RecordError::Signer("d2".into()))
    );
    // The store refuses it as a new write.
    assert_eq!(
        check_sealed(&by_d2, &log.chain),
        Err(RecordError::Signer("d2".into()))
    );
    // Nor can it write a new one.
    assert_eq!(
        seal_record(
            &log.chain,
            2,
            &record_fixture(),
            None,
            &dev(2),
            &mut TestRng::new("revoked")
        ),
        Err(RecordError::Signer("d2".into()))
    );
    // The bot key d3 neither seals nor signs one.
    assert_eq!(
        seal_record(
            &log.chain,
            2,
            &record_fixture(),
            None,
            &dev(3),
            &mut TestRng::new("bot")
        ),
        Err(RecordError::Signer("d3".into()))
    );
    let mut by_bot = sealed(&log, 2, false);
    by_bot.signer = "d3".into();
    resign(&mut by_bot, &log, &dev(3));
    assert_eq!(
        open(&by_bot, &log.chain, "d1", &enc_secret(1)),
        Err(RecordError::Signer("d3".into()))
    );
    // A key the log does not hold cannot seal.
    assert!(matches!(
        seal_record(
            &log.chain,
            2,
            &record_fixture(),
            None,
            &sk("stranger"),
            &mut TestRng::new("stranger")
        ),
        Err(RecordError::Signer(_))
    ));
}

#[test]
fn the_current_master_signs_a_record_and_a_replaced_one_does_not() {
    let mut log = devices_log();
    let by_master = sealed_by(&log, 1, true, &master(log.n));
    assert_eq!(by_master.signer, "m0");
    assert_eq!(
        check_sealed(&by_master, &log.chain),
        Ok(Writer::Master("m0".into()))
    );
    let accepted = receipt(&by_master, &log.chain);
    assert_eq!(
        open(&by_master, &log.chain, "d2", &enc_secret(2)),
        Ok(record_fixture())
    );
    log.rotate(&[], false);
    // The record m0 wrote before the rotation still opens with the receipt from then; m0 can
    // neither write a new one nor pass a receipt from after the rotation.
    assert_eq!(
        open_record(
            &by_master,
            &accepted,
            &log.chain,
            &stores(),
            "d2",
            &enc_secret(2)
        ),
        Ok(record_fixture())
    );
    assert_eq!(
        open(&by_master, &log.chain, "d2", &enc_secret(2)),
        Err(RecordError::Signer("m0".into()))
    );
    assert_eq!(
        check_sealed(&by_master, &log.chain),
        Err(RecordError::Signer("m0".into()))
    );
    assert_eq!(
        seal_record(
            &log.chain,
            2,
            &record_fixture(),
            None,
            &master(0),
            &mut TestRng::new("old master")
        ),
        Err(RecordError::Signer("m0".into()))
    );
    let by_new = sealed_by(&log, 2, false, &master(log.n));
    assert_eq!(
        check_sealed(&by_new, &log.chain),
        Ok(Writer::Master("m1".into()))
    );
}

// ----- the recovery key's attestation -----

/// `s` carrying `key` with `attestation`, signed again by d1 (as a device that forged them).
fn with_recovery(
    log: &Log,
    s: &SealedRecord,
    key: &str,
    attestation: RecoveryAttestation,
) -> SealedRecord {
    let mut s = s.clone();
    s.recovery_key = Some(key.to_string());
    s.recovery_attestation = Some(attestation);
    resign(&mut s, log, &dev(1));
    s
}

/// An attestation of `key` by `signer` under the id `key_id`.
fn attestation_by(log: &Log, key: &str, key_id: &str, signer: &SigningKey) -> RecoveryAttestation {
    let bytes = attestation_signing_bytes(&scid(log), key, key_id).unwrap();
    RecoveryAttestation {
        key_id: key_id.into(),
        sig: ad4m_hash_chain::encode_proof_value(&ad4m_identity_core::build::Signer::sign(
            signer, &bytes,
        )),
    }
}

#[test]
fn a_recovery_key_counts_only_with_the_current_masters_attestation() {
    let log = devices_log();
    let key = x25519_multikey_of(&recovery_secret_fixture());
    let good = sealed(&log, 1, true);
    let attested = recovery_of(&good, &log.chain).unwrap();
    assert_eq!(attested.key, key);
    assert_eq!(attested.attestation.key_id, "m0");
    assert_eq!(
        check_sealed(&good, &log.chain),
        Ok(Writer::Device("d1".into()))
    );

    // A recovery key with no attestation, or an attestation with no key: malformed.
    let mut bare = good.clone();
    bare.recovery_attestation = None;
    resign(&mut bare, &log, &dev(1));
    let mut orphan = sealed(&log, 1, false);
    orphan.recovery_attestation = Some(attested.attestation.clone());
    resign(&mut orphan, &log, &dev(1));
    for s in [&bare, &orphan] {
        assert!(matches!(
            check_sealed(s, &log.chain),
            Err(RecordError::Malformed(_))
        ));
        assert!(matches!(
            open(s, &log.chain, "d1", &enc_secret(1)),
            Err(RecordError::Malformed(_))
        ));
        assert_eq!(recovery_of(s, &log.chain), None);
    }

    // A device attests a key of its own choosing, under its id or the master's: refused by the
    // store and the reader, and it does not count.
    for key_id in ["d1", "m0"] {
        let forged = with_recovery(
            &log,
            &good,
            &key,
            attestation_by(&log, &key, key_id, &dev(1)),
        );
        assert!(
            matches!(
                check_sealed(&forged, &log.chain),
                Err(RecordError::Attestation(_))
            ),
            "{key_id}"
        );
        assert!(
            matches!(
                open(&forged, &log.chain, "d1", &enc_secret(1)),
                Err(RecordError::Attestation(_))
            ),
            "{key_id}"
        );
        assert_eq!(recovery_of(&forged, &log.chain), None, "{key_id}");
    }
    // The master's attestation of one key carried with another.
    let other = x25519_multikey_of(&enc_secret(9));
    let moved = with_recovery(&log, &good, &other, attested.attestation.clone());
    assert!(matches!(
        check_sealed(&moved, &log.chain),
        Err(RecordError::Attestation(_))
    ));
    assert!(matches!(
        open(&moved, &log.chain, "d1", &enc_secret(1)),
        Err(RecordError::Attestation(_))
    ));
    // The attestation names the identity: another identity's master does not attest this one.
    let stranger = Log::new();
    let elsewhere = attestation_by(&stranger, &key, "m0", &master(0));
    assert!(matches!(
        check_sealed(&with_recovery(&log, &good, &key, elsewhere), &log.chain),
        Err(RecordError::Attestation(_))
    ));

    // A writer cannot seal a key the current master did not attest, and only the current master
    // attests.
    let unattested = Recovery {
        key: other.clone(),
        attestation: attested.attestation.clone(),
    };
    assert!(matches!(
        seal_record(
            &log.chain,
            1,
            &record_fixture(),
            Some(unattested),
            &dev(1),
            &mut TestRng::new("unattested")
        ),
        Err(RecordError::Attestation(_))
    ));
    assert!(matches!(
        attest_recovery_key(&log.chain, other.clone(), &dev(1)),
        Err(RecordError::Attestation(_))
    ));
    assert!(matches!(
        attest_recovery_key(&log.chain, mk(&dev(1)), &master(0)),
        Err(RecordError::Malformed(_))
    ));
}

#[test]
fn the_master_replaces_the_recovery_key_and_a_device_copies_it() {
    let log = devices_log();
    let v1 = sealed(&log, 1, true);
    // A device writer copies what counts, unchanged.
    let copied = recovery_of(&v1, &log.chain);
    let v2 = seal_record(
        &log.chain,
        2,
        &record_fixture(),
        copied.clone(),
        &dev(2),
        &mut TestRng::new("copy"),
    )
    .unwrap();
    assert_eq!(recovery_of(&v2, &log.chain), copied);
    assert_eq!(v2.recovery_attestation, v1.recovery_attestation);
    // The master corrects it to another key.
    let replaced = attest(&log, &x25519_multikey_of(&enc_secret(9)));
    let v3 = seal_record(
        &log.chain,
        3,
        &record_fixture(),
        Some(replaced.clone()),
        &master(0),
        &mut TestRng::new("correct"),
    )
    .unwrap();
    assert_eq!(
        check_sealed(&v3, &log.chain),
        Ok(Writer::Master("m0".into()))
    );
    assert_eq!(recovery_of(&v3, &log.chain), Some(replaced));
    assert_eq!(
        open(&v3, &log.chain, RECOVERY_ENTRY, &enc_secret(9)),
        Ok(record_fixture())
    );
}

#[test]
fn after_a_rotation_the_old_masters_attestation_no_longer_counts() {
    let mut log = devices_log();
    let v1 = sealed(&log, 1, true);
    let old = recovery_of(&v1, &log.chain).unwrap();
    log.rotate(&[], false);
    // The record d1 wrote still opens, but its recovery key no longer counts, the store
    // refuses it, and a writer cannot seal it again.
    assert_eq!(
        open(&v1, &log.chain, "d1", &enc_secret(1)),
        Ok(record_fixture())
    );
    assert_eq!(recovery_of(&v1, &log.chain), None);
    assert!(matches!(
        check_sealed(&v1, &log.chain),
        Err(RecordError::Attestation(_))
    ));
    assert!(matches!(
        seal_record(
            &log.chain,
            2,
            &record_fixture(),
            Some(old.clone()),
            &dev(1),
            &mut TestRng::new("stale")
        ),
        Err(RecordError::Attestation(_))
    ));
    // A device writer drops it; the new master attests it again.
    let v2 = sealed_by(&log, 2, false, &dev(1));
    assert_eq!(
        check_sealed(&v2, &log.chain),
        Ok(Writer::Device("d1".into()))
    );
    let again = attest(&log, &old.key);
    assert_eq!(again.attestation.key_id, "m1");
    let v3 = seal_record(
        &log.chain,
        3,
        &record_fixture(),
        Some(again.clone()),
        &master(log.n),
        &mut TestRng::new("again"),
    )
    .unwrap();
    assert_eq!(
        check_sealed(&v3, &log.chain),
        Ok(Writer::Master("m1".into()))
    );
    assert_eq!(recovery_of(&v3, &log.chain), Some(again));
}

#[test]
fn the_record_has_no_preferences_field() {
    let mut value = serde_json::to_value(record_fixture()).unwrap();
    value["preferences"] = json!({"theme": "dark"});
    assert!(serde_json::from_value::<PrivateRecord>(value).is_err());
}

#[test]
fn a_pending_revocation_carries_a_known_reason() {
    let value = serde_json::to_value(record_fixture()).unwrap();
    assert_eq!(value["pendingRevocations"][0]["reason"], "account-deleted");
    assert_eq!(
        serde_json::from_value::<PrivateRecord>(value.clone()).unwrap(),
        record_fixture()
    );
    let mut value = value;
    value["pendingRevocations"][0]["reason"] = "lost".into();
    assert!(serde_json::from_value::<PrivateRecord>(value).is_err());
    assert_eq!(
        serde_json::to_value(RevocationReason::LeftHost).unwrap(),
        "left-host"
    );
}

#[test]
fn holds_master_is_false_offered_or_true() {
    for (holds, wire) in [
        (HoldsMaster::No, json!(false)),
        (HoldsMaster::Offered, json!("offered")),
        (HoldsMaster::Yes, json!(true)),
    ] {
        assert_eq!(serde_json::to_value(holds).unwrap(), wire);
        assert_eq!(serde_json::from_value::<HoldsMaster>(wire).unwrap(), holds);
    }
    assert!(serde_json::from_value::<HoldsMaster>(json!("yes")).is_err());
    assert!(serde_json::from_value::<HoldsMaster>(json!(1)).is_err());
}

#[test]
fn device_kinds_and_places_use_kebab_case() {
    assert_eq!(
        serde_json::to_value(DeviceKind::CoasysCloud).unwrap(),
        "coasys-cloud"
    );
    assert_eq!(
        serde_json::to_value(Place::CommunityHost).unwrap(),
        "community-host"
    );
}

#[cfg(feature = "words")]
#[test]
fn the_words_open_the_record_through_the_recovery_key() {
    use ad4m_identity_core::words;
    let words = words::from_entropy(&WORDS_ENTROPY);
    let secret: X25519Secret = words::recovery_secret(&words::seed(&words));
    let log = devices_log();
    let s = seal_record(
        &log.chain,
        1,
        &record_fixture(),
        Some(attest(&log, &x25519_multikey_of(&secret))),
        &dev(1),
        &mut TestRng::new("words"),
    )
    .unwrap();
    assert_eq!(
        open(&s, &log.chain, RECOVERY_ENTRY, &secret),
        Ok(record_fixture())
    );
}

/// Review coasys/ad4m#1632 (Data's red test): at bc18061d2 the record's AAD was `{scid,
/// version}` and the envelope carried no writer signature, so a store could replace
/// `recoveryKey` with a key of its own while leaving the wraps and the ciphertext intact, and
/// the next honest writer copied the swapped key. The envelope signature covers `recoveryKey`
/// and its attestation, and the AAD binds `recoveryKey`.
#[test]
fn a_record_whose_recovery_key_the_store_swapped_is_refused_by_readers_and_the_store() {
    let log = devices_log();
    let mut s = sealed(&log, 1, true);
    // The store's own X25519 key, in the recovery key's place.
    s.recovery_key = Some(x25519_multikey_of(&enc_secret(3)));
    assert_eq!(
        open(&s, &log.chain, "d1", &enc_secret(1)),
        Err(RecordError::Signature),
        "a device opened a record whose recovery key was swapped"
    );
    assert_eq!(
        check_sealed(&s, &log.chain),
        Err(RecordError::Signature),
        "the store check passed a record whose recovery key was swapped"
    );
    // Nor does a copying writer carry it on: it no longer counts.
    assert_eq!(recovery_of(&s, &log.chain), None);
}

/// Review coasys/ad4m#1632 at d30f21318: the spec (`did-scid-coasys-1.md`, "A reader still
/// opens a record written before the rotation") and `open_record`'s own doc say a stored record
/// stays readable for its surviving recipients once its writer is rotated or revoked. At
/// d30f21318 `verify_envelope` checked the signer against the *current* head, so the last record
/// a device wrote before its revocation, and the record the old master wrote before a rotation,
/// opened for nobody (`Err(Signer("m0"))`). A reader now checks the writer at the seq of the
/// store's receipt.
#[test]
fn a_record_written_before_a_rotation_or_a_revocation_still_opens_for_its_readers() {
    // The master installs the recovery key and writes, then rotates: the devices and the words
    // still read it.
    let mut log = devices_log();
    let by_master = sealed_by(&log, 1, true, &master(log.n));
    let accepted = receipt(&by_master, &log.chain);
    assert_eq!(accepted.seq, log.chain.head().seq);
    log.rotate(&[], false);
    for (entry, secret) in [
        ("d1", enc_secret(1)),
        ("d2", enc_secret(2)),
        (RECOVERY_ENTRY, recovery_secret_fixture()),
    ] {
        assert_eq!(
            open_record(&by_master, &accepted, &log.chain, &stores(), entry, &secret),
            Ok(record_fixture()),
            "the record the old master wrote no longer opens for {entry} after the rotation"
        );
    }
    // The record opens, but its recovery key needs the new master's attestation to count.
    assert_eq!(recovery_of(&by_master, &log.chain), None);

    // d2 writes the latest metadata and is then revoked: d1 still reads it, and rewrites it
    // signed by itself at the head, wrapped to the readers left.
    let mut log = devices_log();
    let by_d2 = sealed_by(&log, 1, false, &dev(2));
    let accepted = receipt(&by_d2, &log.chain);
    log.revoke(&["d2"]);
    let read = open_record(
        &by_d2,
        &accepted,
        &log.chain,
        &stores(),
        "d1",
        &enc_secret(1),
    );
    assert_eq!(
        read,
        Ok(record_fixture()),
        "the record d2 wrote no longer opens after d2's revocation"
    );
    let rewritten = seal_record(
        &log.chain,
        2,
        &read.unwrap(),
        None,
        &dev(1),
        &mut TestRng::new("rewrite"),
    )
    .unwrap();
    assert_eq!(
        check_sealed(&rewritten, &log.chain),
        Ok(Writer::Device("d1".into()))
    );
    let entries: Vec<&str> = rewritten.wrapped.keys().map(String::as_str).collect();
    assert_eq!(entries, ["d1"]);
    assert_eq!(
        open(&rewritten, &log.chain, "d1", &enc_secret(1)),
        Ok(record_fixture())
    );
}

#[test]
fn a_receipt_that_does_not_hold_is_refused() {
    let mut log = devices_log();
    let by_d2 = sealed_by(&log, 1, false, &dev(2));
    let good = receipt(&by_d2, &log.chain);
    let read = |r: &RecordReceipt, chain: &KelChain| {
        open_record(&by_d2, r, chain, &stores(), "d1", &enc_secret(1))
    };
    assert_eq!(read(&good, &log.chain), Ok(record_fixture()));

    // A bad signature: by another key under the store's name, or with any field changed after
    // the store signed.
    let mut forged = good.clone();
    let bytes = forged.signing_bytes().unwrap();
    forged.sig = ad4m_hash_chain::encode_proof_value(&ad4m_identity_core::build::Signer::sign(
        &sk("a hostile store"),
        &bytes,
    ));
    assert!(matches!(
        read(&forged, &log.chain),
        Err(RecordError::Receipt(_))
    ));
    let mut later = good.clone();
    later.seq += 1;
    assert!(matches!(
        read(&later, &log.chain),
        Err(RecordError::Receipt(_))
    ));
    let mut garbled = good.clone();
    garbled.sig = "not a signature".into();
    assert!(matches!(
        read(&garbled, &log.chain),
        Err(RecordError::Receipt(_))
    ));
    // A store the reader does not trust for heads, though its signature holds.
    let other = sign_receipt(&scid(&log), &by_d2, good.seq, &sk("another store")).unwrap();
    assert!(matches!(
        read(&other, &log.chain),
        Err(RecordError::Receipt(_))
    ));
    // A receipt for another envelope (a wrong hash), another version or another identity, each
    // signed by the trusted store.
    let other_envelope = sealed_by(&log, 1, false, &dev(1));
    let wrong_hash = receipt(&other_envelope, &log.chain);
    assert_ne!(wrong_hash.envelope, good.envelope);
    assert!(matches!(
        read(&wrong_hash, &log.chain),
        Err(RecordError::Receipt(_))
    ));
    let mut wrong_version = good.clone();
    wrong_version.version = 2;
    let bytes = wrong_version.signing_bytes().unwrap();
    wrong_version.sig = ad4m_hash_chain::encode_proof_value(
        &ad4m_identity_core::build::Signer::sign(&store_key(), &bytes),
    );
    assert!(matches!(
        read(&wrong_version, &log.chain),
        Err(RecordError::Receipt(_))
    ));
    let stranger = Log::new();
    let mut wrong_scid = good.clone();
    wrong_scid.scid = scid(&stranger);
    let bytes = wrong_scid.signing_bytes().unwrap();
    wrong_scid.sig = ad4m_hash_chain::encode_proof_value(&ad4m_identity_core::build::Signer::sign(
        &store_key(),
        &bytes,
    ));
    assert!(matches!(
        read(&wrong_scid, &log.chain),
        Err(RecordError::Receipt(_))
    ));
    // A seq past the reader's log: it reads the log first.
    let ahead = sign_receipt(&scid(&log), &by_d2, log.chain.head().seq + 1, &store_key()).unwrap();
    assert!(matches!(
        read(&ahead, &log.chain),
        Err(RecordError::Receipt(_))
    ));

    // A receipt at a seq at which the signer was already revoked: the store would have refused
    // that write, so a receipt naming it does not count, even signed by the trusted store.
    log.revoke(&["d2"]);
    let revoked_at = log.chain.head().seq;
    assert_eq!(read(&good, &log.chain), Ok(record_fixture()));
    let backdated = sign_receipt(&scid(&log), &by_d2, revoked_at, &store_key()).unwrap();
    assert_eq!(
        read(&backdated, &log.chain),
        Err(RecordError::Signer("d2".into()))
    );
    // Nor does a receipt make a bot key a writer.
    let mut by_bot = sealed(&log, 2, false);
    by_bot.signer = "d3".into();
    resign(&mut by_bot, &log, &dev(3));
    assert_eq!(
        open(&by_bot, &log.chain, "d1", &enc_secret(1)),
        Err(RecordError::Signer("d3".into()))
    );
}

#[test]
fn the_receipt_round_trips_as_json_and_names_the_signed_envelope() {
    let log = devices_log();
    let s = sealed(&log, 1, true);
    let r = receipt(&s, &log.chain);
    let value = serde_json::to_value(&r).unwrap();
    assert_eq!(
        value.as_object().unwrap().keys().collect::<Vec<_>>(),
        ["envelope", "scid", "seq", "sig", "store", "version"]
    );
    assert_eq!(
        serde_json::from_value::<RecordReceipt>(value.clone()).unwrap(),
        r
    );
    let mut extra = value;
    extra["time"] = json!(1);
    assert!(serde_json::from_value::<RecordReceipt>(extra).is_err());
    assert_eq!(r.store, format!("did:key:{}", mk(&store_key())));
    // The hash covers the writer's signature too: the same fields with another sig differ,
    // and so does the envelope signed again by another writer.
    let mut other_sig = s.clone();
    other_sig.sig = "zAnotherSignature".into();
    assert_ne!(envelope_hash(&scid(&log), &other_sig).unwrap(), r.envelope);
    let mut resigned = s.clone();
    resign(&mut resigned, &log, &dev(2));
    assert_ne!(envelope_hash(&scid(&log), &resigned).unwrap(), r.envelope);
    assert!(String::from_utf8(r.signing_bytes().unwrap())
        .unwrap()
        .contains("\"domain\":\"coasys:record-receipt\""));
}
