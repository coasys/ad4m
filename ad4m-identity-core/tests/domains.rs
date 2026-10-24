//! Signing domains (AR-01 4.2, R3, R58): a log proof never verifies as a link, request, head
//! or note signature, and the reverse; labels are never signing domains.
#![cfg(feature = "build")]

mod common;

use ad4m_hash_chain::{decode_proof_value, encode_proof_value, verify_proof, ProofError};
use ad4m_identity_core::build;
use ad4m_identity_core::domain::{check_domain, Scheme, LABELS, SIGNING_DOMAINS};
use ad4m_identity_core::{
    signing_bytes, verify_signed, ChainError, DomainError, FoldError, HeadStatement, Proof,
    KEL_DOMAIN,
};
use common::*;
use serde_json::{json, Value};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_test::wasm_bindgen_test as test;

const OTHER: &[&str] = &[
    "coasys:link",
    "coasys:store-request",
    "coasys:head",
    "coasys:vouch",
];

fn body(event: &ad4m_identity_core::Event) -> Value {
    let mut value = json(event);
    value.as_object_mut().unwrap().remove("proof");
    value
}

#[test]
fn a_log_proof_fails_as_any_other_signature() {
    let log = Log::new();
    let event = &log.events[0];
    let proof = &event.proof[0];
    let signature = decode_proof_value(&proof.proof_value).unwrap();
    let key = mk(&master(0));
    let input = build::signing_input(event, "m0").unwrap();
    let mut proof_config = serde_json::to_value(proof).unwrap();
    proof_config.as_object_mut().unwrap().remove("proofValue");
    for domain in OTHER {
        for value in [body(event), json(event), proof_config.clone()] {
            assert!(!verify_signed(domain, &value, &key, &signature), "{domain}");
        }
        // Not even when the signed object is the raw signing input itself.
        let raw = json!({ "input": common::hex(&input) });
        assert!(!verify_signed(domain, &raw, &key, &signature));
    }
}

#[test]
fn another_domains_signature_fails_as_a_log_proof() {
    let log = Log::new();
    let base = log.events[0].clone();
    let key = master(0);
    for domain in OTHER {
        // Signed over the event body under the other domain, placed as the m0 proof.
        let sig = build::sign_domain(domain, &body(&base), &key).unwrap();
        let mut event = base.clone();
        event.proof[0].proof_value = encode_proof_value(&sig);
        assert_eq!(
            verify_proof(&event, &event.proof[0], &mk(&key), KEL_DOMAIN),
            Err(ProofError::Signature),
            "{domain}"
        );
        assert_eq!(
            ad4m_identity_core::fold(std::slice::from_ref(&event)).unwrap_err(),
            ChainError::Policy(FoldError::BadProof("m0".into()))
        );
        // A Data Integrity proof that names the other domain is refused by its domain.
        let mut named = base.clone();
        named.proof[0].domain = domain.to_string();
        assert!(matches!(
            verify_proof(&named, &named.proof[0], &mk(&key), KEL_DOMAIN),
            Err(ProofError::Domain { .. })
        ));
        // And a log proof checked as if it were that domain fails too.
        assert!(matches!(
            verify_proof(&base, &base.proof[0], &mk(&key), domain),
            Err(ProofError::Domain { .. })
        ));
    }
}

#[test]
fn a_domain_field_signature_holds_only_in_its_domain() {
    let key = sk("signer");
    let value = json!({"author": mk(&key), "data": [1, 2, 3]});
    let names: Vec<&str> = SIGNING_DOMAINS
        .iter()
        .filter(|d| d.scheme == Scheme::DomainField)
        .map(|d| d.name)
        .collect();
    for domain in &names {
        let sig = build::sign_domain(domain, &value, &key).unwrap();
        for other in &names {
            assert_eq!(
                verify_signed(other, &value, &mk(&key), &sig),
                domain == other,
                "{domain} under {other}"
            );
        }
        assert!(!verify_signed(domain, &value, &mk(&sk("other")), &sig));
        let mut changed = value.clone();
        changed["data"] = json!([1, 2]);
        assert!(!verify_signed(domain, &changed, &mk(&key), &sig));
    }
}

#[test]
fn labels_and_the_kel_domain_are_refused_by_every_signing_api() {
    let key = sk("signer");
    let value = json!({"a": 1});
    for label in LABELS {
        assert_eq!(
            signing_bytes(label, &value).unwrap_err(),
            DomainError::Label(label.to_string())
        );
        assert!(build::sign_domain(label, &value, &key).is_err());
        assert!(!verify_signed(label, &value, &mk(&key), &[0u8; 64]));
        assert!(check_domain(label, Scheme::DataIntegrity).is_err());
    }
    assert_eq!(
        signing_bytes(KEL_DOMAIN, &value).unwrap_err(),
        DomainError::Scheme(KEL_DOMAIN.into())
    );
    assert_eq!(
        signing_bytes("coasys:lang-key", &value).unwrap_err(),
        DomainError::Unknown("coasys:lang-key".into())
    );
    assert_eq!(
        signing_bytes("coasys:link", &json!({"domain": "coasys:head"})).unwrap_err(),
        DomainError::DomainMember
    );
    assert_eq!(
        signing_bytes("coasys:link", &json!([1])).unwrap_err(),
        DomainError::NotObject
    );
    // No label sits in the signature table, and the table names each domain once.
    for d in SIGNING_DOMAINS {
        assert!(!LABELS.contains(&d.name));
        assert_eq!(
            SIGNING_DOMAINS.iter().filter(|e| e.name == d.name).count(),
            1
        );
    }
    // The KEL's own proofs use the Data Integrity scheme of coasys:kel.
    check_domain(KEL_DOMAIN, Scheme::DataIntegrity).unwrap();
    let log = Log::new();
    assert!(log.events[0]
        .proof
        .iter()
        .all(|p: &Proof| p.domain == KEL_DOMAIN));
}

#[test]
fn signing_bytes_are_jcs_with_the_domain() {
    let bytes = signing_bytes("coasys:head", &json!({"seq": 4, "hash": "Qm1"})).unwrap();
    assert_eq!(
        String::from_utf8(bytes).unwrap(),
        r#"{"domain":"coasys:head","hash":"Qm1","seq":4}"#
    );
}

/// The store signs a head and an executor verifies it with the one `HeadStatement`: the bytes
/// are pinned, and a head counts only under a trusted key and only for what was signed.
#[test]
fn a_head_statement_verifies_only_under_a_trusted_key() {
    let store = sk("store");
    let did = format!("did:key:{}", mk(&store));
    let statement = HeadStatement {
        scid: "QmS",
        seq: 4,
        hash: "Qm1",
        time: 7,
        store: &did,
    };
    let bytes = statement.signing_bytes().unwrap();
    assert_eq!(
        String::from_utf8(bytes.clone()).unwrap(),
        format!(
            r#"{{"domain":"coasys:head","hash":"Qm1","scid":"QmS","seq":4,"store":"{did}","time":7}}"#
        )
    );
    let sig = encode_proof_value(&ed25519_dalek::Signer::sign(&store, &bytes).to_bytes());
    assert!(statement.verify(&sig, &[mk(&store)]));
    assert!(!statement.verify(&sig, &[mk(&sk("other"))]));
    assert!(!statement.verify(&sig, &[]));
    let moved = HeadStatement {
        seq: 5,
        ..statement
    };
    assert!(!moved.verify(&sig, &[mk(&store)]));
    // A store field that is not a did:key names no key.
    let bare = mk(&store);
    let no_did = HeadStatement {
        store: &bare,
        ..statement
    };
    assert!(!no_did.verify(&sig, &[mk(&store)]));
}
