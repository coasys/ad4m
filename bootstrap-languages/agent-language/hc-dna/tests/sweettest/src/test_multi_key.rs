//! Tests for multi-key agent identity support with Ed25519 signature verification

use crate::utils::{call_zome, create_test_agent_expression, setup_1_conductor};
use agent_store_integrity::{
    AddAuthorisedKeyInput, AgentExpression, IsKeyValidInput, KeyAuthorisation, RevokeKeyInput,
};
use ed25519_dalek::{Signer, SigningKey};
use rand::rngs::OsRng;

/// Helper: generate an Ed25519 keypair and return (multibase_key_string, signing_key)
fn generate_test_keypair() -> (String, SigningKey) {
    let signing_key = SigningKey::generate(&mut OsRng);
    let verifying_key = signing_key.verifying_key();
    // Encode as multibase (z = base58btc) with Ed25519 multicodec prefix (0xed 0x01)
    let mut prefixed = vec![0xed, 0x01];
    prefixed.extend_from_slice(verifying_key.as_bytes());
    let encoded = format!("z{}", bs58::encode(&prefixed).into_string());
    (encoded, signing_key)
}

/// Helper: sign (subject_key + did + timestamp) and return hex-encoded signature
fn sign_key_message(signing_key: &SigningKey, subject_key: &str, did: &str, timestamp: &str) -> String {
    let message = format!("{}{}{}", subject_key, did, timestamp);
    let signature = signing_key.sign(message.as_bytes());
    hex::encode(signature.to_bytes())
}

/// Simple hex encoding (tests only)
mod hex {
    pub fn encode(bytes: &[u8]) -> String {
        bytes.iter().map(|b| format!("{:02x}", b)).collect()
    }
}

/// Test that creating an agent expression with a did:key DID auto-populates root key
#[tokio::test(flavor = "multi_thread")]
async fn test_create_expression_auto_populates_root_key() {
    let (conductor, cell) = setup_1_conductor().await;

    let (root_key, _signing_key) = generate_test_keypair();
    let did = format!("did:key:{}", root_key);
    let agent_expression = create_test_agent_expression(&did, None);

    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    let retrieved: Option<AgentExpression> =
        call_zome(&conductor, &cell, "get_agent_expression", did.clone()).await;

    let expr = retrieved.expect("Should exist");
    assert_eq!(expr.data.authorised_keys.len(), 1, "Should have root key auto-populated");
    assert_eq!(expr.data.authorised_keys[0].key, root_key);
    assert_eq!(expr.data.authorised_keys[0].name, "Root Key");
    assert_eq!(expr.data.authorised_keys[0].proof.signature, "self");
}

/// Test that creating with a non-did:key DID does NOT auto-populate
#[tokio::test(flavor = "multi_thread")]
async fn test_create_expression_non_did_key_no_auto_populate() {
    let (conductor, cell) = setup_1_conductor().await;

    let did = "did:test:alice";
    let agent_expression = create_test_agent_expression(did, None);

    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    let retrieved: Option<AgentExpression> =
        call_zome(&conductor, &cell, "get_agent_expression", did.to_string()).await;

    let expr = retrieved.expect("Should exist");
    assert!(
        expr.data.authorised_keys.is_empty(),
        "Non did:key DID should not auto-populate keys"
    );
}

/// Test adding an authorised key with a valid Ed25519 signature
#[tokio::test(flavor = "multi_thread")]
async fn test_add_authorised_key_valid_signature() {
    let (conductor, cell) = setup_1_conductor().await;

    let (root_key, root_signing_key) = generate_test_keypair();
    let did = format!("did:key:{}", root_key);
    let agent_expression = create_test_agent_expression(&did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    let (new_key, _new_signing_key) = generate_test_keypair();
    let timestamp = "2025-01-01T00:00:00Z";
    let signature = sign_key_message(&root_signing_key, &new_key, &did, timestamp);

    let input = AddAuthorisedKeyInput {
        did: did.clone(),
        key: new_key.clone(),
        name: "My Phone".to_string(),
        proof: KeyAuthorisation {
            authorising_key: root_key.clone(),
            signature,
            timestamp: timestamp.to_string(),
        },
    };

    let result: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", input).await;

    assert_eq!(result.data.authorised_keys.len(), 2);
    assert_eq!(result.data.authorised_keys[1].key, new_key);
    assert_eq!(result.data.authorised_keys[1].name, "My Phone");
}

/// Test adding a key with an invalid/tampered signature fails
#[tokio::test(flavor = "multi_thread")]
#[should_panic(expected = "Invalid signature")]
async fn test_add_authorised_key_invalid_signature_fails() {
    let (conductor, cell) = setup_1_conductor().await;

    let (root_key, _root_signing_key) = generate_test_keypair();
    let did = format!("did:key:{}", root_key);
    let agent_expression = create_test_agent_expression(&did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    let (new_key, _) = generate_test_keypair();
    let timestamp = "2025-01-01T00:00:00Z";

    // Use a different key to sign (wrong signer)
    let (_other_key, other_signing_key) = generate_test_keypair();
    let bad_signature = sign_key_message(&other_signing_key, &new_key, &did, timestamp);

    let input = AddAuthorisedKeyInput {
        did: did.clone(),
        key: new_key.clone(),
        name: "Attacker".to_string(),
        proof: KeyAuthorisation {
            authorising_key: root_key.clone(),
            signature: bad_signature,
            timestamp: timestamp.to_string(),
        },
    };

    let _: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", input).await;
}

/// Test adding a key with a tampered message (wrong key in message) fails
#[tokio::test(flavor = "multi_thread")]
#[should_panic(expected = "Invalid signature")]
async fn test_add_authorised_key_tampered_message_fails() {
    let (conductor, cell) = setup_1_conductor().await;

    let (root_key, root_signing_key) = generate_test_keypair();
    let did = format!("did:key:{}", root_key);
    let agent_expression = create_test_agent_expression(&did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    let (new_key, _) = generate_test_keypair();
    let timestamp = "2025-01-01T00:00:00Z";

    // Sign over a different key than what we submit
    let (different_key, _) = generate_test_keypair();
    let signature = sign_key_message(&root_signing_key, &different_key, &did, timestamp);

    let input = AddAuthorisedKeyInput {
        did: did.clone(),
        key: new_key.clone(), // Different from what was signed
        name: "Tampered".to_string(),
        proof: KeyAuthorisation {
            authorising_key: root_key.clone(),
            signature,
            timestamp: timestamp.to_string(),
        },
    };

    let _: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", input).await;
}

/// Test adding a key with an unauthorised (unknown) authorising key fails
#[tokio::test(flavor = "multi_thread")]
#[should_panic(expected = "Authorising key is not in the current authorised keys")]
async fn test_add_key_with_invalid_authorising_key_fails() {
    let (conductor, cell) = setup_1_conductor().await;

    let (root_key, _) = generate_test_keypair();
    let did = format!("did:key:{}", root_key);
    let agent_expression = create_test_agent_expression(&did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    let (unknown_key, unknown_signing_key) = generate_test_keypair();
    let (new_key, _) = generate_test_keypair();
    let timestamp = "2025-01-01T00:00:00Z";
    let signature = sign_key_message(&unknown_signing_key, &new_key, &did, timestamp);

    let input = AddAuthorisedKeyInput {
        did: did.clone(),
        key: new_key,
        name: "Attacker".to_string(),
        proof: KeyAuthorisation {
            authorising_key: unknown_key,
            signature,
            timestamp: timestamp.to_string(),
        },
    };

    let _: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", input).await;
}

/// Test adding a key that is already authorised fails
#[tokio::test(flavor = "multi_thread")]
#[should_panic(expected = "Key is already authorised")]
async fn test_add_duplicate_key_fails() {
    let (conductor, cell) = setup_1_conductor().await;

    let (root_key, root_signing_key) = generate_test_keypair();
    let did = format!("did:key:{}", root_key);
    let agent_expression = create_test_agent_expression(&did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    let timestamp = "2025-01-01T00:00:00Z";
    let signature = sign_key_message(&root_signing_key, &root_key, &did, timestamp);

    let input = AddAuthorisedKeyInput {
        did: did.clone(),
        key: root_key.clone(),
        name: "Duplicate".to_string(),
        proof: KeyAuthorisation {
            authorising_key: root_key.clone(),
            signature,
            timestamp: timestamp.to_string(),
        },
    };

    let _: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", input).await;
}

/// Test revoking a key with valid signature moves it from authorised to revoked
#[tokio::test(flavor = "multi_thread")]
async fn test_revoke_key_valid_signature() {
    let (conductor, cell) = setup_1_conductor().await;

    let (root_key, root_signing_key) = generate_test_keypair();
    let did = format!("did:key:{}", root_key);
    let agent_expression = create_test_agent_expression(&did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    // Add a second key
    let (device_key, _device_signing_key) = generate_test_keypair();
    let add_ts = "2025-01-01T00:00:00Z";
    let add_sig = sign_key_message(&root_signing_key, &device_key, &did, add_ts);

    let add_input = AddAuthorisedKeyInput {
        did: did.clone(),
        key: device_key.clone(),
        name: "Phone".to_string(),
        proof: KeyAuthorisation {
            authorising_key: root_key.clone(),
            signature: add_sig,
            timestamp: add_ts.to_string(),
        },
    };
    let _: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", add_input).await;

    // Revoke the device key using root key
    let revoke_ts = "2025-01-02T00:00:00Z";
    let revoke_sig = sign_key_message(&root_signing_key, &device_key, &did, revoke_ts);

    let revoke_input = RevokeKeyInput {
        did: did.clone(),
        key: device_key.clone(),
        revoked_by_key: root_key.clone(),
        signature: revoke_sig,
        timestamp: revoke_ts.to_string(),
        reason: Some("Lost device".to_string()),
    };
    let result: AgentExpression = call_zome(&conductor, &cell, "revoke_key", revoke_input).await;

    assert_eq!(result.data.authorised_keys.len(), 1, "Revoked key should be removed");
    assert_eq!(result.data.revoked_keys.len(), 1);
    assert_eq!(result.data.revoked_keys[0].revoked_key, device_key);
    assert_eq!(result.data.revoked_keys[0].reason, Some("Lost device".to_string()));
}

/// Test revoking a key with invalid signature fails
#[tokio::test(flavor = "multi_thread")]
#[should_panic(expected = "Invalid signature")]
async fn test_revoke_key_invalid_signature_fails() {
    let (conductor, cell) = setup_1_conductor().await;

    let (root_key, root_signing_key) = generate_test_keypair();
    let did = format!("did:key:{}", root_key);
    let agent_expression = create_test_agent_expression(&did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    // Add a second key
    let (device_key, _) = generate_test_keypair();
    let add_ts = "2025-01-01T00:00:00Z";
    let add_sig = sign_key_message(&root_signing_key, &device_key, &did, add_ts);

    let add_input = AddAuthorisedKeyInput {
        did: did.clone(),
        key: device_key.clone(),
        name: "Phone".to_string(),
        proof: KeyAuthorisation {
            authorising_key: root_key.clone(),
            signature: add_sig,
            timestamp: add_ts.to_string(),
        },
    };
    let _: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", add_input).await;

    // Try to revoke with a bad signature
    let revoke_ts = "2025-01-02T00:00:00Z";
    let bad_sig = "00".repeat(64); // 64 zero bytes — invalid signature

    let revoke_input = RevokeKeyInput {
        did: did.clone(),
        key: device_key.clone(),
        revoked_by_key: root_key.clone(),
        signature: bad_sig,
        timestamp: revoke_ts.to_string(),
        reason: None,
    };
    let _: AgentExpression = call_zome(&conductor, &cell, "revoke_key", revoke_input).await;
}

/// Test is_key_valid returns true for authorised keys, false for revoked/unknown
#[tokio::test(flavor = "multi_thread")]
async fn test_is_key_valid() {
    let (conductor, cell) = setup_1_conductor().await;

    let (root_key, _) = generate_test_keypair();
    let did = format!("did:key:{}", root_key);
    let agent_expression = create_test_agent_expression(&did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    // Root key should be valid
    let valid: bool = call_zome(
        &conductor,
        &cell,
        "is_key_valid",
        IsKeyValidInput {
            did: did.clone(),
            key: root_key.clone(),
        },
    )
    .await;
    assert!(valid, "Root key should be valid");

    // Unknown key should be invalid
    let valid: bool = call_zome(
        &conductor,
        &cell,
        "is_key_valid",
        IsKeyValidInput {
            did: did.clone(),
            key: "unknown-key".to_string(),
        },
    )
    .await;
    assert!(!valid, "Unknown key should be invalid");
}

/// Test that a revoked key cannot be used to add new keys
#[tokio::test(flavor = "multi_thread")]
#[should_panic(expected = "Authorising key has been revoked")]
async fn test_revoked_key_cannot_add_new_keys() {
    let (conductor, cell) = setup_1_conductor().await;

    let (root_key, root_signing_key) = generate_test_keypair();
    let did = format!("did:key:{}", root_key);
    let agent_expression = create_test_agent_expression(&did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    // Add a secondary key
    let (secondary_key, secondary_signing_key) = generate_test_keypair();
    let ts1 = "2025-01-01T00:00:00Z";
    let sig1 = sign_key_message(&root_signing_key, &secondary_key, &did, ts1);

    let add_input = AddAuthorisedKeyInput {
        did: did.clone(),
        key: secondary_key.clone(),
        name: "Secondary".to_string(),
        proof: KeyAuthorisation {
            authorising_key: root_key.clone(),
            signature: sig1,
            timestamp: ts1.to_string(),
        },
    };
    let _: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", add_input).await;

    // Revoke the secondary key
    let ts2 = "2025-01-02T00:00:00Z";
    let revoke_sig = sign_key_message(&root_signing_key, &secondary_key, &did, ts2);

    let revoke_input = RevokeKeyInput {
        did: did.clone(),
        key: secondary_key.clone(),
        revoked_by_key: root_key.clone(),
        signature: revoke_sig,
        timestamp: ts2.to_string(),
        reason: None,
    };
    let _: AgentExpression = call_zome(&conductor, &cell, "revoke_key", revoke_input).await;

    // Try to use revoked key to add another — should fail
    let (new_key, _) = generate_test_keypair();
    let ts3 = "2025-01-03T00:00:00Z";
    let sig3 = sign_key_message(&secondary_signing_key, &new_key, &did, ts3);

    let add_with_revoked = AddAuthorisedKeyInput {
        did: did.clone(),
        key: new_key,
        name: "Malicious".to_string(),
        proof: KeyAuthorisation {
            authorising_key: secondary_key,
            signature: sig3,
            timestamp: ts3.to_string(),
        },
    };
    let _: AgentExpression =
        call_zome(&conductor, &cell, "add_authorised_key", add_with_revoked).await;
}

/// Test backward compatibility: old-format expressions without authorised_keys
#[tokio::test(flavor = "multi_thread")]
async fn test_backward_compat_old_format() {
    let (conductor, cell) = setup_1_conductor().await;

    let did = "did:test:legacy-agent";
    let agent_expression = create_test_agent_expression(did, Some("lang://old".to_string()));
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    let retrieved: Option<AgentExpression> =
        call_zome(&conductor, &cell, "get_agent_expression", did.to_string()).await;

    let expr = retrieved.expect("Should exist");
    assert!(expr.data.authorised_keys.is_empty());
    assert!(expr.data.revoked_keys.is_empty());
    assert_eq!(expr.data.direct_message_language, Some("lang://old".to_string()));
}
