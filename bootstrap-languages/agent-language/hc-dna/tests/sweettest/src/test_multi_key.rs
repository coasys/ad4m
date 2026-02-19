//! Tests for multi-key agent identity support

use crate::utils::{call_zome, create_test_agent_expression, setup_1_conductor};
use agent_store_integrity::{
    AddAuthorisedKeyInput, AgentExpression, IsKeyValidInput, KeyAuthorisation, RevokeKeyInput,
};

/// Test that creating an agent expression with a did:key DID auto-populates root key
#[tokio::test(flavor = "multi_thread")]
async fn test_create_expression_auto_populates_root_key() {
    let (conductor, cell) = setup_1_conductor().await;

    let did = "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let agent_expression = create_test_agent_expression(did, None);

    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    let retrieved: Option<AgentExpression> =
        call_zome(&conductor, &cell, "get_agent_expression", did.to_string()).await;

    let expr = retrieved.expect("Should exist");
    assert_eq!(expr.data.authorised_keys.len(), 1, "Should have root key auto-populated");
    assert_eq!(
        expr.data.authorised_keys[0].key,
        "z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK"
    );
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

/// Test adding an authorised key with a valid (existing) authorising key
#[tokio::test(flavor = "multi_thread")]
async fn test_add_authorised_key_success() {
    let (conductor, cell) = setup_1_conductor().await;

    let did = "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let root_key = "z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let agent_expression = create_test_agent_expression(did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    let input = AddAuthorisedKeyInput {
        did: did.to_string(),
        key: "new-device-key-123".to_string(),
        name: "My Phone".to_string(),
        proof: KeyAuthorisation {
            authorising_key: root_key.to_string(),
            signature: "test-signature".to_string(),
        },
    };

    let result: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", input).await;

    assert_eq!(result.data.authorised_keys.len(), 2);
    assert_eq!(result.data.authorised_keys[1].key, "new-device-key-123");
    assert_eq!(result.data.authorised_keys[1].name, "My Phone");
}

/// Test adding a key with an unauthorised (unknown) authorising key fails
#[tokio::test(flavor = "multi_thread")]
#[should_panic(expected = "Authorising key is not in the current authorised keys")]
async fn test_add_key_with_invalid_authorising_key_fails() {
    let (conductor, cell) = setup_1_conductor().await;

    let did = "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let agent_expression = create_test_agent_expression(did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    let input = AddAuthorisedKeyInput {
        did: did.to_string(),
        key: "malicious-key".to_string(),
        name: "Attacker".to_string(),
        proof: KeyAuthorisation {
            authorising_key: "unknown-key-not-authorised".to_string(),
            signature: "fake-sig".to_string(),
        },
    };

    let _: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", input).await;
}

/// Test adding a key that is already authorised fails
#[tokio::test(flavor = "multi_thread")]
#[should_panic(expected = "Key is already authorised")]
async fn test_add_duplicate_key_fails() {
    let (conductor, cell) = setup_1_conductor().await;

    let did = "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let root_key = "z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let agent_expression = create_test_agent_expression(did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    // Try to add the root key again
    let input = AddAuthorisedKeyInput {
        did: did.to_string(),
        key: root_key.to_string(),
        name: "Duplicate".to_string(),
        proof: KeyAuthorisation {
            authorising_key: root_key.to_string(),
            signature: "sig".to_string(),
        },
    };

    let _: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", input).await;
}

/// Test revoking a key moves it from authorised to revoked
#[tokio::test(flavor = "multi_thread")]
async fn test_revoke_key_success() {
    let (conductor, cell) = setup_1_conductor().await;

    let did = "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let root_key = "z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let agent_expression = create_test_agent_expression(did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    // Add a second key first
    let add_input = AddAuthorisedKeyInput {
        did: did.to_string(),
        key: "device-key-2".to_string(),
        name: "Phone".to_string(),
        proof: KeyAuthorisation {
            authorising_key: root_key.to_string(),
            signature: "sig".to_string(),
        },
    };
    let _: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", add_input).await;

    // Now revoke the second key
    let revoke_input = RevokeKeyInput {
        did: did.to_string(),
        key: "device-key-2".to_string(),
        signature: "revoke-sig".to_string(),
        reason: Some("Lost device".to_string()),
    };
    let result: AgentExpression = call_zome(&conductor, &cell, "revoke_key", revoke_input).await;

    assert_eq!(result.data.authorised_keys.len(), 1, "Revoked key should be removed from authorised");
    assert_eq!(result.data.revoked_keys.len(), 1);
    assert_eq!(result.data.revoked_keys[0].revoked_key, "device-key-2");
    assert_eq!(result.data.revoked_keys[0].reason, Some("Lost device".to_string()));
}

/// Test revoking an already-revoked key fails
#[tokio::test(flavor = "multi_thread")]
#[should_panic(expected = "Key not found in authorised keys")]
async fn test_revoke_already_revoked_key_fails() {
    let (conductor, cell) = setup_1_conductor().await;

    let did = "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let root_key = "z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let agent_expression = create_test_agent_expression(did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    // Add then revoke a key
    let add_input = AddAuthorisedKeyInput {
        did: did.to_string(),
        key: "temp-key".to_string(),
        name: "Temp".to_string(),
        proof: KeyAuthorisation {
            authorising_key: root_key.to_string(),
            signature: "sig".to_string(),
        },
    };
    let _: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", add_input).await;

    let revoke_input = RevokeKeyInput {
        did: did.to_string(),
        key: "temp-key".to_string(),
        signature: "sig".to_string(),
        reason: None,
    };
    let _: AgentExpression = call_zome(&conductor, &cell, "revoke_key", revoke_input).await;

    // Try to revoke again — should fail (key no longer in authorised_keys)
    let revoke_input2 = RevokeKeyInput {
        did: did.to_string(),
        key: "temp-key".to_string(),
        signature: "sig".to_string(),
        reason: None,
    };
    let _: AgentExpression = call_zome(&conductor, &cell, "revoke_key", revoke_input2).await;
}

/// Test is_key_valid returns true for authorised keys, false for revoked/unknown
#[tokio::test(flavor = "multi_thread")]
async fn test_is_key_valid() {
    let (conductor, cell) = setup_1_conductor().await;

    let did = "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let root_key = "z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let agent_expression = create_test_agent_expression(did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    // Root key should be valid
    let valid: bool = call_zome(
        &conductor,
        &cell,
        "is_key_valid",
        IsKeyValidInput {
            did: did.to_string(),
            key: root_key.to_string(),
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
            did: did.to_string(),
            key: "unknown-key".to_string(),
        },
    )
    .await;
    assert!(!valid, "Unknown key should be invalid");

    // Non-existent DID should return false
    let valid: bool = call_zome(
        &conductor,
        &cell,
        "is_key_valid",
        IsKeyValidInput {
            did: "did:key:nonexistent".to_string(),
            key: root_key.to_string(),
        },
    )
    .await;
    assert!(!valid, "Non-existent DID should return false");
}

/// Test that a revoked key cannot be used to add new keys
#[tokio::test(flavor = "multi_thread")]
#[should_panic(expected = "Authorising key has been revoked")]
async fn test_revoked_key_cannot_add_new_keys() {
    let (conductor, cell) = setup_1_conductor().await;

    let did = "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let root_key = "z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
    let agent_expression = create_test_agent_expression(did, None);
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    // Add a second key
    let add_input = AddAuthorisedKeyInput {
        did: did.to_string(),
        key: "secondary-key".to_string(),
        name: "Secondary".to_string(),
        proof: KeyAuthorisation {
            authorising_key: root_key.to_string(),
            signature: "sig".to_string(),
        },
    };
    let _: AgentExpression = call_zome(&conductor, &cell, "add_authorised_key", add_input).await;

    // Revoke the secondary key
    let revoke_input = RevokeKeyInput {
        did: did.to_string(),
        key: "secondary-key".to_string(),
        signature: "sig".to_string(),
        reason: None,
    };
    let _: AgentExpression = call_zome(&conductor, &cell, "revoke_key", revoke_input).await;

    // Try to use revoked key to add another key — should fail
    let add_with_revoked = AddAuthorisedKeyInput {
        did: did.to_string(),
        key: "malicious-new-key".to_string(),
        name: "Malicious".to_string(),
        proof: KeyAuthorisation {
            authorising_key: "secondary-key".to_string(),
            signature: "fake-sig".to_string(),
        },
    };
    let _: AgentExpression =
        call_zome(&conductor, &cell, "add_authorised_key", add_with_revoked).await;
}

/// Test backward compatibility: old-format expressions without authorised_keys deserialize correctly
#[tokio::test(flavor = "multi_thread")]
async fn test_backward_compat_old_format() {
    let (conductor, cell) = setup_1_conductor().await;

    // Create expression with did:test (non-did:key), which won't auto-populate
    let did = "did:test:legacy-agent";
    let agent_expression = create_test_agent_expression(did, Some("lang://old".to_string()));
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression).await;

    let retrieved: Option<AgentExpression> =
        call_zome(&conductor, &cell, "get_agent_expression", did.to_string()).await;

    let expr = retrieved.expect("Should exist");
    // #[serde(default)] should give empty vecs
    assert!(expr.data.authorised_keys.is_empty());
    assert!(expr.data.revoked_keys.is_empty());
    assert_eq!(expr.data.direct_message_language, Some("lang://old".to_string()));
}
