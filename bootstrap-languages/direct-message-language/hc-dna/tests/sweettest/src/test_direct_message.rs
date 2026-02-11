//! Tests for direct message functionality
//!
//! These tests verify sending messages between agents via P2P signals
//! and inbox messages.

use crate::utils::*;
use holochain_types::prelude::AgentPubKey;

/// Test setting and getting status
#[tokio::test(flavor = "multi_thread")]
async fn test_status() {
    // Setup two conductors
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    // Get Alice's agent pubkey for setting as recipient
    let alice_pubkey: AgentPubKey = conductors[0]
        .call(&alice_cell.zome("direct-message"), "get_test_recipient", ())
        .await;

    // Set Alice as the recipient on both cells
    let _: () = call_zome(&conductors[0], alice_cell, "set_test_recipient", alice_pubkey.clone()).await;
    let _: () = call_zome(&conductors[1], bob_cell, "set_test_recipient", alice_pubkey.clone()).await;

    await_consistency(1000).await;

    // Get initial status (should be empty)
    let empty_status: Option<serde_json::Value> = call_zome(&conductors[0], alice_cell, "get_status", ()).await;
    assert!(empty_status.is_none(), "Initial status should be None");

    // Set status
    let status = generate_message_with_link("alice");
    let _: () = call_zome(&conductors[0], alice_cell, "set_status", status.clone()).await;

    await_consistency(1000).await;

    // Get status from Alice (local)
    let alice_status: Option<serde_json::Value> = call_zome(&conductors[0], alice_cell, "get_status", ()).await;
    assert!(alice_status.is_some(), "Alice should have status set");

    // Get status from Bob (remote call)
    let bob_status: Option<serde_json::Value> = call_zome(&conductors[1], bob_cell, "get_status", ()).await;
    assert!(bob_status.is_some(), "Bob should be able to get Alice's status");
}

/// Test sending inbox messages
#[tokio::test(flavor = "multi_thread")]
async fn test_inbox_message() {
    // Setup two conductors
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    // Get Alice's agent pubkey for setting as recipient
    let alice_pubkey: AgentPubKey = conductors[0]
        .call(&alice_cell.zome("direct-message"), "get_test_recipient", ())
        .await;

    // Set Alice as the recipient on both cells
    let _: () = call_zome(&conductors[0], alice_cell, "set_test_recipient", alice_pubkey.clone()).await;
    let _: () = call_zome(&conductors[1], bob_cell, "set_test_recipient", alice_pubkey.clone()).await;

    await_consistency(1000).await;

    // Check initial inbox is empty
    let initial_inbox: Vec<serde_json::Value> = call_zome(&conductors[0], alice_cell, "inbox", ()).await;
    assert_eq!(initial_inbox.len(), 0, "Initial inbox should be empty");

    // Bob sends a message to Alice's inbox
    let message = generate_message("bob");
    let _: () = call_zome(&conductors[1], bob_cell, "send_inbox", message.clone()).await;

    await_consistency(5000).await;

    // Alice fetches her inbox
    let _: () = call_zome(&conductors[0], alice_cell, "fetch_inbox", ()).await;

    // Check Alice's inbox has the message
    let inbox: Vec<serde_json::Value> = call_zome(&conductors[0], alice_cell, "inbox", ()).await;
    assert_eq!(inbox.len(), 1, "Alice should have 1 message in inbox");
}

/// Test that only recipient can fetch inbox
#[tokio::test(flavor = "multi_thread")]
async fn test_inbox_access_control() {
    // Setup two conductors
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    // Get Alice's agent pubkey for setting as recipient
    let alice_pubkey: AgentPubKey = conductors[0]
        .call(&alice_cell.zome("direct-message"), "get_test_recipient", ())
        .await;

    // Set Alice as the recipient on both cells
    let _: () = call_zome(&conductors[0], alice_cell, "set_test_recipient", alice_pubkey.clone()).await;
    let _: () = call_zome(&conductors[1], bob_cell, "set_test_recipient", alice_pubkey.clone()).await;

    await_consistency(1000).await;

    // Bob tries to fetch inbox (should fail - only recipient can)
    let bob_fetch_result = call_zome_fallible::<_, ()>(&conductors[1], bob_cell, "fetch_inbox", ()).await;
    assert!(bob_fetch_result.is_err(), "Bob should not be able to fetch Alice's inbox");
}

/// Test inbox filtering by DID
#[tokio::test(flavor = "multi_thread")]
async fn test_inbox_filter() {
    // Setup two conductors
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    // Get Alice's agent pubkey for setting as recipient
    let alice_pubkey: AgentPubKey = conductors[0]
        .call(&alice_cell.zome("direct-message"), "get_test_recipient", ())
        .await;

    // Set Alice as the recipient on both cells
    let _: () = call_zome(&conductors[0], alice_cell, "set_test_recipient", alice_pubkey.clone()).await;
    let _: () = call_zome(&conductors[1], bob_cell, "set_test_recipient", alice_pubkey.clone()).await;

    await_consistency(1000).await;

    // Bob sends a message
    let message = generate_message("bob");
    let _: () = call_zome(&conductors[1], bob_cell, "send_inbox", message).await;

    await_consistency(5000).await;

    // Alice fetches her inbox
    let _: () = call_zome(&conductors[0], alice_cell, "fetch_inbox", ()).await;

    // Filter by bob's DID - should find the message
    let filtered_inbox: Vec<serde_json::Value> = call_zome(
        &conductors[0], 
        alice_cell, 
        "inbox", 
        Some("did:test:bob".to_string())
    ).await;
    assert_eq!(filtered_inbox.len(), 1, "Should find bob's message");

    // Filter by other DID - should find nothing
    let empty_inbox: Vec<serde_json::Value> = call_zome(
        &conductors[0], 
        alice_cell, 
        "inbox", 
        Some("did:test:other".to_string())
    ).await;
    assert_eq!(empty_inbox.len(), 0, "Should not find messages from other");
}
