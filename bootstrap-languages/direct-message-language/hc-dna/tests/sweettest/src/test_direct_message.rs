//! Tests for direct-message-language zome functions

use crate::utils::{
    await_consistency, call_zome, call_zome_fallible, create_test_perspective_expression,
    setup_1_conductor, setup_conductors,
};
use direct_message_integrity::Recipient;

/// Test that set_test_recipient and get_test_recipient work correctly
#[tokio::test(flavor = "multi_thread")]
async fn test_set_get_test_recipient() {
    let (conductor, cell) = setup_1_conductor().await;

    let agent_pubkey = cell.agent_pubkey().clone();

    // Set the test recipient to this agent's pubkey
    let _: () = call_zome(&conductor, &cell, "set_test_recipient", agent_pubkey.clone()).await;

    // Retrieve and verify
    let retrieved: Option<Recipient> =
        call_zome(&conductor, &cell, "get_test_recipient", ()).await;

    assert!(retrieved.is_some(), "Recipient should be stored");
    assert_eq!(
        retrieved.unwrap().get(),
        agent_pubkey,
        "Retrieved pubkey should match stored pubkey"
    );
}

/// Test status: Alice sets her status, Alice retrieves it locally,
/// Bob retrieves it via remote call
#[tokio::test(flavor = "multi_thread")]
async fn test_status() {
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    // Get Alice's agent pubkey
    let alice_pubkey = alice_cell.agent_pubkey().clone();

    // Both conductors set the recipient to Alice's pubkey
    let _: () = call_zome(&conductors[0], alice_cell, "set_test_recipient", alice_pubkey.clone()).await;
    let _: () = call_zome(&conductors[1], bob_cell, "set_test_recipient", alice_pubkey.clone()).await;

    // Create a test status expression
    let status = create_test_perspective_expression("did:test:alice");

    // Alice sets her own status (she is the recipient, so this is allowed)
    let _: () = call_zome(&conductors[0], alice_cell, "set_status", status.clone()).await;

    // Alice retrieves her own status (local source chain query)
    let alice_status: Option<serde_json::Value> =
        call_zome(&conductors[0], alice_cell, "get_status", ()).await;
    assert!(alice_status.is_some(), "Alice should be able to get her own status");

    // Wait for network propagation
    await_consistency(2000).await;

    // Bob retrieves Alice's status (via remote call to Alice's agent)
    let bob_retrieved_status: Option<serde_json::Value> =
        call_zome(&conductors[1], bob_cell, "get_status", ()).await;
    assert!(
        bob_retrieved_status.is_some(),
        "Bob should be able to get Alice's status via remote call"
    );

    // Both should return the same status data
    assert_eq!(
        alice_status.unwrap()["author"],
        bob_retrieved_status.unwrap()["author"],
        "Status author should match"
    );
}

/// Test send_inbox: Bob sends a message to Alice's DHT inbox,
/// Alice fetches it and it appears in her inbox
#[tokio::test(flavor = "multi_thread")]
async fn test_send_inbox_and_fetch() {
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    let alice_pubkey = alice_cell.agent_pubkey().clone();

    // Both set recipient to Alice
    let _: () = call_zome(&conductors[0], alice_cell, "set_test_recipient", alice_pubkey.clone()).await;
    let _: () = call_zome(&conductors[1], bob_cell, "set_test_recipient", alice_pubkey.clone()).await;

    let message = create_test_perspective_expression("did:test:bob");

    // Bob sends a message to Alice's DHT inbox
    let _: () = call_zome(&conductors[1], bob_cell, "send_inbox", message.clone()).await;

    // Wait for DHT propagation
    await_consistency(3000).await;

    // Alice's inbox should be empty before fetching
    let inbox_before: Vec<serde_json::Value> =
        call_zome(&conductors[0], alice_cell, "inbox", Option::<String>::None).await;
    assert_eq!(
        inbox_before.len(),
        0,
        "Alice's inbox should be empty before fetch_inbox"
    );

    // Alice fetches inbox (pulls DHT messages into her local StoredMessages)
    let _: () = call_zome(&conductors[0], alice_cell, "fetch_inbox", ()).await;

    // Alice's inbox should now have 1 message
    let inbox_after: Vec<serde_json::Value> =
        call_zome(&conductors[0], alice_cell, "inbox", Option::<String>::None).await;
    assert_eq!(inbox_after.len(), 1, "Alice's inbox should have 1 message after fetch_inbox");
    assert_eq!(
        inbox_after[0]["author"],
        "did:test:bob",
        "Message author should match"
    );
}

/// Test that only the recipient can call fetch_inbox
#[tokio::test(flavor = "multi_thread")]
async fn test_inbox_access_control() {
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    let alice_pubkey = alice_cell.agent_pubkey().clone();

    // Both set recipient to Alice
    let _: () = call_zome(&conductors[0], alice_cell, "set_test_recipient", alice_pubkey.clone()).await;
    let _: () = call_zome(&conductors[1], bob_cell, "set_test_recipient", alice_pubkey.clone()).await;

    // Bob tries to fetch Alice's inbox - this should fail
    let bob_fetch_result =
        call_zome_fallible::<_, ()>(&conductors[1], bob_cell, "fetch_inbox", ()).await;
    assert!(
        bob_fetch_result.is_err(),
        "Bob should not be able to fetch Alice's inbox (only recipient can)"
    );

    // Alice can fetch her own inbox
    let alice_fetch_result =
        call_zome_fallible::<_, ()>(&conductors[0], alice_cell, "fetch_inbox", ()).await;
    assert!(
        alice_fetch_result.is_ok(),
        "Alice should be able to fetch her own inbox"
    );
}

/// Test inbox DID filtering: inbox returns messages matching author DID
#[tokio::test(flavor = "multi_thread")]
async fn test_inbox_filter_by_did() {
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    let alice_pubkey = alice_cell.agent_pubkey().clone();

    // Both set recipient to Alice
    let _: () = call_zome(&conductors[0], alice_cell, "set_test_recipient", alice_pubkey.clone()).await;
    let _: () = call_zome(&conductors[1], bob_cell, "set_test_recipient", alice_pubkey.clone()).await;

    let author_did = "did:test:bob";
    let message1 = create_test_perspective_expression(author_did);
    let message2 = create_test_perspective_expression(author_did);

    // Bob sends two messages to Alice's DHT inbox
    let _: () = call_zome(&conductors[1], bob_cell, "send_inbox", message1).await;
    let _: () = call_zome(&conductors[1], bob_cell, "send_inbox", message2).await;

    // Wait for DHT propagation
    await_consistency(3000).await;

    // Alice fetches inbox
    let _: () = call_zome(&conductors[0], alice_cell, "fetch_inbox", ()).await;

    // Filter by the actual author DID - should return 2 messages
    let filtered: Vec<serde_json::Value> = call_zome(
        &conductors[0],
        alice_cell,
        "inbox",
        Some(author_did.to_string()),
    )
    .await;
    assert_eq!(
        filtered.len(),
        2,
        "Should return 2 messages from {}",
        author_did
    );

    // Filter by a different DID - should return 0 messages
    let filtered_empty: Vec<serde_json::Value> = call_zome(
        &conductors[0],
        alice_cell,
        "inbox",
        Some("did:test:other".to_string()),
    )
    .await;
    assert_eq!(
        filtered_empty.len(),
        0,
        "Should return 0 messages for unknown DID"
    );

    // No filter - should return all messages
    let all_messages: Vec<serde_json::Value> =
        call_zome(&conductors[0], alice_cell, "inbox", Option::<String>::None).await;
    assert_eq!(all_messages.len(), 2, "Should return all 2 messages without filter");
}

/// Test send_p2p: Bob sends a p2p signal to Alice, which shows up in Alice's inbox
#[tokio::test(flavor = "multi_thread")]
async fn test_send_p2p() {
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    let alice_pubkey = alice_cell.agent_pubkey().clone();

    // Both set recipient to Alice
    let _: () = call_zome(&conductors[0], alice_cell, "set_test_recipient", alice_pubkey.clone()).await;
    let _: () = call_zome(&conductors[1], bob_cell, "set_test_recipient", alice_pubkey.clone()).await;

    let message = create_test_perspective_expression("did:test:bob");

    // Bob sends a p2p message to Alice via remote signal
    let _: () = call_zome(&conductors[1], bob_cell, "send_p2p", message.clone()).await;

    // Wait for signal delivery and StoredMessage creation on Alice's chain
    await_consistency(3000).await;

    // Alice's inbox should contain the message (stored via recv_remote_signal)
    let inbox: Vec<serde_json::Value> =
        call_zome(&conductors[0], alice_cell, "inbox", Option::<String>::None).await;
    assert_eq!(
        inbox.len(),
        1,
        "Alice's inbox should have 1 message from p2p send"
    );
    assert_eq!(
        inbox[0]["author"], "did:test:bob",
        "Message author should match"
    );
}
