//! Tests for agent expression creation, update, and retrieval

use crate::utils::{await_consistency, call_zome, create_test_agent_expression, setup_1_conductor, setup_conductors};
use agent_store_integrity::AgentExpression;

/// Test creating and retrieving an agent expression with a single agent
#[tokio::test(flavor = "multi_thread")]
async fn test_create_and_retrieve_agent_expression() {
    let (conductor, cell) = setup_1_conductor().await;

    let did = "did:test:alice";
    let dml = Some("language://direct-message/abc123".to_string());
    let agent_expression = create_test_agent_expression(did, dml.clone());

    // Create the agent expression
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression.clone()).await;

    // Retrieve it
    let retrieved: Option<AgentExpression> = call_zome(&conductor, &cell, "get_agent_expression", did.to_string()).await;

    // Verify
    assert!(retrieved.is_some(), "Agent expression should exist");
    let retrieved = retrieved.unwrap();
    assert_eq!(retrieved.author, did);
    assert_eq!(retrieved.data.direct_message_language, dml);
}

/// Test updating an agent expression (creating a new one with the same DID)
/// Verifies that the latest expression is returned
#[tokio::test(flavor = "multi_thread")]
async fn test_update_agent_expression() {
    let (conductor, cell) = setup_1_conductor().await;

    let did = "did:test:bob";

    // Create first version
    let first_dml = Some("language://v1".to_string());
    let agent_expression_v1 = create_test_agent_expression(did, first_dml.clone());
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression_v1).await;

    // Small delay to ensure timestamp ordering
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    // Create updated version with same DID
    let second_dml = Some("language://v2".to_string());
    let agent_expression_v2 = create_test_agent_expression(did, second_dml.clone());
    let _: () = call_zome(&conductor, &cell, "create_agent_expression", agent_expression_v2).await;

    // Retrieve - should get the latest
    let retrieved: Option<AgentExpression> = call_zome(&conductor, &cell, "get_agent_expression", did.to_string()).await;

    // Verify we got the updated version
    assert!(retrieved.is_some(), "Agent expression should exist");
    let retrieved = retrieved.unwrap();
    assert_eq!(retrieved.author, did);
    assert_eq!(retrieved.data.direct_message_language, second_dml, "Should retrieve the latest version");
}

/// Test multi-agent scenario: Agent A creates, Agent B retrieves after DHT sync
#[tokio::test(flavor = "multi_thread")]
async fn test_multi_agent_sync() {
    let (conductors, cells) = setup_conductors(2, true).await;

    let did = "did:test:charlie";
    let dml = Some("language://multi-agent/test".to_string());
    let agent_expression = create_test_agent_expression(did, dml.clone());

    // Agent A creates the expression
    let _: () = call_zome(&conductors[0], &cells[0], "create_agent_expression", agent_expression).await;

    // Wait for DHT consistency
    await_consistency(3).await;

    // Agent B retrieves the expression
    let retrieved: Option<AgentExpression> = call_zome(&conductors[1], &cells[1], "get_agent_expression", did.to_string()).await;

    // Verify
    assert!(retrieved.is_some(), "Agent B should be able to retrieve Agent A's expression after DHT sync");
    let retrieved = retrieved.unwrap();
    assert_eq!(retrieved.author, did);
    assert_eq!(retrieved.data.direct_message_language, dml);
}
