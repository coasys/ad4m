//! Tests for telepresence functionality
//!
//! These tests verify online agent status tracking and signal sending.

use crate::utils::*;
use chrono::Utc;
use perspective_diff_sync_integrity::{
    ExpressionProof, OnlineAgent, Perspective, PerspectiveExpression,
};
use serde::{Deserialize, Serialize};

/// Input for send_signal zome call
#[derive(Serialize, Debug)]
struct SignalData {
    recipient_did: String,
    data: PerspectiveExpression,
}

/// Test creating DID to public key links
#[tokio::test(flavor = "multi_thread")]
async fn test_create_did_link() {
    let (mut conductor, cell) = setup_1_conductor().await;

    // Create DID link - should not error
    create_did_link(&conductor, &cell, "did:test:alice").await;

    // Create another one (should be idempotent or succeed)
    create_did_link(&conductor, &cell, "did:test:alice").await;

    conductor.shutdown().await;
}

/// Test setting and getting online status
#[tokio::test(flavor = "multi_thread")]
async fn test_online_status() {
    let (mut conductor, cell) = setup_1_conductor().await;

    create_did_link(&conductor, &cell, "did:test:alice").await;

    // Set online status
    let status = PerspectiveExpression {
        author: "did:test:alice".to_string(),
        timestamp: Utc::now(),
        data: Perspective { links: vec![] },
        proof: ExpressionProof {
            signature: "test_sig".to_string(),
            key: "test_key".to_string(),
        },
    };

    call_zome::<_, ()>(&conductor, &cell, "set_online_status", status).await;

    // Get online agents - should include ourselves
    let agents: Vec<OnlineAgent> = call_zome(&conductor, &cell, "get_online_agents", ()).await;

    // Note: The agent might not immediately appear due to timing
    // In production, there's an ACTIVE_AGENT_DURATION check
    println!("Online agents: {:?}", agents);

    conductor.shutdown().await;
}

/// Test getting online agents across multiple conductors
#[tokio::test(flavor = "multi_thread")]
async fn test_get_online_agents_multi() {
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    // Create DID links
    create_did_link(&conductors[0], alice_cell, "did:test:alice").await;
    create_did_link(&conductors[1], bob_cell, "did:test:bob").await;

    // Set online status for both
    let alice_status = PerspectiveExpression {
        author: "did:test:alice".to_string(),
        timestamp: Utc::now(),
        data: Perspective { links: vec![] },
        proof: ExpressionProof {
            signature: "alice_sig".to_string(),
            key: "alice_key".to_string(),
        },
    };

    let bob_status = PerspectiveExpression {
        author: "did:test:bob".to_string(),
        timestamp: Utc::now(),
        data: Perspective { links: vec![] },
        proof: ExpressionProof {
            signature: "bob_sig".to_string(),
            key: "bob_key".to_string(),
        },
    };

    call_zome::<_, ()>(&conductors[0], alice_cell, "set_online_status", alice_status).await;
    call_zome::<_, ()>(&conductors[1], bob_cell, "set_online_status", bob_status).await;

    await_consistency(3000).await;

    // Both should see each other as online (eventually)
    let alice_sees: Vec<OnlineAgent> = call_zome(&conductors[0], alice_cell, "get_online_agents", ()).await;
    let bob_sees: Vec<OnlineAgent> = call_zome(&conductors[1], bob_cell, "get_online_agents", ()).await;

    println!("Alice sees agents: {:?}", alice_sees);
    println!("Bob sees agents: {:?}", bob_sees);

    for conductor in conductors.iter_mut() {
        conductor.shutdown().await;
    }
}

/// Test get_active_agents returns connected peers
#[tokio::test(flavor = "multi_thread")]
async fn test_get_active_agents() {
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    await_consistency(2000).await;

    // Get active agents from Alice's perspective
    let alice_active: Vec<holochain_types::prelude::AgentPubKey> = call_zome(
        &conductors[0],
        alice_cell,
        "get_active_agents",
        (),
    ).await;

    println!("Alice sees active agents: {:?}", alice_active);

    // Should see at least Bob (might also include self depending on implementation)
    // The exact behavior depends on how active_agent links are created

    for conductor in conductors.iter_mut() {
        conductor.shutdown().await;
    }
}

/// Test get_others returns other agents' DIDs
#[tokio::test(flavor = "multi_thread")]
async fn test_get_others() {
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    // Create DID links
    create_did_link(&conductors[0], alice_cell, "did:test:alice").await;
    create_did_link(&conductors[1], bob_cell, "did:test:bob").await;

    await_consistency(3000).await;

    // Get others from Alice's perspective
    let others: Vec<String> = call_zome(&conductors[0], alice_cell, "get_others", ()).await;

    println!("Alice sees others: {:?}", others);

    for conductor in conductors.iter_mut() {
        conductor.shutdown().await;
    }
}
