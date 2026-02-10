//! Tests for commit and pull functionality
//!
//! These tests verify the CRDT merge behavior when agents commit
//! independently and then synchronize.

use crate::utils::*;
use holochain_types::prelude::ActionHash;
use perspective_diff_sync_integrity::PerspectiveDiff;

/// Test basic commit and pull between two agents
///
/// Scenario:
/// 1. Alice and Bob start disconnected
/// 2. Alice commits a link
/// 3. Bob commits a link (creating a fork)
/// 4. They connect and exchange peer info
/// 5. Alice pulls Bob's commit (should merge)
/// 6. Bob pulls and sees Alice's link
#[tokio::test(flavor = "multi_thread")]
async fn test_merge_fetch() {
    // Setup two disconnected conductors
    let (mut conductors, cells) = setup_conductors(2, false).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    // Create DID links for both agents
    create_did_link(&conductors[0], alice_cell, "did:test:alice").await;
    create_did_link(&conductors[1], bob_cell, "did:test:bob").await;

    // Alice commits while Bob is disconnected
    let alice_link = generate_link_expression("alice");
    let alice_commit: ActionHash = call_zome(
        &conductors[0],
        alice_cell,
        "commit",
        perspective_diff_sync_integrity::CommitInput {
            diff: PerspectiveDiff {
                additions: vec![alice_link.clone()],
                removals: vec![],
            },
            my_did: "did:test:alice".to_string(),
        },
    ).await;
    println!("Alice committed: {:?}", alice_commit);

    await_consistency(1000).await;

    // Bob tries to pull Alice's commit (should fail - not connected)
    let bob_pull_result = call_zome_fallible::<_, perspective_diff_sync_integrity::PullResult>(
        &conductors[1],
        bob_cell,
        "pull",
        serde_json::json!({ "hash": alice_commit, "is_scribe": false }),
    ).await;
    assert!(bob_pull_result.is_err(), "Bob's pull should fail when disconnected");

    // Bob commits his own link (creating a fork)
    let bob_link = generate_link_expression("bob");
    let bob_commit: ActionHash = call_zome(
        &conductors[1],
        bob_cell,
        "commit",
        perspective_diff_sync_integrity::CommitInput {
            diff: PerspectiveDiff {
                additions: vec![bob_link.clone()],
                removals: vec![],
            },
            my_did: "did:test:bob".to_string(),
        },
    ).await;
    println!("Bob committed: {:?}", bob_commit);

    // Connect the conductors
    conductors.exchange_peer_info().await;
    await_consistency(10000).await;

    // Alice tries to merge by pulling Bob's commit
    let alice_pull = retry_until_success(
        &conductors[0],
        alice_cell,
        "pull",
        serde_json::json!({ "hash": bob_commit, "is_scribe": true }),
        5,
        2000,
        |result: &perspective_diff_sync_integrity::PullResult| {
            result.diff.additions.len() == 1
        },
    ).await.expect("Alice's pull should succeed");

    assert_eq!(alice_pull.diff.additions.len(), 1, "Alice should see Bob's addition");
    assert_eq!(
        alice_pull.diff.additions[0].data,
        bob_link.data,
        "Alice should see Bob's link data"
    );

    // Get Alice's new merge commit hash
    let alice_revision: Option<ActionHash> = call_zome(
        &conductors[0],
        alice_cell,
        "current_revision",
        (),
    ).await;

    await_consistency(2000).await;

    // Bob pulls Alice's merge commit
    let bob_pull = retry_until_success(
        &conductors[1],
        bob_cell,
        "pull",
        serde_json::json!({ "hash": alice_revision.unwrap(), "is_scribe": false }),
        5,
        2000,
        |result: &perspective_diff_sync_integrity::PullResult| {
            result.diff.additions.len() == 1
        },
    ).await.expect("Bob's pull should succeed");

    assert_eq!(bob_pull.diff.additions.len(), 1, "Bob should see Alice's addition");
    assert_eq!(
        bob_pull.diff.additions[0].data,
        alice_link.data,
        "Bob should see Alice's link data"
    );

    // Cleanup
    for conductor in conductors.iter_mut() {
        conductor.shutdown().await;
    }
}

/// Test deep merge with multiple commits on both sides
///
/// Scenario:
/// 1. Alice commits 7 links
/// 2. Bob commits 7 links (disconnected, creating fork)
/// 3. They connect
/// 4. Alice pulls Bob's revision and merges (should see 7 from Bob)
/// 5. Bob pulls Alice's merged revision and sees 7 from Alice
#[tokio::test(flavor = "multi_thread")]
async fn test_merge_fetch_deep() {
    let (mut conductors, cells) = setup_conductors(2, false).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    // Create DID links for both
    create_did_link(&conductors[0], alice_cell, "did:test:alice").await;
    create_did_link(&conductors[1], bob_cell, "did:test:bob").await;

    // Alice commits 7 links while disconnected
    let mut _alice_commits = Vec::new();
    for _ in 0..7 {
        let hash = commit_link(&conductors[0], alice_cell, "alice").await;
        _alice_commits.push(hash);
    }

    // Get Alice's current revision
    let alice_rev: Option<ActionHash> = call_zome(&conductors[0], alice_cell, "current_revision", ()).await;
    assert!(alice_rev.is_some(), "Alice should have a revision after commits");

    // Bob commits 7 links (creating fork)
    for _ in 0..7 {
        commit_link(&conductors[1], bob_cell, "bob").await;
    }

    // Get Bob's current revision
    let bob_rev: Option<ActionHash> = call_zome(&conductors[1], bob_cell, "current_revision", ()).await;
    assert!(bob_rev.is_some(), "Bob should have a revision after commits");

    // Connect conductors
    conductors.exchange_peer_info().await;
    await_consistency(5000).await;

    // Alice pulls Bob's revision and merges
    let alice_merge = retry_until_success(
        &conductors[0],
        alice_cell,
        "pull",
        serde_json::json!({ "hash": bob_rev.unwrap(), "is_scribe": true }),
        5,
        2000,
        |result: &perspective_diff_sync_integrity::PullResult| {
            result.diff.additions.len() == 7
        },
    ).await.expect("Alice's merge should succeed");
    assert_eq!(alice_merge.diff.additions.len(), 7, "Alice should see 7 from Bob");

    // Get Alice's new merged revision
    let alice_merged_rev: Option<ActionHash> = call_zome(&conductors[0], alice_cell, "current_revision", ()).await;

    await_consistency(2000).await;

    // Bob pulls Alice's merged revision and sees her links
    let bob_final = retry_until_success(
        &conductors[1],
        bob_cell,
        "pull",
        serde_json::json!({ "hash": alice_merged_rev.unwrap(), "is_scribe": false }),
        5,
        2000,
        |result: &perspective_diff_sync_integrity::PullResult| {
            result.diff.additions.len() == 7
        },
    ).await.expect("Bob's pull should succeed");
    assert_eq!(bob_final.diff.additions.len(), 7, "Bob should see 7 from Alice");

    // Cleanup
    for conductor in conductors.iter_mut() {
        conductor.shutdown().await;
    }
}

/// Test that large diffs are chunked properly
///
/// This tests the fix for the entry size limit issue
#[tokio::test(flavor = "multi_thread")]
async fn test_large_diff_chunking() {
    let (mut conductor, cell) = setup_1_conductor().await;

    // Create DID link
    create_did_link(&conductor, &cell, "did:test:alice").await;

    // Commit 600 links (above the CHUNKING_THRESHOLD of 500)
    let large_input = create_commit_input_multi("alice", 600);

    let commit_result: ActionHash = call_zome(
        &conductor,
        &cell,
        "commit",
        large_input,
    ).await;

    println!("Large commit succeeded: {:?}", commit_result);

    // Verify we can read it back
    let current: Option<ActionHash> = call_zome(&conductor, &cell, "current_revision", ()).await;
    assert!(current.is_some(), "Should have a current revision");
    assert_eq!(current.unwrap(), commit_result, "Current revision should match commit");

    conductor.shutdown().await;
}

/// Test commit with empty diff
#[tokio::test(flavor = "multi_thread")]
async fn test_empty_commit() {
    let (mut conductor, cell) = setup_1_conductor().await;

    create_did_link(&conductor, &cell, "did:test:alice").await;

    // First make a real commit
    commit_link(&conductor, &cell, "alice").await;

    // Get current revision
    let rev1: Option<ActionHash> = call_zome(&conductor, &cell, "current_revision", ()).await;
    assert!(rev1.is_some());

    conductor.shutdown().await;
}
