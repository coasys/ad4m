//! Tests for revision tracking functionality
//!
//! These tests verify that current_revision is properly tracked
//! across commits and that revision updates are isolated per agent.

use crate::utils::*;
use holochain_types::prelude::ActionHash;

/// Test that current_revision updates after each commit
#[tokio::test(flavor = "multi_thread")]
async fn test_revision_updates_on_commit() {
    let (mut conductor, cell) = setup_1_conductor().await;

    create_did_link(&conductor, &cell, "did:test:alice").await;

    // Initially no revision
    let rev0: Option<ActionHash> = call_zome(&conductor, &cell, "current_revision", ()).await;
    assert!(rev0.is_none(), "Should have no revision initially");

    // First commit
    let commit1 = commit_link(&conductor, &cell, "alice").await;
    let rev1: Option<ActionHash> = call_zome(&conductor, &cell, "current_revision", ()).await;
    assert_eq!(rev1, Some(commit1.clone()), "Revision should match first commit");

    // Second commit
    let commit2 = commit_link(&conductor, &cell, "alice").await;
    let rev2: Option<ActionHash> = call_zome(&conductor, &cell, "current_revision", ()).await;
    assert_eq!(rev2, Some(commit2.clone()), "Revision should match second commit");
    assert_ne!(rev1, rev2, "Revisions should be different");

    // Third commit
    let commit3 = commit_link(&conductor, &cell, "alice").await;
    let rev3: Option<ActionHash> = call_zome(&conductor, &cell, "current_revision", ()).await;
    assert_eq!(rev3, Some(commit3), "Revision should match third commit");

    conductor.shutdown().await;
}

/// Test that revisions are isolated between disconnected agents
#[tokio::test(flavor = "multi_thread")]
async fn test_revision_isolation() {
    let (mut conductors, cells) = setup_conductors(2, false).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    create_did_link(&conductors[0], alice_cell, "did:test:alice").await;
    create_did_link(&conductors[1], bob_cell, "did:test:bob").await;

    // Alice commits
    let alice_commit = commit_link(&conductors[0], alice_cell, "alice").await;
    let alice_rev: Option<ActionHash> = call_zome(&conductors[0], alice_cell, "current_revision", ()).await;
    assert_eq!(alice_rev, Some(alice_commit), "Alice should see her commit");

    // Bob should still have no revision (disconnected)
    let bob_rev: Option<ActionHash> = call_zome(&conductors[1], bob_cell, "current_revision", ()).await;
    assert!(bob_rev.is_none(), "Bob should have no revision yet");

    // Bob commits
    let bob_commit = commit_link(&conductors[1], bob_cell, "bob").await;
    let bob_rev2: Option<ActionHash> = call_zome(&conductors[1], bob_cell, "current_revision", ()).await;
    assert_eq!(bob_rev2, Some(bob_commit), "Bob should see his commit");

    // Alice's revision should still be her own commit
    let alice_rev2: Option<ActionHash> = call_zome(&conductors[0], alice_cell, "current_revision", ()).await;
    assert_eq!(alice_rev2, alice_rev, "Alice's revision should be unchanged");

    for conductor in conductors.iter_mut() {
        conductor.shutdown().await;
    }
}

/// Test revision updates after merge
#[tokio::test(flavor = "multi_thread")]
async fn test_revision_after_merge() {
    let (mut conductors, cells) = setup_conductors(2, false).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    create_did_link(&conductors[0], alice_cell, "did:test:alice").await;
    create_did_link(&conductors[1], bob_cell, "did:test:bob").await;

    // Both commit independently
    let alice_commit = commit_link(&conductors[0], alice_cell, "alice").await;
    let bob_commit = commit_link(&conductors[1], bob_cell, "bob").await;

    // Connect
    conductors.exchange_peer_info().await;
    await_consistency(5000).await;

    // Alice pulls Bob's commit (triggers merge)
    let _ = retry_until_success(
        &conductors[0],
        alice_cell,
        "pull",
        serde_json::json!({ "hash": bob_commit, "is_scribe": true }),
        5,
        2000,
        |result: &perspective_diff_sync_integrity::PullResult| {
            result.diff.additions.len() == 1
        },
    ).await;

    // Alice's revision should now be the merge commit (different from both originals)
    let alice_merge_rev: Option<ActionHash> = call_zome(&conductors[0], alice_cell, "current_revision", ()).await;
    assert!(alice_merge_rev.is_some(), "Alice should have a revision");
    assert_ne!(alice_merge_rev.as_ref(), Some(&alice_commit), "Merge revision should differ from Alice's original");
    assert_ne!(alice_merge_rev.as_ref(), Some(&bob_commit), "Merge revision should differ from Bob's commit");

    for conductor in conductors.iter_mut() {
        conductor.shutdown().await;
    }
}

/// Test multiple commits in sequence update revision correctly
#[tokio::test(flavor = "multi_thread")]
async fn test_sequential_commits() {
    let (mut conductor, cell) = setup_1_conductor().await;

    create_did_link(&conductor, &cell, "did:test:alice").await;

    let mut revisions = Vec::new();

    // Make 10 sequential commits
    for i in 0..10 {
        let commit = commit_link(&conductor, &cell, &format!("alice_{}", i)).await;
        let rev: Option<ActionHash> = call_zome(&conductor, &cell, "current_revision", ()).await;
        assert_eq!(rev, Some(commit.clone()), "Revision should match commit {}", i);
        revisions.push(commit);
    }

    // All revisions should be unique
    for i in 0..revisions.len() {
        for j in (i + 1)..revisions.len() {
            assert_ne!(revisions[i], revisions[j], "Revisions {} and {} should differ", i, j);
        }
    }

    conductor.shutdown().await;
}
