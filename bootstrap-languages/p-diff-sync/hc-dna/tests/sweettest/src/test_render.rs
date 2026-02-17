//! Tests for render functionality and snapshots
//!
//! These tests verify that the render function correctly aggregates
//! all links from the perspective's history.

use crate::utils::*;
use holochain_types::prelude::ActionHash;
use perspective_diff_sync_integrity::Perspective;

/// Test basic render after commits
///
/// Scenario:
/// 1. Commit several links
/// 2. Verify render returns all links
#[tokio::test(flavor = "multi_thread")]
async fn test_render_basic() {
    let (mut conductor, cell) = setup_1_conductor().await;

    create_did_link(&conductor, &cell, "did:test:alice").await;

    // Commit 5 links
    for i in 0..5 {
        commit_link(&conductor, &cell, &format!("alice_{}", i)).await;
    }

    // Render should return all links
    let rendered: Perspective = call_zome(&conductor, &cell, "render", ()).await;
    assert_eq!(rendered.links.len(), 5, "Should have 5 links after rendering");

    conductor.shutdown().await;
}

/// Test render triggers snapshot creation
///
/// The SNAPSHOT_INTERVAL is set to 100 by default
#[tokio::test(flavor = "multi_thread")]
async fn test_render_with_snapshot() {
    let (mut conductor, cell) = setup_1_conductor().await;

    create_did_link(&conductor, &cell, "did:test:alice").await;

    // Commit enough links to trigger snapshot (SNAPSHOT_INTERVAL = 100)
    // We'll commit 105 to ensure a snapshot is created
    for i in 0..105 {
        if i % 20 == 0 {
            println!("Committing link {}/105...", i);
        }
        commit_link(&conductor, &cell, "alice").await;
    }

    // Render should still work after snapshot
    let rendered: Perspective = call_zome(&conductor, &cell, "render", ()).await;
    assert_eq!(rendered.links.len(), 105, "Should have 105 links after rendering");

    conductor.shutdown().await;
}

/// Test render with two agents that sync
#[tokio::test(flavor = "multi_thread")]
async fn test_render_after_sync() {
    let (mut conductors, cells) = setup_conductors(2, false).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    create_did_link(&conductors[0], alice_cell, "did:test:alice").await;
    create_did_link(&conductors[1], bob_cell, "did:test:bob").await;

    // Alice commits 3 links
    for _ in 0..3 {
        commit_link(&conductors[0], alice_cell, "alice").await;
    }

    // Bob has no current revision yet (can't render without one)
    let bob_rev_before: Option<ActionHash> = call_zome(&conductors[1], bob_cell, "current_revision", ()).await;
    assert!(bob_rev_before.is_none(), "Bob should have no revision before sync");

    // Connect and sync
    conductors.exchange_peer_info().await;
    await_consistency(10000).await;

    // Bob pulls from Alice's revision
    let alice_revision: Option<ActionHash> = call_zome(
        &conductors[0],
        alice_cell,
        "current_revision",
        (),
    ).await;
    assert!(alice_revision.is_some(), "Alice should have a revision");

    // When an agent with no current revision pulls, the pull returns an empty diff
    // but updates the current revision. This is expected behavior - the diff is emitted
    // as a signal instead of returned.
    let alice_rev = alice_revision.unwrap();

    // Retry until the data is available to fetch
    let mut success = false;
    for _attempt in 1..=10 {
        let result = call_zome_fallible::<_, perspective_diff_sync_integrity::PullResult>(
            &conductors[1],
            bob_cell,
            "pull",
            serde_json::json!({ "hash": alice_rev, "is_scribe": false }),
        ).await;

        if result.is_ok() {
            // Check if Bob now has a current revision (meaning the pull succeeded)
            let bob_rev: Option<ActionHash> = call_zome(&conductors[1], bob_cell, "current_revision", ()).await;
            if bob_rev.is_some() {
                success = true;
                break;
            }
        }
        await_consistency(2000).await;
    }

    assert!(success, "Bob should successfully pull and have a current revision");

    // Now Bob should be able to render (he has a current revision from the pull)
    let bob_render_after: Perspective = call_zome(&conductors[1], bob_cell, "render", ()).await;
    assert_eq!(bob_render_after.links.len(), 3, "Bob should see 3 links after sync");

    for conductor in conductors.iter_mut() {
        conductor.shutdown().await;
    }
}

/// Test render with removals
#[tokio::test(flavor = "multi_thread")]
async fn test_render_with_removals() {
    let (mut conductor, cell) = setup_1_conductor().await;

    create_did_link(&conductor, &cell, "did:test:alice").await;

    // Create specific links we can track
    let link1 = generate_link_expression("alice");
    let link2 = generate_link_expression("alice");
    let link3 = generate_link_expression("alice");

    // Commit additions
    let _: ActionHash = call_zome(
        &conductor,
        &cell,
        "commit",
        perspective_diff_sync_integrity::CommitInput {
            diff: perspective_diff_sync_integrity::PerspectiveDiff {
                additions: vec![link1.clone(), link2.clone(), link3.clone()],
                removals: vec![],
            },
            my_did: "did:test:alice".to_string(),
        },
    ).await;

    // Render should show 3 links
    let rendered1: Perspective = call_zome(&conductor, &cell, "render", ()).await;
    assert_eq!(rendered1.links.len(), 3, "Should have 3 links");

    // Now remove one
    let _: ActionHash = call_zome(
        &conductor,
        &cell,
        "commit",
        perspective_diff_sync_integrity::CommitInput {
            diff: perspective_diff_sync_integrity::PerspectiveDiff {
                additions: vec![],
                removals: vec![link2.clone()],
            },
            my_did: "did:test:alice".to_string(),
        },
    ).await;

    // Render should show 2 links
    let rendered2: Perspective = call_zome(&conductor, &cell, "render", ()).await;
    assert_eq!(rendered2.links.len(), 2, "Should have 2 links after removal");

    // Verify link2 is not in the result
    let has_link2 = rendered2.links.iter().any(|l| {
        l.data.source == link2.data.source && l.data.target == link2.data.target
    });
    assert!(!has_link2, "link2 should have been removed");

    conductor.shutdown().await;
}
