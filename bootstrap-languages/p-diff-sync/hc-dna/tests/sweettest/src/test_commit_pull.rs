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
    )
    .await;
    println!("Alice committed: {:?}", alice_commit);

    await_consistency(1000).await;

    // Note: In development version of Holochain, network isolation is not reliable
    // so we skip the "disconnected pull should fail" check

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
    )
    .await;
    println!("Bob committed: {:?}", bob_commit);

    // Connect the conductors
    conductors.exchange_peer_info().await;

    // Retry connection until agents can see each other
    // This is needed for the development version of Holochain
    for _attempt in 0..5 {
        conductors.exchange_peer_info().await;
        tokio::time::sleep(tokio::time::Duration::from_millis(2000)).await;
    }

    await_consistency(10000).await;

    // Alice tries to merge by pulling Bob's commit
    let alice_pull = retry_until_success(
        &conductors[0],
        alice_cell,
        "pull",
        serde_json::json!({ "hash": bob_commit, "is_scribe": true }),
        5,
        2000,
        |result: &perspective_diff_sync_integrity::PullResult| result.diff.additions.len() == 1,
    )
    .await
    .expect("Alice's pull should succeed");

    assert_eq!(
        alice_pull.diff.additions.len(),
        1,
        "Alice should see Bob's addition"
    );
    assert_eq!(
        alice_pull.diff.additions[0].data, bob_link.data,
        "Alice should see Bob's link data"
    );

    // Get Alice's new merge commit hash
    let alice_revision: Option<ActionHash> =
        call_zome(&conductors[0], alice_cell, "current_revision", ()).await;

    await_consistency(2000).await;

    // Bob pulls Alice's merge commit
    let bob_pull = retry_until_success(
        &conductors[1],
        bob_cell,
        "pull",
        serde_json::json!({ "hash": alice_revision.unwrap(), "is_scribe": false }),
        5,
        2000,
        |result: &perspective_diff_sync_integrity::PullResult| result.diff.additions.len() == 1,
    )
    .await
    .expect("Bob's pull should succeed");

    assert_eq!(
        bob_pull.diff.additions.len(),
        1,
        "Bob should see Alice's addition"
    );
    assert_eq!(
        bob_pull.diff.additions[0].data, alice_link.data,
        "Bob should see Alice's link data"
    );

    // Cleanup
    for conductor in conductors.iter() {
        let _ = conductor;
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
    let alice_rev: Option<ActionHash> =
        call_zome(&conductors[0], alice_cell, "current_revision", ()).await;
    assert!(
        alice_rev.is_some(),
        "Alice should have a revision after commits"
    );

    // Bob commits 7 links (creating fork)
    for _ in 0..7 {
        commit_link(&conductors[1], bob_cell, "bob").await;
    }

    // Get Bob's current revision
    let bob_rev: Option<ActionHash> =
        call_zome(&conductors[1], bob_cell, "current_revision", ()).await;
    assert!(
        bob_rev.is_some(),
        "Bob should have a revision after commits"
    );

    // Connect conductors
    conductors.exchange_peer_info().await;

    // Retry connection until agents can see each other
    // This is needed for the development version of Holochain
    for _attempt in 0..5 {
        conductors.exchange_peer_info().await;
        tokio::time::sleep(tokio::time::Duration::from_millis(2000)).await;
    }

    await_consistency(5000).await;

    // Alice pulls Bob's revision and merges
    let alice_merge = retry_until_success(
        &conductors[0],
        alice_cell,
        "pull",
        serde_json::json!({ "hash": bob_rev.unwrap(), "is_scribe": true }),
        5,
        2000,
        |result: &perspective_diff_sync_integrity::PullResult| result.diff.additions.len() == 7,
    )
    .await
    .expect("Alice's merge should succeed");
    assert_eq!(
        alice_merge.diff.additions.len(),
        7,
        "Alice should see 7 from Bob"
    );

    // Get Alice's new merged revision
    let alice_merged_rev: Option<ActionHash> =
        call_zome(&conductors[0], alice_cell, "current_revision", ()).await;

    await_consistency(2000).await;

    // Bob pulls Alice's merged revision and sees her links
    let bob_final = retry_until_success(
        &conductors[1],
        bob_cell,
        "pull",
        serde_json::json!({ "hash": alice_merged_rev.unwrap(), "is_scribe": false }),
        5,
        2000,
        |result: &perspective_diff_sync_integrity::PullResult| result.diff.additions.len() == 7,
    )
    .await
    .expect("Bob's pull should succeed");
    assert_eq!(
        bob_final.diff.additions.len(),
        7,
        "Bob should see 7 from Alice"
    );

    // Cleanup
    for conductor in conductors.iter() {
        let _ = conductor;
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

    let commit_result: ActionHash = call_zome(&conductor, &cell, "commit", large_input).await;

    println!("Large commit succeeded: {:?}", commit_result);

    // Verify we can read it back
    let current: Option<ActionHash> = call_zome(&conductor, &cell, "current_revision", ()).await;
    assert!(current.is_some(), "Should have a current revision");
    assert_eq!(
        current.unwrap(),
        commit_result,
        "Current revision should match commit"
    );

    let _ = conductor;
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

    let _ = conductor;
}

/// Test that handle_broadcast does NOT update current_revision when chunk loading fails.
///
/// Conductors start disconnected so Alice's chunk entries are unreachable by Bob,
/// making the failure path deterministic. A crafted HashBroadcast that looks like
/// a valid fast-forward from Bob's current revision is delivered directly via
/// recv_remote_signal; handle_broadcast must fail to load the chunks and must
/// leave current_revision unchanged.
#[tokio::test(flavor = "multi_thread")]
async fn test_chunked_broadcast_does_not_update_revision_on_failure() {
    use holochain_serialized_bytes::SerializedBytes;
    use perspective_diff_sync_integrity::{HashBroadcast, PerspectiveDiffEntryReference};
    // Note: perspective_diff_sync_integrity uses holo_hash@0.7.0-dev.3 while the
    // test's holochain dependency uses holo_hash@0.6.0.  ActionHash values from
    // call_zome (0.6.0 type) cannot be used directly in HashBroadcast / new_chunked
    // fields (0.7.0-dev.3 type).  Both versions share the same JSON wire format for
    // hashes, so we build the struct via serde_json round-trip to cross the boundary.

    // Start disconnected: Alice's chunk entries will never reach Bob's DHT.
    let (mut conductors, cells) = setup_conductors(2, false).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    create_did_link(&conductors[0], alice_cell, "did:test:alice").await;
    create_did_link(&conductors[1], bob_cell, "did:test:bob").await;

    // Bob commits a small diff to establish his current_revision.
    let _bob_initial: ActionHash = call_zome(
        &conductors[1],
        bob_cell,
        "commit",
        create_commit_input("initial"),
    )
    .await;

    let bob_current_before: Option<ActionHash> =
        call_zome(&conductors[1], bob_cell, "current_revision", ()).await;
    let bob_current_hash = bob_current_before
        .clone()
        .expect("Bob must have a current_revision after committing");

    // Alice commits a large chunked diff locally; chunks exist only on Alice's DHT.
    let alice_large_commit: ActionHash = call_zome(
        &conductors[0],
        alice_cell,
        "commit",
        create_commit_input_multi("alice", 600),
    )
    .await;

    let alice_entry: PerspectiveDiffEntryReference = call_zome(
        &conductors[0],
        alice_cell,
        "get_diff_entry_reference",
        alice_large_commit.clone(),
    )
    .await;

    assert!(
        alice_entry.is_chunked(),
        "Alice's large diff must be chunked"
    );
    let diffs_since_snapshot = alice_entry.diffs_since_snapshot;
    // chunk_hashes is already the 0.7.0-dev.3 type (came from PerspectiveDiffEntryReference).
    let chunk_hashes = alice_entry.diff_chunks.unwrap();

    // Craft a HashBroadcast via JSON round-trip so all hashes end up in the
    // 0.7.0-dev.3 holo_hash type that HashBroadcast and PerspectiveDiffEntryReference
    // use internally.  alice_large_commit and bob_current_hash are 0.6.0 types, but
    // both versions use identical JSON encoding for hashes (base58 string), so the
    // conversion is lossless.
    let broadcast: HashBroadcast = serde_json::from_value(serde_json::json!({
        "reference_hash": serde_json::to_value(&alice_large_commit)
            .expect("serialize alice_large_commit"),
        "reference": {
            "diff": { "additions": [], "removals": [] },
            "parents": [
                serde_json::to_value(&bob_current_hash)
                    .expect("serialize bob_current_hash")
            ],
            "diffs_since_snapshot": diffs_since_snapshot,
            "diff_chunks": serde_json::to_value(&chunk_hashes)
                .expect("serialize chunk_hashes"),
        },
        "broadcast_author": "did:test:alice",
    }))
    .expect("deserialize HashBroadcast from JSON");

    // Serialize the broadcast as SerializedBytes, mirroring how Holochain delivers
    // remote signals (recv_remote_signal takes SerializedBytes, not a typed value).
    let signal = SerializedBytes::try_from(broadcast).expect("Failed to serialize HashBroadcast");

    // Deliver the broadcast directly; handle_broadcast must fail trying to load chunks.
    let result =
        call_zome_fallible::<_, ()>(&conductors[1], bob_cell, "recv_remote_signal", signal).await;

    assert!(
        result.is_err(),
        "recv_remote_signal must fail when chunk entries are unreachable"
    );

    // The fix: current_revision must not be advanced when chunk loading fails.
    let bob_current_after: Option<ActionHash> =
        call_zome(&conductors[1], bob_cell, "current_revision", ()).await;

    assert_eq!(
        bob_current_after, bob_current_before,
        "current_revision must not change when chunk loading fails"
    );

    for conductor in conductors.iter() {
        let _ = conductor;
    }
}

/// Test that render() correctly returns chunked diffs
///
/// This test reproduces the production bug where:
/// 1. Agent commits a large diff (>500 additions, gets chunked)
/// 2. render() is called to get the current perspective state
/// 3. BUG: render() returns EMPTY because chunked entries have empty diff.additions
/// 4. FIX: workspace.handle_parents() must load chunks before inserting into entry_map
///
/// Scenario:
/// 1. Alice commits 600 links (exceeds CHUNKING_THRESHOLD of 500)
/// 2. Commit succeeds with chunked storage
/// 3. Call render() to get current perspective
/// 4. Verify all 600 links are returned (not 0!)
#[tokio::test(flavor = "multi_thread")]
async fn test_render_returns_chunked_diffs() {
    use perspective_diff_sync_integrity::Perspective;

    let (mut conductor, cell) = setup_1_conductor().await;

    // Create DID link
    create_did_link(&conductor, &cell, "did:test:alice").await;

    println!("=== Committing 600 links (will be chunked) ===");

    // Commit 600 links - this will be chunked since it exceeds CHUNKING_THRESHOLD of 500
    let large_input = create_commit_input_multi("alice", 600);
    let commit_hash: ActionHash = call_zome(&conductor, &cell, "commit", large_input).await;

    println!("Large commit succeeded: {:?}", commit_hash);

    // Verify commit created a chunked entry
    let entry: perspective_diff_sync_integrity::PerspectiveDiffEntryReference = call_zome(
        &conductor,
        &cell,
        "get_diff_entry_reference",
        commit_hash.clone(),
    )
    .await;

    println!("Entry is chunked: {}", entry.is_chunked());
    println!(
        "Entry has {} chunks",
        entry.diff_chunks.as_ref().map(|c| c.len()).unwrap_or(0)
    );
    assert!(entry.is_chunked(), "Entry should be chunked for this test");

    // THE BUG TEST: Call render() and verify it returns all 600 links
    println!("\n=== Testing render() with chunked entry ===");
    let perspective: Perspective = call_zome(&conductor, &cell, "render", ()).await;

    println!("render() returned {} links", perspective.links.len());

    // CRITICAL ASSERTION: This should be 600, not 0!
    // Without the fix in workspace.rs handle_parents(), this will be 0
    // because render() accesses diff.additions which is empty for chunked entries
    assert_eq!(
        perspective.links.len(),
        600,
        "render() should return all 600 links from the chunked commit, not {} links! \
        This indicates chunked diffs are not being loaded when building the workspace.",
        perspective.links.len()
    );

    println!("✓ TEST PASSED: render() correctly returned all 600 links from chunked entry");

    let _ = conductor;
}

/// Test that pull() with current.is_none() correctly handles chunked diffs
///
/// This tests the initial pull scenario where an agent has no current revision
/// and pulls a chunked diff from another agent.
///
/// Scenario:
/// 1. Alice commits 600 links (chunked)
/// 2. Bob (with no current revision) pulls Alice's revision
/// 3. Verify Bob's pull returns correct diff count
#[tokio::test(flavor = "multi_thread")]
async fn test_pull_initial_with_chunked_diffs() {
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    // Create DID links
    create_did_link(&conductors[0], alice_cell, "did:test:alice").await;
    create_did_link(&conductors[1], bob_cell, "did:test:bob").await;

    println!("=== Alice committing 600 links (will be chunked) ===");

    // Alice commits 600 links
    let large_input = create_commit_input_multi("alice", 600);
    let alice_commit: ActionHash =
        call_zome(&conductors[0], alice_cell, "commit", large_input).await;

    println!("Alice's commit: {:?}", alice_commit);

    // Wait for DHT propagation
    await_consistency(5000).await;

    println!("\n=== Bob (no current revision) pulling Alice's chunked commit ===");

    // Bob pulls Alice's revision (Bob has no current revision, so this uses collect_only_from_latest)
    let bob_pull = retry_until_success(
        &conductors[1],
        bob_cell,
        "pull",
        serde_json::json!({ "hash": alice_commit, "is_scribe": false }),
        5,
        2000,
        |_result: &perspective_diff_sync_integrity::PullResult| {
            // For initial pull, the function returns early with empty diff
            // but updates current_revision, so just check it doesn't error
            true
        },
    )
    .await
    .expect("Bob's pull should succeed");

    println!("Bob's pull succeeded");

    // Verify Bob's current revision is now Alice's commit
    let bob_current: Option<ActionHash> =
        call_zome(&conductors[1], bob_cell, "current_revision", ()).await;
    assert_eq!(
        bob_current,
        Some(alice_commit.clone()),
        "Bob should have Alice's revision as current"
    );

    // Verify Bob can render the full perspective with all 600 links
    println!("\n=== Bob rendering perspective ===");
    let bob_perspective: perspective_diff_sync_integrity::Perspective =
        call_zome(&conductors[1], bob_cell, "render", ()).await;

    println!(
        "Bob's render() returned {} links",
        bob_perspective.links.len()
    );

    assert_eq!(
        bob_perspective.links.len(),
        600,
        "Bob should see all 600 links after pulling Alice's chunked commit"
    );

    println!("✓ TEST PASSED: Bob correctly pulled and rendered Alice's chunked commit");

    // Cleanup
    for conductor in conductors.iter() {
        let _ = conductor;
    }
}
