//! Step 6f — two-node end-to-end via the HolographRuntime wires.
//!
//! This test exercises the `holograph_wires` runtime surface from
//! outside `rust-executor` (i.e. through the same `pub` entry points
//! the deno op layer calls into). It complements Step 4d's
//! `tests/space_two_node.rs` (which drives `HolographSpace` directly
//! and proves cross-node propagation through K2 mem transport) by
//! confirming the wire surface plumbs through correctly for two
//! neighborhoods on the same `HolographRuntime`.
//!
//! Scope:
//!   - Create two neighborhood handles via `create_neighborhood`.
//!   - Commit a typed `WireDiff` on each via `commit`.
//!   - Verify each receives its own emit via `next_emitted`.
//!   - Verify state isolation: closing one neighborhood doesn't
//!     affect the other.
//!
//! Out of scope (covered by `space_two_node.rs`):
//!   - K2 cross-node propagation through publish/fetch.
//!   - mem-transport peer-URL cross-registration.

use rust_executor::holograph_wires::{HolographRuntime, WireDiff, WireDiffBuilder};
use std::time::Duration;

fn unique_dir(name: &str) -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix(&format!("holograph-2node-{name}-"))
        .tempdir()
        .unwrap()
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn two_neighborhoods_commit_and_emit_independently() {
    let rt = HolographRuntime::get();

    let alice_dir = unique_dir("alice");
    let bob_dir = unique_dir("bob");

    let alice = rt
        .create_neighborhood(
            "holograph-wires-test-alice",
            alice_dir.path().to_str().unwrap(),
        )
        .await
        .expect("alice create");
    let bob = rt
        .create_neighborhood("holograph-wires-test-bob", bob_dir.path().to_str().unwrap())
        .await
        .expect("bob create");
    assert_ne!(alice, bob, "handles must be distinct");

    // Alice commits a diff. Her emit channel receives it; Bob's does not.
    let alice_diff = WireDiffBuilder::default()
        .add(serde_json::json!({"source": "alice", "target": "wire"}))
        .build();
    let alice_op_id = rt
        .commit(alice, alice_diff.clone())
        .await
        .expect("alice commit");

    let alice_emit = tokio::time::timeout(Duration::from_secs(5), rt.next_emitted(alice))
        .await
        .expect("alice timeout")
        .expect("alice err")
        .expect("alice some");
    assert_eq!(alice_emit.op_id_b64, alice_op_id);
    assert_eq!(alice_emit.diff, alice_diff);

    // Bob's emit channel has no pending items — verify via a short timeout.
    let bob_drain = tokio::time::timeout(Duration::from_millis(200), rt.next_emitted(bob)).await;
    assert!(
        bob_drain.is_err(),
        "Bob's channel should not have received Alice's commit; got {:?}",
        bob_drain.ok()
    );

    // Bob commits his own diff; only Bob's channel sees it.
    let bob_diff = WireDiffBuilder::default()
        .remove(serde_json::json!({"source": "bob", "target": "wire"}))
        .build();
    let bob_op_id = rt.commit(bob, bob_diff.clone()).await.expect("bob commit");
    let bob_emit = tokio::time::timeout(Duration::from_secs(5), rt.next_emitted(bob))
        .await
        .expect("bob timeout")
        .expect("bob err")
        .expect("bob some");
    assert_eq!(bob_emit.op_id_b64, bob_op_id);
    assert_eq!(bob_emit.diff, bob_diff);

    // Closing Bob's neighborhood doesn't affect Alice's; subsequent
    // ops on Alice still succeed.
    rt.close_neighborhood(bob)
        .await
        .expect("bob close idempotent");
    let alice_diff2 = WireDiff {
        additions: vec![serde_json::json!({"a": "again"})],
        removals: vec![],
    };
    let alice_op_id2 = rt
        .commit(alice, alice_diff2.clone())
        .await
        .expect("alice second commit");
    let alice_emit2 = tokio::time::timeout(Duration::from_secs(5), rt.next_emitted(alice))
        .await
        .expect("alice 2 timeout")
        .expect("alice 2 err")
        .expect("alice 2 some");
    assert_eq!(alice_emit2.op_id_b64, alice_op_id2);
    assert_eq!(alice_emit2.diff, alice_diff2);

    rt.close_neighborhood(alice).await.expect("alice close");
}

/// Render returns the v1 placeholder shape for any active handle.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn render_shape_matches_spec() {
    let rt = HolographRuntime::get();
    let dir = unique_dir("render-spec");
    let h = rt
        .create_neighborhood(
            "holograph-wires-test-render-spec",
            dir.path().to_str().unwrap(),
        )
        .await
        .expect("create");
    let v = rt.render(h).await.expect("render");
    // The runtime surface promise: a `links` array (may be empty).
    let links = v.get("links").expect("links field present");
    assert!(links.is_array(), "links must be an array");
    rt.close_neighborhood(h).await.expect("close");
}
