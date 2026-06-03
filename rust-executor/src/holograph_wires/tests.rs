//! Step 6 unit tests for `HolographRuntime`.
//!
//! These tests drive `HolographRuntime` directly — no deno ops, no
//! isolate, no JS. The end-to-end JS round-trip lands in the Step 6f
//! integration test.

use super::*;

fn unique_dir(name: &str) -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix(&format!("holograph-test-{name}-"))
        .tempdir()
        .unwrap()
}

#[test]
fn wire_diff_serde_round_trips() {
    let diff = WireDiffBuilder::default()
        .add(serde_json::json!({"source": "a", "target": "b"}))
        .remove(serde_json::json!({"source": "c", "target": "d"}))
        .build();
    let s = serde_json::to_string(&diff).unwrap();
    let back: WireDiff = serde_json::from_str(&s).unwrap();
    assert_eq!(diff, back);
}

#[test]
fn encode_decode_envelope_round_trip() {
    let diff = WireDiff {
        additions: vec![serde_json::json!({"k": "v"})],
        removals: vec![],
    };
    let (bytes, _) = encode_envelope(&diff).unwrap();
    let back = decode_envelope(bytes.as_ref()).unwrap();
    assert_eq!(diff, back);
}

#[test]
fn invalid_envelope_decode_returns_error() {
    let err = decode_envelope(b"not-cbor-junk").unwrap_err();
    assert!(matches!(err, HolographWireError::InvalidEnvelope(_)));
}

#[test]
fn unknown_handle_returns_error() {
    let rt = HolographRuntime::get();
    let result = rt.state(HolographHandle(99999));
    assert!(matches!(
        result,
        Err(HolographWireError::UnknownHandle { .. })
    ));
}

/// Create a neighborhood, commit a diff, observe the emit on the
/// receiver. End-to-end through `HolographRuntime` without the JS
/// layer.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn create_commit_and_emit_round_trip() {
    let dir = unique_dir("commit-emit");
    let rt = HolographRuntime::get();

    let handle = rt
        .create_neighborhood(
            "holograph-test-space-commit-emit",
            dir.path().to_str().unwrap(),
        )
        .await
        .expect("create");

    let diff = WireDiff {
        additions: vec![serde_json::json!({"source": "self", "target": "test"})],
        removals: vec![],
    };
    let op_id = rt.commit(handle, diff.clone()).await.expect("commit");
    assert!(!op_id.is_empty());

    // Subscriber: the next emit on this neighborhood is the op we
    // just committed.
    let emit = tokio::time::timeout(std::time::Duration::from_secs(5), rt.next_emitted(handle))
        .await
        .expect("next_emitted timeout")
        .expect("next_emitted ok")
        .expect("emit some");
    assert_eq!(emit.op_id_b64, op_id);
    assert_eq!(emit.diff, diff);

    rt.close_neighborhood(handle).await.expect("close");
}

/// `render` returns the v1 empty-links placeholder.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn render_returns_empty_links_v1() {
    let dir = unique_dir("render");
    let rt = HolographRuntime::get();
    let handle = rt
        .create_neighborhood("holograph-test-space-render", dir.path().to_str().unwrap())
        .await
        .expect("create");

    let v = rt.render(handle).await.expect("render");
    assert_eq!(v, serde_json::json!({"links": []}));

    rt.close_neighborhood(handle).await.expect("close");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn close_neighborhood_releases_handle() {
    let dir = unique_dir("close");
    let rt = HolographRuntime::get();
    let before = rt.handle_count();
    let handle = rt
        .create_neighborhood("holograph-test-space-close", dir.path().to_str().unwrap())
        .await
        .expect("create");
    assert_eq!(rt.handle_count(), before + 1);
    rt.close_neighborhood(handle).await.expect("close");
    assert_eq!(rt.handle_count(), before);
    // Subsequent ops on a closed handle error.
    assert!(matches!(
        rt.commit(handle, WireDiff::default()).await,
        Err(HolographWireError::UnknownHandle { .. })
    ));
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn revisions_default_to_none() {
    let dir = unique_dir("revs");
    let rt = HolographRuntime::get();
    let handle = rt
        .create_neighborhood("holograph-test-space-revs", dir.path().to_str().unwrap())
        .await
        .expect("create");
    assert_eq!(rt.current_revision(handle).await.unwrap(), None);
    assert_eq!(rt.latest_revision(handle).await.unwrap(), None);
    rt.close_neighborhood(handle).await.expect("close");
}
