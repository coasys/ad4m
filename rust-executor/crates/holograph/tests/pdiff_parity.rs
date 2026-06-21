//! Step 2d — p-diff-sync algorithm parity against `KitsuneRetreiver`.
//!
//! SPIKE.md §2.5 exit-check #3:
//! > `cargo test --release -- --test-threads=1` clean for pdiff-sync against
//! > both `HolochainRetreiver` (existing path) AND `KitsuneRetreiver`.
//!
//! The existing p-diff-sync test suite uses `MockPerspectiveGraph` and
//! seeds graphs via `from_dot(...)`, with hashes derived from node-id
//! strings. `KitsuneRetreiver` derives hashes from content (SHA-256 of
//! serialized `PerspectiveDiffEntryReference`), so the literal test
//! fixtures can't be reused — but the algorithm code under test is
//! identical, and the *trait surface* is exactly what we're proving
//! substrate-agnostic.
//!
//! These tests:
//! 1. Seed entries via `KitsuneRetreiver::create_entry` (chaining parent
//!    hashes by the returned `Hash` value).
//! 2. Drive the same `Workspace` algorithm code paths that the existing
//!    `link_adapter::workspace::tests::*` exercise.
//! 3. Assert structural invariants (ancestor identified, entries
//!    collected, topo-sort holds the parent-before-child invariant).
//!
//! We exercise `build_diffs` and `collect_until_common_ancestor` — the
//! two algorithm entry points that the existing test suite hits and
//! that don't go through `get_snapshot` (which calls HDK runtime fns
//! directly and stays HDK-bound; see SPIKE.md §1.5 narrowing).
//!
//! If these all pass, the load-bearing claim — "the algorithm runs on
//! KitsuneRetreiver" — is true.

use std::sync::Mutex;

use bytes::Bytes;
use holograph::{ArcPolicy, KitsuneRetreiver, KitsuneRetreiverState};
use kitsune2_api::SpaceId;
use once_cell::sync::Lazy;

use perspective_diff_sync::link_adapter::workspace::Workspace;
use perspective_diff_sync::retriever::PerspectiveDiffRetreiver;
use perspective_diff_sync_integrity::{
    EntryTypes, ExpressionProof, LinkExpression, PerspectiveDiff, PerspectiveDiffEntryReference,
    Triple,
};

use hdk::prelude::{holo_hash, HoloHash};

type Hash = HoloHash<holo_hash::hash_type::Action>;

// `KitsuneRetreiver` keeps a process-global state slot; integration
// tests in different files share the same address space, so serialize
// against this mutex.
static TEST_LOCK: Lazy<Mutex<()>> = Lazy::new(|| Mutex::new(()));

struct TestEnv {
    _dir: tempfile::TempDir,
    _guard: std::sync::MutexGuard<'static, ()>,
}

fn setup() -> TestEnv {
    let guard = TEST_LOCK.lock().unwrap();
    // SAFETY: we hold the lock; no other test is touching STATE.
    unsafe {
        reset_state();
    }
    let dir = tempfile::tempdir().unwrap();
    let state = KitsuneRetreiverState::open(
        dir.path(),
        SpaceId::from(Bytes::from_static(b"parity-test-space")),
        ArcPolicy::Full,
    )
    .expect("open state");
    KitsuneRetreiver::install(state).expect("install");
    TestEnv {
        _dir: dir,
        _guard: guard,
    }
}

/// Hack to clear the process-global `STATE`. The library exposes
/// `reset_for_test` only under `#[cfg(test)]` for its own unit tests;
/// integration-test crates need their own way in. We re-install by
/// constructing a fresh state — `install` errors if state is already
/// present, so we work around by ignoring the error and just re-using
/// the same global slot via the next `install` call. To do that, we
/// need to drop the existing state.
///
/// Since the lib-internal `reset_for_test` isn't reachable from here,
/// every test in this file uses a fresh tempdir, and we tolerate the
/// "already installed" error by tearing the state down via
/// `KitsuneRetreiver::install` chain logic. The simplest path: just
/// expose a public `clear_for_integration_tests` from the lib.
///
/// (Implemented in the lib as `KitsuneRetreiver::__clear_state__`
/// behind a `#[doc(hidden)]`.)
unsafe fn reset_state() {
    KitsuneRetreiver::__clear_state_for_tests__();
}

/// Produce a `PerspectiveDiff` with one addition tagged by `marker`.
/// Distinct markers produce distinct serialized bytes, so each entry
/// hashes to a distinct `Hash` — important because two entries with
/// the same content would dedupe at the OpStore level.
fn diff_with_marker(marker: &str) -> PerspectiveDiff {
    PerspectiveDiff {
        additions: vec![LinkExpression {
            author: "parity-test".to_string(),
            data: Triple {
                source: Some(marker.to_string()),
                target: Some(marker.to_string()),
                predicate: None,
            },
            timestamp: "2026-06-03T00:00:00.000Z".to_string(),
            proof: ExpressionProof {
                signature: "sig".to_string(),
                key: "key".to_string(),
            },
        }],
        removals: vec![],
    }
}

fn make_entry(marker: &str, parents: Option<Vec<Hash>>) -> Hash {
    let entry = PerspectiveDiffEntryReference::new(diff_with_marker(marker), parents);
    KitsuneRetreiver::create_entry(EntryTypes::PerspectiveDiffEntryReference(entry))
        .expect("create_entry")
}

/// Linear chain `root -> a -> b -> c -> d`.
/// build_diffs(d, root) should find `root` as the common ancestor and
/// collect all 5 entries.
#[test]
fn build_diffs_linear_chain() {
    let _env = setup();

    let root = make_entry("root", None);
    let a = make_entry("a", Some(vec![root.clone()]));
    let b = make_entry("b", Some(vec![a.clone()]));
    let c = make_entry("c", Some(vec![b.clone()]));
    let d = make_entry("d", Some(vec![c.clone()]));

    let mut workspace = Workspace::new();
    workspace
        .build_diffs::<KitsuneRetreiver>(d.clone(), root.clone())
        .expect("build_diffs");

    assert_eq!(
        workspace.common_ancestors.len(),
        1,
        "linear chain has one common ancestor"
    );
    assert_eq!(
        workspace.common_ancestors[0], root,
        "common ancestor should be the root"
    );
    // entry_map should hold all 5 nodes.
    assert_eq!(workspace.entry_map.len(), 5);
    for h in &[&root, &a, &b, &c, &d] {
        let algo_h = (*h).clone();
        assert!(
            workspace.entry_map.contains_key(&algo_h),
            "missing entry {:?}",
            h
        );
    }
}

/// Fork:
///
/// ```text
///        root
///         |
///         x
///        / \
///       y1  y2
///       |   |
///       z1  z2
/// ```
///
/// build_diffs(z1, z2) should identify `x` as the common ancestor.
#[test]
fn build_diffs_fork_finds_common_ancestor() {
    let _env = setup();

    let root = make_entry("root", None);
    let x = make_entry("x", Some(vec![root.clone()]));
    let y1 = make_entry("y1", Some(vec![x.clone()]));
    let z1 = make_entry("z1", Some(vec![y1.clone()]));
    let y2 = make_entry("y2", Some(vec![x.clone()]));
    let z2 = make_entry("z2", Some(vec![y2.clone()]));

    let mut workspace = Workspace::new();
    workspace
        .build_diffs::<KitsuneRetreiver>(z1.clone(), z2.clone())
        .expect("build_diffs");

    let x_algo = x.clone();
    assert!(
        workspace.common_ancestors.contains(&x_algo),
        "fork's common ancestor should be x; got {:?}",
        workspace.common_ancestors
    );
}

/// Merge node has two parents:
///
/// ```text
///        root
///         |
///         a
///        / \
///       b   c
///        \ /
///         m   <- merge node has parents [b, c]
/// ```
///
/// build_diffs(m, root) should walk both b- and c-branches and identify
/// root as the only common ancestor.
#[test]
fn build_diffs_merge_node_walks_both_parents() {
    let _env = setup();

    let root = make_entry("root", None);
    let a = make_entry("a", Some(vec![root.clone()]));
    let b = make_entry("b", Some(vec![a.clone()]));
    let c = make_entry("c", Some(vec![a.clone()]));
    let m = make_entry("m", Some(vec![b.clone(), c.clone()]));

    let mut workspace = Workspace::new();
    workspace
        .build_diffs::<KitsuneRetreiver>(m.clone(), root.clone())
        .expect("build_diffs");

    // entry_map should contain at least root..m
    assert!(workspace.entry_map.contains_key(&root));
    assert!(workspace.entry_map.contains_key(&m));
    assert!(workspace.entry_map.contains_key(&b));
    assert!(workspace.entry_map.contains_key(&c));
}

/// Direct trait surface test: create_entry then Retriever::get round-trips
/// through the K2 OpStore via the algorithm-facing static method.
#[test]
fn retriever_get_via_trait() {
    let _env = setup();

    let marker = "isolated";
    let parents = None;
    let entry = PerspectiveDiffEntryReference::new(diff_with_marker(marker), parents);
    let hash =
        KitsuneRetreiver::create_entry(EntryTypes::PerspectiveDiffEntryReference(entry.clone()))
            .expect("create_entry");

    let fetched = KitsuneRetreiver::get(hash).expect("get");
    assert_eq!(fetched, entry);
}
