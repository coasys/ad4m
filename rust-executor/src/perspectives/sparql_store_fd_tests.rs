//! Descriptor accounting for persistent [`SparqlStore`]s (Linux: reads
//! `/proc/self/fd`). Every perspective opens one RocksDB-backed store, so the
//! descriptors a store holds multiply by the number of perspectives an
//! executor serves.

use super::*;

/// Most descriptors one persistent store may hold: the 64 SSTs of its table
/// cache, its fixed files (LOCK, LOG, MANIFEST, WAL, one directory handle per
/// column family: 17 on a fresh store), and one SST being written.
const SPARQL_STORE_FD_BUDGET: usize = 88;

fn open_fd_targets() -> Vec<String> {
    std::fs::read_dir("/proc/self/fd")
        .expect("read /proc/self/fd")
        .filter_map(|entry| entry.ok())
        .filter_map(|entry| std::fs::read_link(entry.path()).ok())
        .map(|target| target.to_string_lossy().to_string())
        .collect()
}

/// Descriptors whose target lies under `dir`: the ones a store at `dir`
/// holds, whatever other threads of the test process open meanwhile.
fn fds_under(dir: &std::path::Path) -> Vec<String> {
    let dir = dir.to_string_lossy().to_string();
    open_fd_targets()
        .into_iter()
        .filter(|target| target.starts_with(&dir))
        .collect()
}

fn subject(round: usize, i: usize) -> NamedNode {
    NamedNode::new(format!("ad4m://fd-test/s/{round}/{i}")).unwrap()
}

fn object(round: usize, i: usize) -> Literal {
    Literal::new_simple_literal(format!("value {round} {i}"))
}

/// Write `rounds` batches of distinct quads, flushing after each so RocksDB
/// writes a fresh SST per column family, and read every earlier round back by
/// subject and by object so their SSTs pass through the table cache. Returns
/// the most descriptors the store held under `dir` at any sample.
fn fill_and_read(
    store: &SparqlStore,
    dir: &std::path::Path,
    rounds: usize,
    quads_per_round: usize,
) -> usize {
    let mut peak = fds_under(dir).len();
    for round in 0..rounds {
        for i in 0..quads_per_round {
            store
                .store
                .insert(&Quad::new(
                    subject(round, i),
                    NamedNode::new(format!("ad4m://fd-test/p/{}", i % 7)).unwrap(),
                    object(round, i),
                    GraphName::DefaultGraph,
                ))
                .unwrap();
        }
        store.flush().unwrap();
        for earlier in (0..=round).step_by(1 + round / 16) {
            assert_eq!(
                store
                    .store
                    .quads_for_pattern(Some(subject(earlier, 0).as_ref().into()), None, None, None)
                    .count(),
                1
            );
            assert_eq!(
                store
                    .store
                    .quads_for_pattern(None, None, Some(object(earlier, 1).as_ref().into()), None)
                    .count(),
                1
            );
        }
        peak = peak.max(fds_under(dir).len());
    }
    assert_eq!(
        store
            .store
            .quads_for_pattern(None, None, None, None)
            .count(),
        rounds * quads_per_round
    );
    peak.max(fds_under(dir).len())
}

/// The `max_open_files` RocksDB runs the store with, from the OPTIONS file it
/// writes into the store directory on open.
fn persisted_max_open_files(data_dir: &std::path::Path) -> i64 {
    let store_dir = data_dir.join("sparql_store");
    let options = std::fs::read_dir(&store_dir)
        .unwrap()
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.file_name().to_string_lossy().starts_with("OPTIONS-"))
        .max_by_key(|entry| entry.file_name())
        .expect("RocksDB writes an OPTIONS file on open");
    std::fs::read_to_string(options.path())
        .unwrap()
        .lines()
        .find_map(|line| line.trim().strip_prefix("max_open_files="))
        .expect("OPTIONS has max_open_files")
        .parse()
        .unwrap()
}

#[test]
fn store_opens_rocksdb_with_the_capped_max_open_files() {
    let dir = tempfile::tempdir().unwrap();
    let store = SparqlStore::new(Some(dir.path().to_str().unwrap())).unwrap();
    assert_eq!(
        persisted_max_open_files(dir.path()),
        i64::from(SPARQL_STORE_MAX_OPEN_FILES),
        "oxigraph's default derives max_open_files from the process fd limit, \
         so every store may claim nearly all of it"
    );
    drop(store);
}

/// A store holds its fixed files (LOCK, LOG, MANIFEST, WAL, one directory
/// handle per column family) plus the SSTs its table cache keeps open. RocksDB
/// sizes the table cache from `max_open_files`; with oxigraph's default
/// (process fd limit minus 48) it grows with the data, once the soft limit is
/// raised without bound in practice.
#[test]
fn store_descriptors_stay_within_the_per_store_budget() {
    let dir = tempfile::tempdir().unwrap();
    let store = SparqlStore::new(Some(dir.path().to_str().unwrap())).unwrap();
    let after_open = fds_under(dir.path()).len();
    let peak = fill_and_read(&store, dir.path(), 200, 300);
    let settled = fds_under(dir.path());
    eprintln!(
        "fds per store: {after_open} after open, peak {peak} over 200 flushed rounds, {} settled",
        settled.len()
    );
    assert!(
        peak <= SPARQL_STORE_FD_BUDGET,
        "a store held {peak} descriptors, budget is {SPARQL_STORE_FD_BUDGET}: {settled:#?}"
    );
}

/// Opening N stores costs N times the per-store budget at most, and dropping
/// a store (the last `Arc` of a removed perspective) closes every descriptor
/// it held.
#[test]
fn n_stores_stay_within_budget_and_dropping_releases_their_descriptors() {
    const N: usize = 12;
    let root = tempfile::tempdir().unwrap();
    let mut stores = Vec::new();
    for i in 0..N {
        let path = root.path().join(format!("p{i}"));
        let store = SparqlStore::new(Some(path.to_str().unwrap())).unwrap();
        fill_and_read(&store, &path, 20, 100);
        // A clone shares the RocksDB handle: it must not open the files again.
        let clone = store.clone();
        stores.push((store, clone));
    }
    let held = fds_under(root.path()).len();
    eprintln!(
        "{N} stores hold {held} descriptors ({} per store)",
        held / N
    );
    assert!(
        held <= N * SPARQL_STORE_FD_BUDGET,
        "{N} stores hold {held} descriptors, budget is {}",
        N * SPARQL_STORE_FD_BUDGET
    );

    // Dropping one handle of a pair keeps the store open.
    let (first, first_clone) = stores.remove(0);
    drop(first);
    let first_path = root.path().join("p0");
    assert!(!fds_under(&first_path).is_empty());
    drop(first_clone);
    assert_eq!(fds_under(&first_path), Vec::<String>::new());

    drop(stores);
    assert_eq!(fds_under(root.path()), Vec::<String>::new());
}

/// Reopening a store after the previous handle is gone (a perspective
/// reloaded at startup) works and leaves exactly one store's descriptors.
#[test]
fn reopening_a_dropped_store_does_not_accumulate_descriptors() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().to_str().unwrap();
    let mut counts = Vec::new();
    for _ in 0..5 {
        let store = SparqlStore::new(Some(path)).unwrap();
        counts.push(fds_under(dir.path()).len());
        drop(store);
        assert_eq!(fds_under(dir.path()), Vec::<String>::new());
    }
    assert!(
        counts.iter().all(|&c| c <= SPARQL_STORE_FD_BUDGET),
        "descriptors per reopen: {counts:?}"
    );
}
