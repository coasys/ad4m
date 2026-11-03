//! The `ChainStore` contract suite, run on `MemoryChainStore` and `SqliteChainStore`
//! (the same cases on both), plus the SQLite-only checks: forks after reopening the file, two
//! connections racing, and `append_in` rolling back with its transaction.
#![cfg(all(feature = "store", not(target_arch = "wasm32")))]

mod common;

use ad4m_hash_chain::store::{ChainStore, MemoryChainStore, StoreError, StoredEntry};
use ad4m_hash_chain::{fold, import, ChainPolicy, ForkEvidence, Head, ImportOutcome};
use common::*;
use std::sync::Arc;

const TY: &str = TestPolicy::TYPE;

fn stored(entries: &[TestEntry]) -> Vec<StoredEntry> {
    entries
        .iter()
        .map(|e| StoredEntry::from_entry(e, 1_000 + e.seq).unwrap())
        .collect()
}

fn head_of(e: &StoredEntry) -> Head {
    Head {
        seq: e.seq,
        hash: e.hash.clone(),
    }
}

/// The contract every adapter meets. `make` gives a fresh, empty store.
async fn contract_suite<S: ChainStore + 'static>(make: impl Fn() -> S) {
    let chain = "c".to_string();
    let entries = make_chain("c", 1, 6);
    let rows = stored(&entries);

    // Empty store.
    let store = make();
    assert_eq!(store.head(TY, &chain).await.unwrap(), None);
    assert_eq!(store.range(TY, &chain, 0, 10).await.unwrap(), vec![]);
    assert_eq!(store.forks(TY, &chain).await.unwrap(), vec![]);

    // A new chain appends with expect = None; a second None is a conflict with the head.
    let h2 = store.append(TY, &chain, None, &rows[..3]).await.unwrap();
    assert_eq!(h2, head_of(&rows[2]));
    assert_eq!(
        store
            .append(TY, &chain, None, &rows[..1])
            .await
            .unwrap_err(),
        StoreError::Conflict(Some(h2.clone()))
    );

    // A stale expect is a conflict carrying the current head; nothing changes.
    let stale = head_of(&rows[1]);
    assert_eq!(
        store
            .append(TY, &chain, Some(&stale), &rows[2..4])
            .await
            .unwrap_err(),
        StoreError::Conflict(Some(h2.clone()))
    );
    // A stale expect with a batch that does not run on from it: the conflict comes first.
    assert_eq!(
        store
            .append(TY, &chain, Some(&stale), &rows[4..5])
            .await
            .unwrap_err(),
        StoreError::Conflict(Some(h2.clone()))
    );
    // An expect for a chain the store does not hold is a conflict with no head.
    assert_eq!(
        store
            .append(TY, &"other".to_string(), Some(&h2), &rows[3..4])
            .await
            .unwrap_err(),
        StoreError::Conflict(None)
    );
    assert_eq!(store.head(TY, &chain).await.unwrap(), Some(h2.clone()));

    // Entries that do not run on from expect, or no entries, are refused.
    assert!(matches!(
        store.append(TY, &chain, Some(&h2), &rows[4..5]).await,
        Err(StoreError::Backend(_))
    ));
    assert!(matches!(
        store.append(TY, &chain, Some(&h2), &[]).await,
        Err(StoreError::Backend(_))
    ));
    assert_eq!(store.head(TY, &chain).await.unwrap(), Some(h2.clone()));

    // The right expect appends.
    let h5 = store
        .append(TY, &chain, Some(&h2), &rows[3..])
        .await
        .unwrap();
    assert_eq!(h5, head_of(&rows[5]));
    assert_eq!(store.range(TY, &chain, 0, 100).await.unwrap(), rows);
    assert_eq!(
        store.range(TY, &chain, 2, 2).await.unwrap(),
        rows[2..4].to_vec()
    );
    assert_eq!(store.range(TY, &chain, 9, 2).await.unwrap(), vec![]);
    // Past every seq an SQLite integer holds: nothing, not an error.
    assert_eq!(store.range(TY, &chain, u64::MAX, 2).await.unwrap(), vec![]);
    assert_eq!(
        store
            .range(TY, &chain, i64::MAX as u64 + 1, 2)
            .await
            .unwrap(),
        vec![]
    );

    // Chain types and chains are separate.
    assert_eq!(store.head("other:type", &chain).await.unwrap(), None);

    // The stored text folds back to the same chain.
    let back: Vec<TestEntry> = store
        .range(TY, &chain, 0, 100)
        .await
        .unwrap()
        .iter()
        .map(|r| r.entry().unwrap())
        .collect();
    assert_eq!(back, entries);
    assert_eq!(fold(&TestPolicy, &back).unwrap().head(), h5);

    let theirs = next_entry(&entries[2], "their 3", &signing_key(1), None);
    let fork = ForkEvidence {
        chain: chain.clone(),
        seq: 3,
        ours: rows[3].hash.clone(),
        theirs: ad4m_hash_chain::entry_hash(&theirs).unwrap(),
    };

    // record_fork keeps both; recording twice keeps one.
    let mut held = fold(&TestPolicy, &entries).unwrap();
    let ImportOutcome::Fork(found) =
        import(&mut held, &TestPolicy, std::slice::from_ref(&theirs)).unwrap()
    else {
        panic!("expected a fork");
    };
    assert_eq!(found, fork);
    let theirs_json = ad4m_hash_chain::entry_json(&theirs).unwrap();
    store.record_fork(TY, &found, &theirs_json).await.unwrap();
    store.record_fork(TY, &found, &theirs_json).await.unwrap();
    assert_eq!(store.forks(TY, &chain).await.unwrap(), vec![fork.clone()]);
    // Our entry stays: the store holds both sides.
    assert_eq!(store.range(TY, &chain, 3, 1).await.unwrap()[0], rows[3]);
    assert_eq!(store.head(TY, &chain).await.unwrap(), Some(h5.clone()));

    // Another chain type with the same chain id holds its own entries and forks.
    const OTHER: &str = "other:type";
    assert_eq!(store.range(OTHER, &chain, 0, 100).await.unwrap(), vec![]);
    assert_eq!(store.forks(OTHER, &chain).await.unwrap(), vec![]);
    let other_rows = stored(&make_chain("c", 2, 2));
    store
        .append(OTHER, &chain, None, &other_rows)
        .await
        .unwrap();
    let other_fork = ForkEvidence {
        seq: 1,
        ours: other_rows[1].hash.clone(),
        ..fork.clone()
    };
    store.record_fork(OTHER, &other_fork, "{}").await.unwrap();
    assert_eq!(
        store.range(OTHER, &chain, 0, 100).await.unwrap(),
        other_rows
    );
    assert_eq!(store.forks(OTHER, &chain).await.unwrap(), vec![other_fork]);
    assert_eq!(store.range(TY, &chain, 0, 100).await.unwrap(), rows);
    assert_eq!(store.forks(TY, &chain).await.unwrap(), vec![fork.clone()]);
    assert_eq!(store.head(TY, &chain).await.unwrap(), Some(h5));

    // Two racing appends with one expect: one success, one conflict with the winner's head.
    let racing = Arc::new(make());
    racing.append(TY, &chain, None, &rows[..2]).await.unwrap();
    let expect = head_of(&rows[1]);
    let other_branch = stored(&[next_entry(&entries[1], "branch", &signing_key(1), None)]);
    let (a, b) = {
        let (s1, s2) = (racing.clone(), racing.clone());
        let (e1, e2) = (expect.clone(), expect.clone());
        let (c1, c2) = (chain.clone(), chain.clone());
        let ours = rows[2..3].to_vec();
        let branch = other_branch.clone();
        let t1 = tokio::spawn(async move { s1.append(TY, &c1, Some(&e1), &ours).await });
        let t2 = tokio::spawn(async move { s2.append(TY, &c2, Some(&e2), &branch).await });
        (t1.await.unwrap(), t2.await.unwrap())
    };
    check_race(a, b, &racing.head(TY, &chain).await.unwrap().unwrap());
}

fn check_race(a: Result<Head, StoreError>, b: Result<Head, StoreError>, stored_head: &Head) {
    match (a, b) {
        (Ok(win), Err(StoreError::Conflict(Some(seen))))
        | (Err(StoreError::Conflict(Some(seen))), Ok(win)) => {
            assert_eq!(&win, stored_head);
            assert_eq!(&seen, stored_head);
        }
        other => panic!("expected one success and one conflict, got {other:?}"),
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn memory_store_meets_the_contract() {
    contract_suite(MemoryChainStore::new).await;
}

#[cfg(feature = "sqlite")]
mod sqlite {
    use super::*;
    use ad4m_hash_chain::store::SqliteChainStore;
    use rusqlite::Connection;

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn sqlite_store_in_memory_meets_the_contract() {
        contract_suite(|| SqliteChainStore::open_in_memory().unwrap()).await;
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn sqlite_store_on_a_file_meets_the_contract() {
        let dir = tempfile::tempdir().unwrap();
        let counter = std::sync::atomic::AtomicU32::new(0);
        contract_suite(|| {
            let n = counter.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            SqliteChainStore::open(dir.path().join(format!("chains-{n}.db"))).unwrap()
        })
        .await;
    }

    #[tokio::test]
    async fn forks_and_entries_survive_reopening_the_file() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("chains.db");
        let chain = "c".to_string();
        let entries = make_chain("c", 1, 4);
        let theirs = next_entry(&entries[1], "theirs", &signing_key(1), None);
        let fork = ForkEvidence {
            chain: chain.clone(),
            seq: 2,
            ours: ad4m_hash_chain::entry_hash(&entries[2]).unwrap(),
            theirs: ad4m_hash_chain::entry_hash(&theirs).unwrap(),
        };
        {
            let store = SqliteChainStore::open(&path).unwrap();
            store
                .append(TY, &chain, None, &stored(&entries))
                .await
                .unwrap();
            store
                .record_fork(TY, &fork, &ad4m_hash_chain::entry_json(&theirs).unwrap())
                .await
                .unwrap();
        }
        let store = SqliteChainStore::open(&path).unwrap();
        assert_eq!(store.forks(TY, &chain).await.unwrap(), vec![fork]);
        assert_eq!(
            store.head(TY, &chain).await.unwrap(),
            Some(head_of(&stored(&entries)[3]))
        );
        let conn = Connection::open(&path).unwrap();
        let (json, seen_at): (String, i64) = conn
            .query_row(
                "SELECT theirs_json, seen_at FROM chain_forks WHERE chain = 'c'",
                [],
                |r| Ok((r.get(0)?, r.get(1)?)),
            )
            .unwrap();
        assert_eq!(serde_json::from_str::<TestEntry>(&json).unwrap(), theirs);
        assert!(seen_at > 0);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn two_connections_racing_give_one_success_and_one_conflict() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("race.db");
        let first = Arc::new(SqliteChainStore::open(&path).unwrap());
        let second = Arc::new(SqliteChainStore::open(&path).unwrap());
        for round in 0..20 {
            let chain = format!("c{round}");
            let entries = make_chain(&chain, 1, 3);
            let rows = stored(&entries);
            first.append(TY, &chain, None, &rows[..2]).await.unwrap();
            let branch = stored(&[next_entry(&entries[1], "branch", &signing_key(1), None)]);
            let expect = head_of(&rows[1]);
            let (s1, s2) = (first.clone(), second.clone());
            let (e1, e2) = (expect.clone(), expect.clone());
            let (c1, c2) = (chain.clone(), chain.clone());
            let ours = rows[2..].to_vec();
            let t1 = tokio::spawn(async move { s1.append(TY, &c1, Some(&e1), &ours).await });
            let t2 = tokio::spawn(async move { s2.append(TY, &c2, Some(&e2), &branch).await });
            let (a, b) = (t1.await.unwrap(), t2.await.unwrap());
            check_race(a, b, &first.head(TY, &chain).await.unwrap().unwrap());
            assert_eq!(first.range(TY, &chain, 0, 10).await.unwrap().len(), 3);
        }
    }

    #[tokio::test]
    async fn append_in_rolls_back_with_its_transaction() {
        let chain = "c".to_string();
        let rows = stored(&make_chain("c", 1, 3));
        let mut conn = Connection::open_in_memory().unwrap();
        SqliteChainStore::create_tables(&conn).unwrap();
        conn.execute_batch("CREATE TABLE key_index (k TEXT)")
            .unwrap();

        // Rolled back: neither the append nor the caller's own write stays.
        {
            let tx = conn.transaction().unwrap();
            let head = SqliteChainStore::append_in(&tx, TY, &chain, None, &rows).unwrap();
            assert_eq!(head, head_of(&rows[2]));
            tx.execute("INSERT INTO key_index VALUES ('k1')", [])
                .unwrap();
            assert_eq!(
                SqliteChainStore::head_in(&tx, TY, &chain).unwrap(),
                Some(head)
            );
            tx.rollback().unwrap();
        }
        assert_eq!(SqliteChainStore::head_in(&conn, TY, &chain).unwrap(), None);
        let n: i64 = conn
            .query_row("SELECT count(*) FROM key_index", [], |r| r.get(0))
            .unwrap();
        assert_eq!(n, 0);

        // A refused append_in writes nothing, and the caller's transaction still commits.
        {
            let tx = conn.transaction().unwrap();
            SqliteChainStore::append_in(&tx, TY, &chain, None, &rows[..1]).unwrap();
            let refused = SqliteChainStore::append_in(&tx, TY, &chain, None, &rows[..1]);
            assert_eq!(
                refused.unwrap_err(),
                StoreError::Conflict(Some(head_of(&rows[0])))
            );
            // Entries that skip a seq are refused, and write nothing either.
            assert!(SqliteChainStore::append_in(
                &tx,
                TY,
                &chain,
                Some(&head_of(&rows[0])),
                &rows[2..]
            )
            .is_err());
            tx.execute("INSERT INTO key_index VALUES ('k2')", [])
                .unwrap();
            tx.commit().unwrap();
        }
        let store = SqliteChainStore::from_connection(conn).unwrap();
        assert_eq!(
            store.head(TY, &chain).await.unwrap(),
            Some(head_of(&rows[0]))
        );
        assert_eq!(
            store.range(TY, &chain, 0, 10).await.unwrap(),
            rows[..1].to_vec()
        );
    }

    #[tokio::test]
    async fn append_in_that_fails_after_writing_the_head_row_writes_nothing() {
        let chain = "c".to_string();
        let rows = stored(&make_chain("c", 1, 3));
        // `stored_at` past i64::MAX: the head row is written, then the entry insert fails.
        let mut late = rows.clone();
        late[1].stored_at = u64::MAX;
        let mut conn = Connection::open_in_memory().unwrap();
        SqliteChainStore::create_tables(&conn).unwrap();
        conn.execute_batch("CREATE TABLE key_index (k TEXT)")
            .unwrap();
        {
            let tx = conn.transaction().unwrap();
            // A new chain (the head row inserted), then a held one (the head row updated).
            let refused = SqliteChainStore::append_in(&tx, TY, &chain, None, &late[..2]);
            assert!(matches!(refused, Err(StoreError::Backend(_))));
            assert_eq!(SqliteChainStore::head_in(&tx, TY, &chain).unwrap(), None);
            SqliteChainStore::append_in(&tx, TY, &chain, None, &rows[..1]).unwrap();
            let expect = head_of(&rows[0]);
            let refused = SqliteChainStore::append_in(&tx, TY, &chain, Some(&expect), &late[1..]);
            assert!(matches!(refused, Err(StoreError::Backend(_))));
            assert_eq!(
                SqliteChainStore::head_in(&tx, TY, &chain).unwrap(),
                Some(expect)
            );
            tx.execute("INSERT INTO key_index VALUES ('k')", [])
                .unwrap();
            tx.commit().unwrap();
        }
        let n: i64 = conn
            .query_row("SELECT count(*) FROM key_index", [], |r| r.get(0))
            .unwrap();
        assert_eq!(n, 1);
        let store = SqliteChainStore::from_connection(conn).unwrap();
        assert_eq!(
            store.head(TY, &chain).await.unwrap(),
            Some(head_of(&rows[0]))
        );
        assert_eq!(
            store.range(TY, &chain, 0, 10).await.unwrap(),
            rows[..1].to_vec()
        );
    }

    #[tokio::test]
    async fn a_deferred_writer_whose_head_read_went_stale_writes_nothing() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("deferred.db");
        let chain = "c".to_string();
        let entries = make_chain("c", 1, 3);
        let rows = stored(&entries);
        let branch = stored(&[next_entry(&entries[1], "branch", &signing_key(1), None)]);
        let mut conn = Connection::open(&path).unwrap();
        conn.pragma_update(None, "journal_mode", "WAL").unwrap();
        let other = SqliteChainStore::open(&path).unwrap();
        other.append(TY, &chain, None, &rows[..2]).await.unwrap();

        // The caller's deferred transaction reads the head; another connection then moves it
        // (WAL lets it commit while the reader is open).
        let tx = conn.transaction().unwrap();
        let seen = SqliteChainStore::head_in(&tx, TY, &chain).unwrap().unwrap();
        assert_eq!(seen, head_of(&rows[1]));
        other
            .append(TY, &chain, Some(&seen), &rows[2..])
            .await
            .unwrap();
        // The stale writer is refused (SQLite will not let a stale snapshot write).
        let refused = SqliteChainStore::append_in(&tx, TY, &chain, Some(&seen), &branch);
        assert!(refused.is_err(), "{refused:?}");
        drop(tx);
        // A fresh transaction with the stale expect: a conflict naming the new head.
        let tx = conn.transaction().unwrap();
        assert_eq!(
            SqliteChainStore::append_in(&tx, TY, &chain, Some(&seen), &branch).unwrap_err(),
            StoreError::Conflict(Some(head_of(&rows[2])))
        );
        tx.commit().unwrap();
        assert_eq!(
            other.head(TY, &chain).await.unwrap(),
            Some(head_of(&rows[2]))
        );
        assert_eq!(other.range(TY, &chain, 0, 10).await.unwrap(), rows);
    }

    #[tokio::test]
    async fn a_negative_stored_seq_is_an_error_not_zero() {
        let chain = "c".to_string();
        let conn = Connection::open_in_memory().unwrap();
        SqliteChainStore::create_tables(&conn).unwrap();
        conn.execute_batch(
            "INSERT INTO chain_heads VALUES ('test:chain', 'c', -1, 'Qmh');
             INSERT INTO chain_entries VALUES ('test:chain', 'c', -1, 'Qmh', '{}', 5);
             INSERT INTO chain_entries VALUES ('test:chain', 'd', 0, 'Qmh', '{}', -5);
             INSERT INTO chain_forks (type, chain, seq, ours, theirs, theirs_json)
                 VALUES ('test:chain', 'c', -2, 'Qma', 'Qmb', '{}');",
        )
        .unwrap();
        let store = SqliteChainStore::from_connection(conn).unwrap();
        assert!(matches!(
            store.head(TY, &chain).await,
            Err(StoreError::Backend(_))
        ));
        // `range` from 0 never selects a negative seq; a negative `stored_at` is an error too.
        assert!(matches!(
            store.range(TY, &"d".to_string(), 0, 10).await,
            Err(StoreError::Backend(_))
        ));
        assert!(matches!(
            store.forks(TY, &chain).await,
            Err(StoreError::Backend(_))
        ));
    }
}
