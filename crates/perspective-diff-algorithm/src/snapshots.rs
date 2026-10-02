//! Snapshot builder — substrate-agnostic.
//!
//! Originally lived in
//! `bootstrap-languages/p-diff-sync/hc-dna/zomes/perspective_diff_sync/src/link_adapter/snapshots.rs`,
//! parameterized on the integrity-zome `PerspectiveDiffEntryReference` /
//! `Snapshot` types and HDK runtime calls (`get`, `hash_entry`,
//! `LinkQuery`, `get_links`).
//!
//! Step 13b-D (wake-16): the pure DAG walk + chunk-aggregation logic
//! moves here, parameterized on the algorithm-crate mirror types and
//! the [`SnapshotRetriever`] / [`WorkspaceRetriever`] traits. The HDK
//! adapter (`link_adapter::snapshots`) becomes a ~10-line shim that
//! converts integrity ↔ mirror types at the boundary.
//!
//! Behaviour matches the original commit-time snapshot generation:
//!
//! 1. Walk parents from `latest` (DFS with sibling-branch deferral).
//! 2. At each node, aggregate inline / chunked diffs.
//! 3. If we hit a node with `diffs_since_snapshot == 0` AND a
//!    `Snapshot` link is attached, fold the snapshot's diffs into the
//!    aggregator, mark its `included_diffs` as seen, and stop walking
//!    that branch.
//! 4. After the walk, chunk the aggregated additions/removals, write
//!    each chunk back to the substrate, and assemble a new `Snapshot`
//!    record (the caller persists it).

use std::collections::{BTreeSet, HashSet};

use crate::chunked_diffs::{load_diff_aggregated, ChunkedDiffs};
use crate::errors::AlgoResult;
use crate::retriever::SnapshotRetriever;
use perspective_diff_types::{Hash, LinkExpression, PerspectiveDiffEntryReference, Snapshot};

struct SearchPosition {
    hash: Hash,
    is_unseen: bool,
}

/// Build a `Snapshot` summarizing every diff between `latest` and the
/// nearest existing snapshot (or the orphan root). `chunk_size` caps
/// how many additions+removals fit in each chunk entry the new
/// snapshot references.
pub fn generate_snapshot<R: SnapshotRetriever>(
    latest: Hash,
    chunk_size: u16,
) -> AlgoResult<Snapshot> {
    let mut search_position = SearchPosition {
        hash: latest.clone(),
        is_unseen: false,
    };
    let mut seen: HashSet<Hash> = HashSet::new();
    let mut unseen_parents: Vec<SearchPosition> = Vec::new();

    let mut all_additions: BTreeSet<LinkExpression> = BTreeSet::new();
    let mut all_removals: BTreeSet<LinkExpression> = BTreeSet::new();

    loop {
        let diff = R::get_p_diff_reference(&search_position.hash)?;

        if diff.diffs_since_snapshot == 0 && search_position.hash != latest {
            // Boundary node — look for an attached Snapshot to fold in.
            let snapshot_opt = R::get_snapshot_by_target(&search_position.hash)?;

            if let Some(snapshot) = snapshot_opt {
                // Materialize the snapshot's diff by walking its chunk
                // hashes. Reuses the shared chunked-load helper by
                // synthesising a placeholder entry-ref that points at
                // the chunks.
                let placeholder = PerspectiveDiffEntryReference {
                    diff: perspective_diff_types::PerspectiveDiff::new(),
                    parents: None,
                    diffs_since_snapshot: 0,
                    diff_chunks: Some(snapshot.diff_chunks.clone()),
                };
                let aggregated = load_diff_aggregated::<R>(&placeholder)?;
                for addition in aggregated.additions {
                    all_additions.insert(addition);
                }
                for removal in aggregated.removals {
                    all_removals.insert(removal);
                }
                for hash in &snapshot.included_diffs {
                    seen.insert(hash.clone());
                }
                if unseen_parents.is_empty() {
                    break;
                } else {
                    search_position = unseen_parents.remove(0);
                }
            } else {
                // No snapshot attached — treat the node as a regular
                // parent and fall through to the BFS.
                let should_break = handle_parents::<R>(
                    diff,
                    &mut search_position,
                    &mut seen,
                    &mut unseen_parents,
                    &mut all_additions,
                    &mut all_removals,
                )?;
                if should_break {
                    break;
                }
            }
        } else {
            let should_break = handle_parents::<R>(
                diff,
                &mut search_position,
                &mut seen,
                &mut unseen_parents,
                &mut all_additions,
                &mut all_removals,
            )?;
            if should_break {
                break;
            }
        }
    }

    // Write each chunk back to the substrate and assemble the snapshot
    // record. The caller (commit) persists the Snapshot itself + the
    // snapshot-link from the source entry.
    let mut chunked_diffs = ChunkedDiffs::new(chunk_size);
    chunked_diffs.add_additions(all_additions.into_iter().collect());
    chunked_diffs.add_removals(all_removals.into_iter().collect());

    let mut chunk_hashes: Vec<Hash> = Vec::with_capacity(chunked_diffs.chunks.len());
    for chunk in chunked_diffs.chunks {
        let entry = PerspectiveDiffEntryReference::new(chunk, None);
        let hash = R::create_diff_entry(entry)?;
        chunk_hashes.push(hash);
    }

    Ok(Snapshot {
        diff_chunks: chunk_hashes,
        included_diffs: seen.into_iter().collect(),
    })
}

fn handle_parents<R: SnapshotRetriever>(
    diff: PerspectiveDiffEntryReference,
    search_position: &mut SearchPosition,
    seen: &mut HashSet<Hash>,
    unseen_parents: &mut Vec<SearchPosition>,
    all_additions: &mut BTreeSet<LinkExpression>,
    all_removals: &mut BTreeSet<LinkExpression>,
) -> AlgoResult<bool> {
    if !seen.contains(&search_position.hash) {
        seen.insert(search_position.hash.clone());

        let loaded_diff = load_diff_aggregated::<R>(&diff)?;
        for addition in loaded_diff.additions {
            all_additions.insert(addition);
        }
        for removal in loaded_diff.removals {
            all_removals.insert(removal);
        }

        if diff.parents.is_none() {
            if unseen_parents.is_empty() {
                return Ok(true);
            }
            *search_position = unseen_parents.remove(0);
            return Ok(false);
        }

        let mut parents = diff.parents.unwrap();
        if parents.iter().all(|val| seen.contains(val)) {
            if unseen_parents.is_empty() {
                return Ok(true);
            }
            *search_position = unseen_parents.remove(0);
            return Ok(false);
        }

        *search_position = SearchPosition {
            hash: parents.remove(0),
            is_unseen: false,
        };
        unseen_parents.append(
            &mut parents
                .into_iter()
                .map(|val| SearchPosition {
                    hash: val,
                    is_unseen: true,
                })
                .collect(),
        );
        Ok(false)
    } else if search_position.is_unseen {
        if unseen_parents.is_empty() {
            return Ok(true);
        }
        *search_position = unseen_parents.remove(0);
        Ok(false)
    } else if diff.parents.is_none() {
        if unseen_parents.is_empty() {
            return Ok(true);
        }
        *search_position = unseen_parents.remove(0);
        Ok(false)
    } else {
        let mut parents = diff.parents.unwrap();
        if parents.iter().all(|val| seen.contains(val)) {
            if unseen_parents.is_empty() {
                return Ok(true);
            }
            *search_position = unseen_parents.remove(0);
            return Ok(false);
        }
        *search_position = SearchPosition {
            hash: parents.remove(0),
            is_unseen: false,
        };
        unseen_parents.append(
            &mut parents
                .into_iter()
                .map(|val| SearchPosition {
                    hash: val,
                    is_unseen: true,
                })
                .collect(),
        );
        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::retriever::WorkspaceRetriever;
    use once_cell::sync::Lazy;
    use perspective_diff_types::{ExpressionProof, PerspectiveDiff, Triple};
    use std::collections::BTreeMap;
    use std::sync::Mutex;

    // Tiny in-crate retriever for the snapshot tests. Tracks the
    // diff-entry table and a side-table for snapshot links. Both
    // mutable so a single test can seed entries and then assert on
    // chunks written by `generate_snapshot`.

    #[derive(Default)]
    struct MockStore {
        entries: BTreeMap<Hash, PerspectiveDiffEntryReference>,
        snapshots_by_target: BTreeMap<Hash, Snapshot>,
        next_id: u32,
    }

    static STORE: Lazy<Mutex<MockStore>> = Lazy::new(|| Mutex::new(MockStore::default()));

    fn reset() {
        let mut g = STORE.lock().unwrap();
        *g = MockStore::default();
    }

    fn next_hash(g: &mut MockStore) -> Hash {
        g.next_id += 1;
        let mut buf = [0u8; 36];
        buf[..4].copy_from_slice(&g.next_id.to_be_bytes());
        Hash::from_raw_36(buf.to_vec())
    }

    fn put_entry(diff: PerspectiveDiff, parents: Option<Vec<Hash>>) -> Hash {
        let mut g = STORE.lock().unwrap();
        let hash = next_hash(&mut g);
        let entry = PerspectiveDiffEntryReference {
            diff,
            parents,
            diffs_since_snapshot: 0,
            diff_chunks: None,
        };
        g.entries.insert(hash.clone(), entry);
        hash
    }

    fn put_entry_with_dss(
        diff: PerspectiveDiff,
        parents: Option<Vec<Hash>>,
        diffs_since_snapshot: usize,
    ) -> Hash {
        let mut g = STORE.lock().unwrap();
        let hash = next_hash(&mut g);
        let entry = PerspectiveDiffEntryReference {
            diff,
            parents,
            diffs_since_snapshot,
            diff_chunks: None,
        };
        g.entries.insert(hash.clone(), entry);
        hash
    }

    struct MockRetriever;

    impl WorkspaceRetriever for MockRetriever {
        fn get_p_diff_reference(hash: &Hash) -> AlgoResult<PerspectiveDiffEntryReference> {
            let g = STORE.lock().unwrap();
            g.entries
                .get(hash)
                .cloned()
                .ok_or(crate::errors::AlgoError::Retriever(format!(
                    "mock: hash not found"
                )))
        }
        fn get_snapshot_by_target(target: &Hash) -> AlgoResult<Option<Snapshot>> {
            let g = STORE.lock().unwrap();
            Ok(g.snapshots_by_target.get(target).cloned())
        }
    }

    impl SnapshotRetriever for MockRetriever {
        fn create_diff_entry(entry: PerspectiveDiffEntryReference) -> AlgoResult<Hash> {
            let mut g = STORE.lock().unwrap();
            let hash = next_hash(&mut g);
            g.entries.insert(hash.clone(), entry);
            Ok(hash)
        }
    }

    fn lnk(s: &str) -> LinkExpression {
        LinkExpression {
            author: "t".into(),
            data: Triple {
                source: Some(s.into()),
                target: Some(s.into()),
                predicate: None,
            },
            timestamp: "0".into(),
            proof: ExpressionProof {
                signature: "".into(),
                key: "".into(),
            },
        }
    }

    #[test]
    fn collects_inline_chain_into_chunked_snapshot() {
        reset();
        // root <- a <- b <- c   (c is "latest")
        let root = put_entry(
            PerspectiveDiff {
                additions: vec![lnk("L0")],
                removals: vec![],
            },
            None,
        );
        let a = put_entry(
            PerspectiveDiff {
                additions: vec![lnk("L1")],
                removals: vec![],
            },
            Some(vec![root.clone()]),
        );
        let b = put_entry(
            PerspectiveDiff {
                additions: vec![lnk("L2")],
                removals: vec![],
            },
            Some(vec![a.clone()]),
        );
        let c = put_entry(
            PerspectiveDiff {
                additions: vec![lnk("L3")],
                removals: vec![],
            },
            Some(vec![b.clone()]),
        );

        let snapshot = generate_snapshot::<MockRetriever>(c, 10).expect("snapshot");

        // included_diffs should contain all four entries
        assert_eq!(snapshot.included_diffs.len(), 4);
        assert!(snapshot.included_diffs.contains(&root));
        assert!(snapshot.included_diffs.contains(&a));
        assert!(snapshot.included_diffs.contains(&b));

        // chunks should sum to 4 link expressions across all chunk entries
        let g = STORE.lock().unwrap();
        let mut total = 0usize;
        for h in &snapshot.diff_chunks {
            let e = g.entries.get(h).expect("chunk written");
            total += e.diff.additions.len() + e.diff.removals.len();
        }
        assert_eq!(total, 4, "aggregated 4 links into chunked snapshot");
    }

    #[test]
    fn folds_previous_snapshot_into_new_one() {
        reset();
        // prior snapshot has chunks { snap_chunk } summarizing diffs [s1, s2]
        let snap_chunk = put_entry(
            PerspectiveDiff {
                additions: vec![lnk("S1"), lnk("S2")],
                removals: vec![],
            },
            None,
        );
        let s_inc1 = put_entry(PerspectiveDiff::new(), None);
        let s_inc2 = put_entry(PerspectiveDiff::new(), None);
        let prior_snapshot = Snapshot {
            diff_chunks: vec![snap_chunk],
            included_diffs: vec![s_inc1.clone(), s_inc2.clone()],
        };

        // boundary node `b0` has diffs_since_snapshot=0 and a snapshot
        // link → generate_snapshot folds the prior snapshot into the
        // new one.
        let b0 = put_entry_with_dss(
            PerspectiveDiff {
                additions: vec![lnk("B0-ignored-on-boundary")],
                removals: vec![],
            },
            None,
            0,
        );
        STORE
            .lock()
            .unwrap()
            .snapshots_by_target
            .insert(b0.clone(), prior_snapshot);

        // Forward chain: b0 <- d1 <- d2 (d2 is latest, dss > 0)
        let d1 = put_entry_with_dss(
            PerspectiveDiff {
                additions: vec![lnk("D1")],
                removals: vec![],
            },
            Some(vec![b0.clone()]),
            1,
        );
        let d2 = put_entry_with_dss(
            PerspectiveDiff {
                additions: vec![lnk("D2")],
                removals: vec![],
            },
            Some(vec![d1.clone()]),
            2,
        );

        let snapshot = generate_snapshot::<MockRetriever>(d2.clone(), 10).expect("snapshot");

        // Aggregated chunks should hold S1 + S2 + D1 + D2 = 4 link expressions
        let g = STORE.lock().unwrap();
        let mut total = 0usize;
        for h in &snapshot.diff_chunks {
            let e = g.entries.get(h).expect("chunk written");
            total += e.diff.additions.len() + e.diff.removals.len();
        }
        assert_eq!(total, 4, "S1+S2+D1+D2 folded into new snapshot chunks");

        // included_diffs from the prior snapshot should be carried forward.
        assert!(snapshot.included_diffs.contains(&s_inc1));
        assert!(snapshot.included_diffs.contains(&s_inc2));
    }
}
