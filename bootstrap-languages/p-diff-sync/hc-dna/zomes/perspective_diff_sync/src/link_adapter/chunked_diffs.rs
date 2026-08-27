//! Holochain-side adapter onto the substrate-agnostic chunked-diff
//! splitter / aggregator.
//!
//! Step 13a (the wide extraction Nico asked for in the wake-13 audio
//! note): the pure splitter/aggregator logic now lives in
//! `perspective_diff_algorithm::ChunkedDiffs`. This module keeps the
//! HDK IO side (create-entry / get / DHT round-trips) plus the
//! integrity-zome ↔ algorithm-mirror-type conversions.

use hdk::prelude::*;
use perspective_diff_algorithm::ChunkedDiffs as AlgoChunkedDiffs;
use perspective_diff_sync_integrity::{
    EntryTypes, LinkExpression, PerspectiveDiff, PerspectiveDiffEntryReference,
};

use crate::errors::SocialContextResult;
use crate::retriever::PerspectiveDiffRetreiver;
use crate::{Hash, CHUNK_SIZE};

// ---- integrity ↔ algorithm conversions ---------------------------------
//
// The algorithm crate's mirror types have identical serde shape but no
// HDI/SerializedBytes derives, so the conversions are field-by-field
// (cheap; no allocations beyond the inner Vecs).

fn link_to_algo(l: LinkExpression) -> perspective_diff_algorithm::LinkExpression {
    perspective_diff_algorithm::LinkExpression {
        author: l.author,
        data: perspective_diff_algorithm::Triple {
            source: l.data.source,
            target: l.data.target,
            predicate: l.data.predicate,
        },
        timestamp: l.timestamp,
        proof: perspective_diff_algorithm::ExpressionProof {
            signature: l.proof.signature,
            key: l.proof.key,
        },
    }
}

fn link_from_algo(l: perspective_diff_algorithm::LinkExpression) -> LinkExpression {
    use perspective_diff_sync_integrity::{ExpressionProof, Triple};
    LinkExpression {
        author: l.author,
        data: Triple {
            source: l.data.source,
            target: l.data.target,
            predicate: l.data.predicate,
        },
        timestamp: l.timestamp,
        proof: ExpressionProof {
            signature: l.proof.signature,
            key: l.proof.key,
        },
    }
}

fn diff_to_algo(d: PerspectiveDiff) -> perspective_diff_algorithm::PerspectiveDiff {
    perspective_diff_algorithm::PerspectiveDiff {
        additions: d.additions.into_iter().map(link_to_algo).collect(),
        removals: d.removals.into_iter().map(link_to_algo).collect(),
    }
}

fn diff_from_algo(d: perspective_diff_algorithm::PerspectiveDiff) -> PerspectiveDiff {
    PerspectiveDiff {
        additions: d.additions.into_iter().map(link_from_algo).collect(),
        removals: d.removals.into_iter().map(link_from_algo).collect(),
    }
}

// ---- HDK adapter -------------------------------------------------------

/// Holochain-flavored wrapper around the algorithm crate's `ChunkedDiffs`.
/// The internal field is an `AlgoChunkedDiffs` whose chunks are the
/// algorithm mirror `PerspectiveDiff` — conversions happen on the IO
/// boundary (`into_entries` / `from_entries`).
#[derive(Clone)]
pub struct ChunkedDiffs {
    inner: AlgoChunkedDiffs,
}

impl ChunkedDiffs {
    pub fn new(max: u16) -> Self {
        Self {
            inner: AlgoChunkedDiffs::new(max),
        }
    }

    /// View the underlying chunks as integrity-zome `PerspectiveDiff`
    /// values. Used by the tests and a couple of `format!("{:?}")`
    /// debug assertions in pull/commit.
    pub fn chunks(&self) -> Vec<PerspectiveDiff> {
        self.inner
            .chunks
            .iter()
            .cloned()
            .map(diff_from_algo)
            .collect()
    }

    pub fn add_additions(&mut self, links: Vec<LinkExpression>) {
        self.inner
            .add_additions(links.into_iter().map(link_to_algo).collect())
    }

    pub fn add_removals(&mut self, links: Vec<LinkExpression>) {
        self.inner
            .add_removals(links.into_iter().map(link_to_algo).collect())
    }

    /// Write each chunk to the DHT as a `PerspectiveDiffEntryReference`
    /// with no parents, returning the action hashes.
    pub fn into_entries<Retreiver: PerspectiveDiffRetreiver>(
        self,
    ) -> SocialContextResult<Vec<Hash>> {
        debug!("ChunkedDiffs.into_entries()");
        self.inner
            .chunks
            .into_iter()
            .map(|algo_chunk| {
                let chunk_diff = diff_from_algo(algo_chunk);
                debug!(
                    "ChunkedDiffs writing chunk of size: {}",
                    chunk_diff.total_diff_number()
                );
                let diff_entry = PerspectiveDiffEntryReference::new(
                    chunk_diff, None, // No parents for chunk entries
                );
                Retreiver::create_entry(EntryTypes::PerspectiveDiffEntryReference(diff_entry))
            })
            .collect()
    }

    /// Recover chunks from the DHT by their action hashes.
    pub fn from_entries<Retreiver: PerspectiveDiffRetreiver>(
        hashes: Vec<Hash>,
    ) -> SocialContextResult<Self> {
        debug!(
            "ChunkedDiffs::from_entries: START - Loading {} chunk(s) from DHT",
            hashes.len()
        );

        let mut diffs = Vec::new();
        for (idx, hash) in hashes.iter().enumerate() {
            debug!(
                "ChunkedDiffs::from_entries: Loading chunk {}/{} (hash: {:?})",
                idx + 1,
                hashes.len(),
                hash
            );

            // NO RETRY LOOP - fail fast if chunks aren't available
            // Validation dependencies ensure chunks arrive before parent entry validates
            // If this fails, the caller will retry the entire operation later
            let diff_entry = match Retreiver::get(hash.clone()) {
                Ok(entry) => {
                    debug!(
                        "ChunkedDiffs::from_entries: ✓ Chunk {}/{} retrieved successfully",
                        idx + 1,
                        hashes.len()
                    );
                    entry
                }
                Err(e) => {
                    warn!(
                        "ChunkedDiffs::from_entries: ✗ FAILED to retrieve chunk {}/{} (hash: {:?}) - Error: {:?}",
                        idx + 1, hashes.len(), hash, e
                    );
                    warn!(
                        "ChunkedDiffs::from_entries: Chunks not available - operation will be retried by caller"
                    );
                    return Err(e);
                }
            };

            // Use load_diff_from_entry to handle both inline and chunked entries properly
            // This prevents loading empty diffs if a chunk hash accidentally points to a chunked entry
            debug!(
                "ChunkedDiffs::from_entries: Processing chunk {}/{} - is_chunked: {}, has inline diff: {}",
                idx + 1, hashes.len(), diff_entry.is_chunked(), diff_entry.diff.total_diff_number() > 0
            );
            let diff = load_diff_from_entry::<Retreiver>(&diff_entry)?;
            debug!(
                "ChunkedDiffs::from_entries: Chunk {}/{} processed - additions: {}, removals: {}",
                idx + 1,
                hashes.len(),
                diff.additions.len(),
                diff.removals.len()
            );
            diffs.push(diff_to_algo(diff));
        }

        debug!(
            "ChunkedDiffs::from_entries: COMPLETE - Successfully loaded all {} chunk(s)",
            hashes.len()
        );

        Ok(ChunkedDiffs {
            inner: AlgoChunkedDiffs::from_chunks(*CHUNK_SIZE, diffs),
        })
    }

    pub fn into_aggregated_diff(self) -> PerspectiveDiff {
        diff_from_algo(self.inner.into_aggregated_diff())
    }
}

/// Load the diff from a PerspectiveDiffEntryReference, handling both inline and chunked storage.
/// If the entry has diff_chunks, loads and aggregates them. Otherwise, returns the inline diff.
pub fn load_diff_from_entry<Retriever: PerspectiveDiffRetreiver>(
    entry: &PerspectiveDiffEntryReference,
) -> SocialContextResult<PerspectiveDiff> {
    if entry.is_chunked() {
        // Load chunks and aggregate them
        let chunk_hashes = entry.diff_chunks.as_ref().unwrap();
        debug!(
            "load_diff_from_entry: Entry is CHUNKED - loading {} chunk(s) from DHT",
            chunk_hashes.len()
        );
        let chunked_diffs = ChunkedDiffs::from_entries::<Retriever>(chunk_hashes.clone())?;
        let aggregated = chunked_diffs.into_aggregated_diff();
        debug!(
            "load_diff_from_entry: Successfully aggregated {} chunk(s) - total additions: {}, removals: {}",
            chunk_hashes.len(), aggregated.additions.len(), aggregated.removals.len()
        );
        Ok(aggregated)
    } else {
        // Return inline diff
        debug!(
            "load_diff_from_entry: Entry is INLINE - additions: {}, removals: {}",
            entry.diff.additions.len(),
            entry.diff.removals.len()
        );
        Ok(entry.diff.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::ChunkedDiffs;
    use crate::retriever::{MockPerspectiveGraph, GLOBAL_MOCKED_GRAPH};
    use crate::utils::create_link_expression;

    // NOTE: the pure splitter/aggregator unit tests (can_chunk,
    // can_aggregate, can_chunk_big_diffs) moved to the algorithm crate
    // alongside the `ChunkedDiffs` struct itself. The remaining tests
    // exercise the HDK IO + integrity-conversion boundary, which still
    // lives here.

    #[test]
    fn can_write_and_read_entries() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot("digraph{}")
                .expect("can create mock graph from empty dot");
        }
        update();

        let mut chunks = ChunkedDiffs::new(500);

        let mut big_diff_add = Vec::new();
        for i in 0..5000 {
            big_diff_add.push(create_link_expression("a", &format!("{}", i)));
        }
        chunks.add_additions(big_diff_add);

        assert_eq!(chunks.chunks().len(), 10);

        let chunks_clone = chunks.clone();
        let hashes = chunks
            .into_entries::<MockPerspectiveGraph>()
            .expect("into_entries does not error");
        let read_chunks = ChunkedDiffs::from_entries::<MockPerspectiveGraph>(hashes)
            .expect("from_entries does not error");

        assert_eq!(read_chunks.chunks().len(), 10);
        assert_eq!(
            format!("{:?}", read_chunks.chunks()),
            format!("{:?}", chunks_clone.chunks())
        );
    }

    /// Test that demonstrates the bug fix: from_entries can handle chunk hashes that point to chunked entries.
    /// This simulates the scenario where snapshot.diff_chunks accidentally contains hashes of chunked entries
    /// instead of regular chunk entries. Before the fix, this would return empty diffs and cause memcmp errors.
    /// After the fix, it properly recursively loads the nested chunks.
    #[test]
    fn test_nested_chunked_entries_are_handled() {
        use crate::retriever::PerspectiveDiffRetreiver;
        use perspective_diff_sync_integrity::{
            EntryTypes, PerspectiveDiff, PerspectiveDiffEntryReference,
        };

        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot("digraph{}")
                .expect("can create mock graph from empty dot");
        }
        update();

        // Create a large diff that will be chunked
        let mut chunks = ChunkedDiffs::new(50);
        let mut big_diff = Vec::new();
        for i in 0..150 {
            big_diff.push(create_link_expression("nested", &format!("item_{}", i)));
        }
        chunks.add_additions(big_diff.clone());

        // This creates 3 chunk entries (50 items each)
        assert_eq!(chunks.chunks().len(), 3);

        // Store the chunk entries and get their hashes
        let chunk_hashes = chunks
            .into_entries::<MockPerspectiveGraph>()
            .expect("into_entries should work");

        // Now create a chunked entry that references these chunks (simulating nested chunking)
        let chunked_entry = PerspectiveDiffEntryReference {
            diff: PerspectiveDiff::new(), // Empty inline diff
            parents: None,
            diffs_since_snapshot: 0,
            diff_chunks: Some(chunk_hashes.clone()),
        };

        // Store this chunked entry
        let chunked_entry_hash = MockPerspectiveGraph::create_entry(
            EntryTypes::PerspectiveDiffEntryReference(chunked_entry),
        )
        .expect("create_entry should work");

        // Create a "broken" snapshot that points to the chunked entry instead of direct chunks
        // This simulates the bug scenario
        let broken_chunk_refs = vec![chunked_entry_hash];

        // Before the fix, this would return empty chunks because it would load the chunked entry's
        // empty inline diff. After the fix, it should recursively load the nested chunks.
        let loaded_chunks = ChunkedDiffs::from_entries::<MockPerspectiveGraph>(broken_chunk_refs)
            .expect("from_entries should handle nested chunks");

        let aggregated = loaded_chunks.into_aggregated_diff();

        // Verify we got all 150 items back, not 0 (which would happen with the bug)
        assert_eq!(
            aggregated.additions.len(),
            150,
            "Should load all nested chunks, not return empty diff"
        );
        assert_eq!(
            format!("{:?}", aggregated.additions),
            format!("{:?}", big_diff),
            "Should have the same data after loading nested chunks"
        );
    }

    /// Test that from_entries can handle a mix of inline and chunked entries
    #[test]
    fn test_from_entries_with_mixed_chunked_and_inline() {
        use crate::retriever::PerspectiveDiffRetreiver;
        use perspective_diff_sync_integrity::{
            EntryTypes, PerspectiveDiff, PerspectiveDiffEntryReference,
        };

        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot("digraph{}")
                .expect("can create mock graph from empty dot");
        }
        update();

        // Create an inline entry (small diff, no chunks)
        let inline_diff = PerspectiveDiff {
            additions: vec![
                create_link_expression("inline", "1"),
                create_link_expression("inline", "2"),
            ],
            removals: vec![],
        };
        let inline_entry = PerspectiveDiffEntryReference::new(inline_diff.clone(), None);
        let inline_hash = MockPerspectiveGraph::create_entry(
            EntryTypes::PerspectiveDiffEntryReference(inline_entry),
        )
        .expect("create inline entry");

        // Create a chunked entry (large diff split into chunks)
        let mut chunks = ChunkedDiffs::new(50);
        let mut big_diff = Vec::new();
        for i in 0..100 {
            big_diff.push(create_link_expression("chunked", &format!("item_{}", i)));
        }
        chunks.add_additions(big_diff.clone());
        let chunk_hashes = chunks
            .into_entries::<MockPerspectiveGraph>()
            .expect("into_entries should work");

        let chunked_entry = PerspectiveDiffEntryReference {
            diff: PerspectiveDiff::new(),
            parents: None,
            diffs_since_snapshot: 0,
            diff_chunks: Some(chunk_hashes),
        };
        let chunked_hash = MockPerspectiveGraph::create_entry(
            EntryTypes::PerspectiveDiffEntryReference(chunked_entry),
        )
        .expect("create chunked entry");

        // Load both entries - one inline, one chunked
        let mixed_hashes = vec![inline_hash, chunked_hash];
        let loaded_chunks = ChunkedDiffs::from_entries::<MockPerspectiveGraph>(mixed_hashes)
            .expect("from_entries should handle mixed entries");

        let aggregated = loaded_chunks.into_aggregated_diff();

        // Should have data from both inline (2 items) and chunked (100 items) = 102 total
        assert_eq!(
            aggregated.additions.len(),
            102,
            "Should aggregate both inline and chunked entries"
        );

        // Verify inline data is present
        assert!(aggregated
            .additions
            .contains(&create_link_expression("inline", "1")));
        assert!(aggregated
            .additions
            .contains(&create_link_expression("inline", "2")));

        // Verify chunked data is present
        assert!(aggregated
            .additions
            .contains(&create_link_expression("chunked", "item_0")));
        assert!(aggregated
            .additions
            .contains(&create_link_expression("chunked", "item_99")));
    }

    /// Test that demonstrates the bug scenario: what happens when chunk hashes accidentally
    /// point to empty chunked entries (the original bug). This test documents the expected
    /// behavior - with the fix, it should return empty diffs gracefully rather than causing
    /// memcmp errors.
    #[test]
    fn test_loading_empty_chunked_entry_returns_empty_diff() {
        use crate::retriever::PerspectiveDiffRetreiver;
        use perspective_diff_sync_integrity::{
            EntryTypes, PerspectiveDiff, PerspectiveDiffEntryReference,
        };

        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot("digraph{}")
                .expect("can create mock graph from empty dot");
        }
        update();

        // Create an empty chunked entry (no diff_chunks)
        let empty_chunked_entry = PerspectiveDiffEntryReference {
            diff: PerspectiveDiff::new(), // Empty
            parents: None,
            diffs_since_snapshot: 0,
            diff_chunks: None, // No chunks
        };
        let empty_hash = MockPerspectiveGraph::create_entry(
            EntryTypes::PerspectiveDiffEntryReference(empty_chunked_entry),
        )
        .expect("create empty entry");

        // Loading this should return empty diffs without errors
        let loaded = ChunkedDiffs::from_entries::<MockPerspectiveGraph>(vec![empty_hash])
            .expect("from_entries should handle empty entries");

        let aggregated = loaded.into_aggregated_diff();

        // Should be empty but not crash
        assert_eq!(aggregated.additions.len(), 0);
        assert_eq!(aggregated.removals.len(), 0);
    }
}
