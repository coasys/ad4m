use hdk::prelude::*;
use perspective_diff_sync_integrity::{
    EntryTypes, LinkExpression, PerspectiveDiff, PerspectiveDiffEntryReference,
};

use crate::errors::SocialContextResult;
use crate::retriever::PerspectiveDiffRetreiver;
use crate::{Hash, CHUNK_SIZE};

#[derive(Clone)]
pub struct ChunkedDiffs {
    max_changes_per_chunk: u16,
    pub chunks: Vec<PerspectiveDiff>,
}

impl ChunkedDiffs {
    pub fn new(max: u16) -> Self {
        Self {
            max_changes_per_chunk: max,
            chunks: vec![PerspectiveDiff::new()],
        }
    }

    pub fn add_additions(&mut self, links: Vec<LinkExpression>) {
        let mut reverse_links = links.into_iter().rev().collect::<Vec<_>>();
        while reverse_links.len() > 0 {
            let len = self.chunks.len();
            let current_chunk = self
                .chunks
                .get_mut(len - 1)
                .expect("must have at least one");

            while current_chunk.total_diff_number() < self.max_changes_per_chunk.into()
                && reverse_links.len() > 0
            {
                current_chunk.additions.push(reverse_links.pop().unwrap());
            }

            if reverse_links.len() > 0 {
                self.chunks.push(PerspectiveDiff::new())
            }
        }
    }

    pub fn add_removals(&mut self, links: Vec<LinkExpression>) {
        let mut reverse_links = links.into_iter().rev().collect::<Vec<_>>();
        while reverse_links.len() > 0 {
            let len = self.chunks.len();
            let current_chunk = self
                .chunks
                .get_mut(len - 1)
                .expect("must have at least one");

            while current_chunk.total_diff_number() < self.max_changes_per_chunk.into()
                && reverse_links.len() > 0
            {
                current_chunk.removals.push(reverse_links.pop().unwrap());
            }

            if reverse_links.len() > 0 {
                self.chunks.push(PerspectiveDiff::new())
            }
        }
    }

    pub fn into_entries<Retreiver: PerspectiveDiffRetreiver>(
        self,
    ) -> SocialContextResult<Vec<Hash>> {
        debug!("ChunkedDiffs.into_entries()");
        self.chunks
            .into_iter()
            .map(|chunk_diff| {
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

    pub fn from_entries<Retreiver: PerspectiveDiffRetreiver>(
        hashes: Vec<Hash>,
    ) -> SocialContextResult<Self> {
        let mut diffs = Vec::new();
        for hash in hashes.into_iter() {
            let diff_entry = Retreiver::get::<PerspectiveDiffEntryReference>(hash)?;
            // Use load_diff_from_entry to handle both inline and chunked entries properly
            // This prevents loading empty diffs if a chunk hash accidentally points to a chunked entry
            let diff = load_diff_from_entry::<Retreiver>(&diff_entry)?;
            diffs.push(diff);
        }

        Ok(ChunkedDiffs {
            max_changes_per_chunk: *CHUNK_SIZE,
            chunks: diffs,
        })
    }

    pub fn into_aggregated_diff(self) -> PerspectiveDiff {
        self.chunks
            .into_iter()
            .reduce(|mut accum, mut item| {
                // No need to clone - we own both accum and item from the iterator
                accum.additions.append(&mut item.additions);
                accum.removals.append(&mut item.removals);
                accum
            })
            .unwrap_or(PerspectiveDiff::new())
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
            "load_diff_from_entry: Loading {} chunks",
            chunk_hashes.len()
        );
        let chunked_diffs = ChunkedDiffs::from_entries::<Retriever>(chunk_hashes.clone())?;
        Ok(chunked_diffs.into_aggregated_diff())
    } else {
        // Return inline diff
        Ok(entry.diff.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::ChunkedDiffs;
    use crate::retriever::{MockPerspectiveGraph, GLOBAL_MOCKED_GRAPH};
    use crate::utils::create_link_expression;

    #[test]
    fn can_chunk() {
        let mut chunks = ChunkedDiffs::new(5);

        chunks.add_additions(vec![
            create_link_expression("a", "1"),
            create_link_expression("a", "2"),
            create_link_expression("a", "3"),
        ]);

        assert_eq!(chunks.chunks.len(), 1);

        chunks.add_additions(vec![
            create_link_expression("a", "4"),
            create_link_expression("a", "5"),
            create_link_expression("a", "6"),
        ]);

        assert_eq!(chunks.chunks.len(), 2);

        chunks.add_removals(vec![
            create_link_expression("a", "1"),
            create_link_expression("a", "2"),
            create_link_expression("a", "3"),
            create_link_expression("a", "4"),
            create_link_expression("a", "5"),
            create_link_expression("a", "6"),
        ]);

        assert_eq!(chunks.chunks.len(), 3);
    }

    #[test]
    fn can_aggregate() {
        let mut chunks = ChunkedDiffs::new(5);

        let _a1 = create_link_expression("a", "1");
        let _a2 = create_link_expression("a", "2");
        let _r1 = create_link_expression("r", "1");
        let _r2 = create_link_expression("r", "2");
        let _r3 = create_link_expression("r", "3");
        let _r4 = create_link_expression("r", "4");

        chunks.add_additions(vec![_a1.clone()]);
        chunks.add_additions(vec![_a2.clone()]);
        chunks.add_removals(vec![_r1.clone(), _r2.clone(), _r3.clone(), _r4.clone()]);

        assert_eq!(chunks.chunks.len(), 2);

        let diff = chunks.into_aggregated_diff();

        assert_eq!(diff.additions, vec![_a1, _a2]);
        assert_eq!(diff.removals, vec![_r1, _r2, _r3, _r4]);
    }

    #[test]
    fn can_chunk_big_diffs() {
        let mut chunks = ChunkedDiffs::new(500);

        let mut big_diff_add = Vec::new();
        for i in 0..5000 {
            big_diff_add.push(create_link_expression("a", &format!("{}", i)));
        }
        chunks.add_additions(big_diff_add);

        let mut big_diff_remove = Vec::new();
        for i in 0..800 {
            big_diff_remove.push(create_link_expression("a", &format!("{}", i)));
        }
        chunks.add_removals(big_diff_remove);

        let mut big_diff_add = Vec::new();
        for i in 0..213 {
            big_diff_add.push(create_link_expression("a", &format!("{}", i)));
        }
        chunks.add_additions(big_diff_add);

        assert_eq!(chunks.chunks.len(), 13);
        for i in 0..12 {
            assert_eq!(chunks.chunks[i].total_diff_number(), 500);
        }
        assert_eq!(chunks.chunks[12].total_diff_number(), 13);
    }

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

        assert_eq!(chunks.chunks.len(), 10);

        let chunks_clone = chunks.clone();
        let hashes = chunks
            .into_entries::<MockPerspectiveGraph>()
            .expect("into_entries does not error");
        let read_chunks = ChunkedDiffs::from_entries::<MockPerspectiveGraph>(hashes)
            .expect("from_entries does not error");

        assert_eq!(read_chunks.chunks.len(), 10);
        assert_eq!(
            format!("{:?}", read_chunks.chunks),
            format!("{:?}", chunks_clone.chunks)
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
        assert_eq!(chunks.chunks.len(), 3);

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
