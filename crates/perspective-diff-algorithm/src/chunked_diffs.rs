//! Chunked perspective-diff splitter / aggregator.
//!
//! Originally lived in
//! `bootstrap-languages/p-diff-sync/hc-dna/zomes/perspective_diff_sync/src/link_adapter/chunked_diffs.rs`,
//! parameterized concretely on the integrity-zome `PerspectiveDiff` /
//! `LinkExpression` types and the HDK `Retreiver` trait.
//!
//! Step 13a (the wide extraction Nico asked for in the wake-13 audio
//! note): the pure splitter/aggregator logic moves here, parameterized
//! on the algorithm crate's own [`PerspectiveDiff`] mirror type. The
//! HDK IO side (`into_entries` / `from_entries` / `load_diff_from_entry`)
//! stays in p-diff-sync as a thin wrapper that converts between
//! `integrity::PerspectiveDiff` and `algorithm::PerspectiveDiff` at the
//! boundary.

use crate::diff_types::{LinkExpression, PerspectiveDiff};

/// Splits an unbounded list of additions/removals into bounded chunks
/// of at most `max_changes_per_chunk` items each.
///
/// Independent of any storage backend — see p-diff-sync's
/// `link_adapter::chunked_diffs` wrapper for the HDK IO that turns
/// these chunks into DHT entries.
#[derive(Clone, Debug)]
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

    pub fn max(&self) -> u16 {
        self.max_changes_per_chunk
    }

    /// Construct from a pre-populated vector of chunk-diffs (the path
    /// `from_entries` uses after reading the chunks from storage).
    pub fn from_chunks(max: u16, chunks: Vec<PerspectiveDiff>) -> Self {
        Self {
            max_changes_per_chunk: max,
            chunks,
        }
    }

    pub fn add_additions(&mut self, links: Vec<LinkExpression>) {
        let mut reverse_links = links.into_iter().rev().collect::<Vec<_>>();
        while !reverse_links.is_empty() {
            let len = self.chunks.len();
            let current_chunk = self
                .chunks
                .get_mut(len - 1)
                .expect("must have at least one");

            while current_chunk.total_diff_number() < self.max_changes_per_chunk.into()
                && !reverse_links.is_empty()
            {
                current_chunk.additions.push(reverse_links.pop().unwrap());
            }

            if !reverse_links.is_empty() {
                self.chunks.push(PerspectiveDiff::new())
            }
        }
    }

    pub fn add_removals(&mut self, links: Vec<LinkExpression>) {
        let mut reverse_links = links.into_iter().rev().collect::<Vec<_>>();
        while !reverse_links.is_empty() {
            let len = self.chunks.len();
            let current_chunk = self
                .chunks
                .get_mut(len - 1)
                .expect("must have at least one");

            while current_chunk.total_diff_number() < self.max_changes_per_chunk.into()
                && !reverse_links.is_empty()
            {
                current_chunk.removals.push(reverse_links.pop().unwrap());
            }

            if !reverse_links.is_empty() {
                self.chunks.push(PerspectiveDiff::new())
            }
        }
    }

    /// Flatten all chunks into a single `PerspectiveDiff`. Used by
    /// `load_diff_from_entry` on the HDK side after re-assembling
    /// chunks from storage.
    pub fn into_aggregated_diff(self) -> PerspectiveDiff {
        self.chunks
            .into_iter()
            .reduce(|mut accum, mut item| {
                accum.additions.append(&mut item.additions);
                accum.removals.append(&mut item.removals);
                accum
            })
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn link(source: &str, target: &str) -> LinkExpression {
        use crate::diff_types::{ExpressionProof, Triple};
        LinkExpression {
            author: "test".into(),
            data: Triple {
                source: Some(source.into()),
                target: Some(target.into()),
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
    fn can_chunk() {
        let mut chunks = ChunkedDiffs::new(5);
        chunks.add_additions(vec![link("a", "1"), link("a", "2"), link("a", "3")]);
        assert_eq!(chunks.chunks.len(), 1);

        chunks.add_additions(vec![link("a", "4"), link("a", "5"), link("a", "6")]);
        assert_eq!(chunks.chunks.len(), 2);

        chunks.add_removals(vec![
            link("a", "1"),
            link("a", "2"),
            link("a", "3"),
            link("a", "4"),
            link("a", "5"),
            link("a", "6"),
        ]);
        assert_eq!(chunks.chunks.len(), 3);
    }

    #[test]
    fn can_aggregate() {
        let mut chunks = ChunkedDiffs::new(5);
        let a1 = link("a", "1");
        let a2 = link("a", "2");
        let r1 = link("r", "1");
        let r2 = link("r", "2");
        let r3 = link("r", "3");
        let r4 = link("r", "4");

        chunks.add_additions(vec![a1.clone()]);
        chunks.add_additions(vec![a2.clone()]);
        chunks.add_removals(vec![r1.clone(), r2.clone(), r3.clone(), r4.clone()]);
        assert_eq!(chunks.chunks.len(), 2);

        let diff = chunks.into_aggregated_diff();
        assert_eq!(diff.additions, vec![a1, a2]);
        assert_eq!(diff.removals, vec![r1, r2, r3, r4]);
    }

    #[test]
    fn can_chunk_big_diffs() {
        let mut chunks = ChunkedDiffs::new(500);
        let big_diff_add: Vec<LinkExpression> =
            (0..5000).map(|i| link("a", &i.to_string())).collect();
        chunks.add_additions(big_diff_add);

        let big_diff_remove: Vec<LinkExpression> =
            (0..800).map(|i| link("a", &i.to_string())).collect();
        chunks.add_removals(big_diff_remove);

        let big_diff_add: Vec<LinkExpression> =
            (0..213).map(|i| link("a", &i.to_string())).collect();
        chunks.add_additions(big_diff_add);

        assert_eq!(chunks.chunks.len(), 13);
        for i in 0..12 {
            assert_eq!(chunks.chunks[i].total_diff_number(), 500);
        }
        assert_eq!(chunks.chunks[12].total_diff_number(), 13);
    }
}
