//! Holochain-side adapter onto the substrate-agnostic topo-sort.
//!
//! The algorithm itself now lives in the `perspective-diff-algorithm` crate
//! (Step 1.5 of the holograph spike). This module concretizes it on
//! `HoloHash<Action>` + the integrity-zome `PerspectiveDiffEntryReference`,
//! and provides the `HasDiffParents` impl that bridges them.

use crate::errors::{SocialContextError, SocialContextResult};
use hdk::prelude::*;
use perspective_diff_algorithm::TopoSortError;
use perspective_diff_sync_integrity::PerspectiveDiffEntryReference;

type Hash = HoloHash<holo_hash::hash_type::Action>;

/// Backwards-compatible re-export of the topo-sort entry point used by
/// `link_adapter::workspace` and `link_adapter::pull`.
///
/// The `HasDiffParents<Hash>` impl on `PerspectiveDiffEntryReference` lives
/// in `perspective_diff_sync_integrity` (orphan rule), so we just call
/// straight into the algorithm crate here.
pub fn topo_sort_diff_references(
    arr: &Vec<(Hash, PerspectiveDiffEntryReference)>,
) -> SocialContextResult<Vec<(Hash, PerspectiveDiffEntryReference)>> {
    perspective_diff_algorithm::topo_sort_diff_references(arr).map_err(|e| match e {
        TopoSortError::NoOrphan => {
            debug!("No orphans found! Length: {}, list: {:?}", arr.len(), arr);
            SocialContextError::InternalError("Can't topologically sort list without orphan!")
        }
        TopoSortError::MissingChild(child) => {
            debug!("Topo-sort missing child: {}", child);
            SocialContextError::InternalError(
                "Topological sort couldn't find child in input vector, which was mentioned in an edge. This can only be an error in the topological sorting code..",
            )
        }
        TopoSortError::UnresolvedEdges => {
            debug!("Unresolved parent links after topologically sorting");
            SocialContextError::InternalError(
                "Cycle or missing nodes detected. Unresolved parent links after topologically sorting.",
            )
        }
    })
}

#[cfg(test)]
mod tests {
    use super::topo_sort_diff_references;
    use crate::errors::SocialContextResult;
    use hdk::prelude::*;
    use perspective_diff_sync_integrity::{PerspectiveDiff, PerspectiveDiffEntryReference};

    #[test]
    fn test_topo_sort() -> SocialContextResult<()> {
        let h1 = HoloHash::<holo_hash::hash_type::Action>::from_raw_36(vec![1; 36]);
        let h2 = HoloHash::<holo_hash::hash_type::Action>::from_raw_36(vec![2; 36]);
        let h3 = HoloHash::<holo_hash::hash_type::Action>::from_raw_36(vec![3; 36]);
        let h4 = HoloHash::<holo_hash::hash_type::Action>::from_raw_36(vec![4; 36]);

        let r1 = PerspectiveDiffEntryReference::new(
            PerspectiveDiff::new(),
            Some(vec![h2.clone(), h3.clone()]),
        );
        let r2 = PerspectiveDiffEntryReference::new(PerspectiveDiff::new(), Some(vec![h4.clone()]));
        let r3 = PerspectiveDiffEntryReference::new(PerspectiveDiff::new(), Some(vec![h4.clone()]));
        let r4 = PerspectiveDiffEntryReference::new(PerspectiveDiff::new(), None);

        let example_arr = vec![(h1, r1), (h2, r2), (h3, r3), (h4, r4)];

        let sorted = topo_sort_diff_references(&example_arr)?;
        assert_eq!(sorted.len(), 4);

        // Check that all diffs are empty (since we created them that way)
        for item in &sorted {
            assert!(item.1.diff.additions.is_empty() && item.1.diff.removals.is_empty());
        }

        // Find the item with no parents (should be first in topo order)
        let orphan_count = sorted
            .iter()
            .filter(|item| item.1.parents.is_none())
            .count();
        assert_eq!(orphan_count, 1, "Should have exactly one orphan node");

        // Find the item with parents
        let parent_count = sorted
            .iter()
            .filter(|item| item.1.parents.is_some())
            .count();
        assert_eq!(
            parent_count, 3,
            "Should have exactly three nodes with parents"
        );

        Ok(())
    }
}
