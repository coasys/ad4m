use crate::errors::{SocialContextError, SocialContextResult};
use hdk::prelude::*;
use perspective_diff_sync_integrity::PerspectiveDiffEntryReference;
use std::collections::BTreeSet;

// Applies Kahn's algorithm for topologically sorting a graph
pub fn topo_sort_diff_references(
    arr: &Vec<(
        HoloHash<holo_hash::hash_type::Action>,
        PerspectiveDiffEntryReference,
    )>,
) -> SocialContextResult<
    Vec<(
        HoloHash<holo_hash::hash_type::Action>,
        PerspectiveDiffEntryReference,
    )>,
> {
    type Hash = HoloHash<holo_hash::hash_type::Action>;
    let mut result = Vec::<(Hash, PerspectiveDiffEntryReference)>::new();

    // first collect orphaned nodes (=without parent) as starting points:
    let mut orphaned_nodes: Vec<(Hash, PerspectiveDiffEntryReference)> = arr
        .iter()
        .filter(|&e| e.1.parents == None)
        .cloned()
        .collect();

    if orphaned_nodes.len() == 0 {
        debug!("No orphans found! Length: {}, list: {:?}", arr.len(), arr);
        return Err(SocialContextError::InternalError(
            "Can't topologically sort list without orphan!",
        ));
    }

    let mut edges = BTreeSet::new();
    for i in 0..arr.len() {
        if let Some(parents) = &arr[i].1.parents {
            for p in 0..parents.len() {
                let child = arr[i].0.clone();
                let parent = parents[p].clone();
                edges.insert((child, parent));
            }
        }
    }

    // Starting from the nodes without parents...
    while let Some(n) = orphaned_nodes.pop() {
        //.. we put them into the result list.
        result.push(n.clone());

        println!("Added orphan {:?}", n);

        // and then we look for any nodes that have it as parent
        // (using the edges set)
        let edges_with_n_as_parent = edges
            .iter()
            .filter(|&e| e.1 == n.0)
            .cloned()
            .collect::<Vec<(Hash, Hash)>>();

        println!("Edges with orphan as parent {:?}", edges_with_n_as_parent);

        // So for every parent relationship with n as parent...
        for edge in &edges_with_n_as_parent {
            println!("Removing edge {:?}", edge);
            // we remove that edge
            edges.remove(edge);

            // and then check if that child of n has any other parents...
            let child = edge.0.clone();

            println!("Found child {:?}", child);
            let edges_with_child_as_child = edges
                .iter()
                .filter(|&e| e.0 == child)
                .cloned()
                .collect::<Vec<(Hash, Hash)>>();

            println!("Edges with child as child {:?}", edges_with_child_as_child);

            // if the child does not have any other parents (left unprocessed)
            if edges_with_child_as_child.len() == 0 {
                // we're good to add the child to the results as well.
                let child_item = arr.iter().find(|&e| e.0 == child).ok_or(SocialContextError::InternalError("Topological sort couldn't find child in input vector, which was mentioned in an edge. This can only be an error in the topological sorting code.."))?;
                println!("Adding newly orphaned child {:?}", child_item);
                orphaned_nodes.push((child.clone(), child_item.1.clone()));
            }
        }
    }

    if edges.len() > 0 {
        debug!(
            "Unresolved parent links after topologically sorting: {:?}",
            edges
        );

        debug!("Number of unresolved parent links {:?}", edges.len());
        debug!("Number of items to sort: {:?}", arr.len());
        Err(SocialContextError::InternalError(
            "Cycle or missing nodes detected. Unresolved parent links after topologically sorting.",
        ))
        //Ok(result)
    } else {
        Ok(result)
    }
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
