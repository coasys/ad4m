//! Kahn-style topological sort over a DAG of perspective-diff entries.
//!
//! Originally lived in
//! `bootstrap-languages/p-diff-sync/hc-dna/zomes/perspective_diff_sync/src/link_adapter/topo_sort.rs`,
//! parameterized concretely on `HoloHash<holo_hash::hash_type::Action>` and
//! `PerspectiveDiffEntryReference`.
//!
//! Now generic over `O: OpId` and any node `V` that implements
//! [`HasDiffParents`]. p-diff-sync re-exports this with
//! `O = HoloHash<Action>` and `V = PerspectiveDiffEntryReference` — the
//! reference impl of `HasDiffParents` is provided there since the integrity
//! data types still live in `perspective_diff_sync_integrity`.

use std::collections::BTreeSet;

use thiserror::Error;

use crate::{HasDiffParents, OpId};

#[derive(Debug, Error)]
pub enum TopoSortError {
    #[error("Can't topologically sort list without orphan!")]
    NoOrphan,
    #[error("Topological sort couldn't find child {0} in input vector, which was mentioned in an edge. This can only be an error in the topological sorting code.")]
    MissingChild(String),
    #[error(
        "Cycle or missing nodes detected. Unresolved parent links after topologically sorting."
    )]
    UnresolvedEdges,
}

/// Apply Kahn's algorithm to topologically sort an array of
/// `(op_id, node)` pairs by parent relationships.
///
/// Nodes with no parents are the roots. Nodes are emitted in an order
/// such that every parent precedes its children in the output.
///
/// Returns `Err(TopoSortError::NoOrphan)` if no root node exists,
/// `Err(TopoSortError::UnresolvedEdges)` if a cycle is detected or a
/// declared parent is missing from `arr`.
pub fn topo_sort_diff_references<O, V>(arr: &[(O, V)]) -> Result<Vec<(O, V)>, TopoSortError>
where
    O: OpId,
    V: Clone + HasDiffParents<O>,
{
    let mut result = Vec::<(O, V)>::new();

    // First collect orphaned nodes (= without parents) as starting points:
    let mut orphaned_nodes: Vec<(O, V)> = arr
        .iter()
        .filter(|&e| e.1.parents().is_none())
        .cloned()
        .collect();

    if orphaned_nodes.is_empty() {
        return Err(TopoSortError::NoOrphan);
    }

    let mut edges: BTreeSet<(O, O)> = BTreeSet::new();
    for (child_id, node) in arr.iter() {
        if let Some(parents) = node.parents() {
            for parent in parents.iter() {
                edges.insert((child_id.clone(), parent.clone()));
            }
        }
    }

    // Starting from the nodes without parents...
    while let Some(n) = orphaned_nodes.pop() {
        result.push(n.clone());

        // Find every (child, n) edge — children of n.
        let edges_with_n_as_parent: Vec<(O, O)> =
            edges.iter().filter(|&e| e.1 == n.0).cloned().collect();

        for edge in &edges_with_n_as_parent {
            // Drop the edge.
            edges.remove(edge);

            let child = edge.0.clone();

            // If the child has no other unprocessed parents, it's now an orphan too.
            let still_has_parents = edges.iter().any(|e| e.0 == child);

            if !still_has_parents {
                let child_item = arr
                    .iter()
                    .find(|&e| e.0 == child)
                    .ok_or_else(|| TopoSortError::MissingChild(format!("{:?}", child)))?;
                orphaned_nodes.push((child.clone(), child_item.1.clone()));
            }
        }
    }

    if !edges.is_empty() {
        Err(TopoSortError::UnresolvedEdges)
    } else {
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // A minimal stand-in node type for testing the algorithm in isolation
    // (i.e., without needing the integrity zome's PerspectiveDiffEntryReference).
    #[derive(Debug, Clone, PartialEq, Eq)]
    struct TestNode {
        parents: Option<Vec<u32>>,
    }

    impl HasDiffParents<u32> for TestNode {
        fn parents(&self) -> Option<&[u32]> {
            self.parents.as_deref()
        }
    }

    #[test]
    fn sorts_linear_chain() {
        let arr = vec![
            (
                3u32,
                TestNode {
                    parents: Some(vec![2]),
                },
            ),
            (
                2u32,
                TestNode {
                    parents: Some(vec![1]),
                },
            ),
            (1u32, TestNode { parents: None }),
        ];
        let out = topo_sort_diff_references(&arr).expect("sort");
        assert_eq!(out.len(), 3);
        assert_eq!(out[0].0, 1, "root should be first");
    }

    #[test]
    fn diamond_graph_with_two_paths() {
        // 4 ┐
        //   ├─ 2 ─ 1
        //   └─ 3 ─ 1
        // 1 has no parents (root)
        let arr = vec![
            (
                4u32,
                TestNode {
                    parents: Some(vec![2, 3]),
                },
            ),
            (
                3u32,
                TestNode {
                    parents: Some(vec![1]),
                },
            ),
            (
                2u32,
                TestNode {
                    parents: Some(vec![1]),
                },
            ),
            (1u32, TestNode { parents: None }),
        ];
        let out = topo_sort_diff_references(&arr).expect("sort");
        assert_eq!(out.len(), 4);
        // 1 must come before 2 and 3, which must come before 4
        let pos = |id: u32| out.iter().position(|(o, _)| *o == id).unwrap();
        assert!(pos(1) < pos(2));
        assert!(pos(1) < pos(3));
        assert!(pos(2) < pos(4));
        assert!(pos(3) < pos(4));
    }

    #[test]
    fn rejects_graph_with_no_root() {
        let arr = vec![
            (
                1u32,
                TestNode {
                    parents: Some(vec![2]),
                },
            ),
            (
                2u32,
                TestNode {
                    parents: Some(vec![1]),
                },
            ),
        ];
        let err = topo_sort_diff_references(&arr).expect_err("should error");
        assert!(matches!(err, TopoSortError::NoOrphan));
    }

    #[test]
    fn rejects_missing_parent() {
        // 2 declares parent 1, but only 2 is in the input.
        let arr = vec![(
            2u32,
            TestNode {
                parents: Some(vec![1]),
            },
        )];
        let err = topo_sort_diff_references(&arr).expect_err("should error");
        assert!(matches!(err, TopoSortError::NoOrphan));
    }
}
