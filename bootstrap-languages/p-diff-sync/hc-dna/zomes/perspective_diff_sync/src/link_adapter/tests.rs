// HDK-boundary tests that drive the algorithm-crate Workspace via the
// MockPerspectiveGraph. With the shared `perspective-diff-types` crate,
// hashes flow as `ActionHash` (= `algo::Hash`) directly — no conversion.
//
// The pure BFS coverage lives in
// `perspective_diff_algorithm::workspace::tests`.

#[test]
pub fn test_merge_fast_forward() {
    use hdk::prelude::*;
    use perspective_diff_algorithm as algo;

    use crate::link_adapter::workspace::Workspace;
    use crate::retriever::{Associations, GraphInput, MockPerspectiveGraph, GLOBAL_MOCKED_GRAPH};

    fn update() {
        let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
        *graph = MockPerspectiveGraph::new(GraphInput {
            nodes: 6,
            associations: vec![
                Associations {
                    node_source: 1,
                    node_targets: vec![0],
                },
                Associations {
                    node_source: 2,
                    node_targets: vec![0],
                },
                Associations {
                    node_source: 3,
                    node_targets: vec![1],
                },
                Associations {
                    node_source: 4,
                    node_targets: vec![2],
                },
                Associations {
                    node_source: 5,
                    node_targets: vec![3, 4],
                },
            ],
        });
    }
    update();

    let mut workspace = Workspace::new();
    let res = workspace.collect_until_common_ancestor::<MockPerspectiveGraph>(
        ActionHash::from_raw_36(vec![5; 36]),
        ActionHash::from_raw_36(vec![4; 36]),
    );
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), ActionHash::from_raw_36(vec![0; 36]));
    let _ = algo::null_node;
}

#[test]
pub fn test_fork_with_none_source() {
    use hdk::prelude::*;
    use perspective_diff_algorithm as algo;

    use crate::link_adapter::workspace::Workspace;
    use crate::retriever::{GraphInput, MockPerspectiveGraph, GLOBAL_MOCKED_GRAPH};

    fn update() {
        let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
        *graph = MockPerspectiveGraph::new(GraphInput {
            nodes: 2,
            associations: vec![],
        });
    }
    update();

    let mut workspace = Workspace::new();
    let res = workspace.collect_until_common_ancestor::<MockPerspectiveGraph>(
        ActionHash::from_raw_36(vec![0; 36]),
        ActionHash::from_raw_36(vec![1; 36]),
    );
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), algo::null_node());
}

#[test]
pub fn test_merge_fast_forward_none_source() {
    use hdk::prelude::*;
    use perspective_diff_algorithm as algo;

    use crate::link_adapter::workspace::Workspace;
    use crate::retriever::{Associations, GraphInput, MockPerspectiveGraph, GLOBAL_MOCKED_GRAPH};

    fn update() {
        let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
        *graph = MockPerspectiveGraph::new(GraphInput {
            nodes: 3,
            associations: vec![Associations {
                node_source: 2,
                node_targets: vec![0, 1],
            }],
        });
    }
    update();

    let mut workspace = Workspace::new();
    let res = workspace.collect_until_common_ancestor::<MockPerspectiveGraph>(
        ActionHash::from_raw_36(vec![2; 36]),
        ActionHash::from_raw_36(vec![1; 36]),
    );
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), algo::null_node());
}
