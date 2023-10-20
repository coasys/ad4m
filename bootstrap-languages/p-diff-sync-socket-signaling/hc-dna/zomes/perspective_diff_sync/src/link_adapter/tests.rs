#[test]
pub fn test_merge_fast_forward() {
    use hdk::prelude::*;

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
}

#[test]
pub fn test_fork_with_none_source() {
    use hdk::prelude::*;

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
    //TODO; this is a problem since our pull code is not expecting to find a common ancestor, since both tips are forks
    //but in the case below where we have a merge entry we need to register the None node as a common ancestor so we can traverse the "their" branch back until the root
    //and not break the traversal with common ancestor as the "ours" node as was happening before
    //
    //So what do we actually need to return here?
    assert_eq!(res.unwrap(), ActionHash::from_raw_36(vec![0xdb; 36]));
}

#[test]
pub fn test_merge_fast_forward_none_source() {
    use hdk::prelude::*;

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
    assert_eq!(res.unwrap(), ActionHash::from_raw_36(vec![0xdb; 36]));
}
