//! HDK-side shim onto the algorithm-crate `pull` and `handle_broadcast`.
//!
//! The implementations live in `perspective_diff_algorithm::pull`. This
//! module keeps the legacy import path
//! (`crate::link_adapter::pull::pull`) and the SocialContextResult
//! error mapping so existing zome callers don't have to change.

use perspective_diff_algorithm as algo;
use perspective_diff_sync_integrity::{HashBroadcast, PullResult};

use crate::errors::SocialContextResult;
use crate::retriever::PerspectiveDiffRetreiver;
use crate::Hash;

pub fn pull<
    Retriever: PerspectiveDiffRetreiver
        + algo::WorkspaceRetriever
        + algo::RevisionsRetriever
        + algo::SnapshotRetriever
        + algo::PullCommitEnv,
>(
    emit: bool,
    theirs: Hash,
    is_scribe: bool,
) -> SocialContextResult<PullResult> {
    Ok(algo::pull::<Retriever>(emit, theirs, is_scribe)?)
}

pub fn handle_broadcast<
    Retriever: PerspectiveDiffRetreiver
        + algo::WorkspaceRetriever
        + algo::RevisionsRetriever
        + algo::SnapshotRetriever
        + algo::PullCommitEnv,
>(
    broadcast: HashBroadcast,
) -> SocialContextResult<()> {
    Ok(algo::handle_broadcast::<Retriever>(broadcast)?)
}

#[cfg(test)]
mod tests {
    use super::pull;
    use crate::retriever::{
        create_node_id_link_expression, create_node_id_vec, node_id_hash, MockPerspectiveGraph,
        PerspectiveDiffRetreiver, GLOBAL_MOCKED_GRAPH,
    };
    use crate::utils::create_link_expression;
    use dot_structures;

    #[test]
    fn test_fast_forward_merge() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                r#"digraph {
                0 [ label = "0" ]
                1 [ label = "1" ]
                2 [ label = "2" ]
                3 [ label = "3" ]

                1 -> 0
                2 -> 0
                3 -> 1
                3 -> 2

            }"#,
            )
            .unwrap();
        }
        update();

        let latest_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("3")));

        let current_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("2")));
        let update_current =
            MockPerspectiveGraph::update_current_revision(current_node_hash, chrono::Utc::now());
        assert!(update_current.is_ok());

        let pull_res = pull::<MockPerspectiveGraph>(false, latest_node_hash, true);
        assert!(pull_res.is_ok());
        let pull_res = pull_res.unwrap();

        let node_1 = &node_id_hash(&dot_structures::Id::Plain(String::from("1"))).to_string();
        let node_3 = &node_id_hash(&dot_structures::Id::Plain(String::from("3"))).to_string();
        let expected_additions = vec![
            create_link_expression(node_1, node_1),
            create_link_expression(node_3, node_3),
        ];

        assert!(pull_res
            .diff
            .additions
            .iter()
            .all(|item| expected_additions.contains(item)));
    }

    #[test]
    fn test_complex_merge() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                r#"digraph {
                1 [ label = "1" ]
                2 [ label = "2" ]
                3 [ label = "3" ]
                4 [ label = "4" ]
                5 [ label = "5" ]
                6 [ label = "6" ]

                3 -> 2
                4 -> 2
                5 -> 3
                5 -> 4
                6 -> 5
            }"#,
            )
            .unwrap();
        }
        update();

        let latest_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("6")));

        let current_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("1")));
        let update_current =
            MockPerspectiveGraph::update_current_revision(current_node_hash, chrono::Utc::now());
        assert!(update_current.is_ok());

        let pull_res = pull::<MockPerspectiveGraph>(false, latest_node_hash.clone(), true);
        assert!(pull_res.is_ok());
        let pull_res = pull_res.unwrap();

        let node_2 = &node_id_hash(&dot_structures::Id::Plain(String::from("2"))).to_string();
        let node_3 = &node_id_hash(&dot_structures::Id::Plain(String::from("3"))).to_string();
        let node_4 = &node_id_hash(&dot_structures::Id::Plain(String::from("4"))).to_string();
        let node_5 = &node_id_hash(&dot_structures::Id::Plain(String::from("5"))).to_string();
        let node_6 = &node_id_hash(&dot_structures::Id::Plain(String::from("6"))).to_string();
        let expected_additions = vec![
            create_link_expression(node_2, node_2),
            create_link_expression(node_3, node_3),
            create_link_expression(node_4, node_4),
            create_link_expression(node_5, node_5),
            create_link_expression(node_6, node_6),
        ];

        assert!(pull_res
            .diff
            .additions
            .iter()
            .all(|item| expected_additions.contains(item)));

        let new_current = MockPerspectiveGraph::current_revision();
        assert!(new_current.is_ok());
        let new_current = new_current.unwrap();
        assert!(new_current.unwrap().hash != latest_node_hash);
    }

    #[test]
    fn test_complex_fast_forward() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                r#"digraph {
                1 [ label = "1" ]
                2 [ label = "2" ]
                3 [ label = "3" ]
                4 [ label = "4" ]
                5 [ label = "5" ]
                6 [ label = "6" ]

                3 -> 2
                4 -> 2
                5 -> 3
                5 -> 4
                6 -> 5
            }"#,
            )
            .unwrap();
        }
        update();

        let latest_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("6")));

        let current_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("4")));
        let update_current =
            MockPerspectiveGraph::update_current_revision(current_node_hash, chrono::Utc::now());
        assert!(update_current.is_ok());

        let pull_res = pull::<MockPerspectiveGraph>(false, latest_node_hash, true);
        assert!(pull_res.is_ok());
        let pull_res = pull_res.unwrap();

        let node_3 = &node_id_hash(&dot_structures::Id::Plain(String::from("3"))).to_string();
        let node_5 = &node_id_hash(&dot_structures::Id::Plain(String::from("5"))).to_string();
        let node_6 = &node_id_hash(&dot_structures::Id::Plain(String::from("6"))).to_string();
        let expected_additions = vec![
            create_link_expression(node_3, node_3),
            create_link_expression(node_5, node_5),
            create_link_expression(node_6, node_6),
        ];

        assert!(pull_res
            .diff
            .additions
            .iter()
            .all(|item| expected_additions.contains(item)));
    }

    #[test]
    fn test_fast_forward_after_merge() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                r#"digraph {
                1 [ label = "1" ]
                2 [ label = "2" ]
                3 [ label = "3" ]
                4 [ label = "4" ]
                5 [ label = "5" ]
                6 [ label = "6" ]
                7 [ label = "7" ]

                3 -> 2
                4 -> 2
                5 -> 3
                5 -> 4
                6 -> 5
                7 -> 1
                7 -> 6
            }"#,
            )
            .unwrap();
        }
        update();

        let latest_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("7")));

        let current_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("6")));
        let update_current =
            MockPerspectiveGraph::update_current_revision(current_node_hash, chrono::Utc::now());
        assert!(update_current.is_ok());

        let pull_res = pull::<MockPerspectiveGraph>(false, latest_node_hash, true);
        assert!(pull_res.is_ok());
        let pull_res = pull_res.unwrap();

        let node_1 = &node_id_hash(&dot_structures::Id::Plain(String::from("1"))).to_string();
        let node_7 = &node_id_hash(&dot_structures::Id::Plain(String::from("7"))).to_string();
        let expected_additions = vec![
            create_link_expression(node_1, node_1),
            create_link_expression(node_7, node_7),
        ];

        assert!(pull_res
            .diff
            .additions
            .iter()
            .all(|item| expected_additions.contains(item)));
    }

    #[test]
    fn test_pull_complex_merge_implicit_zero() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                r#"digraph {
                1 [ label = "1" ]
                2 [ label = "2" ]
                3 [ label = "3" ]
                4 [ label = "4" ]
                5 [ label = "5" ]
                6 [ label = "6" ]
                4 -> 2 [ label = "()" ]
                5 -> 4 [ label = "()" ]
                5 -> 3 [ label = "()" ]
                6 -> 5 [ label = "()" ]
            }"#,
            )
            .unwrap();
        }
        update();

        let node_1 = node_id_hash(&dot_structures::Id::Plain(String::from("1")));
        let node_6 = node_id_hash(&dot_structures::Id::Plain(String::from("6")));

        let latest_node_hash = node_1;

        let current_node_hash = node_6;
        let update_current = MockPerspectiveGraph::update_current_revision(
            current_node_hash.clone(),
            chrono::Utc::now(),
        );
        assert!(update_current.is_ok());

        let node_1 = &node_id_hash(&dot_structures::Id::Plain(String::from("1"))).to_string();
        let expected_additions = vec![create_link_expression(node_1, node_1)];

        let pull_res = pull::<MockPerspectiveGraph>(false, latest_node_hash.clone(), true);
        assert!(pull_res.is_ok());
        assert!(pull_res
            .unwrap()
            .diff
            .additions
            .iter()
            .all(|item| expected_additions.contains(item)));

        let current = MockPerspectiveGraph::current_revision();
        assert!(current.unwrap().unwrap().hash != current_node_hash);
    }

    #[test]
    fn test_pull_complex_merge_implicit_zero_reversed() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                r#"digraph {
                1 [ label = "1" ]
                2 [ label = "2" ]
                3 [ label = "3" ]
                4 [ label = "4" ]
                5 [ label = "5" ]
                6 [ label = "6" ]
                4 -> 2 [ label = "()" ]
                5 -> 4 [ label = "()" ]
                5 -> 3 [ label = "()" ]
                6 -> 5 [ label = "()" ]
            }"#,
            )
            .unwrap();
        }
        update();

        let node_1 = node_id_hash(&dot_structures::Id::Plain(String::from("1")));
        let node_6 = node_id_hash(&dot_structures::Id::Plain(String::from("6")));

        let latest_node_hash = node_6;

        let current_node_hash = node_1;
        let update_current = MockPerspectiveGraph::update_current_revision(
            current_node_hash.clone(),
            chrono::Utc::now(),
        );
        assert!(update_current.is_ok());

        let node_6 = &node_id_hash(&dot_structures::Id::Plain(String::from("6"))).to_string();
        let node_5 = &node_id_hash(&dot_structures::Id::Plain(String::from("5"))).to_string();
        let node_4 = &node_id_hash(&dot_structures::Id::Plain(String::from("4"))).to_string();
        let node_3 = &node_id_hash(&dot_structures::Id::Plain(String::from("3"))).to_string();
        let node_2 = &node_id_hash(&dot_structures::Id::Plain(String::from("2"))).to_string();
        let expected_additions = vec![
            create_link_expression(node_6, node_6),
            create_link_expression(node_5, node_5),
            create_link_expression(node_4, node_4),
            create_link_expression(node_3, node_3),
            create_link_expression(node_2, node_2),
        ];

        let pull_res = pull::<MockPerspectiveGraph>(false, latest_node_hash.clone(), true);
        assert!(pull_res.is_ok());
        assert!(pull_res
            .unwrap()
            .diff
            .additions
            .iter()
            .all(|item| expected_additions.contains(item)));

        let current = MockPerspectiveGraph::current_revision();
        assert!(current.unwrap().unwrap().hash != current_node_hash);
    }

    #[test]
    fn test_three_null_parents() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                r#"digraph {
                1 [ label = "1" ]
                2 [ label = "2" ]
                3 [ label = "3" ]
                4 [ label = "4" ]
                5 [ label = "5" ]

                4 -> 2
                4 -> 3
                5 -> 4
                5 -> 1
            }"#,
            )
            .unwrap();
        }
        update();

        let latest_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("5")));

        let current_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("2")));
        let update_current = MockPerspectiveGraph::update_current_revision(
            current_node_hash.clone(),
            chrono::Utc::now(),
        );
        assert!(update_current.is_ok());

        let pull_res = pull::<MockPerspectiveGraph>(false, latest_node_hash.clone(), true);
        assert!(pull_res.is_ok());
        let pull_res = pull_res.unwrap();

        let node_5 = &node_id_hash(&dot_structures::Id::Plain(String::from("5"))).to_string();
        let node_4 = &node_id_hash(&dot_structures::Id::Plain(String::from("4"))).to_string();
        let node_3 = &node_id_hash(&dot_structures::Id::Plain(String::from("3"))).to_string();
        let node_1 = &node_id_hash(&dot_structures::Id::Plain(String::from("1"))).to_string();
        let expected_additions = vec![
            create_link_expression(node_5, node_5),
            create_link_expression(node_4, node_4),
            create_link_expression(node_3, node_3),
            create_link_expression(node_1, node_1),
        ];

        assert!(pull_res
            .diff
            .additions
            .iter()
            .all(|item| expected_additions.contains(item)));
    }

    #[test]
    fn test_four_null_parents() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                r#"digraph {
                1 [ label = "1" ]
                2 [ label = "2" ]
                3 [ label = "3" ]
                4 [ label = "4" ]
                5 [ label = "5" ]
                6 [ label = "6" ]

                4 -> 2
                4 -> 3
                5 -> 4
                5 -> 1
            }"#,
            )
            .unwrap();
        }
        update();

        let latest_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("5")));

        let current_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("6")));
        let update_current = MockPerspectiveGraph::update_current_revision(
            current_node_hash.clone(),
            chrono::Utc::now(),
        );
        assert!(update_current.is_ok());

        let pull_res = pull::<MockPerspectiveGraph>(false, latest_node_hash.clone(), true);
        assert!(pull_res.is_ok());
        let pull_res = pull_res.unwrap();

        let node_5 = &node_id_hash(&dot_structures::Id::Plain(String::from("5"))).to_string();
        let node_4 = &node_id_hash(&dot_structures::Id::Plain(String::from("4"))).to_string();
        let node_3 = &node_id_hash(&dot_structures::Id::Plain(String::from("3"))).to_string();
        let node_2 = &node_id_hash(&dot_structures::Id::Plain(String::from("2"))).to_string();
        let node_1 = &node_id_hash(&dot_structures::Id::Plain(String::from("1"))).to_string();
        let expected_additions = vec![
            create_link_expression(node_5, node_5),
            create_link_expression(node_4, node_4),
            create_link_expression(node_3, node_3),
            create_link_expression(node_2, node_2),
            create_link_expression(node_1, node_1),
        ];

        assert!(pull_res
            .diff
            .additions
            .iter()
            .all(|item| expected_additions.contains(item)));

        let current = MockPerspectiveGraph::current_revision();
        assert!(current.unwrap().unwrap().hash != current_node_hash);
    }

    #[test]
    fn test_high_complex_graph() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                &crate::link_adapter::test_graphs::HIGH_COMPLEX_GRAPH,
            )
            .unwrap();
        }
        update();

        let latest_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("52")));

        let current_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("55")));
        let update_current = MockPerspectiveGraph::update_current_revision(
            current_node_hash.clone(),
            chrono::Utc::now(),
        );
        assert!(update_current.is_ok());

        let pull_res = pull::<MockPerspectiveGraph>(false, latest_node_hash.clone(), true);
        assert!(pull_res.is_ok());
        let pull_res = pull_res.unwrap();

        let mut expected_additions = create_node_id_vec(23, 52);
        expected_additions.push(create_node_id_link_expression(20));
        expected_additions.push(create_node_id_link_expression(21));

        for addition in expected_additions.clone() {
            assert!(pull_res.diff.additions.contains(&addition));
        }
        assert!(pull_res
            .diff
            .additions
            .iter()
            .all(|item| expected_additions.contains(item)));

        let current = MockPerspectiveGraph::current_revision();
        assert!(current.unwrap().unwrap().hash != current_node_hash);
    }

    #[test]
    fn test_late_join() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(&crate::link_adapter::test_graphs::LATE_JOIN)
                .unwrap();
        }
        update();

        let latest_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("314")));

        let current_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("313")));
        let update_current = MockPerspectiveGraph::update_current_revision(
            current_node_hash.clone(),
            chrono::Utc::now(),
        );
        assert!(update_current.is_ok());

        let pull_res = pull::<MockPerspectiveGraph>(false, latest_node_hash.clone(), true);
        assert!(pull_res.is_ok());
        let pull_res = pull_res.unwrap();

        let expected_additions = vec![create_node_id_link_expression(314)];

        assert!(pull_res
            .diff
            .additions
            .iter()
            .all(|item| expected_additions.contains(item)));

        let current = MockPerspectiveGraph::current_revision();
        assert!(current.unwrap().unwrap().hash != current_node_hash);
    }

    #[test]
    fn test_late_join_from_syncd() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(&crate::link_adapter::test_graphs::LATE_JOIN2)
                .unwrap();
        }
        update();

        let latest_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("304")));

        let current_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("301")));
        let update_current = MockPerspectiveGraph::update_current_revision(
            current_node_hash.clone(),
            chrono::Utc::now(),
        );
        assert!(update_current.is_ok());

        let pull_res = pull::<MockPerspectiveGraph>(false, latest_node_hash.clone(), true);
        assert!(pull_res.is_ok());
        let pull_res = pull_res.unwrap();

        let expected_additions = vec![
            create_node_id_link_expression(304),
            create_node_id_link_expression(303),
            create_node_id_link_expression(302),
        ];

        assert!(pull_res
            .diff
            .additions
            .iter()
            .all(|item| expected_additions.contains(item)));

        let current = MockPerspectiveGraph::current_revision();
        assert!(current.unwrap().unwrap().hash != current_node_hash);
    }

    #[test]
    fn test_late_join_from_unsyncd() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(&crate::link_adapter::test_graphs::LATE_JOIN2)
                .unwrap();
        }
        update();

        let latest_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("301")));

        let current_node_hash = node_id_hash(&dot_structures::Id::Plain(String::from("304")));
        let update_current = MockPerspectiveGraph::update_current_revision(
            current_node_hash.clone(),
            chrono::Utc::now(),
        );
        assert!(update_current.is_ok());

        let pull_res = pull::<MockPerspectiveGraph>(false, latest_node_hash.clone(), true);
        assert!(pull_res.is_ok());
        let pull_res = pull_res.unwrap();

        let expected_additions = create_node_id_vec(1, 301);

        assert!(pull_res
            .diff
            .additions
            .iter()
            .all(|item| expected_additions.contains(item)));

        let current = MockPerspectiveGraph::current_revision();
        assert!(current.unwrap().unwrap().hash != current_node_hash);
    }
}
