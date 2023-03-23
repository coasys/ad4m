use hdk::prelude::*;
use perspective_diff_sync_integrity::{
    EntryTypes, HashBroadcast, PerspectiveDiff, PerspectiveDiffEntryReference, PullResult,
};

use crate::errors::SocialContextResult;
use crate::link_adapter::revisions::{current_revision, update_current_revision};
use crate::link_adapter::workspace::{Workspace, NULL_NODE};
use crate::retriever::PerspectiveDiffRetreiver;
use crate::utils::get_now;
use crate::Hash;

fn merge<Retriever: PerspectiveDiffRetreiver>(
    latest: Hash,
    current: Hash,
) -> SocialContextResult<Hash> {
    debug!("===PerspectiveDiffSync.merge(): Function start");
    let fn_start = get_now()?.time();

    let latest_diff = Retriever::get::<PerspectiveDiffEntryReference>(latest.clone())?;
    let current_diff = Retriever::get::<PerspectiveDiffEntryReference>(current.clone())?;
    //Create the merge diff
    let merge_diff = PerspectiveDiff {
        additions: vec![],
        removals: vec![],
    };
    let merge_entry_hash =
        Retriever::create_entry(EntryTypes::PerspectiveDiff(merge_diff.clone()))?;

    //Create the merge entry reference
    let merge_entry_reference = PerspectiveDiffEntryReference {
        parents: Some(vec![latest, current]),
        diff: merge_entry_hash.clone(),
        diffs_since_snapshot: latest_diff.diffs_since_snapshot
            + current_diff.diffs_since_snapshot
            + 1,
    };
    let merge_entry_reference_hash = Retriever::create_entry(
        EntryTypes::PerspectiveDiffEntryReference(merge_entry_reference.clone()),
    )?;
    debug!(
        "===PerspectiveDiffSync.merge(): Commited merge entry: {:#?}",
        merge_entry_reference_hash
    );

    let now = get_now()?;
    update_current_revision::<Retriever>(merge_entry_reference_hash.clone(), now)?;

    let fn_end = get_now()?.time();
    debug!(
        "===PerspectiveDiffSync.merge() - Profiling: Took: {} to complete merge() function",
        (fn_end - fn_start).num_milliseconds()
    );
    Ok(merge_entry_reference_hash)
}

pub fn pull<Retriever: PerspectiveDiffRetreiver>(
    emit: bool,
    theirs: Hash,
    is_scribe: bool,
) -> SocialContextResult<PullResult> {
    debug!("===PerspectiveDiffSync.pull(): Function start");
    let fn_start = get_now()?.time();

    let current = current_revision::<Retriever>()?;
    let current_hash = current.clone().map(|val| val.hash);
    debug!(
        "===PerspectiveDiffSync.pull(): Pull made with theirs: {:#?} and current: {:#?}",
        theirs, current
    );

    let theirs_hash = theirs.clone();

    if Some(theirs_hash) == current_hash {
        return Ok(PullResult {
            diff: PerspectiveDiff::default(),
            current_revision: current_hash,
        });
    }

    let mut workspace = Workspace::new();

    if current.is_none() {
        workspace.collect_only_from_latest::<Retriever>(theirs.clone())?;
        let diff = workspace.squashed_diff::<Retriever>()?;
        update_current_revision::<Retriever>(theirs, get_now()?)?;
        emit_signal(diff.clone())?;
        return Ok(PullResult {
            diff: PerspectiveDiff::default(),
            current_revision: None,
        });
    }

    let current = current.expect("current missing handled above");

    workspace.build_diffs::<Retriever>(theirs.clone(), current.hash.clone())?;

    // First check if we are actually ahead of them -> we don't have to do anything
    // they will have to merge with / or fast-forward to our current
    if workspace.all_ancestors(&current.hash)?.contains(&theirs) {
        return Ok(PullResult {
            diff: PerspectiveDiff::default(),
            current_revision: Some(current.hash),
        });
    }

    let fast_forward_possible = workspace.all_ancestors(&theirs)?.contains(&current.hash);

    // If we can't fast forward, we have to merge
    // but if we are not a scribe, we can't merge
    // so in that case, we can't do anything
    if !fast_forward_possible && !is_scribe {
        debug!("===PerspectiveDiffSync.pull(): Have to merge but I'm not a scribe. Exiting without change...");
        return Ok(PullResult {
            diff: PerspectiveDiff::default(),
            current_revision: Some(current.hash),
        });
    }

    //Get all the diffs which exist between current and the last ancestor that we got
    let seen_diffs = workspace.all_ancestors(&current.hash)?;
    // println!("SEEN DIFFS: {:#?}", seen_diffs);

    //Get all the diffs in the graph which we havent seen
    let unseen_diffs = if seen_diffs.len() > 0 {
        let diffs = workspace
            .sorted_diffs
            .clone()
            .expect("should be unseen diffs after build_diffs() call")
            .into_iter()
            .filter(|val| {
                if val.0 == NULL_NODE() {
                    return false;
                };
                if val.0 == current.hash {
                    return false;
                };
                if seen_diffs.contains(&val.0) {
                    return false;
                };
                true
            })
            .collect::<Vec<(Hash, PerspectiveDiffEntryReference)>>();
        diffs
    } else {
        workspace
            .sorted_diffs
            .expect("should be unseen diffs after build_diffs() call")
            .into_iter()
            .filter(|val| val.0 != NULL_NODE() && val.0 != current.hash)
            .collect::<Vec<(Hash, PerspectiveDiffEntryReference)>>()
    };

    let (diffs, current_revision) = if fast_forward_possible {
        debug!("===PerspectiveDiffSync.pull(): There are paths between current and latest, lets fast forward the changes we have missed!");
        let mut out = PerspectiveDiff {
            additions: vec![],
            removals: vec![],
        };
        for diff in unseen_diffs {
            let diff_entry = Retriever::get::<PerspectiveDiff>(diff.1.diff.clone())?;
            out.additions.append(&mut diff_entry.additions.clone());
            out.removals.append(&mut diff_entry.removals.clone());
        }
        update_current_revision::<Retriever>(theirs.clone(), get_now()?)?;
        let fn_end = get_now()?.time();
        debug!(
            "===PerspectiveDiffSync.pull() - Profiling: Took: {} to complete pull() function",
            (fn_end - fn_start).num_milliseconds()
        );
        (out, theirs)
    } else if is_scribe {
        debug!("===PerspectiveDiffSync.pull():There are no paths between current and latest, we must merge current and latest");
        //Get the entries we missed from unseen diff
        let mut out = PerspectiveDiff {
            additions: vec![],
            removals: vec![],
        };
        for diff in unseen_diffs {
            let diff_entry = Retriever::get::<PerspectiveDiff>(diff.1.diff.clone())?;
            out.additions.append(&mut diff_entry.additions.clone());
            out.removals.append(&mut diff_entry.removals.clone());
        }

        let merge_hash = merge::<Retriever>(theirs, current.hash)?;
        let fn_end = get_now()?.time();
        debug!(
            "===PerspectiveDiffSync.pull() - Profiling: Took: {} to complete pull() function",
            (fn_end - fn_start).num_milliseconds()
        );
        (out, merge_hash)
    } else {
        (
            PerspectiveDiff {
                additions: vec![],
                removals: vec![],
            },
            current.hash,
        )
    };

    //Emit the signal in case the client connection has a timeout during the zome call
    if emit {
        if diffs.additions.len() > 0 || diffs.removals.len() > 0 {
            emit_signal(diffs.clone())?;
        }
    }
    Ok(PullResult {
        diff: diffs,
        current_revision: Some(current_revision),
    })
}

pub fn handle_broadcast<Retriever: PerspectiveDiffRetreiver>(
    broadcast: HashBroadcast,
) -> SocialContextResult<()> {
    // debug!("===PerspectiveDiffSync.fast_forward_signal(): Function start");
    // let fn_start = get_now()?.time();
    let diff_reference = broadcast.reference.clone();
    let revision = broadcast.reference_hash.clone();

    let current_revision = current_revision::<Retriever>()?;

    if current_revision.is_some() {
        let current_revision = current_revision.unwrap();
        if revision == current_revision.hash {
            // debug!("===PerspectiveDiffSync.fast_forward_signal(): Revision is the same as current");
        };
        if diff_reference.parents == Some(vec![current_revision.hash]) {
            // debug!("===PerspectiveDiffSync.fast_forward_signal(): Revisions parent is the same as current, we can fast forward our current");
            update_current_revision::<Retriever>(revision, get_now()?)?;
            emit_signal(broadcast.diff.clone())?;
        };
    };
    emit_signal(broadcast)?;
    // let fn_end = get_now()?.time();
    // debug!("===PerspectiveDiffSync.fast_forward_signal() - Profiling: Took: {} to complete fast_forward_signal() function", (fn_end - fn_start).num_milliseconds());
    Ok(())
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

        //Test that a merge actually happened and current was updated
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

        //ensure that merge was created and thus current revision got updated
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

        //ensure that merge was created and thus current revision got updated
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
        println!("{:#?}", pull_res);
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

        //ensure that no merge was created
        //let latest = MockPerspectiveGraph::latest_revision();
        //assert!(latest.unwrap().unwrap().hash == latest_node_hash);
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
        println!("{:#?}", pull_res);
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

        //ensure that merge was created and thus current revision got updated
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
        //println!("{:#?}", pull_res);
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

        //ensure that merge was created and thus current revision got updated
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
        //println!("{:#?}", pull_res);
        let pull_res = pull_res.unwrap();

        let expected_additions = vec![create_node_id_link_expression(314)];

        assert!(pull_res
            .diff
            .additions
            .iter()
            .all(|item| expected_additions.contains(item)));

        //ensure that merge was created and thus current revision got updated
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

        //ensure that merge was created and thus current revision got updated
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

        //ensure that merge was created and thus current revision got updated
        let current = MockPerspectiveGraph::current_revision();
        assert!(current.unwrap().unwrap().hash != current_node_hash);
    }
}
