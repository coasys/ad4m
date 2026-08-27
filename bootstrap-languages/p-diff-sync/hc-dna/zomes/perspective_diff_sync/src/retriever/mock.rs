use chrono::{DateTime, Utc};
use dot_structures;
use graphviz_rust;
use hdk::prelude::*;
use perspective_diff_sync_integrity::{
    HashReference, LinkExpression, LocalHashReference, PerspectiveDiff,
    PerspectiveDiffEntryReference,
};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::sync::Mutex;

use super::PerspectiveDiffRetreiver;
use crate::errors::{SocialContextError, SocialContextResult};
use crate::link_adapter::conversions::{
    entry_ref_from_algo, entry_ref_to_algo, hash_from_algo, hash_ref_to_algo, hash_to_algo,
    local_hash_ref_to_algo,
};
use crate::link_adapter::workspace::NULL_NODE;
use crate::utils::create_link_expression;
use crate::Hash;
use perspective_diff_algorithm as algo;

#[derive(Debug)]
pub struct MockPerspectiveGraph {
    pub graph_map: BTreeMap<Hash, SerializedBytes>,
}

impl PerspectiveDiffRetreiver for MockPerspectiveGraph {
    fn get(hash: Hash) -> SocialContextResult<PerspectiveDiffEntryReference> {
        let value = GLOBAL_MOCKED_GRAPH
            .lock()
            .expect("Could not get lock on graph map")
            .graph_map
            .get(&hash)
            .expect("Could not find entry in map")
            .to_owned();
        Ok(PerspectiveDiffEntryReference::try_from(value)?)
    }

    fn get_with_timestamp(
        hash: Hash,
    ) -> SocialContextResult<(PerspectiveDiffEntryReference, DateTime<Utc>)> {
        let value = GLOBAL_MOCKED_GRAPH
            .lock()
            .expect("Could not get lock on graph map")
            .graph_map
            .get(&hash)
            .expect("Could not find entry in map")
            .to_owned();
        Ok((PerspectiveDiffEntryReference::try_from(value)?, Utc::now()))
    }

    fn create_entry(
        entry: perspective_diff_sync_integrity::EntryTypes,
    ) -> SocialContextResult<Hash> {
        let mut object_store = GLOBAL_MOCKED_GRAPH
            .lock()
            .expect("Could not get lock on OBJECT_STORE");

        let entry: Entry = entry.try_into().expect("Could not get Entry");
        let sb = match entry {
            Entry::App(bytes) => bytes,
            _ => panic!("Should not get any entry except app"),
        };
        let bytes = sb.bytes();

        let mut hasher = Sha256::new();
        hasher.update(bytes);
        let mut result = hasher.finalize().as_slice().to_owned();
        result.append(&mut vec![0xdb, 0xdb, 0xdb, 0xdb]);

        let hash = ActionHash::from_raw_36(result);
        object_store.graph_map.insert(hash.clone(), sb.0);
        Ok(hash)
    }

    fn current_revision() -> SocialContextResult<Option<LocalHashReference>> {
        let revision = CURRENT_REVISION
            .lock()
            .expect("Could not get lock on CURRENT_REVISION");
        Ok(revision.clone().map(|val| LocalHashReference {
            hash: val,
            timestamp: Utc::now(),
        }))
    }

    fn latest_revision() -> SocialContextResult<Option<HashReference>> {
        let revision = LATEST_REVISION
            .lock()
            .expect("Could not get lock on LATEST_REVISION");
        Ok(revision.clone().map(|val| HashReference {
            hash: val,
            timestamp: Utc::now(),
        }))
    }

    fn update_current_revision(hash: Hash, _timestamp: DateTime<Utc>) -> SocialContextResult<()> {
        let mut revision = CURRENT_REVISION
            .lock()
            .expect("Could not get lock on CURRENT_REVISION");
        *revision = Some(hash);
        Ok(())
    }

    fn update_latest_revision(hash: Hash, _timestamp: DateTime<Utc>) -> SocialContextResult<()> {
        let mut revision = LATEST_REVISION
            .lock()
            .expect("Could not get lock on LATEST_REVISION");
        *revision = Some(hash);
        Ok(())
    }
}

// Step 13b-C phase 2: bridge to the algorithm-crate's `WorkspaceRetriever`
// trait. Conversions take the algo `Hash` → HoloHash via the existing
// integrity-zome retrieval, then return the algo mirror entry-ref.
//
// The mock graph never carries Snapshot links — the workspace tests
// that need snapshots are the holochain-side `snapshots::tests`, not
// the algorithm-crate's BFS tests. Return `Ok(None)` for snapshots.
impl algo::WorkspaceRetriever for MockPerspectiveGraph {
    fn get_p_diff_reference(
        hash: &algo::Hash,
    ) -> algo::AlgoResult<algo::PerspectiveDiffEntryReference> {
        let h = hash_from_algo(hash);
        let entry = <Self as PerspectiveDiffRetreiver>::get(h)
            .map_err(|e| algo::AlgoError::Retriever(format!("{}", e)))?;
        Ok(entry_ref_to_algo(entry))
    }

    fn get_snapshot_by_target(
        _target_hash: &algo::Hash,
    ) -> algo::AlgoResult<Option<algo::Snapshot>> {
        Ok(None)
    }
}

// Step 13b-D — round-trips through the existing
// `PerspectiveDiffRetreiver::create_entry` (which hashes the
// SerializedBytes payload, matching MockPerspectiveGraph's hashing
// convention).
impl algo::SnapshotRetriever for MockPerspectiveGraph {
    fn create_diff_entry(
        entry: algo::PerspectiveDiffEntryReference,
    ) -> algo::AlgoResult<algo::Hash> {
        let integrity = entry_ref_from_algo(entry);
        let hash = <Self as PerspectiveDiffRetreiver>::create_entry(
            perspective_diff_sync_integrity::EntryTypes::PerspectiveDiffEntryReference(integrity),
        )
        .map_err(|e| algo::AlgoError::Retriever(format!("{}", e)))?;
        Ok(hash_to_algo(&hash))
    }
}

// Step 13b-E — forwards to the existing HDK-trait methods, which back
// onto the in-process `CURRENT_REVISION` / `LATEST_REVISION` Mutex
// statics declared further down this file.
impl algo::RevisionsRetriever for MockPerspectiveGraph {
    fn current_revision() -> algo::AlgoResult<Option<algo::LocalHashReference>> {
        let rev = <Self as PerspectiveDiffRetreiver>::current_revision()
            .map_err(|e| algo::AlgoError::Retriever(format!("{}", e)))?;
        Ok(rev.map(local_hash_ref_to_algo))
    }

    fn latest_revision() -> algo::AlgoResult<Option<algo::HashReference>> {
        let rev = <Self as PerspectiveDiffRetreiver>::latest_revision()
            .map_err(|e| algo::AlgoError::Retriever(format!("{}", e)))?;
        Ok(rev.map(hash_ref_to_algo))
    }

    fn update_current_revision(
        hash: algo::Hash,
        timestamp: chrono::DateTime<chrono::Utc>,
    ) -> algo::AlgoResult<()> {
        <Self as PerspectiveDiffRetreiver>::update_current_revision(
            hash_from_algo(&hash),
            timestamp,
        )
        .map_err(|e| algo::AlgoError::Retriever(format!("{}", e)))
    }
}

pub struct GraphInput {
    pub nodes: u8,
    pub associations: Vec<Associations>,
}

pub struct Associations {
    pub node_source: u8,
    pub node_targets: Vec<u8>,
}

#[allow(dead_code)]
pub fn node_id_hash(id: &dot_structures::Id) -> Hash {
    let mut string = match id {
        dot_structures::Id::Html(s) => s,
        dot_structures::Id::Escaped(s) => s,
        dot_structures::Id::Plain(s) => s,
        dot_structures::Id::Anonymous(s) => s,
    }
    .clone();
    if string.len() > 36 {
        let _ = string.split_off(36);
    } else {
        while string.len() < 36 {
            string.push_str("x");
        }
    }
    ActionHash::from_raw_36(string.into_bytes())
}

#[allow(dead_code)]
pub fn hash_to_node_id(hash: ActionHash) -> String {
    if hash == NULL_NODE() {
        return String::from("NULL_NODE");
    };
    let hash_bytes = hash.get_raw_36();

    match std::str::from_utf8(hash_bytes) {
        Ok(node_id_string) => {
            let string_split = node_id_string
                .split("x")
                .collect::<Vec<&str>>()
                .first()
                .unwrap()
                .to_owned();
            string_split.to_string()
        }
        Err(_err) => hash.to_string(),
    }
}

// #[allow(dead_code)]
// pub fn string_to_node_id(mut hash: String) -> String {
//     if hash == NULL_NODE().to_string() {
//         return String::from("NULL_NODE")
//     };
//     if hash.len() > 36 {
//         let _ = hash.split_off(36);
//     };
//     let hash = ActionHash::from_raw_36(hash.into_bytes());
//     let hash = hash.get_raw_36();
//     let node_id_string = std::str::from_utf8(hash).expect("could not get string from hash array");
//     let string_split = node_id_string.split("x").collect::<Vec<&str>>().first().unwrap().to_owned();
//     string_split.to_string()
// }

#[allow(dead_code)]
pub fn create_node_id_link_expression(node_id: u32) -> LinkExpression {
    let node_id = node_id.to_string();
    let node_id = dot_structures::Id::Plain(node_id);
    let node = &node_id_hash(&node_id).to_string();
    create_link_expression(node, node)
}

#[allow(dead_code)]
pub fn create_node_id_vec(range_start: u32, range_end: u32) -> Vec<LinkExpression> {
    let mut out = vec![];
    for n in range_start..=range_end {
        out.push(create_node_id_link_expression(n));
    }
    out
}

// #[allow(dead_code)]
// pub fn link_expression_to_node_id(links: &mut Vec<LinkExpression>) {
//     links.iter_mut().for_each(|link| {
//         if link.data.source.is_some() {
//             link.data.source = Some(string_to_node_id(link.data.source.clone().unwrap()));
//         }
//         if link.data.target.is_some() {
//             link.data.target = Some(string_to_node_id(link.data.target.clone().unwrap()));
//         }
//     })
// }

#[allow(dead_code)]
fn unwrap_vertex(v: dot_structures::Vertex) -> Option<dot_structures::NodeId> {
    match v {
        dot_structures::Vertex::N(id) => Some(id),
        _ => None,
    }
}

#[allow(dead_code)]
fn unwrap_edge(
    edge: dot_structures::Edge,
) -> Option<(dot_structures::NodeId, dot_structures::NodeId)> {
    match edge.ty {
        dot_structures::EdgeTy::Pair(a, b) => {
            let au = unwrap_vertex(a);
            let ab = unwrap_vertex(b);
            if au.is_some() && ab.is_some() {
                Some((au.unwrap(), ab.unwrap()))
            } else {
                None
            }
        }
        _ => None,
    }
}

impl MockPerspectiveGraph {
    pub fn new(graph_input: GraphInput) -> MockPerspectiveGraph {
        let mut graph = MockPerspectiveGraph {
            graph_map: BTreeMap::new(),
        };

        for n in 0..graph_input.nodes {
            let mocked_hash = ActionHash::from_raw_36(vec![n; 36]);
            let associations: Vec<&Associations> = graph_input
                .associations
                .iter()
                .filter(|association| association.node_source == n)
                .collect();
            let parents = if associations.len() > 0 {
                let mut temp = vec![];
                for association in associations.clone() {
                    for targets in association.node_targets.clone() {
                        temp.push(ActionHash::from_raw_36(vec![targets; 36]))
                    }
                }
                Some(temp)
            } else {
                None
            };
            let mocked_diff = PerspectiveDiffEntryReference::new(
                PerspectiveDiff {
                    additions: vec![create_link_expression(
                        &mocked_hash.to_string(),
                        &mocked_hash.to_string(),
                    )],
                    removals: vec![],
                },
                parents,
            );
            let sb = mocked_diff
                .try_into()
                .expect("Could not create serialized bytes for mocked_diff");
            graph.graph_map.insert(mocked_hash, sb);
        }

        graph
    }

    #[allow(dead_code)]
    pub fn from_dot(source: &str) -> SocialContextResult<MockPerspectiveGraph> {
        match graphviz_rust::parse(source)
            .map_err(|_| SocialContextError::InternalError("Can't parse as DOT string"))?
        {
            dot_structures::Graph::Graph { .. } => Err(SocialContextError::InternalError(
                "Can't work with undirected DOT graphs",
            )),
            dot_structures::Graph::DiGraph { stmts, .. } => {
                let mut graph = MockPerspectiveGraph {
                    graph_map: BTreeMap::new(),
                };

                let mut hashes = Vec::<Hash>::new();
                let mut parents: BTreeMap<Hash, Vec<Hash>> = BTreeMap::new();

                for s in stmts.iter() {
                    match s {
                        dot_structures::Stmt::Node(node) => hashes.push(node_id_hash(&node.id.0)),
                        dot_structures::Stmt::Edge(edge) => {
                            if let Some(e) = unwrap_edge(edge.clone()) {
                                let id_0 = e.0 .0;
                                let id_1 = e.1 .0;
                                let child = node_id_hash(&id_0);
                                let parent = node_id_hash(&id_1);
                                //println!("Edge: {} -> {}", id_0, id_1);
                                //println!("Edge: {} -> {}", child, parent);
                                match parents.remove(&child) {
                                    None => parents.insert(child, vec![parent]),
                                    Some(mut prev) => {
                                        prev.push(parent);
                                        parents.insert(child, prev)
                                    }
                                };
                            }
                        }
                        _ => {}
                    }
                }

                for ref_hash in hashes.iter() {
                    //Create a mock diff
                    let diff = PerspectiveDiff {
                        additions: vec![create_link_expression(
                            &ref_hash.to_string(),
                            &ref_hash.to_string(),
                        )],
                        removals: vec![],
                    };

                    //Create the diff reference with embedded diff data
                    let diff_ref = PerspectiveDiffEntryReference::new(
                        diff,
                        parents.get(ref_hash).as_ref().cloned().cloned(),
                    );
                    //Insert only the diff reference into the map at the node hash
                    let diff_ref_sb = diff_ref
                        .try_into()
                        .expect("Could not create serialized bytes for mocked_diff");
                    graph.graph_map.insert(ref_hash.clone(), diff_ref_sb);
                }

                Ok(graph)
            }
        }
    }
}

lazy_static! {
    pub static ref GLOBAL_MOCKED_GRAPH: Mutex<MockPerspectiveGraph> =
        Mutex::new(MockPerspectiveGraph::new(GraphInput {
            nodes: 1,
            associations: vec![]
        }));
    pub static ref CURRENT_REVISION: Mutex<Option<Hash>> = Mutex::new(None);
    pub static ref LATEST_REVISION: Mutex<Option<Hash>> = Mutex::new(None);
}

#[test]
fn can_create_graph() {
    let test = MockPerspectiveGraph::new(GraphInput {
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
    assert_eq!(test.graph_map.keys().len(), 6);
    println!("Got graph: {:#?}", test.graph_map);
}

#[test]
fn can_create_graph_from_dot() {
    let dot = "digraph {
        0 [ label = \"0\" ]
        1 [ label = \"1\" ]
        2 [ label = \"2\" ]
        3 [ label = \"3\" ]
        4 [ label = \"4\" ]
        5 [ label = \"5\" ]
        6 [ label = \"6\" ]
        7 [ label = \"7\" ]
        8 [ label = \"8\" ]
        9 [ label = \"9\" ]
        10 [ label = \"10\" ]
        11 [ label = \"11\" ]
        12 [ label = \"12\" ]
        1 -> 0 [ label = \"()\" ]
        2 -> 1 [ label = \"()\" ]
        3 -> 2 [ label = \"()\" ]
        4 -> 3 [ label = \"()\" ]
        5 -> 4 [ label = \"()\" ]
        6 -> 5 [ label = \"()\" ]
        7 -> 1 [ label = \"()\" ]
        8 -> 7 [ label = \"()\" ]
        9 -> 8 [ label = \"()\" ]
        10 -> 9 [ label = \"()\" ]
        11 -> 10 [ label = \"()\" ]
        12 -> 11 [ label = \"()\" ]
        12 -> 10 [ label = \"()\" ]
    }";

    let graph = MockPerspectiveGraph::from_dot(dot).expect("from_dot not to return error");
    //13 since we only create PerspectiveDiffEntryReference entries, not separate PerspectiveDiff entries
    assert_eq!(graph.graph_map.keys().len(), 13);

    let node_12 = node_id_hash(&dot_structures::Id::Plain(String::from("12")));
    let node_11 = node_id_hash(&dot_structures::Id::Plain(String::from("11")));
    let node_10 = node_id_hash(&dot_structures::Id::Plain(String::from("10")));

    let diff_12 = graph.graph_map.get(&node_12).unwrap();
    let diff_12 = PerspectiveDiffEntryReference::try_from(diff_12.to_owned()).unwrap();
    assert_eq!(diff_12.parents, Some(vec![node_11, node_10]));
}

#[test]
fn example_test() {
    use crate::link_adapter::conversions::hash_to_algo;
    use crate::link_adapter::workspace::Workspace;

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
        hash_to_algo(&ActionHash::from_raw_36(vec![5; 36])),
        hash_to_algo(&ActionHash::from_raw_36(vec![4; 36])),
    );
    println!("Got result: {:#?}", res);
}

#[test]
fn can_get_and_create_mocked_holochain_objects() {
    fn update() {
        let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
        let dot = "digraph {
            0 [ label = \"0\" ]
            1 [ label = \"1\" ]
            2 [ label = \"2\" ]
            3 [ label = \"3\" ]
            4 [ label = \"4\" ]
            5 [ label = \"5\" ]
            6 [ label = \"6\" ]
            7 [ label = \"7\" ]
            8 [ label = \"8\" ]
            9 [ label = \"9\" ]
            10 [ label = \"10\" ]
            11 [ label = \"11\" ]
            12 [ label = \"12\" ]
            1 -> 0 [ label = \"()\" ]
            2 -> 1 [ label = \"()\" ]
            3 -> 2 [ label = \"()\" ]
            4 -> 3 [ label = \"()\" ]
            5 -> 4 [ label = \"()\" ]
            6 -> 5 [ label = \"()\" ]
            7 -> 1 [ label = \"()\" ]
            8 -> 7 [ label = \"()\" ]
            9 -> 8 [ label = \"()\" ]
            10 -> 9 [ label = \"()\" ]
            11 -> 10 [ label = \"()\" ]
            12 -> 11 [ label = \"()\" ]
            12 -> 10 [ label = \"()\" ]
        }";
        *graph = MockPerspectiveGraph::from_dot(dot).expect("Could not create graph");
    }
    update();
    let diff_ref =
        MockPerspectiveGraph::get(node_id_hash(&dot_structures::Id::Plain(String::from("1"))));
    assert!(diff_ref.is_ok());

    use perspective_diff_sync_integrity::{
        EntryTypes, PerspectiveDiff, PerspectiveDiffEntryReference,
    };
    let commit = MockPerspectiveGraph::create_entry(EntryTypes::PerspectiveDiffEntryReference(
        PerspectiveDiffEntryReference::new(
            PerspectiveDiff {
                additions: vec![create_link_expression("test", "test")],
                removals: vec![],
            },
            None,
        ),
    ));
    assert!(commit.is_ok());

    let get_commit = MockPerspectiveGraph::get(commit.unwrap());
    assert!(get_commit.is_ok());
}
