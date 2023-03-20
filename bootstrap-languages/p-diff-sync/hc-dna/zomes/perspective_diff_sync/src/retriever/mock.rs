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
use crate::link_adapter::workspace::NULL_NODE;
use crate::utils::create_link_expression;
use crate::Hash;

#[derive(Debug)]
pub struct MockPerspectiveGraph {
    pub graph_map: BTreeMap<Hash, SerializedBytes>,
}

impl PerspectiveDiffRetreiver for MockPerspectiveGraph {
    fn get<T>(hash: Hash) -> SocialContextResult<T>
    where
        T: TryFrom<SerializedBytes, Error = SerializedBytesError>,
    {
        let value = &GLOBAL_MOCKED_GRAPH
            .lock()
            .expect("Could not get lock on graph map")
            .graph_map
            .get(&hash)
            .expect("Could not find entry in map")
            .to_owned();
        Ok(T::try_from(value.to_owned())?)
    }

    fn get_with_timestamp<T>(hash: Hash) -> SocialContextResult<(T, DateTime<Utc>)>
    where
        T: TryFrom<SerializedBytes, Error = SerializedBytesError>,
    {
        let value = &GLOBAL_MOCKED_GRAPH
            .lock()
            .expect("Could not get lock on graph map")
            .graph_map
            .get(&hash)
            .expect("Could not find entry in map")
            .to_owned();
        Ok((T::try_from(value.to_owned())?, Utc::now()))
    }

    fn create_entry<I, E: std::fmt::Debug, E2>(entry: I) -> SocialContextResult<Hash>
    where
        ScopedEntryDefIndex: for<'a> TryFrom<&'a I, Error = E2>,
        EntryVisibility: for<'a> From<&'a I>,
        Entry: TryFrom<I, Error = E>,
        WasmError: From<E>,
        WasmError: From<E2>,
    {
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
            let mocked_diff = PerspectiveDiffEntryReference::new(mocked_hash.clone(), parents);
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

                    //Create a mock hash for the fake diff
                    let ref_sb = SerializedBytes::try_from(diff.clone())?;
                    let mut hasher = Sha256::new();
                    hasher.update(ref_sb.bytes());
                    let mut result = hasher.finalize().as_slice().to_owned();
                    result.append(&mut vec![0xdb, 0xdb, 0xdb, 0xdb]);
                    let diff_hash = ActionHash::from_raw_36(result);

                    //Create the diff reference
                    let diff_ref = PerspectiveDiffEntryReference::new(
                        diff_hash.clone(),
                        parents.get(ref_hash).as_ref().cloned().cloned(),
                    );
                    //Insert the diff reference into the map
                    let diff_ref_sb = diff_ref
                        .try_into()
                        .expect("Could not create serialized bytes for mocked_diff");
                    graph.graph_map.insert(ref_hash.clone(), diff_ref_sb);

                    //Insert the diff into the map
                    graph.graph_map.insert(diff_hash, ref_sb);
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
    //26 since there is a diffref & diff for each node
    assert_eq!(graph.graph_map.keys().len(), 26);

    let node_12 = node_id_hash(&dot_structures::Id::Plain(String::from("12")));
    let node_11 = node_id_hash(&dot_structures::Id::Plain(String::from("11")));
    let node_10 = node_id_hash(&dot_structures::Id::Plain(String::from("10")));

    let diff_12 = graph.graph_map.get(&node_12).unwrap();
    let diff_12 = PerspectiveDiffEntryReference::try_from(diff_12.to_owned()).unwrap();
    assert_eq!(diff_12.parents, Some(vec![node_11, node_10]));
}

#[test]
fn example_test() {
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
        ActionHash::from_raw_36(vec![5; 36]),
        ActionHash::from_raw_36(vec![4; 36]),
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
    let diff_ref = MockPerspectiveGraph::get::<PerspectiveDiffEntryReference>(node_id_hash(
        &dot_structures::Id::Plain(String::from("1")),
    ));
    assert!(diff_ref.is_ok());

    use perspective_diff_sync_integrity::{
        EntryTypes, PerspectiveDiff, PerspectiveDiffEntryReference,
    };
    let commit = MockPerspectiveGraph::create_entry(EntryTypes::PerspectiveDiff(PerspectiveDiff {
        additions: vec![],
        removals: vec![],
    }));
    assert!(commit.is_ok());

    let get_commit = MockPerspectiveGraph::get::<PerspectiveDiff>(commit.unwrap());
    assert!(get_commit.is_ok());
}
