use hdk::prelude::*;
use itertools::Itertools;
use perspective_diff_sync_integrity::{
    LinkTypes, PerspectiveDiff, PerspectiveDiffEntryReference, Snapshot,
};
use petgraph::{
    algo::dominators::simple_fast,
    dot::{Config, Dot},
    graph::{DiGraph, Graph, NodeIndex, UnGraph},
};
use std::cell::RefCell;
use std::collections::{BTreeMap, VecDeque};

use crate::errors::{SocialContextError, SocialContextResult};
use crate::link_adapter::topo_sort::topo_sort_diff_references;
use crate::retriever::{hash_to_node_id, PerspectiveDiffRetreiver};
use crate::utils::get_now;
use crate::Hash;

pub struct Workspace {
    pub graph: DiGraph<Hash, ()>,
    pub undirected_graph: UnGraph<Hash, ()>,
    pub node_index_map: BTreeMap<Hash, NodeIndex<u32>>,
    pub entry_map: BTreeMap<Hash, PerspectiveDiffEntryReference>,
    pub sorted_diffs: Option<Vec<(Hash, PerspectiveDiffEntryReference)>>,
    pub common_ancestors: Vec<Hash>,
    pub diffs: BTreeMap<Hash, PerspectiveDiffEntryReference>,
    pub back_links: BTreeMap<Hash, BTreeSet<Hash>>,
    unexplored_side_branches: BTreeSet<Hash>,
}

#[derive(Clone)]
struct BfsSearch {
    pub found_ancestors: RefCell<Vec<Hash>>,
    pub bfs_branches: RefCell<Vec<Hash>>,
    pub reached_end: bool,
}

#[allow(non_snake_case)]
pub fn NULL_NODE() -> ActionHash {
    ActionHash::from_raw_36(vec![0xdb; 36])
}

impl BfsSearch {
    pub fn new(start: Hash) -> BfsSearch {
        let branches = RefCell::new(Vec::from([start]));
        BfsSearch {
            found_ancestors: RefCell::new(Vec::new()),
            bfs_branches: branches,
            reached_end: false,
        }
    }
}

impl std::fmt::Debug for BfsSearch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if cfg!(test) {
            let ancestors: Vec<_> = self
                .found_ancestors
                .borrow()
                .clone()
                .into_iter()
                .map(|val| hash_to_node_id(val))
                .collect();
            let branches: Vec<_> = self
                .bfs_branches
                .borrow()
                .clone()
                .into_iter()
                .map(|val| hash_to_node_id(val))
                .collect();
            write!(
                f,
                "BfsSearch {{ found_ancestors: {:?},\n bfs_branches: {:?},\n reached_end: {:?} }}",
                ancestors, branches, self.reached_end
            )
        } else {
            write!(
                f,
                "BfsSearch {{ found_ancestors: {:?},\n bfs_branches: {:?},\n reached_end: {:?} }}",
                self.found_ancestors, self.bfs_branches, self.reached_end
            )
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
enum SearchSide {
    Theirs,
    Ours,
}

fn other_side(side: &SearchSide) -> SearchSide {
    match side {
        SearchSide::Theirs => SearchSide::Ours,
        SearchSide::Ours => SearchSide::Theirs,
    }
}

impl Workspace {
    pub fn new() -> Workspace {
        Workspace {
            graph: Graph::new(),
            undirected_graph: Graph::new_undirected(),
            node_index_map: BTreeMap::new(),
            entry_map: BTreeMap::new(),
            sorted_diffs: None,
            common_ancestors: vec![],
            diffs: BTreeMap::new(),
            back_links: BTreeMap::new(),
            unexplored_side_branches: BTreeSet::new(),
        }
    }

    // This is the easy case when we only build from one hash.
    // (either latest or our current hash, like in render).
    // We don't have to check for forks, we just deep search from the given
    // diff and terminate at leafs and snapshots.
    // Since we don't have to detect and handle forks, we don't need
    // to unroll snapshots and just treat them as leafs.
    pub fn collect_only_from_latest<Retriever: PerspectiveDiffRetreiver>(
        &mut self,
        latest: Hash,
    ) -> SocialContextResult<()> {
        debug!("===Workspace.collect_only_from_latest(): Function start");
        let fn_start = get_now()?.time();

        // Initializing with only one branch starting from the given hash.
        let mut unprocessed_branches = VecDeque::new();
        unprocessed_branches.push_back(latest);

        let mut snapshot_seen = vec![];

        while !unprocessed_branches.is_empty() {
            let current_hash = unprocessed_branches[0].clone();

            if self.entry_map.contains_key(&current_hash) && !snapshot_seen.contains(&current_hash)
            {
                debug!("===Workspace.collect_only_from_latest(): CIRCLE DETECTED! Closing current branch...");
                unprocessed_branches.pop_front();
                continue;
            }

            let current_diff = Self::get_p_diff_reference::<Retriever>(current_hash.clone())?;

            if current_diff.diffs_since_snapshot == 0 {
                debug!("===Workspace.collect_only_from_latest(): Found a perspective diff reference containing a snapshot!");
                let snapshot = Self::get_snapshot(current_diff.clone())?;

                if snapshot.is_none() {
                    debug!("===Workspace.collect_only_from_latest(): ERROR: Expected to find snapshot link on current_diff where diffs_since_snapshot was 0");
                    self.handle_parents(current_diff, current_hash, &mut unprocessed_branches);
                } else {
                    let mut snapshot = snapshot.unwrap();

                    let mut last_diff = None;
                    for i in 0..snapshot.diff_chunks.len() {
                        let diff_chunk = &snapshot.diff_chunks[i];
                        self.entry_map.insert(
                            diff_chunk.clone(),
                            PerspectiveDiffEntryReference::new(
                                diff_chunk.clone(),
                                last_diff.clone(),
                            ),
                        );
                        last_diff = Some(vec![diff_chunk.clone()]);
                    }

                    self.entry_map.insert(
                        current_hash.clone(),
                        PerspectiveDiffEntryReference::new(current_diff.diff, last_diff.clone()),
                    );

                    snapshot_seen.append(&mut snapshot.included_diffs);

                    // Snapshot terminates like an orphan.
                    // So we can close this branch and potentially continue
                    // with other unprocessed branches, if they exist.
                    unprocessed_branches.pop_front();
                };
            } else {
                self.handle_parents(current_diff, current_hash, &mut unprocessed_branches);
            }
        }

        let fn_end = get_now()?.time();
        debug!("===Workspace.collect_only_from_latest() - Profiling: Took: {} to complete collect_only_from_latest() function", (fn_end - fn_start).num_milliseconds());

        Ok(())
    }

    fn handle_parents(
        &mut self,
        current_diff: PerspectiveDiffEntryReference,
        current_hash: Hash,
        unprocessed_branches: &mut VecDeque<Hash>,
    ) {
        if let Some(parents) = &current_diff.parents {
            for i in 0..parents.len() {
                // Depth-first search:
                // We are replacing our search position (==current_hash==unprocessed_branches[0])
                // with the first parent.
                // Other parents are pushed on the vec as new branches to search later..
                if i == 0 {
                    unprocessed_branches[0] = parents[i].clone();
                } else {
                    unprocessed_branches.push_back(parents[i].clone())
                }
            }
        } else {
            // We arrived at a leaf/orphan (no parents).
            // So we can close this branch and potentially continue
            // with other unprocessed branches, if they exist.
            unprocessed_branches.pop_front();
        }

        self.entry_map.insert(current_hash, current_diff);
    }

    pub fn sort_graph(&mut self) -> SocialContextResult<()> {
        //debug!("===Workspace.sort_graph(): Function start");
        //let fn_start = get_now()?.time();

        let common_ancestor = self.common_ancestors.last().unwrap();

        //TODO; this should probably be a Map but tests break when it is a map
        let mut sorted: Vec<(Hash, PerspectiveDiffEntryReference)> = Vec::new();
        let mut visited: HashSet<Hash> = HashSet::new();
        let mut next: VecDeque<Hash> = VecDeque::new();
        self.unexplored_side_branches = BTreeSet::new();
        //let mut inner_iter = 0;

        next.push_back(common_ancestor.clone());

        while !next.is_empty() {
            let current = next.pop_front().expect("must be Ok since next !is_empty()");
            if !visited.contains(&current) {
                //inner_iter += 1;
                //println!("current: {:?}", hash_to_node_id(current.clone()));
                match self.back_links.get(&current) {
                    Some(children) => {
                        //println!("--> has {} children, checking the children to see if there is a missing parent link", children.len());
                        //println!("Children are: {:#?}", children.clone().into_iter().map(|child| hash_to_node_id(child)).collect::<Vec<String>>());
                        for child in children.iter() {
                            let diff = self.diffs.get(&child).expect("Should child must exist");
                            if diff.parents.is_some() {
                                for parent in diff.parents.as_ref().unwrap() {
                                    if parent != &current {
                                        //println!("Found missing parent: {:?}", hash_to_node_id(parent.clone()));
                                        self.unexplored_side_branches.insert(parent.clone());
                                    }
                                }
                            }
                        }
                        let mut unseen_children = children
                            .to_owned()
                            .into_iter()
                            .filter(|child| !next.contains(child))
                            .collect::<VecDeque<_>>();
                        next.append(&mut unseen_children);
                    }
                    None => {}
                };
                let current_diff = self.diffs.get(&current).expect("diffs should be populated");
                sorted.push((current.clone(), current_diff.clone()));
                if self.entry_map.get(&current).is_none() {
                    self.entry_map.insert(current.clone(), current_diff.clone());
                };
                visited.insert(current);
            }
        }
        //debug!(
        //    "===Workspace.sort_graph(): Made {:?} total iterations",
        //    inner_iter
        //);

        self.unexplored_side_branches = self
            .unexplored_side_branches
            .iter()
            .filter(|b| !sorted.iter().find(|s| s.0 == **b).is_some())
            .cloned()
            .collect();

        // println!("SortGraph iter: Unexplored side branches: {:?}", self.unexplored_side_branches.clone().into_iter().map(|child| hash_to_node_id(child)).collect::<Vec<String>>());

        //println!("Sorted is: {:?}", sorted.clone().into_iter().map(|val| hash_to_node_id(val.0)).collect::<Vec<_>>());
        self.sorted_diffs = Some(sorted.into_iter().unique().collect());

        //let fn_end = get_now()?.time();
        //debug!(
        //    "===Workspace.sort_graph() - Profiling: Took: {} to complete sort_graph() function",
        //    (fn_end - fn_start).num_milliseconds()
        //);

        Ok(())
    }

    pub fn build_diffs<Retriever: PerspectiveDiffRetreiver>(
        &mut self,
        theirs: Hash,
        ours: Hash,
    ) -> SocialContextResult<()> {
        debug!("===Workspace.build_diffs(): Function start");
        let fn_start = get_now()?.time();

        let common_ancestor = self.collect_until_common_ancestor::<Retriever>(theirs, ours)?;
        self.common_ancestors.push(common_ancestor);

        //println!("===PerspectiveDiffSunc.build_diffs(): Got diffs: {:?}", self.diffs.iter().map(|x| hash_to_node_id(x.0.to_owned())).collect::<Vec<_>>());
        //println!("===PerspectiveDiffSunc.build_diffs(): Got back_links: {:?}", self.back_links.iter().map(|x| hash_to_node_id(x.0.to_owned())).collect::<Vec<_>>());

        self.sort_graph()?;
        //println!("===PerspectiveDiffSunc.build_diffs(): Got unexplored side branches parent: {:#?}", self.unexplored_side_branches.iter().map(|x| hash_to_node_id(x.to_owned())).collect::<Vec<_>>());

        while self.unexplored_side_branches.len() > 0 {
            let unexplored_side_branch = self
                .unexplored_side_branches
                .iter()
                .next_back()
                .unwrap()
                .to_owned();
            let ours = self
                .common_ancestors
                .last()
                .expect("There should have been a common ancestor above")
                .to_owned();
            //println!("===Workspace.build_diffs(): making an explored side branch iteration: {:?} and ours: {:?}", hash_to_node_id(unexplored_side_branch.clone()), hash_to_node_id(ours.clone()));
            let common_ancestor =
                self.collect_until_common_ancestor::<Retriever>(unexplored_side_branch, ours)?;
            self.common_ancestors.push(common_ancestor.clone());
            self.sort_graph()?;
            //println!("===PerspectiveDiffSync.build_diffs(): Got common ancestor: {:?}", hash_to_node_id(common_ancestor));
        }

        let sorted_diffs = self.sorted_diffs.as_mut().unwrap();
        sorted_diffs.get_mut(0).unwrap().1.parents = None;
        self.sorted_diffs = Some(topo_sort_diff_references(sorted_diffs)?);
        // println!("===PerspectiveDiffSunc.build_diffs(): Got sorted diffs: {:#?}", self.sorted_diffs);

        self.build_graph()?;
        self.print_graph_debug();

        let fn_end = get_now()?.time();
        debug!(
            "===Workspace.build_diffs() - Profiling: Took: {} to complete build_diffs() function",
            (fn_end - fn_start).num_milliseconds()
        );

        Ok(())
    }

    fn terminate_with_null_node(
        &mut self,
        current_hash: Hash,
        side: SearchSide,
        searches: &mut BTreeMap<SearchSide, BfsSearch>,
    ) -> SocialContextResult<()> {
        let search_clone = searches.clone();
        let other = search_clone
            .get(&other_side(&side))
            .ok_or(SocialContextError::InternalError("search side not found"))?;
        let search = searches
            .get_mut(&side)
            .ok_or(SocialContextError::InternalError("search side not found"))?;

        if !search.found_ancestors.borrow().contains(&NULL_NODE()) {
            search.found_ancestors.get_mut().push(NULL_NODE());
        };
        if !other.found_ancestors.borrow().contains(&NULL_NODE()) {
            let other_mut = searches
                .get_mut(&other_side(&side))
                .ok_or(SocialContextError::InternalError("search side not found"))?;
            other_mut.found_ancestors.get_mut().push(NULL_NODE());
        };
        if self.diffs.get(&NULL_NODE()).is_none() {
            let current_diff = PerspectiveDiffEntryReference::new(NULL_NODE(), None);
            self.diffs.insert(NULL_NODE(), current_diff.clone());
        };

        let mut set = if let Some(nodes_back_links) = self.back_links.get(&NULL_NODE()) {
            let mut nodes_back_links = nodes_back_links.clone();
            if let Some(other_last) = other.found_ancestors.borrow().last().clone() {
                if other_last != &NULL_NODE() {
                    nodes_back_links.insert(other_last.clone());
                }
            }
            nodes_back_links.clone()
        } else {
            let mut set = BTreeSet::new();
            if let Some(other_last) = other.found_ancestors.borrow().last().clone() {
                if other_last != &NULL_NODE() {
                    set.insert(other_last.clone());
                }
            }
            set
        };
        if current_hash != NULL_NODE() {
            set.insert(current_hash);
        };
        self.back_links.insert(NULL_NODE(), set);
        Ok(())
    }

    pub fn collect_until_common_ancestor<Retriever: PerspectiveDiffRetreiver>(
        &mut self,
        theirs: Hash,
        ours: Hash,
    ) -> SocialContextResult<Hash> {
        //debug!("===Workspace.collect_until_common_ancestor(): Function start");
        let fn_start = get_now()?.time();

        let mut common_ancestor: Option<Hash> = None;

        let mut searches = btreemap! {
            SearchSide::Theirs => BfsSearch::new(theirs),
            SearchSide::Ours => BfsSearch::new(ours),
        };

        while common_ancestor.is_none() {
            // println!("===Workspace.collect_until_common_ancestor(): collect_until_common_ancestor 2: {:#?}", searches.get(&SearchSide::Theirs).unwrap().bfs_branches.borrow());
            // println!("===Workspace.collect_until_common_ancestor(): collect_until_common_ancestor 2: {:#?}", searches.get(&SearchSide::Ours).unwrap().bfs_branches.borrow());
            // do the same BFS for theirs_branches and ours_branches..
            for side in vec![SearchSide::Theirs, SearchSide::Ours] {
                println!("Checking side: {:#?}", side);
                let search_clone = searches.clone();
                let other = search_clone.get(&other_side(&side)).ok_or(
                    SocialContextError::InternalError("other search side not found"),
                )?;
                let search = searches
                    .get_mut(&side)
                    .ok_or(SocialContextError::InternalError("search side not found"))?;
                let branches = search.bfs_branches.get_mut();
                branches.dedup();

                for branch_index in 0..branches.len() {
                    println!("===Workspace.collect_until_common_ancestor(): collect_until_common_ancestor 2.1");
                    let current_hash = branches[branch_index].clone();
                    println!(
                        "Checking current hash: {:#?}",
                        hash_to_node_id(current_hash.clone())
                    );

                    let already_visited = search.found_ancestors.borrow().contains(&current_hash);
                    let seen_on_other_side = other.found_ancestors.borrow().contains(&current_hash)
                        || other.bfs_branches.borrow().contains(&current_hash);

                    if already_visited {
                        println!("===Workspace.collect_until_common_ancestor(): collect_until_common_ancestor 2.2 ALREADY VISITED");
                        // We've seen this diff on this side, so we are at the end of a branch.
                        // Just ignore this hash and close the branch.
                        branches.remove(branch_index);
                        break;
                    }

                    if seen_on_other_side {
                        println!("===Workspace.collect_until_common_ancestor(): collect_until_common_ancestor 2.2 SEEN ON OTHER SIDE");

                        //Add the diff to both searches if it is not there
                        if !search.found_ancestors.borrow().contains(&current_hash) {
                            search.found_ancestors.get_mut().push(current_hash.clone());
                        };
                        if !other.found_ancestors.borrow().contains(&current_hash) {
                            searches
                                .get_mut(&other_side(&side))
                                .ok_or(SocialContextError::InternalError(
                                    "other search side not found",
                                ))?
                                .found_ancestors
                                .get_mut()
                                .push(current_hash.clone());
                        };
                        if self.diffs.get(&current_hash).is_none() && current_hash != NULL_NODE() {
                            let current_diff =
                                Self::get_p_diff_reference::<Retriever>(current_hash.clone())?;
                            self.diffs
                                .insert(current_hash.clone(), current_diff.clone());
                        };
                        // current hash is already in, so it must be our common ancestor!
                        common_ancestor = Some(current_hash);
                        break;
                    }

                    //println!("===Workspace.collect_until_common_ancestor(): collect_until_common_ancestor 2.3");
                    search.found_ancestors.get_mut().push(current_hash.clone());

                    if current_hash == NULL_NODE() {
                        branches.remove(branch_index);
                        search.reached_end = true;
                        if common_ancestor.is_none() && other.reached_end == true {
                            common_ancestor = Some(NULL_NODE());
                            self.terminate_with_null_node(current_hash, side, &mut searches)?;
                        };

                        break;
                    }

                    //TODO; this should have caching builtin, since on some iterations we will get the same P reference multiple times
                    let current_diff =
                        Self::get_p_diff_reference::<Retriever>(current_hash.clone())?;
                    self.diffs
                        .insert(current_hash.clone(), current_diff.clone());

                    match &current_diff.parents {
                        None => {
                            // We arrived at a leaf/orphan (no parents).
                            // So we can close this branch and potentially continue
                            // with other unprocessed branches, if they exist.
                            println!("===Workspace.collect_until_common_ancestor(): collect_until_common_ancestor 2.4, no more parents");
                            branches.remove(branch_index);
                            //If there are no more branches and we have truly reached the end
                            search.reached_end = true;
                            //NOTE: this if block is the code that breaks the test_latest_join tests, with it removed the tests pass, but test three null parents fails
                            if common_ancestor.is_none() && other.reached_end == true {
                                common_ancestor = Some(NULL_NODE());
                                self.terminate_with_null_node(current_hash, side, &mut searches)?;
                            };
                            // We have to break out of loop to avoid having branch_index run out of bounds
                            break;
                        }
                        Some(parents) => {
                            // println!("===Workspace.collect_until_common_ancestor(): collect_until_common_ancestor 2.4, more parents: {:#?}", parents);
                            for parent_index in 0..parents.len() {
                                println!("===Workspace.collect_until_common_ancestor(): collect_until_common_ancestor 2.5, more parents after filter");
                                let parent = parents[parent_index].clone();
                                if let Some(links) = self.back_links.get_mut(&parent) {
                                    links.insert(current_hash.clone());
                                } else {
                                    let mut set = BTreeSet::new();
                                    set.insert(current_hash.clone());
                                    self.back_links.insert(parent.clone(), set);
                                }
                                // The first parent is taken as the successor for the current branch.
                                // If there are multiple parents (i.e. merge commit), we create a new branch..
                                if parent_index == 0 {
                                    println!("Adding new parent to existing branch index");
                                    let _ = std::mem::replace(
                                        &mut branches[branch_index],
                                        parent.clone(),
                                    );
                                } else {
                                    let already_visited =
                                        search.found_ancestors.borrow().contains(&parent)
                                            || other.bfs_branches.borrow().contains(&parent);
                                    let seen_on_other_side =
                                        other.found_ancestors.borrow().contains(&parent);
                                    if !already_visited && !seen_on_other_side {
                                        println!("===Workspace.collect_until_common_ancestor(): Adding a new branch");
                                        branches.push(parent.clone())
                                    }
                                }
                            }
                        }
                    };

                    //println!("===Workspace.collect_until_common_ancestor(): collect_until_common_ancestor 2.7");
                }
            }
        }

        let fn_end = get_now()?.time();
        let ms_spent = (fn_end - fn_start).num_milliseconds();
        if ms_spent > 1000 {
            debug!("===Workspace.collect_until_common_ancestor() - Profiling: Took: {} to complete collect_until_common_ancestor() function", ms_spent);
        }

        if common_ancestor.is_none() {
            return Err(SocialContextError::NoCommonAncestorFound);
        };

        // println!("===Workspace.collect_until_common_ancestor(): collect_until_common_ancestor 3: {:#?} and common ancestor is: {:#?}", searches, hash_to_node_id(common_ancestor.clone().unwrap()));

        Ok(common_ancestor.unwrap())
    }

    // pub fn topo_sort_graph(&mut self) -> SocialContextResult<()> {
    //     debug!("===Workspace.topo_sort_graph(): Function start");
    //     let fn_start = get_now()?.time();

    //     let entry_vec = self.entry_map
    //         .clone()
    //         .into_iter()
    //         .collect::<Vec<(Hash, PerspectiveDiffEntryReference)>>();

    //     let mut dot = Vec::<String>::new();

    //     dot.push("digraph {".to_string());
    //     for entry in entry_vec.iter() {
    //         dot.push(format!("{}", entry.0.clone()));
    //         if let Some(parents) = &entry.1.parents {
    //             for p in parents.iter() {
    //                 dot.push(format!("{} -> {}", entry.0, p));
    //             }
    //         }
    //     }
    //     dot.push("}".to_string());

    //     println!("{}", dot.join("\n"));

    //     self.sorted_diffs = Some(topo_sort_diff_references(&entry_vec)?);

    //     let fn_end = get_now()?.time();
    //     debug!("===Workspace.topo_sort_graph() - Profiling: Took: {} to complete topo_sort_graph() function", (fn_end - fn_start).num_milliseconds());
    //     Ok(())
    // }

    pub fn build_graph(&mut self) -> SocialContextResult<()> {
        debug!("===Workspace.build_graph(): Function start");
        let fn_start = get_now()?.time();

        match self.sorted_diffs.clone() {
            None => Err(SocialContextError::InternalError(
                "Need to 1. collect diffs and then 2. sort them before building the graph",
            )),
            Some(sorted_diffs) => {
                //Add root node
                if self.get_node_index(&NULL_NODE()).is_none() {
                    self.add_node(None, NULL_NODE());
                };

                for diff in sorted_diffs {
                    if diff.0 != NULL_NODE() {
                        if diff.1.parents.is_some() {
                            let mut parents = vec![];
                            for parent in diff.1.parents.as_ref().unwrap() {
                                let parent = self.get_node_index(&parent).ok_or(
                                    SocialContextError::InternalError("Did not find parent"),
                                )?;
                                parents.push(parent.clone());
                            }
                            self.add_node(Some(parents), diff.0.clone());
                        } else {
                            self.add_node(Some(vec![NodeIndex::from(0)]), diff.0.clone());
                        }
                    }
                }

                let fn_end = get_now()?.time();
                debug!("===Workspace.build_graph() - Profiling: Took: {} to complete build_graph() function", (fn_end - fn_start).num_milliseconds());

                Ok(())
            }
        }
    }

    pub fn get_p_diff_reference<Retriever: PerspectiveDiffRetreiver>(
        address: Hash,
    ) -> SocialContextResult<PerspectiveDiffEntryReference> {
        Retriever::get(address)
    }

    fn get_snapshot(
        address: PerspectiveDiffEntryReference,
    ) -> SocialContextResult<Option<Snapshot>> {
        debug!("===Workspace.get_snapshot(): Function start");
        let fn_start = get_now()?.time();

        let input = GetLinksInputBuilder::try_new(
            hash_entry(address)?,
            LinkTypes::Snapshot
        )
        .unwrap()
        .tag_prefix(LinkTag::new("snapshot"))
        .build();
        let mut snapshot_links = get_links(input)?;

        if snapshot_links.len() > 0 {
            let snapshot = get(
                snapshot_links
                    .remove(0)
                    .target
                    .into_entry_hash()
                    .expect("Could not get entry hash"),
                GetOptions::latest(),
            )?
            .ok_or(SocialContextError::InternalError(
                "Workspace::get_snapshot: Could not find entry while populating search",
            ))?
            .entry()
            .to_app_option::<Snapshot>()?
            .ok_or(SocialContextError::InternalError(
                "Expected element to contain app entry data",
            ))?;

            let fn_end = get_now()?.time();
            debug!("===Workspace.get_snapshot() - Profiling: Took: {} to complete get_snapshot() function", (fn_end - fn_start).num_milliseconds());

            Ok(Some(snapshot))
        } else {
            let fn_end = get_now()?.time();
            debug!("===Workspace.get_snapshot() - Profiling: Took: {} to complete get_snapshot() function", (fn_end - fn_start).num_milliseconds());

            Ok(None)
        }
    }

    fn add_node(&mut self, parents: Option<Vec<NodeIndex<u32>>>, diff: Hash) -> NodeIndex<u32> {
        let index = self.graph.add_node(diff.clone());
        self.undirected_graph.add_node(diff.clone());
        self.node_index_map.insert(diff, index);
        if parents.is_some() {
            for parent in parents.unwrap() {
                self.graph.add_edge(index, parent, ());
                self.undirected_graph.add_edge(index, parent, ());
            }
        }
        index
    }

    pub fn get_node_index(&self, node: &Hash) -> Option<&NodeIndex<u32>> {
        self.node_index_map.get(node)
    }

    // pub fn get_paths(
    //     &self,
    //     child: &Hash,
    //     ancestor: &Hash,
    // ) -> SocialContextResult<Vec<Vec<NodeIndex>>> {
    //     debug!("===Workspace.get_paths(): Function start");
    //     let fn_start = get_now()?.time();

    //     let child_node = self.get_node_index(child).expect("Could not get child node index");
    //     let ancestor_node = self.get_node_index(ancestor).expect("Could not get ancestor node index");
    //     let paths = all_simple_paths::<Vec<_>, _>(&self.graph, *child_node, *ancestor_node, 0, None)
    //         .collect::<Vec<_>>();

    //     let fn_end = get_now()?.time();
    //     debug!("===Workspace.get_paths() - Profiling: Took: {} to complete get_paths() function", (fn_end - fn_start).num_milliseconds());

    //     Ok(paths)
    // }

    pub fn _find_common_ancestor(
        &self,
        root: NodeIndex<u32>,
        second: NodeIndex<u32>,
    ) -> Option<NodeIndex> {
        let imm = simple_fast(&self.undirected_graph, root);
        let imm = imm.dominators(second);
        let mut index: Option<NodeIndex> = None;
        match imm {
            Some(imm_iter) => {
                for dom in imm_iter {
                    match index {
                        Some(current_index) => {
                            if current_index.index() > dom.index() {
                                index = Some(dom)
                            }
                        }
                        None => index = Some(dom),
                    };
                }
            }
            None => {}
        };
        index
    }

    pub fn squashed_diff<Retriever: PerspectiveDiffRetreiver>(
        &self,
    ) -> SocialContextResult<PerspectiveDiff> {
        debug!("===Workspace.squashed_diff(): Function start");
        let fn_start = get_now()?.time();

        let mut out = PerspectiveDiff {
            additions: vec![],
            removals: vec![],
        };
        for (_key, value) in self.entry_map.iter() {
            if value.diff == NULL_NODE() {
                continue;
            }
            let diff_entry = Retriever::get::<PerspectiveDiff>(value.diff.clone())?;
            out.additions.append(&mut diff_entry.additions.clone());
            out.removals.append(&mut diff_entry.removals.clone());
        }

        let fn_end = get_now()?.time();
        debug!("===Workspace.squashed_diff() - Profiling: Took: {} to complete squashed_diff() function", (fn_end - fn_start).num_milliseconds());

        Ok(out)
    }

    // pub fn squashed_fast_forward_from(&self, base: Hash) -> SocialContextResult<PerspectiveDiff> {
    //     match &self.sorted_diffs {
    //         None => Err(SocialContextError::InternalError("Need to sort first for this fast-forward optimzed squash")),
    //         Some(sorted_diffs) => {
    //             let mut base_found = false;
    //             let mut out = PerspectiveDiff {
    //                 additions: vec![],
    //                 removals: vec![],
    //             };
    //             for i in 0..sorted_diffs.len() {
    //                 let current = &sorted_diffs[i];
    //                 if !base_found {
    //                     if current.0 == base {
    //                         base_found = true;
    //                     }
    //                 } else {
    //                     let diff_entry = get(current.1.diff.clone(), GetOptions::latest())?
    //                         .ok_or(SocialContextError::InternalError(
    //                             "Could not find diff entry for given diff entry reference",
    //                         ))?
    //                         .entry()
    //                         .to_app_option::<PerspectiveDiff>()?
    //                         .ok_or(SocialContextError::InternalError(
    //                             "Expected element to contain app entry data",
    //                         ))?;
    //                     out.additions.append(&mut diff_entry.additions.clone());
    //                     out.removals.append(&mut diff_entry.removals.clone());
    //                 }
    //             }
    //             Ok(out)
    //         }
    //     }
    // }

    pub fn print_graph_debug(&self) {
        if cfg!(test) {
            println!(
                "Directed: {:?}\n",
                Dot::with_config(
                    &self.graph.map(
                        |_node_index, node| { crate::retriever::hash_to_node_id(node.to_owned()) },
                        |_edge_index, _edge| {}
                    ),
                    &[]
                )
            );
            println!(
                "Undirected: {:?}\n",
                Dot::with_config(
                    &self.undirected_graph.map(
                        |_node_index, node| { crate::retriever::hash_to_node_id(node.to_owned()) },
                        |_edge_index, _edge| {}
                    ),
                    &[]
                )
            );
        } else {
            debug!(
                "Directed: {:?}\n",
                Dot::with_config(&self.graph, &[Config::NodeIndexLabel])
            );
            debug!(
                "Undirected: {:?}\n",
                Dot::with_config(&self.undirected_graph, &[])
            );
        }
    }

    pub fn all_ancestors(&self, child: &Hash) -> SocialContextResult<Vec<Hash>> {
        debug!("===Workspace.all_ancestors(): Function start");
        let fn_start = get_now()?.time();

        let child_node = self
            .get_node_index(child)
            .expect("Could not get child node index");
        let mut ancestors = vec![];
        let mut visited = HashSet::new();
        let mut stack = vec![*child_node];
        while !stack.is_empty() {
            let current = stack.pop().unwrap();
            if visited.contains(&current) {
                continue;
            }
            visited.insert(current);
            let mut parents = self
                .graph
                .neighbors_directed(current, petgraph::Direction::Outgoing);
            while let Some(parent) = parents.next() {
                stack.push(parent);
            }
            ancestors.push(self.graph.node_weight(current).unwrap().to_owned());
        }

        let fn_end = get_now()?.time();
        debug!("===Workspace.all_ancestors() - Profiling: Took: {} to complete all_ancestors() function", (fn_end - fn_start).num_milliseconds());

        Ok(ancestors)
    }
}

#[cfg(test)]
mod tests {
    use super::NULL_NODE;
    use crate::link_adapter::workspace::Workspace;
    use crate::retriever::{node_id_hash, MockPerspectiveGraph, GLOBAL_MOCKED_GRAPH};
    use dot_structures;

    #[test]
    fn test_collect_until_common_ancestor_forked() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                "digraph {
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
            }",
            )
            .unwrap();
        }
        update();

        let node_1 = node_id_hash(&dot_structures::Id::Plain(String::from("1")));
        let node_6 = node_id_hash(&dot_structures::Id::Plain(String::from("6")));
        let node_12 = node_id_hash(&dot_structures::Id::Plain(String::from("12")));

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockPerspectiveGraph>(node_12.clone(), node_6.clone());
        assert!(res.is_ok());

        assert!(workspace.common_ancestors.len() == 1);
        assert_eq!(workspace.common_ancestors.first().unwrap(), &node_1);

        assert_eq!(workspace.entry_map.len(), 12);

        let node_2 = node_id_hash(&dot_structures::Id::Plain(String::from("2")));
        let node_3 = node_id_hash(&dot_structures::Id::Plain(String::from("3")));
        let node_4 = node_id_hash(&dot_structures::Id::Plain(String::from("4")));
        let node_5 = node_id_hash(&dot_structures::Id::Plain(String::from("5")));
        let node_7 = node_id_hash(&dot_structures::Id::Plain(String::from("7")));
        let node_8 = node_id_hash(&dot_structures::Id::Plain(String::from("8")));
        let node_9 = node_id_hash(&dot_structures::Id::Plain(String::from("9")));
        let node_10 = node_id_hash(&dot_structures::Id::Plain(String::from("10")));
        let node_11 = node_id_hash(&dot_structures::Id::Plain(String::from("11")));

        assert!(workspace.entry_map.get(&node_1).is_some());
        assert!(workspace.entry_map.get(&node_2).is_some());
        assert!(workspace.entry_map.get(&node_3).is_some());
        assert!(workspace.entry_map.get(&node_4).is_some());
        assert!(workspace.entry_map.get(&node_5).is_some());
        assert!(workspace.entry_map.get(&node_6).is_some());
        assert!(workspace.entry_map.get(&node_7).is_some());
        assert!(workspace.entry_map.get(&node_8).is_some());
        assert!(workspace.entry_map.get(&node_9).is_some());
        assert!(workspace.entry_map.get(&node_10).is_some());
        assert!(workspace.entry_map.get(&node_11).is_some());
        assert!(workspace.entry_map.get(&node_12).is_some());
    }

    #[test]
    fn test_collect_until_common_ancestor_forward_to_merge_commit() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                "digraph {
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
                13 [ label = \"12\" ]

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
                12 -> 6  [ label = \"()\" ]

                13 -> 12 [ label = \"()\" ]
                
            }",
            )
            .unwrap();
        }
        update();

        let node_1 = node_id_hash(&dot_structures::Id::Plain(String::from("1")));
        let node_6 = node_id_hash(&dot_structures::Id::Plain(String::from("6")));
        let node_12 = node_id_hash(&dot_structures::Id::Plain(String::from("12")));
        let node_13 = node_id_hash(&dot_structures::Id::Plain(String::from("13")));

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockPerspectiveGraph>(node_13.clone(), node_6.clone());
        assert!(res.is_ok());

        assert!(workspace.common_ancestors.len() == 1);
        assert_eq!(workspace.common_ancestors.first().unwrap(), &node_1);
        assert_eq!(workspace.entry_map.len(), 13);

        let node_2 = node_id_hash(&dot_structures::Id::Plain(String::from("2")));
        let node_3 = node_id_hash(&dot_structures::Id::Plain(String::from("3")));
        let node_4 = node_id_hash(&dot_structures::Id::Plain(String::from("4")));
        let node_5 = node_id_hash(&dot_structures::Id::Plain(String::from("5")));
        let node_7 = node_id_hash(&dot_structures::Id::Plain(String::from("7")));
        let node_8 = node_id_hash(&dot_structures::Id::Plain(String::from("8")));
        let node_9 = node_id_hash(&dot_structures::Id::Plain(String::from("9")));
        let node_10 = node_id_hash(&dot_structures::Id::Plain(String::from("10")));
        let node_11 = node_id_hash(&dot_structures::Id::Plain(String::from("11")));

        assert!(workspace.entry_map.get(&node_1).is_some());
        assert!(workspace.entry_map.get(&node_2).is_some());
        assert!(workspace.entry_map.get(&node_3).is_some());
        assert!(workspace.entry_map.get(&node_4).is_some());
        assert!(workspace.entry_map.get(&node_5).is_some());
        assert!(workspace.entry_map.get(&node_6).is_some());
        assert!(workspace.entry_map.get(&node_7).is_some());
        assert!(workspace.entry_map.get(&node_8).is_some());
        assert!(workspace.entry_map.get(&node_9).is_some());
        assert!(workspace.entry_map.get(&node_10).is_some());
        assert!(workspace.entry_map.get(&node_11).is_some());
        assert!(workspace.entry_map.get(&node_12).is_some());
        assert!(workspace.entry_map.get(&node_13).is_some());
    }

    #[test]
    fn test_collect_until_common_ancestor_multi_fork() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                r#"digraph {
                0 [ label = "0" ]
                1 [ label = "1" ]
                2 [ label = "2" ]
                3 [ label = "3" ]
                4 [ label = "4" ]
                5 [ label = "5" ]

                1 -> 0 [ label = "()" ]
                2 -> 1 [ label = "()" ]

                3 -> 0 [ label = "()" ]

                4 -> 0 [ label = "()" ]
                5 -> 4 [ label = "()" ]
            }"#,
            )
            .unwrap();
        }
        update();

        let node_0 = node_id_hash(&dot_structures::Id::Plain(String::from("0")));
        let node_1 = node_id_hash(&dot_structures::Id::Plain(String::from("1")));
        let node_2 = node_id_hash(&dot_structures::Id::Plain(String::from("2")));
        let node_3 = node_id_hash(&dot_structures::Id::Plain(String::from("3")));
        //let node_4 = node_id_hash(&dot_structures::Id::Plain(String::from("4")));
        //let node_5 = node_id_hash(&dot_structures::Id::Plain(String::from("5")));

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockPerspectiveGraph>(node_3.clone(), node_2.clone());
        assert!(res.is_ok());

        assert!(workspace.common_ancestors.len() == 1);
        assert_eq!(workspace.common_ancestors.first().unwrap(), &node_0);
        assert_eq!(workspace.entry_map.len(), 4);

        assert!(workspace.entry_map.get(&node_0).is_some());
        assert!(workspace.entry_map.get(&node_1).is_some());
        assert!(workspace.entry_map.get(&node_2).is_some());
        assert!(workspace.entry_map.get(&node_3).is_some());
    }

    #[test]
    fn test_collect_until_common_ancestor_fork_on_top_of_merge() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                r#"digraph {
                0 [ label = "0" ]
                1 [ label = "1" ]
                2 [ label = "2" ]
                3 [ label = "3" ]
                4 [ label = "4" ]
                5 [ label = "5" ]

                1 -> 0 
                2 -> 1 
                3 -> 2 
                4 -> 3 
                5 -> 4
                
                6
                7
                8
                9
                10

                7 -> 6
                8 -> 7
                9 -> 8
                10 -> 9

                8 -> 0
                
            }"#,
            )
            .unwrap();
        }
        update();

        let node_0 = node_id_hash(&dot_structures::Id::Plain(String::from("0")));
        let node_1 = node_id_hash(&dot_structures::Id::Plain(String::from("1")));
        let node_2 = node_id_hash(&dot_structures::Id::Plain(String::from("2")));
        let node_3 = node_id_hash(&dot_structures::Id::Plain(String::from("3")));
        let node_4 = node_id_hash(&dot_structures::Id::Plain(String::from("4")));
        let node_5 = node_id_hash(&dot_structures::Id::Plain(String::from("5")));
        //let node_6 = node_id_hash(&dot_structures::Id::Plain(String::from("6")));
        //let node_7 = node_id_hash(&dot_structures::Id::Plain(String::from("7")));
        let node_8 = node_id_hash(&dot_structures::Id::Plain(String::from("8")));
        let node_9 = node_id_hash(&dot_structures::Id::Plain(String::from("9")));
        let node_10 = node_id_hash(&dot_structures::Id::Plain(String::from("10")));

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockPerspectiveGraph>(node_5.clone(), node_10.clone());
        println!("Got result: {:#?}", res);
        assert!(res.is_ok());

        assert!(workspace.common_ancestors.len() == 2);
        assert_eq!(workspace.common_ancestors.first().unwrap(), &node_0);
        assert_eq!(workspace.common_ancestors.last().unwrap(), &NULL_NODE());
        assert_eq!(workspace.entry_map.len(), 12);

        assert!(workspace.entry_map.get(&node_0).is_some());
        assert!(workspace.entry_map.get(&node_1).is_some());
        assert!(workspace.entry_map.get(&node_2).is_some());
        assert!(workspace.entry_map.get(&node_3).is_some());
        assert!(workspace.entry_map.get(&node_4).is_some());
        assert!(workspace.entry_map.get(&node_5).is_some());
        //assert!(workspace.entry_map.get(&node_7).is_some());
        assert!(workspace.entry_map.get(&node_8).is_some());
        assert!(workspace.entry_map.get(&node_9).is_some());
        assert!(workspace.entry_map.get(&node_10).is_some());
    }

    #[test]
    fn test_collect_until_common_ancestor_unconnected_fork() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                r#"digraph {
                0 [ label = "0" ]
                1 [ label = "1" ]
                2 [ label = "2" ]
                3 [ label = "3" ]
                4 [ label = "4" ]
                5 [ label = "5" ]

                1 -> 0 
                2 -> 1 
                3 -> 2 
                4 -> 3 
                5 -> 4
                
                6
                7
                8
                9
                10

                7 -> 6
                8 -> 7
                9 -> 8
                10 -> 9
            }"#,
            )
            .unwrap();
        }
        update();

        let node_0 = node_id_hash(&dot_structures::Id::Plain(String::from("0")));
        let node_1 = node_id_hash(&dot_structures::Id::Plain(String::from("1")));
        let node_2 = node_id_hash(&dot_structures::Id::Plain(String::from("2")));
        let node_3 = node_id_hash(&dot_structures::Id::Plain(String::from("3")));
        let node_4 = node_id_hash(&dot_structures::Id::Plain(String::from("4")));
        let node_5 = node_id_hash(&dot_structures::Id::Plain(String::from("5")));
        let node_6 = node_id_hash(&dot_structures::Id::Plain(String::from("6")));
        let node_7 = node_id_hash(&dot_structures::Id::Plain(String::from("7")));
        let node_8 = node_id_hash(&dot_structures::Id::Plain(String::from("8")));
        let node_9 = node_id_hash(&dot_structures::Id::Plain(String::from("9")));
        let node_10 = node_id_hash(&dot_structures::Id::Plain(String::from("10")));

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockPerspectiveGraph>(node_5.clone(), node_10.clone());
        println!("Got result: {:#?}", res);
        assert!(res.is_ok());

        assert!(workspace.common_ancestors.len() == 1);
        assert_eq!(workspace.common_ancestors.first().unwrap(), &NULL_NODE());
        assert_eq!(workspace.entry_map.len(), 12);

        assert!(workspace.entry_map.get(&NULL_NODE()).is_some());
        assert!(workspace.entry_map.get(&node_0).is_some());
        assert!(workspace.entry_map.get(&node_1).is_some());
        assert!(workspace.entry_map.get(&node_2).is_some());
        assert!(workspace.entry_map.get(&node_3).is_some());
        assert!(workspace.entry_map.get(&node_4).is_some());
        assert!(workspace.entry_map.get(&node_5).is_some());
        assert!(workspace.entry_map.get(&node_6).is_some());
        assert!(workspace.entry_map.get(&node_7).is_some());
        assert!(workspace.entry_map.get(&node_8).is_some());
        assert!(workspace.entry_map.get(&node_9).is_some());
        assert!(workspace.entry_map.get(&node_10).is_some());
    }

    #[test]
    fn test_collect_until_common_ancestor_ff_to_merge() {
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

        let node_0 = node_id_hash(&dot_structures::Id::Plain(String::from("0")));
        let node_1 = node_id_hash(&dot_structures::Id::Plain(String::from("1")));
        let node_2 = node_id_hash(&dot_structures::Id::Plain(String::from("2")));
        let node_3 = node_id_hash(&dot_structures::Id::Plain(String::from("3")));

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockPerspectiveGraph>(node_1.clone(), node_3.clone());
        println!("Got result: {:#?}", res);
        assert!(res.is_ok());

        println!("common ancestors: {:?}", workspace.common_ancestors);
        assert_eq!(workspace.common_ancestors.len(), 2);
        assert_eq!(workspace.common_ancestors.first().unwrap(), &node_1);
        assert_eq!(workspace.common_ancestors.last().unwrap(), &node_0);
        assert_eq!(workspace.entry_map.len(), 4);

        assert!(workspace.entry_map.get(&node_0).is_some());
        assert!(workspace.entry_map.get(&node_1).is_some());
        assert!(workspace.entry_map.get(&node_2).is_some());
        assert!(workspace.entry_map.get(&node_3).is_some());
    }

    #[test]
    fn test_collect_until_common_ancestor_complex_merge() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                r#"digraph {
                0 [ label = "0" ]
                1 [ label = "1" ]
                2 [ label = "2" ]
                3 [ label = "3" ]
                4 [ label = "4" ]
                5 [ label = "5" ]
                6 [ label = "6" ]
                1 -> 0 [ label = "()" ]
                2 -> 0 [ label = "()" ]
                3 -> 0 [ label = "()" ]
                4 -> 2 [ label = "()" ]
                5 -> 4 [ label = "()" ]
                5 -> 3 [ label = "()" ]
                6 -> 5 [ label = "()" ]
            }"#,
            )
            .unwrap();
        }
        update();

        let node_0 = node_id_hash(&dot_structures::Id::Plain(String::from("0")));
        let node_1 = node_id_hash(&dot_structures::Id::Plain(String::from("1")));
        let node_2 = node_id_hash(&dot_structures::Id::Plain(String::from("2")));
        let node_3 = node_id_hash(&dot_structures::Id::Plain(String::from("3")));
        let node_4 = node_id_hash(&dot_structures::Id::Plain(String::from("4")));
        let node_5 = node_id_hash(&dot_structures::Id::Plain(String::from("5")));
        let node_6 = node_id_hash(&dot_structures::Id::Plain(String::from("6")));

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockPerspectiveGraph>(node_1.clone(), node_6.clone());
        println!("Got result: {:#?}", res);
        assert!(res.is_ok());

        println!("common ancestors: {:?}", workspace.common_ancestors);
        assert_eq!(workspace.common_ancestors.len(), 1);
        assert_eq!(workspace.common_ancestors.last().unwrap(), &node_0);
        assert_eq!(workspace.entry_map.len(), 7);

        assert!(workspace.entry_map.get(&node_0).is_some());
        assert!(workspace.entry_map.get(&node_1).is_some());
        assert!(workspace.entry_map.get(&node_2).is_some());
        assert!(workspace.entry_map.get(&node_3).is_some());
        assert!(workspace.entry_map.get(&node_4).is_some());
        assert!(workspace.entry_map.get(&node_5).is_some());
        assert!(workspace.entry_map.get(&node_6).is_some());
    }

    #[test]
    fn test_collect_until_common_ancestor_complex_merge_implicit_zero() {
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

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockPerspectiveGraph>(node_1.clone(), node_6.clone());
        println!("Got result: {:#?}", res);
        assert!(res.is_ok());
        assert_eq!(workspace.common_ancestors.len(), 2);
        assert_eq!(workspace.common_ancestors.last().unwrap(), &NULL_NODE());
    }

    #[test]
    fn real_world_graph() {
        fn update() {
            let mut graph = GLOBAL_MOCKED_GRAPH.lock().unwrap();
            *graph = MockPerspectiveGraph::from_dot(
                r#"digraph {
                0 [ label = "0" ]
                1 [ label = "1" ]
                2 [ label = "2" ]
                3 [ label = "3" ]
                4 [ label = "4" ]
                5 [ label = "5" ]
                6 [ label = "6" ]
                7 [ label = "7" ]
                8 [ label = "8" ]
                9 [ label = "9" ]
                10 [ label = "10" ]
                11 [ label = "11" ]
                12 [ label = "12" ]
                13 [ label = "13" ]
                14 [ label = "14" ]
                15 [ label = "15" ]
                16 [ label = "16" ]
                1 -> 0 [ label = "()" ]
                2 -> 1 [ label = "()" ]
                3 -> 2 [ label = "()" ]
                4 -> 3 [ label = "()" ]
                5 -> 4 [ label = "()" ]
                6 -> 5 [ label = "()" ]
                7 -> 6 [ label = "()" ]
                8 -> 7 [ label = "()" ]
                9 -> 8 [ label = "()" ]
                10 -> 9 [ label = "()" ]
                11 -> 1 [ label = "()" ]
                12 -> 2 [ label = "()" ]
                12 -> 11 [ label = "()" ]
                13 -> 3 [ label = "()" ]
                13 -> 12 [ label = "()" ]
                14 -> 6 [ label = "()" ]
                14 -> 13 [ label = "()" ]
                15 -> 7 [ label = "()" ]
                15 -> 14 [ label = "()" ]
                16 -> 8 [ label = "()" ]
                16 -> 15 [ label = "()" ]
            }"#,
            )
            .unwrap();
        }
        update();

        let node_10 = node_id_hash(&dot_structures::Id::Plain(String::from("10")));
        let node_16 = node_id_hash(&dot_structures::Id::Plain(String::from("16")));
        let node_8 = node_id_hash(&dot_structures::Id::Plain(String::from("8")));

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockPerspectiveGraph>(node_16.clone(), node_10.clone());
        assert!(res.is_ok());
        assert_eq!(workspace.common_ancestors.len(), 6);
        assert_eq!(workspace.common_ancestors.first().unwrap(), &node_8);
        println!("Got result: {:#?}", res);
    }
}
