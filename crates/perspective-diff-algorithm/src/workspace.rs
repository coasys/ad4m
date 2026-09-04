//! Substrate-agnostic DAG `Workspace` builder.
//!
//! Originally lived in
//! `bootstrap-languages/p-diff-sync/hc-dna/zomes/perspective_diff_sync/src/link_adapter/workspace.rs`,
//! parameterized concretely on `HoloHash<Action>` + the integrity-zome
//! `PerspectiveDiffEntryReference` + HDK lookup calls.
//!
//! Step 13b-C (the wide extraction Nico asked for in the wake-13 audio
//! note): the Workspace struct + every algorithm method moves here,
//! generic over the algorithm-crate mirror types (`Hash`,
//! `PerspectiveDiffEntryReference`, `Snapshot`) and the
//! [`WorkspaceRetriever`] trait. p-diff-sync's
//! `link_adapter::workspace` becomes a thin re-export shim plus the
//! HDK impl of `WorkspaceRetriever`.

use std::collections::{BTreeMap, BTreeSet, HashSet, VecDeque};

use petgraph::{
    algo::dominators::simple_fast,
    graph::{DiGraph, Graph, NodeIndex, UnGraph},
};

use crate::chunked_diffs::load_diff_aggregated;
use crate::errors::{AlgoError, AlgoResult};
use crate::retriever::WorkspaceRetriever;
use crate::topo_sort::topo_sort_diff_references;
use perspective_diff_types::{null_node, Hash, PerspectiveDiff, PerspectiveDiffEntryReference};

#[derive(Debug)]
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

#[derive(Clone, Debug)]
struct BfsSearch {
    pub found_ancestors: std::cell::RefCell<Vec<Hash>>,
    pub bfs_branches: std::cell::RefCell<Vec<Hash>>,
    pub reached_end: bool,
}

impl BfsSearch {
    fn new(start: Hash) -> BfsSearch {
        let branches = std::cell::RefCell::new(Vec::from([start]));
        BfsSearch {
            found_ancestors: std::cell::RefCell::new(Vec::new()),
            bfs_branches: branches,
            reached_end: false,
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

impl Default for Workspace {
    fn default() -> Self {
        Self::new()
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

    /// Single-hash variant of the workspace builder — walk back from
    /// `latest` to the first snapshot / orphan and populate `entry_map`.
    /// Used by `render` (we don't have to detect forks).
    pub fn collect_only_from_latest<R: WorkspaceRetriever>(
        &mut self,
        latest: Hash,
    ) -> AlgoResult<()> {
        let mut unprocessed_branches = VecDeque::new();
        unprocessed_branches.push_back(latest);

        let mut snapshot_seen: Vec<Hash> = vec![];

        while !unprocessed_branches.is_empty() {
            let current_hash = unprocessed_branches[0].clone();

            if self.entry_map.contains_key(&current_hash) && !snapshot_seen.contains(&current_hash)
            {
                unprocessed_branches.pop_front();
                continue;
            }

            let current_diff = Self::get_p_diff_reference::<R>(current_hash.clone())?;

            if current_diff.diffs_since_snapshot == 0 {
                let snapshot = R::get_snapshot_by_target(&current_hash)?;

                if let Some(mut snapshot) = snapshot {
                    // Process chunked diffs from snapshot
                    let mut last_diff = None;
                    for diff_chunk_hash in &snapshot.diff_chunks {
                        let chunked_diff_entry =
                            Self::get_p_diff_reference::<R>(diff_chunk_hash.clone())?;

                        self.entry_map
                            .insert(diff_chunk_hash.clone(), chunked_diff_entry);
                        last_diff = Some(vec![diff_chunk_hash.clone()]);
                    }

                    self.entry_map.insert(
                        current_hash.clone(),
                        PerspectiveDiffEntryReference::new(PerspectiveDiff::new(), last_diff),
                    );

                    snapshot_seen.append(&mut snapshot.included_diffs);
                    unprocessed_branches.pop_front();
                } else {
                    self.handle_parents::<R>(
                        current_diff,
                        current_hash,
                        &mut unprocessed_branches,
                    )?;
                }
            } else {
                self.handle_parents::<R>(current_diff, current_hash, &mut unprocessed_branches)?;
            }
        }

        Ok(())
    }

    fn handle_parents<R: WorkspaceRetriever>(
        &mut self,
        current_diff: PerspectiveDiffEntryReference,
        current_hash: Hash,
        unprocessed_branches: &mut VecDeque<Hash>,
    ) -> AlgoResult<()> {
        if let Some(parents) = &current_diff.parents {
            for i in 0..parents.len() {
                if i == 0 {
                    unprocessed_branches[0] = parents[i].clone();
                } else {
                    unprocessed_branches.push_back(parents[i].clone())
                }
            }
        } else {
            unprocessed_branches.pop_front();
        }

        // Chunked-entry inline-load: if the current entry stores its diff
        // as chunks, materialize them before inserting into entry_map so
        // downstream render() / squashed_diff() sees the full payload.
        let resolved_diff = if current_diff.is_chunked() {
            let loaded = load_diff_aggregated::<R>(&current_diff)?;
            PerspectiveDiffEntryReference {
                diff: loaded,
                parents: current_diff.parents.clone(),
                diffs_since_snapshot: current_diff.diffs_since_snapshot,
                diff_chunks: None,
            }
        } else {
            current_diff
        };

        self.entry_map.insert(current_hash, resolved_diff);
        Ok(())
    }

    pub fn sort_graph(&mut self) -> AlgoResult<()> {
        let common_ancestor = self
            .common_ancestors
            .last()
            .ok_or(AlgoError::Internal("no common ancestor to sort from"))?;

        let mut sorted: Vec<(Hash, PerspectiveDiffEntryReference)> = Vec::new();
        let mut visited: HashSet<Hash> = HashSet::new();
        let mut next: VecDeque<Hash> = VecDeque::new();
        self.unexplored_side_branches = BTreeSet::new();

        next.push_back(common_ancestor.clone());

        while !next.is_empty() {
            let current = next.pop_front().expect("must be Ok since next !is_empty()");
            if !visited.contains(&current) {
                match self.back_links.get(&current) {
                    Some(children) => {
                        for child in children.iter() {
                            let diff = self
                                .diffs
                                .get(child)
                                .ok_or(AlgoError::Internal("child must exist in diffs map"))?;
                            if diff.parents.is_some() {
                                for parent in diff.parents.as_ref().unwrap() {
                                    if parent != &current {
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
                let current_diff = self
                    .diffs
                    .get(&current)
                    .ok_or(AlgoError::Internal("diffs should be populated"))?
                    .clone();
                sorted.push((current.clone(), current_diff.clone()));
                self.entry_map
                    .entry(current.clone())
                    .or_insert(current_diff);
                visited.insert(current);
            }
        }

        self.unexplored_side_branches = self
            .unexplored_side_branches
            .iter()
            .filter(|b| !sorted.iter().any(|s| s.0 == **b))
            .cloned()
            .collect();

        // Dedupe-by-hash without itertools::unique() (algorithm crate
        // stays light on deps): track seen-set, keep first occurrence.
        let mut seen = HashSet::new();
        let deduped: Vec<_> = sorted
            .into_iter()
            .filter(|item| seen.insert(item.0.clone()))
            .collect();
        self.sorted_diffs = Some(deduped);

        Ok(())
    }

    pub fn build_diffs<R: WorkspaceRetriever>(
        &mut self,
        theirs: Hash,
        ours: Hash,
    ) -> AlgoResult<()> {
        let common_ancestor = self.collect_until_common_ancestor::<R>(theirs, ours)?;
        self.common_ancestors.push(common_ancestor);
        self.sort_graph()?;

        while !self.unexplored_side_branches.is_empty() {
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
            let common_ancestor =
                self.collect_until_common_ancestor::<R>(unexplored_side_branch, ours)?;
            self.common_ancestors.push(common_ancestor.clone());
            self.sort_graph()?;
        }

        let sorted_diffs = self
            .sorted_diffs
            .as_mut()
            .ok_or(AlgoError::Internal("sorted_diffs must be Some"))?;
        if let Some(first) = sorted_diffs.get_mut(0) {
            first.1.parents = None;
        }
        self.sorted_diffs = Some(topo_sort_diff_references(sorted_diffs).map_err(AlgoError::from)?);

        self.build_graph()?;

        Ok(())
    }

    fn terminate_with_null_node(
        &mut self,
        current_hash: Hash,
        side: SearchSide,
        searches: &mut BTreeMap<SearchSide, BfsSearch>,
    ) -> AlgoResult<()> {
        let search_clone = searches.clone();
        let other = search_clone
            .get(&other_side(&side))
            .ok_or(AlgoError::Internal("search side not found"))?;
        let search = searches
            .get_mut(&side)
            .ok_or(AlgoError::Internal("search side not found"))?;

        if !search.found_ancestors.borrow().contains(&null_node()) {
            search.found_ancestors.get_mut().push(null_node());
        };
        if !other.found_ancestors.borrow().contains(&null_node()) {
            let other_mut = searches
                .get_mut(&other_side(&side))
                .ok_or(AlgoError::Internal("search side not found"))?;
            other_mut.found_ancestors.get_mut().push(null_node());
        };
        if self.diffs.get(&null_node()).is_none() {
            let current_diff = PerspectiveDiffEntryReference::new(PerspectiveDiff::new(), None);
            self.diffs.insert(null_node(), current_diff);
        };

        let mut set = if let Some(nodes_back_links) = self.back_links.get(&null_node()) {
            let mut nodes_back_links = nodes_back_links.clone();
            if let Some(other_last) = other.found_ancestors.borrow().last() {
                if other_last != &null_node() {
                    nodes_back_links.insert(other_last.clone());
                }
            }
            nodes_back_links.clone()
        } else {
            let mut set = BTreeSet::new();
            if let Some(other_last) = other.found_ancestors.borrow().last() {
                if other_last != &null_node() {
                    set.insert(other_last.clone());
                }
            }
            set
        };
        if current_hash != null_node() {
            set.insert(current_hash);
        };
        self.back_links.insert(null_node(), set);
        Ok(())
    }

    pub fn collect_until_common_ancestor<R: WorkspaceRetriever>(
        &mut self,
        theirs: Hash,
        ours: Hash,
    ) -> AlgoResult<Hash> {
        let mut common_ancestor: Option<Hash> = None;

        let mut searches: BTreeMap<SearchSide, BfsSearch> = BTreeMap::new();
        searches.insert(SearchSide::Theirs, BfsSearch::new(theirs));
        searches.insert(SearchSide::Ours, BfsSearch::new(ours));

        while common_ancestor.is_none() {
            for side in [SearchSide::Theirs, SearchSide::Ours] {
                let search_clone = searches.clone();
                let other = search_clone
                    .get(&other_side(&side))
                    .ok_or(AlgoError::Internal("other search side not found"))?;
                let search = searches
                    .get_mut(&side)
                    .ok_or(AlgoError::Internal("search side not found"))?;
                let branches = search.bfs_branches.get_mut();
                branches.dedup();

                for branch_index in 0..branches.len() {
                    let current_hash = branches[branch_index].clone();

                    let already_visited = search.found_ancestors.borrow().contains(&current_hash);
                    let seen_on_other_side = other.found_ancestors.borrow().contains(&current_hash)
                        || other.bfs_branches.borrow().contains(&current_hash);

                    if already_visited {
                        branches.remove(branch_index);
                        break;
                    }

                    if seen_on_other_side {
                        if !search.found_ancestors.borrow().contains(&current_hash) {
                            search.found_ancestors.get_mut().push(current_hash.clone());
                        };
                        if !other.found_ancestors.borrow().contains(&current_hash) {
                            searches
                                .get_mut(&other_side(&side))
                                .ok_or(AlgoError::Internal("other search side not found"))?
                                .found_ancestors
                                .get_mut()
                                .push(current_hash.clone());
                        };
                        if self.diffs.get(&current_hash).is_none() && current_hash != null_node() {
                            let current_diff =
                                Self::get_p_diff_reference::<R>(current_hash.clone())?;
                            self.diffs
                                .insert(current_hash.clone(), current_diff.clone());
                        };
                        common_ancestor = Some(current_hash);
                        break;
                    }

                    search.found_ancestors.get_mut().push(current_hash.clone());

                    if current_hash == null_node() {
                        branches.remove(branch_index);
                        search.reached_end = true;
                        if common_ancestor.is_none() && other.reached_end {
                            common_ancestor = Some(null_node());
                            self.terminate_with_null_node(current_hash, side, &mut searches)?;
                        };
                        break;
                    }

                    let current_diff = Self::get_p_diff_reference::<R>(current_hash.clone())?;
                    self.diffs
                        .insert(current_hash.clone(), current_diff.clone());

                    match &current_diff.parents {
                        None => {
                            branches.remove(branch_index);
                            search.reached_end = true;
                            if common_ancestor.is_none() && other.reached_end {
                                common_ancestor = Some(null_node());
                                self.terminate_with_null_node(current_hash, side, &mut searches)?;
                            };
                            break;
                        }
                        Some(parents) => {
                            for parent_index in 0..parents.len() {
                                let parent = parents[parent_index].clone();
                                if let Some(links) = self.back_links.get_mut(&parent) {
                                    links.insert(current_hash.clone());
                                } else {
                                    let mut set = BTreeSet::new();
                                    set.insert(current_hash.clone());
                                    self.back_links.insert(parent.clone(), set);
                                }
                                if parent_index == 0 {
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
                                        branches.push(parent.clone())
                                    }
                                }
                            }
                        }
                    };
                }
            }
        }

        common_ancestor.ok_or(AlgoError::NoCommonAncestorFound)
    }

    pub fn build_graph(&mut self) -> AlgoResult<()> {
        let sorted_diffs = self.sorted_diffs.clone().ok_or(AlgoError::Internal(
            "Need to 1. collect diffs and then 2. sort them before building the graph",
        ))?;

        if self.get_node_index(&null_node()).is_none() {
            self.add_node(None, null_node());
        };

        for diff in sorted_diffs {
            if diff.0 != null_node() {
                if diff.1.parents.is_some() {
                    let mut parents = vec![];
                    for parent in diff.1.parents.as_ref().unwrap() {
                        let parent = self
                            .get_node_index(parent)
                            .ok_or(AlgoError::Internal("Did not find parent"))?;
                        parents.push(*parent);
                    }
                    self.add_node(Some(parents), diff.0.clone());
                } else {
                    self.add_node(Some(vec![NodeIndex::from(0)]), diff.0.clone());
                }
            }
        }

        Ok(())
    }

    pub fn get_p_diff_reference<R: WorkspaceRetriever>(
        address: Hash,
    ) -> AlgoResult<PerspectiveDiffEntryReference> {
        R::get_p_diff_reference(&address)
    }

    fn add_node(&mut self, parents: Option<Vec<NodeIndex<u32>>>, diff: Hash) -> NodeIndex<u32> {
        let index = self.graph.add_node(diff.clone());
        self.undirected_graph.add_node(diff.clone());
        self.node_index_map.insert(diff, index);
        if let Some(parents) = parents {
            for parent in parents {
                self.graph.add_edge(index, parent, ());
                self.undirected_graph.add_edge(index, parent, ());
            }
        }
        index
    }

    pub fn get_node_index(&self, node: &Hash) -> Option<&NodeIndex<u32>> {
        self.node_index_map.get(node)
    }

    pub fn find_common_ancestor(
        &self,
        root: NodeIndex<u32>,
        second: NodeIndex<u32>,
    ) -> Option<NodeIndex> {
        let imm = simple_fast(&self.undirected_graph, root);
        let imm = imm.dominators(second);
        let mut index: Option<NodeIndex> = None;
        if let Some(imm_iter) = imm {
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
        };
        index
    }

    pub fn squashed_diff(&self) -> PerspectiveDiff {
        let mut out = PerspectiveDiff {
            additions: vec![],
            removals: vec![],
        };
        for (key, value) in self.entry_map.iter() {
            if key == &null_node() {
                continue;
            }
            out.additions.extend(value.diff.additions.iter().cloned());
            out.removals.extend(value.diff.removals.iter().cloned());
        }
        out
    }

    pub fn all_ancestors(&self, child: &Hash) -> AlgoResult<Vec<Hash>> {
        let child_node = self
            .get_node_index(child)
            .ok_or(AlgoError::Internal("Could not get child node index"))?;
        let mut ancestors = vec![];
        let mut visited = HashSet::new();
        let mut stack = vec![*child_node];
        while let Some(current) = stack.pop() {
            if visited.contains(&current) {
                continue;
            }
            visited.insert(current);
            let mut parents = self
                .graph
                .neighbors_directed(current, petgraph::Direction::Outgoing);
            for parent in &mut parents {
                stack.push(parent);
            }
            ancestors.push(self.graph.node_weight(current).unwrap().to_owned());
        }
        Ok(ancestors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    // ----- In-crate mock retriever for the workspace tests ------------
    //
    // The tests originally lived in p-diff-sync and used
    // `MockPerspectiveGraph` from `crate::retriever::mock`. That mock is
    // HDK-shaped (returns `SocialContextResult<PerspectiveDiffEntryReference>`
    // and operates on `HoloHash<Action>`). We need an equivalent that
    // implements the algorithm-crate's `WorkspaceRetriever` over the
    // mirror `Hash` type.
    //
    // The mock graph is built from a graphviz-style dot description.

    use once_cell::sync::Lazy;

    static MOCK_GRAPH: Lazy<Mutex<MockGraph>> = Lazy::new(|| Mutex::new(MockGraph::default()));

    #[derive(Default, Clone, Debug)]
    struct MockGraph {
        // node_id (string label from dot) -> Hash
        labels: BTreeMap<String, Hash>,
        // Hash -> entry
        entries: BTreeMap<Hash, PerspectiveDiffEntryReference>,
    }

    impl MockGraph {
        fn from_dot(s: &str) -> MockGraph {
            // Minimal dot parser: identify `N [ label = "X" ]` and
            // `A -> B` edges. Sufficient for the workspace tests.
            let mut nodes: Vec<String> = Vec::new();
            let mut edges: Vec<(String, String)> = Vec::new();
            for raw in s.lines() {
                let line = raw.trim();
                if line.is_empty() || line.starts_with("digraph") || line.starts_with("}") {
                    continue;
                }
                // edge: "A -> B [ label = \"()\" ]"
                if let Some(arrow_pos) = line.find("->") {
                    let lhs = line[..arrow_pos].trim();
                    let rest = &line[arrow_pos + 2..];
                    let rhs = rest
                        .split_whitespace()
                        .next()
                        .unwrap_or("")
                        .trim_end_matches([',', ';']);
                    edges.push((lhs.to_string(), rhs.to_string()));
                    // edges also implicitly declare nodes
                    nodes.push(lhs.to_string());
                    nodes.push(rhs.to_string());
                    continue;
                }
                // node: "N [ label = \"X\" ]" — first token is the id
                let first = line.split_whitespace().next().unwrap_or("");
                if !first.is_empty()
                    && first
                        .chars()
                        .next()
                        .map(|c| c.is_ascii_digit())
                        .unwrap_or(false)
                {
                    nodes.push(first.to_string());
                }
            }
            nodes.sort();
            nodes.dedup();

            let mut g = MockGraph::default();
            for n in &nodes {
                g.labels.insert(n.clone(), node_label_to_hash(n));
            }
            // edges A -> B mean A's parent is B.
            let mut parents_map: BTreeMap<String, Vec<String>> = BTreeMap::new();
            for (a, b) in &edges {
                parents_map.entry(a.clone()).or_default().push(b.clone());
            }
            for n in &nodes {
                let hash = g.labels[n].clone();
                let parents = parents_map.get(n).map(|ps| {
                    ps.iter()
                        .filter_map(|p| g.labels.get(p).cloned())
                        .collect::<Vec<_>>()
                });
                let entry = PerspectiveDiffEntryReference {
                    diff: PerspectiveDiff::new(),
                    parents,
                    diffs_since_snapshot: 0,
                    diff_chunks: None,
                };
                g.entries.insert(hash, entry);
            }
            g
        }
    }

    fn node_label_to_hash(label: &str) -> Hash {
        // Map the dot label into a deterministic 36-byte payload so the
        // tests get distinct, reproducible hashes per node.
        let mut buf = [0u8; 36];
        // First 32 bytes: BLAKE-style mixing isn't needed; the label
        // itself encoded is sufficient since labels in the test set are
        // short ascii.
        let bytes = label.as_bytes();
        for (i, &b) in bytes.iter().enumerate().take(32) {
            buf[i] = b;
        }
        // Last 4 bytes: a marker so a label "1" and "11" don't alias.
        buf[32] = bytes.len() as u8;
        Hash::from_raw_36(buf.to_vec())
    }

    struct MockRetriever;

    impl WorkspaceRetriever for MockRetriever {
        fn get_p_diff_reference(hash: &Hash) -> AlgoResult<PerspectiveDiffEntryReference> {
            let g = MOCK_GRAPH.lock().unwrap();
            g.entries
                .get(hash)
                .cloned()
                .ok_or(AlgoError::Retriever(format!(
                    "mock: hash not found {:?}",
                    hash
                )))
        }
        fn get_snapshot_by_target(_target: &Hash) -> AlgoResult<Option<crate::Snapshot>> {
            Ok(None)
        }
    }

    fn load_graph(dot: &str) {
        *MOCK_GRAPH.lock().unwrap() = MockGraph::from_dot(dot);
    }

    fn h(label: &str) -> Hash {
        node_label_to_hash(label)
    }

    #[test]
    fn test_collect_until_common_ancestor_forked() {
        load_graph(
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
            1 -> 0
            2 -> 1
            3 -> 2
            4 -> 3
            5 -> 4
            6 -> 5
            7 -> 1
            8 -> 7
            9 -> 8
            10 -> 9
            11 -> 10
            12 -> 11
        }"#,
        );

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockRetriever>(h("12"), h("6"));
        assert!(res.is_ok(), "{:?}", res);
        assert_eq!(workspace.common_ancestors.len(), 1);
        assert_eq!(workspace.common_ancestors.first().unwrap(), &h("1"));
        assert_eq!(workspace.entry_map.len(), 12);
        for label in [
            "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
        ] {
            assert!(workspace.entry_map.get(&h(label)).is_some(), "{}", label);
        }
    }

    #[test]
    fn test_collect_until_common_ancestor_forward_to_merge_commit() {
        load_graph(
            r#"digraph {
            0 [ label = "0" ]
            1 -> 0
            2 -> 1
            3 -> 2
            4 -> 3
            5 -> 4
            6 -> 5

            7 -> 1
            8 -> 7
            9 -> 8
            10 -> 9
            11 -> 10

            12 -> 11
            12 -> 6

            13 -> 12
        }"#,
        );

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockRetriever>(h("13"), h("6"));
        assert!(res.is_ok());
        assert_eq!(workspace.common_ancestors.len(), 1);
        assert_eq!(workspace.common_ancestors.first().unwrap(), &h("1"));
        assert_eq!(workspace.entry_map.len(), 13);
    }

    #[test]
    fn test_collect_until_common_ancestor_multi_fork() {
        load_graph(
            r#"digraph {
            1 -> 0
            2 -> 1
            3 -> 0
            4 -> 0
            5 -> 4
        }"#,
        );

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockRetriever>(h("3"), h("2"));
        assert!(res.is_ok());
        assert_eq!(workspace.common_ancestors.len(), 1);
        assert_eq!(workspace.common_ancestors.first().unwrap(), &h("0"));
        assert_eq!(workspace.entry_map.len(), 4);
    }

    #[test]
    fn test_collect_until_common_ancestor_unconnected_fork() {
        load_graph(
            r#"digraph {
            1 -> 0
            2 -> 1
            3 -> 2
            4 -> 3
            5 -> 4

            7 -> 6
            8 -> 7
            9 -> 8
            10 -> 9
        }"#,
        );

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockRetriever>(h("5"), h("10"));
        assert!(res.is_ok());
        assert_eq!(workspace.common_ancestors.len(), 1);
        assert_eq!(workspace.common_ancestors.first().unwrap(), &null_node());
        assert_eq!(workspace.entry_map.len(), 12);
        assert!(workspace.entry_map.get(&null_node()).is_some());
    }

    #[test]
    fn test_collect_until_common_ancestor_ff_to_merge() {
        load_graph(
            r#"digraph {
            1 -> 0
            2 -> 0
            3 -> 1
            3 -> 2
        }"#,
        );

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockRetriever>(h("1"), h("3"));
        assert!(res.is_ok());
        assert_eq!(workspace.common_ancestors.len(), 2);
        assert_eq!(workspace.common_ancestors.first().unwrap(), &h("1"));
        assert_eq!(workspace.common_ancestors.last().unwrap(), &h("0"));
        assert_eq!(workspace.entry_map.len(), 4);
    }

    // Ported from p-diff-sync's link_adapter::workspace::tests in wake-15
    // Step 13b-C phase 2 — same `build_diffs` BFS exercised on
    // additional graph shapes.

    #[test]
    fn test_collect_until_common_ancestor_complex_merge() {
        load_graph(
            r#"digraph {
            1 -> 0
            2 -> 0
            3 -> 0
            4 -> 2
            5 -> 4
            5 -> 3
            6 -> 5
        }"#,
        );

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockRetriever>(h("1"), h("6"));
        assert!(res.is_ok());
        assert_eq!(workspace.common_ancestors.len(), 1);
        assert_eq!(workspace.common_ancestors.last().unwrap(), &h("0"));
        assert_eq!(workspace.entry_map.len(), 7);
    }

    #[test]
    fn test_collect_until_common_ancestor_complex_merge_implicit_zero() {
        // Nodes 1, 2, 3 have no parents (orphans). Node 1 isn't on any
        // edge — declare it explicitly so MockGraph::from_dot picks
        // it up.
        load_graph(
            r#"digraph {
            1 [ label = "1" ]
            2 [ label = "2" ]
            3 [ label = "3" ]
            4 -> 2
            5 -> 4
            5 -> 3
            6 -> 5
        }"#,
        );

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockRetriever>(h("1"), h("6"));
        assert!(res.is_ok(), "{:?}", res);
        assert_eq!(workspace.common_ancestors.len(), 2);
        assert_eq!(workspace.common_ancestors.last().unwrap(), &null_node());
    }

    #[test]
    fn real_world_graph() {
        load_graph(
            r#"digraph {
            1 -> 0
            2 -> 1
            3 -> 2
            4 -> 3
            5 -> 4
            6 -> 5
            7 -> 6
            8 -> 7
            9 -> 8
            10 -> 9
            11 -> 1
            12 -> 2
            12 -> 11
            13 -> 3
            13 -> 12
            14 -> 6
            14 -> 13
            15 -> 7
            15 -> 14
            16 -> 8
            16 -> 15
        }"#,
        );

        let mut workspace = Workspace::new();
        let res = workspace.build_diffs::<MockRetriever>(h("16"), h("10"));
        assert!(res.is_ok());
        assert_eq!(workspace.common_ancestors.len(), 6);
        assert_eq!(workspace.common_ancestors.first().unwrap(), &h("8"));
    }
}
