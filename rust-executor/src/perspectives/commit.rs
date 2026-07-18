use crate::types::GraphDiff;
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use std::sync::RwLock;

pub fn canonical_commit_payload(diff: &GraphDiff, parent_iris: &[String]) -> String {
    let mut lines = Vec::new();

    let mut sorted_parents: Vec<&String> = parent_iris.iter().collect();
    sorted_parents.sort();
    for p in sorted_parents {
        lines.push(format!("parent:{p}"));
    }

    let mut sorted_additions: Vec<&String> = diff.additions.iter().collect();
    sorted_additions.sort();
    for a in sorted_additions {
        lines.push(format!("+{a}"));
    }

    let mut sorted_removals: Vec<&String> = diff.removals.iter().collect();
    sorted_removals.sort();
    for r in sorted_removals {
        lines.push(format!("-{r}"));
    }

    lines.join("\n")
}

pub fn commit_hash(diff: &GraphDiff, parent_iris: &[String]) -> String {
    let payload = canonical_commit_payload(diff, parent_iris);
    let mut hasher = Sha256::new();
    hasher.update(payload.as_bytes());
    hex::encode(hasher.finalize())
}

pub fn commit_iri(diff: &GraphDiff, parent_iris: &[String]) -> String {
    format!("graph://{}", commit_hash(diff, parent_iris))
}

pub fn compute_diff(old_triples: &[String], new_triples: &[String]) -> GraphDiff {
    let old_set: HashSet<&String> = old_triples.iter().collect();
    let new_set: HashSet<&String> = new_triples.iter().collect();

    let mut additions: Vec<String> = new_set.difference(&old_set).map(|s| (*s).clone()).collect();
    let mut removals: Vec<String> = old_set.difference(&new_set).map(|s| (*s).clone()).collect();
    additions.sort();
    removals.sort();

    GraphDiff {
        additions,
        removals,
    }
}

struct CachedGraphState {
    commit_iri: String,
    triples: Vec<String>,
}

/// In-memory cache mirroring the per-graph head state that the link language
/// owns authoritatively (SPEC_8 §5).  `get_graph_as_expression` is sync but
/// link-language calls are async, so this cache avoids a context switch on
/// every diff computation.  It is populated after each commit and seeded from
/// the link language on perspective init when an async context is available.
pub struct GraphDiffCache {
    state: RwLock<HashMap<String, CachedGraphState>>,
}

impl GraphDiffCache {
    pub fn new() -> Self {
        Self {
            state: RwLock::new(HashMap::new()),
        }
    }

    pub fn get(&self, subject_base: &str) -> Option<(String, Vec<String>)> {
        self.state
            .read()
            .unwrap()
            .get(subject_base)
            .map(|s| (s.commit_iri.clone(), s.triples.clone()))
    }

    pub fn set(&self, subject_base: &str, commit_iri: String, triples: Vec<String>) {
        self.state.write().unwrap().insert(
            subject_base.to_string(),
            CachedGraphState {
                commit_iri,
                triples,
            },
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_diff() -> GraphDiff {
        GraphDiff {
            additions: vec![
                "<s> <p> \"hello\" .".to_string(),
                "<s> <q> \"world\" .".to_string(),
            ],
            removals: vec![],
        }
    }

    #[test]
    fn genesis_commit_has_no_parents() {
        let diff = sample_diff();
        let payload = canonical_commit_payload(&diff, &[]);
        assert!(!payload.contains("parent:"), "genesis has no parent lines");
        assert!(payload.contains("+<s> <p> \"hello\" ."));
        assert!(payload.contains("+<s> <q> \"world\" ."));
    }

    #[test]
    fn commit_hash_is_deterministic() {
        let diff = sample_diff();
        let parents = vec!["graph://abc123".to_string()];
        let h1 = commit_hash(&diff, &parents);
        let h2 = commit_hash(&diff, &parents);
        assert_eq!(h1, h2, "same inputs must produce same hash");
        assert_eq!(h1.len(), 64, "SHA-256 hex is 64 chars");
    }

    #[test]
    fn different_diffs_produce_different_hashes() {
        let d1 = GraphDiff {
            additions: vec!["<a> <b> <c> .".to_string()],
            removals: vec![],
        };
        let d2 = GraphDiff {
            additions: vec!["<x> <y> <z> .".to_string()],
            removals: vec![],
        };
        assert_ne!(commit_hash(&d1, &[]), commit_hash(&d2, &[]));
    }

    #[test]
    fn different_parents_produce_different_hashes() {
        let diff = sample_diff();
        let h1 = commit_hash(&diff, &["graph://aaa".to_string()]);
        let h2 = commit_hash(&diff, &["graph://bbb".to_string()]);
        assert_ne!(h1, h2);
    }

    #[test]
    fn commit_iri_has_graph_prefix() {
        let diff = sample_diff();
        let iri = commit_iri(&diff, &[]);
        assert!(iri.starts_with("graph://"), "got: {iri}");
        assert_eq!(iri.len(), "graph://".len() + 64);
    }

    #[test]
    fn compute_diff_finds_additions_and_removals() {
        let old = vec!["<a> <b> <c> .".to_string(), "<d> <e> <f> .".to_string()];
        let new = vec!["<a> <b> <c> .".to_string(), "<x> <y> <z> .".to_string()];
        let diff = compute_diff(&old, &new);
        assert_eq!(diff.additions, vec!["<x> <y> <z> ."]);
        assert_eq!(diff.removals, vec!["<d> <e> <f> ."]);
    }

    #[test]
    fn compute_diff_empty_to_full_is_all_additions() {
        let triples = vec!["<a> <b> <c> .".to_string()];
        let diff = compute_diff(&[], &triples);
        assert_eq!(diff.additions, triples);
        assert!(diff.removals.is_empty());
    }

    #[test]
    fn graph_diff_cache_get_set() {
        let cache = GraphDiffCache::new();
        assert!(cache.get("foo").is_none());
        cache.set(
            "foo",
            "graph://abc".to_string(),
            vec!["<a> <b> <c> .".to_string()],
        );
        let (iri, triples) = cache.get("foo").unwrap();
        assert_eq!(iri, "graph://abc");
        assert_eq!(triples, vec!["<a> <b> <c> ."]);
    }

    #[test]
    fn parent_order_does_not_affect_hash() {
        let diff = sample_diff();
        let h1 = commit_hash(&diff, &["graph://a".to_string(), "graph://b".to_string()]);
        let h2 = commit_hash(&diff, &["graph://b".to_string(), "graph://a".to_string()]);
        assert_eq!(h1, h2, "parent order must be normalized");
    }
}
