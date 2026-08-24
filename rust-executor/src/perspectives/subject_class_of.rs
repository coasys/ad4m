//! Resolve a URI to the subject class it is an instance of.
//!
//! The mirror of this — "is this URI an instance of class X" — has existed since
//! the beginning as `isSubjectInstance`. The other direction had no answer at
//! all: `getSubjectClassMetadata` goes className → metadata and nothing goes
//! back. Callers needing it looped over every registered class asking the
//! yes/no question, one round trip per class per URI.
//!
//! # Why this is a classification, not a lookup
//!
//! AD4M writes no `rdf:type` triple for instances. Class membership is
//! *structural*: a URI is an instance of a class when it carries that class's
//! required triples — its flags, and its required properties. That has two
//! consequences this module has to handle.
//!
//! **It cannot be answered by reading one triple.** There is nothing on the
//! instance that names its class.
//!
//! **It is not exclusive.** A subclass carries everything its parent requires,
//! so an `ImagePost` conforms to `Post` as well — and to any unrelated class
//! whose required set happens to be a subset. The answer is genuinely a set of
//! candidates, and something has to choose. See [`most_specific`].
//!
//! # Strategy
//!
//! Flags first. A `@Flag` is a fixed `(predicate, value)` pair a class stamps on
//! every instance, which is as close to a type tag as the model has — and the
//! class families that matter in practice all declare one. All
//! flags across all classes are fetched in a single query.
//!
//! Conformance second, for classes with no flag, and only for the URIs the flag
//! pass left unresolved.

use deno_core::anyhow::Error;
use serde_json::Value;
use std::collections::{HashMap, HashSet};

use super::model_query::types::{ModelShape, ShapeResolver};
use super::model_query::utils::validate_iri;
use super::sparql_store::SparqlStore;

/// Every subject class registered in this perspective.
///
/// The SHACL writer records each one as
/// `<namespace://ClassName> rdf://type <ad4m://SubjectClass>`, so the class
/// names are the local parts of those subjects.
pub fn all_subject_class_names(store: &SparqlStore) -> Result<Vec<String>, Error> {
    let query = r#"
        SELECT ?targetClass WHERE {
            ?targetClass <rdf://type> <ad4m://SubjectClass> .
        }
    "#;
    let rows: Vec<Value> = serde_json::from_str(&store.query(query)?)?;

    let mut names: Vec<String> = rows
        .iter()
        .filter_map(|r| r["targetClass"].as_str())
        .map(local_name)
        .filter(|n| !n.is_empty())
        .collect();
    names.sort();
    names.dedup();
    Ok(names)
}

fn local_name(uri: &str) -> String {
    if let Some(p) = uri.rfind('#') {
        return uri[p + 1..].to_string();
    }
    if let Some(p) = uri.rfind('/') {
        let after = &uri[p + 1..];
        if !after.is_empty() {
            return after.to_string();
        }
    }
    uri.to_string()
}

/// The triples an instance of this class must carry.
///
/// Flags contribute `(predicate, Some(value))`; required properties contribute
/// `(predicate, None)` — the value is whatever the instance holds, so only the
/// predicate's presence is checked. This mirrors what `isSubjectInstance` builds
/// its `ASK` from, so the two agree on what membership means.
fn required_triples(shape: &ModelShape) -> Vec<(String, Option<String>)> {
    shape
        .properties
        .iter()
        .filter(|p| !p.predicate.is_empty() && validate_iri(&p.predicate).is_ok())
        .filter_map(|p| {
            if p.is_flag {
                p.initial_value
                    .as_ref()
                    .filter(|v| validate_iri(v).is_ok())
                    .map(|v| (p.predicate.clone(), Some(v.clone())))
            } else if p.is_required && p.getter.is_none() {
                Some((p.predicate.clone(), None))
            } else {
                None
            }
        })
        .collect()
}

/// Choose between classes a URI structurally conforms to.
///
/// **More required triples wins.** A subclass's required set is a superset of
/// its parent's — it inherits every flag and required property and adds its own
/// — so "matched more" is the same ordering as "more derived", without this
/// module needing to know anything about the inheritance graph (which SHACL does
/// not record).
///
/// Ties break alphabetically. Two genuinely different classes with identical
/// required sets are indistinguishable *by definition of how membership works
/// here*, so any choice is arbitrary; the point of sorting is that every peer
/// makes the same arbitrary choice rather than returning whatever the hash map
/// iterated first.
fn most_specific(candidates: &mut Vec<(String, usize)>) -> Option<String> {
    candidates.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
    candidates.first().map(|(name, _)| name.clone())
}

/// Resolve each URI to the name of the class it is an instance of.
///
/// URIs that match no registered class are absent from the result rather than
/// mapped to a placeholder — "not a subject instance" and "an instance of
/// something unnameable" are different answers, and a caller can tell them apart
/// by absence.
pub fn subject_class_of(
    store: &SparqlStore,
    resolver: &dyn ShapeResolver,
    uris: &[String],
) -> Result<HashMap<String, String>, Error> {
    let mut out: HashMap<String, String> = HashMap::new();
    if uris.is_empty() {
        return Ok(out);
    }

    let valid: Vec<&String> = uris.iter().filter(|u| validate_iri(u).is_ok()).collect();
    if valid.is_empty() {
        return Ok(out);
    }
    let values_clause = valid
        .iter()
        .map(|u| format!("<{u}>"))
        .collect::<Vec<_>>()
        .join(" ");

    // Load every class's shape once. Shapes are cached per perspective, so this
    // is a map lookup after the first call.
    let class_names = all_subject_class_names(store)?;
    let mut shapes: Vec<(String, Vec<(String, Option<String>)>)> = Vec::new();
    for name in class_names {
        match resolver.get_shape(&name) {
            Ok(shape) => shapes.push((name, required_triples(shape.as_ref()))),
            // A class whose SHACL will not parse cannot classify anything; skip
            // it rather than failing the whole batch for the others.
            Err(e) => log::warn!("subjectClassOf: skipping class '{name}': {e}"),
        }
    }
    if shapes.is_empty() {
        return Ok(out);
    }

    // ---- pass 1: flags -----------------------------------------------------

    let mut by_flag: HashMap<(String, String), Vec<(String, usize)>> = HashMap::new();
    for (name, triples) in &shapes {
        for (pred, value) in triples {
            if let Some(v) = value {
                by_flag
                    .entry((pred.clone(), v.clone()))
                    .or_default()
                    .push((name.clone(), triples.len()));
            }
        }
    }

    let mut candidates: HashMap<String, Vec<(String, usize)>> = HashMap::new();

    if !by_flag.is_empty() {
        let flag_preds: HashSet<&str> = by_flag.keys().map(|(p, _)| p.as_str()).collect();
        let pred_values = flag_preds
            .iter()
            .map(|p| format!("<{p}>"))
            .collect::<Vec<_>>()
            .join(" ");

        let query = format!(
            "SELECT ?s ?p ?o WHERE {{ VALUES ?s {{ {values_clause} }} VALUES ?p {{ {pred_values} }} ?s ?p ?o . }}"
        );
        let rows: Vec<Value> = serde_json::from_str(&store.query(&query)?)?;

        for row in &rows {
            let (s, p, o) = match (row["s"].as_str(), row["p"].as_str(), row["o"].as_str()) {
                (Some(s), Some(p), Some(o)) => (s, p, o),
                _ => continue,
            };
            if let Some(classes) = by_flag.get(&(p.to_string(), o.to_string())) {
                candidates
                    .entry(s.to_string())
                    .or_default()
                    .extend(classes.iter().cloned());
            }
        }
    }

    for (uri, mut cands) in candidates {
        if let Some(best) = most_specific(&mut cands) {
            out.insert(uri, best);
        }
    }

    // ---- pass 2: conformance, for whatever the flags did not settle --------

    let unresolved: Vec<&String> = valid
        .iter()
        .filter(|u| !out.contains_key(u.as_str()))
        .copied()
        .collect();
    if unresolved.is_empty() {
        return Ok(out);
    }

    let unresolved_values = unresolved
        .iter()
        .map(|u| format!("<{u}>"))
        .collect::<Vec<_>>()
        .join(" ");

    let mut fallback: HashMap<String, Vec<(String, usize)>> = HashMap::new();
    for (name, triples) in &shapes {
        // A class with no required triples at all matches literally every URI
        // that has any link, which is not an answer — it is the absence of one.
        if triples.is_empty() {
            continue;
        }
        let patterns = triples
            .iter()
            .enumerate()
            .map(|(i, (pred, value))| match value {
                Some(v) => format!("    ?s <{pred}> <{v}> ."),
                None => format!("    ?s <{pred}> ?_v{i} ."),
            })
            .collect::<Vec<_>>()
            .join("\n");

        let query = format!(
            "SELECT DISTINCT ?s WHERE {{ VALUES ?s {{ {unresolved_values} }}\n{patterns}\n}}"
        );
        let rows: Vec<Value> = serde_json::from_str(&store.query(&query)?)?;
        for row in &rows {
            if let Some(s) = row["s"].as_str() {
                fallback
                    .entry(s.to_string())
                    .or_default()
                    .push((name.clone(), triples.len()));
            }
        }
    }

    for (uri, mut cands) in fallback {
        if let Some(best) = most_specific(&mut cands) {
            out.insert(uri, best);
        }
    }

    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_most_specific_prefers_more_required_triples() {
        // A subclass inherits its parent's requirements and adds its own, so it
        // matches strictly more — which is how "more derived" is recovered
        // without SHACL recording an inheritance graph.
        let mut c = vec![("Post".to_string(), 1), ("ImagePost".to_string(), 3)];
        assert_eq!(most_specific(&mut c), Some("ImagePost".to_string()));
    }

    #[test]
    fn test_most_specific_tie_is_deterministic() {
        // Indistinguishable by construction, so the choice is arbitrary — but it
        // must be the *same* arbitrary choice on every peer, not hash order.
        let mut a = vec![("Beta".to_string(), 2), ("Alpha".to_string(), 2)];
        let mut b = vec![("Alpha".to_string(), 2), ("Beta".to_string(), 2)];
        assert_eq!(most_specific(&mut a), most_specific(&mut b));
        assert_eq!(most_specific(&mut a), Some("Alpha".to_string()));
    }

    #[test]
    fn test_local_name_handles_both_separators() {
        assert_eq!(local_name("ns://models/ImagePost"), "ImagePost");
        assert_eq!(local_name("http://x/y#ImagePost"), "ImagePost");
        assert_eq!(local_name("ImagePost"), "ImagePost");
    }
}
