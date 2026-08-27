//! Resolve a URI to the subject classes it is an instance of.
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
//! whose required set happens to be a subset. Membership is therefore genuinely
//! a *set*, and that set is what this module returns. Picking one member out of
//! it is a policy, not a fact, so it lives in [`most_specific`] where a caller
//! opts into it by name.
//!
//! # Strategy
//!
//! Every class has to be tested against every URI — that is what returning the
//! whole set means — so there is nothing to be gained by testing them one class
//! at a time. Two queries fetch the entire batch's relevant triples up front,
//! and the set containment runs in memory against shapes that are already
//! cached:
//!
//! - flag predicates, with their objects, since a flag matches a fixed
//!   `(predicate, value)` pair;
//! - required-property predicates, without their objects, since only the
//!   predicate's presence is checked and hauling literal bodies back for a
//!   presence test wastes bandwidth on exactly the largest fields.
//!
//! Both are `VALUES`-bound to the URIs asked about, so neither scans the store.

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

/// Order the classes a URI belongs to, most specific first.
///
/// **More required triples wins.** A subclass's required set is a superset of
/// its parent's — it inherits every flag and required property and adds its own
/// — so "matched more" is the same ordering as "more derived", without this
/// module needing to know anything about the inheritance graph (which SHACL does
/// not record).
///
/// Ties break alphabetically. Two genuinely different classes with identical
/// required sets are indistinguishable *by definition of how membership works
/// here*, so any tie-break is arbitrary; the point of sorting is that every peer
/// makes the same arbitrary choice rather than returning whatever the hash map
/// iterated first.
///
/// This is only an ordering. It ranks the members of a set without removing any
/// of them — the caller decides whether the tail matters.
fn order_by_specificity(matches: &mut Vec<(String, usize)>) -> Vec<String> {
    matches.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
    matches.iter().map(|(name, _)| name.clone()).collect()
}

/// The single best class for a caller that can only act on one.
///
/// Some callers genuinely need one answer — hydrating a URI against a shape, for
/// instance, can only use one shape. This is the policy those callers should
/// share, so that "which one did we pick" is a named, greppable decision rather
/// than an inline `[0]` repeated at every call site with its own justification.
///
/// It is the *first* member under [`order_by_specificity`], which is the order
/// [`subject_class_of`] already returns. Callers that can handle the whole set
/// should use the whole set: a URI conforming to two unrelated classes is a real
/// situation, and this function's answer to it is arbitrary by construction.
pub fn most_specific(classes: &[String]) -> Option<&str> {
    classes.first().map(String::as_str)
}

/// Resolve each URI to every subject class it is an instance of, most specific
/// first.
///
/// Membership is structural and therefore not exclusive: an instance conforms to
/// its parent classes, and to any unrelated class whose required set happens to
/// be a subset of what it carries. All of them are returned. Use
/// [`most_specific`] to collapse the list where a caller can only act on one.
///
/// URIs that match no registered class are absent from the result rather than
/// mapped to an empty list — "not a subject instance" and "an instance of
/// something unnameable" are different answers, and a caller can tell them apart
/// by absence.
pub fn subject_class_of(
    store: &SparqlStore,
    resolver: &dyn ShapeResolver,
    uris: &[String],
) -> Result<HashMap<String, Vec<String>>, Error> {
    let mut out: HashMap<String, Vec<String>> = HashMap::new();
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
            Ok(shape) => {
                let triples = required_triples(shape.as_ref());
                // A class with no required triples at all matches literally
                // every URI, which is not an answer — it is the absence of one.
                if !triples.is_empty() {
                    shapes.push((name, triples));
                }
            }
            // A class whose SHACL will not parse cannot classify anything; skip
            // it rather than failing the whole batch for the others.
            Err(e) => log::warn!("subjectClassOf: skipping class '{name}': {e}"),
        }
    }
    if shapes.is_empty() {
        return Ok(out);
    }

    // Split the predicates by what each is used for: a flag has to match a
    // specific object, a required property only has to be present.
    let mut flag_preds: HashSet<&str> = HashSet::new();
    let mut presence_preds: HashSet<&str> = HashSet::new();
    for (_, triples) in &shapes {
        for (pred, value) in triples {
            match value {
                Some(_) => flag_preds.insert(pred.as_str()),
                None => presence_preds.insert(pred.as_str()),
            };
        }
    }

    // What each URI actually carries, in the two shapes membership asks about.
    let mut pairs: HashMap<String, HashSet<(String, String)>> = HashMap::new();
    let mut present: HashMap<String, HashSet<String>> = HashMap::new();

    if !flag_preds.is_empty() {
        let rows = fetch(store, &values_clause, &flag_preds, true)?;
        for row in &rows {
            let (s, p, o) = match (row["s"].as_str(), row["p"].as_str(), row["o"].as_str()) {
                (Some(s), Some(p), Some(o)) => (s, p, o),
                _ => continue,
            };
            pairs
                .entry(s.to_string())
                .or_default()
                .insert((p.to_string(), o.to_string()));
            // A flag predicate can also be some other class's required
            // property, so it counts as present too.
            present
                .entry(s.to_string())
                .or_default()
                .insert(p.to_string());
        }
    }

    if !presence_preds.is_empty() {
        let rows = fetch(store, &values_clause, &presence_preds, false)?;
        for row in &rows {
            let (s, p) = match (row["s"].as_str(), row["p"].as_str()) {
                (Some(s), Some(p)) => (s, p),
                _ => continue,
            };
            present
                .entry(s.to_string())
                .or_default()
                .insert(p.to_string());
        }
    }

    // Set containment, in memory. Every class is tested against every URI —
    // which is what returning the full set requires, and is also why there was
    // nothing to gain from querying class by class.
    for uri in &valid {
        let uri = uri.as_str();
        let uri_pairs = pairs.get(uri);
        let uri_present = present.get(uri);

        let mut matches: Vec<(String, usize)> = Vec::new();
        for (name, triples) in &shapes {
            let conforms = triples.iter().all(|(pred, value)| match value {
                Some(v) => uri_pairs
                    .map(|p| p.contains(&(pred.clone(), v.clone())))
                    .unwrap_or(false),
                None => uri_present.map(|p| p.contains(pred)).unwrap_or(false),
            });
            if conforms {
                matches.push((name.clone(), triples.len()));
            }
        }

        if !matches.is_empty() {
            out.insert(uri.to_string(), order_by_specificity(&mut matches));
        }
    }

    Ok(out)
}

/// One `VALUES`-bound query for the whole batch over one set of predicates.
///
/// `with_object` selects the object as well, which flag matching needs and a
/// presence test does not — a required property's value can be an arbitrarily
/// long literal, and none of it is read.
fn fetch(
    store: &SparqlStore,
    values_clause: &str,
    preds: &HashSet<&str>,
    with_object: bool,
) -> Result<Vec<Value>, Error> {
    let pred_values = preds
        .iter()
        .map(|p| format!("<{p}>"))
        .collect::<Vec<_>>()
        .join(" ");

    let query = if with_object {
        format!(
            "SELECT DISTINCT ?s ?p ?o WHERE {{ VALUES ?s {{ {values_clause} }} VALUES ?p {{ {pred_values} }} ?s ?p ?o . }}"
        )
    } else {
        format!(
            "SELECT DISTINCT ?s ?p WHERE {{ VALUES ?s {{ {values_clause} }} VALUES ?p {{ {pred_values} }} ?s ?p ?_o . }}"
        )
    };

    Ok(serde_json::from_str(&store.query(&query)?)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_order_puts_more_required_triples_first() {
        // A subclass inherits its parent's requirements and adds its own, so it
        // matches strictly more — which is how "more derived" is recovered
        // without SHACL recording an inheritance graph. Both are returned: the
        // instance really is a Post as well.
        let mut c = vec![("Post".to_string(), 1), ("ImagePost".to_string(), 3)];
        assert_eq!(
            order_by_specificity(&mut c),
            vec!["ImagePost".to_string(), "Post".to_string()]
        );
    }

    #[test]
    fn test_order_is_deterministic_on_ties() {
        // Indistinguishable by construction, so the order is arbitrary — but it
        // must be the *same* arbitrary order on every peer, not hash order.
        let mut a = vec![("Beta".to_string(), 2), ("Alpha".to_string(), 2)];
        let mut b = vec![("Alpha".to_string(), 2), ("Beta".to_string(), 2)];
        assert_eq!(order_by_specificity(&mut a), order_by_specificity(&mut b));
        assert_eq!(
            order_by_specificity(&mut a),
            vec!["Alpha".to_string(), "Beta".to_string()]
        );
    }

    #[test]
    fn test_most_specific_reads_the_head_of_the_ordering() {
        let mut c = vec![("Post".to_string(), 1), ("ImagePost".to_string(), 3)];
        let ordered = order_by_specificity(&mut c);
        assert_eq!(most_specific(&ordered), Some("ImagePost"));
        assert_eq!(most_specific(&[]), None);
    }

    #[test]
    fn test_local_name_handles_both_separators() {
        assert_eq!(local_name("ns://models/ImagePost"), "ImagePost");
        assert_eq!(local_name("http://x/y#ImagePost"), "ImagePost");
        assert_eq!(local_name("ImagePost"), "ImagePost");
    }
}
