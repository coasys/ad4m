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
//! it is a policy, not a fact, so this module does not pick: it orders the set
//! most specific first and leaves the choice to the caller.
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
use super::model_query::utils::looks_like_absolute_iri;
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

/// The triples an instance of this class must carry, or `None` when the class
/// cannot be tested at all.
///
/// Flags contribute `(predicate, Some(value))`; required properties contribute
/// `(predicate, None)` — the value is whatever the instance holds, so only the
/// predicate's presence is checked. This mirrors what `isSubjectInstance` builds
/// its `ASK` from, so the two agree on what membership means.
///
/// Every part of a required triple is emitted as an `<…>` IRIREF, where a
/// relative reference does not merely fail to match — it fails to *parse*, and
/// SPARQL rejects the whole query. A class carrying one is therefore dropped
/// entirely rather than tested with the offending property quietly removed,
/// which would only make the class easier to match than its definition says.
fn required_triples(shape: &ModelShape) -> Option<Vec<(String, Option<String>)>> {
    let mut triples = Vec::new();
    for p in &shape.properties {
        if p.is_flag {
            // A flag with no fixed value pins nothing, so there is no triple to
            // require — that is not a reason to reject the class.
            let Some(value) = p.initial_value.as_ref() else {
                continue;
            };
            if !looks_like_absolute_iri(&p.predicate) || !looks_like_absolute_iri(value) {
                return None;
            }
            triples.push((p.predicate.clone(), Some(value.clone())));
        } else if p.is_required && p.getter.is_none() {
            if !looks_like_absolute_iri(&p.predicate) {
                return None;
            }
            triples.push((p.predicate.clone(), None));
        }
    }
    Some(triples)
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

/// Resolve each URI to every subject class it is an instance of, most specific
/// first.
///
/// Membership is structural and therefore not exclusive: an instance conforms to
/// its parent classes, and to any unrelated class whose required set happens to
/// be a subset of what it carries. All of them are returned, ordered by
/// [`order_by_specificity`]; a caller that can only act on one takes the first,
/// accepting that a URI conforming to two unrelated classes makes that choice
/// arbitrary.
///
/// A URI is **absent** from the result when no *registered* class matched it.
/// That covers two situations, and nothing here separates them: the URI may not
/// be a subject instance at all, or it may be an instance of a class this
/// perspective has not registered. Absence is used rather than an empty list
/// because an empty list would claim the stronger thing — that the URI belongs
/// to no class — which this cannot know.
pub fn subject_classes_of(
    store: &SparqlStore,
    resolver: &dyn ShapeResolver,
    uris: &[String],
) -> Result<HashMap<String, Vec<String>>, Error> {
    subject_classes_of_with_pending(store, resolver, uris, &[])
}

/// [`subject_classes_of`], counting links that are staged but not yet committed.
///
/// `pending` is `(source, predicate, target)` for links an open batch holds. The
/// store cannot see them until the batch commits, and a subject is *created*
/// inside a batch: the constructor actions that write its class flags and the
/// setters that populate its relations are all staged together. Classifying such
/// a URI against the store alone answers "no class", which for the ordering
/// lookup means a collection created and ordered in one batch silently gets no
/// ordering entries.
///
/// Membership is set containment, so a staged triple counts exactly as a
/// committed one does — this widens what is known, never what conforms.
pub fn subject_classes_of_with_pending(
    store: &SparqlStore,
    resolver: &dyn ShapeResolver,
    uris: &[String],
    pending: &[(String, String, String)],
) -> Result<HashMap<String, Vec<String>>, Error> {
    let mut out: HashMap<String, Vec<String>> = HashMap::new();
    if uris.is_empty() {
        return Ok(out);
    }

    // Only URIs that can be emitted as an `<…>` IRIREF survive. A relative
    // reference in the VALUES clause is a parse error, not a non-match, so one
    // unusable input would otherwise cost every other URI in the batch its
    // answer. Dropping it here leaves it absent from the result, which is
    // already what "nothing matched" looks like.
    let valid: Vec<&String> = uris.iter().filter(|u| looks_like_absolute_iri(u)).collect();
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
            // A class with no required triples at all matches literally every
            // URI, which is not an answer — it is the absence of one.
            Ok(shape) => match required_triples(shape.as_ref()) {
                Some(triples) if !triples.is_empty() => shapes.push((name, triples)),
                Some(_) => {}
                None => log::warn!(
                    "subjectClassesOf: skipping class '{name}': a required predicate or flag \
                     value is not an absolute IRI"
                ),
            },
            // A class whose SHACL will not parse cannot classify anything; skip
            // it rather than failing the whole batch for the others.
            Err(e) => log::warn!("subjectClassesOf: skipping class '{name}': {e}"),
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

    // Staged links first. Both maps are sets, so a triple the store also
    // returns simply lands in the same slot.
    for (s, p, o) in pending {
        pairs
            .entry(s.clone())
            .or_default()
            .insert((p.clone(), o.clone()));
        present.entry(s.clone()).or_default().insert(p.clone());
    }

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
    fn test_local_name_handles_both_separators() {
        assert_eq!(local_name("ns://models/ImagePost"), "ImagePost");
        assert_eq!(local_name("http://x/y#ImagePost"), "ImagePost");
        assert_eq!(local_name("ImagePost"), "ImagePost");
    }
}
