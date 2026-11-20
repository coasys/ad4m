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
use super::model_query::{
    conformance_triples, instance_link_predicates, local_predicates, ConformanceObject, LinkGuard,
};
use super::shacl_parser::ABSTRACT_PREDICATE;
use super::sparql_store::stored_as_literal;
use super::sparql_store::{EvalStop, SparqlStore};
use crate::types::LinkStatus;

/// Every subject class registered in this perspective.
///
/// The SHACL writer records each one as
/// `<namespace://ClassName> rdf://type <ad4m://SubjectClass>`, so the class
/// names are the local parts of those subjects.
pub fn all_subject_class_names(stop: &EvalStop, store: &SparqlStore) -> Result<Vec<String>, Error> {
    let query = r#"
        SELECT ?targetClass WHERE {
            ?targetClass <rdf://type> <ad4m://SubjectClass> .
        }
    "#;
    let rows: Vec<Value> = serde_json::from_str(&store.query(stop, query)?)?;

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

/// Every registered class that is not abstract (#1435): the classes a record
/// can be classified as. An abstract class names what a record has, not what
/// it is, so it is never one of a record's classes.
pub fn concrete_subject_class_names(
    stop: &EvalStop,
    store: &SparqlStore,
) -> Result<Vec<String>, Error> {
    let query = format!(
        r#"
        SELECT ?targetClass WHERE {{
            ?targetClass <rdf://type> <ad4m://SubjectClass> .
            FILTER NOT EXISTS {{
                ?targetClass <ad4m://shape> ?shape .
                ?shape <{ABSTRACT_PREDICATE}> ?marker .
            }}
        }}
    "#
    );
    let rows: Vec<Value> = serde_json::from_str(&store.query(stop, &query)?)?;
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

pub(crate) fn local_name(uri: &str) -> String {
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

/// One triple an instance of a class must carry: `value` for a flag's fixed
/// object, `None` when only the predicate's presence is required, and `local`
/// when the class declares the predicate `local`, so only a Local link
/// asserts it.
struct Required {
    pred: String,
    value: Option<FlagValue>,
    local: bool,
}

/// A flag's fixed object, as conformance matches it.
#[derive(Clone)]
enum FlagValue {
    /// `?source <pred> <iri>`: this IRI, not a literal with the same text.
    Iri(String),
    /// `FILTER(STR(?o) = "…")`: any term whose lexical form is this.
    Str(String),
}

/// What a node must carry to be returned by a class's own query: its
/// conformance triples ([`required_triples`]), and at least one link that
/// query loads, for an instance with no loaded link has no row and is not
/// returned. `loaded` is [`instance_link_predicates`] with each predicate's
/// local admission; empty means every predicate, so the required links
/// themselves are loaded.
struct Membership {
    required: Vec<Required>,
    loaded: Vec<(String, bool)>,
}

/// The triples an instance of this class must carry, or `None` when the class
/// cannot be tested at all.
///
/// Derived from the class's own query: [`conformance_triples`] is what its
/// conformance patterns are emitted from (required properties, getter-backed
/// ones and value-less flags included; else the first initial value), and
/// [`local_predicates`] is what loading's local filter keeps to Local links.
/// With [`Membership`]'s loaded-link clause, a node is classified as a class
/// exactly when that class's query, under the same read, returns it.
///
/// The predicates are emitted as `<…>` IRIREFs, where a relative reference does
/// not merely fail to match — it fails to *parse*, and SPARQL rejects the whole
/// query. A class carrying one is therefore dropped entirely rather than tested
/// with the offending property quietly removed, which would only make the
/// class easier to match than its definition says.
fn required_triples(shape: &ModelShape) -> Option<Vec<Required>> {
    let local = local_predicates(shape);
    let mut triples = Vec::new();
    for t in conformance_triples(shape).triples {
        let pred = &t.property.predicate;
        if !looks_like_absolute_iri(pred) {
            return None;
        }
        triples.push(Required {
            pred: pred.clone(),
            value: match t.object {
                ConformanceObject::Iri(v) => Some(FlagValue::Iri(v)),
                ConformanceObject::Str(v) => Some(FlagValue::Str(v)),
                ConformanceObject::Any => None,
            },
            local: local.contains(&format!("<{pred}>")),
        });
    }
    Some(triples)
}

/// [`Membership`] of `shape`, or `None` when the class cannot be tested.
fn membership(shape: &ModelShape) -> Option<Membership> {
    let required = required_triples(shape)?;
    let local = local_predicates(shape);
    let loaded = instance_link_predicates(shape)
        .into_iter()
        // A predicate that cannot be emitted cannot be fetched; it is never
        // counted as carried (the class's own query fails on it anyway).
        .filter(|p| looks_like_absolute_iri(p))
        .map(|p| (p.to_string(), local.contains(&format!("<{p}>"))))
        .collect();
    Some(Membership { required, loaded })
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
///
/// `guard` is the links classification reads: a triple counts only when a link
/// asserting it passes the guard, the same check the read's own conformance
/// and hydration apply. A caller serving a read passes the read's
/// (`LinkGuard::of(query)`), so a target is never classified through a link
/// the read cannot see (a Local type link under a Shared read); one that
/// serves no read passes [`LinkGuard::ANY`], every link.
pub(crate) fn subject_classes_of(
    stop: &EvalStop,
    store: &SparqlStore,
    resolver: &dyn ShapeResolver,
    uris: &[String],
    guard: LinkGuard<'_>,
) -> Result<HashMap<String, Vec<String>>, Error> {
    subject_classes_of_with_pending(stop, store, resolver, uris, &[], guard)
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
pub(crate) fn subject_classes_of_with_pending(
    stop: &EvalStop,
    store: &SparqlStore,
    resolver: &dyn ShapeResolver,
    uris: &[String],
    pending: &[(String, String, String)],
    guard: LinkGuard<'_>,
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
    let class_names = concrete_subject_class_names(stop, store)?;
    let mut shapes: Vec<(String, Membership)> = Vec::new();
    for name in class_names {
        match resolver.get_shape(&name) {
            // A class with no required triples at all matches literally every
            // URI, which is not an answer — it is the absence of one.
            Ok(shape) => match membership(shape.as_ref()) {
                Some(m) if !m.required.is_empty() => shapes.push((name, m)),
                Some(_) => {}
                None => log::warn!(
                    "subjectClassesOf: skipping class '{name}': a required predicate is not \
                     an absolute IRI"
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
    // specific object, a required property only has to be present. A local
    // predicate is read again through Local links alone.
    let mut flag_preds: HashSet<&str> = HashSet::new();
    let mut presence_preds: HashSet<&str> = HashSet::new();
    let mut local_flag_preds: HashSet<&str> = HashSet::new();
    let mut local_presence_preds: HashSet<&str> = HashSet::new();
    for (_, m) in &shapes {
        for t in &m.required {
            let (flags, presence) = if t.local {
                (&mut local_flag_preds, &mut local_presence_preds)
            } else {
                (&mut flag_preds, &mut presence_preds)
            };
            match t.value {
                Some(_) => flags.insert(t.pred.as_str()),
                None => presence.insert(t.pred.as_str()),
            };
        }
        for (pred, local) in &m.loaded {
            if *local {
                local_presence_preds.insert(pred.as_str());
            } else {
                presence_preds.insert(pred.as_str());
            }
        }
    }

    // What each URI carries under the read, and under the read through Local
    // links only (what loading keeps for a `local` predicate). Staged links
    // count in both: they are this agent's own, not yet committed.
    let mut carried = Carried::default();
    let mut carried_local = Carried::default();
    for (s, p, o) in pending {
        let iri = !stored_as_literal(o);
        carried.add(s, p, Some((o, iri)));
        carried_local.add(s, p, Some((o, iri)));
    }
    carried.fetch(
        stop,
        store,
        &values_clause,
        &flag_preds,
        &presence_preds,
        guard,
    )?;
    // Local and Shared at once admits nothing: under a Shared read a `local`
    // predicate loads nothing, so it is never carried.
    if guard.status != Some(&LinkStatus::Shared) {
        let local_guard = LinkGuard {
            status: Some(&LinkStatus::Local),
            include_unverified: guard.include_unverified,
        };
        carried_local.fetch(
            stop,
            store,
            &values_clause,
            &local_flag_preds,
            &local_presence_preds,
            local_guard,
        )?;
    }

    // Set containment, in memory. Every class is tested against every URI —
    // which is what returning the full set requires, and is also why there was
    // nothing to gain from querying class by class.
    for uri in &valid {
        let uri = uri.as_str();
        let mut matches: Vec<(String, usize)> = Vec::new();
        for (name, m) in &shapes {
            let carries = |pred: &str, local: bool, value: Option<&FlagValue>| {
                let c = if local { &carried_local } else { &carried };
                c.has(uri, pred, value)
            };
            let conforms = m
                .required
                .iter()
                .all(|t| carries(&t.pred, t.local, t.value.as_ref()));
            let loads_a_link = m.loaded.is_empty()
                || m.loaded
                    .iter()
                    .any(|(pred, local)| carries(pred, *local, None));
            if conforms && loads_a_link {
                matches.push((name.clone(), m.required.len()));
            }
        }

        if !matches.is_empty() {
            out.insert(uri.to_string(), order_by_specificity(&mut matches));
        }
    }

    Ok(out)
}

/// The triples each URI carries, in the two forms membership asks about:
/// `(predicate, object's lexical form, object is an IRI)` for flags,
/// predicates for presence.
#[derive(Default)]
struct Carried {
    pairs: HashMap<String, HashSet<(String, String, bool)>>,
    present: HashMap<String, HashSet<String>>,
}

impl Carried {
    fn add(&mut self, s: &str, p: &str, o: Option<(&str, bool)>) {
        if let Some((o, iri)) = o {
            self.pairs.entry(s.to_string()).or_default().insert((
                p.to_string(),
                o.to_string(),
                iri,
            ));
        }
        // A flag predicate can also be some other class's required property,
        // so it counts as present too.
        self.present
            .entry(s.to_string())
            .or_default()
            .insert(p.to_string());
    }

    fn has(&self, s: &str, p: &str, value: Option<&FlagValue>) -> bool {
        let pair = |v: &str, iri: bool| {
            self.pairs
                .get(s)
                .is_some_and(|pairs| pairs.contains(&(p.to_string(), v.to_string(), iri)))
        };
        match value {
            Some(FlagValue::Iri(v)) => pair(v, true),
            Some(FlagValue::Str(v)) => pair(v, true) || pair(v, false),
            None => self.present.get(s).is_some_and(|preds| preds.contains(p)),
        }
    }

    /// Both fetches under `guard`; nothing is queried for an empty set.
    fn fetch(
        &mut self,
        stop: &EvalStop,
        store: &SparqlStore,
        values_clause: &str,
        flag_preds: &HashSet<&str>,
        presence_preds: &HashSet<&str>,
        guard: LinkGuard<'_>,
    ) -> Result<(), Error> {
        if !flag_preds.is_empty() {
            for row in &fetch(stop, store, values_clause, flag_preds, true, guard)? {
                if let (Some(s), Some(p), Some(o)) =
                    (row["s"].as_str(), row["p"].as_str(), row["o"].as_str())
                {
                    self.add(s, p, Some((o, row["iri"].as_str() == Some("true"))));
                }
            }
        }
        if !presence_preds.is_empty() {
            for row in &fetch(stop, store, values_clause, presence_preds, false, guard)? {
                if let (Some(s), Some(p)) = (row["s"].as_str(), row["p"].as_str()) {
                    self.add(s, p, None);
                }
            }
        }
        Ok(())
    }
}

/// One `VALUES`-bound query for the whole batch over one set of predicates.
///
/// `with_object` selects the object as well, and whether it is an IRI (`?iri`,
/// conformance's `<iri>` matches no literal), which flag matching needs and a
/// presence test does not — a required property's value can be an arbitrarily
/// long literal, and none of it is read. `guard` keeps a triple only when a
/// link asserting it passes; empty when it admits every link.
fn fetch(
    stop: &EvalStop,
    store: &SparqlStore,
    values_clause: &str,
    preds: &HashSet<&str>,
    with_object: bool,
    guard: LinkGuard<'_>,
) -> Result<Vec<Value>, Error> {
    let pred_values = preds
        .iter()
        .map(|p| format!("<{p}>"))
        .collect::<Vec<_>>()
        .join(" ");

    let query = if with_object {
        format!(
            "SELECT DISTINCT ?s ?p ?o (isIRI(?o) AS ?iri) WHERE {{ VALUES ?s {{ {values_clause} }} VALUES ?p {{ {pred_values} }} ?s ?p ?o .{} }}",
            guard.exists("?s", "?p", "?o")
        )
    } else {
        format!(
            "SELECT DISTINCT ?s ?p WHERE {{ VALUES ?s {{ {values_clause} }} VALUES ?p {{ {pred_values} }} ?s ?p ?_o .{} }}",
            guard.exists("?s", "?p", "?_o")
        )
    };

    Ok(serde_json::from_str(&store.query(stop, &query)?)?)
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
