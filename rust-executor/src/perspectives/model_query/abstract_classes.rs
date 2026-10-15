//! Abstract model classes (#1435, design agreed on #1440).
//!
//! An abstract class is an ordinary model class whose shape carries one
//! [`ABSTRACT_PREDICATE`] marker. It names what a record must have, not which
//! class wrote it. A model query over it reads every installed concrete class
//! that has all of its names, as one union (`execute_union_query`), so a
//! caller can ask for "anything with `text`" without knowing which app wrote
//! it or what it named its classes.
//!
//! ```text
//!  modelQuery("TextContent") ──▶ shape is abstract? ──▶ resolve(): every concrete
//!                                      │                class with the names,
//!                                      │                each as a view
//!                                      ▼
//!                       execute_union_query_with_views ──▶ rows keep their class
//! ```
//!
//! # Conformance
//!
//! Checked on shapes, never on instances, and on names only: a concrete class
//! conforms when, for each of the abstract class's properties, it has a
//! property answering to that name, and for each relation a relation
//! answering to that name. Predicates, datatypes and directions are not
//! compared. A property answers to its own name, or to its alias for this
//! abstract class (`@Property({ alias: { TextContent: "text" } })`, one
//! [`ALIAS_PREDICATE`] link on the property shape).
//!
//! # Views
//!
//! Read through an abstract class, a concrete class's aliased properties carry
//! the abstract class's names: `Message.body` with alias `text` comes back as
//! `text`, read through `Message.body`'s predicate, and `where: { text }`
//! filters on it. The view is the concrete shape with those properties
//! renamed; nothing else changes. It is used for the queried rows only:
//! records reached through an `include` are read as their own class. Querying
//! the concrete class itself returns its own names.
//!
//! # What an abstract class is not
//!
//! It is never created (`create_subject` refuses it) and never chosen when a
//! record is classified (`subject_classes_of` lists concrete classes only).
//! An abstract class does not conform to another abstract class.
//!
//! `@Model({ implements: [...] })` is checked by the SDK when a class is
//! declared; the executor does not read it. Matching stays structural.

use super::types::{ModelShape, ShapeResolver};
use crate::perspectives::shacl_parser::{ABSTRACT_PREDICATE, ALIAS_PREDICATE};
use crate::perspectives::sparql_store::SparqlStore;
use crate::perspectives::subject_classes_of::{concrete_subject_class_names, local_name};
use deno_core::anyhow::Error;
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::sync::Arc;

/// The abstract classes and aliases in a perspective's installed SHACL. Read
/// in two queries, and cached by the perspective until the installed shapes
/// change.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct AbstractIndex {
    abstract_classes: BTreeSet<String>,
    /// class name → property name → { abstract class name → alias }.
    aliases: HashMap<String, HashMap<String, BTreeMap<String, String>>>,
}

impl AbstractIndex {
    pub fn load(store: &SparqlStore) -> Result<Self, Error> {
        let marked = format!(
            r#"
            SELECT ?targetClass WHERE {{
                ?targetClass <rdf://type> <ad4m://SubjectClass> .
                ?targetClass <ad4m://shape> ?shape .
                ?shape <{ABSTRACT_PREDICATE}> ?marker .
            }}
            "#
        );
        let rows: Vec<Value> = serde_json::from_str(&store.query(&marked)?)?;
        let abstract_classes = rows
            .iter()
            .filter_map(|r| r["targetClass"].as_str())
            .map(local_name)
            .filter(|n| !n.is_empty())
            .collect();

        let aliased = format!(
            r#"
            SELECT ?targetClass ?prop ?alias WHERE {{
                ?targetClass <rdf://type> <ad4m://SubjectClass> .
                ?targetClass <ad4m://shape> ?shape .
                ?shape <sh://property> ?prop .
                ?prop <{ALIAS_PREDICATE}> ?alias .
            }}
            "#
        );
        let rows: Vec<Value> = serde_json::from_str(&store.query(&aliased)?)?;
        let mut aliases: HashMap<String, HashMap<String, BTreeMap<String, String>>> =
            HashMap::new();
        for row in &rows {
            let (Some(class_uri), Some(prop_uri), Some(raw)) = (
                row["targetClass"].as_str(),
                row["prop"].as_str(),
                row["alias"].as_str(),
            ) else {
                continue;
            };
            // `namespace://Class.prop`, as the SHACL writer and the shape
            // loader name it.
            let Some((_, prop)) = prop_uri.rsplit_once('.') else {
                continue;
            };
            let json = super::shape::decode_literal_string_target(raw);
            match serde_json::from_str::<BTreeMap<String, String>>(&json) {
                Ok(map) => {
                    aliases
                        .entry(local_name(class_uri))
                        .or_default()
                        .entry(prop.to_string())
                        .or_default()
                        .extend(map);
                }
                Err(e) => log::warn!("{prop_uri}: ignoring an alias that is not a JSON map: {e}"),
            }
        }
        Ok(AbstractIndex {
            abstract_classes,
            aliases,
        })
    }

    pub fn is_abstract(&self, class_name: &str) -> bool {
        self.abstract_classes.contains(class_name)
    }

    /// The name `property` of `class` answers to read through
    /// `abstract_class`: its alias for it, or its own name.
    fn name_through<'a>(&'a self, class: &str, property: &'a str, abstract_class: &str) -> &'a str {
        self.aliases
            .get(class)
            .and_then(|props| props.get(property))
            .and_then(|map| map.get(abstract_class))
            .map(String::as_str)
            .unwrap_or(property)
    }
}

/// The concrete classes an abstract class reads, each with its view (see the
/// module docs). Sorted by class name.
#[derive(Debug, Default, Clone)]
pub struct Resolution {
    pub classes: Vec<String>,
    pub views: HashMap<String, Arc<ModelShape>>,
}

/// `concrete` read through `abstract_shape` (named `abstract_class`), or
/// `None` when it does not have every name the abstract class asks for.
pub fn view_through(
    index: &AbstractIndex,
    abstract_class: &str,
    abstract_shape: &ModelShape,
    concrete_class: &str,
    concrete: &ModelShape,
) -> Option<ModelShape> {
    let relation_names = |shape: &ModelShape| -> HashSet<String> {
        shape
            .include_relations
            .iter()
            .map(|r| r.name.clone())
            .collect()
    };
    let concrete_relations = relation_names(concrete);
    let mut view = concrete.clone();
    for p in &mut view.properties {
        p.name = index
            .name_through(concrete_class, &p.name, abstract_class)
            .to_string();
    }
    for r in &mut view.include_relations {
        r.name = index
            .name_through(concrete_class, &r.name, abstract_class)
            .to_string();
    }
    // Two properties answering to one name would make every read of it a
    // coin toss. Registration refuses it; a shape that arrived another way
    // (sync, raw links) is left out rather than read wrongly.
    let mut seen = HashSet::new();
    if let Some(dup) = view.properties.iter().find(|p| !seen.insert(&p.name)) {
        log::warn!(
            "abstract class '{abstract_class}': skipping '{concrete_class}', two of its \
             properties answer to '{}'",
            dup.name
        );
        return None;
    }

    let view_relations = relation_names(&view);
    let abstract_relations = relation_names(abstract_shape);
    let has_property = |name: &str| {
        view.properties
            .iter()
            .zip(&concrete.properties)
            .any(|(v, c)| v.name == name && !concrete_relations.contains(&c.name))
    };
    let conforms = abstract_shape
        .properties
        .iter()
        .filter(|p| !abstract_relations.contains(&p.name))
        .all(|p| has_property(&p.name))
        && abstract_relations
            .iter()
            .all(|name| view_relations.contains(name));
    conforms.then_some(view)
}

/// Every installed concrete class that conforms to `abstract_class`, with its
/// view.
///
/// A class whose shape does not load is skipped with a warning rather than
/// failing the query: one broken class must not hide every other app's rows.
pub fn resolve(
    store: &SparqlStore,
    resolver: &dyn ShapeResolver,
    index: &AbstractIndex,
    abstract_class: &str,
) -> Result<Resolution, Error> {
    let abstract_shape = resolver.get_shape(abstract_class)?;
    let mut resolution = Resolution::default();
    // The classifier's list: an abstract class never conforms to another.
    for class in concrete_subject_class_names(store)? {
        let shape = match resolver.get_shape(&class) {
            Ok(shape) => shape,
            Err(e) => {
                log::warn!(
                    "abstract class '{abstract_class}': skipping class '{class}', its shape \
                     did not load: {e}"
                );
                continue;
            }
        };
        if let Some(view) = view_through(index, abstract_class, &abstract_shape, &class, &shape) {
            resolution.views.insert(class.clone(), Arc::new(view));
            resolution.classes.push(class);
        }
    }
    Ok(resolution)
}

/// A resolver that answers with the views of an abstract class's classes,
/// and with `inner`'s shapes for every other class. For a subscription's
/// trigger, whose `where` keys are the abstract class's names.
pub struct ViewResolver<'a> {
    inner: &'a dyn ShapeResolver,
    views: &'a HashMap<String, Arc<ModelShape>>,
}

impl<'a> ViewResolver<'a> {
    pub fn new(inner: &'a dyn ShapeResolver, views: &'a HashMap<String, Arc<ModelShape>>) -> Self {
        ViewResolver { inner, views }
    }
}

impl ShapeResolver for ViewResolver<'_> {
    fn get_shape(&self, class_name: &str) -> Result<Arc<ModelShape>, Error> {
        match self.views.get(class_name) {
            Some(view) => Ok(view.clone()),
            None => self.inner.get_shape(class_name),
        }
    }
}

#[cfg(test)]
#[path = "abstract_classes_tests.rs"]
mod tests;
