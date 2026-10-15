//! Abstract model classes (#1435, design agreed on #1440).
//!
//! An abstract class is a model class whose shape carries one
//! [`ABSTRACT_PREDICATE`] marker. It names what a record has, not which class
//! wrote it. A query over it reads every installed concrete class that has
//! all of its names, as one union (`execute_union_query_with_views`).
//!
//! **Conformance** is checked on shapes and names only: a concrete class
//! conforms when it has a property answering to each of the abstract class's
//! property names, and a relation for each relation name. Predicates,
//! datatypes and directions are not compared. A property answers to its own
//! name, or to its alias for this abstract class (one [`ALIAS_PREDICATE`]
//! link, `@Property({ alias: { TextContent: "text" } })`). An abstract shape
//! with no names reads no class.
//!
//! **Views**: read through an abstract class, a concrete class's aliased
//! properties carry the abstract names (`Message.body` comes back as `text`,
//! read through `Message.body`'s predicate). Records reached through an
//! `include` are read as their own class.
//!
//! **Writes** dispatch to the record's own classes (see
//! `perspective_instance::abstract_classes`); [`AbstractIndex::own_name`] is
//! the view read backwards. An abstract class is never created, never one of
//! a record's classes, and never conforms to another abstract class.
//! `@Model({ implements })` is an SDK check; the executor matches by
//! structure.

use super::types::{ModelShape, ShapeResolver};
use crate::perspectives::shacl_parser::{ABSTRACT_PREDICATE, ALIAS_PREDICATE};
use crate::perspectives::sparql_store::SparqlStore;
use crate::perspectives::subject_classes_of::{concrete_subject_class_names, local_name};
use deno_core::anyhow::Error;
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::sync::Arc;

/// The abstract classes and aliases in a perspective's installed SHACL,
/// cached by the perspective until the installed shapes change.
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

    /// The inverse of [`Self::name_through`]: the property or relation of
    /// `class` (with `shape`) that answers to `name` through `abstract_class`.
    pub fn own_name<'a>(
        &self,
        class: &str,
        shape: &'a ModelShape,
        name: &str,
        abstract_class: &str,
    ) -> Option<&'a str> {
        shape
            .properties
            .iter()
            .map(|p| p.name.as_str())
            .chain(shape.include_relations.iter().map(|r| r.name.as_str()))
            .find(|own| self.name_through(class, own, abstract_class) == name)
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
    // With no name to ask for, every class would conform. Registration refuses
    // such a class; a shape that arrived another way (its marker synced
    // before its properties, raw links) matches nothing rather than all.
    if abstract_shape.properties.is_empty() && abstract_shape.include_relations.is_empty() {
        return None;
    }
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
/// view. A class whose shape does not load is skipped with a warning: one
/// broken class must not hide every other app's rows.
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

/// Answers with the views of an abstract class's classes and with `inner`'s
/// shapes otherwise: a subscription trigger's `where` keys are abstract names.
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
