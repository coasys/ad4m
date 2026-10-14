//! Interface (structural) types for model query (#1435).
//!
//! An interface names what a class must have, not which class it is. A model
//! query over an interface reads every installed class that has those
//! properties and relations, as one union (`execute_union_query`), so a
//! caller can ask for "anything with `text`" without knowing which app wrote
//! it or what it named its classes.
//!
//! ```text
//!  { interface: 'Message' }  ──▶ load_interface ──┐
//!                                                 ├─▶ InterfaceSpec ──▶ classes_implementing
//!  { implements: {...} }     ──▶ normalise ───────┘        │        (every installed class's
//!                                                          │         shape, conformance rule)
//!                                                          ▼
//!                                       QueryClasses::union ──▶ execute_union_query
//! ```
//!
//! Conformance (class C implements interface I), checked on shapes only,
//! never on instances:
//!
//! | interface part            | matches when                                  |
//! |---------------------------|-----------------------------------------------|
//! | property, name only       | C has a property with that name (loose)       |
//! | property with `through`   | ... and its predicate is `through` (strict)   |
//! | property with `datatype`  | ... and its datatype is `datatype` (strict)   |
//! | relation                  | C has a relation through that predicate, in   |
//! |                           | that direction (forward by default)           |
//!
//! A property and a relation are told apart as the shape loader tells them
//! apart: a relation has a relation kind or a target class. A getter-backed
//! property counts as a property by its name. `sh://node` (model inheritance)
//! is not read: a class implements an interface by its shape alone.
//!
//! The resolved class list is derived from the installed SHACL, so it is
//! cached beside the shape cache and dropped whenever that is
//! (`PerspectiveInstance::classes_implementing`).

use super::shape::decode_literal_string_target;
use super::types::{ModelShape, ShapeResolver};
use super::union::QueryClasses;
use super::utils::escape_sparql_string;
use crate::perspectives::shacl_parser::INTERFACE_TYPE;
use crate::perspectives::sparql_store::SparqlStore;
use crate::perspectives::subject_classes_of::all_subject_class_names;
use deno_core::anyhow::{anyhow, Error};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use ts_rs::TS;

/// A property an interface asks for. `through` and `datatype` make the match
/// strict; without them a property matches by name alone.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Deserialize, Serialize, TS)]
#[serde(deny_unknown_fields)]
#[ts(export)]
pub struct InterfaceProperty {
    pub name: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub through: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub datatype: Option<String>,
}

/// Which way a relation's links point from the instance: `forward` is
/// `instance -through-> target` (`hasMany`/`hasOne`), `reverse` is
/// `source -through-> instance` (`belongsTo*`).
#[derive(
    Debug, Clone, Copy, Default, PartialEq, Eq, Hash, PartialOrd, Ord, Deserialize, Serialize, TS,
)]
#[serde(rename_all = "lowercase")]
#[ts(export)]
pub enum RelationDirection {
    #[default]
    Forward,
    Reverse,
}

impl RelationDirection {
    fn as_str(self) -> &'static str {
        match self {
            RelationDirection::Forward => "forward",
            RelationDirection::Reverse => "reverse",
        }
    }
}

/// A relation an interface asks for. Matched by predicate and direction only:
/// relation names mean nothing across apps, so `name` is documentation.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Deserialize, Serialize, TS)]
#[serde(deny_unknown_fields)]
#[ts(export)]
pub struct InterfaceRelation {
    pub through: String,
    #[serde(default)]
    #[ts(as = "Option<RelationDirection>", optional)]
    pub direction: RelationDirection,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub name: Option<String>,
}

/// A property in the ad-hoc `implements` form: a bare name, or the pinned form.
#[derive(Debug, Clone, Deserialize, TS)]
#[serde(untagged)]
#[ts(export)]
pub enum InterfacePropertyInput {
    Name(String),
    Pinned(InterfaceProperty),
}

/// The ad-hoc `implements` parameter of `modelQuery` / `modelSubscribe`.
#[derive(Debug, Clone, Default, Deserialize, TS)]
#[serde(deny_unknown_fields)]
#[ts(export)]
pub struct ImplementsInput {
    #[serde(default)]
    #[ts(optional)]
    pub properties: Option<Vec<InterfacePropertyInput>>,
    #[serde(default)]
    #[ts(optional)]
    pub relations: Option<Vec<InterfaceRelation>>,
}

/// What a class must have to implement an interface. Kept sorted and without
/// duplicates, so two specs that ask for the same thing are equal and share
/// one cache entry.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct InterfaceSpec {
    pub properties: Vec<InterfaceProperty>,
    pub relations: Vec<InterfaceRelation>,
}

impl InterfaceSpec {
    pub fn new(
        mut properties: Vec<InterfaceProperty>,
        mut relations: Vec<InterfaceRelation>,
    ) -> Result<Self, Error> {
        if properties.is_empty() && relations.is_empty() {
            return Err(anyhow!(
                "implements: name at least one property or relation — an empty interface \
                 would match every class"
            ));
        }
        if let Some(p) = properties.iter().find(|p| p.name.is_empty()) {
            return Err(anyhow!("implements: a property needs a name, got {p:?}"));
        }
        if let Some(r) = relations.iter().find(|r| r.through.is_empty()) {
            return Err(anyhow!(
                "implements: a relation needs `through` (relations match by predicate), got {r:?}"
            ));
        }
        properties.sort();
        properties.dedup();
        relations.sort();
        relations.dedup();
        Ok(InterfaceSpec {
            properties,
            relations,
        })
    }
}

impl TryFrom<ImplementsInput> for InterfaceSpec {
    type Error = Error;

    fn try_from(input: ImplementsInput) -> Result<Self, Error> {
        let properties = input
            .properties
            .unwrap_or_default()
            .into_iter()
            .map(|p| match p {
                InterfacePropertyInput::Name(name) => InterfaceProperty {
                    name,
                    through: None,
                    datatype: None,
                },
                InterfacePropertyInput::Pinned(p) => p,
            })
            .collect();
        InterfaceSpec::new(properties, input.relations.unwrap_or_default())
    }
}

/// An interface as a query names it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InterfaceRef {
    /// Registered with `addSdna(name, …, "interface", shacl)`.
    Named(String),
    /// Given inline as `implements`.
    AdHoc(InterfaceSpec),
}

/// What a model query reads: classes by name, or every class that implements
/// an interface.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassSelector {
    Classes(QueryClasses),
    Interface(InterfaceRef),
}

impl From<QueryClasses> for ClassSelector {
    fn from(classes: QueryClasses) -> Self {
        ClassSelector::Classes(classes)
    }
}

/// No interface of this name is stored. Typed so the caller can wait for it
/// on a shared perspective, as it waits for a class's shape.
#[derive(Debug)]
pub(crate) struct MissingInterface {
    pub(crate) name: String,
}

impl std::fmt::Display for MissingInterface {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "No interface '{}' stored. Register it with addSdna(name, '', 'interface', shacl) first.",
            self.name
        )
    }
}

impl std::error::Error for MissingInterface {}

/// Read a named interface back from its SHACL triples (written by
/// `parse_interface_shacl_to_links`).
pub(crate) fn load_interface(store: &SparqlStore, name: &str) -> Result<InterfaceSpec, Error> {
    let safe_name = escape_sparql_string(name);
    // The same name match `load_shape` uses for a class.
    let query = format!(
        r##"
        SELECT ?shapeUri WHERE {{
            ?target <rdf://type> <{INTERFACE_TYPE}> .
            ?target <ad4m://shape> ?shapeUri .
            FILTER(STRENDS(STR(?target), "/{safe_name}") || STRENDS(STR(?target), "#{safe_name}") || STR(?target) = "{safe_name}")
        }}
        LIMIT 1
        "##
    );
    let rows: Vec<Value> = serde_json::from_str(&store.query(&query)?)?;
    let Some(shape_uri) = rows.first().and_then(|r| r["shapeUri"].as_str()) else {
        return Err(MissingInterface {
            name: name.to_string(),
        }
        .into());
    };

    let props_query = format!(
        r#"
        SELECT ?propUri ?path ?datatype ?relationKind WHERE {{
            <{shape_uri}> <sh://property> ?propUri .
            OPTIONAL {{ ?propUri <sh://path> ?path . }}
            OPTIONAL {{ ?propUri <sh://datatype> ?datatype . }}
            OPTIONAL {{ ?propUri <ad4m://relationKind> ?relationKind . }}
        }}
        "#
    );
    let rows: Vec<Value> = serde_json::from_str(&store.query(&props_query)?)?;
    let mut properties = Vec::new();
    let mut relations = Vec::new();
    for row in &rows {
        let prop_uri = row["propUri"].as_str().unwrap_or("");
        // `namespace://Interface.propName`, as the writer names it.
        let prop_name = prop_uri
            .rsplit_once('.')
            .map(|(_, n)| n.to_string())
            .unwrap_or_default();
        let path = row["path"].as_str().map(str::to_string);
        match row["relationKind"].as_str().map(decode_literal_string_target) {
            Some(kind) => {
                let Some(through) = path else {
                    return Err(anyhow!(
                        "Interface '{name}': relation '{prop_name}' has no sh://path"
                    ));
                };
                relations.push(InterfaceRelation {
                    through,
                    direction: match kind.as_str() {
                        "belongsToOne" | "belongsToMany" => RelationDirection::Reverse,
                        _ => RelationDirection::Forward,
                    },
                    name: Some(prop_name),
                });
            }
            None => properties.push(InterfaceProperty {
                name: prop_name,
                through: path,
                datatype: row["datatype"].as_str().map(str::to_string),
            }),
        }
    }
    InterfaceSpec::new(properties, relations).map_err(|e| anyhow!("Interface '{name}': {e}"))
}

/// `xsd:string`, `xsd://string` and the full XML Schema IRI are one datatype.
fn datatype_key(datatype: &str) -> &str {
    for prefix in ["http://www.w3.org/2001/XMLSchema#", "xsd://", "xsd:"] {
        if let Some(local) = datatype.strip_prefix(prefix) {
            return local;
        }
    }
    datatype
}

/// Whether `shape` has everything `spec` asks for (the table in the module
/// docs).
pub fn implements(shape: &ModelShape, spec: &InterfaceSpec) -> bool {
    let is_relation = |name: &str| shape.include_relations.iter().any(|r| r.name == name);
    let has_property = |want: &InterfaceProperty| {
        shape.properties.iter().any(|p| {
            p.name == want.name
                && !is_relation(&p.name)
                && want.through.as_ref().is_none_or(|t| *t == p.predicate)
                && want.datatype.as_deref().is_none_or(|want_dt| {
                    p.datatype
                        .as_deref()
                        .is_some_and(|dt| datatype_key(dt) == datatype_key(want_dt))
                })
        })
    };
    let has_relation = |want: &InterfaceRelation| {
        shape
            .include_relations
            .iter()
            .any(|r| r.predicate == want.through && r.direction == want.direction.as_str())
    };
    spec.properties.iter().all(has_property) && spec.relations.iter().all(has_relation)
}

/// Every installed class that implements `spec`, sorted by name.
///
/// A class whose shape does not load is skipped with a warning rather than
/// failing the query: one broken class must not hide every other app's rows.
pub fn classes_implementing(
    store: &SparqlStore,
    resolver: &dyn ShapeResolver,
    spec: &InterfaceSpec,
) -> Result<Vec<String>, Error> {
    let mut classes = Vec::new();
    for class_name in all_subject_class_names(store)? {
        match resolver.get_shape(&class_name) {
            Ok(shape) if implements(&shape, spec) => classes.push(class_name),
            Ok(_) => {}
            Err(e) => log::warn!(
                "classes_implementing: skipping class '{class_name}', its shape did not load: {e}"
            ),
        }
    }
    Ok(classes)
}

#[cfg(test)]
#[path = "interfaces_tests.rs"]
mod tests;
