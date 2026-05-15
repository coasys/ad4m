//! Data types for the model query DSL and internal query execution.
//!
//! This module mirrors the TypeScript query types (`Query`, `WhereCondition`,
//! `IncludeProjection`, etc.) as Rust structs with serde deserialization.  It
//! also defines the internal shape metadata types ([`ModelShape`],
//! [`ShapeProperty`], [`ShapeRelation`]) and the query execution plan
//! ([`InstanceQueryPlan`]).

use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};

// ---------------------------------------------------------------------------
// Query DSL types (mirrors TS types.ts)
// ---------------------------------------------------------------------------

/// Comparison operators for where-clause conditions.
///
/// Used inside [`WhereCondition::Ops`] to express range queries, negation,
/// and substring matching.  Multiple fields can be combined (e.g. `gt` + `lt`
/// for an open range).
#[derive(Debug, Clone, Deserialize)]
pub struct WhereOps {
    #[serde(default)]
    pub not: Option<Value>,
    #[serde(default)]
    pub between: Option<(f64, f64)>,
    #[serde(default)]
    pub lt: Option<f64>,
    #[serde(default)]
    pub lte: Option<f64>,
    #[serde(default)]
    pub gt: Option<f64>,
    #[serde(default)]
    pub gte: Option<f64>,
    #[serde(default)]
    pub contains: Option<Value>,
}

impl Default for WhereOps {
    fn default() -> Self {
        WhereOps {
            not: None,
            between: None,
            lt: None,
            lte: None,
            gt: None,
            gte: None,
            contains: None,
        }
    }
}

/// A single where-clause condition.
///
/// Deserialized from JSON with `#[serde(untagged)]` — the variant is inferred
/// from the JSON value's type:
/// - `"active"` → [`String`](WhereCondition::String)
/// - `42.0` → [`Number`](WhereCondition::Number)
/// - `true` → [`Bool`](WhereCondition::Bool)
/// - `["a","b"]` → [`StringArray`](WhereCondition::StringArray) (IN operator)
/// - `[1,2,3]` → [`NumberArray`](WhereCondition::NumberArray) (IN operator)
/// - `{"gt": 5, "lt": 10}` → [`Ops`](WhereCondition::Ops)
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum WhereCondition {
    String(String),
    Number(f64),
    Bool(bool),
    StringArray(Vec<String>),
    NumberArray(Vec<f64>),
    Ops(WhereOps),
}

/// Sort direction for ORDER BY clauses.
#[derive(Debug, Clone, Copy, Deserialize, PartialEq)]
pub enum OrderDirection {
    ASC,
    DESC,
}

/// Custom serde deserializer for the `order` field.
///
/// Accepts two JSON shapes that the TS client may send:
/// - Tuple array: `[["name", "ASC"], ["age", "DESC"]]`
/// - Object map: `{"name": "ASC", "age": "DESC"}`
///
/// Top-level queries typically send the tuple form; sub-queries inside
/// `include` may send the object form.
fn deserialize_order_flex<'de, D>(
    deserializer: D,
) -> Result<Option<Vec<(String, OrderDirection)>>, D::Error>
where
    D: Deserializer<'de>,
{
    let val: Option<Value> = Option::deserialize(deserializer)?;
    let val = match val {
        Some(v) => v,
        None => return Ok(None),
    };
    match val {
        // Array of [key, direction] tuples
        Value::Array(arr) => {
            let mut out = Vec::new();
            for item in arr {
                if let Value::Array(pair) = item {
                    if pair.len() != 2 {
                        return Err(serde::de::Error::custom(
                            "order entry must be a [key, direction] pair",
                        ));
                    }
                    let key = pair[0]
                        .as_str()
                        .ok_or_else(|| serde::de::Error::custom("order key must be a string"))?
                        .to_string();
                    if key.is_empty() {
                        return Err(serde::de::Error::custom("order key must not be empty"));
                    }
                    let dir_str = pair[1].as_str().unwrap_or("ASC");
                    let dir = if dir_str.eq_ignore_ascii_case("desc") {
                        OrderDirection::DESC
                    } else {
                        OrderDirection::ASC
                    };
                    out.push((key, dir));
                } else {
                    return Err(serde::de::Error::custom("order entry must be an array"));
                }
            }
            Ok(Some(out))
        }
        // Object map { key: direction }
        Value::Object(map) => {
            let mut out = Vec::new();
            for (key, dir_val) in map {
                if key.is_empty() {
                    return Err(serde::de::Error::custom("order key must not be empty"));
                }
                let dir_str = dir_val
                    .as_str()
                    .ok_or_else(|| serde::de::Error::custom("order direction must be a string"))?;
                let dir = if dir_str.eq_ignore_ascii_case("desc") {
                    OrderDirection::DESC
                } else {
                    OrderDirection::ASC
                };
                out.push((key, dir));
            }
            Ok(Some(out))
        }
        _ => Ok(None),
    }
}

/// Parent scope for scoped queries.
///
/// When a query targets instances that are children of a specific parent
/// (e.g. "all Messages belonging to Channel X"), the parent scope constrains
/// the SPARQL query with an additional triple pattern.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum ParentScope {
    Model {
        model: String,
        id: String,
        field: Option<String>,
    },
    Raw {
        id: String,
        predicate: String,
    },
}

/// Value in the `include` map for eager-loading relations.
///
/// - `Bool(true)` — include with default sub-query
/// - `SubQuery(...)` — include with a custom nested query (supports where,
///   order, limit, and further nested includes)
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum IncludeValue {
    Bool(bool),
    SubQuery(Box<ModelQueryInput>),
}

/// Configuration for a single projection key (mirrors TS `IncludeProjection`).
///
/// Projections are lightweight aggregations that begin with `$` in the query
/// object.  They compute either a count or a filtered list of related IRIs
/// using a single grouped SPARQL query per key.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ProjectionInput {
    /// The relation name on the parent model to project over.
    pub from: String,
    /// When true, attach a count (integer) instead of a list.
    #[serde(default)]
    pub count: bool,
    /// Optional target class shape for resolving where-clause predicates.
    #[serde(default)]
    pub target_shape: Option<Value>,
    /// Optional where filter applied against target instance properties.
    #[serde(default, rename = "where")]
    pub where_clause: Option<BTreeMap<String, WhereCondition>>,
    /// Limit the number of linked results (when 1, value is unwrapped to scalar).
    pub limit: Option<usize>,
    /// Order results before limiting.
    #[serde(default, deserialize_with = "deserialize_order_flex")]
    pub order: Option<Vec<(String, OrderDirection)>>,
}

/// The structured query input (mirrors the TS `Query<T>` type).
///
/// This is the top-level request object deserialized from the JSON that
/// the TypeScript client sends.  It supports filtering (`where`), sorting
/// (`order`), pagination (`limit`/`offset`), eager-loading (`include`),
/// projections, and property selection.
#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ModelQueryInput {
    #[serde(default)]
    pub parent: Option<ParentScope>,
    #[serde(default)]
    pub properties: Option<Vec<String>>,
    #[serde(default)]
    pub include: Option<HashMap<String, IncludeValue>>,
    /// Projection keys (begin with `$`): lightweight aggregations/lists that
    /// are computed Rust-side with a single grouped SPARQL per key.
    #[serde(default)]
    pub projections: Option<HashMap<String, ProjectionInput>>,
    #[serde(default, rename = "where")]
    pub where_clause: Option<BTreeMap<String, WhereCondition>>,
    #[serde(default, deserialize_with = "deserialize_order_flex")]
    pub order: Option<Vec<(String, OrderDirection)>>,
    #[serde(default)]
    pub offset: Option<usize>,
    #[serde(default)]
    pub limit: Option<usize>,
    #[serde(default)]
    pub count: Option<bool>,
    /// When true, evaluate **property** getters (@Property with `getter`) during
    /// hydration. Relation conformance getters always run regardless.
    /// Defaults to true — property getters are evaluated post-pagination via
    /// batched VALUES queries (O(M) cost).  Set to false to skip them.
    #[serde(default, rename = "deepQuery")]
    pub deep_query: Option<bool>,
}

/// Result returned by the model query endpoint.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ModelQueryResult {
    pub instances: Vec<Value>,
    pub total_count: usize,
}

/// Parameters for SPARQL-side pagination (pushed ORDER BY + LIMIT + OFFSET).
pub(super) struct SparqlPagination {
    pub(super) sort_key: SortKey,
    pub(super) direction: OrderDirection,
    pub(super) offset: Option<usize>,
    pub(super) limit: Option<usize>,
}

/// What to sort by when pagination is pushed to SPARQL.
pub(super) enum SortKey {
    /// Sort by reifier timestamp (MIN(?_ts) per source).
    Timestamp,
    /// Sort by a property value extracted from its literal IRI.
    Property(String), // predicate IRI
}

// ---------------------------------------------------------------------------
// Internal shape metadata (derived from SHACL links or client JSON)
// ---------------------------------------------------------------------------

/// A single property or relation declared in a model class's shape.
///
/// Constructed either by reading SHACL triples from the store
/// ([`super::shape::load_shape`]) or by parsing the JSON metadata sent
/// alongside the query ([`super::shape::parse_shape_from_json`]).
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(super) struct ShapeProperty {
    pub(super) name: String,
    pub(super) predicate: String,
    pub(super) is_collection: bool,
    pub(super) is_flag: bool,
    pub(super) is_required: bool,
    pub(super) initial_value: Option<String>,
    pub(super) resolve_language: Option<String>,
    pub(super) datatype: Option<String>,
    pub(super) direction: Option<String>, // "forward" or "reverse" for relation properties
    pub(super) is_scalar_relation: bool, // true for hasOne/belongsToOne (render as scalar, not array)
    /// SPARQL getter expression (e.g. `SELECT ?value WHERE { ... }` or `ASK WHERE { ... }`).
    /// For properties: returns a scalar value.
    /// For relations: returns target IDs (conformance-filtered).
    pub(super) getter: Option<String>,
    /// Post-getter where-clause filter for relations.  Used to apply
    /// where conditions on related instances after the getter runs,
    /// by fetching the target property values and comparing the parsed data.
    pub(super) where_filter: Option<BTreeMap<String, WhereCondition>>,
    /// Predicate mappings for `where_filter` (property name → predicate IRI).
    pub(super) where_predicates: Option<HashMap<String, String>>,
}

/// Enriched relation metadata for include (eager-loading) resolution.
/// Populated when the TS client sends target class shapes alongside the query.
#[derive(Debug, Clone)]
pub(super) struct ShapeRelation {
    pub(super) name: String,
    pub(super) predicate: String,
    pub(super) direction: String, // "forward" or "reverse"
    pub(super) kind: String,      // "hasMany", "hasOne", "belongsToOne", "belongsToMany"
    pub(super) max_count: Option<usize>,
    pub(super) target_class_name: String,
    pub(super) target_shape_json: String, // Serialised ModelMetadata JSON for recursive queries
}

/// Complete shape of a model class — the set of all properties, relations,
/// and include metadata needed to query, hydrate, and enrich instances.
///
/// This is the central metadata object threaded through the entire query
/// pipeline.
#[derive(Debug)]
#[allow(dead_code)]
pub(crate) struct ModelShape {
    pub(super) target_class: String,
    #[allow(dead_code)]
    pub(super) shape_uri: String,
    pub(super) properties: Vec<ShapeProperty>,
    /// Enriched relation metadata for include resolution (only populated
    /// when the TS client sends target shapes for included relations).
    pub(super) include_relations: Vec<ShapeRelation>,
}

impl ModelShape {
    /// Returns all predicate IRIs declared in this shape (properties + relations + flags).
    pub fn predicates(&self) -> Vec<String> {
        let mut preds: Vec<String> = self
            .properties
            .iter()
            .filter(|p| !p.predicate.is_empty())
            .map(|p| p.predicate.clone())
            .collect();
        for r in &self.include_relations {
            if !r.predicate.is_empty() {
                preds.push(r.predicate.clone());
            }
        }
        preds.sort();
        preds.dedup();
        preds
    }
}

/// SPARQL execution plan for an instance query.
///
/// For non-paginated queries, a single SPARQL `SELECT` fetches all matching
/// rows.  For paginated queries we use a **two-phase** approach because
/// Oxigraph's query planner doesn't push nested sub-queries with `ORDER BY`
/// + `LIMIT` down efficiently (O(N * total_triples) vs O(page_size) lookups):
///
/// 1. **Phase 1** — A lightweight pagination sub-query retrieves just the
///    source IRIs in sorted/limited order.
/// 2. **Phase 2** — A `VALUES ?source { ... }` property query fetches all
///    triples for those specific instances.
pub(super) enum InstanceQueryPlan {
    /// Single query -- no pagination or non-paginated query.
    Single(String),
    /// Two-phase: (pagination_subquery, predicate_filter, conformance, where_extra).
    /// Phase 1: execute pagination_subquery -> get source IRIs.
    /// Phase 2: build property query with VALUES ?source { ... }.
    TwoPhase {
        pagination_subquery: String,
        predicate_filter: String,
    },
}

impl InstanceQueryPlan {
    /// Extract the SPARQL string for non-paginated queries (Single variant).
    /// Panics for TwoPhase. Used only in unit tests.
    #[cfg(test)]
    pub(super) fn into_single(self) -> String {
        match self {
            InstanceQueryPlan::Single(s) => s,
            InstanceQueryPlan::TwoPhase { .. } => {
                panic!("Expected Single query plan, got TwoPhase")
            }
        }
    }
}

/// Intermediate representation of all RDF links belonging to one instance.
///
/// Produced by [`super::hydration::group_results_by_source`] from raw SPARQL
/// result rows, then consumed by [`super::hydration::hydrate_one`] to build
/// a fully typed JSON object.
#[derive(Debug)]
pub(super) struct InstanceLinks {
    pub(super) source: String,
    /// (predicate, target, author, timestamp) for each link
    pub(super) links: Vec<(String, String, String, String)>,
}
