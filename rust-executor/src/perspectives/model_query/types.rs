use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};

// ---------------------------------------------------------------------------
// Query DSL types (mirrors TS types.ts)
// ---------------------------------------------------------------------------

/// Comparison operators for where conditions.
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

/// A single where condition: simple value or operator object.
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

/// Order direction.
#[derive(Debug, Clone, Copy, Deserialize, PartialEq)]
pub enum OrderDirection {
    ASC,
    DESC,
}

/// Deserialize `order` from either `[["key","ASC"]]` (tuple array) or
/// `{"key":"ASC"}` (object map).  Sub-query includes send the object form
/// while the top-level query converts to tuples in TS.
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

/// Include map for eager-loading relations.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum IncludeValue {
    Bool(bool),
    SubQuery(Box<ModelQueryInput>),
}

/// Input for a single projection key (mirrors TS IncludeProjection).
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

/// The structured query input (mirrors TS Query type).
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
// Internal shape info (derived from SHACL links in the store)
// ---------------------------------------------------------------------------

/// A property discovered from SHACL links.
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

/// A model shape reconstructed from SHACL links in the store.
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

/// Represents the SPARQL execution plan for an instance query.
/// Paginated queries use a two-phase approach: first execute the pagination
/// subquery to get source IDs, then build a VALUES-based property query.
/// Oxigraph's query planner doesn't push nested subqueries with ORDER BY +
/// LIMIT down efficiently, resulting in O(N * total_triples) scans instead
/// of O(page_size) lookups.
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

/// An intermediate representation of all links belonging to one instance.
#[derive(Debug)]
pub(super) struct InstanceLinks {
    pub(super) source: String,
    /// (predicate, target, author, timestamp) for each link
    pub(super) links: Vec<(String, String, String, String)>,
}
