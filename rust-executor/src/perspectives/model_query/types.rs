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
///
/// `deny_unknown_fields` ensures that when `#[serde(untagged)]` tries this
/// variant, objects that contain non-WhereOps keys (e.g. a nested where clause
/// like `{"name": "Alice"}`) are rejected and fall through to `SubClause`.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
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
/// - `[{"name":"Alice"},{"name":"Bob"}]` → [`SubClauses`](WhereCondition::SubClauses) (OR / AND)
/// - `{"name":"Alice","status":"active"}` → [`SubClause`](WhereCondition::SubClause) (NOT)
///
/// **Variant ordering** is significant for `#[serde(untagged)]`:
/// - `Bool` before `Number` so JSON booleans don't coerce to `f64`.
/// - `StringArray` / `NumberArray` before `SubClauses` so empty arrays `[]`
///   become `StringArray([])` rather than `SubClauses([])`.
/// - `Ops` (with `deny_unknown_fields`) before `SubClause` so operator objects
///   like `{"gt": 5}` match `Ops` and where-clause objects like `{"name":"Alice"}`
///   fall through to `SubClause`.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum WhereCondition {
    Bool(bool),
    Number(f64),
    String(String),
    StringArray(Vec<String>),
    NumberArray(Vec<f64>),
    /// Array of nested where clauses used for OR and AND combinators.
    /// The key name in the parent `BTreeMap` determines the semantic:
    /// `"OR"` → any branch must pass; `"AND"` → all branches must pass.
    SubClauses(Vec<BTreeMap<String, WhereCondition>>),
    /// Operator-based condition (`gt`, `lt`, `contains`, `not`, etc.).
    /// `WhereOps` uses `deny_unknown_fields` so objects whose keys are not
    /// WhereOps fields are rejected here and fall through to `SubClause`.
    Ops(WhereOps),
    /// Single nested where clause used for the NOT combinator.
    SubClause(BTreeMap<String, WhereCondition>),
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
    /// Bare class name of the projection target.  The executor resolves the
    /// target shape from this through its in-memory cache when projection
    /// where-clauses reference target properties by name.
    #[serde(default)]
    pub target_class_name: Option<String>,
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
    /// Sort by a projection count — uses `COUNT(DISTINCT ?_proj_t)` in the
    /// GROUP BY pagination subquery.  The string is the relation predicate IRI
    /// used to reach the counted items.
    Projection(String), // relation predicate IRI
    /// Sort by a scalar property on a directly-related model instance.
    /// Emits a double-OPTIONAL join in the pagination subquery:
    /// `?source <rel_pred> ?_rel . ?_rel <prop_pred> ?_sort_val`.
    RelationProperty { rel_pred: String, prop_pred: String },
}

// ---------------------------------------------------------------------------
// Internal shape metadata (derived from SHACL links or client JSON)
// ---------------------------------------------------------------------------

/// Transform expression for property values (SHACL-AF Node Expression).
/// Applied in the Rust executor during hydration for resolveLanguage properties.
#[derive(Debug, Clone, Deserialize, PartialEq)]
#[serde(tag = "type", rename_all = "camelCase")]
pub(crate) enum TransformExpression {
    Focus,
    Literal {
        value: serde_json::Value,
    },
    Path {
        predicate: String,
    },
    Exists {
        expr: Box<TransformExpression>,
    },
    If {
        cond: Box<TransformExpression>,
        then: Box<TransformExpression>,
        #[serde(rename = "else")]
        else_expr: Option<Box<TransformExpression>>,
    },
    Concat {
        args: Vec<TransformExpression>,
    },
    Coalesce {
        args: Vec<TransformExpression>,
    },
    Function {
        iri: String,
        args: Vec<TransformExpression>,
    },
}

/// Default file decode transform: converts FileData objects to data: URIs.
///
/// Represents: `if (exists(path('data_base64'))) then concat(...) else focus()`
pub(crate) fn default_file_decode() -> TransformExpression {
    TransformExpression::If {
        cond: Box::new(TransformExpression::Exists {
            expr: Box::new(TransformExpression::Path {
                predicate: "data_base64".to_string(),
            }),
        }),
        then: Box::new(TransformExpression::Concat {
            args: vec![
                TransformExpression::Literal {
                    value: Value::String("data:".to_string()),
                },
                TransformExpression::Coalesce {
                    args: vec![
                        TransformExpression::Path {
                            predicate: "file_type".to_string(),
                        },
                        TransformExpression::Literal {
                            value: Value::String("image/png".to_string()),
                        },
                    ],
                },
                TransformExpression::Literal {
                    value: Value::String(";base64,".to_string()),
                },
                TransformExpression::Path {
                    predicate: "data_base64".to_string(),
                },
            ],
        }),
        else_expr: Some(Box::new(TransformExpression::Focus)),
    }
}

/// A single property or relation declared in a model class's shape.
///
/// Constructed by reading SHACL triples from the store
/// ([`super::shape::load_shape`]).
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct ShapeProperty {
    pub(crate) name: String,
    pub(crate) predicate: String,
    pub(crate) is_collection: bool,
    pub(crate) is_flag: bool,
    pub(crate) is_required: bool,
    pub(crate) initial_value: Option<String>,
    pub(crate) resolve_language: Option<String>,
    pub(crate) datatype: Option<String>,
    pub(crate) direction: Option<String>, // "forward" or "reverse" for relation properties
    pub(crate) is_scalar_relation: bool, // true for hasOne/belongsToOne (render as scalar, not array)
    /// SPARQL getter expression (e.g. `SELECT ?value WHERE { ... }` or `ASK WHERE { ... }`).
    /// For properties: returns a scalar value.
    /// For relations: returns target IDs (conformance-filtered).
    pub(crate) getter: Option<String>,
    /// Post-getter where-clause filter for relations.  Used to apply
    /// where conditions on related instances after the getter runs,
    /// by fetching the target property values and comparing the parsed data.
    pub(crate) where_filter: Option<BTreeMap<String, WhereCondition>>,
    /// Predicate mappings for `where_filter` (property name → predicate IRI).
    pub(crate) where_predicates: Option<HashMap<String, String>>,
    /// Transform expression (SHACL-AF Node Expression).
    /// Applied in hydration for resolveLanguage properties.
    pub(super) transform: Option<TransformExpression>,
    /// Natural-language hint describing this property's meaning, read back from
    /// the `ad4m://extraction_hint` link on the property node.  Surfaced so the
    /// generic LLM extractor (and MCP tool-schema generation) can inject it as
    /// semantic guidance.  `None` when the SDNA declared no hint.
    pub(crate) extraction_hint: Option<String>,
    /// Whether this property is the class's dedup identity (its title-like
    /// interpretation key), read back from the `ad4m://identity` link on the
    /// property node.  `false` when the SDNA declared no identity — a class
    /// with no identity property is never deduplicated.
    pub(crate) identity: bool,
}

/// Enriched relation metadata for include (eager-loading) resolution.
/// Target shapes are resolved at recursion time through a [`ShapeResolver`]
/// against the perspective's in-memory shape cache.
#[derive(Debug, Clone)]
pub struct ShapeRelation {
    pub(crate) name: String,
    pub(crate) predicate: String,
    pub(crate) direction: String, // "forward" or "reverse"
    pub(crate) kind: String,      // "hasMany", "hasOne", "belongsToOne", "belongsToMany"
    pub(crate) max_count: Option<usize>,
    pub(crate) target_class_name: String,
    /// Original `sh:class` URI as emitted by the SHACL writer.  Preserved
    /// alongside `target_class_name` so the MCP layer (which exposes the
    /// node-shape URI to clients) can return the full IRI rather than the
    /// bare class name.  Empty when no `sh:class` was set on the property.
    pub(crate) target_class_uri: String,
}

/// Complete shape of a model class — the set of all properties, relations,
/// and include metadata needed to query, hydrate, and enrich instances.
///
/// This is the central metadata object threaded through the entire query
/// pipeline.  Built once from SHACL triples in the perspective's store and
/// memoized in `PerspectiveInstance::shape_cache`.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct ModelShape {
    pub(crate) target_class: String,
    #[allow(dead_code)]
    pub(crate) shape_uri: String,
    pub(crate) properties: Vec<ShapeProperty>,
    /// Enriched relation metadata for include resolution, populated
    /// directly from the perspective's SHACL triples.
    pub(crate) include_relations: Vec<ShapeRelation>,
    /// Class-level natural-language hint, read back from the
    /// `ad4m://extraction_hint` link on the shape node.  Steers the generic
    /// LLM extractor toward what instances of this class represent.  `None`
    /// when the SDNA declared no class hint.
    pub(crate) extraction_hint: Option<String>,
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

/// Resolves shapes by class name.  Implementations memoize results so that
/// repeated lookups (including recursive include traversal across the same
/// class) parse SHACL at most once per process lifetime.
///
/// The concrete production implementation lives on `PerspectiveInstance`
/// and reads from the perspective's `shape_cache` + SHACL store.  Tests
/// typically supply a static implementation pre-populated with `ModelShape`
/// instances.
pub trait ShapeResolver: Send + Sync {
    fn get_shape(
        &self,
        class_name: &str,
    ) -> Result<std::sync::Arc<ModelShape>, deno_core::anyhow::Error>;
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

#[cfg(test)]
mod where_condition_deser_tests {
    use super::*;
    use serde_json::from_value;

    #[test]
    fn deser_string() {
        let v: WhereCondition = from_value(serde_json::json!("hello")).unwrap();
        assert!(matches!(v, WhereCondition::String(s) if s == "hello"));
    }

    #[test]
    fn deser_number() {
        let v: WhereCondition = from_value(serde_json::json!(42.0)).unwrap();
        assert!(matches!(v, WhereCondition::Number(n) if (n - 42.0).abs() < f64::EPSILON));
    }

    #[test]
    fn deser_bool() {
        let v: WhereCondition = from_value(serde_json::json!(true)).unwrap();
        assert!(matches!(v, WhereCondition::Bool(true)));
    }

    #[test]
    fn deser_string_array() {
        let v: WhereCondition = from_value(serde_json::json!(["a", "b", "c"])).unwrap();
        assert!(matches!(v, WhereCondition::StringArray(arr) if arr == vec!["a", "b", "c"]));
    }

    #[test]
    fn deser_number_array() {
        let v: WhereCondition = from_value(serde_json::json!([1.0, 2.0, 3.0])).unwrap();
        assert!(matches!(v, WhereCondition::NumberArray(_)));
    }

    #[test]
    fn deser_empty_array_is_string_array() {
        // Empty arrays land on StringArray (first Vec variant), not SubClauses.
        let v: WhereCondition = from_value(serde_json::json!([])).unwrap();
        assert!(matches!(v, WhereCondition::StringArray(arr) if arr.is_empty()));
    }

    #[test]
    fn deser_ops_with_known_fields() {
        let v: WhereCondition = from_value(serde_json::json!({ "gt": 5, "lt": 10 })).unwrap();
        assert!(
            matches!(v, WhereCondition::Ops(ops) if ops.gt == Some(5.0) && ops.lt == Some(10.0))
        );
    }

    #[test]
    fn deser_sub_clauses_for_or_and() {
        // Array of objects → SubClauses (used as OR/AND value)
        let v: WhereCondition =
            from_value(serde_json::json!([{"name": "Alice"}, {"name": "Bob"}])).unwrap();
        match v {
            WhereCondition::SubClauses(branches) => {
                assert_eq!(branches.len(), 2);
                assert!(branches[0].contains_key("name"));
            }
            other => panic!("expected SubClauses, got {other:?}"),
        }
    }

    #[test]
    fn deser_sub_clause_for_not() {
        // Object with non-WhereOps keys → SubClause (used as NOT value)
        let v: WhereCondition =
            from_value(serde_json::json!({"name": "Alice", "status": "active"})).unwrap();
        match v {
            WhereCondition::SubClause(map) => {
                assert_eq!(map.len(), 2);
                assert!(map.contains_key("name"));
                assert!(map.contains_key("status"));
            }
            other => panic!("expected SubClause, got {other:?}"),
        }
    }

    #[test]
    fn deser_where_clause_with_or() {
        // Full where-clause map with OR key
        let json = serde_json::json!({
            "OR": [
                { "name": { "contains": "foo" } },
                { "description": { "contains": "foo" } }
            ]
        });
        let wc: BTreeMap<String, WhereCondition> = serde_json::from_value(json).unwrap();
        assert!(wc.contains_key("OR"));
        let or_val = wc.get("OR").unwrap();
        assert!(matches!(or_val, WhereCondition::SubClauses(b) if b.len() == 2));
    }

    #[test]
    fn deser_where_clause_with_not() {
        let json = serde_json::json!({ "NOT": { "status": "deleted" } });
        let wc: BTreeMap<String, WhereCondition> = serde_json::from_value(json).unwrap();
        assert!(wc.contains_key("NOT"));
        let not_val = wc.get("NOT").unwrap();
        assert!(matches!(not_val, WhereCondition::SubClause(_)));
    }

    #[test]
    fn deser_model_query_with_or_where() {
        let json = serde_json::json!({
            "where": {
                "OR": [
                    { "name": "Alice" },
                    { "name": "Bob" }
                ]
            }
        });
        let mq: ModelQueryInput = serde_json::from_value(json).unwrap();
        let wc = mq.where_clause.unwrap();
        assert!(matches!(wc.get("OR"), Some(WhereCondition::SubClauses(b)) if b.len() == 2));
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
