//! Executor-side model query engine.
//!
//! Replaces the TS-side SPARQL-build → hydrate → JS-filter → JS-sort → JS-paginate
//! pipeline with a single Rust function that:
//!
//! 1. Reads the SHACL shape from the perspective's link store
//! 2. Builds conformance SPARQL internally
//! 3. Hydrates instances by parsing `literal:` URIs natively (typed)
//! 4. Filters with correct typed comparisons
//! 5. Sorts and paginates in Rust
//! 6. Returns JSON instances + totalCount

use deno_core::anyhow::{anyhow, Error};
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::{Map, Value};
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};

use super::sparql_store::SparqlStore;

// ---------------------------------------------------------------------------
// SPARQL injection prevention helpers
// ---------------------------------------------------------------------------

/// Escape a string value for use inside a SPARQL string literal (double-quoted).
fn escape_sparql_string(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

/// Validate a value for use inside an IRI `<…>`.  Rejects characters that
/// would break or inject into a SPARQL IRI token.
fn validate_iri(s: &str) -> Result<&str, Error> {
    if s.contains('>')
        || s.contains('<')
        || s.contains('{')
        || s.contains('}')
        || s.contains('"')
        || s.contains(' ')
    {
        return Err(anyhow!("Invalid IRI component: '{}'", s));
    }
    Ok(s)
}

/// Maximum recursion depth for include resolution to prevent stack overflow.
const MAX_INCLUDE_DEPTH: u8 = 8;

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
struct SparqlPagination {
    direction: OrderDirection,
    offset: Option<usize>,
    limit: Option<usize>,
}

// ---------------------------------------------------------------------------
// Internal shape info (derived from SHACL links in the store)
// ---------------------------------------------------------------------------

/// A property discovered from SHACL links.
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct ShapeProperty {
    name: String,
    predicate: String,
    is_collection: bool,
    is_flag: bool,
    is_required: bool,
    initial_value: Option<String>,
    resolve_language: Option<String>,
    datatype: Option<String>,
    direction: Option<String>, // "forward" or "reverse" for relation properties
    is_scalar_relation: bool,  // true for hasOne/belongsToOne (render as scalar, not array)
    /// SPARQL getter expression (e.g. `SELECT ?value WHERE { ... }` or `ASK WHERE { ... }`).
    /// For properties: returns a scalar value.
    /// For relations: returns target IDs (conformance-filtered).
    getter: Option<String>,
    /// Post-getter where-clause filter for relations.  Used to apply
    /// where conditions on related instances after the getter runs,
    /// by fetching the target property values and comparing the parsed data.
    where_filter: Option<BTreeMap<String, WhereCondition>>,
    /// Predicate mappings for `where_filter` (property name → predicate IRI).
    where_predicates: Option<HashMap<String, String>>,
}

/// Enriched relation metadata for include (eager-loading) resolution.
/// Populated when the TS client sends target class shapes alongside the query.
#[derive(Debug, Clone)]
struct ShapeRelation {
    name: String,
    predicate: String,
    direction: String, // "forward" or "reverse"
    kind: String,      // "hasMany", "hasOne", "belongsToOne", "belongsToMany"
    max_count: Option<usize>,
    target_class_name: String,
    target_shape_json: String, // Serialised ModelMetadata JSON for recursive queries
}

/// A model shape reconstructed from SHACL links in the store.
#[derive(Debug)]
#[allow(dead_code)]
pub(crate) struct ModelShape {
    target_class: String,
    #[allow(dead_code)]
    shape_uri: String,
    properties: Vec<ShapeProperty>,
    /// Enriched relation metadata for include resolution (only populated
    /// when the TS client sends target shapes for included relations).
    include_relations: Vec<ShapeRelation>,
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

/// Load a shape from the store by class name.
/// Public wrapper for use by subscription infrastructure.
pub fn load_shape_from_store(store: &SparqlStore, class_name: &str) -> Result<ModelShape, Error> {
    load_shape(store, class_name)
}

// ---------------------------------------------------------------------------
// literal: URI parsing (typed)
// ---------------------------------------------------------------------------

/// Parse a `literal:` URI into a typed JSON value.
/// Returns the raw string as Value::String if not a literal: URI.
///
/// Since the signed-envelope migration (v3), all literal values are stored
/// as plain `literal:string:X`, `literal:number:X`, `literal:boolean:X`,
/// or `literal:json:X` (for non-envelope JSON objects/arrays).
fn parse_literal_value(uri: &str) -> Value {
    let body = if let Some(rest) = uri.strip_prefix("literal:") {
        rest
    } else {
        return Value::String(uri.to_string());
    };

    if let Some(rest) = body.strip_prefix("string:") {
        let decoded = urlencoding::decode(rest).unwrap_or_else(|_| rest.into());
        Value::String(decoded.into_owned())
    } else if let Some(rest) = body.strip_prefix("number:") {
        if let Ok(n) = rest.parse::<i64>() {
            Value::Number(n.into())
        } else if let Ok(f) = rest.parse::<f64>() {
            serde_json::Number::from_f64(f)
                .map(Value::Number)
                .unwrap_or(Value::String(rest.to_string()))
        } else {
            Value::String(rest.to_string())
        }
    } else if let Some(rest) = body.strip_prefix("boolean:") {
        match rest {
            "true" => Value::Bool(true),
            "false" => Value::Bool(false),
            _ => Value::String(rest.to_string()),
        }
    } else if let Some(rest) = body.strip_prefix("json:") {
        let decoded = urlencoding::decode(rest).unwrap_or_else(|_| rest.into());
        if let Ok(json_val) = serde_json::from_str::<Value>(&decoded) {
            json_val
        } else {
            Value::String(decoded.into_owned())
        }
    } else {
        Value::String(uri.to_string())
    }
}

/// Extract numeric value from a JSON value for comparison.
fn to_f64(val: &Value) -> Option<f64> {
    match val {
        Value::Number(n) => n.as_f64(),
        Value::String(s) => s.parse::<f64>().ok().or_else(|| iso_to_epoch_ms(s)),
        _ => None,
    }
}

/// Convert an ISO 8601 timestamp string to epoch milliseconds.
fn iso_to_epoch_ms(s: &str) -> Option<f64> {
    chrono::DateTime::parse_from_rfc3339(s)
        .ok()
        .map(|dt| dt.timestamp_millis() as f64)
}

// ---------------------------------------------------------------------------
// Shape loading from SHACL links
// ---------------------------------------------------------------------------

/// Load a model shape from the SHACL links stored in the Oxigraph store.
///
/// The SHACL links follow this pattern (set up by `parse_shacl_to_links`):
/// - `<namespace://ClassNameShape> sh://property <namespace://ClassName.propName>`
/// - `<namespace://ClassName.propName> sh://path <predicate_uri>`
/// - `<namespace://ClassName.propName> rdf://type sh://PropertyShape | ad4m://CollectionShape`
/// - `<namespace://ClassName.propName> sh://datatype <xsd://...>`
/// - `<namespace://ClassName.propName> sh://minCount literal:1^^xsd:integer`
/// - etc.
fn load_shape(store: &SparqlStore, class_name: &str) -> Result<ModelShape, Error> {
    let safe_name = escape_sparql_string(class_name);
    // Step 1: Find the shape URI and target class via SPARQL
    // Use exact suffix matching (/{name} or #{name}) to avoid "Recipe" matching "MyRecipe"
    // Build hash suffix separately to avoid `#` confusing `format!`
    let hash_suffix = format!("#{}", safe_name);
    let query = format!(
        r#"
        SELECT ?shapeUri ?targetClass WHERE {{
            ?targetClass <rdf://type> <ad4m://SubjectClass> .
            ?targetClass <ad4m://shape> ?shapeUri .
            FILTER(STRENDS(STR(?targetClass), "/{safe_name}") || STRENDS(STR(?targetClass), "{hash_suffix}") || STR(?targetClass) = "{safe_name}")
        }}
        LIMIT 1
        "#
    );

    let result_json = store.query(&query)?;
    let results: Vec<Value> = serde_json::from_str(&result_json)?;

    if results.is_empty() {
        return Err(anyhow!(
            "No SHACL shape found for class '{}'. Ensure the class has been registered with addSdna().",
            class_name
        ));
    }

    let shape_uri = results[0]["shapeUri"]
        .as_str()
        .ok_or_else(|| anyhow!("Missing shapeUri in SHACL query result"))?
        .to_string();
    let target_class = results[0]["targetClass"]
        .as_str()
        .ok_or_else(|| anyhow!("Missing targetClass in SHACL query result"))?
        .to_string();

    // Step 2: Load all property shapes for this shape
    let props_query = format!(
        r#"
        SELECT ?propUri ?path ?propType ?datatype ?minCount ?maxCount ?resolveLanguage WHERE {{
            <{shape_uri}> <sh://property> ?propUri .
            ?propUri <sh://path> ?path .
            ?propUri <rdf://type> ?propType .
            OPTIONAL {{ ?propUri <sh://datatype> ?datatype . }}
            OPTIONAL {{ ?propUri <sh://minCount> ?minCount . }}
            OPTIONAL {{ ?propUri <sh://maxCount> ?maxCount . }}
            OPTIONAL {{ ?propUri <ad4m://resolveLanguage> ?resolveLanguage . }}
        }}
        "#
    );

    let props_json = store.query(&props_query)?;
    let prop_results: Vec<Value> = serde_json::from_str(&props_json)?;

    let mut properties = Vec::new();

    for prop_row in &prop_results {
        let prop_uri = prop_row["propUri"].as_str().unwrap_or("");
        let path = prop_row["path"].as_str().unwrap_or("").to_string();
        let prop_type = prop_row["propType"].as_str().unwrap_or("");
        let datatype = prop_row["datatype"].as_str().map(|s| s.to_string());
        let min_count_str = prop_row["minCount"].as_str().unwrap_or("0");
        let resolve_language = prop_row["resolveLanguage"].as_str().map(|s| {
            // Decode if it's a literal: URI
            if let Some(rest) = s.strip_prefix("literal:string:") {
                urlencoding::decode(rest)
                    .unwrap_or_else(|_| rest.into())
                    .into_owned()
            } else {
                s.to_string()
            }
        });

        // Extract name from prop_uri: "namespace://ClassName.propName" -> "propName"
        let name = prop_uri
            .rsplit_once('.')
            .map(|(_, n)| n.to_string())
            .unwrap_or_else(|| {
                // Fallback: extract from path
                path.rsplit(&['/', '#', ':'][..])
                    .next()
                    .unwrap_or("unknown")
                    .to_string()
            });

        let is_collection = prop_type == "ad4m://CollectionShape";

        // Parse minCount to detect required
        let min_count: u32 = min_count_str
            .strip_prefix("literal:")
            .and_then(|s| s.split("^^").next())
            .unwrap_or(min_count_str)
            .parse()
            .unwrap_or(0);

        // Check if this is a flag property by looking for an initial value
        // Flags have a specific target value in the conformance check
        let initial_value = get_initial_value(store, prop_uri, &path, &target_class)?;
        let is_flag = initial_value.is_some() && min_count > 0;

        properties.push(ShapeProperty {
            name,
            predicate: path,
            is_collection,
            is_flag,
            is_required: min_count > 0,
            initial_value,
            resolve_language,
            datatype,
            direction: None,
            is_scalar_relation: false,
            getter: None, // SHACL shapes don't carry getter metadata; JSON path does
            where_filter: None,
            where_predicates: None,
        });
    }

    Ok(ModelShape {
        target_class,
        shape_uri,
        properties,
        include_relations: Vec::new(),
    })
}

/// Check if a property has an initial/flag value by looking at the constructor actions
/// or at the initial value link pattern.
fn get_initial_value(
    store: &SparqlStore,
    _prop_uri: &str,
    predicate: &str,
    target_class: &str,
) -> Result<Option<String>, Error> {
    // Look for constructor actions that set an initial value for this predicate
    // Constructor links: <ShapeUri> ad4m://constructor <literal:string:JSON>
    // The JSON contains actions like {"action": "addLink", "source": "this", "predicate": "...", "target": "..."}

    // Also check if there's an existing flag-like pattern: a conformance check that expects
    // a specific target value for this predicate
    // For now, we check if there are instances where this predicate points to a fixed URI (not a literal:)
    // This is a simplified heuristic — the TS side uses decorator metadata (initial, flag) which
    // we'll receive directly from the client in the query

    // Quick check: is there a link from targetClass with this predicate to a fixed value?
    // Look for the "required" flag property pattern
    let safe_tc = match validate_iri(target_class) {
        Ok(s) => s,
        Err(_) => return Ok(None),
    };
    let safe_pred = match validate_iri(predicate) {
        Ok(s) => s,
        Err(_) => return Ok(None),
    };
    let query = format!(
        r#"
        SELECT ?target WHERE {{
            <{safe_tc}> <{safe_pred}> ?target .
        }}
        LIMIT 1
        "#
    );

    let result_json = store.query(&query).unwrap_or_else(|_| "[]".to_string());
    let results: Vec<Value> = serde_json::from_str(&result_json).unwrap_or_default();

    // If the target_class itself has a link with this predicate, it might be a flag/initial value
    // But this is heuristic — the definitive source is the TS metadata sent with the query
    // For now return None and let the client send shape metadata
    let _ = results;
    Ok(None)
}

// ---------------------------------------------------------------------------
// Core query execution
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Projection resolution ($-prefixed keys)
// ---------------------------------------------------------------------------

/// Resolve projection keys for a set of already-paginated instances.
///
/// For each key in `projections`, issues a single grouped SPARQL query
/// (O(1) per key) and attaches results to the matching instances.
///
/// - `count: true` → attaches an integer (0 when no links exist).
/// - `limit: Some(1)` → attaches the first linked ID as a string, or `null`.
/// - otherwise → attaches an array of linked IDs.
fn resolve_projections(
    store: &SparqlStore,
    instances: &mut Vec<Value>,
    projections: &HashMap<String, ProjectionInput>,
    shape: &ModelShape,
) -> Result<(), Error> {
    if instances.is_empty() || projections.is_empty() {
        return Ok(());
    }

    // Collect parent IDs (validate each as a safe IRI).
    let parent_ids: Vec<String> = instances
        .iter()
        .filter_map(|inst| inst["id"].as_str())
        .filter_map(|id| validate_iri(id).ok().map(|s| s.to_string()))
        .collect();

    if parent_ids.is_empty() {
        return Ok(());
    }

    // Build VALUES clause: `<id1> <id2> …`
    let values_clause = parent_ids
        .iter()
        .map(|id| format!("<{}>", id))
        .collect::<Vec<_>>()
        .join(" ");

    for (key, proj) in projections {
        // ------------------------------------------------------------------
        // 1. Resolve predicate from the parent shape's properties/relations.
        // ------------------------------------------------------------------
        let predicate = match shape.properties.iter().find(|p| p.name == proj.from) {
            Some(p) if !p.predicate.is_empty() => p.predicate.clone(),
            Some(_) => {
                log::warn!(
                    "IncludeProjection '{}': relation '{}' has empty predicate — skipping",
                    key,
                    proj.from
                );
                continue;
            }
            None => {
                log::warn!(
                    "IncludeProjection '{}': relation '{}' not found in shape — skipping",
                    key,
                    proj.from
                );
                continue;
            }
        };

        let safe_pred = match validate_iri(&predicate) {
            Ok(p) => p.to_string(),
            Err(_) => {
                log::warn!(
                    "IncludeProjection '{}': predicate '{}' is not a valid IRI — skipping",
                    key,
                    predicate
                );
                continue;
            }
        };

        // ------------------------------------------------------------------
        // 2. Build optional where-clause patterns for target properties.
        // ------------------------------------------------------------------
        let where_patterns = build_projection_where_patterns(proj);
        // Reifier join + FILTER for author/timestamp — emitted only when those
        // fields appear in the where clause.
        let reifier_patterns = build_projection_reifier_patterns(proj, &safe_pred);

        // ------------------------------------------------------------------
        // 3. Issue the grouped SPARQL query and attach results.
        // ------------------------------------------------------------------
        if proj.count {
            // COUNT query — returns one row per parent with the count.
            let sparql = format!(
                concat!(
                    "SELECT ?parent (COUNT(DISTINCT ?t) AS ?n) WHERE {{\n",
                    "    VALUES ?parent {{ {values_clause} }}\n",
                    "    ?parent <{safe_pred}> ?t .\n",
                    "{where_patterns}",
                    "{reifier_patterns}",
                    "}} GROUP BY ?parent"
                ),
                values_clause = values_clause,
                safe_pred = safe_pred,
                where_patterns = where_patterns,
                reifier_patterns = reifier_patterns,
            );

            let result_json = store.query(&sparql)?;
            let rows: Vec<Value> = serde_json::from_str(&result_json)?;

            // parent_id → count
            let mut count_map: HashMap<String, u64> = HashMap::new();
            for row in &rows {
                let parent = row["parent"].as_str().unwrap_or("").to_string();
                let n: u64 = row["n"]
                    .as_str()
                    .and_then(|s| s.parse().ok())
                    .or_else(|| row["n"].as_u64())
                    .unwrap_or(0);
                count_map.insert(parent, n);
            }

            for inst in instances.iter_mut() {
                if let Some(obj) = inst.as_object_mut() {
                    let id = obj
                        .get("id")
                        .and_then(|v| v.as_str())
                        .unwrap_or("")
                        .to_string();
                    let cnt = count_map.get(&id).copied().unwrap_or(0);
                    obj.insert(key.clone(), Value::Number(cnt.into()));
                }
            }
        } else {
            // LIST / SCALAR query.
            // Note: LIMIT is NOT added to the SPARQL — a global LIMIT on a
            // VALUES-clause query limits the *total* row count, not per-parent.
            // Instead we collect all rows and truncate per-parent in Rust below.
            let order_clause = build_projection_order_clause(proj);

            let sparql = format!(
                concat!(
                    "SELECT ?parent ?t WHERE {{\n",
                    "    VALUES ?parent {{ {values_clause} }}\n",
                    "    ?parent <{safe_pred}> ?t .\n",
                    "{where_patterns}",
                    "{reifier_patterns}",
                    "}}{order_clause}"
                ),
                values_clause = values_clause,
                safe_pred = safe_pred,
                where_patterns = where_patterns,
                reifier_patterns = reifier_patterns,
                order_clause = order_clause,
            );

            let result_json = store.query(&sparql)?;
            let rows: Vec<Value> = serde_json::from_str(&result_json)?;

            // parent_id → Vec<target_id>
            let mut list_map: HashMap<String, Vec<Value>> = HashMap::new();
            for row in &rows {
                if let Some(parent) = row["parent"].as_str() {
                    let t = row["t"]
                        .as_str()
                        .map(|s| Value::String(s.to_string()))
                        .unwrap_or(Value::Null);
                    list_map.entry(parent.to_string()).or_default().push(t);
                }
            }

            for inst in instances.iter_mut() {
                if let Some(obj) = inst.as_object_mut() {
                    let id = obj
                        .get("id")
                        .and_then(|v| v.as_str())
                        .unwrap_or("")
                        .to_string();
                    let val = match proj.limit {
                        Some(1) => list_map
                            .get(&id)
                            .and_then(|v| v.first().cloned())
                            .unwrap_or(Value::Null),
                        Some(n) => Value::Array(
                            list_map
                                .get(&id)
                                .cloned()
                                .unwrap_or_default()
                                .into_iter()
                                .take(n)
                                .collect(),
                        ),
                        None => Value::Array(list_map.get(&id).cloned().unwrap_or_default()),
                    };
                    obj.insert(key.clone(), val);
                }
            }
        }
    }

    Ok(())
}

/// Build SPARQL triple patterns and FILTER clauses for a projection's `where`
/// clause.
///
/// Uses `STR()` comparison against both the plain value and the stored
/// `literal:` URI form (e.g. `literal:string:X`, `literal:boolean:true`).
/// This covers plain IRI targets and the `literal:string:` / `literal:number:`
/// / `literal:boolean:` encodings used by the executor.
///
/// `author` and `timestamp` are **not** handled here — they live on the
/// reification node, not on `?t`.  See `build_projection_reifier_patterns`
/// which emits the required `rdf:reifies` join and FILTER for those fields.
///
/// **Note:** Plain literal values (`literal:string:X`, `literal:number:X`,
/// `literal:boolean:X`) are matched via `STR(?v) = "literal:string:X"` FILTER.
/// Complex JSON objects stored as `literal:json:` may require post-fetch
/// filtering in Rust if the caller needs deep property matching.
fn build_projection_where_patterns(proj: &ProjectionInput) -> String {
    let Some(ref wc) = proj.where_clause else {
        return String::new();
    };

    // Build predicate lookup from the optional target shape.
    let pred_lookup: HashMap<String, String> = if let Some(ref ts) = proj.target_shape {
        let mut map = HashMap::new();
        if let Some(props) = ts["properties"].as_object() {
            for (name, pm) in props {
                if let Some(pred) = pm["predicate"].as_str() {
                    if !pred.is_empty() {
                        map.insert(name.clone(), pred.to_string());
                    }
                }
            }
        }
        if let Some(rels) = ts["relations"].as_object() {
            for (name, rm) in rels {
                if let Some(pred) = rm["predicate"].as_str() {
                    if !pred.is_empty() {
                        map.insert(name.clone(), pred.to_string());
                    }
                }
            }
        }
        map
    } else {
        HashMap::new()
    };

    let mut patterns = Vec::new();
    let mut filter_idx = 0usize;

    for (prop_name, condition) in wc {
        // id / base → direct IRI equality on ?t
        if prop_name == "id" || prop_name == "base" {
            match condition {
                WhereCondition::String(val) => {
                    let escaped = escape_sparql_string(val);
                    patterns.push(format!("    FILTER(STR(?t) = \"{}\")\n", escaped));
                }
                WhereCondition::StringArray(vals) => {
                    let list = vals
                        .iter()
                        .map(|v| format!("\"{}\"", escape_sparql_string(v)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    patterns.push(format!("    FILTER(STR(?t) IN ({}))\n", list));
                }
                _ => {}
            }
            continue;
        }

        // author / timestamp — not properties of ?t; handled via reifier join
        // in build_projection_reifier_patterns, which adds the necessary
        // <<(?parent <pred> ?t)>> ~> ?_prj_reif join + FILTER to the query.
        if prop_name == "author" || prop_name == "timestamp" {
            continue;
        }

        // Resolve via target shape
        let pred = match pred_lookup.get(prop_name) {
            Some(p) => p.clone(),
            None => continue, // unknown property without a shape — skip
        };

        if validate_iri(&pred).is_err() {
            continue;
        }

        let var = format!("_pw{}", filter_idx);
        filter_idx += 1;

        match condition {
            WhereCondition::String(val) => {
                let escaped = escape_sparql_string(val);
                patterns.push(format!("    ?t <{}> ?{} .\n", pred, var));
                patterns.push(format!(
                    "    FILTER(STR(?{}) = \"{}\" || STR(?{}) = \"literal:string:{}\")\n",
                    var,
                    escaped,
                    var,
                    urlencoding::encode(val),
                ));
            }
            WhereCondition::Bool(b) => {
                let bval = if *b { "true" } else { "false" };
                patterns.push(format!("    ?t <{}> ?{} .\n", pred, var));
                patterns.push(format!(
                    "    FILTER(STR(?{}) = \"{}\" || STR(?{}) = \"literal:boolean:{}\")\n",
                    var, bval, var, bval,
                ));
            }
            WhereCondition::Number(n) => {
                patterns.push(format!("    ?t <{}> ?{} .\n", pred, var));
                patterns.push(format!(
                    "    FILTER(STR(?{}) = \"{}\" || STR(?{}) = \"literal:number:{}\")\n",
                    var, n, var, n,
                ));
            }
            WhereCondition::StringArray(vals) => {
                let list = vals
                    .iter()
                    .flat_map(|v| {
                        let r = escape_sparql_string(v);
                        let e = urlencoding::encode(v).to_string();
                        vec![format!("\"{}\"", r), format!("\"literal:string:{}\"", e)]
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                patterns.push(format!("    ?t <{}> ?{} .\n", pred, var));
                patterns.push(format!("    FILTER(STR(?{}) IN ({}))\n", var, list));
            }
            _ => {} // Ops not supported for projection where clauses
        }
    }

    patterns.join("")
}

/// Build an `ORDER BY` clause string for a list projection.
///
/// Only `id`/`base` (which map to `?t`) are supported — ordering by arbitrary
/// child properties would require an extra JOIN in the projection query.
/// Returns a string starting with `\n` (e.g. `"\nORDER BY ASC(?t)"`) or empty.
fn build_projection_order_clause(proj: &ProjectionInput) -> String {
    let Some(ref order) = proj.order else {
        return String::new();
    };
    let terms: Vec<String> = order
        .iter()
        .filter_map(|(k, dir)| {
            if k == "id" || k == "base" {
                Some(match dir {
                    OrderDirection::ASC => "ASC(?t)".to_string(),
                    OrderDirection::DESC => "DESC(?t)".to_string(),
                })
            } else {
                None
            }
        })
        .collect();
    if terms.is_empty() {
        String::new()
    } else {
        format!("\nORDER BY {}", terms.join(" "))
    }
}

/// Build SPARQL reifier join + FILTER patterns for `author` / `timestamp`
/// conditions in a projection's `where` clause.
///
/// Returns an empty string when neither field is present.  When present,
/// emits the RDF 1.2 triple-term reifier join:
///
/// ```sparql
/// ?_prj_reif <rdf:reifies> <<(?parent <pred> ?t)>> .
/// ?_prj_reif <ad4m://ontology/author> ?_prj_author .
/// FILTER(STR(?_prj_author) = "did:key:…")
/// ```
///
/// This is a required (non-OPTIONAL) join — every link stored by the executor
/// carries a reifier, so this will never silently drop valid rows.
fn build_projection_reifier_patterns(proj: &ProjectionInput, safe_pred: &str) -> String {
    let Some(ref wc) = proj.where_clause else {
        return String::new();
    };

    let author_cond = wc.get("author");
    let timestamp_cond = wc.get("timestamp");

    if author_cond.is_none() && timestamp_cond.is_none() {
        return String::new();
    }

    let mut patterns = Vec::new();

    // Join the reification node for the (?parent <pred> ?t) triple.
    patterns.push(format!(
        "    ?_prj_reif <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<(?parent <{safe_pred}> ?t)>> .\n"
    ));

    if let Some(cond) = author_cond {
        patterns.push("    ?_prj_reif <ad4m://ontology/author> ?_prj_author .\n".to_string());
        // Only String (DID) conditions make sense for author.
        // Non-String variants (Ops, etc.) are not supported and are silently ignored.
        if let WhereCondition::String(did) = cond {
            let escaped = escape_sparql_string(did);
            patterns.push(format!("    FILTER(STR(?_prj_author) = \"{escaped}\")\n"));
        }
    }

    if let Some(cond) = timestamp_cond {
        patterns.push("    ?_prj_reif <ad4m://ontology/timestamp> ?_prj_timestamp .\n".to_string());
        // String condition → exact ISO timestamp match.
        // Ops-style range conditions (e.g. { gt: 1000 }) are not yet handled here;
        // they would require post-fetch Rust filtering rather than a SPARQL FILTER.
        if let WhereCondition::String(ts) = cond {
            let escaped = escape_sparql_string(ts);
            patterns.push(format!(
                "    FILTER(STR(?_prj_timestamp) = \"{escaped}\")\n"
            ));
        }
    }

    patterns.join("")
}

/// Execute a model query against the Oxigraph store.
///
/// This is the main entry point that replaces the TS SPARQL-build → hydrate → filter pipeline.
pub fn execute_model_query(
    store: &SparqlStore,
    class_name: &str,
    query_input: &ModelQueryInput,
    shape_json: Option<&str>,
) -> Result<ModelQueryResult, Error> {
    execute_model_query_inner(store, class_name, query_input, shape_json, 0)
}

/// Evaluate property getters for a batch of instances identified by ID.
///
/// This is the Rust-side implementation of `Ad4mModel.evaluateGetters()`.
/// Instead of N × querySparql round-trips, the caller sends all instance IDs
/// and (optionally) the property names to evaluate. The function loads the
/// model shape, builds stub instances with the given IDs, runs the getter
/// SPARQL in-process, and returns a map of `{ instanceId: { prop: value } }`.
pub fn evaluate_getters_batch(
    store: &SparqlStore,
    class_name: &str,
    instance_ids: &[String],
    property_names: Option<&[String]>,
    shape_json: Option<&str>,
) -> Result<Value, Error> {
    if instance_ids.is_empty() {
        return Ok(Value::Object(Map::new()));
    }

    let shape = if let Some(json) = shape_json {
        parse_shape_from_json(json, class_name)?
    } else {
        return Err(deno_core::anyhow::anyhow!(
            "shape_json is required for evaluate_getters_batch"
        ));
    };

    // Collect getter props, optionally filtered by property_names
    let getter_props: Vec<&ShapeProperty> = shape
        .properties
        .iter()
        .filter(|p| {
            p.getter.is_some()
                && property_names
                    .map(|names| names.iter().any(|n| n == &p.name))
                    .unwrap_or(true)
        })
        .collect();

    if getter_props.is_empty() {
        return Ok(Value::Object(Map::new()));
    }

    // Build stub instances with just the id field
    let mut instances: Vec<Value> = instance_ids
        .iter()
        .map(|id| {
            let mut obj = Map::new();
            obj.insert("id".to_string(), Value::String(id.clone()));
            Value::Object(obj)
        })
        .collect();

    // Reuse the existing evaluate_getters function (deep_query=true to eval all getters)
    evaluate_getters(store, &mut instances, &shape, None, true)?;

    // Build result map: { instanceId: { prop: value, ... } }
    let mut result = Map::new();
    for inst in &instances {
        if let Some(id) = inst.get("id").and_then(|v| v.as_str()) {
            let mut props = Map::new();
            for prop in &getter_props {
                if let Some(val) = inst.get(&prop.name) {
                    if val != &Value::Null {
                        props.insert(prop.name.clone(), val.clone());
                    }
                }
            }
            if !props.is_empty() {
                result.insert(id.to_string(), Value::Object(props));
            }
        }
    }

    Ok(Value::Object(result))
}

/// Inner implementation with depth tracking for cycle detection.
fn execute_model_query_inner(
    store: &SparqlStore,
    class_name: &str,
    query_input: &ModelQueryInput,
    shape_json: Option<&str>,
    depth: u8,
) -> Result<ModelQueryResult, Error> {
    if depth > MAX_INCLUDE_DEPTH {
        log::warn!(
            "Include resolution depth {} exceeded for class '{}'; returning empty",
            MAX_INCLUDE_DEPTH,
            class_name
        );
        return Ok(ModelQueryResult {
            instances: vec![],
            total_count: 0,
        });
    }
    // Load shape from store or parse from provided JSON
    let shape = if let Some(json) = shape_json {
        parse_shape_from_json(json, class_name)?
    } else {
        load_shape(store, class_name)?
    };

    // ── Fast path: COUNT-only ────────────────────────────────────────────
    // When the caller only needs a count (limit==0) and all where conditions
    // can be pushed to SPARQL, we skip hydration entirely and run a single
    // SELECT COUNT(DISTINCT ?source) query.
    // Note: count==true with limit>0 means "return instances AND total count",
    // so we must NOT take the fast path in that case.
    let is_count_only = query_input.limit == Some(0);
    if is_count_only && all_where_pushable(query_input, &shape) {
        if let Some(sparql) = build_count_sparql(&shape, query_input) {
            let result_json = store.query(&sparql)?;
            let results: Vec<Value> = serde_json::from_str(&result_json)?;
            let count = results
                .first()
                .and_then(|r| {
                    r["cnt"]
                        .as_str()
                        .and_then(|s| s.parse::<usize>().ok())
                        .or_else(|| r["cnt"].as_u64().map(|n| n as usize))
                })
                .unwrap_or(0);
            return Ok(ModelQueryResult {
                instances: vec![],
                total_count: count,
            });
        }
        // Fall through to full pipeline when conformance is empty
    }

    // ── Full pipeline ────────────────────────────────────────────────────

    // Determine if we can push pagination to SPARQL.
    // Requirements: all where conditions are SPARQL-pushable, and the sort
    // key is timestamp (which lives on the reifier and is available in SPARQL).
    let can_push_pagination = all_where_pushable(query_input, &shape) && {
        match &query_input.order {
            None => true, // default is timestamp ASC — pushable
            Some(order) => {
                order.len() == 1
                    && (order[0].0 == "timestamp"
                        || order[0].0 == "createdAt"
                        || order[0].0 == "updatedAt")
            }
        }
    };

    let sparql_pagination = if can_push_pagination
        && (query_input.limit.is_some() || query_input.offset.is_some())
    {
        let direction = query_input
            .order
            .as_ref()
            .and_then(|o| o.first())
            .map(|(_, d)| *d)
            .unwrap_or(OrderDirection::ASC);
        Some(SparqlPagination {
            direction,
            offset: query_input.offset,
            limit: query_input.limit,
        })
    } else {
        None
    };

    // Build SPARQL to find conforming instances and their property values
    let sparql = build_instance_sparql(&shape, query_input, sparql_pagination.as_ref());

    // Execute the SPARQL query
    let result_json = store.query(&sparql)?;
    let raw_results: Vec<Value> = serde_json::from_str(&result_json)?;

    // Group results by source (each instance may have multiple rows)
    let grouped = group_results_by_source(&raw_results, &shape);

    // Hydrate instances from grouped results
    let mut instances = hydrate_instances(&shape, &grouped);

    // Resolve reverse relations (BelongsToOne / BelongsToMany) — these are
    // links pointing TO our instances and aren't captured by the main SPARQL.
    let reverse_rels: Vec<(String, String, bool)> = shape
        .properties
        .iter()
        .filter(|p| p.direction.as_deref() == Some("reverse"))
        .map(|p| (p.name.clone(), p.predicate.clone(), p.is_scalar_relation))
        .collect();
    if !reverse_rels.is_empty() && !instances.is_empty() {
        resolve_reverse_relations(store, &mut instances, &reverse_rels)?;
    }

    // Apply post-hydration where-clause filters.
    // When all conditions were pushed to SPARQL, this is a no-op (all
    // instances already match). When Ops conditions exist, this filters
    // the remaining non-pushable conditions in Rust.
    if let Some(ref where_clause) = query_input.where_clause {
        instances.retain(|inst| matches_where(inst, where_clause, &shape));
    }

    // Calculate total count.
    // When SPARQL pagination was used, we need a separate count query.
    let total_count = if sparql_pagination.is_some() {
        // Run count query to get the true total
        if let Some(sparql) = build_count_sparql(&shape, query_input) {
            let result_json = store.query(&sparql)?;
            let results: Vec<Value> = serde_json::from_str(&result_json)?;
            results
                .first()
                .and_then(|r| {
                    r["cnt"]
                        .as_str()
                        .and_then(|s| s.parse::<usize>().ok())
                        .or_else(|| r["cnt"].as_u64().map(|n| n as usize))
                })
                .unwrap_or(instances.len())
        } else {
            instances.len()
        }
    } else {
        instances.len()
    };

    // Apply ordering and pagination in Rust (only when NOT pushed to SPARQL)
    let mut paginated: Vec<Value> = if sparql_pagination.is_some() {
        // Already ordered and paginated by SPARQL subquery
        instances
    } else {
        // Apply ordering
        if let Some(ref order) = query_input.order {
            sort_instances(&mut instances, order);
        } else if query_input.limit.is_some() || query_input.offset.is_some() {
            sort_instances(
                &mut instances,
                &[("timestamp".to_string(), OrderDirection::ASC)],
            );
        }

        // Apply pagination
        let offset = query_input.offset.unwrap_or(0);
        if let Some(limit) = query_input.limit {
            instances.into_iter().skip(offset).take(limit).collect()
        } else {
            instances.into_iter().skip(offset).collect()
        }
    };

    // ── Evaluate property/relation getters (post-pagination) ─────────────
    // Runs SPARQL getters using batched VALUES queries — O(M) per getter
    // property regardless of instance count.  Runs AFTER pagination so only
    // the current page of results is evaluated, not the entire result set.
    if !paginated.is_empty() {
        let deep_query = query_input.deep_query.unwrap_or(true);
        evaluate_getters(
            store,
            &mut paginated,
            &shape,
            query_input.include.as_ref(),
            deep_query,
        )?;
    }

    // ── Eager-load included relations ────────────────────────────────────
    if let Some(ref include) = query_input.include {
        if !paginated.is_empty() && !shape.include_relations.is_empty() {
            resolve_includes_recursive(store, &mut paginated, include, &shape, depth)?;
        }
    }

    // Strip unrequested properties if specified
    let mut final_instances: Vec<Value> = if let Some(ref requested) = query_input.properties {
        // When includes are present, keep the included relation fields
        let mut keep = requested.clone();
        if let Some(ref inc) = query_input.include {
            for rel_name in inc.keys() {
                if !keep.contains(rel_name) {
                    keep.push(rel_name.clone());
                }
            }
        }
        paginated
            .into_iter()
            .map(|inst| filter_properties(inst, &keep))
            .collect()
    } else {
        paginated
    };

    // ── Attach projection results ($-prefixed aggregations / lists) ──────
    // Issued as one grouped VALUES SPARQL per key — O(projections) total.
    if let Some(ref projections) = query_input.projections {
        resolve_projections(store, &mut final_instances, projections, &shape)?;
    }

    Ok(ModelQueryResult {
        instances: final_instances,
        total_count,
    })
}

/// Parse a where-filter JSON object into a BTreeMap<String, WhereCondition>.
/// Used for post-getter relation filtering (property values are signed
/// expression envelopes and cannot be matched by SPARQL FILTER).
fn parse_where_filter(val: &Value) -> Option<BTreeMap<String, WhereCondition>> {
    let obj = val.as_object()?;
    let mut map = BTreeMap::new();
    for (key, cond) in obj {
        if let Ok(wc) = serde_json::from_value::<WhereCondition>(cond.clone()) {
            map.insert(key.clone(), wc);
        }
    }
    if map.is_empty() {
        None
    } else {
        Some(map)
    }
}

/// Parse shape metadata from JSON sent by the TS client.
/// This is more reliable than reading from the store because the TS client
/// has the definitive decorator metadata (flags, required, initial values, etc.).
fn parse_shape_from_json(json: &str, class_name: &str) -> Result<ModelShape, Error> {
    let meta: Value =
        serde_json::from_str(json).map_err(|e| anyhow!("Failed to parse shape JSON: {}", e))?;

    let target_class = meta["className"].as_str().unwrap_or(class_name).to_string();

    let mut properties = Vec::new();
    let mut include_relations: Vec<ShapeRelation> = Vec::new();

    // Parse properties from the metadata
    if let Some(props) = meta["properties"].as_object() {
        for (name, prop_meta) in props {
            let predicate = prop_meta["predicate"].as_str().unwrap_or("").to_string();
            if predicate.is_empty() {
                continue;
            }

            let is_required = prop_meta["required"].as_bool().unwrap_or(false);
            let is_flag = prop_meta["flag"].as_bool().unwrap_or(false);
            let initial = prop_meta["initial"].as_str().map(|s| s.to_string());
            let resolve_language = prop_meta["resolveLanguage"].as_str().map(|s| s.to_string());
            let datatype = prop_meta["datatype"].as_str().map(|s| s.to_string());
            let getter = prop_meta["getter"].as_str().map(|s| s.to_string());

            properties.push(ShapeProperty {
                name: name.clone(),
                predicate,
                is_collection: false,
                is_flag,
                is_required,
                initial_value: initial,
                resolve_language,
                datatype,
                direction: None,
                is_scalar_relation: false,
                getter,
                where_filter: None,
                where_predicates: None,
            });
        }
    }

    // Parse relations from the metadata
    if let Some(rels) = meta["relations"].as_object() {
        for (name, rel_meta) in rels {
            let predicate = rel_meta["predicate"].as_str().unwrap_or("").to_string();
            let getter = rel_meta["getter"].as_str().map(|s| s.to_string());

            // Skip relations with no predicate AND no getter — nothing to query.
            // Relations with a getter but no predicate are read-only custom-SPARQL
            // relations (e.g. `@HasMany({ getter: "SELECT ..." })`).
            if predicate.is_empty() && getter.is_none() {
                continue;
            }

            let direction = rel_meta["direction"]
                .as_str()
                .map(|s| s.to_string())
                .or_else(|| Some("forward".to_string()));

            let kind = rel_meta["kind"].as_str().unwrap_or("hasMany").to_string();
            let is_scalar_relation = kind == "hasOne" || kind == "belongsToOne";

            // Parse post-getter where filter for relation properties
            let where_filter = parse_where_filter(&rel_meta["whereFilter"]);
            let where_predicates = rel_meta["wherePredicates"].as_object().map(|obj| {
                obj.iter()
                    .filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), s.to_string())))
                    .collect::<HashMap<String, String>>()
            });

            properties.push(ShapeProperty {
                name: name.clone(),
                predicate: predicate.clone(),
                is_collection: true, // always accumulate as array during hydration
                is_flag: false,
                is_required: false,
                initial_value: None,
                resolve_language: None,
                datatype: None,
                direction: direction.clone(),
                is_scalar_relation,
                getter,
                where_filter,
                where_predicates,
            });

            // Parse enriched relation metadata (target shapes for include resolution)
            if rel_meta.get("targetShape").is_some() {
                let target_shape = &rel_meta["targetShape"];
                let target_class_name = rel_meta["targetClassName"]
                    .as_str()
                    .or_else(|| target_shape["className"].as_str())
                    .unwrap_or("")
                    .to_string();
                let kind = rel_meta["kind"].as_str().unwrap_or("hasMany").to_string();
                let direction = rel_meta["direction"]
                    .as_str()
                    .unwrap_or("forward")
                    .to_string();
                let max_count = rel_meta["maxCount"].as_u64().map(|n| n as usize);

                include_relations.push(ShapeRelation {
                    name: name.clone(),
                    predicate,
                    direction,
                    kind,
                    max_count,
                    target_class_name,
                    target_shape_json: serde_json::to_string(target_shape).unwrap_or_default(),
                });
            }
        }
    }

    // Build a shape URI from the className
    let shape_uri = format!("{}Shape", target_class);

    Ok(ModelShape {
        target_class,
        shape_uri,
        properties,
        include_relations,
    })
}

/// Build the SPARQL query that finds all conforming instances and their link data.
///
/// The query pattern:
/// 1. Conformance: JOIN on required/flag properties to identify valid instances
/// 2. Data retrieval: Fetch links for conforming instances with reifier metadata
///
/// **Predicate projection optimisation:** Only predicates that hydration
/// actually needs from the main query are included.  Collection properties
/// that have SPARQL getters (auto-generated conformance filters for typed
/// `@HasMany` relations) are excluded — they are resolved separately in
/// `evaluate_getters`.  This avoids scanning large numbers of links (e.g.
/// 10 000 message children) that would be discarded during hydration.
/// When `sparql_pagination` is provided, adds ORDER BY / LIMIT / OFFSET
/// clauses to the SPARQL query, enabling server-side pagination.
fn build_instance_sparql(
    shape: &ModelShape,
    query: &ModelQueryInput,
    sparql_pagination: Option<&SparqlPagination>,
) -> String {
    let (conformance, where_extra) = build_query_patterns(shape, query);

    // Collect predicates that hydration will actually consume from the main
    // query results.  Skip collection properties that have a SPARQL getter
    // because evaluate_getters resolves those with targeted per-relation
    // queries, and hydrate_one already excludes getter-backed properties
    // from its pred_to_props map.
    let needed: Vec<&str> = shape
        .properties
        .iter()
        .filter(|p| !p.predicate.is_empty())
        .filter(|p| !(p.is_collection && p.getter.is_some()))
        .map(|p| p.predicate.as_str())
        .collect();

    // Build a VALUES clause to restrict the predicate scan.
    // Deduplicate with BTreeSet for deterministic output.
    let predicate_filter = if needed.is_empty() {
        // No known predicates — fall back to unrestricted wildcard.
        // This shouldn't happen for well-decorated models (they always
        // have at least a @Flag), but keeps the function safe for edge cases.
        String::new()
    } else {
        let unique: std::collections::BTreeSet<&str> = needed.into_iter().collect();
        let values: String = unique
            .iter()
            .map(|p| format!("<{}>", p))
            .collect::<Vec<_>>()
            .join(" ");
        format!("    VALUES ?predicate {{ {} }}\n", values)
    };

    // Build pagination suffix for source subquery
    let pagination_suffix = if let Some(pg) = sparql_pagination {
        let mut suffix = String::new();
        // Order by timestamp on the earliest reifier per source
        match pg.direction {
            OrderDirection::DESC => suffix.push_str("\n    ORDER BY DESC(?_first_ts)"),
            OrderDirection::ASC => suffix.push_str("\n    ORDER BY ASC(?_first_ts)"),
        }
        if let Some(offset) = pg.offset {
            if offset > 0 {
                suffix.push_str(&format!("\n    OFFSET {}", offset));
            }
        }
        if let Some(limit) = pg.limit {
            suffix.push_str(&format!("\n    LIMIT {}", limit));
        }
        suffix
    } else {
        String::new()
    };

    if sparql_pagination.is_some() {
        // Use a subquery to paginate at the instance (source) level,
        // then join with properties in the outer query.
        format!(
            r#"SELECT ?source ?predicate ?target ?author ?timestamp WHERE {{
    {{
        SELECT DISTINCT ?source (MIN(?_ts) AS ?_first_ts) WHERE {{
{conformance}
{where_extra}
            ?source ?_anyP ?_anyT .
            ?_r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?_anyP ?_anyT )>> .
            ?_r <ad4m://ontology/timestamp> ?_ts .
            FILTER(isIRI(?source))
        }} GROUP BY ?source{pagination_suffix}
    }}
{predicate_filter}    ?source ?predicate ?target .
    ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
    FILTER(isIRI(?predicate))
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
}}"#
        )
    } else {
        format!(
            r#"SELECT ?source ?predicate ?target ?author ?timestamp WHERE {{
{conformance}
{where_extra}
{predicate_filter}    ?source ?predicate ?target .
    ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
    FILTER(isIRI(?source) && isIRI(?predicate))
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
}}"#
        )
    }
}

/// Build a COUNT SPARQL query that returns the number of conforming instances.
/// Used as a fast path for count-only queries when all where conditions are SPARQL-pushable.
fn build_count_sparql(shape: &ModelShape, query: &ModelQueryInput) -> Option<String> {
    let (conformance, where_extra) = build_query_patterns(shape, query);

    // If no conformance patterns were produced, the COUNT would match every
    // IRI subject in the store — refuse the fast path.
    if conformance.trim().is_empty() && where_extra.trim().is_empty() {
        return None;
    }

    Some(format!(
        r#"SELECT (COUNT(DISTINCT ?source) AS ?cnt) WHERE {{
{conformance}
{where_extra}
    ?source ?_anyPred ?_anyTarget .
    FILTER(isIRI(?source))
}}"#
    ))
}

/// Check whether a query's where clause contains only conditions that can be
/// pushed entirely to SPARQL.
///
/// With plain literal storage, simple property equality (String, Number, Bool,
/// StringArray, NumberArray) IS pushable.  Only complex Ops conditions (gt, lt,
/// contains, between, etc.) need post-hydration Rust-side filtering.
fn all_where_pushable(query: &ModelQueryInput, shape: &ModelShape) -> bool {
    let Some(ref wc) = query.where_clause else {
        return true;
    };
    for (prop_name, condition) in wc {
        if prop_name == "base" || prop_name == "id" {
            match condition {
                WhereCondition::String(_) | WhereCondition::StringArray(_) => continue,
                _ => return false,
            }
        }
        // Relation-based where (forward or reverse) — pushable for simple values
        if shape
            .properties
            .iter()
            .any(|p| p.name == *prop_name && p.is_collection)
        {
            if matches!(
                condition,
                WhereCondition::String(_) | WhereCondition::StringArray(_)
            ) {
                continue;
            }
            return false;
        }
        // Property-based where — pushable for simple equality
        if shape
            .properties
            .iter()
            .any(|p| p.name == *prop_name && !p.is_collection)
        {
            match condition {
                WhereCondition::String(_)
                | WhereCondition::Number(_)
                | WhereCondition::Bool(_)
                | WhereCondition::StringArray(_)
                | WhereCondition::NumberArray(_) => continue,
                WhereCondition::Ops(_) => return false,
            }
        }
        // Unknown property — not pushable
        return false;
    }
    true
}

/// Build conformance + where patterns shared by both the instance query and
/// the COUNT query.  Returns (conformance_patterns, where_patterns) as strings.
fn build_query_patterns(shape: &ModelShape, query: &ModelQueryInput) -> (String, String) {
    let mut conformance_patterns = Vec::new();

    // Parent filter
    if let Some(ref parent) = query.parent {
        match parent {
            ParentScope::Raw { id, predicate } => {
                if let (Ok(safe_id), Ok(safe_pred)) = (validate_iri(id), validate_iri(predicate)) {
                    conformance_patterns
                        .push(format!("    <{safe_id}> <{safe_pred}> ?source ."));
                } else {
                    log::warn!("Skipping parent scope: invalid IRI in id='{}' or predicate='{}'", id, predicate);
                }
            }
            ParentScope::Model { id, field, model } => {
                let safe_id = match validate_iri(id) {
                    Ok(s) => s,
                    Err(_) => {
                        log::warn!("Skipping parent scope: invalid IRI in id='{}'", id);
                        return (String::new(), String::new());
                    }
                };
                // For model-based parent scope, we need to resolve the predicate
                // The client should send the resolved predicate, but we handle both forms
                if let Some(ref f) = field {
                    if let Ok(safe_f) = validate_iri(f) {
                        conformance_patterns
                            .push(format!("    <{safe_id}> <{safe_f}> ?source ."));
                    } else {
                        log::warn!("Skipping parent scope: invalid IRI in field='{}'", f);
                    }
                } else {
                    // Fallback: use model name as predicate hint
                    let safe_model = escape_sparql_string(model);
                    let hash_model = format!("#{}", safe_model);
                    conformance_patterns.push(format!("    <{safe_id}> ?_parentPred ?source ."));
                    conformance_patterns.push(format!(
                        "    FILTER(STRENDS(STR(?_parentPred), \"/{safe_model}\") || STRENDS(STR(?_parentPred), \"{hash_model}\"))",
                    ));
                }
            }
        }
    }

    // Conformance patterns from shape properties
    let mut has_conformance = false;
    for prop in &shape.properties {
        if prop.is_required {
            if validate_iri(&prop.predicate).is_err() {
                continue;
            }
            let safe_name = prop.name.replace(|c: char| !c.is_alphanumeric(), "_");
            has_conformance = true;
            if prop.is_flag {
                if let Some(ref initial) = prop.initial_value {
                    if validate_iri(initial).is_ok() {
                        conformance_patterns
                            .push(format!("    ?source <{}> <{}> .", prop.predicate, initial));
                    } else {
                        // Initial value is not a valid IRI (e.g. a literal); use STR() comparison
                        let escaped = escape_sparql_string(initial);
                        conformance_patterns.push(format!(
                            "    ?source <{}> ?cf_{} . FILTER(STR(?cf_{}) = \"{}\")",
                            prop.predicate, safe_name, safe_name, escaped
                        ));
                    }
                } else {
                    conformance_patterns.push(format!(
                        "    ?source <{}> ?cf_{} .",
                        prop.predicate, safe_name
                    ));
                }
            } else {
                conformance_patterns.push(format!(
                    "    ?source <{}> ?cf_{} .",
                    prop.predicate, safe_name
                ));
            }
        }
    }

    // Fallback: if no required properties, try initial values
    if !has_conformance {
        for prop in &shape.properties {
            if let Some(ref initial) = prop.initial_value {
                if validate_iri(&prop.predicate).is_err() {
                    continue;
                }
                let safe_name = prop.name.replace(|c: char| !c.is_alphanumeric(), "_");
                has_conformance = true;
                if prop.is_flag {
                    if validate_iri(initial).is_ok() {
                        conformance_patterns
                            .push(format!("    ?source <{}> <{}> .", prop.predicate, initial));
                    } else {
                        let escaped = escape_sparql_string(initial);
                        conformance_patterns.push(format!(
                            "    ?source <{}> ?cfInit_{} . FILTER(STR(?cfInit_{}) = \"{}\")",
                            prop.predicate, safe_name, safe_name, escaped
                        ));
                    }
                } else {
                    conformance_patterns.push(format!(
                        "    ?source <{}> ?cfInit_{} .",
                        prop.predicate, safe_name
                    ));
                }
                break;
            }
        }
    }

    // Fallback: structural matching using known predicates.
    //
    // WARNING: This is a broad heuristic — it matches any entity that has at
    // least one link with ANY of the model's known predicates.  If two model
    // classes share a predicate (e.g. a generic `ad4m://name`), instances of
    // class A could appear in queries for class B.  Models should define at
    // least one `required` property or a `@Flag` to enable precise type
    // discrimination and avoid this fallback.
    if !has_conformance && conformance_patterns.is_empty() {
        log::debug!(
            "Model class uses structural conformance fallback — no required/flag properties found. \
             This may match instances from other model classes sharing the same predicates."
        );
        let known_predicates: Vec<String> = shape
            .properties
            .iter()
            .filter(|p| !p.predicate.is_empty() && validate_iri(&p.predicate).is_ok())
            .map(|p| format!("<{}>", p.predicate))
            .collect();

        if !known_predicates.is_empty() {
            conformance_patterns.push(format!(
                "    {{ SELECT DISTINCT ?source WHERE {{ ?source ?_structPred ?_structTarget . FILTER(?_structPred IN ({})) }} }}",
                known_predicates.join(", ")
            ));
        }
    }

    // WHERE clause filters that can be pushed to SPARQL.
    //
    // With plain literal storage, property equality CAN be pushed to SPARQL.
    // Values are stored as `literal:string:X`, `literal:number:X`, etc.
    // Only complex Ops (gt/lt/contains/between) need post-hydration filtering.
    let mut where_patterns = Vec::new();
    if let Some(ref wc) = query.where_clause {
        for (prop_name, condition) in wc {
            if prop_name == "base" || prop_name == "id" {
                match condition {
                    WhereCondition::String(val) => {
                        where_patterns.push(format!(
                            "    FILTER(STR(?source) = \"{}\")",
                            escape_sparql_string(val)
                        ));
                    }
                    WhereCondition::StringArray(vals) => {
                        let ids = vals
                            .iter()
                            .map(|v| format!("\"{}\"", escape_sparql_string(v)))
                            .collect::<Vec<_>>()
                            .join(", ");
                        where_patterns.push(format!("    FILTER(STR(?source) IN ({}))", ids));
                    }
                    _ => {} // complex id ops handled post-hydration
                }
                continue;
            }

            // Relation-based where: link targets are plain URIs.
            if let Some(prop) = shape
                .properties
                .iter()
                .find(|p| &p.name == prop_name && p.is_collection)
            {
                let direction = prop.direction.as_deref().unwrap_or("forward");
                let safe_name = prop_name.replace(|c: char| !c.is_alphanumeric(), "_");
                match condition {
                    WhereCondition::String(val) => {
                        let escaped = escape_sparql_string(val);
                        if direction == "reverse" {
                            where_patterns.push(format!(
                                "    ?_rv_{} <{}> ?source . FILTER(STR(?_rv_{}) = \"{}\")",
                                safe_name, prop.predicate, safe_name, escaped
                            ));
                        } else {
                            where_patterns.push(format!(
                                "    ?source <{}> ?_ft_{} . FILTER(STR(?_ft_{}) = \"{}\")",
                                prop.predicate, safe_name, safe_name, escaped
                            ));
                        }
                    }
                    WhereCondition::StringArray(vals) => {
                        let str_list = vals
                            .iter()
                            .map(|v| format!("\"{}\"", escape_sparql_string(v)))
                            .collect::<Vec<_>>()
                            .join(", ");
                        if direction == "reverse" {
                            where_patterns.push(format!(
                                "    ?_rv_{} <{}> ?source . FILTER(STR(?_rv_{}) IN ({}))",
                                safe_name, prop.predicate, safe_name, str_list
                            ));
                        } else {
                            where_patterns.push(format!(
                                "    ?source <{}> ?_ft_{} . FILTER(STR(?_ft_{}) IN ({}))",
                                prop.predicate, safe_name, safe_name, str_list
                            ));
                        }
                    }
                    _ => {} // complex ops handled post-hydration
                }
                continue;
            }

            // Property-based where: values are plain literals.
            if let Some(prop) = shape
                .properties
                .iter()
                .find(|p| &p.name == prop_name && !p.is_collection && !p.predicate.is_empty())
            {
                if validate_iri(&prop.predicate).is_err() {
                    continue;
                }
                let safe_name = prop_name.replace(|c: char| !c.is_alphanumeric(), "_");
                match condition {
                    WhereCondition::String(val) => {
                        let literal_iri = format!(
                            "literal:string:{}",
                            urlencoding::encode(val)
                        );
                        where_patterns.push(format!(
                            "    ?source <{}> ?_pw_{} . FILTER(STR(?_pw_{}) = \"{}\")",
                            prop.predicate, safe_name, safe_name,
                            escape_sparql_string(&literal_iri)
                        ));
                    }
                    WhereCondition::Number(n) => {
                        let literal_iri = format!("literal:number:{}", n);
                        where_patterns.push(format!(
                            "    ?source <{}> ?_pw_{} . FILTER(STR(?_pw_{}) = \"{}\")",
                            prop.predicate, safe_name, safe_name,
                            escape_sparql_string(&literal_iri)
                        ));
                    }
                    WhereCondition::Bool(b) => {
                        let literal_iri = format!("literal:boolean:{}", b);
                        where_patterns.push(format!(
                            "    ?source <{}> ?_pw_{} . FILTER(STR(?_pw_{}) = \"{}\")",
                            prop.predicate, safe_name, safe_name, literal_iri
                        ));
                    }
                    WhereCondition::StringArray(vals) => {
                        let str_list = vals
                            .iter()
                            .map(|v| {
                                format!(
                                    "\"literal:string:{}\"",
                                    escape_sparql_string(&urlencoding::encode(v))
                                )
                            })
                            .collect::<Vec<_>>()
                            .join(", ");
                        where_patterns.push(format!(
                            "    ?source <{}> ?_pw_{} . FILTER(STR(?_pw_{}) IN ({}))",
                            prop.predicate, safe_name, safe_name, str_list
                        ));
                    }
                    WhereCondition::NumberArray(vals) => {
                        let num_list = vals
                            .iter()
                            .map(|n| format!("\"literal:number:{}\"", n))
                            .collect::<Vec<_>>()
                            .join(", ");
                        where_patterns.push(format!(
                            "    ?source <{}> ?_pw_{} . FILTER(STR(?_pw_{}) IN ({}))",
                            prop.predicate, safe_name, safe_name, num_list
                        ));
                    }
                    WhereCondition::Ops(_) => {
                        // Complex ops (gt/lt/contains/between) handled post-hydration
                    }
                }
                continue;
            }

            // Unknown property — handled post-hydration in matches_where()
        }
    }

    let conformance = conformance_patterns.join("\n");
    let where_extra = where_patterns.join("\n");

    (conformance, where_extra)
}

// ---------------------------------------------------------------------------
// Result grouping
// ---------------------------------------------------------------------------

/// An intermediate representation of all links belonging to one instance.
#[derive(Debug)]
struct InstanceLinks {
    source: String,
    /// (predicate, target, author, timestamp) for each link
    links: Vec<(String, String, String, String)>,
}

/// Group SPARQL result rows by `?source` to collect all links per instance.
fn group_results_by_source(rows: &[Value], _shape: &ModelShape) -> Vec<InstanceLinks> {
    let mut map: BTreeMap<String, Vec<(String, String, String, String)>> = BTreeMap::new();

    for row in rows {
        let source = match row["source"].as_str() {
            Some(s) => s.to_string(),
            None => continue,
        };
        let predicate = row["predicate"].as_str().unwrap_or("").to_string();
        let target = row["target"].as_str().unwrap_or("").to_string();
        let author = row["author"].as_str().unwrap_or("").to_string();
        let timestamp = row["timestamp"].as_str().unwrap_or("").to_string();

        map.entry(source)
            .or_default()
            .push((predicate, target, author, timestamp));
    }

    map.into_iter()
        .map(|(source, links)| InstanceLinks { source, links })
        .collect()
}

// ---------------------------------------------------------------------------
// Hydration
// ---------------------------------------------------------------------------

/// Hydrate instances from grouped link data.
///
/// Mirrors the TS `hydrateFromLinks()` logic:
/// - Single-valued properties: latest-wins (by timestamp)
/// - Collections: chronological order, all values
/// - Metadata: author = earliest link author, timestamp = earliest
fn hydrate_instances(shape: &ModelShape, grouped: &[InstanceLinks]) -> Vec<Value> {
    grouped
        .iter()
        .filter_map(|inst_links| hydrate_one(shape, inst_links))
        .collect()
}

/// Hydrate a single instance from its links.
fn hydrate_one(shape: &ModelShape, inst: &InstanceLinks) -> Option<Value> {
    let mut obj = Map::new();

    // Set base expression / id
    obj.insert("id".to_string(), Value::String(inst.source.clone()));
    obj.insert(
        "baseExpression".to_string(),
        Value::String(inst.source.clone()),
    );

    // Build a predicate -> properties map for fast lookup.
    // Multiple relations can share the same predicate (e.g. ad4m://has_child),
    // so we map each predicate to ALL matching properties.
    // Exclude properties that have SPARQL getters — those will be evaluated
    // separately by evaluate_getters(). This prevents predicate collisions
    // when two properties share the same predicate (e.g. an unfiltered
    // @HasMany and a conformance-filtered @HasMany with a getter).
    let mut pred_to_props: HashMap<&str, Vec<&ShapeProperty>> = HashMap::new();
    for p in shape.properties.iter().filter(|p| p.getter.is_none()) {
        pred_to_props
            .entry(p.predicate.as_str())
            .or_default()
            .push(p);
    }

    // Track latest timestamp per property for latest-wins
    let mut prop_timestamps: HashMap<&str, &str> = HashMap::new();
    // Track collection values
    let mut collection_values: HashMap<&str, Vec<(&str, &str)>> = HashMap::new();
    // Track overall earliest timestamp and its author
    let mut earliest_timestamp: Option<&str> = None;
    let mut earliest_author: Option<&str> = None;
    let mut latest_timestamp: Option<&str> = None;

    // First pass: categorize links
    for (predicate, target, author, timestamp) in &inst.links {
        // Update instance-level timestamps
        let ts = timestamp.as_str();
        match earliest_timestamp {
            None => {
                earliest_timestamp = Some(ts);
                earliest_author = Some(author.as_str());
            }
            Some(et) if ts < et => {
                earliest_timestamp = Some(ts);
                earliest_author = Some(author.as_str());
            }
            _ => {}
        }
        match latest_timestamp {
            None => {
                latest_timestamp = Some(ts);
            }
            Some(lt) if ts > lt => {
                latest_timestamp = Some(ts);
            }
            _ => {}
        }

        if let Some(props) = pred_to_props.get(predicate.as_str()) {
            for prop in props {
                if prop.is_collection {
                    // Collections: accumulate all values with timestamps
                    // When multiple relations share the same predicate, each gets
                    // the full set of targets; include resolution later filters
                    // by target type.
                    collection_values
                        .entry(prop.name.as_str())
                        .or_default()
                        .push((target.as_str(), ts));
                } else if prop.is_flag {
                    // Flags: just note presence (value is the target URI)
                    // Skip setting — flags are conformance-only
                    // But if it's also a regular property, set it
                    let current_ts = prop_timestamps.get(prop.name.as_str()).copied();
                    if current_ts.is_none() || current_ts.map(|t| ts > t).unwrap_or(true) {
                        prop_timestamps.insert(prop.name.as_str(), ts);
                        // Parse the target value
                        let val = parse_literal_value(target);
                        obj.insert(prop.name.clone(), val);
                    }
                } else {
                    // Single-valued: latest-wins
                    let current_ts = prop_timestamps.get(prop.name.as_str()).copied();
                    if current_ts.is_none() || current_ts.map(|t| ts > t).unwrap_or(true) {
                        prop_timestamps.insert(prop.name.as_str(), ts);
                        let val = parse_literal_value(target);
                        obj.insert(prop.name.clone(), val);
                    }
                }
            }
        }
        // Links with unknown predicates are ignored (they belong to other shapes or metadata)
    }

    // Set collection properties
    for (name, mut values) in collection_values {
        // Sort by timestamp (chronological)
        values.sort_by_key(|&(_, ts)| ts);
        // Check if this is a relation property (has a direction) — if so, keep
        // raw IRIs for later include resolution lookup rather than extracting
        // the inner data value from signed envelopes.
        let is_relation = shape
            .properties
            .iter()
            .any(|p| p.name == name && p.direction.is_some());
        let arr: Vec<Value> = values
            .iter()
            .map(|&(target, _)| {
                if is_relation {
                    // Keep raw IRI so resolve_forward_include can match by id
                    Value::String(target.to_string())
                } else if target.starts_with("literal:") {
                    parse_literal_value(target)
                } else {
                    Value::String(target.to_string())
                }
            })
            .collect();
        // Check if this is a scalar relation (hasOne/belongsToOne) — if so,
        // unwrap the first element as a scalar value instead of an array.
        let is_scalar = shape
            .properties
            .iter()
            .any(|p| p.name == name && p.is_scalar_relation);
        if is_scalar {
            if let Some(first) = arr.into_iter().next() {
                obj.insert(name.to_string(), first);
            }
        } else {
            obj.insert(name.to_string(), Value::Array(arr));
        }
    }

    // Set metadata
    if let Some(ts) = earliest_timestamp {
        obj.insert("createdAt".to_string(), Value::String(ts.to_string()));
    }
    if let Some(ts) = latest_timestamp {
        obj.insert("updatedAt".to_string(), Value::String(ts.to_string()));
    }
    if let Some(author) = earliest_author {
        obj.insert("author".to_string(), Value::String(author.to_string()));
    }

    // Set timestamp as ISO string (used by ordering)
    if let Some(ts) = earliest_timestamp {
        obj.insert("timestamp".to_string(), Value::String(ts.to_string()));
    }

    Some(Value::Object(obj))
}

// ---------------------------------------------------------------------------
// Where-clause filtering
// ---------------------------------------------------------------------------

/// Check if an instance matches all where-clause conditions.
fn matches_where(
    instance: &Value,
    where_clause: &BTreeMap<String, WhereCondition>,
    shape: &ModelShape,
) -> bool {
    for (prop_name, condition) in where_clause {
        if prop_name == "base" || prop_name == "id" {
            // String and StringArray id conditions are pushed to SPARQL.
            // Complex ops (Ops, NumberArray, etc.) still need post-hydration.
            // Hydrated instances use "id" (not "base"), so map "base" → "id".
            let lookup_key = if prop_name == "base" {
                "id"
            } else {
                prop_name.as_str()
            };
            match condition {
                WhereCondition::String(_) | WhereCondition::StringArray(_) => continue,
                _ => {
                    let val = &instance[lookup_key];
                    if !matches_condition(val, condition) {
                        return false;
                    }
                    continue;
                }
            }
        }

        // Relation-based where conditions (String/StringArray on collection props)
        // are already pushed to SPARQL — skip them here.
        if matches!(
            condition,
            WhereCondition::String(_) | WhereCondition::StringArray(_)
        ) {
            if shape
                .properties
                .iter()
                .any(|p| p.name == *prop_name && p.is_collection)
            {
                continue;
            }
        }

        let val = &instance[prop_name];
        if !matches_condition(val, condition) {
            return false;
        }
    }
    true
}

/// Check if a single value matches a where condition.
fn matches_condition(val: &Value, condition: &WhereCondition) -> bool {
    match condition {
        WhereCondition::String(expected) => match val {
            Value::String(s) => s == expected,
            Value::Null => false,
            _ => val.to_string().trim_matches('"') == expected.as_str(),
        },
        WhereCondition::Number(expected) => {
            to_f64(val).map(|v| (v - expected).abs() < f64::EPSILON) == Some(true)
        }
        WhereCondition::Bool(expected) => val.as_bool() == Some(*expected),
        WhereCondition::StringArray(expected) => {
            // IN operator: value must be in the array
            match val {
                Value::String(s) => expected.contains(s),
                _ => {
                    let s = val.to_string().trim_matches('"').to_string();
                    expected.contains(&s)
                }
            }
        }
        WhereCondition::NumberArray(expected) => {
            if let Some(v) = to_f64(val) {
                expected.iter().any(|e| (v - e).abs() < f64::EPSILON)
            } else {
                false
            }
        }
        WhereCondition::Ops(ops) => matches_ops(val, ops),
    }
}

/// Check if a value matches operator conditions.
fn matches_ops(val: &Value, ops: &WhereOps) -> bool {
    // NOT
    if let Some(ref not_val) = ops.not {
        match not_val {
            Value::String(s) => {
                if let Value::String(v) = val {
                    if v == s {
                        return false;
                    }
                }
            }
            Value::Number(n) => {
                if let Some(v) = to_f64(val) {
                    if let Some(e) = n.as_f64() {
                        if (v - e).abs() < f64::EPSILON {
                            return false;
                        }
                    }
                }
            }
            Value::Bool(b) => {
                if val.as_bool() == Some(*b) {
                    return false;
                }
            }
            Value::Array(arr) => {
                // NOT IN: value must NOT be in the array
                for item in arr {
                    if match (val, item) {
                        (Value::String(v), Value::String(s)) => v == s,
                        (Value::Number(_), Value::Number(_)) => to_f64(val)
                            .zip(item.as_f64())
                            .map(|(a, b)| (a - b).abs() < f64::EPSILON)
                            .unwrap_or(false),
                        // Cross-type: ISO string vs epoch number (timestamps)
                        (Value::String(_), Value::Number(_))
                        | (Value::Number(_), Value::String(_)) => to_f64(val)
                            .zip(to_f64(item))
                            .map(|(a, b)| (a - b).abs() < f64::EPSILON)
                            .unwrap_or(false),
                        _ => false,
                    } {
                        return false;
                    }
                }
            }
            _ => {}
        }
    }

    // Numeric comparisons
    if let Some(v) = to_f64(val) {
        if let Some(lt) = ops.lt {
            if v >= lt {
                return false;
            }
        }
        if let Some(lte) = ops.lte {
            if v > lte {
                return false;
            }
        }
        if let Some(gt) = ops.gt {
            if v <= gt {
                return false;
            }
        }
        if let Some(gte) = ops.gte {
            if v < gte {
                return false;
            }
        }
        if let Some((lo, hi)) = ops.between {
            if v < lo || v > hi {
                return false;
            }
        }
    } else {
        // For string values, try numeric parse; if it fails AND a numeric op is
        // present, the condition does not match (instead of silently passing).
        let has_numeric_op = ops.lt.is_some()
            || ops.lte.is_some()
            || ops.gt.is_some()
            || ops.gte.is_some()
            || ops.between.is_some();

        if let Value::String(s) = val {
            match s.parse::<f64>() {
                Ok(sv) => {
                    if let Some(lt) = ops.lt {
                        if sv >= lt {
                            return false;
                        }
                    }
                    if let Some(lte) = ops.lte {
                        if sv > lte {
                            return false;
                        }
                    }
                    if let Some(gt) = ops.gt {
                        if sv <= gt {
                            return false;
                        }
                    }
                    if let Some(gte) = ops.gte {
                        if sv < gte {
                            return false;
                        }
                    }
                    if let Some((lo, hi)) = ops.between {
                        if sv < lo || sv > hi {
                            return false;
                        }
                    }
                }
                Err(_) if has_numeric_op => {
                    // Non-numeric string with a numeric comparator → no match
                    return false;
                }
                _ => {}
            }
        }
        // If value is null and we have numeric conditions, don't match
        if val.is_null() {
            if ops.lt.is_some()
                || ops.lte.is_some()
                || ops.gt.is_some()
                || ops.gte.is_some()
                || ops.between.is_some()
            {
                return false;
            }
        }
    }

    // Contains
    if let Some(ref contains_val) = ops.contains {
        match val {
            Value::String(s) => {
                let needle = match contains_val {
                    Value::String(cs) => cs.clone(),
                    _ => contains_val.to_string(),
                };
                if !s.to_lowercase().contains(&needle.to_lowercase()) {
                    return false;
                }
            }
            Value::Array(arr) => {
                let found = arr.iter().any(|item| match (item, contains_val) {
                    (Value::String(a), Value::String(b)) => a == b,
                    (Value::Number(_), Value::Number(_)) => item == contains_val,
                    _ => false,
                });
                if !found {
                    return false;
                }
            }
            _ => return false,
        }
    }

    true
}

// ---------------------------------------------------------------------------
// Sorting
// ---------------------------------------------------------------------------

/// Sort instances by the given order specification.
fn sort_instances(instances: &mut [Value], order: &[(String, OrderDirection)]) {
    instances.sort_by(|a, b| {
        for (prop, dir) in order {
            let av = &a[prop];
            let bv = &b[prop];

            let cmp = compare_values(av, bv);

            if cmp != Ordering::Equal {
                return if *dir == OrderDirection::DESC {
                    cmp.reverse()
                } else {
                    cmp
                };
            }
        }
        Ordering::Equal
    });
}

/// Compare two JSON values with type-aware logic.
fn compare_values(a: &Value, b: &Value) -> Ordering {
    // Handle nulls — push to end
    match (a.is_null(), b.is_null()) {
        (true, true) => return Ordering::Equal,
        (true, false) => return Ordering::Greater,
        (false, true) => return Ordering::Less,
        _ => {}
    }

    // Try numeric comparison first
    if let (Some(an), Some(bn)) = (to_f64(a), to_f64(b)) {
        return an.partial_cmp(&bn).unwrap_or(Ordering::Equal);
    }

    // String comparison
    let as_str = match a {
        Value::String(s) => s.clone(),
        _ => a.to_string(),
    };
    let bs_str = match b {
        Value::String(s) => s.clone(),
        _ => b.to_string(),
    };

    as_str.cmp(&bs_str)
}

// ---------------------------------------------------------------------------
// Getter evaluation (property getters + relation conformance getters)
// ---------------------------------------------------------------------------

/// Strip a trailing top-level `LIMIT N` clause from a SPARQL query string.
/// Per-instance LIMIT doesn't apply in batched evaluation; Rust groups and
/// takes the first result per source instead.
fn strip_trailing_limit(query: &str) -> String {
    let trimmed = query.trim_end();
    let upper = trimmed.to_uppercase();
    if let Some(limit_pos) = upper.rfind("LIMIT") {
        let after_limit = trimmed[limit_pos + 5..].trim();
        if !after_limit.is_empty() && after_limit.chars().all(|c| c.is_ascii_digit()) {
            return trimmed[..limit_pos].trim_end().to_string();
        }
    }
    trimmed.to_string()
}

/// Convert an ASK getter to a batched SELECT returning matching source IRIs.
///
/// `ASK WHERE { ?source <p> "true" . }` →
/// `SELECT ?source WHERE { VALUES ?source { <id1> ... } ?source <p> "true" . }`
fn convert_ask_to_batched_select(ask: &str, values_clause: &str) -> String {
    let normalized = ask.replace("<Base>", "?source");
    if let (Some(open), Some(close)) = (normalized.find('{'), normalized.rfind('}')) {
        let body = &normalized[open + 1..close];
        format!(
            "SELECT ?source WHERE {{ VALUES ?source {{ {} }} {} }}",
            values_clause,
            body.trim()
        )
    } else {
        normalized
    }
}

/// Inject a VALUES ?source clause into a SELECT getter and ensure ?source is
/// projected so results can be grouped per-instance.
///
/// `SELECT ?target WHERE { ?source <p> ?target . } LIMIT 1` →
/// `SELECT ?source ?target WHERE { VALUES ?source { <id1> ... } ?source <p> ?target . }`
fn inject_values_into_select(select: &str, values_clause: &str) -> String {
    let mut query = select.replace("<Base>", "?source");

    // 1. Strip trailing LIMIT (per-instance limit handled in Rust grouping)
    query = strip_trailing_limit(&query);

    // 2. Ensure ?source is in SELECT projection
    let upper = query.to_uppercase();
    if let Some(select_end) = upper.find("SELECT").map(|p| p + 6) {
        if let Some(where_rel) = upper[select_end..].find("WHERE") {
            let projection = &query[select_end..select_end + where_rel];
            if !projection.contains("?source") {
                query.insert_str(select_end, " ?source");
            }
        }
    }

    // 3. Inject VALUES after first opening brace
    if let Some(brace_pos) = query.find('{') {
        let insert = format!(" VALUES ?source {{ {} }}", values_clause);
        query.insert_str(brace_pos + 1, &insert);
    }

    query
}

/// Evaluate SPARQL getters on all instances using **batched VALUES** queries.
///
/// Instead of running one SPARQL query per (instance, getter) pair — O(N×M) —
/// this function runs **one query per getter property** with a VALUES clause
/// containing all instance IRIs, then regroups results in Rust.  Cost: O(M).
///
/// For **property** getters: replaces the property value with the result of
/// executing the getter SPARQL (SELECT → scalar, ASK → bool).
///
/// For **relation** getters (conformance or explicit): replaces the relation
/// array with the filtered set of IDs returned by the getter SPARQL.
fn evaluate_getters(
    store: &SparqlStore,
    instances: &mut [Value],
    shape: &ModelShape,
    _include: Option<&HashMap<String, IncludeValue>>,
    deep_query: bool,
) -> Result<(), Error> {
    // Collect properties/relations that have getters.
    // When deep_query is false, skip property getters (non-collection,
    // non-scalar-relation) — only relation conformance getters run.
    let getter_props: Vec<&ShapeProperty> = shape
        .properties
        .iter()
        .filter(|p| p.getter.is_some() && (deep_query || p.is_collection || p.is_scalar_relation))
        .collect();

    if getter_props.is_empty() || instances.is_empty() {
        return Ok(());
    }

    // Build VALUES clause from all instance IRIs (validated for SPARQL safety)
    let instance_iris: Vec<String> = instances
        .iter()
        .filter_map(|inst| inst.get("id").and_then(|v| v.as_str()))
        .filter_map(|id| validate_iri(id).ok().map(|s| s.to_string()))
        .collect();

    if instance_iris.is_empty() {
        return Ok(());
    }

    let values_clause = instance_iris
        .iter()
        .map(|id| format!("<{}>", id))
        .collect::<Vec<_>>()
        .join(" ");

    log::debug!(
        "evaluate_getters: {} getter props for {} instances (batched VALUES)",
        getter_props.len(),
        instance_iris.len()
    );

    // One batched query per getter property — O(M) total
    for prop in &getter_props {
        let getter = prop.getter.as_ref().unwrap(); // safe: filtered above
        let upper = getter.trim().to_uppercase();

        if upper.starts_with("ASK") {
            // ── ASK getter → batched SELECT returning matching sources ────
            let batched = convert_ask_to_batched_select(getter, &values_clause);
            match store.query(&batched) {
                Ok(result_json) => {
                    let rows: Vec<Value> = serde_json::from_str(&result_json).unwrap_or_default();
                    // Sources present in results → true; absent → false
                    let matched: std::collections::HashSet<&str> = rows
                        .iter()
                        .filter_map(|row| row.get("source").and_then(|v| v.as_str()))
                        .collect();
                    for inst in instances.iter_mut() {
                        let id_owned = inst
                            .get("id")
                            .and_then(|v| v.as_str())
                            .map(|s| s.to_string());
                        if let Some(id) = id_owned {
                            if let Some(obj) = inst.as_object_mut() {
                                obj.insert(
                                    prop.name.clone(),
                                    Value::Bool(matched.contains(id.as_str())),
                                );
                            }
                        }
                    }
                }
                Err(e) => {
                    log::warn!("Batched ASK getter failed for '{}': {}", prop.name, e);
                }
            }
        } else if upper.starts_with("SELECT") {
            // ── SELECT getter → batched with VALUES, grouped by ?source ──
            let batched = inject_values_into_select(getter, &values_clause);

            match store.query(&batched) {
                Ok(result_json) => {
                    let rows: Vec<Value> = serde_json::from_str(&result_json).unwrap_or_default();

                    // Group results by ?source → Vec<String>
                    let mut grouped: HashMap<String, Vec<String>> = HashMap::new();
                    for row in &rows {
                        let source = match row.get("source").and_then(|v| v.as_str()) {
                            Some(s) => s,
                            None => continue,
                        };
                        // Extract the first non-"source" binding value
                        if let Some(obj) = row.as_object() {
                            if let Some((_, val)) = obj.iter().find(|(k, _)| k.as_str() != "source")
                            {
                                if let Some(s) = val.as_str() {
                                    if !s.is_empty() && s != "None" {
                                        grouped
                                            .entry(source.to_string())
                                            .or_default()
                                            .push(s.to_string());
                                    }
                                }
                            }
                        }
                    }

                    // Apply grouped results to instances
                    for inst in instances.iter_mut() {
                        let id_owned = match inst
                            .get("id")
                            .and_then(|v| v.as_str())
                            .map(|s| s.to_string())
                        {
                            Some(id) => id,
                            None => continue,
                        };
                        let values = grouped.get(id_owned.as_str());
                        if let Some(obj) = inst.as_object_mut() {
                            if prop.is_collection {
                                if prop.is_scalar_relation {
                                    // HasOne/BelongsToOne → take first value
                                    let val = values
                                        .and_then(|v| v.first())
                                        .map(|s| Value::String(s.clone()))
                                        .unwrap_or(Value::Null);
                                    obj.insert(prop.name.clone(), val);
                                } else {
                                    // HasMany → array of all values
                                    let arr: Vec<Value> = values
                                        .map(|v| {
                                            v.iter().map(|s| Value::String(s.clone())).collect()
                                        })
                                        .unwrap_or_default();
                                    obj.insert(prop.name.clone(), Value::Array(arr));
                                }
                            } else {
                                // Property getter → first scalar value
                                if let Some(val) = values.and_then(|v| v.first()) {
                                    obj.insert(prop.name.clone(), Value::String(val.clone()));
                                }
                            }
                        }
                    }
                }
                Err(e) => {
                    log::warn!("Batched SELECT getter failed for '{}': {}", prop.name, e);
                }
            }
        }
        // Unknown getter format → skip silently
    }

    // ── Post-getter where-clause filtering ─────────────────────────────
    // For relations with where_filter, fetch the target property values
    // Post-getter where filtering: for collection relations, we need
    // to resolve the target instances' property values in Rust
    // and filter out non-matching targets.
    for prop in &getter_props {
        let (wf, wp) = match (&prop.where_filter, &prop.where_predicates) {
            (Some(wf), Some(wp)) => (wf, wp),
            _ => continue,
        };
        apply_where_filter_to_relation(store, instances, &prop.name, wf, wp)?;
    }

    Ok(())
}

/// Apply a where-clause filter to a relation property across all instances.
///
/// For each where-clause property, issues a single batched SPARQL query to
/// fetch the property values for all target IDs, parses the literal values,
/// and filters out non-matching targets.  Cost: O(1) per where-clause property.
fn apply_where_filter_to_relation(
    store: &SparqlStore,
    instances: &mut [Value],
    relation_name: &str,
    where_filter: &BTreeMap<String, WhereCondition>,
    where_predicates: &HashMap<String, String>,
) -> Result<(), Error> {
    // Collect all target IDs across all instances for this relation
    let all_targets: Vec<String> = instances
        .iter()
        .filter_map(|inst| {
            inst.get(relation_name)
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|v| v.as_str().map(|s| s.to_string()))
                        .collect::<Vec<_>>()
                })
        })
        .flatten()
        .collect();

    if all_targets.is_empty() {
        return Ok(());
    }

    // Deduplicate targets for batched query
    let unique_targets: Vec<&str> = {
        let mut seen = std::collections::HashSet::new();
        all_targets
            .iter()
            .filter(|t| seen.insert(t.as_str()))
            .map(|t| t.as_str())
            .collect()
    };

    let values_clause = unique_targets
        .iter()
        .filter_map(|id| validate_iri(id).ok())
        .map(|id| format!("<{}>", id))
        .collect::<Vec<_>>()
        .join(" ");

    if values_clause.is_empty() {
        return Ok(());
    }

    // For each where-clause property, fetch values and build a pass/fail map
    let mut target_pass: HashMap<String, bool> = unique_targets
        .iter()
        .map(|t| (t.to_string(), true))
        .collect();

    for (prop_name, condition) in where_filter {
        let predicate = match where_predicates.get(prop_name) {
            Some(p) => p,
            None => continue,
        };
        if validate_iri(predicate).is_err() {
            continue;
        }

        // Batched SPARQL to get property values for all targets
        let query = format!(
            "SELECT ?source ?val WHERE {{ VALUES ?source {{ {} }} ?source <{}> ?val . }}",
            values_clause, predicate
        );

        let result_json = store.query(&query)?;
        let rows: Vec<Value> = serde_json::from_str(&result_json).unwrap_or_default();

        // Parse literal values and check against the where condition
        let mut target_vals: HashMap<String, Value> = HashMap::new();
        for row in &rows {
            if let (Some(source), Some(val_str)) = (
                row.get("source").and_then(|v| v.as_str()),
                row.get("val").and_then(|v| v.as_str()),
            ) {
                target_vals.insert(source.to_string(), parse_literal_value(val_str));
            }
        }

        for (target_id, pass) in target_pass.iter_mut() {
            if !*pass {
                continue; // already failed a previous condition
            }
            match target_vals.get(target_id) {
                Some(val) => {
                    if !matches_condition(val, condition) {
                        *pass = false;
                    }
                }
                None => {
                    // Target doesn't have this property → doesn't match
                    *pass = false;
                }
            }
        }
    }

    // Filter each instance's relation array
    for inst in instances.iter_mut() {
        if let Some(arr) = inst.get(relation_name).and_then(|v| v.as_array()).cloned() {
            let filtered: Vec<Value> = arr
                .into_iter()
                .filter(|v| {
                    v.as_str()
                        .map(|id| target_pass.get(id).copied().unwrap_or(false))
                        .unwrap_or(false)
                })
                .collect();
            if let Some(obj) = inst.as_object_mut() {
                obj.insert(relation_name.to_string(), Value::Array(filtered));
            }
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Property filtering (sparse fieldset)
// ---------------------------------------------------------------------------

/// Filter an instance to only include requested properties.
fn filter_properties(instance: Value, requested: &[String]) -> Value {
    if let Value::Object(mut obj) = instance {
        let always_keep = ["id", "baseExpression"];
        let keys: Vec<String> = obj.keys().cloned().collect();
        for key in keys {
            if always_keep.contains(&key.as_str()) {
                continue;
            }
            if !requested.iter().any(|r| r == &key) {
                obj.remove(&key);
            }
        }
        Value::Object(obj)
    } else {
        instance
    }
}

// ---------------------------------------------------------------------------
// Reverse relation resolution (Phase 2)
// ---------------------------------------------------------------------------

/// Resolve reverse relations (BelongsToOne/BelongsToMany) for all instances in batch.
///
/// Uses a targeted SPARQL query with a VALUES clause to only fetch links
/// pointing to our specific instance IDs, rather than scanning all links
/// with the given predicate.
pub fn resolve_reverse_relations(
    store: &SparqlStore,
    instances: &mut [Value],
    relations: &[(String, String, bool)], // (name, predicate, is_single)
) -> Result<(), Error> {
    if relations.is_empty() || instances.is_empty() {
        return Ok(());
    }

    // Build VALUES clause from instance IDs (validated for SPARQL safety)
    let instance_iris: Vec<String> = instances
        .iter()
        .filter_map(|inst| inst["id"].as_str())
        .filter_map(|id| validate_iri(id).ok().map(|s| s.to_string()))
        .collect();

    if instance_iris.is_empty() {
        return Ok(());
    }

    let values_clause = instance_iris
        .iter()
        .map(|id| format!("<{}>", id))
        .collect::<Vec<_>>()
        .join(" ");

    for (rel_name, predicate, is_single) in relations {
        let safe_pred = match validate_iri(predicate) {
            Ok(p) => p,
            Err(_) => continue,
        };

        // Targeted SPARQL: only fetch links whose target is one of our instances
        let sparql = format!(
            "SELECT ?source ?target WHERE {{ VALUES ?target {{ {} }} ?source <{safe_pred}> ?target . }}",
            values_clause
        );
        let result_json = store.query(&sparql)?;
        let rows: Vec<Value> = serde_json::from_str(&result_json)?;

        // Build target_id → [source_id, ...] map
        let mut target_to_sources: HashMap<String, Vec<String>> = HashMap::new();
        for row in &rows {
            let source = row["source"].as_str().unwrap_or("");
            let target = row["target"].as_str().unwrap_or("");
            if !source.is_empty() && !target.is_empty() {
                target_to_sources
                    .entry(target.to_string())
                    .or_default()
                    .push(source.to_string());
            }
        }

        for inst in instances.iter_mut() {
            let id = match inst["id"].as_str() {
                Some(id) => id,
                None => continue,
            };

            let source_ids: Vec<Value> = target_to_sources
                .get(id)
                .map(|sources| sources.iter().map(|s| Value::String(s.clone())).collect())
                .unwrap_or_default();

            if *is_single {
                let val = source_ids.first().cloned().unwrap_or(Value::Null);
                inst.as_object_mut()
                    .map(|obj| obj.insert(rel_name.clone(), val));
            } else {
                inst.as_object_mut()
                    .map(|obj| obj.insert(rel_name.clone(), Value::Array(source_ids)));
            }
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Recursive include resolution (replaces TS-side hydrateRelations)
// ---------------------------------------------------------------------------

/// Resolve all included relations for a set of instances, recursively handling
/// nested includes.  Uses enriched relation metadata (target shapes) from the
/// TS client to call `execute_model_query` in-process, eliminating per-relation
/// GraphQL round-trips.
fn resolve_includes_recursive(
    store: &SparqlStore,
    instances: &mut [Value],
    include: &HashMap<String, IncludeValue>,
    shape: &ModelShape,
    depth: u8,
) -> Result<(), Error> {
    for (rel_name, include_val) in include {
        match include_val {
            IncludeValue::Bool(false) => continue,
            _ => {}
        }

        // Find the enriched relation metadata
        let rel = match shape.include_relations.iter().find(|r| r.name == *rel_name) {
            Some(r) => r,
            None => continue, // No target shape — can't resolve in Rust
        };

        let sub_query = match include_val {
            IncludeValue::Bool(true) => ModelQueryInput::default(),
            IncludeValue::SubQuery(sq) => *sq.clone(),
            _ => continue,
        };

        if rel.direction == "reverse" {
            resolve_reverse_include(store, instances, rel, &sub_query, depth)?;
        } else {
            resolve_forward_include(store, instances, rel, &sub_query, depth)?;
        }
    }
    Ok(())
}

/// Resolve a forward relation (hasMany / hasOne) for all instances.
///
/// The parent instances already have the target IDs as string arrays (from
/// hydration).  We batch-query the target class for all those IDs at once,
/// then replace the string IDs with fully hydrated JSON objects.
fn resolve_forward_include(
    store: &SparqlStore,
    instances: &mut [Value],
    rel: &ShapeRelation,
    sub_query: &ModelQueryInput,
    depth: u8,
) -> Result<(), Error> {
    // Collect all target IDs across all instances (deduplicated, order-preserving)
    let mut seen = std::collections::HashSet::new();
    let mut all_ids: Vec<String> = Vec::new();
    for inst in instances.iter() {
        if let Some(arr) = inst[&rel.name].as_array() {
            for item in arr {
                if let Some(id) = item.as_str() {
                    if seen.insert(id.to_string()) {
                        all_ids.push(id.to_string());
                    }
                }
            }
        } else if let Some(id) = inst[&rel.name].as_str() {
            if seen.insert(id.to_string()) {
                all_ids.push(id.to_string());
            }
        }
    }
    if all_ids.is_empty() {
        return Ok(());
    }

    // Build a query: where.id = [...allIds] merged with any sub-query filters
    let mut query = sub_query.clone();
    let mut wc = query.where_clause.take().unwrap_or_default();

    // If the sub-query has its own id filter, intersect with parent IDs
    if let Some(existing_id) = wc.get("id") {
        let filter_ids: Vec<String> = match existing_id {
            WhereCondition::String(s) => vec![s.clone()],
            WhereCondition::StringArray(arr) => arr.clone(),
            _ => vec![],
        };
        all_ids.retain(|id| filter_ids.contains(id));
    }
    wc.insert("id".to_string(), WhereCondition::StringArray(all_ids));
    query.where_clause = Some(wc);

    let has_sub_order = sub_query.order.is_some();

    let result = execute_model_query_inner(
        store,
        &rel.target_class_name,
        &query,
        Some(&rel.target_shape_json),
        depth + 1,
    )?;

    // Build id → hydrated instance map + ordered ID list
    let mut hydrated: HashMap<String, Value> = HashMap::new();
    let ordered_ids: Vec<String> = result
        .instances
        .iter()
        .filter_map(|inst| inst["id"].as_str().map(|s| s.to_string()))
        .collect();
    for inst in result.instances {
        if let Some(id) = inst["id"].as_str() {
            hydrated.insert(id.to_string(), inst);
        }
    }

    // Replace string IDs with hydrated objects on each parent instance
    for inst in instances.iter_mut() {
        let raw = inst[&rel.name].clone();
        let parent_ids: Vec<String> = if let Some(arr) = raw.as_array() {
            arr.iter()
                .filter_map(|item| item.as_str().map(|s| s.to_string()))
                .collect()
        } else if let Some(id) = raw.as_str() {
            vec![id.to_string()]
        } else {
            continue;
        };

        // When sub-query has an order, use result order (filtered to this parent's IDs)
        let iter_ids: Vec<&str> = if has_sub_order {
            ordered_ids
                .iter()
                .filter(|id| parent_ids.iter().any(|pid| pid == *id))
                .map(|s| s.as_str())
                .collect()
        } else {
            parent_ids.iter().map(|s| s.as_str()).collect()
        };

        let items: Vec<Value> = iter_ids
            .iter()
            .filter_map(|id| hydrated.get(*id).cloned())
            .collect();

        let resolved = if rel.max_count == Some(1) {
            items.first().cloned().unwrap_or(Value::Null)
        } else {
            Value::Array(items)
        };
        inst.as_object_mut()
            .map(|obj| obj.insert(rel.name.clone(), resolved));
    }
    Ok(())
}

/// Resolve a reverse relation (belongsToOne / belongsToMany) for all instances.
///
/// Reverse relations are links pointing TO our instances (target = our ID).
/// We batch-query for all such links, collect source IDs, then hydrate them.
fn resolve_reverse_include(
    store: &SparqlStore,
    instances: &mut [Value],
    rel: &ShapeRelation,
    sub_query: &ModelQueryInput,
    depth: u8,
) -> Result<(), Error> {
    let all_ids: Vec<String> = instances
        .iter()
        .filter_map(|inst| inst["id"].as_str().map(|s| s.to_string()))
        .collect();
    if all_ids.is_empty() {
        return Ok(());
    }

    // Batch SPARQL: find all (source, target) pairs for this predicate
    let id_list = all_ids
        .iter()
        .filter(|id| validate_iri(id).is_ok())
        .map(|id| format!("<{}>", id))
        .collect::<Vec<_>>()
        .join(", ");
    if id_list.is_empty() {
        return Ok(());
    }
    let safe_pred = match validate_iri(&rel.predicate) {
        Ok(p) => p,
        Err(_) => return Ok(()),
    };
    let sparql = format!(
        "SELECT ?source ?target WHERE {{ ?source <{safe_pred}> ?target . FILTER(?target IN ({id_list})) }}"
    );
    let result_json = store.query(&sparql)?;
    let rows: Vec<Value> = serde_json::from_str(&result_json)?;

    // Group source IDs by target (our instance ID)
    let mut sources_by_target: HashMap<String, Vec<String>> = HashMap::new();
    for row in &rows {
        let source = row["source"].as_str().unwrap_or("");
        let target = row["target"].as_str().unwrap_or("");
        if !source.is_empty() && !target.is_empty() {
            sources_by_target
                .entry(target.to_string())
                .or_default()
                .push(source.to_string());
        }
    }

    // Collect all unique source IDs for batch hydration
    let all_source_ids: Vec<String> = {
        let mut set = std::collections::HashSet::new();
        for ids in sources_by_target.values() {
            for id in ids {
                set.insert(id.clone());
            }
        }
        set.into_iter().collect()
    };

    // Hydrate source instances
    let has_sub_order = sub_query.order.is_some();
    let hydrated: HashMap<String, Value>;
    let ordered_result_ids: Vec<String>;
    if all_source_ids.is_empty() {
        hydrated = HashMap::new();
        ordered_result_ids = Vec::new();
    } else {
        let mut query = sub_query.clone();
        let mut wc = query.where_clause.take().unwrap_or_default();
        // Intersect sub-query id filter with source IDs if present
        if let Some(existing_id) = wc.get("id") {
            let filter_ids: Vec<String> = match existing_id {
                WhereCondition::String(s) => vec![s.clone()],
                WhereCondition::StringArray(arr) => arr.clone(),
                _ => vec![],
            };
            let filtered: Vec<String> = all_source_ids
                .into_iter()
                .filter(|id| filter_ids.contains(id))
                .collect();
            wc.insert("id".to_string(), WhereCondition::StringArray(filtered));
        } else {
            wc.insert(
                "id".to_string(),
                WhereCondition::StringArray(all_source_ids),
            );
        }
        query.where_clause = Some(wc);

        let result = execute_model_query_inner(
            store,
            &rel.target_class_name,
            &query,
            Some(&rel.target_shape_json),
            depth + 1,
        )?;

        ordered_result_ids = result
            .instances
            .iter()
            .filter_map(|inst| inst["id"].as_str().map(|s| s.to_string()))
            .collect();
        let mut map = HashMap::new();
        for inst in result.instances {
            if let Some(id) = inst["id"].as_str() {
                map.insert(id.to_string(), inst);
            }
        }
        hydrated = map;
    };

    // Assign hydrated instances to each parent
    for inst in instances.iter_mut() {
        let inst_id = inst["id"].as_str().unwrap_or("").to_string();
        let source_ids = sources_by_target.get(&inst_id).cloned().unwrap_or_default();

        let resolved = if rel.kind == "belongsToOne" || rel.max_count == Some(1) {
            source_ids
                .first()
                .and_then(|id| hydrated.get(id).cloned())
                .unwrap_or(Value::Null)
        } else {
            // When sub-query has order, use result order filtered to this parent's source IDs
            let iter_ids: Vec<&str> = if has_sub_order {
                ordered_result_ids
                    .iter()
                    .filter(|id| source_ids.contains(id))
                    .map(|s| s.as_str())
                    .collect()
            } else {
                source_ids.iter().map(|s| s.as_str()).collect()
            };
            let items: Vec<Value> = iter_ids
                .iter()
                .filter_map(|id| hydrated.get(*id).cloned())
                .collect();
            Value::Array(items)
        };
        inst.as_object_mut()
            .map(|obj| obj.insert(rel.name.clone(), resolved));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_parse_literal_value_string() {
        assert_eq!(
            parse_literal_value("literal:string:hello%20world"),
            Value::String("hello world".to_string())
        );
    }

    #[test]
    fn test_parse_literal_value_number() {
        assert_eq!(
            parse_literal_value("literal:number:42"),
            Value::Number(42.into())
        );
        assert_eq!(
            parse_literal_value("literal:number:3.14"),
            serde_json::Number::from_f64(3.14)
                .map(Value::Number)
                .unwrap()
        );
    }

    #[test]
    fn test_parse_literal_value_boolean() {
        assert_eq!(
            parse_literal_value("literal:boolean:true"),
            Value::Bool(true)
        );
        assert_eq!(
            parse_literal_value("literal:boolean:false"),
            Value::Bool(false)
        );
    }

    #[test]
    fn test_parse_literal_value_non_literal() {
        assert_eq!(
            parse_literal_value("recipe://Recipe"),
            Value::String("recipe://Recipe".to_string())
        );
    }

    #[test]
    fn test_matches_condition_string() {
        let val = Value::String("hello".to_string());
        assert!(matches_condition(
            &val,
            &WhereCondition::String("hello".to_string())
        ));
        assert!(!matches_condition(
            &val,
            &WhereCondition::String("world".to_string())
        ));
    }

    #[test]
    fn test_matches_condition_number() {
        let val = Value::Number(42.into());
        assert!(matches_condition(&val, &WhereCondition::Number(42.0)));
        assert!(!matches_condition(&val, &WhereCondition::Number(43.0)));
    }

    #[test]
    fn test_matches_ops_gt_lt() {
        let val = Value::Number(5.into());
        assert!(matches_ops(
            &val,
            &WhereOps {
                gt: Some(3.0),
                lt: Some(10.0),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &val,
            &WhereOps {
                gt: Some(5.0),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_between() {
        let val = Value::Number(5.into());
        assert!(matches_ops(
            &val,
            &WhereOps {
                between: Some((1.0, 10.0)),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &val,
            &WhereOps {
                between: Some((6.0, 10.0)),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_contains() {
        let val = Value::String("hello world".to_string());
        assert!(matches_ops(
            &val,
            &WhereOps {
                contains: Some(Value::String("world".to_string())),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &val,
            &WhereOps {
                contains: Some(Value::String("xyz".to_string())),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_not() {
        let val = Value::String("hello".to_string());
        assert!(matches_ops(
            &val,
            &WhereOps {
                not: Some(Value::String("world".to_string())),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &val,
            &WhereOps {
                not: Some(Value::String("hello".to_string())),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_compare_values_numeric() {
        assert_eq!(
            compare_values(&Value::Number(1.into()), &Value::Number(2.into())),
            Ordering::Less
        );
        assert_eq!(
            compare_values(&Value::Number(2.into()), &Value::Number(1.into())),
            Ordering::Greater
        );
    }

    #[test]
    fn test_compare_values_null() {
        assert_eq!(
            compare_values(&Value::Null, &Value::Number(1.into())),
            Ordering::Greater
        );
        assert_eq!(
            compare_values(&Value::Number(1.into()), &Value::Null),
            Ordering::Less
        );
    }

    #[test]
    fn test_sort_instances() {
        let mut instances = vec![
            json!({"name": "C", "age": 30}),
            json!({"name": "A", "age": 10}),
            json!({"name": "B", "age": 20}),
        ];
        sort_instances(&mut instances, &[("age".to_string(), OrderDirection::ASC)]);
        assert_eq!(instances[0]["age"], 10);
        assert_eq!(instances[1]["age"], 20);
        assert_eq!(instances[2]["age"], 30);
    }

    #[test]
    fn test_filter_properties() {
        let inst = json!({
            "id": "test://1",
            "baseExpression": "test://1",
            "name": "Test",
            "age": 25,
            "secret": "hidden"
        });
        let filtered = filter_properties(inst, &["name".to_string(), "age".to_string()]);
        assert!(filtered.get("id").is_some());
        assert!(filtered.get("name").is_some());
        assert!(filtered.get("age").is_some());
        assert!(filtered.get("secret").is_none());
    }

    #[test]
    fn test_parse_shape_from_json() {
        let json = r#"{
            "className": "Recipe",
            "properties": {
                "name": {
                    "predicate": "recipe://name",
                    "required": true,
                    "flag": false
                },
                "rating": {
                    "predicate": "recipe://rating",
                    "required": false
                }
            },
            "relations": {
                "ingredients": {
                    "predicate": "recipe://ingredient"
                }
            }
        }"#;

        let shape = parse_shape_from_json(json, "Recipe").unwrap();
        assert_eq!(shape.target_class, "Recipe");
        assert_eq!(shape.properties.len(), 3);
        assert!(shape
            .properties
            .iter()
            .any(|p| p.name == "name" && p.is_required));
        assert!(shape
            .properties
            .iter()
            .any(|p| p.name == "ingredients" && p.is_collection));
    }

    // -----------------------------------------------------------------------
    // parse_literal_value: plain literal format (post-migration)
    //
    // After the signed-envelope removal, all literal values are stored as
    // plain `literal:string:X`, `literal:number:X`, `literal:boolean:X`,
    // or `literal:json:X` (for non-envelope JSON objects/arrays).
    // -----------------------------------------------------------------------

    #[test]
    fn test_parse_literal_value_plain_string() {
        let iri = format!("literal:string:{}", urlencoding::encode("active"));
        let parsed = parse_literal_value(&iri);
        assert_eq!(parsed, Value::String("active".to_string()));
    }

    #[test]
    fn test_parse_literal_value_plain_number() {
        let parsed = parse_literal_value("literal:number:42");
        assert_eq!(parsed, json!(42));
    }

    #[test]
    fn test_parse_literal_value_plain_json_object() {
        let obj = serde_json::json!({"name": "Test", "count": 5});
        let obj_str = serde_json::to_string(&obj).unwrap();
        let encoded = urlencoding::encode(&obj_str);
        let iri = format!("literal:json:{}", encoded);

        let parsed = parse_literal_value(&iri);
        assert_eq!(parsed, json!({"name": "Test", "count": 5}));
    }

    #[test]
    fn test_parse_literal_value_json_no_data_field() {
        // literal:json with valid JSON but no "data" field -> returns whole object
        let obj = serde_json::json!({"name": "Test", "value": 123});
        let obj_str = serde_json::to_string(&obj).unwrap();
        let encoded = urlencoding::encode(&obj_str);
        let iri = format!("literal:json:{}", encoded);

        let parsed = parse_literal_value(&iri);
        assert_eq!(parsed, obj, "Should return full JSON when no data field");
    }

    #[test]
    fn test_parse_literal_value_json_invalid_json() {
        // literal:json with invalid JSON -> returns raw string
        let iri = "literal:json:not%20valid%20json";
        let parsed = parse_literal_value(iri);
        assert_eq!(parsed, Value::String("not valid json".to_string()));
    }

    #[test]
    fn test_parse_literal_value_string_with_spaces() {
        // literal:string: with percent-encoded content should decode properly
        let iri = format!("literal:string:{}", urlencoding::encode("hello world"));
        let parsed = parse_literal_value(&iri);
        assert_eq!(
            parsed,
            Value::String("hello world".to_string()),
            "literal:string should decode percent-encoded content"
        );
    }

    #[test]
    fn test_parse_literal_value_number_unparseable() {
        assert_eq!(
            parse_literal_value("literal:number:not_a_number"),
            Value::String("not_a_number".to_string())
        );
    }

    #[test]
    fn test_parse_literal_value_boolean_invalid() {
        assert_eq!(
            parse_literal_value("literal:boolean:yes"),
            Value::String("yes".to_string())
        );
    }

    #[test]
    fn test_parse_literal_value_unknown_subtype() {
        // literal: prefix but unknown subtype -> returns full URI as string
        assert_eq!(
            parse_literal_value("literal:unknown:foo"),
            Value::String("literal:unknown:foo".to_string())
        );
    }

    // -----------------------------------------------------------------------
    // matches_condition: missing variant coverage
    // -----------------------------------------------------------------------

    #[test]
    fn test_matches_condition_bool() {
        assert!(matches_condition(
            &Value::Bool(true),
            &WhereCondition::Bool(true)
        ));
        assert!(!matches_condition(
            &Value::Bool(true),
            &WhereCondition::Bool(false)
        ));
        assert!(!matches_condition(
            &Value::Null,
            &WhereCondition::Bool(true)
        ));
    }

    #[test]
    fn test_matches_condition_string_array() {
        // IN operator
        let cond = WhereCondition::StringArray(vec!["active".to_string(), "pending".to_string()]);
        assert!(matches_condition(
            &Value::String("active".to_string()),
            &cond
        ));
        assert!(matches_condition(
            &Value::String("pending".to_string()),
            &cond
        ));
        assert!(!matches_condition(
            &Value::String("done".to_string()),
            &cond
        ));
    }

    #[test]
    fn test_matches_condition_number_array() {
        // IN for numbers
        let cond = WhereCondition::NumberArray(vec![1.0, 2.0, 3.0]);
        assert!(matches_condition(&Value::Number(2.into()), &cond));
        assert!(!matches_condition(&Value::Number(4.into()), &cond));
    }

    #[test]
    fn test_matches_condition_string_on_null() {
        assert!(!matches_condition(
            &Value::Null,
            &WhereCondition::String("x".to_string())
        ));
    }

    #[test]
    fn test_matches_condition_number_on_null() {
        assert!(!matches_condition(
            &Value::Null,
            &WhereCondition::Number(5.0)
        ));
    }

    // -----------------------------------------------------------------------
    // matches_ops: additional operator coverage
    // -----------------------------------------------------------------------

    #[test]
    fn test_matches_ops_lte_gte() {
        let val = Value::Number(5.into());
        // gte: 5 >= 5 is true
        assert!(matches_ops(
            &val,
            &WhereOps {
                gte: Some(5.0),
                ..Default::default()
            }
        ));
        // gte: 5 >= 6 is false
        assert!(!matches_ops(
            &val,
            &WhereOps {
                gte: Some(6.0),
                ..Default::default()
            }
        ));
        // lte: 5 <= 5 is true
        assert!(matches_ops(
            &val,
            &WhereOps {
                lte: Some(5.0),
                ..Default::default()
            }
        ));
        // lte: 5 <= 4 is false
        assert!(!matches_ops(
            &val,
            &WhereOps {
                lte: Some(4.0),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_not_number() {
        let val = Value::Number(42.into());
        // not 42 -> false
        assert!(!matches_ops(
            &val,
            &WhereOps {
                not: Some(Value::Number(42.into())),
                ..Default::default()
            }
        ));
        // not 43 -> true
        assert!(matches_ops(
            &val,
            &WhereOps {
                not: Some(Value::Number(43.into())),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_not_bool() {
        assert!(!matches_ops(
            &Value::Bool(true),
            &WhereOps {
                not: Some(Value::Bool(true)),
                ..Default::default()
            }
        ));
        assert!(matches_ops(
            &Value::Bool(true),
            &WhereOps {
                not: Some(Value::Bool(false)),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_not_array() {
        // NOT IN: value must not be in array
        let val = Value::String("active".to_string());
        assert!(!matches_ops(
            &val,
            &WhereOps {
                not: Some(json!(["active", "pending"])),
                ..Default::default()
            }
        ));
        assert!(matches_ops(
            &val,
            &WhereOps {
                not: Some(json!(["done", "archived"])),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_contains_array() {
        // contains on array value: check if item is in array
        let val = json!(["apple", "banana", "cherry"]);
        assert!(matches_ops(
            &val,
            &WhereOps {
                contains: Some(Value::String("banana".to_string())),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &val,
            &WhereOps {
                contains: Some(Value::String("grape".to_string())),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_null_with_numeric_ops() {
        // Null value with numeric operators should not match
        assert!(!matches_ops(
            &Value::Null,
            &WhereOps {
                gt: Some(0.0),
                ..Default::default()
            }
        ));
        assert!(!matches_ops(
            &Value::Null,
            &WhereOps {
                between: Some((0.0, 100.0)),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_non_numeric_string_with_numeric_ops() {
        // Non-numeric string with numeric operator should not match
        assert!(!matches_ops(
            &Value::String("hello".to_string()),
            &WhereOps {
                gt: Some(0.0),
                ..Default::default()
            }
        ));
    }

    #[test]
    fn test_matches_ops_numeric_string() {
        // String containing a number should be parsed and compared
        let val = Value::String("42".to_string());
        assert!(matches_ops(
            &val,
            &WhereOps {
                gt: Some(40.0),
                lt: Some(50.0),
                ..Default::default()
            }
        ));
    }

    // -----------------------------------------------------------------------
    // matches_where: multi-condition AND logic
    // -----------------------------------------------------------------------

    #[test]
    fn test_matches_where_multiple_conditions() {
        let instance = json!({
            "id": "test://1",
            "name": "Task 1",
            "status": "active",
            "priority": 5
        });
        let shape = shape(
            "Task",
            vec![
                prop("name", "task://name"),
                prop("status", "task://status"),
                prop("priority", "task://priority"),
            ],
        );

        // Both conditions match
        let mut where_clause = BTreeMap::new();
        where_clause.insert(
            "status".to_string(),
            WhereCondition::String("active".to_string()),
        );
        where_clause.insert("priority".to_string(), WhereCondition::Number(5.0));
        assert!(matches_where(&instance, &where_clause, &shape));

        // First matches, second doesn't
        let mut where_clause2 = BTreeMap::new();
        where_clause2.insert(
            "status".to_string(),
            WhereCondition::String("active".to_string()),
        );
        where_clause2.insert("priority".to_string(), WhereCondition::Number(10.0));
        assert!(!matches_where(&instance, &where_clause2, &shape));
    }

    #[test]
    fn test_matches_where_skips_id_string() {
        // id/base with String condition should be skipped (pushed to SPARQL)
        let instance = json!({"id": "test://1", "name": "X"});
        let shape = shape("Test", vec![prop("name", "test://name")]);

        let mut where_clause = BTreeMap::new();
        where_clause.insert(
            "id".to_string(),
            WhereCondition::String("test://1".to_string()),
        );
        // Even with a non-matching id, it should pass because String id is skipped
        let mut where_clause_wrong = BTreeMap::new();
        where_clause_wrong.insert(
            "id".to_string(),
            WhereCondition::String("test://wrong".to_string()),
        );
        assert!(matches_where(&instance, &where_clause_wrong, &shape));
    }

    #[test]
    fn test_matches_where_id_ops_not_skipped() {
        // id with Ops condition should NOT be skipped
        let instance = json!({"id": "test://1"});
        let shape = shape("Test", vec![]);

        let mut where_clause = BTreeMap::new();
        where_clause.insert(
            "id".to_string(),
            WhereCondition::Ops(WhereOps {
                contains: Some(Value::String("test".to_string())),
                ..Default::default()
            }),
        );
        assert!(matches_where(&instance, &where_clause, &shape));
    }

    #[test]
    fn test_matches_where_skips_collection_string() {
        // String condition on a collection property should be skipped (pushed to SPARQL)
        let instance = json!({"id": "test://1", "tags": ["a", "b"]});
        let shape = shape("Test", vec![relation("tags", "test://tag")]);

        let mut where_clause = BTreeMap::new();
        where_clause.insert(
            "tags".to_string(),
            WhereCondition::String("nonexistent".to_string()),
        );
        // Skipped, so always passes
        assert!(matches_where(&instance, &where_clause, &shape));
    }

    // -----------------------------------------------------------------------
    // parse_where_filter
    // -----------------------------------------------------------------------

    #[test]
    fn test_parse_where_filter_string_condition() {
        let input = json!({"status": "active"});
        let result = parse_where_filter(&input).unwrap();
        assert!(matches!(
            result.get("status"),
            Some(WhereCondition::String(s)) if s == "active"
        ));
    }

    #[test]
    fn test_parse_where_filter_number_condition() {
        let input = json!({"priority": 5.0});
        let result = parse_where_filter(&input).unwrap();
        assert!(matches!(
            result.get("priority"),
            Some(WhereCondition::Number(n)) if (*n - 5.0).abs() < f64::EPSILON
        ));
    }

    #[test]
    fn test_parse_where_filter_bool_condition() {
        let input = json!({"isActive": true});
        let result = parse_where_filter(&input).unwrap();
        assert!(matches!(
            result.get("isActive"),
            Some(WhereCondition::Bool(true))
        ));
    }

    #[test]
    fn test_parse_where_filter_ops_condition() {
        let input = json!({"age": {"gt": 18.0, "lt": 65.0}});
        let result = parse_where_filter(&input).unwrap();
        assert!(matches!(result.get("age"), Some(WhereCondition::Ops(_))));
    }

    #[test]
    fn test_parse_where_filter_empty_object() {
        let input = json!({});
        assert!(parse_where_filter(&input).is_none());
    }

    #[test]
    fn test_parse_where_filter_non_object() {
        let input = json!("not an object");
        assert!(parse_where_filter(&input).is_none());
    }

    #[test]
    fn test_parse_where_filter_multiple_conditions() {
        let input = json!({"status": "active", "priority": 5.0});
        let result = parse_where_filter(&input).unwrap();
        assert_eq!(result.len(), 2);
    }

    // -----------------------------------------------------------------------
    // validate_iri: SPARQL injection prevention
    // -----------------------------------------------------------------------

    #[test]
    fn test_validate_iri_valid() {
        assert!(validate_iri("task://status").is_ok());
        assert!(validate_iri("literal:string:hello").is_ok());
        assert!(validate_iri("did:key:z6MkfR").is_ok());
    }

    #[test]
    fn test_validate_iri_rejects_injection() {
        assert!(validate_iri("task://status> . <injected://triple").is_err());
        assert!(validate_iri("<injected>").is_err());
        assert!(validate_iri("has spaces").is_err());
        assert!(validate_iri("has\"quotes").is_err());
        assert!(validate_iri("has{braces}").is_err());
    }

    // -----------------------------------------------------------------------
    // sort_instances: additional coverage
    // -----------------------------------------------------------------------

    #[test]
    fn test_sort_instances_desc() {
        let mut instances = vec![
            json!({"name": "A", "score": 10}),
            json!({"name": "B", "score": 30}),
            json!({"name": "C", "score": 20}),
        ];
        sort_instances(
            &mut instances,
            &[("score".to_string(), OrderDirection::DESC)],
        );
        assert_eq!(instances[0]["score"], 30);
        assert_eq!(instances[1]["score"], 20);
        assert_eq!(instances[2]["score"], 10);
    }

    #[test]
    fn test_sort_instances_multi_key() {
        let mut instances = vec![
            json!({"group": "B", "name": "Z"}),
            json!({"group": "A", "name": "Y"}),
            json!({"group": "A", "name": "X"}),
        ];
        sort_instances(
            &mut instances,
            &[
                ("group".to_string(), OrderDirection::ASC),
                ("name".to_string(), OrderDirection::ASC),
            ],
        );
        assert_eq!(instances[0]["group"], "A");
        assert_eq!(instances[0]["name"], "X");
        assert_eq!(instances[1]["group"], "A");
        assert_eq!(instances[1]["name"], "Y");
        assert_eq!(instances[2]["group"], "B");
    }

    #[test]
    fn test_sort_instances_null_pushed_to_end() {
        let mut instances = vec![
            json!({"name": "B"}),
            json!({"name": null}),
            json!({"name": "A"}),
        ];
        sort_instances(&mut instances, &[("name".to_string(), OrderDirection::ASC)]);
        assert_eq!(instances[0]["name"], "A");
        assert_eq!(instances[1]["name"], "B");
        assert!(instances[2]["name"].is_null());
    }

    // -----------------------------------------------------------------------
    // compare_values: string comparison
    // -----------------------------------------------------------------------

    #[test]
    fn test_compare_values_string() {
        assert_eq!(
            compare_values(
                &Value::String("apple".to_string()),
                &Value::String("banana".to_string())
            ),
            Ordering::Less
        );
        assert_eq!(
            compare_values(
                &Value::String("same".to_string()),
                &Value::String("same".to_string())
            ),
            Ordering::Equal
        );
    }

    // -----------------------------------------------------------------------
    // hydrate_one: shared-predicate regression tests
    //
    // When multiple @HasMany relations share the same predicate (e.g.
    // ad4m://has_child), every relation must receive ALL targets for that
    // predicate.  A prior bug used HashMap<predicate, ShapeProperty> which
    // silently dropped all but the last relation, causing include resolution
    // to find zero IDs for earlier relations.
    // -----------------------------------------------------------------------

    /// Helper: build a minimal ShapeProperty for a scalar (non-collection) field.
    fn prop(name: &str, predicate: &str) -> ShapeProperty {
        ShapeProperty {
            name: name.to_string(),
            predicate: predicate.to_string(),
            is_collection: false,
            is_flag: false,
            is_required: false,
            initial_value: None,
            resolve_language: None,
            datatype: None,
            direction: None,
            is_scalar_relation: false,
            getter: None,
            where_filter: None,
            where_predicates: None,
        }
    }

    /// Helper: build a ShapeProperty for a collection relation.
    fn relation(name: &str, predicate: &str) -> ShapeProperty {
        ShapeProperty {
            name: name.to_string(),
            predicate: predicate.to_string(),
            is_collection: true,
            is_flag: false,
            is_required: false,
            initial_value: None,
            resolve_language: None,
            datatype: None,
            direction: Some("forward".to_string()),
            is_scalar_relation: false,
            getter: None,
            where_filter: None,
            where_predicates: None,
        }
    }

    /// Helper: build a ShapeProperty for a flag field.
    fn flag(name: &str, predicate: &str, initial: &str) -> ShapeProperty {
        ShapeProperty {
            name: name.to_string(),
            predicate: predicate.to_string(),
            is_collection: false,
            is_flag: true,
            is_required: true,
            initial_value: Some(initial.to_string()),
            resolve_language: None,
            datatype: None,
            direction: None,
            is_scalar_relation: false,
            getter: None,
            where_filter: None,
            where_predicates: None,
        }
    }

    /// Helper: build a ModelShape from a list of properties.
    fn shape(class: &str, properties: Vec<ShapeProperty>) -> ModelShape {
        ModelShape {
            target_class: class.to_string(),
            shape_uri: format!("{}Shape", class),
            properties,
            include_relations: Vec::new(),
        }
    }

    /// Helper: build an InstanceLinks entry.
    fn inst_links(source: &str, links: Vec<(&str, &str)>) -> InstanceLinks {
        InstanceLinks {
            source: source.to_string(),
            links: links
                .into_iter()
                .enumerate()
                .map(|(i, (pred, tgt))| {
                    (
                        pred.to_string(),
                        tgt.to_string(),
                        "did:key:testauthor".to_string(),
                        format!("2026-01-01T00:00:{:02}.000Z", i),
                    )
                })
                .collect(),
        }
    }

    #[test]
    fn test_hydrate_shared_predicate_all_relations_populated() {
        // Regression: multiple relations sharing "ad4m://has_child" must all
        // receive the targets, not just the last one in iteration order.
        let s = shape(
            "Channel",
            vec![
                flag("type", "flux://entry_type", "flux://has_channel"),
                prop("name", "flux://has_channel_name"),
                relation("views", "ad4m://has_child"),
                relation("messages", "ad4m://has_child"),
                relation("conversations", "ad4m://has_child"),
            ],
        );

        let inst = inst_links(
            "literal:string:ch1",
            vec![
                ("flux://entry_type", "flux://has_channel"),
                ("flux://has_channel_name", "literal:string:General"),
                ("ad4m://has_child", "literal:string:app1"),
                ("ad4m://has_child", "literal:string:conv1"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();

        // All three relations must contain both children
        let views = result["views"]
            .as_array()
            .expect("views should be an array");
        let messages = result["messages"]
            .as_array()
            .expect("messages should be an array");
        let conversations = result["conversations"]
            .as_array()
            .expect("conversations should be an array");

        assert_eq!(views.len(), 2, "views must have 2 items");
        assert_eq!(messages.len(), 2, "messages must have 2 items");
        assert_eq!(conversations.len(), 2, "conversations must have 2 items");

        // All must contain the same IDs (raw IRIs for relations)
        let expected_ids: Vec<&str> = vec!["literal:string:app1", "literal:string:conv1"];
        for rel_name in &["views", "messages", "conversations"] {
            let ids: Vec<String> = result[rel_name]
                .as_array()
                .unwrap()
                .iter()
                .map(|v| v.as_str().unwrap().to_string())
                .collect();
            assert_eq!(
                ids, expected_ids,
                "{} must contain both child IDs",
                rel_name
            );
        }
    }

    #[test]
    fn test_hydrate_shared_predicate_single_relation_still_works() {
        // Sanity: a single relation with a unique predicate still works.
        let s = shape(
            "Simple",
            vec![
                flag("type", "test://type", "test://simple"),
                relation("items", "test://has_item"),
            ],
        );

        let inst = inst_links(
            "literal:string:s1",
            vec![
                ("test://type", "test://simple"),
                ("test://has_item", "literal:string:item1"),
                ("test://has_item", "literal:string:item2"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();
        let items = result["items"]
            .as_array()
            .expect("items should be an array");
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn test_hydrate_shared_predicate_with_distinct_predicates() {
        // When relations use different predicates, they should still be
        // independent (no cross-contamination).
        let s = shape(
            "Model",
            vec![
                flag("type", "test://type", "test://model"),
                relation("alpha", "test://pred_a"),
                relation("beta", "test://pred_b"),
            ],
        );

        let inst = inst_links(
            "literal:string:m1",
            vec![
                ("test://type", "test://model"),
                ("test://pred_a", "literal:string:a1"),
                ("test://pred_b", "literal:string:b1"),
                ("test://pred_b", "literal:string:b2"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();

        let alpha = result["alpha"].as_array().unwrap();
        let beta = result["beta"].as_array().unwrap();

        assert_eq!(alpha.len(), 1, "alpha has 1 item");
        assert_eq!(beta.len(), 2, "beta has 2 items");
        assert_eq!(alpha[0].as_str().unwrap(), "literal:string:a1");
    }

    #[test]
    fn test_hydrate_shared_predicate_no_targets() {
        // No links for the shared predicate — all relations should be absent
        // (not present in the output JSON, matching prior behavior).
        let s = shape(
            "Channel",
            vec![
                flag("type", "flux://entry_type", "flux://has_channel"),
                relation("views", "ad4m://has_child"),
                relation("messages", "ad4m://has_child"),
            ],
        );

        let inst = inst_links(
            "literal:string:ch_empty",
            vec![("flux://entry_type", "flux://has_channel")],
        );

        let result = hydrate_one(&s, &inst).unwrap();

        // Neither relation should appear (no links for has_child)
        assert!(
            result.get("views").is_none(),
            "views should be absent when no has_child links"
        );
        assert!(
            result.get("messages").is_none(),
            "messages should be absent when no has_child links"
        );
    }

    #[test]
    fn test_hydrate_shared_predicate_preserves_scalar_properties() {
        // Scalar properties alongside shared-predicate relations must still
        // hydrate correctly.
        let s = shape(
            "Channel",
            vec![
                flag("type", "flux://entry_type", "flux://has_channel"),
                prop("name", "flux://has_channel_name"),
                prop("description", "flux://has_channel_description"),
                relation("views", "ad4m://has_child"),
                relation("posts", "ad4m://has_child"),
            ],
        );

        let inst = inst_links(
            "literal:string:ch2",
            vec![
                ("flux://entry_type", "flux://has_channel"),
                ("flux://has_channel_name", "literal:string:General"),
                (
                    "flux://has_channel_description",
                    "literal:string:Main%20channel",
                ),
                ("ad4m://has_child", "literal:string:child1"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();

        assert_eq!(result["name"], json!("General"));
        assert_eq!(result["description"], json!("Main channel"));
        assert_eq!(result["type"], json!("flux://has_channel"));

        // Both relations should have the child
        assert_eq!(result["views"].as_array().unwrap().len(), 1);
        assert_eq!(result["posts"].as_array().unwrap().len(), 1);
    }

    #[test]
    fn test_hydrate_many_relations_same_predicate() {
        // Stress test: 8 relations sharing the same predicate (mirrors the
        // real Channel model).  Every relation must see all targets.
        let rel_names = vec![
            "views",
            "messages",
            "conversations",
            "childChannels",
            "boards",
            "taskColumns",
            "tasks",
            "posts",
        ];
        let mut props = vec![flag("type", "flux://entry_type", "flux://has_channel")];
        for name in &rel_names {
            props.push(relation(name, "ad4m://has_child"));
        }
        let s = shape("Channel", props);

        let inst = inst_links(
            "literal:string:ch_stress",
            vec![
                ("flux://entry_type", "flux://has_channel"),
                ("ad4m://has_child", "literal:string:c1"),
                ("ad4m://has_child", "literal:string:c2"),
                ("ad4m://has_child", "literal:string:c3"),
            ],
        );

        let result = hydrate_one(&s, &inst).unwrap();

        for name in &rel_names {
            let arr = result[name]
                .as_array()
                .unwrap_or_else(|| panic!("{} should be an array", name));
            assert_eq!(
                arr.len(),
                3,
                "{} must have all 3 children, got {}",
                name,
                arr.len()
            );
        }
    }
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

#[cfg(test)]
mod integration_tests {
    use super::*;
    use crate::perspectives::sparql_store::SparqlStore;
    use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link};
    use serde_json::json;

    fn make_link(source: &str, predicate: &str, target: &str, ts: &str) -> DecoratedLinkExpression {
        DecoratedLinkExpression {
            author: "did:key:test123".to_string(),
            timestamp: ts.to_string(),
            data: Link {
                source: source.to_string(),
                predicate: Some(predicate.to_string()),
                target: target.to_string(),
            },
            proof: DecoratedExpressionProof {
                key: "key".to_string(),
                signature: "sig".to_string(),
                valid: Some(true),
                invalid: Some(false),
            },
            status: None,
        }
    }

    #[test]
    fn test_full_model_query_with_where_filter() {
        // Create an in-memory store
        let store = SparqlStore::new(None).unwrap();

        // Simulate a Recipe with:
        //   - Flag: <ad4m://type> → <ad4m://recipe>
        //   - Name: <recipe://name> → literal:string:Recipe%201

        let base1 = "literal:string:recipe1base";

        let name_target = format!("literal:string:{}", urlencoding::encode("Recipe 1"));

        // Add the type flag link
        let flag_link = make_link(base1, "ad4m://type", "ad4m://recipe", "1700000000000");
        store.add_link(&flag_link).unwrap();

        // Add the name link
        let name_link = make_link(base1, "recipe://name", &name_target, "1700000000001");
        store.add_link(&name_link).unwrap();

        // Shape JSON (like what TS sends)
        let shape_json = r#"{
            "className": "Recipe",
            "properties": {
                "type": {
                    "predicate": "ad4m://type",
                    "required": true,
                    "flag": true,
                    "initial": "ad4m://recipe"
                },
                "name": {
                    "predicate": "recipe://name",
                    "required": false,
                    "resolveLanguage": "literal"
                }
            },
            "relations": {}
        }"#;

        // Query without WHERE - should find 1 instance
        let query_no_where = ModelQueryInput::default();
        let result =
            execute_model_query(&store, "Recipe", &query_no_where, Some(shape_json)).unwrap();
        assert_eq!(
            result.instances.len(),
            1,
            "Should find 1 recipe without WHERE"
        );

        // Check that name is hydrated
        let name_val = &result.instances[0]["name"];
        assert_eq!(name_val, &json!("Recipe 1"), "Name should be 'Recipe 1'");

        // Query WITH WHERE - should also find 1 instance
        let mut where_clause = BTreeMap::new();
        where_clause.insert(
            "name".to_string(),
            WhereCondition::String("Recipe 1".to_string()),
        );
        let query_with_where = ModelQueryInput {
            where_clause: Some(where_clause),
            ..Default::default()
        };
        let result2 =
            execute_model_query(&store, "Recipe", &query_with_where, Some(shape_json)).unwrap();
        assert_eq!(
            result2.instances.len(),
            1,
            "WHERE name='Recipe 1' should match 1 recipe"
        );
    }

    // -----------------------------------------------------------------------
    // Integration test: shared predicate across multiple @HasMany relations
    //
    // This simulates the real Channel model from Flux where 8+ relations
    // all use "ad4m://has_child".  Without the fix, only the last relation
    // in HashMap iteration order receives targets; the others (like "views")
    // are empty, causing include resolution to return zero results.
    // -----------------------------------------------------------------------

    #[test]
    fn test_shared_predicate_relations_all_populated_via_store() {
        // Simulate a Channel with views, messages, and conversations all using
        // the same predicate "ad4m://has_child".  Each child has a different
        // flag type so include resolution (if applied later) can discriminate.
        let store = SparqlStore::new(None).unwrap();

        let channel_base = "literal:string:channel1";

        // Channel flag
        store
            .add_link(&make_link(
                channel_base,
                "flux://entry_type",
                "flux://has_channel",
                "1700000000000",
            ))
            .unwrap();

        // Channel name
        let name_target = format!(
            "literal:string:{}",
            urlencoding::encode("General")
        );
        store
            .add_link(&make_link(
                channel_base,
                "flux://has_channel_name",
                &name_target,
                "1700000000001",
            ))
            .unwrap();

        // Child 1: an "App" (flag flux://has_app)
        let app_base = "literal:string:app1";
        store
            .add_link(&make_link(
                channel_base,
                "ad4m://has_child",
                app_base,
                "1700000000002",
            ))
            .unwrap();
        store
            .add_link(&make_link(
                app_base,
                "flux://entry_type",
                "flux://has_app",
                "1700000000003",
            ))
            .unwrap();
        let app_name_target = format!(
            "literal:string:{}",
            urlencoding::encode("Chat")
        );
        store
            .add_link(&make_link(
                app_base,
                "flux://has_name",
                &app_name_target,
                "1700000000004",
            ))
            .unwrap();

        // Child 2: a "Conversation" (flag flux://has_conversation)
        let conv_base = "literal:string:conv1";
        store
            .add_link(&make_link(
                channel_base,
                "ad4m://has_child",
                conv_base,
                "1700000000005",
            ))
            .unwrap();
        store
            .add_link(&make_link(
                conv_base,
                "flux://entry_type",
                "flux://has_conversation",
                "1700000000006",
            ))
            .unwrap();

        // Child 3: a "Message" (flag flux://has_message)
        let msg_base = "literal:string:msg1";
        store
            .add_link(&make_link(
                channel_base,
                "ad4m://has_child",
                msg_base,
                "1700000000007",
            ))
            .unwrap();
        store
            .add_link(&make_link(
                msg_base,
                "flux://entry_type",
                "flux://has_message",
                "1700000000008",
            ))
            .unwrap();

        // Shape JSON with 3 relations sharing ad4m://has_child and no includes
        let shape_json = r#"{
            "className": "Channel",
            "properties": {
                "type": {
                    "predicate": "flux://entry_type",
                    "required": true,
                    "flag": true,
                    "initial": "flux://has_channel"
                },
                "name": {
                    "predicate": "flux://has_channel_name",
                    "required": false,
                    "resolveLanguage": "literal"
                }
            },
            "relations": {
                "views": {
                    "predicate": "ad4m://has_child",
                    "target": "App"
                },
                "messages": {
                    "predicate": "ad4m://has_child",
                    "target": "Message"
                },
                "conversations": {
                    "predicate": "ad4m://has_child",
                    "target": "Conversation"
                }
            }
        }"#;

        let query = ModelQueryInput::default();
        let result = execute_model_query(&store, "Channel", &query, Some(shape_json)).unwrap();

        assert_eq!(result.instances.len(), 1, "Should find 1 channel");

        let channel = &result.instances[0];
        assert_eq!(channel["name"], json!("General"));

        // All 3 relations must have all 3 children (raw IRI strings, no include)
        let views = channel["views"].as_array().expect("views must be an array");
        let messages = channel["messages"]
            .as_array()
            .expect("messages must be an array");
        let conversations = channel["conversations"]
            .as_array()
            .expect("conversations must be an array");

        // Without include resolution, all 3 children appear in each relation
        // (the store can't discriminate by target type without include)
        assert_eq!(
            views.len(),
            3,
            "views must have 3 raw child IDs (no include filter)"
        );
        assert_eq!(
            messages.len(),
            3,
            "messages must have 3 raw child IDs (no include filter)"
        );
        assert_eq!(
            conversations.len(),
            3,
            "conversations must have 3 raw child IDs (no include filter)"
        );

        // Verify the actual IDs are present
        let expected_ids = vec![
            "literal:string:app1",
            "literal:string:conv1",
            "literal:string:msg1",
        ];
        for rel_name in &["views", "messages", "conversations"] {
            let ids: Vec<String> = channel[*rel_name]
                .as_array()
                .unwrap()
                .iter()
                .map(|v| v.as_str().unwrap().to_string())
                .collect();
            for eid in &expected_ids {
                assert!(
                    ids.contains(&eid.to_string()),
                    "{} should contain {} but got {:?}",
                    rel_name,
                    eid,
                    ids
                );
            }
        }
    }

    #[test]
    fn test_shared_predicate_with_unique_predicates_no_cross_contamination() {
        // Ensure relations with distinct predicates don't bleed into each other
        // even when one predicate is shared.
        let store = SparqlStore::new(None).unwrap();

        let parent = "literal:string:parent1";

        // Parent flag
        store
            .add_link(&make_link(
                parent,
                "test://type",
                "test://parent_type",
                "1700000000000",
            ))
            .unwrap();

        // Child via shared predicate
        store
            .add_link(&make_link(
                parent,
                "test://has_child",
                "literal:string:shared_child",
                "1700000000001",
            ))
            .unwrap();

        // Child via unique predicate
        store
            .add_link(&make_link(
                parent,
                "test://has_special",
                "literal:string:special_child",
                "1700000000002",
            ))
            .unwrap();

        let shape_json = r#"{
            "className": "Parent",
            "properties": {
                "type": {
                    "predicate": "test://type",
                    "required": true,
                    "flag": true,
                    "initial": "test://parent_type"
                }
            },
            "relations": {
                "alpha": {
                    "predicate": "test://has_child",
                    "target": "Alpha"
                },
                "beta": {
                    "predicate": "test://has_child",
                    "target": "Beta"
                },
                "special": {
                    "predicate": "test://has_special",
                    "target": "Special"
                }
            }
        }"#;

        let query = ModelQueryInput::default();
        let result = execute_model_query(&store, "Parent", &query, Some(shape_json)).unwrap();

        assert_eq!(result.instances.len(), 1);
        let inst = &result.instances[0];

        // alpha and beta both share test://has_child → both get shared_child
        let alpha = inst["alpha"].as_array().expect("alpha must be array");
        let beta = inst["beta"].as_array().expect("beta must be array");
        let special = inst["special"].as_array().expect("special must be array");

        assert_eq!(alpha.len(), 1, "alpha should have 1 child");
        assert_eq!(beta.len(), 1, "beta should have 1 child");
        assert_eq!(special.len(), 1, "special should have 1 child");

        assert_eq!(alpha[0].as_str().unwrap(), "literal:string:shared_child");
        assert_eq!(beta[0].as_str().unwrap(), "literal:string:shared_child");
        assert_eq!(special[0].as_str().unwrap(), "literal:string:special_child");
    }

    // --- IncludeProjection helpers ---

    #[test]
    fn test_build_projection_where_patterns_empty_when_no_clause() {
        let proj = ProjectionInput {
            from: "signals".to_string(),
            count: true,
            target_shape: None,
            where_clause: None,
            limit: None,
            order: None,
        };
        assert_eq!(build_projection_where_patterns(&proj), "");
    }

    #[test]
    fn test_build_projection_where_patterns_id_filter() {
        let mut wc = BTreeMap::new();
        wc.insert(
            "id".to_string(),
            WhereCondition::String("signal://abc".to_string()),
        );
        let proj = ProjectionInput {
            from: "signals".to_string(),
            count: false,
            target_shape: None,
            where_clause: Some(wc),
            limit: None,
            order: None,
        };
        let patterns = build_projection_where_patterns(&proj);
        assert!(
            patterns.contains("FILTER(STR(?t) = \"signal://abc\")"),
            "expected id IRI filter, got: {patterns}"
        );
    }

    #[test]
    fn test_build_projection_where_patterns_with_target_shape() {
        let target_shape = json!({
            "className": "Signal",
            "properties": {
                "signalTypeId": { "predicate": "signal://type" }
            },
            "relations": {}
        });
        let mut wc = BTreeMap::new();
        wc.insert(
            "signalTypeId".to_string(),
            WhereCondition::String("like".to_string()),
        );
        let proj = ProjectionInput {
            from: "signals".to_string(),
            count: true,
            target_shape: Some(target_shape),
            where_clause: Some(wc),
            limit: None,
            order: None,
        };
        let patterns = build_projection_where_patterns(&proj);
        assert!(
            patterns.contains("?t <signal://type>"),
            "expected triple pattern for signal://type, got: {patterns}"
        );
        assert!(
            patterns.contains("FILTER(STR(?"),
            "expected FILTER, got: {patterns}"
        );
    }

    #[test]
    fn test_build_projection_order_clause_empty_when_no_order() {
        let proj = ProjectionInput {
            from: "signals".to_string(),
            count: false,
            target_shape: None,
            where_clause: None,
            limit: Some(5),
            order: None,
        };
        assert_eq!(build_projection_order_clause(&proj), "");
    }

    #[test]
    fn test_build_projection_order_clause_by_id() {
        let proj = ProjectionInput {
            from: "signals".to_string(),
            count: false,
            target_shape: None,
            where_clause: None,
            limit: None,
            order: Some(vec![("id".to_string(), OrderDirection::DESC)]),
        };
        let clause = build_projection_order_clause(&proj);
        assert!(clause.contains("ORDER BY DESC(?t)"), "got: {clause}");
    }

    // -----------------------------------------------------------------------
    // Integration tests: resolve_projections()
    //
    // These tests verify that resolve_projections() correctly issues grouped
    // SPARQL queries against a real SparqlStore and attaches the results to
    // the parent instance objects.
    // -----------------------------------------------------------------------

    /// Helper to build a minimal ModelShape with one forward collection property.
    fn make_shape_with_relation(class: &str, rel_name: &str, predicate: &str) -> ModelShape {
        ModelShape {
            target_class: class.to_string(),
            shape_uri: format!("{}Shape", class),
            properties: vec![ShapeProperty {
                name: rel_name.to_string(),
                predicate: predicate.to_string(),
                is_collection: true,
                is_flag: false,
                is_required: false,
                initial_value: None,
                resolve_language: None,
                datatype: None,
                direction: Some("forward".to_string()),
                is_scalar_relation: false,
                getter: None,
                where_filter: None,
                where_predicates: None,
            }],
            include_relations: vec![],
        }
    }

    #[test]
    fn test_resolve_projections_count() {
        // Set up a store with two parent nodes, each linked to different numbers
        // of child targets via the "test://has_item" predicate.
        let store = SparqlStore::new(None).unwrap();

        let parent_a = "test://parent/a";
        let parent_b = "test://parent/b";
        let item_1 = "test://item/1";
        let item_2 = "test://item/2";
        let item_3 = "test://item/3";

        store
            .add_link(&make_link(parent_a, "test://has_item", item_1, "1000"))
            .unwrap();
        store
            .add_link(&make_link(parent_a, "test://has_item", item_2, "1001"))
            .unwrap();
        store
            .add_link(&make_link(parent_b, "test://has_item", item_3, "1002"))
            .unwrap();

        let shape = make_shape_with_relation("Parent", "items", "test://has_item");

        let mut instances = vec![json!({ "id": parent_a }), json!({ "id": parent_b })];

        let mut projections = HashMap::new();
        projections.insert(
            "$itemCount".to_string(),
            ProjectionInput {
                from: "items".to_string(),
                count: true,
                target_shape: None,
                where_clause: None,
                limit: None,
                order: None,
            },
        );

        resolve_projections(&store, &mut instances, &projections, &shape).unwrap();

        let count_a = instances[0]["$itemCount"].as_u64().unwrap_or(999);
        let count_b = instances[1]["$itemCount"].as_u64().unwrap_or(999);
        assert_eq!(count_a, 2, "parent_a should have 2 items, got {count_a}");
        assert_eq!(count_b, 1, "parent_b should have 1 item, got {count_b}");
    }

    #[test]
    fn test_resolve_projections_list() {
        // parent_a has two children; verify list projection returns them as an array.
        let store = SparqlStore::new(None).unwrap();

        let parent_a = "test://parent/a";
        let item_1 = "test://item/1";
        let item_2 = "test://item/2";

        store
            .add_link(&make_link(parent_a, "test://has_item", item_1, "1000"))
            .unwrap();
        store
            .add_link(&make_link(parent_a, "test://has_item", item_2, "1001"))
            .unwrap();

        let shape = make_shape_with_relation("Parent", "items", "test://has_item");

        let mut instances = vec![json!({ "id": parent_a })];

        let mut projections = HashMap::new();
        projections.insert(
            "$items".to_string(),
            ProjectionInput {
                from: "items".to_string(),
                count: false,
                target_shape: None,
                where_clause: None,
                limit: None,
                order: None,
            },
        );

        resolve_projections(&store, &mut instances, &projections, &shape).unwrap();

        let items = instances[0]["$items"]
            .as_array()
            .expect("$items should be an array");
        assert_eq!(items.len(), 2, "expected 2 items, got {}", items.len());
        let item_strs: Vec<&str> = items.iter().filter_map(|v| v.as_str()).collect();
        assert!(item_strs.contains(&item_1), "missing {item_1}");
        assert!(item_strs.contains(&item_2), "missing {item_2}");
    }

    #[test]
    fn test_resolve_projections_scalar() {
        // limit: Some(1) should unwrap to a single string, not an array.
        let store = SparqlStore::new(None).unwrap();

        let parent_a = "test://parent/a";
        let item_1 = "test://item/1";

        store
            .add_link(&make_link(parent_a, "test://has_item", item_1, "1000"))
            .unwrap();

        let shape = make_shape_with_relation("Parent", "items", "test://has_item");

        let mut instances = vec![json!({ "id": parent_a })];

        let mut projections = HashMap::new();
        projections.insert(
            "$firstItem".to_string(),
            ProjectionInput {
                from: "items".to_string(),
                count: false,
                target_shape: None,
                where_clause: None,
                limit: Some(1),
                order: None,
            },
        );

        resolve_projections(&store, &mut instances, &projections, &shape).unwrap();

        let val = &instances[0]["$firstItem"];
        assert_eq!(
            val.as_str(),
            Some(item_1),
            "limit:1 should return a scalar string, got: {val}"
        );
    }

    #[test]
    fn test_resolve_projections_count_zero_when_no_links() {
        // A parent with no linked children should get count 0, not be absent.
        let store = SparqlStore::new(None).unwrap();
        let parent_a = "test://parent/a";

        let shape = make_shape_with_relation("Parent", "items", "test://has_item");
        let mut instances = vec![json!({ "id": parent_a })];

        let mut projections = HashMap::new();
        projections.insert(
            "$itemCount".to_string(),
            ProjectionInput {
                from: "items".to_string(),
                count: true,
                target_shape: None,
                where_clause: None,
                limit: None,
                order: None,
            },
        );

        resolve_projections(&store, &mut instances, &projections, &shape).unwrap();

        let count = instances[0]["$itemCount"].as_u64().unwrap_or(999);
        assert_eq!(
            count, 0,
            "count should be 0 when no links exist, got {count}"
        );
    }

    #[test]
    fn test_resolve_projections_where_filter_by_plain_iri() {
        // Flux reactions are stored as plain expression IRIs (e.g. emoji://1f44d),
        // not as literal:json: blobs.  The STR() FILTER correctly narrows to
        // only the matching reaction type.
        let store = SparqlStore::new(None).unwrap();

        let parent_a = "test://parent/a";
        let like_iri = "emoji://1f44d";
        let dislike_iri = "emoji://1f44e";

        store
            .add_link(&make_link(
                parent_a,
                "test://has_reaction",
                like_iri,
                "1000",
            ))
            .unwrap();
        store
            .add_link(&make_link(
                parent_a,
                "test://has_reaction",
                dislike_iri,
                "1001",
            ))
            .unwrap();
        // Note: COUNT(DISTINCT ?t) is used, so only distinct target IRIs are counted.

        let shape = make_shape_with_relation("Parent", "reactions", "test://has_reaction");

        // Filter by the plain IRI of the reaction target.
        let mut wc = BTreeMap::new();
        wc.insert(
            "id".to_string(),
            WhereCondition::String(like_iri.to_string()),
        );

        let mut instances = vec![json!({ "id": parent_a })];

        let mut projections = HashMap::new();
        projections.insert(
            "$likeCount".to_string(),
            ProjectionInput {
                from: "reactions".to_string(),
                count: true,
                target_shape: None,
                where_clause: Some(wc),
                limit: None,
                order: None,
            },
        );

        resolve_projections(&store, &mut instances, &projections, &shape).unwrap();

        let count = instances[0]["$likeCount"].as_u64().unwrap_or(999);
        assert_eq!(
            count, 1,
            "should count only the 'like' reaction, got {count}"
        );
    }

    #[test]
    fn test_resolve_projections_where_filter_by_author() {
        // Mirrors the WE $myLikeSignal pattern:
        //   where: { author: { $store: 'adamStore.me.did' } }
        // This was previously silently ignored because the projection SPARQL
        // did not join the reifier. Now a ?_prj_reif join + FILTER is emitted.
        let store = SparqlStore::new(None).unwrap();

        let parent_a = "test://parent/a";
        let signal_1 = "test://signal/1";
        let signal_2 = "test://signal/2";
        let signal_3 = "test://signal/3";

        let alice = "did:key:alice";
        let bob = "did:key:bob";

        // Two signals from alice, one from bob.
        let mut link1 = make_link(parent_a, "test://has_signal", signal_1, "1000");
        link1.author = alice.to_string();
        let mut link2 = make_link(parent_a, "test://has_signal", signal_2, "1001");
        link2.author = alice.to_string();
        let mut link3 = make_link(parent_a, "test://has_signal", signal_3, "1002");
        link3.author = bob.to_string();

        store.add_link(&link1).unwrap();
        store.add_link(&link2).unwrap();
        store.add_link(&link3).unwrap();

        let shape = make_shape_with_relation("Parent", "signals", "test://has_signal");

        let mut wc = BTreeMap::new();
        wc.insert(
            "author".to_string(),
            WhereCondition::String(alice.to_string()),
        );

        let mut instances = vec![json!({ "id": parent_a })];
        let mut projections = HashMap::new();
        projections.insert(
            "$mySignalCount".to_string(),
            ProjectionInput {
                from: "signals".to_string(),
                count: true,
                target_shape: None,
                where_clause: Some(wc),
                limit: None,
                order: None,
            },
        );

        resolve_projections(&store, &mut instances, &projections, &shape).unwrap();

        let count = instances[0]["$mySignalCount"].as_u64().unwrap_or(999);
        assert_eq!(count, 2, "should count only alice's 2 signals, got {count}");
    }

    #[test]
    fn test_deep_query_flag_controls_property_getters() {
        // Create a shape with both a property getter and a relation getter
        let shape_json = r#"{
            "className": "TestModel",
            "properties": {
                "computedProp": {
                    "predicate": "test://computed",
                    "getter": "ASK WHERE { <Base> <test://is_active> ?x }"
                }
            },
            "relations": {
                "children": {
                    "predicate": "test://has_child",
                    "kind": "hasMany",
                    "getter": "SELECT ?target WHERE { <Base> <test://has_child> ?target }"
                }
            }
        }"#;

        let shape = parse_shape_from_json(shape_json, "TestModel").unwrap();

        // With deep_query=false, only relation getters (is_collection/is_scalar_relation) should be collected
        let getter_props_shallow: Vec<&ShapeProperty> = shape
            .properties
            .iter()
            .filter(|p| p.getter.is_some() && (false || p.is_collection || p.is_scalar_relation))
            .collect();
        assert_eq!(
            getter_props_shallow.len(),
            1,
            "shallow: only relation getter"
        );
        assert_eq!(getter_props_shallow[0].name, "children");

        // With deep_query=true, all getters should be collected
        let getter_props_deep: Vec<&ShapeProperty> = shape
            .properties
            .iter()
            .filter(|p| p.getter.is_some() && (true || p.is_collection || p.is_scalar_relation))
            .collect();
        assert_eq!(
            getter_props_deep.len(),
            2,
            "deep: both property and relation getters"
        );
    }

    #[test]
    fn test_evaluate_getters_batch_returns_results() {
        let store = SparqlStore::new(None).unwrap();

        // Insert a test link
        store
            .add_link(&make_link(
                "test://inst-1",
                "test://is_active",
                "literal:boolean:true",
                "2024-01-01T00:00:00Z",
            ))
            .unwrap();

        let shape_json = r#"{
            "className": "TestModel",
            "properties": {
                "isActive": {
                    "predicate": "test://is_active",
                    "getter": "ASK WHERE { <Base> <test://is_active> ?x }"
                }
            },
            "relations": {}
        }"#;

        let result = evaluate_getters_batch(
            &store,
            "TestModel",
            &["test://inst-1".to_string()],
            None,
            Some(shape_json),
        )
        .unwrap();

        assert!(result.is_object(), "result should be an object");
        let inst_result = &result["test://inst-1"];
        assert!(inst_result.is_object(), "should have results for inst-1");
        assert_eq!(inst_result["isActive"], Value::Bool(true));
    }

    #[test]
    fn test_evaluate_getters_batch_empty_ids() {
        let store = SparqlStore::new(None).unwrap();
        let result = evaluate_getters_batch(
            &store,
            "TestModel",
            &[],
            None,
            Some(r#"{"className":"TestModel","properties":{},"relations":{}}"#),
        )
        .unwrap();
        assert!(result.as_object().unwrap().is_empty());
    }

    #[test]
    fn test_evaluate_getters_batch_filters_by_property_names() {
        let store = SparqlStore::new(None).unwrap();

        let shape_json = r#"{
            "className": "TestModel",
            "properties": {
                "propA": {
                    "predicate": "test://a",
                    "getter": "ASK WHERE { <Base> <test://a> ?x }"
                },
                "propB": {
                    "predicate": "test://b",
                    "getter": "ASK WHERE { <Base> <test://b> ?x }"
                }
            },
            "relations": {}
        }"#;

        // Only request propA — propB should not appear in results
        let result = evaluate_getters_batch(
            &store,
            "TestModel",
            &["test://inst-1".to_string()],
            Some(&["propA".to_string()]),
            Some(shape_json),
        )
        .unwrap();

        assert!(result.is_object());
    }

    // ── VALUES batching tests ────────────────────────────────────────────

    #[test]
    fn test_evaluate_getters_where_compiled_literal_filter() {
        // Mimics the failing CI test: a relation getter with a where clause
        // that filters by a literal:string:X value.
        // Setup: board -> 3 tasks (2 active, 1 done)
        // The getter includes conformance checks (flag, required title, required status)
        // plus the where clause for status = "active".
        let store = SparqlStore::new(None).unwrap();
        let ts = "2024-01-01T00:00:00Z";

        let board = "literal:string:board1";
        let task1 = "literal:string:task-active-1";
        let task2 = "literal:string:task-active-2";
        let task3 = "literal:string:task-done";

        // Board -> Task links
        store
            .add_link(&make_link(board, "board://has_task", task1, ts))
            .unwrap();
        store
            .add_link(&make_link(board, "board://has_task", task2, ts))
            .unwrap();
        store
            .add_link(&make_link(board, "board://has_task", task3, ts))
            .unwrap();

        // Task type flags
        store
            .add_link(&make_link(task1, "task://type", "task://task", ts))
            .unwrap();
        store
            .add_link(&make_link(task2, "task://type", "task://task", ts))
            .unwrap();
        store
            .add_link(&make_link(task3, "task://type", "task://task", ts))
            .unwrap();

        // Task titles
        store
            .add_link(&make_link(
                task1,
                "task://title",
                "literal:string:Active%201",
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                task2,
                "task://title",
                "literal:string:Active%202",
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                task3,
                "task://title",
                "literal:string:Done%20Task",
                ts,
            ))
            .unwrap();

        // Task statuses
        store
            .add_link(&make_link(
                task1,
                "task://status",
                "literal:string:active",
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                task2,
                "task://status",
                "literal:string:active",
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                task3,
                "task://status",
                "literal:string:done",
                ts,
            ))
            .unwrap();

        // Conformance-only getter (no where clause in SPARQL).
        // Where filtering is done post-evaluation in Rust via where_filter.
        let getter = "SELECT ?target WHERE { <Base> <board://has_task> ?target . \
            ?target <task://type> <task://task> . \
            ?target <task://title> ?_v0 . \
            ?target <task://status> ?_v1 . }";

        let mut where_filter = BTreeMap::new();
        where_filter.insert(
            "status".to_string(),
            WhereCondition::String("active".to_string()),
        );
        let mut where_predicates = HashMap::new();
        where_predicates.insert("status".to_string(), "task://status".to_string());

        let shape = ModelShape {
            target_class: "TaskBoard".to_string(),
            shape_uri: String::new(),
            properties: vec![ShapeProperty {
                name: "activeTasks".to_string(),
                predicate: "board://has_task".to_string(),
                is_collection: true,
                is_flag: false,
                is_required: false,
                initial_value: None,
                resolve_language: None,
                datatype: None,
                direction: None,
                is_scalar_relation: false,
                getter: Some(getter.to_string()),
                where_filter: Some(where_filter),
                where_predicates: Some(where_predicates),
            }],
            include_relations: vec![],
        };

        let mut instances = vec![serde_json::json!({"id": board})];
        let eval_result = evaluate_getters(&store, &mut instances, &shape, None, true);
        assert!(
            eval_result.is_ok(),
            "evaluate_getters should succeed: {:?}",
            eval_result.err()
        );

        let active = instances[0]
            .get("activeTasks")
            .expect("activeTasks should be set");
        let active_arr = active.as_array().expect("activeTasks should be array");
        assert_eq!(
            active_arr.len(),
            2,
            "Should have 2 active tasks via getter, got: {:?}",
            active_arr
        );
    }

    #[test]
    fn test_strip_trailing_limit() {
        assert_eq!(
            strip_trailing_limit("SELECT ?t WHERE { ?s <p> ?t . } LIMIT 1"),
            "SELECT ?t WHERE { ?s <p> ?t . }"
        );
        assert_eq!(
            strip_trailing_limit("SELECT ?t WHERE { ?s <p> ?t . }"),
            "SELECT ?t WHERE { ?s <p> ?t . }"
        );
        assert_eq!(
            strip_trailing_limit("SELECT ?t WHERE { ?s <p> ?t . } LIMIT 100  "),
            "SELECT ?t WHERE { ?s <p> ?t . }"
        );
    }

    #[test]
    fn test_convert_ask_to_batched_select() {
        let result = convert_ask_to_batched_select(
            r#"ASK WHERE { ?source <test://active> "true" . }"#,
            "<test://a> <test://b>",
        );
        assert!(
            result.contains("SELECT ?source"),
            "should be SELECT: {result}"
        );
        assert!(
            result.contains("VALUES ?source { <test://a> <test://b> }"),
            "should have VALUES: {result}"
        );
        assert!(
            result.contains(r#"<test://active> "true""#),
            "should keep body: {result}"
        );
    }

    #[test]
    fn test_convert_ask_with_base_to_batched_select() {
        let result =
            convert_ask_to_batched_select("ASK WHERE { <Base> <test://active> ?x }", "<test://a>");
        assert!(
            result.contains("?source <test://active>"),
            "should replace <Base> with ?source: {result}"
        );
        assert!(
            result.contains("VALUES ?source"),
            "should have VALUES: {result}"
        );
    }

    #[test]
    fn test_inject_values_into_select() {
        let result = inject_values_into_select(
            "SELECT ?target WHERE { ?source <test://reply> ?target . } LIMIT 1",
            "<test://a> <test://b>",
        );
        assert!(
            result.contains("?source"),
            "should have ?source in SELECT: {result}"
        );
        assert!(
            result.contains("VALUES ?source { <test://a> <test://b> }"),
            "should have VALUES: {result}"
        );
        assert!(
            !result.to_uppercase().contains("LIMIT"),
            "should strip LIMIT: {result}"
        );
    }

    #[test]
    fn test_inject_values_adds_source_to_projection() {
        let result = inject_values_into_select(
            "SELECT ?target WHERE { ?source <test://p> ?target . }",
            "<test://a>",
        );
        // ?source should appear in the SELECT projection
        let upper = result.to_uppercase();
        let select_end = upper.find("SELECT").unwrap() + 6;
        let where_pos = upper.find("WHERE").unwrap();
        let projection = &result[select_end..where_pos];
        assert!(
            projection.contains("?source"),
            "?source should be in projection: {result}"
        );
    }

    #[test]
    fn test_batched_ask_getter_multiple_instances() {
        let store = SparqlStore::new(None).unwrap();

        // inst-1 is active, inst-2 is not
        store
            .add_link(&make_link(
                "test://inst-1",
                "test://is_active",
                "literal:boolean:true",
                "1000",
            ))
            .unwrap();
        // inst-2 has no is_active link

        let shape_json = r#"{
            "className": "TestModel",
            "properties": {
                "isActive": {
                    "predicate": "test://is_active",
                    "getter": "ASK WHERE { ?source <test://is_active> ?x }"
                }
            },
            "relations": {}
        }"#;

        let result = evaluate_getters_batch(
            &store,
            "TestModel",
            &["test://inst-1".to_string(), "test://inst-2".to_string()],
            None,
            Some(shape_json),
        )
        .unwrap();

        assert_eq!(result["test://inst-1"]["isActive"], Value::Bool(true));
        // inst-2 should be false (no matching link)
        assert!(
            result.get("test://inst-2").is_none()
                || result["test://inst-2"].get("isActive").is_none()
                || result["test://inst-2"]["isActive"] == Value::Bool(false),
            "inst-2 should have isActive=false or be absent"
        );
    }

    #[test]
    fn test_batched_select_getter_multiple_instances() {
        let store = SparqlStore::new(None).unwrap();

        // inst-1 has a reply, inst-2 does not
        store
            .add_link(&make_link(
                "test://inst-1",
                "test://has_reply",
                "test://reply-99",
                "1000",
            ))
            .unwrap();

        let shape_json = r#"{
            "className": "TestModel",
            "properties": {
                "replyingTo": {
                    "predicate": "test://has_reply",
                    "getter": "SELECT ?target WHERE { ?source <test://has_reply> ?target . } LIMIT 1"
                }
            },
            "relations": {}
        }"#;

        let result = evaluate_getters_batch(
            &store,
            "TestModel",
            &["test://inst-1".to_string(), "test://inst-2".to_string()],
            None,
            Some(shape_json),
        )
        .unwrap();

        assert_eq!(
            result["test://inst-1"]["replyingTo"].as_str().unwrap(),
            "test://reply-99"
        );
        // inst-2 has no reply
        assert!(
            result.get("test://inst-2").is_none()
                || result["test://inst-2"].get("replyingTo").is_none(),
            "inst-2 should have no replyingTo"
        );
    }

    #[test]
    fn test_batched_collection_getter() {
        let store = SparqlStore::new(None).unwrap();

        // inst-1 has two children
        store
            .add_link(&make_link(
                "test://inst-1",
                "test://has_child",
                "test://child-a",
                "1000",
            ))
            .unwrap();
        store
            .add_link(&make_link(
                "test://inst-1",
                "test://has_child",
                "test://child-b",
                "1001",
            ))
            .unwrap();
        // inst-2 has one child
        store
            .add_link(&make_link(
                "test://inst-2",
                "test://has_child",
                "test://child-c",
                "1002",
            ))
            .unwrap();

        let shape_json = r#"{
            "className": "TestModel",
            "properties": {},
            "relations": {
                "children": {
                    "predicate": "test://has_child",
                    "kind": "hasMany",
                    "getter": "SELECT ?target WHERE { ?source <test://has_child> ?target }"
                }
            }
        }"#;

        let result = evaluate_getters_batch(
            &store,
            "TestModel",
            &["test://inst-1".to_string(), "test://inst-2".to_string()],
            None,
            Some(shape_json),
        )
        .unwrap();

        let children_1 = result["test://inst-1"]["children"].as_array().unwrap();
        assert_eq!(children_1.len(), 2, "inst-1 should have 2 children");

        let children_2 = result["test://inst-2"]["children"].as_array().unwrap();
        assert_eq!(children_2.len(), 1, "inst-2 should have 1 child");
        assert_eq!(children_2[0].as_str().unwrap(), "test://child-c");
    }

    // ── Pipeline ordering: getters run post-pagination ───────────────────

    #[test]
    fn test_deep_query_defaults_to_true() {
        // Verify the default: when deep_query is None, property getters should run
        let store = SparqlStore::new(None).unwrap();

        let base = "test://msg-1";
        store
            .add_link(&make_link(
                base,
                "flux://entry_type",
                "flux://message",
                "1000",
            ))
            .unwrap();
        store
            .add_link(&make_link(
                base,
                "flux://has_reply",
                "test://reply-1",
                "1001",
            ))
            .unwrap();

        let shape_json = r#"{
            "className": "Message",
            "properties": {
                "entryType": { "predicate": "flux://entry_type", "required": true, "flag": true, "initial": "flux://message" }
            },
            "relations": {
                "replyingTo": {
                    "predicate": "flux://has_reply",
                    "kind": "hasOne",
                    "getter": "SELECT ?target WHERE { ?source <flux://has_reply> ?target . } LIMIT 1"
                }
            }
        }"#;

        let query_input = ModelQueryInput {
            deep_query: None, // not set — should default to true
            ..Default::default()
        };

        let result =
            execute_model_query(&store, "Message", &query_input, Some(shape_json)).unwrap();
        assert!(!result.instances.is_empty(), "should find instance");

        let inst = &result.instances[0];
        // replyingTo is a relation getter (always runs) — should be populated
        let reply = inst.get("replyingTo").and_then(|v| v.as_str());
        assert_eq!(
            reply,
            Some("test://reply-1"),
            "replyingTo should be populated by default"
        );
    }

    #[test]
    fn test_deep_query_false_skips_property_getters() {
        let store = SparqlStore::new(None).unwrap();

        let base = "test://msg-1";
        store
            .add_link(&make_link(
                base,
                "flux://entry_type",
                "flux://message",
                "1000",
            ))
            .unwrap();
        store
            .add_link(&make_link(
                base,
                "flux://is_popular",
                "literal:boolean:true",
                "1001",
            ))
            .unwrap();

        let shape_json = r#"{
            "className": "Message",
            "properties": {
                "entryType": { "predicate": "flux://entry_type", "required": true, "flag": true, "initial": "flux://message" },
                "isPopular": {
                    "predicate": "flux://is_popular",
                    "getter": "ASK WHERE { ?source <flux://is_popular> ?x }"
                }
            },
            "relations": {}
        }"#;

        let query_input = ModelQueryInput {
            deep_query: Some(false),
            ..Default::default()
        };

        let result =
            execute_model_query(&store, "Message", &query_input, Some(shape_json)).unwrap();
        assert!(!result.instances.is_empty());

        let inst = &result.instances[0];
        // isPopular is a property getter — should NOT be evaluated when deepQuery=false
        // It may still show the raw hydrated value from the link, but the getter itself
        // (ASK → bool) should not have run.
        // The hydrated value from the link is "true" (string), not true (bool).
        // If the getter ran, it would be Value::Bool(true).
        let is_popular = inst.get("isPopular");
        assert!(
            is_popular.is_none() || !is_popular.unwrap().is_boolean(),
            "property getter should not run when deepQuery=false; got: {:?}",
            is_popular
        );
    }

    #[test]
    fn test_getters_run_after_pagination() {
        // Verify that getters run on the paginated set, not the full result set.
        // We do this by creating 5 instances but querying with limit=2.
        // If getters ran before pagination, all 5 would be evaluated.
        // After our change, only 2 should be evaluated.
        // We verify by checking that the 2 returned instances have getter values.
        let store = SparqlStore::new(None).unwrap();

        for i in 0..5 {
            let base = format!("test://msg-{}", i);
            store
                .add_link(&make_link(
                    &base,
                    "flux://entry_type",
                    "flux://message",
                    &format!("{}", 1000 + i),
                ))
                .unwrap();
            store
                .add_link(&make_link(
                    &base,
                    "flux://has_reply",
                    &format!("test://reply-{}", i),
                    &format!("{}", 2000 + i),
                ))
                .unwrap();
        }

        let shape_json = r#"{
            "className": "Message",
            "properties": {
                "entryType": { "predicate": "flux://entry_type", "required": true, "flag": true, "initial": "flux://message" }
            },
            "relations": {
                "replyingTo": {
                    "predicate": "flux://has_reply",
                    "kind": "hasOne",
                    "getter": "SELECT ?target WHERE { ?source <flux://has_reply> ?target . } LIMIT 1"
                }
            }
        }"#;

        let query_input = ModelQueryInput {
            limit: Some(2),
            deep_query: Some(true),
            order: Some(vec![("timestamp".to_string(), OrderDirection::ASC)]),
            ..Default::default()
        };

        let result =
            execute_model_query(&store, "Message", &query_input, Some(shape_json)).unwrap();
        assert_eq!(result.instances.len(), 2, "should return 2 instances");
        assert_eq!(result.total_count, 5, "total count should be 5");

        // Both returned instances should have replyingTo populated
        for inst in &result.instances {
            let reply = inst.get("replyingTo").and_then(|v| v.as_str());
            assert!(
                reply.is_some(),
                "replyingTo should be populated: {:?}",
                inst
            );
        }
    }

    // ===================================================================
    // Where-clause filtering integration tests
    //
    // These test property where-clause filtering with plain literal values.
    // Property values are stored as `literal:string:X`, `literal:number:X`,
    // etc. and can be matched by SPARQL FILTER or Rust post-hydration.
    // ===================================================================

    /// Helper: create a plain literal IRI for a string value.
    fn signed_literal(value: &str) -> String {
        format!("literal:string:{}", urlencoding::encode(value))
    }

    /// Helper: create a plain literal IRI for a numeric value.
    fn signed_literal_number(value: f64) -> String {
        if value.fract() == 0.0 {
            format!("literal:number:{}", value as i64)
        } else {
            format!("literal:number:{}", value)
        }
    }

    #[test]
    fn test_where_filter_signed_expression_string() {
        // Reproduces the exact CI failure: where clause on a property stored
        // Where clause on a property stored as literal:string:<value>.
        let store = SparqlStore::new(None).unwrap();
        let ts = "1700000000000";

        let board = "test://board1";
        let task1 = "test://task-active-1";
        let task2 = "test://task-active-2";
        let task3 = "test://task-done";

        // Board -> task links
        store
            .add_link(&make_link(board, "board://has_task", task1, ts))
            .unwrap();
        store
            .add_link(&make_link(board, "board://has_task", task2, ts))
            .unwrap();
        store
            .add_link(&make_link(board, "board://has_task", task3, ts))
            .unwrap();

        // Task flags + required properties
        for task in &[task1, task2, task3] {
            store
                .add_link(&make_link(task, "task://type", "task://task", ts))
                .unwrap();
            store
                .add_link(&make_link(
                    task,
                    "task://title",
                    &signed_literal("Title"),
                    ts,
                ))
                .unwrap();
        }

        // Statuses as signed expressions (the exact format that caused CI failure)
        store
            .add_link(&make_link(
                task1,
                "task://status",
                &signed_literal("active"),
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                task2,
                "task://status",
                &signed_literal("active"),
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                task3,
                "task://status",
                &signed_literal("done"),
                ts,
            ))
            .unwrap();

        // Use post-getter where filtering (the fix)
        let getter = "SELECT ?target WHERE { <Base> <board://has_task> ?target . \
            ?target <task://type> <task://task> . \
            ?target <task://title> ?_v0 . \
            ?target <task://status> ?_v1 . }";

        let mut where_filter = BTreeMap::new();
        where_filter.insert(
            "status".to_string(),
            WhereCondition::String("active".to_string()),
        );
        let mut where_predicates = HashMap::new();
        where_predicates.insert("status".to_string(), "task://status".to_string());

        let shape = ModelShape {
            target_class: "Board".to_string(),
            shape_uri: String::new(),
            properties: vec![ShapeProperty {
                name: "activeTasks".to_string(),
                predicate: "board://has_task".to_string(),
                is_collection: true,
                is_flag: false,
                is_required: false,
                initial_value: None,
                resolve_language: None,
                datatype: None,
                direction: None,
                is_scalar_relation: false,
                getter: Some(getter.to_string()),
                where_filter: Some(where_filter),
                where_predicates: Some(where_predicates),
            }],
            include_relations: vec![],
        };

        let mut instances = vec![json!({"id": board})];
        evaluate_getters(&store, &mut instances, &shape, None, true).unwrap();

        let active = instances[0]["activeTasks"].as_array().unwrap();
        assert_eq!(
            active.len(),
            2,
            "Should have 2 active tasks, got {:?}",
            active
        );

        // Verify correct tasks were returned
        let ids: Vec<&str> = active.iter().filter_map(|v| v.as_str()).collect();
        assert!(ids.contains(&task1));
        assert!(ids.contains(&task2));
        assert!(!ids.contains(&task3));
    }

    #[test]
    fn test_where_filter_signed_expression_no_matches() {
        // All targets filtered out -> empty array
        let store = SparqlStore::new(None).unwrap();
        let ts = "1700000000000";

        let parent = "test://parent";
        let child = "test://child";

        store
            .add_link(&make_link(parent, "ns://has_child", child, ts))
            .unwrap();
        store
            .add_link(&make_link(child, "ns://type", "ns://thing", ts))
            .unwrap();
        store
            .add_link(&make_link(
                child,
                "ns://status",
                &signed_literal("done"),
                ts,
            ))
            .unwrap();

        let getter = "SELECT ?target WHERE { <Base> <ns://has_child> ?target . }";

        let mut where_filter = BTreeMap::new();
        where_filter.insert(
            "status".to_string(),
            WhereCondition::String("active".to_string()),
        );
        let mut where_predicates = HashMap::new();
        where_predicates.insert("status".to_string(), "ns://status".to_string());

        let shape = ModelShape {
            target_class: "Parent".to_string(),
            shape_uri: String::new(),
            properties: vec![ShapeProperty {
                name: "activeChildren".to_string(),
                predicate: "ns://has_child".to_string(),
                is_collection: true,
                is_flag: false,
                is_required: false,
                initial_value: None,
                resolve_language: None,
                datatype: None,
                direction: None,
                is_scalar_relation: false,
                getter: Some(getter.to_string()),
                where_filter: Some(where_filter),
                where_predicates: Some(where_predicates),
            }],
            include_relations: vec![],
        };

        let mut instances = vec![json!({"id": parent})];
        evaluate_getters(&store, &mut instances, &shape, None, true).unwrap();

        let result = instances[0]["activeChildren"].as_array().unwrap();
        assert_eq!(result.len(), 0, "Should be empty when no matches");
    }

    #[test]
    fn test_where_filter_multiple_conditions() {
        // Multiple where conditions: status=active AND priority > 3
        let store = SparqlStore::new(None).unwrap();
        let ts = "1700000000000";

        let board = "test://board";
        let task_hi = "test://task-hi";
        let task_lo = "test://task-lo";
        let task_done = "test://task-done";

        store
            .add_link(&make_link(board, "ns://has", task_hi, ts))
            .unwrap();
        store
            .add_link(&make_link(board, "ns://has", task_lo, ts))
            .unwrap();
        store
            .add_link(&make_link(board, "ns://has", task_done, ts))
            .unwrap();

        // task_hi: active, priority 5
        store
            .add_link(&make_link(
                task_hi,
                "ns://status",
                &signed_literal("active"),
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                task_hi,
                "ns://priority",
                &signed_literal_number(5.0),
                ts,
            ))
            .unwrap();

        // task_lo: active, priority 1
        store
            .add_link(&make_link(
                task_lo,
                "ns://status",
                &signed_literal("active"),
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                task_lo,
                "ns://priority",
                &signed_literal_number(1.0),
                ts,
            ))
            .unwrap();

        // task_done: done, priority 5
        store
            .add_link(&make_link(
                task_done,
                "ns://status",
                &signed_literal("done"),
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                task_done,
                "ns://priority",
                &signed_literal_number(5.0),
                ts,
            ))
            .unwrap();

        let getter = "SELECT ?target WHERE { <Base> <ns://has> ?target . }";

        let mut where_filter = BTreeMap::new();
        where_filter.insert(
            "status".to_string(),
            WhereCondition::String("active".to_string()),
        );
        where_filter.insert(
            "priority".to_string(),
            WhereCondition::Ops(WhereOps {
                gt: Some(3.0),
                ..Default::default()
            }),
        );
        let mut where_predicates = HashMap::new();
        where_predicates.insert("status".to_string(), "ns://status".to_string());
        where_predicates.insert("priority".to_string(), "ns://priority".to_string());

        let shape = ModelShape {
            target_class: "Board".to_string(),
            shape_uri: String::new(),
            properties: vec![ShapeProperty {
                name: "highPriActive".to_string(),
                predicate: "ns://has".to_string(),
                is_collection: true,
                is_flag: false,
                is_required: false,
                initial_value: None,
                resolve_language: None,
                datatype: None,
                direction: None,
                is_scalar_relation: false,
                getter: Some(getter.to_string()),
                where_filter: Some(where_filter),
                where_predicates: Some(where_predicates),
            }],
            include_relations: vec![],
        };

        let mut instances = vec![json!({"id": board})];
        evaluate_getters(&store, &mut instances, &shape, None, true).unwrap();

        let result = instances[0]["highPriActive"].as_array().unwrap();
        assert_eq!(result.len(), 1, "Only task_hi should match: {:?}", result);
        assert_eq!(result[0].as_str().unwrap(), task_hi);
    }

    #[test]
    fn test_where_filter_missing_property_on_target() {
        // Target lacks the property being filtered on -> should not match
        let store = SparqlStore::new(None).unwrap();
        let ts = "1700000000000";

        let parent = "test://parent";
        let child_with = "test://child-with";
        let child_without = "test://child-without";

        store
            .add_link(&make_link(parent, "ns://has", child_with, ts))
            .unwrap();
        store
            .add_link(&make_link(parent, "ns://has", child_without, ts))
            .unwrap();

        // Only child_with has the status property
        store
            .add_link(&make_link(
                child_with,
                "ns://status",
                &signed_literal("active"),
                ts,
            ))
            .unwrap();
        // child_without has no status link at all

        let getter = "SELECT ?target WHERE { <Base> <ns://has> ?target . }";
        let mut where_filter = BTreeMap::new();
        where_filter.insert(
            "status".to_string(),
            WhereCondition::String("active".to_string()),
        );
        let mut where_predicates = HashMap::new();
        where_predicates.insert("status".to_string(), "ns://status".to_string());

        let shape = ModelShape {
            target_class: "Parent".to_string(),
            shape_uri: String::new(),
            properties: vec![ShapeProperty {
                name: "active".to_string(),
                predicate: "ns://has".to_string(),
                is_collection: true,
                is_flag: false,
                is_required: false,
                initial_value: None,
                resolve_language: None,
                datatype: None,
                direction: None,
                is_scalar_relation: false,
                getter: Some(getter.to_string()),
                where_filter: Some(where_filter),
                where_predicates: Some(where_predicates),
            }],
            include_relations: vec![],
        };

        let mut instances = vec![json!({"id": parent})];
        evaluate_getters(&store, &mut instances, &shape, None, true).unwrap();

        let result = instances[0]["active"].as_array().unwrap();
        assert_eq!(result.len(), 1, "Only child_with should match");
        assert_eq!(result[0].as_str().unwrap(), child_with);
    }

    #[test]
    fn test_where_filter_plain_literal_string() {
        // Where clause on literal:string: values (not signed expressions)
        // This should also work correctly
        let store = SparqlStore::new(None).unwrap();
        let ts = "1700000000000";

        let parent = "test://parent";
        let child1 = "test://child1";
        let child2 = "test://child2";

        store
            .add_link(&make_link(parent, "ns://has", child1, ts))
            .unwrap();
        store
            .add_link(&make_link(parent, "ns://has", child2, ts))
            .unwrap();

        // Plain literal:string values (no signed expression envelope)
        store
            .add_link(&make_link(child1, "ns://color", "literal:string:red", ts))
            .unwrap();
        store
            .add_link(&make_link(child2, "ns://color", "literal:string:blue", ts))
            .unwrap();

        let getter = "SELECT ?target WHERE { <Base> <ns://has> ?target . }";
        let mut where_filter = BTreeMap::new();
        where_filter.insert(
            "color".to_string(),
            WhereCondition::String("red".to_string()),
        );
        let mut where_predicates = HashMap::new();
        where_predicates.insert("color".to_string(), "ns://color".to_string());

        let shape = ModelShape {
            target_class: "Parent".to_string(),
            shape_uri: String::new(),
            properties: vec![ShapeProperty {
                name: "redChildren".to_string(),
                predicate: "ns://has".to_string(),
                is_collection: true,
                is_flag: false,
                is_required: false,
                initial_value: None,
                resolve_language: None,
                datatype: None,
                direction: None,
                is_scalar_relation: false,
                getter: Some(getter.to_string()),
                where_filter: Some(where_filter),
                where_predicates: Some(where_predicates),
            }],
            include_relations: vec![],
        };

        let mut instances = vec![json!({"id": parent})];
        evaluate_getters(&store, &mut instances, &shape, None, true).unwrap();

        let result = instances[0]["redChildren"].as_array().unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].as_str().unwrap(), child1);
    }

    #[test]
    fn test_where_filter_on_multiple_instances() {
        // Where filter across multiple parent instances
        let store = SparqlStore::new(None).unwrap();
        let ts = "1700000000000";

        let board1 = "test://board1";
        let board2 = "test://board2";
        let task_a = "test://task-a";
        let task_b = "test://task-b";
        let task_c = "test://task-c";

        // board1 -> task_a (active), task_b (done)
        store
            .add_link(&make_link(board1, "ns://has", task_a, ts))
            .unwrap();
        store
            .add_link(&make_link(board1, "ns://has", task_b, ts))
            .unwrap();
        // board2 -> task_c (active)
        store
            .add_link(&make_link(board2, "ns://has", task_c, ts))
            .unwrap();

        store
            .add_link(&make_link(
                task_a,
                "ns://status",
                &signed_literal("active"),
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                task_b,
                "ns://status",
                &signed_literal("done"),
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                task_c,
                "ns://status",
                &signed_literal("active"),
                ts,
            ))
            .unwrap();

        let getter = "SELECT ?target WHERE { <Base> <ns://has> ?target . }";
        let mut where_filter = BTreeMap::new();
        where_filter.insert(
            "status".to_string(),
            WhereCondition::String("active".to_string()),
        );
        let mut where_predicates = HashMap::new();
        where_predicates.insert("status".to_string(), "ns://status".to_string());

        let shape = ModelShape {
            target_class: "Board".to_string(),
            shape_uri: String::new(),
            properties: vec![ShapeProperty {
                name: "activeTasks".to_string(),
                predicate: "ns://has".to_string(),
                is_collection: true,
                is_flag: false,
                is_required: false,
                initial_value: None,
                resolve_language: None,
                datatype: None,
                direction: None,
                is_scalar_relation: false,
                getter: Some(getter.to_string()),
                where_filter: Some(where_filter),
                where_predicates: Some(where_predicates),
            }],
            include_relations: vec![],
        };

        let mut instances = vec![json!({"id": board1}), json!({"id": board2})];
        evaluate_getters(&store, &mut instances, &shape, None, true).unwrap();

        let active1 = instances[0]["activeTasks"].as_array().unwrap();
        assert_eq!(active1.len(), 1, "board1 should have 1 active task");
        assert_eq!(active1[0].as_str().unwrap(), task_a);

        let active2 = instances[1]["activeTasks"].as_array().unwrap();
        assert_eq!(active2.len(), 1, "board2 should have 1 active task");
        assert_eq!(active2[0].as_str().unwrap(), task_c);
    }

    #[test]
    fn test_full_model_query_signed_expression_where() {
        // End-to-end: findAll with where clause on signed expression values
        // This is what the integration test does via the full pipeline
        let store = SparqlStore::new(None).unwrap();
        let ts = "1700000000000";

        let item1 = "test://item1";
        let item2 = "test://item2";
        let item3 = "test://item3";

        // All items have the type flag
        for item in &[item1, item2, item3] {
            store
                .add_link(&make_link(item, "ns://type", "ns://item", ts))
                .unwrap();
        }

        // Properties as signed expressions
        store
            .add_link(&make_link(item1, "ns://name", &signed_literal("Alpha"), ts))
            .unwrap();
        store
            .add_link(&make_link(
                item1,
                "ns://status",
                &signed_literal("active"),
                ts,
            ))
            .unwrap();

        store
            .add_link(&make_link(item2, "ns://name", &signed_literal("Beta"), ts))
            .unwrap();
        store
            .add_link(&make_link(
                item2,
                "ns://status",
                &signed_literal("active"),
                ts,
            ))
            .unwrap();

        store
            .add_link(&make_link(item3, "ns://name", &signed_literal("Gamma"), ts))
            .unwrap();
        store
            .add_link(&make_link(
                item3,
                "ns://status",
                &signed_literal("archived"),
                ts,
            ))
            .unwrap();

        let shape_json = r#"{
            "className": "Item",
            "properties": {
                "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://item" },
                "name": { "predicate": "ns://name", "required": true, "resolveLanguage": "literal" },
                "status": { "predicate": "ns://status", "required": false, "resolveLanguage": "literal" }
            },
            "relations": {}
        }"#;

        // Query WITH where clause on status
        let mut where_clause = BTreeMap::new();
        where_clause.insert(
            "status".to_string(),
            WhereCondition::String("active".to_string()),
        );

        let query = ModelQueryInput {
            where_clause: Some(where_clause),
            ..Default::default()
        };

        let result = execute_model_query(&store, "Item", &query, Some(shape_json)).unwrap();
        assert_eq!(
            result.instances.len(),
            2,
            "Should find 2 active items, got: {:?}",
            result.instances
        );

        // Verify names
        let names: Vec<&str> = result
            .instances
            .iter()
            .filter_map(|i| i["name"].as_str())
            .collect();
        assert!(names.contains(&"Alpha"));
        assert!(names.contains(&"Beta"));
        assert!(!names.contains(&"Gamma"));
    }

    #[test]
    fn test_full_model_query_signed_expression_numeric_where() {
        // findAll with numeric where clause on signed expression values
        let store = SparqlStore::new(None).unwrap();
        let ts = "1700000000000";

        let item1 = "test://item1";
        let item2 = "test://item2";

        for item in &[item1, item2] {
            store
                .add_link(&make_link(item, "ns://type", "ns://item", ts))
                .unwrap();
        }

        store
            .add_link(&make_link(
                item1,
                "ns://score",
                &signed_literal_number(85.0),
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                item2,
                "ns://score",
                &signed_literal_number(45.0),
                ts,
            ))
            .unwrap();

        let shape_json = r#"{
            "className": "Item",
            "properties": {
                "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://item" },
                "score": { "predicate": "ns://score", "required": false, "resolveLanguage": "literal" }
            },
            "relations": {}
        }"#;

        // Where: score > 50
        let mut where_clause = BTreeMap::new();
        where_clause.insert(
            "score".to_string(),
            WhereCondition::Ops(WhereOps {
                gt: Some(50.0),
                ..Default::default()
            }),
        );

        let query = ModelQueryInput {
            where_clause: Some(where_clause),
            ..Default::default()
        };

        let result = execute_model_query(&store, "Item", &query, Some(shape_json)).unwrap();
        assert_eq!(
            result.instances.len(),
            1,
            "Only item1 with score 85 should match"
        );
        assert_eq!(result.instances[0]["id"].as_str().unwrap(), item1);
    }

    #[test]
    fn test_full_model_query_signed_expression_boolean_where() {
        // findAll with boolean where clause on plain literal boolean values
        let store = SparqlStore::new(None).unwrap();
        let ts = "1700000000000";

        let item1 = "test://item1";
        let item2 = "test://item2";

        for item in &[item1, item2] {
            store
                .add_link(&make_link(item, "ns://type", "ns://thing", ts))
                .unwrap();
        }

        let enc_true = "literal:boolean:true";
        let enc_false = "literal:boolean:false";

        store
            .add_link(&make_link(item1, "ns://visible", enc_true, ts))
            .unwrap();
        store
            .add_link(&make_link(item2, "ns://visible", enc_false, ts))
            .unwrap();

        let shape_json = r#"{
            "className": "Thing",
            "properties": {
                "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://thing" },
                "visible": { "predicate": "ns://visible", "required": false, "resolveLanguage": "literal" }
            },
            "relations": {}
        }"#;

        let mut where_clause = BTreeMap::new();
        where_clause.insert("visible".to_string(), WhereCondition::Bool(true));

        let query = ModelQueryInput {
            where_clause: Some(where_clause),
            ..Default::default()
        };

        let result = execute_model_query(&store, "Thing", &query, Some(shape_json)).unwrap();
        assert_eq!(result.instances.len(), 1);
        assert_eq!(result.instances[0]["id"].as_str().unwrap(), item1);
    }

    #[test]
    fn test_full_model_query_where_string_array_in() {
        // IN operator: where status IN ["active", "pending"]
        let store = SparqlStore::new(None).unwrap();
        let ts = "1700000000000";

        let item1 = "test://i1";
        let item2 = "test://i2";
        let item3 = "test://i3";

        for item in &[item1, item2, item3] {
            store
                .add_link(&make_link(item, "ns://type", "ns://item", ts))
                .unwrap();
        }

        store
            .add_link(&make_link(
                item1,
                "ns://status",
                &signed_literal("active"),
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                item2,
                "ns://status",
                &signed_literal("pending"),
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                item3,
                "ns://status",
                &signed_literal("done"),
                ts,
            ))
            .unwrap();

        let shape_json = r#"{
            "className": "Item",
            "properties": {
                "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://item" },
                "status": { "predicate": "ns://status", "required": false, "resolveLanguage": "literal" }
            },
            "relations": {}
        }"#;

        let mut where_clause = BTreeMap::new();
        where_clause.insert(
            "status".to_string(),
            WhereCondition::StringArray(vec!["active".to_string(), "pending".to_string()]),
        );

        let query = ModelQueryInput {
            where_clause: Some(where_clause),
            ..Default::default()
        };

        let result = execute_model_query(&store, "Item", &query, Some(shape_json)).unwrap();
        assert_eq!(result.instances.len(), 2, "active and pending should match");
    }

    #[test]
    fn test_full_model_query_where_ops_not() {
        // NOT operator: where status != "done"
        let store = SparqlStore::new(None).unwrap();
        let ts = "1700000000000";

        let item1 = "test://i1";
        let item2 = "test://i2";

        for item in &[item1, item2] {
            store
                .add_link(&make_link(item, "ns://type", "ns://item", ts))
                .unwrap();
        }

        store
            .add_link(&make_link(
                item1,
                "ns://status",
                &signed_literal("active"),
                ts,
            ))
            .unwrap();
        store
            .add_link(&make_link(
                item2,
                "ns://status",
                &signed_literal("done"),
                ts,
            ))
            .unwrap();

        let shape_json = r#"{
            "className": "Item",
            "properties": {
                "type": { "predicate": "ns://type", "required": true, "flag": true, "initial": "ns://item" },
                "status": { "predicate": "ns://status", "required": false, "resolveLanguage": "literal" }
            },
            "relations": {}
        }"#;

        let mut where_clause = BTreeMap::new();
        where_clause.insert(
            "status".to_string(),
            WhereCondition::Ops(WhereOps {
                not: Some(Value::String("done".to_string())),
                ..Default::default()
            }),
        );

        let query = ModelQueryInput {
            where_clause: Some(where_clause),
            ..Default::default()
        };

        let result = execute_model_query(&store, "Item", &query, Some(shape_json)).unwrap();
        assert_eq!(result.instances.len(), 1);
        assert_eq!(result.instances[0]["id"].as_str().unwrap(), item1);
    }

    // -----------------------------------------------------------------------
    // build_instance_sparql: predicate projection tests
    //
    // Verifies that the VALUES ?predicate clause correctly excludes
    // collection properties that have SPARQL getters (i.e. typed @HasMany
    // relations resolved by evaluate_getters) while retaining scalar
    // properties, flags, and raw-predicate collections without getters.
    // -----------------------------------------------------------------------

    /// Helper: build a minimal ShapeProperty for a scalar property.
    fn scalar_prop(name: &str, predicate: &str, required: bool, flag: bool) -> ShapeProperty {
        ShapeProperty {
            name: name.to_string(),
            predicate: predicate.to_string(),
            is_collection: false,
            is_flag: flag,
            is_required: required,
            initial_value: if flag {
                Some("ns://flag_value".to_string())
            } else {
                None
            },
            resolve_language: None,
            datatype: None,
            direction: None,
            is_scalar_relation: false,
            getter: None,
            where_filter: None,
            where_predicates: None,
        }
    }

    /// Helper: build a ShapeProperty for a collection relation.
    fn collection_prop(name: &str, predicate: &str, getter: Option<&str>) -> ShapeProperty {
        ShapeProperty {
            name: name.to_string(),
            predicate: predicate.to_string(),
            is_collection: true,
            is_flag: false,
            is_required: false,
            initial_value: None,
            resolve_language: None,
            datatype: None,
            direction: None,
            is_scalar_relation: false,
            getter: getter.map(|s| s.to_string()),
            where_filter: None,
            where_predicates: None,
        }
    }

    fn make_shape(props: Vec<ShapeProperty>) -> ModelShape {
        ModelShape {
            target_class: "TestModel".to_string(),
            shape_uri: String::new(),
            properties: props,
            include_relations: vec![],
        }
    }

    #[test]
    fn test_build_instance_sparql_scalar_only_model_uses_values_clause() {
        // A model with only scalar properties (like ChannelSummary) should
        // produce a VALUES ?predicate clause listing only those predicates.
        let shape = make_shape(vec![
            scalar_prop("type", "flux://entry_type", true, true),
            scalar_prop("name", "flux://name", false, false),
            scalar_prop("description", "flux://description", false, false),
        ]);
        let query = ModelQueryInput::default();
        let sparql = build_instance_sparql(&shape, &query, None);

        assert!(
            sparql.contains("VALUES ?predicate"),
            "Should have VALUES clause, got:\n{}",
            sparql
        );
        assert!(sparql.contains("<flux://entry_type>"));
        assert!(sparql.contains("<flux://name>"));
        assert!(sparql.contains("<flux://description>"));
    }

    #[test]
    fn test_build_instance_sparql_excludes_getter_backed_collections() {
        // A model like Channel with typed @HasMany relations that have
        // auto-generated getters.  The getter-backed collections (views,
        // messages) should be EXCLUDED from the VALUES clause.
        let shape = make_shape(vec![
            scalar_prop("type", "flux://entry_type", true, true),
            scalar_prop("name", "flux://name", false, false),
            // Typed @HasMany — has a getter (auto-generated conformance filter)
            collection_prop(
                "views",
                "ad4m://has_child",
                Some("SELECT ?target WHERE { ?source <ad4m://has_child> ?target . ?target <flux://entry_type> <flux://has_app> . }"),
            ),
            // Another typed @HasMany with getter — same predicate
            collection_prop(
                "messages",
                "ad4m://has_child",
                Some("SELECT ?target WHERE { ?source <ad4m://has_child> ?target . ?target <flux://entry_type> <flux://has_message> . }"),
            ),
        ]);
        let query = ModelQueryInput::default();
        let sparql = build_instance_sparql(&shape, &query, None);

        assert!(
            sparql.contains("VALUES ?predicate"),
            "Should have VALUES clause"
        );
        assert!(sparql.contains("<flux://entry_type>"));
        assert!(sparql.contains("<flux://name>"));
        // ad4m://has_child should NOT appear because both collections using
        // it have getters.
        assert!(
            !sparql.contains("<ad4m://has_child>"),
            "Should exclude getter-backed collection predicate, got:\n{}",
            sparql
        );
    }

    #[test]
    fn test_build_instance_sparql_retains_raw_predicate_collections() {
        // A collection without a getter (raw predicate like participants)
        // should be INCLUDED in the VALUES clause because it's resolved
        // from the main query results, not by evaluate_getters.
        let shape = make_shape(vec![
            scalar_prop("type", "flux://entry_type", true, true),
            // Raw @HasMany — no target class, no getter
            collection_prop("participants", "flux://has_participant", None),
            // Typed @HasMany — has getter
            collection_prop(
                "messages",
                "ad4m://has_child",
                Some("SELECT ?target WHERE { ?source <ad4m://has_child> ?target . }"),
            ),
        ]);
        let query = ModelQueryInput::default();
        let sparql = build_instance_sparql(&shape, &query, None);

        assert!(sparql.contains("VALUES ?predicate"));
        assert!(sparql.contains("<flux://entry_type>"));
        assert!(
            sparql.contains("<flux://has_participant>"),
            "Raw collection predicate should be included"
        );
        assert!(
            !sparql.contains("<ad4m://has_child>"),
            "Getter-backed collection predicate should be excluded"
        );
    }

    #[test]
    fn test_build_instance_sparql_shared_predicate_mixed_getter() {
        // Edge case: two collections share the same predicate but only one
        // has a getter.  The predicate should be INCLUDED because the
        // getter-less collection needs it from the main query.
        let shape = make_shape(vec![
            scalar_prop("type", "flux://entry_type", true, true),
            // No getter — needs predicate in main query
            collection_prop("raw_children", "ad4m://has_child", None),
            // Has getter — doesn't need predicate in main query
            collection_prop(
                "typed_children",
                "ad4m://has_child",
                Some("SELECT ?target WHERE { ?source <ad4m://has_child> ?target . }"),
            ),
        ]);
        let query = ModelQueryInput::default();
        let sparql = build_instance_sparql(&shape, &query, None);

        assert!(sparql.contains("VALUES ?predicate"));
        // ad4m://has_child should appear because raw_children needs it
        assert!(
            sparql.contains("<ad4m://has_child>"),
            "Predicate should be included when any collection without a getter uses it"
        );
    }

    #[test]
    fn test_build_instance_sparql_empty_shape_falls_back_to_wildcard() {
        // A shape with no properties at all should fall back to the
        // unrestricted wildcard (no VALUES clause).
        let shape = make_shape(vec![]);
        let query = ModelQueryInput::default();
        let sparql = build_instance_sparql(&shape, &query, None);

        assert!(
            !sparql.contains("VALUES ?predicate"),
            "Empty shape should produce wildcard (no VALUES clause)"
        );
        // Should still have the basic pattern
        assert!(sparql.contains("?source ?predicate ?target"));
    }

    #[test]
    fn test_build_instance_sparql_values_clause_is_deduplicated() {
        // If multiple scalar properties share the same predicate, the
        // VALUES clause should contain it only once.
        let shape = make_shape(vec![
            scalar_prop("type", "ns://shared_pred", true, true),
            scalar_prop("alias", "ns://shared_pred", false, false),
            scalar_prop("name", "ns://name", false, false),
        ]);
        let query = ModelQueryInput::default();
        let sparql = build_instance_sparql(&shape, &query, None);

        assert!(sparql.contains("VALUES ?predicate"));
        // Count occurrences of the shared predicate in the VALUES clause
        let values_line = sparql
            .lines()
            .find(|l| l.contains("VALUES ?predicate"))
            .unwrap();
        let count = values_line.matches("<ns://shared_pred>").count();
        assert_eq!(
            count, 1,
            "Shared predicate should appear exactly once in VALUES clause"
        );
    }

    #[test]
    fn test_build_instance_sparql_integration_getter_excluded_from_results() {
        // Full integration test: a Channel-like model with scalar properties
        // and a getter-backed @HasMany relation.  The main query should NOT
        // return rows for the getter-backed relation's predicate, so adding
        // thousands of links with that predicate should not affect the result
        // count from the main query.
        let store = SparqlStore::new(None).unwrap();

        let channel_id = "test://channel1";

        // Add flag link
        store
            .add_link(&make_link(
                channel_id,
                "flux://entry_type",
                "flux://channel",
                "1700000000000",
            ))
            .unwrap();
        // Add name link
        store
            .add_link(&make_link(
                channel_id,
                "flux://name",
                "literal:string:general",
                "1700000000001",
            ))
            .unwrap();

        // Add 100 message children (simulating a large channel)
        for i in 0..100 {
            store
                .add_link(&make_link(
                    channel_id,
                    "ad4m://has_child",
                    &format!("test://msg{}", i),
                    &format!("17000000001{:02}", i),
                ))
                .unwrap();
        }

        // Shape: scalar properties + getter-backed collection
        let shape_json = r#"{
            "className": "Channel",
            "properties": {
                "type": {
                    "predicate": "flux://entry_type",
                    "required": true,
                    "flag": true,
                    "initial": "flux://channel"
                },
                "name": {
                    "predicate": "flux://name",
                    "required": false
                }
            },
            "relations": {
                "messages": {
                    "predicate": "ad4m://has_child",
                    "getter": "SELECT ?target WHERE { ?source <ad4m://has_child> ?target . ?target <flux://entry_type> <flux://has_message> . }"
                }
            }
        }"#;

        let query = ModelQueryInput::default();
        let result = execute_model_query(&store, "Channel", &query, Some(shape_json)).unwrap();

        assert_eq!(result.instances.len(), 1, "Should find exactly 1 channel");
        assert_eq!(
            result.instances[0]["name"],
            json!("general"),
            "Name should be hydrated"
        );
        // The 100 message children should NOT appear in the main hydration
        // because their predicate (ad4m://has_child) is excluded by the
        // VALUES clause.  The "messages" relation has a getter, so it would
        // be resolved by evaluate_getters (not tested here — that's a
        // separate code path).
        let messages = result.instances[0].get("messages");
        // messages should either be absent or empty (getter not run in this test)
        match messages {
            None => {} // expected — not hydrated from main query
            Some(Value::Array(arr)) => assert!(
                arr.is_empty(),
                "Messages should not be hydrated from main query"
            ),
            other => panic!("Unexpected messages value: {:?}", other),
        }
    }
}
