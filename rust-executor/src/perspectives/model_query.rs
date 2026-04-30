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
    #[serde(default, rename = "where")]
    pub where_clause: Option<HashMap<String, WhereCondition>>,
    #[serde(default, deserialize_with = "deserialize_order_flex")]
    pub order: Option<Vec<(String, OrderDirection)>>,
    #[serde(default)]
    pub offset: Option<usize>,
    #[serde(default)]
    pub limit: Option<usize>,
    #[serde(default)]
    pub count: Option<bool>,
}

/// Result returned by the model query endpoint.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ModelQueryResult {
    pub instances: Vec<Value>,
    pub total_count: usize,
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

// ---------------------------------------------------------------------------
// literal: URI parsing (typed)
// ---------------------------------------------------------------------------

/// Parse a `literal:` URI into a typed JSON value.
/// Returns the raw string as Value::String if not a literal: URI.
fn parse_literal_value(uri: &str) -> Value {
    let body = if let Some(rest) = uri.strip_prefix("literal:") {
        rest
    } else {
        return Value::String(uri.to_string());
    };

    if let Some(rest) = body.strip_prefix("string:") {
        let decoded = urlencoding::decode(rest).unwrap_or_else(|_| rest.into());
        // Check if it's a signed expression JSON
        if let Ok(json_val) = serde_json::from_str::<Value>(&decoded) {
            if let Some(data) = json_val.get("data") {
                return match data {
                    Value::String(s) => Value::String(s.clone()),
                    _ => data.clone(),
                };
            }
        }
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
            if let Some(data) = json_val.get("data") {
                return match data {
                    Value::String(s) => Value::String(s.clone()),
                    _ => data.clone(),
                };
            }
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
    let safe_tc = validate_iri(target_class).unwrap_or(target_class);
    let safe_pred = validate_iri(predicate).unwrap_or(predicate);
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
    // Build SPARQL to find conforming instances and their property values
    let sparql = build_instance_sparql(&shape, query_input);

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

    // Apply where-clause filters
    if let Some(ref where_clause) = query_input.where_clause {
        instances.retain(|inst| matches_where(inst, where_clause, &shape));
    }

    // Calculate total count before pagination
    let total_count = instances.len();

    // Apply ordering
    if let Some(ref order) = query_input.order {
        sort_instances(&mut instances, order);
    } else if query_input.limit.is_some() || query_input.offset.is_some() {
        // Default: order by timestamp ASC when paginating
        sort_instances(
            &mut instances,
            &[("timestamp".to_string(), OrderDirection::ASC)],
        );
    }

    // Apply pagination
    let offset = query_input.offset.unwrap_or(0);
    let mut paginated: Vec<Value> = if let Some(limit) = query_input.limit {
        instances.into_iter().skip(offset).take(limit).collect()
    } else {
        instances.into_iter().skip(offset).collect()
    };

    // ── Eager-load included relations ────────────────────────────────────
    if let Some(ref include) = query_input.include {
        if !paginated.is_empty() && !shape.include_relations.is_empty() {
            resolve_includes_recursive(store, &mut paginated, include, &shape, depth)?;
        }
    }

    // Strip unrequested properties if specified
    let final_instances = if let Some(ref requested) = query_input.properties {
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

    Ok(ModelQueryResult {
        instances: final_instances,
        total_count,
    })
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
            });
        }
    }

    // Parse relations from the metadata
    if let Some(rels) = meta["relations"].as_object() {
        for (name, rel_meta) in rels {
            let predicate = rel_meta["predicate"].as_str().unwrap_or("").to_string();
            if predicate.is_empty() {
                continue;
            }

            let direction = rel_meta["direction"]
                .as_str()
                .map(|s| s.to_string())
                .or_else(|| Some("forward".to_string()));

            let kind = rel_meta["kind"].as_str().unwrap_or("hasMany").to_string();
            let is_scalar_relation = kind == "hasOne" || kind == "belongsToOne";

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
/// 2. Data retrieval: Fetch all links for conforming instances with reifier metadata
fn build_instance_sparql(shape: &ModelShape, query: &ModelQueryInput) -> String {
    let (conformance, where_extra) = build_query_patterns(shape, query);

    format!(
        r#"SELECT ?source ?predicate ?target ?author ?timestamp WHERE {{
{conformance}
{where_extra}
    ?source ?predicate ?target .
    ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
    FILTER(isIRI(?source) && isIRI(?predicate))
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
}}"#
    )
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
/// pushed entirely to SPARQL.  If true, a COUNT query can skip hydration.
///
/// Only id/base filters and relation-based where (which match plain URIs, not
/// signed envelopes) are SPARQL-pushable.  Property equality is NOT pushable
/// because stored values are signed JSON envelopes.
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
        // Relation-based where (forward or reverse) can be pushed
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
        // All other conditions (property values, metadata, ops) need post-hydration
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
                let safe_id = validate_iri(id).unwrap_or(id);
                let safe_pred = validate_iri(predicate).unwrap_or(predicate);
                conformance_patterns.push(format!("    <{safe_id}> <{safe_pred}> ?source ."));
            }
            ParentScope::Model { id, field, model } => {
                let safe_id = validate_iri(id).unwrap_or(id);
                // For model-based parent scope, we need to resolve the predicate
                // The client should send the resolved predicate, but we handle both forms
                if let Some(ref f) = field {
                    let safe_f = validate_iri(f).unwrap_or(f);
                    conformance_patterns.push(format!("    <{safe_id}> <{safe_f}> ?source ."));
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
            has_conformance = true;
            if prop.is_flag {
                if let Some(ref initial) = prop.initial_value {
                    conformance_patterns
                        .push(format!("    ?source <{}> <{}> .", prop.predicate, initial));
                } else {
                    conformance_patterns.push(format!(
                        "    ?source <{}> ?cf_{} .",
                        prop.predicate, prop.name
                    ));
                }
            } else {
                conformance_patterns.push(format!(
                    "    ?source <{}> ?cf_{} .",
                    prop.predicate, prop.name
                ));
            }
        }
    }

    // Fallback: if no required properties, try initial values
    if !has_conformance {
        for prop in &shape.properties {
            if let Some(ref initial) = prop.initial_value {
                has_conformance = true;
                if prop.is_flag {
                    conformance_patterns
                        .push(format!("    ?source <{}> <{}> .", prop.predicate, initial));
                } else {
                    conformance_patterns.push(format!(
                        "    ?source <{}> ?cfInit_{} .",
                        prop.predicate, prop.name
                    ));
                }
                break;
            }
        }
    }

    // Fallback: structural matching using known predicates
    if !has_conformance && conformance_patterns.is_empty() {
        let known_predicates: Vec<String> = shape
            .properties
            .iter()
            .filter(|p| !p.predicate.is_empty())
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
    // Property equality CANNOT be pushed because stored values are signed JSON
    // envelopes (`literal:json:{author,timestamp,data,proof}`), not plain
    // `literal:string:X`.  Only id/base and relation-based where can be pushed.
    let mut where_patterns = Vec::new();
    if let Some(ref wc) = query.where_clause {
        for (prop_name, condition) in wc {
            if prop_name == "base" || prop_name == "id" {
                match condition {
                    WhereCondition::String(val) => {
                        // Use STR() comparison to avoid IRI parsing issues with
                        // literal:json URIs that contain encoded special chars
                        where_patterns.push(format!(
                            "    FILTER(STR(?source) = \"{}\")",
                            val.replace('\\', "\\\\").replace('"', "\\\"")
                        ));
                    }
                    WhereCondition::StringArray(vals) => {
                        let ids = vals
                            .iter()
                            .map(|v| {
                                format!("\"{}\"", v.replace('\\', "\\\\").replace('"', "\\\""))
                            })
                            .collect::<Vec<_>>()
                            .join(", ");
                        where_patterns.push(format!("    FILTER(STR(?source) IN ({}))", ids));
                    }
                    _ => {} // complex id ops handled post-hydration
                }
                continue;
            }

            // Relation-based where: link targets are plain URIs (not signed),
            // so we CAN push these to SPARQL.  Use STR() comparison to avoid
            // IRI parsing failures with literal:json URIs.
            if let Some(prop) = shape
                .properties
                .iter()
                .find(|p| &p.name == prop_name && p.is_collection)
            {
                let direction = prop.direction.as_deref().unwrap_or("forward");
                let safe_name = prop_name.replace(|c: char| !c.is_alphanumeric(), "_");
                match condition {
                    WhereCondition::String(val) => {
                        let escaped = val.replace('\\', "\\\\").replace('"', "\\\"");
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
                            .map(|v| {
                                format!("\"{}\"", v.replace('\\', "\\\\").replace('"', "\\\""))
                            })
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

            // All other conditions (property values, metadata, ops) are
            // handled post-hydration in matches_where().
        }
    }

    let conformance = conformance_patterns.join("\n");
    let where_extra = where_patterns.join("\n");

    (conformance, where_extra)
}

/// Convert a JS value to its literal: IRI form, matching how the Rust executor
/// stores property values.
#[allow(dead_code)]
fn value_to_literal_iri_string(s: &str) -> String {
    // If it already looks like a URI, use as-is
    if s.contains("://") || s.starts_with("literal:") {
        return s.to_string();
    }
    // Wrap as literal:string:
    format!("literal:string:{}", urlencoding::encode(s))
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
    let mut pred_to_props: HashMap<&str, Vec<&ShapeProperty>> = HashMap::new();
    for p in &shape.properties {
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
    where_clause: &HashMap<String, WhereCondition>,
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
pub fn resolve_reverse_relations(
    store: &SparqlStore,
    instances: &mut [Value],
    relations: &[(String, String, bool)], // (name, predicate, is_single)
) -> Result<(), Error> {
    if relations.is_empty() || instances.is_empty() {
        return Ok(());
    }

    for (rel_name, predicate, is_single) in relations {
        // Single batched query per relation: find all links with this predicate
        // targeting ANY of our instances, then group by target in Rust.
        let all_links = store.query_links(None, Some(predicate), None, None, None, None)?;

        // Build target_id → [source_id, ...] map
        let mut target_to_sources: HashMap<String, Vec<String>> = HashMap::new();
        for link in &all_links {
            target_to_sources
                .entry(link.data.target.clone())
                .or_default()
                .push(link.data.source.clone());
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
                let val = source_ids.last().cloned().unwrap_or(Value::Null);
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
    // Collect all target IDs across all instances
    let mut all_ids: Vec<String> = Vec::new();
    for inst in instances.iter() {
        if let Some(arr) = inst[&rel.name].as_array() {
            for item in arr {
                if let Some(id) = item.as_str() {
                    if !all_ids.contains(&id.to_string()) {
                        all_ids.push(id.to_string());
                    }
                }
            }
        } else if let Some(id) = inst[&rel.name].as_str() {
            if !all_ids.contains(&id.to_string()) {
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
            items.last().cloned().unwrap_or(Value::Null)
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
        .map(|id| format!("<{}>", id))
        .collect::<Vec<_>>()
        .join(", ");
    let safe_pred = validate_iri(&rel.predicate).unwrap_or(&rel.predicate);
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
                .last()
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
        //   - Name: <recipe://name> → literal:json:{signed expression with data="Recipe 1"}

        let base1 = "literal:string:recipe1base";

        // Signed expression for name "Recipe 1"
        let signed_name = serde_json::json!({
            "author": "did:key:test123",
            "timestamp": "1700000000000",
            "data": "Recipe 1",
            "proof": {"key": "k", "signature": "s"}
        });
        let signed_name_str = serde_json::to_string(&signed_name).unwrap();
        let name_encoded = urlencoding::encode(&signed_name_str);
        let name_target = format!("literal:json:{}", name_encoded);

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
        let mut where_clause = HashMap::new();
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
        let signed_name = json!({
            "author": "did:key:test123",
            "timestamp": "1700000000000",
            "data": "General",
            "proof": {"key": "k", "signature": "s"}
        });
        let name_target = format!(
            "literal:json:{}",
            urlencoding::encode(&serde_json::to_string(&signed_name).unwrap())
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
        let app_name = json!({
            "author": "did:key:test123",
            "timestamp": "1700000000003",
            "data": "Chat",
            "proof": {"key": "k", "signature": "s"}
        });
        let app_name_target = format!(
            "literal:json:{}",
            urlencoding::encode(&serde_json::to_string(&app_name).unwrap())
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
}
