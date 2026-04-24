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
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};

use super::sparql_store::SparqlStore;

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
    #[serde(default)]
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
}

/// Enriched relation metadata for include (eager-loading) resolution.
/// Populated when the TS client sends target class shapes alongside the query.
#[derive(Debug, Clone)]
struct ShapeRelation {
    name: String,
    predicate: String,
    direction: String,         // "forward" or "reverse"
    kind: String,              // "hasMany", "hasOne", "belongsToOne", "belongsToMany"
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
        Value::String(s) => s.parse::<f64>().ok(),
        _ => None,
    }
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
    // Step 1: Find the shape URI and target class via SPARQL
    let query = format!(
        r#"
        SELECT ?shapeUri ?targetClass WHERE {{
            ?targetClass <rdf://type> <ad4m://SubjectClass> .
            ?targetClass <ad4m://shape> ?shapeUri .
            FILTER(STRENDS(STR(?targetClass), "{class_name}"))
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
    let query = format!(
        r#"
        SELECT ?target WHERE {{
            <{}> <{}> ?target .
        }}
        LIMIT 1
        "#,
        target_class, predicate
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
    // Load shape from store or parse from provided JSON
    let shape = if let Some(json) = shape_json {
        parse_shape_from_json(json, class_name)?
    } else {
        load_shape(store, class_name)?
    };

    // ── Fast path: COUNT-only ────────────────────────────────────────────
    // When the caller only needs a count (limit==0 or count==true) and all
    // where conditions can be pushed to SPARQL, we skip hydration entirely
    // and run a single SELECT COUNT(DISTINCT ?source) query.
    let is_count_only =
        query_input.count == Some(true) || query_input.limit == Some(0);
    if is_count_only && all_where_pushable(query_input, &shape) {
        let sparql = build_count_sparql(&shape, query_input);
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

    // Apply where-clause filters
    if let Some(ref where_clause) = query_input.where_clause {
        instances.retain(|inst| matches_where(inst, where_clause));
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
            resolve_includes_recursive(store, &mut paginated, include, &shape)?;
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

    let target_class = meta["className"]
        .as_str()
        .unwrap_or(class_name)
        .to_string();

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

            properties.push(ShapeProperty {
                name: name.clone(),
                predicate: predicate.clone(),
                is_collection: true,
                is_flag: false,
                is_required: false,
                initial_value: None,
                resolve_language: None,
                datatype: None,
            });

            // Parse enriched relation metadata (target shapes for include resolution)
            if rel_meta.get("targetShape").is_some() {
                let target_shape = &rel_meta["targetShape"];
                let target_class_name = rel_meta["targetClassName"]
                    .as_str()
                    .or_else(|| target_shape["className"].as_str())
                    .unwrap_or("")
                    .to_string();
                let kind = rel_meta["kind"]
                    .as_str()
                    .unwrap_or("hasMany")
                    .to_string();
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
                    target_shape_json: serde_json::to_string(target_shape)
                        .unwrap_or_default(),
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
fn build_count_sparql(shape: &ModelShape, query: &ModelQueryInput) -> String {
    let (conformance, where_extra) = build_query_patterns(shape, query);

    format!(
        r#"SELECT (COUNT(DISTINCT ?source) AS ?cnt) WHERE {{
{conformance}
{where_extra}
    ?source ?_anyPred ?_anyTarget .
    FILTER(isIRI(?source))
}}"#
    )
}

/// Check whether a query's where clause contains only conditions that can be
/// pushed to SPARQL (string equality, boolean equality, id/base filters).
/// If true, a COUNT query can run entirely in SPARQL without hydration.
fn all_where_pushable(query: &ModelQueryInput, shape: &ModelShape) -> bool {
    let Some(ref wc) = query.where_clause else {
        return true;
    };
    for (prop_name, condition) in wc {
        if prop_name == "base" || prop_name == "id" {
            if matches!(condition, WhereCondition::String(_)) {
                continue;
            }
            return false;
        }
        if prop_name == "author"
            || prop_name == "timestamp"
            || prop_name == "createdAt"
            || prop_name == "updatedAt"
        {
            return false;
        }
        if shape.properties.iter().any(|p| p.name == *prop_name) {
            match condition {
                WhereCondition::String(_) | WhereCondition::Bool(_) => continue,
                _ => return false,
            }
        } else {
            return false;
        }
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
                conformance_patterns
                    .push(format!("    <{}> <{}> ?source .", id, predicate));
            }
            ParentScope::Model { id, field, model } => {
                // For model-based parent scope, we need to resolve the predicate
                // The client should send the resolved predicate, but we handle both forms
                if let Some(ref f) = field {
                    conformance_patterns
                        .push(format!("    <{}> <{}> ?source .", id, f));
                } else {
                    // Fallback: use model name as predicate hint
                    conformance_patterns
                        .push(format!("    <{}> ?_parentPred ?source .", id));
                    conformance_patterns.push(format!(
                        "    FILTER(STRENDS(STR(?_parentPred), \"{}\"))",
                        model
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

    // WHERE clause filters that can be pushed to SPARQL (equality, IN)
    let mut where_patterns = Vec::new();
    if let Some(ref wc) = query.where_clause {
        for (prop_name, condition) in wc {
            if prop_name == "base" || prop_name == "id" {
                // Filter by base expression URI directly
                if let WhereCondition::String(val) = condition {
                    where_patterns.push(format!(
                        "    FILTER(?source = <{}>)",
                        val
                    ));
                }
                continue;
            }
            // Skip author/timestamp — handled post-hydration
            if prop_name == "author" || prop_name == "timestamp" || prop_name == "createdAt" || prop_name == "updatedAt" {
                continue;
            }

            // Find the property metadata
            if let Some(prop) = shape.properties.iter().find(|p| &p.name == prop_name) {
                // Only push simple equality to SPARQL
                match condition {
                    WhereCondition::String(val) => {
                        let iri_val = value_to_literal_iri_string(val);
                        where_patterns.push(format!(
                            "    ?source <{}> <{}> .",
                            prop.predicate, iri_val
                        ));
                    }
                    WhereCondition::Bool(val) => {
                        let iri_val = format!("literal:boolean:{}", val);
                        where_patterns.push(format!(
                            "    ?source <{}> <{}> .",
                            prop.predicate, iri_val
                        ));
                    }
                    _ => {
                        // Complex conditions (comparison ops, arrays, etc.) handled post-hydration
                    }
                }
            }
        }
    }

    let conformance = conformance_patterns.join("\n");
    let where_extra = where_patterns.join("\n");

    (conformance, where_extra)
}

/// Convert a JS value to its literal: IRI form, matching how the Rust executor
/// stores property values.
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

    // Build a predicate -> property map for fast lookup
    let pred_to_prop: HashMap<&str, &ShapeProperty> = shape
        .properties
        .iter()
        .map(|p| (p.predicate.as_str(), p))
        .collect();

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

        if let Some(prop) = pred_to_prop.get(predicate.as_str()) {
            if prop.is_collection {
                // Collections: accumulate all values with timestamps
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
        // Links with unknown predicates are ignored (they belong to other shapes or metadata)
    }

    // Set collection properties
    for (name, mut values) in collection_values {
        // Sort by timestamp (chronological)
        values.sort_by_key(|&(_, ts)| ts);
        let arr: Vec<Value> = values
            .iter()
            .map(|&(target, _)| {
                // For collections, the target is typically a URI (relation ID)
                // not a literal: value
                if target.starts_with("literal:") {
                    parse_literal_value(target)
                } else {
                    Value::String(target.to_string())
                }
            })
            .collect();
        obj.insert(name.to_string(), Value::Array(arr));
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
fn matches_where(instance: &Value, where_clause: &HashMap<String, WhereCondition>) -> bool {
    for (prop_name, condition) in where_clause {
        if prop_name == "base" || prop_name == "id" {
            // Already filtered in SPARQL
            continue;
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
                        (Value::Number(_), Value::Number(_)) => {
                            to_f64(val)
                                .zip(item.as_f64())
                                .map(|(a, b)| (a - b).abs() < f64::EPSILON)
                                .unwrap_or(false)
                        }
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
        // For string comparisons, use lexicographic comparison
        if let Value::String(s) = val {
            // Try parsing as timestamp for comparison
            if let Some(lt) = ops.lt {
                if let Ok(sv) = s.parse::<f64>() {
                    if sv >= lt {
                        return false;
                    }
                }
            }
            if let Some(lte) = ops.lte {
                if let Ok(sv) = s.parse::<f64>() {
                    if sv > lte {
                        return false;
                    }
                }
            }
            if let Some(gt) = ops.gt {
                if let Ok(sv) = s.parse::<f64>() {
                    if sv <= gt {
                        return false;
                    }
                }
            }
            if let Some(gte) = ops.gte {
                if let Ok(sv) = s.parse::<f64>() {
                    if sv < gte {
                        return false;
                    }
                }
            }
            if let Some((lo, hi)) = ops.between {
                if let Ok(sv) = s.parse::<f64>() {
                    if sv < lo || sv > hi {
                        return false;
                    }
                }
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
        return an
            .partial_cmp(&bn)
            .unwrap_or(Ordering::Equal);
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
        // Batch query: find all links with this predicate pointing to any of our instances
        for inst in instances.iter_mut() {
            let id = match inst["id"].as_str() {
                Some(id) => id.to_string(),
                None => continue,
            };

            let links = store.query_links(None, Some(predicate), Some(&id), None, None, None)?;

            let source_ids: Vec<Value> = links
                .iter()
                .map(|l| Value::String(l.data.source.clone()))
                .collect();

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
// Include (eager-loading) support (Phase 2)
// ---------------------------------------------------------------------------

/// Eager-load included relations for all instances.
pub(crate) fn resolve_includes(
    store: &SparqlStore,
    instances: &mut [Value],
    include: &HashMap<String, IncludeValue>,
    shape: &ModelShape,
) -> Result<(), Error> {
    for (rel_name, include_val) in include {
        // Find the relation property in the shape
        let prop = match shape.properties.iter().find(|p| &p.name == rel_name) {
            Some(p) => p,
            None => continue,
        };

        if !prop.is_collection {
            continue; // Only collections can be eagerly loaded
        }

        let sub_query = match include_val {
            IncludeValue::Bool(false) => continue,
            IncludeValue::Bool(true) => ModelQueryInput::default(),
            IncludeValue::SubQuery(sq) => *sq.clone(),
        };

        // For each instance, find related entities
        for inst in instances.iter_mut() {
            let id = match inst["id"].as_str() {
                Some(id) => id.to_string(),
                None => continue,
            };

            // Forward relation: source=instance, predicate=rel.predicate, target=?
            let related_links =
                store.query_links(Some(&id), Some(&prop.predicate), None, None, None, None)?;

            let related_ids: Vec<String> =
                related_links.iter().map(|l| l.data.target.clone()).collect();

            // If there's a sub-query with its own class, we'd need to recursively query
            // For now, just set the IDs
            let arr: Vec<Value> = related_ids.into_iter().map(Value::String).collect();

            inst.as_object_mut()
                .map(|obj| obj.insert(rel_name.clone(), Value::Array(arr)));
        }

        let _ = sub_query; // Will be used for nested querying in future
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
            resolve_reverse_include(store, instances, rel, &sub_query)?;
        } else {
            resolve_forward_include(store, instances, rel, &sub_query)?;
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
    wc.insert("id".to_string(), WhereCondition::StringArray(all_ids));
    query.where_clause = Some(wc);

    let result = execute_model_query(
        store,
        &rel.target_class_name,
        &query,
        Some(&rel.target_shape_json),
    )?;

    // Build id → hydrated instance map
    let mut hydrated: HashMap<String, Value> = HashMap::new();
    for inst in result.instances {
        if let Some(id) = inst["id"].as_str() {
            hydrated.insert(id.to_string(), inst);
        }
    }

    // Replace string IDs with hydrated objects on each parent instance
    for inst in instances.iter_mut() {
        let raw = inst[&rel.name].clone();
        let resolved = if let Some(arr) = raw.as_array() {
            let items: Vec<Value> = arr
                .iter()
                .filter_map(|item| {
                    item.as_str().and_then(|id| hydrated.get(id).cloned())
                })
                .collect();
            // hasOne (maxCount==1): unwrap to single value
            if rel.max_count == Some(1) {
                items.last().cloned().unwrap_or(Value::Null)
            } else {
                Value::Array(items)
            }
        } else if let Some(id) = raw.as_str() {
            hydrated.get(id).cloned().unwrap_or(Value::Null)
        } else {
            continue;
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
    let sparql = format!(
        "SELECT ?source ?target WHERE {{ ?source <{}> ?target . FILTER(?target IN ({})) }}",
        rel.predicate, id_list
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
    let hydrated: HashMap<String, Value> = if all_source_ids.is_empty() {
        HashMap::new()
    } else {
        let mut query = sub_query.clone();
        let mut wc = query.where_clause.take().unwrap_or_default();
        wc.insert(
            "id".to_string(),
            WhereCondition::StringArray(all_source_ids),
        );
        query.where_clause = Some(wc);

        let result = execute_model_query(
            store,
            &rel.target_class_name,
            &query,
            Some(&rel.target_shape_json),
        )?;

        let mut map = HashMap::new();
        for inst in result.instances {
            if let Some(id) = inst["id"].as_str() {
                map.insert(id.to_string(), inst);
            }
        }
        map
    };

    // Assign hydrated instances to each parent
    for inst in instances.iter_mut() {
        let inst_id = inst["id"].as_str().unwrap_or("").to_string();
        let source_ids = sources_by_target
            .get(&inst_id)
            .cloned()
            .unwrap_or_default();

        let resolved = if rel.kind == "belongsToOne" || rel.max_count == Some(1) {
            source_ids
                .last()
                .and_then(|id| hydrated.get(id).cloned())
                .unwrap_or(Value::Null)
        } else {
            let items: Vec<Value> = source_ids
                .iter()
                .filter_map(|id| hydrated.get(id).cloned())
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
        sort_instances(
            &mut instances,
            &[("age".to_string(), OrderDirection::ASC)],
        );
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
        let filtered = filter_properties(
            inst,
            &["name".to_string(), "age".to_string()],
        );
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
        assert!(shape.properties.iter().any(|p| p.name == "name" && p.is_required));
        assert!(shape.properties.iter().any(|p| p.name == "ingredients" && p.is_collection));
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
