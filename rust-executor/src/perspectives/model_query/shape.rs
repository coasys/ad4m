//! Model shape resolution from SHACL triples or client JSON metadata.
//!
//! A "shape" describes the structure of a model class: its properties,
//! predicates, relations, flags, initial values, getters, and target shapes
//! for eager-loaded relations.  Shapes are needed at the start of every
//! query to generate correct conformance patterns and hydrate results.
//!
//! Two resolution paths are supported:
//!
//! - **From the store** ([`load_shape`] / [`load_shape_from_store`]) — reads
//!   SHACL triple patterns that were written by `addSdna()` when the model
//!   class was registered.  This is used when no client-side metadata is
//!   available (e.g. for ad-hoc queries).
//!
//! - **From client JSON** ([`parse_shape_from_json`]) — parses the metadata
//!   that the TypeScript client sends alongside the query.  This is more
//!   reliable because it carries decorator information (flags, getters,
//!   relation kinds, target shapes) that SHACL triples don't fully capture.

use super::types::{ModelShape, ShapeProperty, ShapeRelation, TransformExpression, WhereCondition};
use super::utils::{escape_sparql_string, validate_iri};
use crate::perspectives::sparql_store::SparqlStore;
use deno_core::anyhow::{anyhow, Error};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};

/// Public (crate-level) entry point for loading a shape from the SHACL store.
///
/// Delegates to [`load_shape`].
pub(crate) fn load_shape_from_store(
    store: &SparqlStore,
    class_name: &str,
) -> Result<ModelShape, Error> {
    load_shape(store, class_name)
}

/// Load a model shape from the SHACL links stored in the Oxigraph store.
///
/// The SHACL links follow this pattern (set up by `parse_shacl_to_links`):
/// - `<namespace://ClassNameShape> sh://property <namespace://ClassName.propName>`
/// - `<namespace://ClassName.propName> sh://path <predicate_uri>`
/// - `<namespace://ClassName.propName> rdf://type sh://PropertyShape | ad4m://CollectionShape`
/// - `<namespace://ClassName.propName> sh://datatype <xsd://...>`
/// - `<namespace://ClassName.propName> sh://minCount literal:1^^xsd:integer`
/// - etc.
pub(super) fn load_shape(store: &SparqlStore, class_name: &str) -> Result<ModelShape, Error> {
    let safe_name = escape_sparql_string(class_name);
    // Step 1: Find the shape URI and target class via SPARQL
    // Use exact suffix matching (/{name} or #{name}) to avoid "Recipe" matching "MyRecipe"
    // Build hash suffix separately to avoid `#` confusing `format!`
    let hash_suffix = format!("#{safe_name}");
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
            transform: None,
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

/// Parse a JSON object into a where-clause filter map.
///
/// Each key-value pair is deserialized as a [`WhereCondition`].  Returns
/// `None` if the input is not an object or produces an empty map.
pub(super) fn parse_where_filter(val: &Value) -> Option<BTreeMap<String, WhereCondition>> {
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

/// Parse shape metadata from the JSON sent by the TypeScript client.
///
/// This is the preferred resolution path because the client has access to
/// the definitive decorator metadata (flags, required, initial values,
/// getters, relation kinds, target shapes for includes, etc.) that the
/// SHACL store doesn't fully capture.
pub(super) fn parse_shape_from_json(json: &str, class_name: &str) -> Result<ModelShape, Error> {
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

            let transform = prop_meta
                .get("transform")
                .map(|v| {
                    serde_json::from_value::<TransformExpression>(v.clone()).map_err(|e| {
                        deno_core::anyhow::anyhow!(
                            "Failed to parse transform for property '{}': {}",
                            name,
                            e
                        )
                    })
                })
                .transpose()?;

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
                transform,
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
                transform: None,
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
    let shape_uri = format!("{target_class}Shape");

    Ok(ModelShape {
        target_class,
        shape_uri,
        properties,
        include_relations,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

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
}
