//! Model shape resolution from SHACL triples.
//!
//! A "shape" describes the structure of a model class: its properties,
//! predicates, relations, flags, initial values, getters, and target shapes
//! for eager-loaded relations.  Shapes are needed at the start of every
//! query to generate correct conformance patterns and hydrate results.
//!
//! Production resolution is always [`load_shape`] — reads SHACL triples
//! that were written by `addSdna()` into the perspective's link store.
//! The result is memoized by `PerspectiveInstance::shape_cache` so each
//! class is parsed at most once per process lifetime.
//!
//! [`parse_shape_from_json`] survives as a `#[cfg(test)]`-only helper for
//! the integration test suite, which builds shapes from JSON fixtures
//! without going through the SHACL writer.

use super::types::{ModelShape, ShapeProperty, ShapeRelation, WhereCondition};
use super::utils::escape_sparql_string;
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
/// - `<namespace://ClassName.propName> ad4m://relationKind literal:string:hasMany`
/// - `<namespace://ClassName.propName> ad4m://targetClassName literal:string:Ingredient`
/// - etc.
pub(crate) fn load_shape(store: &SparqlStore, class_name: &str) -> Result<ModelShape, Error> {
    let safe_name = escape_sparql_string(class_name);
    // Step 1: Find the shape URI and target class via SPARQL
    // Use exact suffix matching (/{name} or #{name}) to avoid "Recipe" matching "MyRecipe"
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
            "No SHACL shape stored for class '{}'. Call ensureSubjectClasses / addSdna first.",
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

    // Step 2: Load all property shapes for this shape.  Fetches every
    // predicate the SHACL writer emits in one batched SELECT.  Using
    // OPTIONAL keeps unset fields as nulls in the result rows.
    let props_query = format!(
        r#"
        SELECT
            ?propUri ?path ?propType
            ?datatype ?minCount ?maxCount
            ?resolveLanguage ?writable ?local
            ?getter ?hasValue ?className
            ?relationKind ?targetClassName
            ?whereFilter ?wherePredicates ?filterEnabled
        WHERE {{
            <{shape_uri}> <sh://property> ?propUri .
            ?propUri <sh://path> ?path .
            ?propUri <rdf://type> ?propType .
            OPTIONAL {{ ?propUri <sh://datatype> ?datatype . }}
            OPTIONAL {{ ?propUri <sh://minCount> ?minCount . }}
            OPTIONAL {{ ?propUri <sh://maxCount> ?maxCount . }}
            OPTIONAL {{ ?propUri <ad4m://resolveLanguage> ?resolveLanguage . }}
            OPTIONAL {{ ?propUri <ad4m://writable> ?writable . }}
            OPTIONAL {{ ?propUri <ad4m://local> ?local . }}
            OPTIONAL {{ ?propUri <ad4m://getter> ?getter . }}
            OPTIONAL {{ ?propUri <sh://hasValue> ?hasValue . }}
            OPTIONAL {{ ?propUri <sh://class> ?className . }}
            OPTIONAL {{ ?propUri <ad4m://relationKind> ?relationKind . }}
            OPTIONAL {{ ?propUri <ad4m://targetClassName> ?targetClassName . }}
            OPTIONAL {{ ?propUri <ad4m://whereFilter> ?whereFilter . }}
            OPTIONAL {{ ?propUri <ad4m://wherePredicates> ?wherePredicates . }}
            OPTIONAL {{ ?propUri <ad4m://filter> ?filterEnabled . }}
        }}
        "#
    );

    let props_json = store.query(&props_query)?;
    let prop_results: Vec<Value> = serde_json::from_str(&props_json)?;

    // Property shapes can fan into multiple rows because their `rdf://type`
    // may appear more than once (e.g. both sh:PropertyShape and a marker
    // type).  Group by propUri so we coalesce metadata from all rows
    // belonging to the same property shape before building the final
    // ShapeProperty / ShapeRelation.
    let mut grouped: HashMap<String, Vec<&Value>> = HashMap::new();
    let mut prop_order: Vec<String> = Vec::new();
    for row in &prop_results {
        let prop_uri = row["propUri"].as_str().unwrap_or("").to_string();
        if !grouped.contains_key(&prop_uri) {
            prop_order.push(prop_uri.clone());
        }
        grouped.entry(prop_uri).or_default().push(row);
    }

    let mut properties: Vec<ShapeProperty> = Vec::new();
    let mut include_relations: Vec<ShapeRelation> = Vec::new();

    for prop_uri in &prop_order {
        let rows = match grouped.get(prop_uri) {
            Some(r) => r,
            None => continue,
        };
        let first = rows[0];

        let path = first["path"].as_str().unwrap_or("").to_string();
        let datatype = first["datatype"].as_str().map(|s| s.to_string());
        let resolve_language = first["resolveLanguage"]
            .as_str()
            .map(decode_literal_string_target);
        let writable = parse_bool_literal_target(first["writable"].as_str());
        let local = parse_bool_literal_target(first["local"].as_str());
        let getter = first["getter"].as_str().map(decode_literal_string_target);
        let has_value = first["hasValue"].as_str().map(decode_literal_target_value);
        let target_class_uri = first["className"].as_str().map(|s| s.to_string());
        let relation_kind = first["relationKind"]
            .as_str()
            .map(decode_literal_string_target);
        let target_class_name = first["targetClassName"]
            .as_str()
            .map(decode_literal_string_target);
        let where_filter = parse_where_filter_literal(first["whereFilter"].as_str());
        let where_predicates = parse_where_predicates_literal(first["wherePredicates"].as_str());
        let filter_enabled = parse_bool_literal_target(first["filterEnabled"].as_str());

        let min_count = parse_count_literal(first["minCount"].as_str());
        let max_count = parse_count_literal(first["maxCount"].as_str());

        // Collection vs single-valued is signaled two ways by the writer:
        //   - explicit rdf:type ad4m:CollectionShape (legacy path), and
        //   - maxCount unset on a relation (writer omits maxCount for *Many).
        // Either marker makes this a collection.
        let prop_type_is_collection = rows.iter().any(|row| {
            row["propType"]
                .as_str()
                .map(|t| t == "ad4m://CollectionShape")
                .unwrap_or(false)
        });
        let scalar_kind = relation_kind
            .as_deref()
            .map(|k| k == "hasOne" || k == "belongsToOne")
            .unwrap_or(false);
        let direction = match relation_kind.as_deref() {
            Some("belongsToOne") | Some("belongsToMany") => Some("reverse".to_string()),
            Some(_) => Some("forward".to_string()),
            None => None,
        };
        let is_relation = relation_kind.is_some()
            || target_class_uri.is_some()
            || target_class_name.is_some()
            || prop_type_is_collection;
        // All relations are marked `is_collection` so the query pipeline
        // hydrates them as arrays during link grouping; the
        // `is_scalar_relation` flag then tells the renderer to unwrap
        // scalar relations (hasOne / belongsToOne) to a single value.
        let is_collection = if is_relation {
            true
        } else {
            prop_type_is_collection
        };

        // Extract local property name from prop_uri: "namespace://ClassName.propName" -> "propName"
        let name = prop_uri
            .rsplit_once('.')
            .map(|(_, n)| n.to_string())
            .unwrap_or_else(|| {
                path.rsplit(&['/', '#', ':'][..])
                    .next()
                    .unwrap_or("unknown")
                    .to_string()
            });

        // Flags are detected structurally: sh:hasValue + sh:minCount >= 1
        // (canonical SHACL representation, written by shacl-gen for @Flag).
        let is_flag = has_value.is_some() && min_count.unwrap_or(0) >= 1;
        let initial_value = if is_flag {
            has_value.clone()
        } else {
            initial_value_from_constructor(store, &target_class, &path)
        };

        // Suppress writable-derived metadata: when explicitly false the
        // executor should treat the property as read-only.  Currently
        // expressed through the absence of a setter action; just keep
        // the flag available on ShapeProperty for downstream consumers.
        let _ = writable;
        let _ = local;
        let _ = filter_enabled;

        if is_relation {
            // Relations participate in the standard ShapeProperty list so
            // the query/hydration pipeline can see their predicate, AND get
            // an entry in include_relations for eager-loading recursion.
            properties.push(ShapeProperty {
                name: name.clone(),
                predicate: path.clone(),
                is_collection,
                is_flag: false,
                is_required: min_count.unwrap_or(0) >= 1,
                initial_value: None,
                resolve_language: resolve_language.clone(),
                datatype: datatype.clone(),
                direction: direction.clone(),
                is_scalar_relation: scalar_kind,
                getter: getter.clone(),
                where_filter: where_filter.clone(),
                where_predicates: where_predicates.clone(),
            });

            let resolved_target_class_name = target_class_name.clone().unwrap_or_else(|| {
                // Fall back to extracting from the sh:class URI suffix.
                target_class_uri
                    .as_deref()
                    .map(extract_class_local_name)
                    .unwrap_or_default()
            });

            include_relations.push(ShapeRelation {
                name,
                predicate: path,
                direction: direction.unwrap_or_else(|| "forward".to_string()),
                kind: relation_kind.unwrap_or_else(|| "hasMany".to_string()),
                max_count: max_count.map(|m| m as usize),
                target_class_name: resolved_target_class_name,
            });
        } else {
            properties.push(ShapeProperty {
                name,
                predicate: path,
                is_collection,
                is_flag,
                is_required: min_count.unwrap_or(0) >= 1,
                initial_value,
                resolve_language,
                datatype,
                direction: None,
                is_scalar_relation: false,
                getter,
                where_filter,
                where_predicates,
            });
        }
    }

    Ok(ModelShape {
        target_class,
        shape_uri,
        properties,
        include_relations,
    })
}

/// Recover an initial value for a non-flag property by inspecting the
/// constructor actions encoded as `ad4m://constructor` literal JSON on
/// the shape.  Returns the `target` of any `addLink` action whose
/// `predicate` matches `predicate`.
fn initial_value_from_constructor(
    store: &SparqlStore,
    target_class: &str,
    predicate: &str,
) -> Option<String> {
    // The constructor link is anchored to the shape URI, not the target
    // class.  Resolve the shape URI from the target class so we can
    // query for its constructor.
    let safe_tc = escape_sparql_string(target_class);
    let shape_query = format!(
        r#"
        SELECT ?shapeUri WHERE {{
            <{safe_tc}> <ad4m://shape> ?shapeUri .
        }}
        LIMIT 1
        "#
    );
    let shape_result_json = store.query(&shape_query).ok()?;
    let shape_rows: Vec<Value> = serde_json::from_str(&shape_result_json).ok()?;
    let shape_uri = shape_rows.first()?["shapeUri"].as_str()?.to_string();

    let safe_shape = escape_sparql_string(&shape_uri);
    let ctor_query = format!(
        r#"
        SELECT ?ctor WHERE {{
            <{safe_shape}> <ad4m://constructor> ?ctor .
        }}
        LIMIT 1
        "#
    );
    let ctor_result_json = store.query(&ctor_query).ok()?;
    let ctor_rows: Vec<Value> = serde_json::from_str(&ctor_result_json).ok()?;
    let ctor_literal = ctor_rows.first()?["ctor"].as_str()?.to_string();

    let ctor_json = decode_literal_string_target(&ctor_literal);
    let actions: Value = serde_json::from_str(&ctor_json).ok()?;
    let arr = actions.as_array()?;
    for action in arr {
        let action_name = action["action"].as_str().unwrap_or("");
        let pred = action["predicate"].as_str().unwrap_or("");
        if action_name == "addLink" && pred == predicate {
            if let Some(target) = action["target"].as_str() {
                return Some(target.to_string());
            }
        }
    }
    None
}

/// Decode `literal:5^^xsd:integer` or `literal:number:5` into a `u32`.
fn parse_count_literal(raw: Option<&str>) -> Option<u32> {
    let raw = raw?;
    let trimmed = raw
        .strip_prefix("literal://")
        .or_else(|| raw.strip_prefix("literal:"))
        .unwrap_or(raw);
    let base = trimmed.split("^^").next().unwrap_or(trimmed);
    let base = base.strip_prefix("number:").unwrap_or(base);
    base.parse::<u32>().ok()
}

/// Decode `literal:true` / `literal:boolean:true` into a `bool`.
fn parse_bool_literal_target(raw: Option<&str>) -> Option<bool> {
    let raw = raw?;
    let trimmed = raw
        .strip_prefix("literal://")
        .or_else(|| raw.strip_prefix("literal:"))
        .unwrap_or(raw);
    let base = trimmed.strip_prefix("boolean:").unwrap_or(trimmed);
    match base {
        "true" => Some(true),
        "false" => Some(false),
        _ => None,
    }
}

/// Decode the target of a `literal:string:<urlencoded>` link.
fn decode_literal_string_target(raw: &str) -> String {
    let trimmed = raw
        .strip_prefix("literal://string:")
        .or_else(|| raw.strip_prefix("literal:string:"))
        .unwrap_or(raw);
    urlencoding::decode(trimmed)
        .map(|s| s.into_owned())
        .unwrap_or_else(|_| trimmed.to_string())
}

/// Decode a `literal:...` target into its raw value, preserving URI
/// targets unchanged.  Used for `sh:hasValue` which may be either a
/// raw URI (for relation flags) or a literal: form (for scalar flags).
fn decode_literal_target_value(raw: &str) -> String {
    if let Some(rest) = raw.strip_prefix("literal:string:") {
        return urlencoding::decode(rest)
            .map(|s| s.into_owned())
            .unwrap_or_else(|_| rest.to_string());
    }
    if let Some(rest) = raw.strip_prefix("literal://string:") {
        return urlencoding::decode(rest)
            .map(|s| s.into_owned())
            .unwrap_or_else(|_| rest.to_string());
    }
    if let Some(rest) = raw
        .strip_prefix("literal:")
        .or_else(|| raw.strip_prefix("literal://"))
    {
        // Non-string literal — strip any datatype suffix.
        let base = rest.split("^^").next().unwrap_or(rest);
        return base.to_string();
    }
    raw.to_string()
}

/// Decode the `whereFilter` literal:string into a BTreeMap of
/// WhereConditions, matching the shape executor consumers expect.
fn parse_where_filter_literal(raw: Option<&str>) -> Option<BTreeMap<String, WhereCondition>> {
    let raw = raw?;
    let json_str = decode_literal_string_target(raw);
    let parsed: Value = serde_json::from_str(&json_str).ok()?;
    parse_where_filter(&parsed)
}

/// Decode the `wherePredicates` literal:string into a HashMap of
/// property-name → predicate-IRI.
fn parse_where_predicates_literal(raw: Option<&str>) -> Option<HashMap<String, String>> {
    let raw = raw?;
    let json_str = decode_literal_string_target(raw);
    let parsed: Value = serde_json::from_str(&json_str).ok()?;
    let obj = parsed.as_object()?;
    let mut out = HashMap::new();
    for (k, v) in obj {
        if let Some(s) = v.as_str() {
            out.insert(k.clone(), s.to_string());
        }
    }
    if out.is_empty() {
        None
    } else {
        Some(out)
    }
}

/// Extract the local-name portion of a class URI, mirroring the
/// `extractLocalName` helper used by the SHACL writer in TypeScript.
fn extract_class_local_name(uri: &str) -> String {
    if let Some(hash_pos) = uri.rfind('#') {
        return uri[hash_pos + 1..].to_string();
    }
    if let Some(slash_pos) = uri.rfind('/') {
        let after = &uri[slash_pos + 1..];
        if !after.is_empty() {
            return after.to_string();
        }
    }
    if let Some(colon_pos) = uri.rfind(':') {
        let after = &uri[colon_pos + 1..];
        if !after.is_empty() {
            return after.to_string();
        }
    }
    uri.to_string()
}

/// Parse a JSON object into a where-clause filter map.
///
/// Each key-value pair is deserialized as a [`WhereCondition`].  Returns
/// `None` if the input is not an object or produces an empty map.
pub(crate) fn parse_where_filter(val: &Value) -> Option<BTreeMap<String, WhereCondition>> {
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

/// Parse shape metadata from JSON.  **Test-only** — production shape
/// resolution always goes through [`load_shape`] reading the SHACL graph
/// stored in the perspective.  This helper survives because the
/// integration test suite builds shapes from JSON fixtures rather than
/// SHACL writers.
#[cfg(test)]
pub(crate) fn parse_shape_from_json(json: &str, class_name: &str) -> Result<ModelShape, Error> {
    let meta: Value =
        serde_json::from_str(json).map_err(|e| anyhow!("Failed to parse shape JSON: {}", e))?;

    let target_class = meta["className"].as_str().unwrap_or(class_name).to_string();

    let mut properties = Vec::new();
    let mut include_relations: Vec<ShapeRelation> = Vec::new();

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

    if let Some(rels) = meta["relations"].as_object() {
        for (name, rel_meta) in rels {
            let predicate = rel_meta["predicate"].as_str().unwrap_or("").to_string();
            let getter = rel_meta["getter"].as_str().map(|s| s.to_string());

            if predicate.is_empty() && getter.is_none() {
                continue;
            }

            let direction = rel_meta["direction"]
                .as_str()
                .map(|s| s.to_string())
                .or_else(|| Some("forward".to_string()));

            let kind = rel_meta["kind"].as_str().unwrap_or("hasMany").to_string();
            let is_scalar_relation = kind == "hasOne" || kind == "belongsToOne";

            let where_filter = parse_where_filter(&rel_meta["whereFilter"]);
            let where_predicates = rel_meta["wherePredicates"].as_object().map(|obj| {
                obj.iter()
                    .filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), s.to_string())))
                    .collect::<HashMap<String, String>>()
            });

            properties.push(ShapeProperty {
                name: name.clone(),
                predicate: predicate.clone(),
                is_collection: true,
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

            if rel_meta.get("targetShape").is_some() || rel_meta.get("targetClassName").is_some() {
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
                });
            }
        }
    }

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

    #[test]
    fn test_parse_count_literal_xsd_form() {
        assert_eq!(parse_count_literal(Some("literal:5^^xsd:integer")), Some(5));
    }

    #[test]
    fn test_parse_count_literal_number_form() {
        assert_eq!(parse_count_literal(Some("literal:number:42")), Some(42));
    }

    #[test]
    fn test_parse_count_literal_invalid() {
        assert_eq!(parse_count_literal(Some("not-a-literal")), None);
        assert_eq!(parse_count_literal(None), None);
    }

    #[test]
    fn test_parse_bool_literal_target_variants() {
        assert_eq!(parse_bool_literal_target(Some("literal:true")), Some(true));
        assert_eq!(
            parse_bool_literal_target(Some("literal:boolean:false")),
            Some(false)
        );
        assert_eq!(
            parse_bool_literal_target(Some("literal://true")),
            Some(true)
        );
        assert_eq!(parse_bool_literal_target(Some("literal:other")), None);
    }

    #[test]
    fn test_decode_literal_string_target_url_decoded() {
        let encoded = format!("literal:string:{}", urlencoding::encode("hello world"));
        assert_eq!(decode_literal_string_target(&encoded), "hello world");
    }

    #[test]
    fn test_extract_class_local_name() {
        assert_eq!(extract_class_local_name("recipe://Recipe"), "Recipe");
        assert_eq!(
            extract_class_local_name("http://example.com/ns#Channel"),
            "Channel"
        );
        assert_eq!(
            extract_class_local_name("http://example.com/ns/Message"),
            "Message"
        );
    }

    #[test]
    fn test_parse_where_predicates_literal_round_trip() {
        let payload = serde_json::json!({"status": "todo://status", "priority": "todo://priority"});
        let literal = format!(
            "literal:string:{}",
            serde_json::to_string(&payload).unwrap()
        );
        let parsed = parse_where_predicates_literal(Some(&literal)).unwrap();
        assert_eq!(parsed.get("status").unwrap(), "todo://status");
        assert_eq!(parsed.get("priority").unwrap(), "todo://priority");
    }
}
