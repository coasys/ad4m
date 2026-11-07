//! Model shape resolution from SHACL triples.
//!
//! A "shape" describes the structure of a model class: its properties,
//! predicates, relations, flags, initial values, getters, and target shapes
//! for eager-loaded relations.  Shapes are needed at the start of every
//! query to generate correct conformance patterns and hydrate results.
//!
//! Production resolution is always [`load_shape`] — reads SHACL triples
//! that `registerShapes` wrote into the perspective's link store.
//! The result is memoized by `PerspectiveInstance::shape_cache` so each
//! class is parsed at most once per process lifetime.
//!
//! [`parse_shape_from_json`] survives as a `#[cfg(test)]`-only helper for
//! the integration test suite, which builds shapes from JSON fixtures
//! without going through the SHACL writer.

use super::types::{ModelShape, ShapeProperty, ShapeRelation, WhereCondition};
use super::utils::escape_sparql_string;
use crate::perspectives::shacl_parser::{
    class_name_of, current_shape_uri, SHAPE_PREDICATE, SUBJECT_CLASS,
};
use crate::perspectives::sparql_store::SparqlStore;
use deno_core::anyhow::Error;
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};

/// No SHACL is stored for `class_name`. Typed so a caller holding the error
/// can tell which class to wait for: on a shared perspective it may still be
/// syncing (see `PerspectiveInstance::model_query`).
#[derive(Debug)]
pub(crate) struct MissingShape {
    pub(crate) class_name: String,
}

impl std::fmt::Display for MissingShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "No SHACL shape stored for class '{}'. Register it with perspective.registerShapes first.",
            self.class_name
        )
    }
}

impl std::error::Error for MissingShape {}

/// Public (crate-level) entry point for loading a shape from the SHACL store.
///
/// Delegates to [`load_shape`].
pub(crate) fn load_shape_from_store(
    store: &SparqlStore,
    class_name: &str,
) -> Result<ModelShape, Error> {
    load_shape(store, class_name)
}

/// A registered class and the shape node it reads.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ClassShape {
    pub(crate) target_class: String,
    pub(crate) shape_uri: String,
}

/// The class registered under `class_name` (or with that target class IRI)
/// and its current shape. A class's name is `class_name_of` its target class
/// IRI; its shape is the one `current_shape_uri` picks, as the registry
/// (`PerspectiveInstance::shapes`) reads it. An IRI finds exactly its class.
/// Two classes may share a name (`a://Task`, `b://Task`): the bare name reads
/// the least target class IRI, never whichever the store yields first.
pub(crate) fn find_class(
    store: &SparqlStore,
    class_name: &str,
) -> Result<Option<ClassShape>, Error> {
    Ok(find_in(&class_index(store)?, class_name))
}

/// Every registered class with each shape it points to, as
/// (target class, shape URI) pairs.
fn class_index(store: &SparqlStore) -> Result<Vec<(String, String)>, Error> {
    // The triples themselves, as the rest of `load_shape` reads them.
    let rows: Vec<Value> = serde_json::from_str(&store.query(&format!(
        "SELECT ?class ?shape WHERE {{ ?class <rdf://type> <{SUBJECT_CLASS}> . \
         ?class <{SHAPE_PREDICATE}> ?shape . }}"
    ))?)?;
    Ok(rows
        .iter()
        .filter_map(|row| {
            Some((
                row["class"].as_str()?.to_string(),
                row["shape"].as_str()?.to_string(),
            ))
        })
        .collect())
}

/// [`find_class`] over a [`class_index`].
fn find_in(index: &[(String, String)], class_name: &str) -> Option<ClassShape> {
    let mut shapes: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for (target_class, shape_uri) in index {
        if target_class == class_name || class_name_of(target_class) == class_name {
            shapes.entry(target_class).or_default().push(shape_uri);
        }
    }
    shapes.into_iter().next().and_then(|(target_class, uris)| {
        current_shape_uri(uris).map(|shape_uri| ClassShape {
            target_class: target_class.to_string(),
            shape_uri: shape_uri.to_string(),
        })
    })
}

/// The class a relation's `sh:class` names: the class that holds that shape
/// URI (shape URIs belong to one class), or the class of that IRI.
fn class_of_shape(index: &[(String, String)], uri: &str) -> Option<String> {
    index
        .iter()
        .filter(|(class, shape)| shape == uri || class == uri)
        .map(|(class, _)| class)
        .min()
        .cloned()
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
    let index = class_index(store)?;
    let Some(ClassShape {
        target_class,
        shape_uri,
    }) = find_in(&index, class_name)
    else {
        return Err(MissingShape {
            class_name: class_name.to_string(),
        }
        .into());
    };

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
            ?transform ?interpretationHint ?identity ?ordering
        WHERE {{
            <{shape_uri}> <sh://property> ?propUri .
            ?propUri <sh://path> ?path .
            ?propUri <rdf://type> ?propType .
            OPTIONAL {{ ?propUri <ad4m://interpretation_hint> ?interpretationHint . }}
            OPTIONAL {{ ?propUri <ad4m://identity> ?identity . }}
            OPTIONAL {{ ?propUri <sh://datatype> ?datatype . }}
            OPTIONAL {{ ?propUri <sh://minCount> ?minCount . }}
            OPTIONAL {{ ?propUri <sh://maxCount> ?maxCount . }}
            OPTIONAL {{ ?propUri <ad4m://resolveLanguage> ?resolveLanguage . }}
            OPTIONAL {{ ?propUri <ad4m://writable> ?writable . }}
            OPTIONAL {{ ?propUri <ad4m://local> ?local . }}
            OPTIONAL {{ ?propUri <ad4m://getter> ?getter . }}
            OPTIONAL {{ ?propUri <ad4m://ordering> ?ordering . }}
            OPTIONAL {{ ?propUri <sh://hasValue> ?hasValue . }}
            OPTIONAL {{ ?propUri <sh://class> ?className . }}
            OPTIONAL {{ ?propUri <ad4m://relationKind> ?relationKind . }}
            OPTIONAL {{ ?propUri <ad4m://targetClassName> ?targetClassName . }}
            OPTIONAL {{ ?propUri <ad4m://whereFilter> ?whereFilter . }}
            OPTIONAL {{ ?propUri <ad4m://wherePredicates> ?wherePredicates . }}
            OPTIONAL {{ ?propUri <ad4m://filter> ?filterEnabled . }}
            OPTIONAL {{ ?propUri <ad4m://transform> ?transform . }}
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
        // Sole selector of storage mode. `None` → deterministic typed
        // literal (fast POS-index path). `Some("literal")` → signed
        // envelope on the built-in literal language. `Some(<addr>)` →
        // custom-language expression. Preserved verbatim, including an
        // explicit empty string.
        let resolve_language = first["resolveLanguage"]
            .as_str()
            .map(decode_literal_string_target);
        let writable = parse_bool_literal_target(first["writable"].as_str());
        let local = parse_bool_literal_target(first["local"].as_str());
        let getter = first["getter"].as_str().map(decode_literal_string_target);
        let ordering = first["ordering"].as_str().map(decode_literal_string_target);
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
        let transform = first["transform"].as_str().and_then(|raw| {
            let json_str = decode_literal_string_target(raw);
            serde_json::from_str::<super::types::TransformExpression>(&json_str).ok()
        });
        let interpretation_hint = first["interpretationHint"]
            .as_str()
            .map(decode_literal_string_target);
        // Identity marker: the `ad4m://identity` literal decodes to "true"
        // only when the SDNA explicitly declared this property the dedup key.
        let identity = first["identity"]
            .as_str()
            .map(decode_literal_string_target)
            .map(|v| v == "true")
            .unwrap_or(false);

        let min_count = parse_count_literal(first["minCount"].as_str());
        let max_count = parse_count_literal(first["maxCount"].as_str());

        // Collection vs single-valued is signaled two ways by the writer:
        //   - explicit rdf:type ad4m:CollectionShape (legacy path), and
        //   - maxCount unset on a relation (writer omits maxCount for *Many).
        // Either marker makes this a collection of values, but neither on
        // its own indicates a relation — a literal-valued property can also
        // be a collection (e.g. an array of strings).
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
        // A property is a relation only when relation-specific metadata is
        // present (`ad4m://relationKind`, `sh://class`, or
        // `ad4m://targetClassName`).  `CollectionShape` alone signals
        // multi-valued cardinality, not link-typed semantics.
        let is_relation =
            relation_kind.is_some() || target_class_uri.is_some() || target_class_name.is_some();
        // `parse_shacl_to_links` refuses this pair at registration (#908),
        // but a shape can also arrive by sync, raw link writes or import.
        // Such a stored shape still loads (kept as a relation with its
        // `datatype`, as before), so legacy classes keep working; this only
        // makes it visible.
        if datatype.is_some() && (target_class_uri.is_some() || target_class_name.is_some()) {
            log::warn!(
                "SHACL shape for class '{class_name}': property '{prop_uri}' sets both \
                 `datatype` and a target class; treating it as a relation (#908)"
            );
        }
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
            initial_value_from_constructor(store, &shape_uri, &path)
        };

        // Suppress writable-derived metadata: when explicitly false the
        // executor should treat the property as read-only.  Currently
        // expressed through the absence of a setter action; just keep
        // the flag available on ShapeProperty for downstream consumers.
        let _ = writable;
        let _ = filter_enabled;

        // `ad4m://local` marks a property whose links are written with
        // `LinkStatus::Local` (executor-private, never gossiped). Absent or
        // `false` means shared, which is the default for every property.
        let is_local = local.unwrap_or(false);

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
                transform: transform.clone(),
                interpretation_hint: interpretation_hint.clone(),
                identity,
                ordering: ordering.clone(),
                local: is_local,
            });

            let target_class = target_class_uri
                .as_deref()
                .and_then(|uri| class_of_shape(&index, uri));
            let resolved_target_class_name = target_class_name.clone().unwrap_or_else(|| {
                if let Some(class) = &target_class {
                    return class_name_of(class);
                }
                target_class_uri
                    .as_deref()
                    .map(extract_class_local_name)
                    .map(|s| {
                        s.strip_suffix("Shape")
                            .map(|stripped| stripped.to_string())
                            .unwrap_or(s)
                    })
                    .unwrap_or_default()
            });

            include_relations.push(ShapeRelation {
                name,
                predicate: path,
                direction: direction.unwrap_or_else(|| "forward".to_string()),
                kind: relation_kind.unwrap_or_else(|| "hasMany".to_string()),
                max_count: max_count.map(|m| m as usize),
                target_class_name: resolved_target_class_name,
                target_class_uri: target_class_uri.clone().unwrap_or_default(),
                target_class: target_class.unwrap_or_default(),
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
                transform,
                interpretation_hint,
                identity,
                ordering: ordering.clone(),
                local: is_local,
            });
        }
    }

    // Class-level interpretation hint lives on the shape node itself.
    let class_hint_query = format!(
        r#"
        SELECT ?hint WHERE {{
            <{shape_uri}> <ad4m://interpretation_hint> ?hint .
        }}
        LIMIT 1
        "#
    );
    let class_hint = store
        .query(&class_hint_query)
        .ok()
        .and_then(|json| serde_json::from_str::<Vec<Value>>(&json).ok())
        .and_then(|rows| {
            rows.first()
                .and_then(|r| r["hint"].as_str().map(decode_literal_string_target))
        });

    Ok(ModelShape {
        target_class,
        shape_uri,
        properties,
        include_relations,
        interpretation_hint: class_hint,
    })
}

/// Recover an initial value for a non-flag property by inspecting the
/// constructor actions encoded as `ad4m://constructor` literal JSON on
/// the shape.  Returns the `target` of any `addLink` action whose
/// `predicate` matches `predicate`.
fn initial_value_from_constructor(
    store: &SparqlStore,
    shape_uri: &str,
    predicate: &str,
) -> Option<String> {
    // The constructor link is anchored to the shape the class reads
    // (`find_class`), not the target class.
    let safe_shape = escape_sparql_string(shape_uri);
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
pub(crate) fn decode_literal_string_target(raw: &str) -> String {
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
    use super::types::TransformExpression;
    let meta: Value = serde_json::from_str(json)
        .map_err(|e| deno_core::anyhow::anyhow!("Failed to parse shape JSON: {}", e))?;

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
                interpretation_hint: None,
                identity: false,
                ordering: None,
                local: prop_meta["local"].as_bool().unwrap_or(false),
            });
        }
    }

    if let Some(rels) = meta["relations"].as_object() {
        for (name, rel_meta) in rels {
            let predicate = rel_meta["predicate"].as_str().unwrap_or("").to_string();
            let getter = rel_meta["getter"].as_str().map(|s| s.to_string());
            let ordering = rel_meta["ordering"].as_str().map(|s| s.to_string());

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

            // Mirror `load_shape` cardinality handling: scalar relations
            // (hasOne / belongsToOne) are stored as single-valued, collection
            // relations remain marked as `is_collection: true`.  Hard-coding
            // `true` here previously diverged from the runtime path.
            properties.push(ShapeProperty {
                name: name.clone(),
                predicate: predicate.clone(),
                is_collection: !is_scalar_relation,
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
                interpretation_hint: None,
                identity: false,
                ordering: ordering.clone(),
                local: rel_meta["local"].as_bool().unwrap_or(false),
            });

            if rel_meta.get("targetShape").is_some() || rel_meta.get("targetClassName").is_some() {
                let target_shape = &rel_meta["targetShape"];
                let target_class_name = rel_meta["targetClassName"]
                    .as_str()
                    .or_else(|| target_shape["className"].as_str())
                    .unwrap_or("")
                    .to_string();
                let target_class_uri = target_shape["shapeUri"]
                    .as_str()
                    .or_else(|| rel_meta["class"].as_str())
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
                    target_class_uri,
                    target_class: target_shape["targetClass"]
                        .as_str()
                        .unwrap_or("")
                        .to_string(),
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
        interpretation_hint: None,
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
