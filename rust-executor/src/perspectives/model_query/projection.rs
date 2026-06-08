//! Projection resolution for lightweight relation aggregations.
//!
//! Projections (keys prefixed with `$` in the TS query) compute per-instance
//! counts or filtered lists over a relation predicate without fully hydrating
//! the related instances.  Each projection produces a single grouped SPARQL
//! query that runs against the store and attaches results to the parent
//! instances.
//!
//! Two modes are supported:
//! - **Count** (`count: true`) — attaches an integer count per parent.
//! - **List** (`count: false`) — attaches an array of target IRIs per parent,
//!   optionally ordered and limited.
//!
//! Projections can also filter by target properties (via `where`) and by
//! reifier metadata (author/timestamp).

use serde_json::Value;
use std::collections::{BTreeMap, HashMap};

use super::types::{
    ModelQueryInput, ModelShape, OrderDirection, ProjectionInput, ShapeResolver, WhereCondition,
};
use super::utils::{
    escape_sparql_string, format_literal_number, looks_like_absolute_iri, validate_iri,
};

const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";
const XSD_INTEGER: &str = "http://www.w3.org/2001/XMLSchema#integer";
const XSD_DECIMAL: &str = "http://www.w3.org/2001/XMLSchema#decimal";
const XSD_BOOLEAN: &str = "http://www.w3.org/2001/XMLSchema#boolean";

/// Render a finite f64 as a typed-literal SPARQL term, mirroring the storage
/// layer's `xsd:integer` / `xsd:decimal` split.
fn typed_number_literal(n: f64) -> Option<String> {
    let s = format_literal_number(n)?;
    let dt = if n.fract() == 0.0 && n.abs() < (i64::MAX as f64) {
        XSD_INTEGER
    } else {
        XSD_DECIMAL
    };
    Some(format!("\"{s}\"^^<{dt}>"))
}
use crate::perspectives::sparql_store::SparqlStore;

/// Resolve all projections for a set of parent instances.
///
/// Two-stage pipeline (audit item **H**):
///
/// 1. **Count projections fold into one fused SELECT.**  Each count
///    projection contributes a `?source ?_count_<i>` column via an
///    `OPTIONAL { { SELECT ?source (COUNT(?_t_<i>) AS ?_count_<i>) WHERE
///    { ?source <pred_<i>> ?_t_<i> . [extras] } GROUP BY ?source } }`
///    block.  All counts resolve in one round trip.
///
/// 2. **List projections fire as parallel futures.**  Each list projection
///    keeps its own SPARQL (the multi-row binding shape and per-projection
///    LIMIT/ORDER don't fold cleanly into a single SELECT) but every list
///    query is launched together via `futures::future::join_all`, so the
///    wall-clock cost is `max(...)` instead of `sum(...)` of the
///    per-projection latencies.  The fused count query joins them at the
///    front of the batch.
///
/// When `proj.target_shape` is set, raw target IRIs are replaced with
/// fully hydrated model instances via a recursive `execute_model_query_inner`
/// call.  Target hydration happens *after* the parallel SPARQL phase
/// completes — it can't fold into the fused query because the recursive
/// pipeline needs the collected IRIs first.
pub(super) async fn resolve_projections(
    store: &SparqlStore,
    instances: &mut Vec<Value>,
    projections: &HashMap<String, ProjectionInput>,
    shape: &ModelShape,
    resolver: &dyn ShapeResolver,
    depth: u8,
) -> Result<(), deno_core::anyhow::Error> {
    if instances.is_empty() || projections.is_empty() {
        return Ok(());
    }

    let parent_ids: Vec<String> = instances
        .iter()
        .filter_map(|inst| inst["id"].as_str())
        .filter_map(|id| validate_iri(id).ok().map(|s| s.to_string()))
        .collect();

    if parent_ids.is_empty() {
        return Ok(());
    }

    let values_clause = parent_ids
        .iter()
        .map(|id| format!("<{id}>"))
        .collect::<Vec<_>>()
        .join(" ");

    // ---- Stage 1: split projections into count vs list buckets ----
    //
    // We can't pre-build SPARQL until we've resolved predicates against
    // the shape, so first we collect the resolved metadata for each
    // projection key.  Anything that fails resolution (missing relation,
    // invalid predicate IRI, etc.) gets logged + skipped here so the
    // later stages only see well-formed inputs.
    struct ResolvedProj<'a> {
        key: String,
        proj: &'a ProjectionInput,
        safe_pred: String,
        where_patterns: String,
        reifier_patterns: String,
    }
    let mut count_projs: Vec<ResolvedProj> = Vec::new();
    let mut list_projs: Vec<ResolvedProj> = Vec::new();

    for (key, proj) in projections {
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
        let where_patterns = build_projection_where_patterns(proj, resolver);
        let reifier_patterns = build_projection_reifier_patterns(proj, &safe_pred);
        let resolved = ResolvedProj {
            key: key.clone(),
            proj,
            safe_pred,
            where_patterns,
            reifier_patterns,
        };
        if proj.count {
            count_projs.push(resolved);
        } else {
            list_projs.push(resolved);
        }
    }

    // ---- Stage 2: fused count query (one round trip for all counts) ----
    //
    // Each count projection contributes an OPTIONAL sub-SELECT.  When
    // there's only one count, the wrapping SELECT degenerates to the
    // historical shape — no overhead.  When there are several, we save
    // one round trip per count beyond the first.
    let mut count_results: HashMap<String, HashMap<String, u64>> = HashMap::new();
    if !count_projs.is_empty() {
        let count_parts: Vec<(String, String, String)> = count_projs
            .iter()
            .map(|rp| {
                (
                    rp.safe_pred.clone(),
                    rp.where_patterns.clone(),
                    rp.reifier_patterns.clone(),
                )
            })
            .collect();
        let fused_sparql = build_fused_count_sparql(&count_parts, &values_clause);
        let rows = store.query_values_async(&fused_sparql).await?;
        for (i, rp) in count_projs.iter().enumerate() {
            let col = format!("_count_{i}");
            let mut by_parent: HashMap<String, u64> = HashMap::new();
            for row in &rows {
                let parent = row["source"].as_str().unwrap_or("").to_string();
                if parent.is_empty() {
                    continue;
                }
                let n: u64 = row
                    .get(&col)
                    .and_then(|v| {
                        v.as_str().and_then(|s| s.parse().ok()).or_else(|| v.as_u64())
                    })
                    .unwrap_or(0);
                by_parent.insert(parent, n);
            }
            count_results.insert(rp.key.clone(), by_parent);
        }
    }

    // ---- Stage 3: parallel list-projection queries ----
    //
    // Each list projection builds its own SPARQL and we launch them all
    // concurrently via `futures::future::join_all`.  The store's
    // `query_values_async` uses `spawn_blocking` internally, so this
    // produces real wall-clock parallelism on the blocking thread pool.
    let list_sparqls: Vec<(String, String)> = list_projs
        .iter()
        .map(|rp| {
            let order_clause = build_projection_order_clause(rp.proj);
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
                safe_pred = rp.safe_pred,
                where_patterns = rp.where_patterns,
                reifier_patterns = rp.reifier_patterns,
                order_clause = order_clause,
            );
            (rp.key.clone(), sparql)
        })
        .collect();

    let list_results: Vec<Result<Vec<Value>, deno_core::anyhow::Error>> =
        futures::future::join_all(
            list_sparqls.iter().map(|(_, sparql)| store.query_values_async(sparql)),
        )
        .await;

    // ---- Stage 4: attach count results to instances ----
    for rp in &count_projs {
        let map = count_results
            .get(&rp.key)
            .cloned()
            .unwrap_or_default();
        for inst in instances.iter_mut() {
            if let Some(obj) = inst.as_object_mut() {
                let id = obj
                    .get("id")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string();
                let cnt = map.get(&id).copied().unwrap_or(0);
                obj.insert(rp.key.clone(), Value::Number(cnt.into()));
            }
        }
    }

    // ---- Stage 5: build list maps from the join_all results ----
    let mut list_maps: HashMap<String, HashMap<String, Vec<Value>>> = HashMap::new();
    for ((key, _), result) in list_sparqls.iter().zip(list_results.into_iter()) {
        let rows = match result {
            Ok(r) => r,
            Err(e) => {
                log::warn!("List projection '{key}' failed: {e}");
                continue;
            }
        };
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
        list_maps.insert(key.clone(), list_map);
    }

    // ---- Stage 6: list-projection post-processing (target hydration + attach) ----
    //
    // Target hydration runs sequentially because it recurses into the
    // shared pipeline — parallelising recursion would let two callers
    // race the same shape cache.  The parallel cost was in the outer
    // SPARQL queries above, which are already done.
    for rp in &list_projs {
        let key = &rp.key;
        let proj = rp.proj;
        let mut list_map = list_maps.remove(key).unwrap_or_default();

        if let Some(ref target_class) = proj.target_class_name {
            if !target_class.is_empty() {
                if let Ok(target_shape) = resolver.get_shape(target_class) {
                    let mut seen = std::collections::HashSet::new();
                    let mut all_ids: Vec<String> = Vec::new();
                    for vals in list_map.values() {
                        for v in vals {
                            if let Some(s) = v.as_str() {
                                if seen.insert(s.to_string()) {
                                    all_ids.push(s.to_string());
                                }
                            }
                        }
                    }

                    if !all_ids.is_empty() {
                        let mut sub_where = BTreeMap::new();
                        sub_where
                            .insert("id".to_string(), WhereCondition::StringArray(all_ids));
                        let sub_query = ModelQueryInput {
                            where_clause: Some(sub_where),
                            deep_query: Some(true),
                            ..ModelQueryInput::default()
                        };

                        if let Ok(result) = Box::pin(super::query::execute_model_query_inner(
                            store,
                            &target_shape,
                            &sub_query,
                            resolver,
                            depth + 1,
                        ))
                        .await
                        {
                            let hydrated: HashMap<String, Value> = result
                                .instances
                                .into_iter()
                                .filter_map(|inst| {
                                    let id = inst["id"].as_str()?.to_string();
                                    Some((id, inst))
                                })
                                .collect();

                            for vals in list_map.values_mut() {
                                for v in vals.iter_mut() {
                                    if let Some(id) = v.as_str() {
                                        if let Some(obj) = hydrated.get(id) {
                                            *v = obj.clone();
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
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

    Ok(())
}

/// Build one fused SELECT containing N sub-SELECT count blocks.  Each
/// projection contributes an `OPTIONAL { { SELECT ?source (COUNT(?_t_<i>)
/// AS ?_count_<i>) WHERE { ?source <pred_<i>> ?_t_<i> . [where_extras]
/// [reifier_extras] } GROUP BY ?source } }` block.  When there's only
/// one count projection, the wrapping SELECT degenerates to a single
/// sub-block with a stable column name — semantically identical to the
/// historical per-projection SELECT.
/// `parts` is `(safe_pred, where_patterns, reifier_patterns)` per
/// projection in iteration order.  See [`resolve_projections`] for the
/// surrounding pipeline.
fn build_fused_count_sparql(
    parts: &[(String, String, String)],
    values_clause: &str,
) -> String {
    let mut select_cols = String::from("?source");
    let mut option_blocks = String::new();
    for (i, (pred, wp, rp_pat)) in parts.iter().enumerate() {
        let col = format!("?_count_{i}");
        select_cols.push(' ');
        select_cols.push_str(&col);
        let wp_rewritten = rewrite_var(wp, "?t", &format!("?_t_{i}"), "?parent", "?source");
        let rp_rewritten = rewrite_var(rp_pat, "?t", &format!("?_t_{i}"), "?parent", "?source");
        option_blocks.push_str(&format!(
            concat!(
                "    OPTIONAL {{\n",
                "        {{ SELECT ?source (COUNT(DISTINCT ?_t_{i}) AS {col}) WHERE {{\n",
                "            ?source <{pred}> ?_t_{i} .\n",
                "{wp}",
                "{rp}",
                "        }} GROUP BY ?source }}\n",
                "    }}\n"
            ),
            i = i,
            col = col,
            pred = pred,
            wp = wp_rewritten,
            rp = rp_rewritten,
        ));
    }
    format!(
        concat!(
            "SELECT {sel} WHERE {{\n",
            "    VALUES ?source {{ {vc} }}\n",
            "{blocks}",
            "}}",
        ),
        sel = select_cols,
        vc = values_clause,
        blocks = option_blocks,
    )
}

/// `build_projection_where_patterns` and `build_projection_reifier_patterns`
/// were written against `?t` (target) and `?parent` (source) variable
/// names.  The fused query renumbers per-projection variables so each
/// sub-SELECT scope owns its own `?_t_<i>` — we have to rewrite the
/// emitted patterns to match.  Naïve string substitution would also touch
/// `?target` etc., so this rewrite is whole-word-aware via a regex-like
/// scan.
fn rewrite_var(s: &str, old1: &str, new1: &str, old2: &str, new2: &str) -> String {
    fn rep(s: &str, old: &str, new: &str) -> String {
        let mut out = String::with_capacity(s.len());
        let mut rest = s;
        while !rest.is_empty() {
            if rest.starts_with(old) {
                let after = &rest[old.len()..];
                let is_boundary = after
                    .chars()
                    .next()
                    .map_or(true, |c| !c.is_alphanumeric() && c != '_');
                if is_boundary {
                    out.push_str(new);
                    rest = after;
                    continue;
                }
            }
            let mut chars = rest.chars();
            if let Some(c) = chars.next() {
                out.push(c);
                rest = chars.as_str();
            }
        }
        out
    }
    rep(&rep(s, old1, new1), old2, new2)
}

/// Build SPARQL where-clause patterns for a projection's `where` filter.
///
/// Translates property-level conditions (string, number, bool, array) into
/// `FILTER` expressions that constrain which targets are counted/listed.
/// Conditions on `id`/`base` filter on `?t` directly; conditions on
/// `author`/`timestamp` are handled by [`build_projection_reifier_patterns`]
/// instead.
pub(super) fn build_projection_where_patterns(
    proj: &ProjectionInput,
    resolver: &dyn ShapeResolver,
) -> String {
    let Some(ref wc) = proj.where_clause else {
        return String::new();
    };

    // Resolve the target class's shape through the perspective cache so we
    // can translate property names in the projection's where-clause into the
    // predicate IRIs they map to in the store. The second tuple element
    // records whether each property carries `resolveLanguage: "literal"` —
    // only that resolver stores deterministic `literal:*:` targets that we
    // can probe directly. Other resolvers wrap values in author-signed
    // expression IRIs, so we fall back to `fn/parse_literal` for those.
    let (pred_lookup, resolution_failed) = if let Some(ref target_name) = proj.target_class_name {
        match resolver.get_shape(target_name) {
            Ok(target_shape) => {
                let mut map: HashMap<String, (String, bool)> = HashMap::new();
                for p in &target_shape.properties {
                    if !p.predicate.is_empty() {
                        map.insert(
                            p.name.clone(),
                            (
                                p.predicate.clone(),
                                p.resolve_language.as_deref() == Some("literal"),
                            ),
                        );
                    }
                }
                for r in &target_shape.include_relations {
                    if !r.predicate.is_empty() {
                        map.insert(r.name.clone(), (r.predicate.clone(), false));
                    }
                }
                (map, false)
            }
            Err(e) => {
                log::warn!(
                    "Projection where-clause resolution failed for target '{target_name}': {e}"
                );
                (HashMap::<String, (String, bool)>::new(), true)
            }
        }
    } else {
        (HashMap::<String, (String, bool)>::new(), false)
    };

    // Fail closed when the projection has non-system property filters but
    // the target shape could not be resolved.  Silently emitting an empty
    // pred_lookup would drop those filters and unexpectedly broaden the
    // projection results.
    if resolution_failed {
        let has_property_filters = wc
            .keys()
            .any(|k| k != "id" && k != "base" && k != "author" && k != "timestamp");
        if has_property_filters {
            return "    FILTER(false)\n".to_string();
        }
    }

    let mut patterns = Vec::new();
    let mut filter_idx = 0usize;

    for (prop_name, condition) in wc {
        if prop_name == "id" || prop_name == "base" {
            match condition {
                WhereCondition::String(val) => {
                    let escaped = escape_sparql_string(val);
                    patterns.push(format!("    FILTER(STR(?t) = \"{escaped}\")\n"));
                }
                WhereCondition::StringArray(vals) => {
                    let list = vals
                        .iter()
                        .map(|v| format!("\"{}\"", escape_sparql_string(v)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    patterns.push(format!("    FILTER(STR(?t) IN ({list}))\n"));
                }
                _ => {}
            }
            continue;
        }

        if prop_name == "author" || prop_name == "timestamp" {
            continue;
        }

        let (pred, is_literal_prop) = match pred_lookup.get(prop_name) {
            Some(v) => (v.0.clone(), v.1),
            None => continue,
        };

        if validate_iri(&pred).is_err() {
            continue;
        }

        let var = format!("_pw{filter_idx}");
        filter_idx += 1;

        match condition {
            WhereCondition::String(val) => {
                if is_literal_prop {
                    let escaped = escape_sparql_string(val);
                    let mut items = vec![format!("\"{escaped}\"^^<{XSD_STRING}>")];
                    if looks_like_absolute_iri(val) {
                        items.push(format!("<{val}>"));
                    }
                    patterns.push(format!("    VALUES ?{var} {{ {} }}\n", items.join(" ")));
                    patterns.push(format!("    ?t <{pred}> ?{var} .\n"));
                } else {
                    let escaped = escape_sparql_string(val);
                    patterns.push(format!("    ?t <{pred}> ?{var} .\n"));
                    patterns.push(format!("    FILTER(STR(?{var}) = \"{escaped}\")\n",));
                }
            }
            WhereCondition::Bool(b) => {
                if is_literal_prop {
                    patterns.push(format!("    ?t <{pred}> \"{b}\"^^<{XSD_BOOLEAN}> .\n"));
                } else {
                    let bval = if *b { "true" } else { "false" };
                    patterns.push(format!("    ?t <{pred}> ?{var} .\n"));
                    patterns.push(format!("    FILTER(STR(?{var}) = \"{bval}\")\n",));
                }
            }
            WhereCondition::Number(n) => {
                if is_literal_prop {
                    if let Some(typed) = typed_number_literal(*n) {
                        patterns.push(format!("    ?t <{pred}> {typed} .\n"));
                    } else {
                        patterns.push("    FILTER(false)\n".to_string());
                    }
                } else {
                    patterns.push(format!("    ?t <{pred}> ?{var} .\n"));
                    patterns.push(format!("    FILTER(STR(?{var}) = \"{n}\")\n",));
                }
            }
            WhereCondition::StringArray(vals) => {
                if is_literal_prop {
                    let mut items: Vec<String> = Vec::with_capacity(vals.len() * 2);
                    for v in vals {
                        let escaped = escape_sparql_string(v);
                        items.push(format!("\"{escaped}\"^^<{XSD_STRING}>"));
                        if looks_like_absolute_iri(v) {
                            items.push(format!("<{v}>"));
                        }
                    }
                    patterns.push(format!("    VALUES ?{var} {{ {} }}\n", items.join(" ")));
                    patterns.push(format!("    ?t <{pred}> ?{var} .\n"));
                } else {
                    let list = vals
                        .iter()
                        .map(|v| format!("\"{}\"", escape_sparql_string(v)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    patterns.push(format!("    ?t <{pred}> ?{var} .\n"));
                    patterns.push(format!("    FILTER(STR(?{var}) IN ({list}))\n",));
                }
            }
            WhereCondition::NumberArray(vals) => {
                if is_literal_prop {
                    let items: Vec<String> = vals
                        .iter()
                        .filter_map(|n| typed_number_literal(*n))
                        .collect();
                    if items.is_empty() {
                        patterns.push("    FILTER(false)\n".to_string());
                    } else {
                        patterns.push(format!("    VALUES ?{var} {{ {} }}\n", items.join(" ")));
                        patterns.push(format!("    ?t <{pred}> ?{var} .\n"));
                    }
                } else {
                    let list = vals
                        .iter()
                        .map(|n| format!("\"{n}\""))
                        .collect::<Vec<_>>()
                        .join(", ");
                    patterns.push(format!("    ?t <{pred}> ?{var} .\n"));
                    patterns.push(format!("    FILTER(STR(?{var}) IN ({list}))\n",));
                }
            }
            _ => {}
        }
    }

    patterns.join("")
}

/// Build an `ORDER BY` clause for a projection query.
///
/// Only `id`/`base` ordering is supported (ordering by `?t`).  Other
/// property-level ordering would require joining additional triples and
/// is not implemented for projections.
pub(super) fn build_projection_order_clause(proj: &ProjectionInput) -> String {
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
        let joined = terms.join(" ");
        format!("\nORDER BY {joined}")
    }
}

/// Build SPARQL patterns that filter projection targets by reifier metadata.
///
/// When the projection's where clause includes `author` or `timestamp`
/// conditions, this generates triple patterns that join against the RDF 1.2
/// reifier (the statement that records who created the link and when).
#[cfg(test)]
mod fused_count_tests {
    use super::*;

    #[test]
    fn fused_count_single_projection_is_well_formed() {
        let parts = vec![(
            "flux://has_tag".to_string(),
            String::new(),
            String::new(),
        )];
        let s = build_fused_count_sparql(&parts, "<a:1> <a:2>");
        assert!(s.contains("SELECT ?source ?_count_0 WHERE"));
        assert!(s.contains("VALUES ?source { <a:1> <a:2> }"));
        assert!(s.contains("OPTIONAL"));
        assert!(s.contains("(COUNT(DISTINCT ?_t_0) AS ?_count_0)"));
        assert!(s.contains("?source <flux://has_tag> ?_t_0"));
        assert!(s.contains("GROUP BY ?source"));
    }

    #[test]
    fn fused_count_multiple_projections_separated_by_optional() {
        let parts = vec![
            ("a://p1".to_string(), String::new(), String::new()),
            ("a://p2".to_string(), String::new(), String::new()),
            ("a://p3".to_string(), String::new(), String::new()),
        ];
        let s = build_fused_count_sparql(&parts, "<x>");
        assert!(s.contains("?_count_0"));
        assert!(s.contains("?_count_1"));
        assert!(s.contains("?_count_2"));
        assert!(s.contains("<a://p1>"));
        assert!(s.contains("<a://p2>"));
        assert!(s.contains("<a://p3>"));
        let optionals = s.matches("OPTIONAL").count();
        assert_eq!(optionals, 3, "expected one OPTIONAL block per projection");
    }

    #[test]
    fn rewrite_var_respects_word_boundaries() {
        let s = "?source <p> ?t . FILTER(?target = <x>)";
        let out = rewrite_var(s, "?t", "?_t_0", "?parent", "?source");
        assert!(out.contains("?_t_0"), "out={out}");
        assert!(out.contains("?target"), "`?target` should remain untouched: out={out}");
        assert!(!out.contains("?_t_0arget"), "must not splice mid-word: out={out}");
    }

    #[test]
    fn rewrite_var_rewrites_parent() {
        let s = "VALUES ?parent { <x> } ?parent <p> ?t .";
        let out = rewrite_var(s, "?t", "?_t_0", "?parent", "?source");
        assert!(out.contains("?source"));
        assert!(out.contains("?_t_0"));
        assert!(!out.contains("?parent"));
    }
}

pub(super) fn build_projection_reifier_patterns(proj: &ProjectionInput, safe_pred: &str) -> String {
    let Some(ref wc) = proj.where_clause else {
        return String::new();
    };

    let author_cond = wc.get("author");
    let timestamp_cond = wc.get("timestamp");

    if author_cond.is_none() && timestamp_cond.is_none() {
        return String::new();
    }

    let mut patterns = Vec::new();

    patterns.push(format!(
        "    ?_prj_reif <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<(?parent <{safe_pred}> ?t)>> .\n"
    ));

    if let Some(cond) = author_cond {
        patterns.push("    ?_prj_reif <ad4m://ontology/author> ?_prj_author .\n".to_string());
        if let WhereCondition::String(did) = cond {
            let escaped = escape_sparql_string(did);
            patterns.push(format!("    FILTER(STR(?_prj_author) = \"{escaped}\")\n"));
        }
    }

    if let Some(cond) = timestamp_cond {
        patterns.push("    ?_prj_reif <ad4m://ontology/timestamp> ?_prj_timestamp .\n".to_string());
        if let WhereCondition::String(ts) = cond {
            let escaped = escape_sparql_string(ts);
            patterns.push(format!(
                "    FILTER(STR(?_prj_timestamp) = \"{escaped}\")\n"
            ));
        }
    }

    patterns.join("")
}
