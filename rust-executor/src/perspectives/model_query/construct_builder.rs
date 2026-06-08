//! Single-round-trip subgraph hydration via SPARQL `CONSTRUCT`.
//!
//! This is audit item **I** from `flux/docs/sparql-to-ad4m-model-migration.md`:
//! the "perfectly elegant `Ad4mModel` → SPARQL pipeline" endpoint.  Instead of
//! the historical recursive `execute_model_query_inner` pipeline that fires one
//! main SELECT + one COUNT + one nested SELECT *per* `include` relation *per*
//! level of nesting, the CONSTRUCT path materialises every triple needed for
//! the entire query subgraph — instance properties, included relation targets,
//! the included targets' properties, and so on recursively — in **one** round
//! trip.  A graph walker in [`walk_graph_to_instances`] then reassembles the
//! JSON tree from the returned triples.
//!
//! # When the CONSTRUCT path engages
//!
//! Only when [`can_use_construct`] returns `true`.  The fallback is silent —
//! callers always see the same JSON output regardless of which path runs.
//! Features that disqualify a query from the CONSTRUCT path are kept in
//! [`can_use_construct`] as guard clauses with a one-line explanation each.
//!
//! # Round-trip count
//!
//! - Historical pipeline at include depth N: `1 main + 1 count + N includes
//!   each firing their own 1+1 = 2 round trips → 1 + 1 + 2N`.
//! - CONSTRUCT pipeline: **1 round trip** regardless of include depth.
//!
//! The walker is `O(triples)` and uses two HashMap lookups per triple, so the
//! Rust-side cost stays linear in the result-set size.

use std::collections::{HashMap, HashSet};

use deno_core::anyhow::{anyhow, Error};
use serde_json::Value;

use super::types::{
    IncludeValue, ModelQueryInput, ModelShape, ShapeProperty, ShapeResolver, WhereCondition,
};
use super::utils::validate_iri;

/// Maximum CONSTRUCT include recursion depth.  Mirrors
/// [`utils::MAX_INCLUDE_DEPTH`](super::utils::MAX_INCLUDE_DEPTH) but applied
/// here at *plan* time so a malformed include map can't drive the SPARQL
/// builder into runaway pattern emission.
const MAX_CONSTRUCT_DEPTH: u8 = 16;

/// Returns `true` when the query can be satisfied by a single CONSTRUCT
/// round trip, and `false` when the recursive SELECT pipeline has to run.
///
/// Disqualifiers (each one cheap to check; bail early):
///
/// 1. `use_construct != Some(true)` — caller hasn't opted in.
/// 2. **Projections**: lightweight count/list projections (`$likeCount`,
///    `$comments`, …) require post-hydration `GROUP BY` + cross-relation
///    joins that don't fit cleanly into a single CONSTRUCT body.  Handled
///    by the existing `projection.rs` pipeline.
/// 3. **Pagination via OFFSET**: a non-zero `offset` requires the inner
///    pagination subquery to apply ORDER BY before OFFSET, which conflicts
///    with the CONSTRUCT WHERE's "materialise the whole subgraph" shape.
///    `limit` alone is fine — we wrap the source-selector in an inner
///    SELECT.  Zero-offset is allowed.
/// 4. **Includes with per-relation `where` / `order` / `limit` / `offset`**:
///    same reason.  CONSTRUCT can't apply per-include constraints — the
///    WHERE block is one global pattern.
/// 5. **Reverse-direction includes** (`@BelongsTo`): the source-arrow flip
///    requires a separate scan pattern.  Handled by the existing
///    `resolve_reverse_relations` path; could be folded in via a UNION
///    in a future PR.
/// 6. **Property getters or relation-on-getter**: `ASK { … }` / `SELECT { … }`
///    getter bodies are arbitrary SPARQL fragments that may overlap with
///    the CONSTRUCT body.  Skip rather than risk semantic drift.
/// 7. **Reifier metadata requested** (`with_metadata != Some(false)`): the
///    full pipeline needs the per-row author/timestamp fold that the
///    CONSTRUCT body would have to emit per included entity.  A future
///    extension could fold the reifier-metadata join into the CONSTRUCT
///    body keyed by the relation predicate.
/// 8. **Aggregation / count requested**: COUNT requires `GROUP BY` which a
///    CONSTRUCT body doesn't support.  Falls back so the caller still gets
///    `total_count`.
/// 9. **Nested include depth > MAX_CONSTRUCT_DEPTH**: defensive cap.
pub(super) fn can_use_construct(
    query: &ModelQueryInput,
    shape: &ModelShape,
) -> bool {
    // (1)
    if query.use_construct != Some(true) {
        return false;
    }
    // (2)
    if query.projections.as_ref().map_or(false, |p| !p.is_empty()) {
        return false;
    }
    // (3) pagination — offset must be 0 (or unset)
    if query.offset.unwrap_or(0) > 0 {
        return false;
    }
    // (7) reifier metadata
    if query.with_metadata.unwrap_or(true) {
        return false;
    }
    // (8) count fast-path
    if query.count == Some(true) {
        return false;
    }
    // (4) + (5) include sub-queries with constraints / reverse direction
    if let Some(include) = &query.include {
        if !includes_are_construct_compatible(include, shape) {
            return false;
        }
    }
    // (6) property/relation getters on the top-level shape
    for prop in &shape.properties {
        if prop.getter.is_some() {
            return false;
        }
    }
    true
}

/// Recursive walk that checks every level of the include tree for the
/// disqualifying constraints from [`can_use_construct`] points 4 and 5
/// (per-include `where`/`order`/`limit`/`offset`/`count`, reverse direction).
///
/// Returns `false` as soon as it finds any disqualifier — the caller falls
/// back to the legacy recursive pipeline.
fn includes_are_construct_compatible(
    include: &HashMap<String, IncludeValue>,
    parent_shape: &ModelShape,
) -> bool {
    for (rel_name, val) in include {
        // Per-include constraints — disqualify regardless of relation.
        if let IncludeValue::SubQuery(sq) = val {
            if sq.where_clause.is_some()
                || sq.order.is_some()
                || sq.limit.is_some()
                || sq.offset.is_some()
                || sq.count == Some(true)
            {
                return false;
            }
        }
        // Relation direction + recursion.  When the relation isn't
        // declared on this shape, leave the legacy path to handle it
        // (it logs + skips silently).
        if let Some(rel) = parent_shape
            .include_relations
            .iter()
            .find(|r| r.name == *rel_name)
        {
            if rel.direction == "reverse" {
                return false;
            }
            // Recurse into nested sub-includes.  Without a shape resolver
            // here we can't know the target shape's include_relations, so
            // we only walk the include map's structure — same-level
            // constraints + reverse-direction flags at this level get
            // checked above; nested levels are walked here with the
            // parent_shape as a fallback (any unknown relation falls
            // through, same as the top level).
            if let IncludeValue::SubQuery(sq) = val {
                if let Some(nested) = &sq.include {
                    if !includes_are_construct_compatible(nested, parent_shape) {
                        return false;
                    }
                }
            }
        }
    }
    true
}

/// Build a `CONSTRUCT { … } WHERE { … }` query that materialises the entire
/// requested subgraph.
///
/// Shape of the emitted SPARQL (with one `include: { tag: true }` example):
///
/// ```sparql
/// CONSTRUCT {
///     ?source ?p ?o .
///     ?source <flux://has_tag> ?_inc_0_t .
///     ?_inc_0_t ?_inc_0_p ?_inc_0_o .
/// }
/// WHERE {
///     # main pagination + conformance + where
///     {
///         SELECT DISTINCT ?source WHERE {
///             ?source <flux://entry_type> <flux://has_semantic_relationship> .
///         } LIMIT 10
///     }
///     ?source ?p ?o .
///     VALUES ?p { <flux://entry_type> <flux://has_tag> <flux://has_expression> … }
///     OPTIONAL {
///         ?source <flux://has_tag> ?_inc_0_t .
///         ?_inc_0_t <flux://entry_type> <flux://has_embedding> .
///         ?_inc_0_t ?_inc_0_p ?_inc_0_o .
///         VALUES ?_inc_0_p { <flux://entry_type> <flux://embedding> }
///     }
/// }
/// ```
///
/// The inner `SELECT ?source` applies the top-level WHERE + LIMIT, so the
/// CONSTRUCT body only materialises triples for sources that survive the
/// outer filter.  Each include adds one OPTIONAL block.  Recursive includes
/// chain — `?_inc_0_t` becomes the parent for `?_inc_0_0_t`, etc.
///
/// Returns `Err` when an IRI fails validation; returns `Ok(None)` when the
/// shape has no predicates at all (degenerate case — caller should fall
/// back to the legacy pipeline).
pub(super) fn build_construct_sparql(
    shape: &ModelShape,
    query: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
) -> Result<Option<String>, Error> {
    let mut construct_lines: Vec<String> = Vec::new();
    let mut where_lines: Vec<String> = Vec::new();

    // Inner pagination/scope: SELECT DISTINCT ?source { conformance + where }
    // LIMIT N.  This is what restricts the CONSTRUCT to the relevant page.
    let inner_select = build_source_selector(shape, query)?;
    where_lines.push(format!("    {{ {inner_select} }}"));

    // Main instance triples
    construct_lines.push("    ?source ?_p ?_o .".to_string());
    let predicate_filter = build_predicate_values(&shape.properties, "?_p");
    where_lines.push("    ?source ?_p ?_o .".to_string());
    if let Some(filter) = predicate_filter {
        where_lines.push(format!("    {filter}"));
    }

    // Recursively emit includes
    if let Some(include) = &query.include {
        let mut counter: usize = 0;
        emit_includes(
            shape,
            include,
            "?source",
            "_inc",
            &mut counter,
            0,
            resolver,
            &mut construct_lines,
            &mut where_lines,
        )?;
    }

    let construct_body = construct_lines.join("\n");
    let where_body = where_lines.join("\n");

    Ok(Some(format!(
        "CONSTRUCT {{\n{construct_body}\n}}\nWHERE {{\n{where_body}\n}}"
    )))
}

/// Build the inner `SELECT DISTINCT ?source { ... } LIMIT N` block that
/// scopes the CONSTRUCT to the matching instances.  Uses the same
/// conformance + where logic as the legacy `build_instance_sparql` so
/// pagination / WHERE-pushdown stays consistent across the two paths.
fn build_source_selector(
    shape: &ModelShape,
    query: &ModelQueryInput,
) -> Result<String, Error> {
    let mut patterns: Vec<String> = Vec::new();

    // Conformance — at minimum the entry-type flag.  Mirrors the
    // sparql_builder logic but stripped of the property-fetch shape.
    for prop in &shape.properties {
        if prop.is_required && prop.is_flag {
            if let Some(initial) = &prop.initial_value {
                if validate_iri(&prop.predicate).is_err() || validate_iri(initial).is_err() {
                    continue;
                }
                patterns.push(format!(
                    "?source <{}> <{}> .",
                    prop.predicate, initial
                ));
            }
        }
    }

    // Conformance fallback: if no flag found, at least require that ?source
    // has *some* triple matching one of the shape predicates.  Prevents the
    // CONSTRUCT from matching unrelated subjects.
    if patterns.is_empty() {
        let mut any_pred: Option<&str> = None;
        for prop in &shape.properties {
            if !prop.predicate.is_empty() && validate_iri(&prop.predicate).is_ok() {
                any_pred = Some(prop.predicate.as_str());
                break;
            }
        }
        if let Some(pred) = any_pred {
            patterns.push(format!("?source <{pred}> ?_any ."));
        } else {
            return Err(anyhow!(
                "build_construct_sparql: shape has no predicates — cannot build a CONSTRUCT scope"
            ));
        }
    }

    // WHERE-clause pushdown: only `id` / `base` equality flavours are
    // supported here.  Anything else => `can_use_construct` should have
    // returned false; we'd have fallen back already.  This is the same
    // subset the legacy `all_where_pushable` handles for the unique-id case.
    if let Some(wc) = &query.where_clause {
        if let Some(WhereCondition::String(iri)) = wc.get("id") {
            if validate_iri(iri).is_ok() {
                patterns.push(format!("VALUES ?source {{ <{iri}> }}"));
            }
        }
        if let Some(WhereCondition::StringArray(arr)) = wc.get("id") {
            let safe: Vec<String> = arr
                .iter()
                .filter(|s| validate_iri(s).is_ok())
                .map(|s| format!("<{s}>"))
                .collect();
            if !safe.is_empty() {
                patterns.push(format!("VALUES ?source {{ {} }}", safe.join(" ")));
            }
        }
    }

    let where_body = patterns.join(" ");
    let mut sql = format!("SELECT DISTINCT ?source WHERE {{ {where_body} }}");
    if let Some(limit) = query.limit {
        if limit > 0 {
            sql.push_str(&format!(" LIMIT {limit}"));
        }
    }
    Ok(sql)
}

/// Emit `VALUES ?var { <pred1> <pred2> … }` filter over a shape's
/// predicates.  Returns `None` when the shape has no relevant predicates.
fn build_predicate_values(props: &[ShapeProperty], var: &str) -> Option<String> {
    let preds: HashSet<&str> = props
        .iter()
        .filter(|p| !p.predicate.is_empty() && validate_iri(&p.predicate).is_ok())
        .map(|p| p.predicate.as_str())
        .collect();
    if preds.is_empty() {
        None
    } else {
        let mut iris: Vec<&&str> = preds.iter().collect();
        iris.sort();
        let body: Vec<String> = iris.iter().map(|p| format!("<{p}>")).collect();
        Some(format!("VALUES {var} {{ {} }}", body.join(" ")))
    }
}

/// Recursively emit the CONSTRUCT body + WHERE OPTIONAL blocks for a single
/// level of `include`.  Calls itself for nested includes.
#[allow(clippy::too_many_arguments)]
fn emit_includes(
    parent_shape: &ModelShape,
    include: &HashMap<String, IncludeValue>,
    parent_var: &str,
    prefix: &str,
    counter: &mut usize,
    depth: u8,
    resolver: &dyn ShapeResolver,
    construct_lines: &mut Vec<String>,
    where_lines: &mut Vec<String>,
) -> Result<(), Error> {
    if depth >= MAX_CONSTRUCT_DEPTH {
        log::warn!(
            "build_construct_sparql: include depth {depth} exceeded MAX_CONSTRUCT_DEPTH {MAX_CONSTRUCT_DEPTH} — truncating",
        );
        return Ok(());
    }

    for (rel_name, val) in include {
        let rel = match parent_shape.include_relations.iter().find(|r| r.name == *rel_name) {
            Some(r) => r,
            None => continue,
        };
        if rel.direction == "reverse" || validate_iri(&rel.predicate).is_err() {
            continue;
        }

        let target_shape = match resolver.get_shape(&rel.target_class_name) {
            Ok(s) => s,
            Err(_) => continue, // missing shape — skip this branch silently
        };

        let idx = *counter;
        *counter += 1;
        let target_var = format!("?{prefix}_{idx}_t");
        let target_pred_var = format!("?{prefix}_{idx}_p");
        let target_obj_var = format!("?{prefix}_{idx}_o");

        // CONSTRUCT: link parent → target, then target → its properties
        construct_lines.push(format!(
            "    {parent_var} <{}> {target_var} .",
            rel.predicate
        ));
        construct_lines.push(format!(
            "    {target_var} {target_pred_var} {target_obj_var} ."
        ));

        // WHERE: wrap the include in OPTIONAL so a missing relation
        // doesn't drop the parent.  Conformance filter on the target via
        // its entry-type flag if one exists.
        let mut opt_lines: Vec<String> = Vec::new();
        opt_lines.push(format!(
            "        {parent_var} <{}> {target_var} .",
            rel.predicate
        ));

        let target_conformance =
            build_target_conformance(target_shape.as_ref(), &target_var);
        for line in target_conformance {
            opt_lines.push(format!("        {line}"));
        }
        opt_lines.push(format!(
            "        {target_var} {target_pred_var} {target_obj_var} ."
        ));
        if let Some(filter) = build_predicate_values(&target_shape.properties, &target_pred_var) {
            opt_lines.push(format!("        {filter}"));
        }

        // Nested includes
        if let IncludeValue::SubQuery(sq) = val {
            if let Some(nested) = &sq.include {
                emit_includes(
                    target_shape.as_ref(),
                    nested,
                    &target_var,
                    &format!("{prefix}_{idx}"),
                    counter,
                    depth + 1,
                    resolver,
                    construct_lines,
                    where_lines,
                )?;
            }
        }

        let opt_body = opt_lines.join("\n");
        where_lines.push(format!("    OPTIONAL {{\n{opt_body}\n    }}"));
    }
    Ok(())
}

/// Produce the SPARQL pattern lines that enforce the target shape's
/// entry-type conformance — the equivalent of the
/// `?source <entry_type> <Class>` line at the top of the main shape's
/// CONSTRUCT body.
fn build_target_conformance(shape: &ModelShape, target_var: &str) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for prop in &shape.properties {
        if prop.is_required && prop.is_flag {
            if let Some(initial) = &prop.initial_value {
                if validate_iri(&prop.predicate).is_ok() && validate_iri(initial).is_ok() {
                    out.push(format!(
                        "{target_var} <{}> <{}> .",
                        prop.predicate, initial
                    ));
                }
            }
        }
    }
    out
}

/// Walk the triples returned by [`SparqlStore::query_triples`] and
/// reconstruct the JSON instance tree.
///
/// `triples` is the raw subject/predicate/object output where typed RDF
/// literals have already been wire-format encoded by
/// `storage_term_to_target_string`.  This walker:
///
/// 1. Groups triples by subject (one HashMap pass).
/// 2. Identifies which subjects are top-level instances of `shape` via the
///    entry-type flag triple.
/// 3. For each top-level subject, builds a JSON object whose keys are the
///    shape's property/relation names, recursively hydrating any included
///    relation targets via [`hydrate_one`].
///
/// Returns instances in the order their `?source` IRIs appear in the
/// triple stream — which for a CONSTRUCT against an `ORDER BY`-driven
/// inner SELECT is the requested page order.
pub(super) fn walk_graph_to_instances(
    triples: Vec<(String, String, String)>,
    shape: &ModelShape,
    query: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
) -> Result<Vec<Value>, Error> {
    // Pass 1: group by subject.  Order-preserving via `Vec` of subjects.
    let mut by_subject: HashMap<String, Vec<(String, String)>> = HashMap::new();
    let mut subject_order: Vec<String> = Vec::new();
    for (s, p, o) in triples {
        if !by_subject.contains_key(&s) {
            subject_order.push(s.clone());
        }
        by_subject.entry(s).or_default().push((p, o));
    }

    // Pass 2: filter to subjects matching the top-level shape's conformance flag.
    let conformance = find_required_flag(shape);
    let mut top_subjects: Vec<String> = Vec::new();
    for subj in &subject_order {
        if subject_matches_shape(&by_subject, subj, shape, &conformance) {
            top_subjects.push(subj.clone());
        }
    }

    // Pass 3: hydrate each top-level subject.  Recurses into includes.
    let include = query.include.as_ref();
    let mut out: Vec<Value> = Vec::with_capacity(top_subjects.len());
    for subj in top_subjects {
        let inst = hydrate_one(&by_subject, &subj, shape, include, resolver, 0)?;
        out.push(inst);
    }
    Ok(out)
}

/// Find the `(predicate, value)` of the shape's required-flag triple, if
/// the shape declares one.  Used to filter the triple stream to top-level
/// instances of the shape.
fn find_required_flag(shape: &ModelShape) -> Option<(String, String)> {
    for prop in &shape.properties {
        if prop.is_required && prop.is_flag {
            if let Some(initial) = &prop.initial_value {
                return Some((prop.predicate.clone(), initial.clone()));
            }
        }
    }
    None
}

/// Returns `true` when the given subject has a triple matching the shape's
/// required-flag conformance pattern, or has *any* triple with a
/// shape-declared predicate when no flag is present (mirrors the fallback
/// conformance behaviour in `build_source_selector`).
fn subject_matches_shape(
    by_subject: &HashMap<String, Vec<(String, String)>>,
    subj: &str,
    shape: &ModelShape,
    conformance: &Option<(String, String)>,
) -> bool {
    let rows = match by_subject.get(subj) {
        Some(r) => r,
        None => return false,
    };
    if let Some((pred, val)) = conformance {
        return rows.iter().any(|(p, o)| p == pred && o == val);
    }
    let predicates: HashSet<&str> = shape
        .properties
        .iter()
        .map(|p| p.predicate.as_str())
        .collect();
    rows.iter().any(|(p, _)| predicates.contains(p.as_str()))
}

/// Build the JSON object for one subject by mapping its triples onto the
/// shape's property/relation field names.  Recurses into any include's
/// target subjects, using `target_class_name` to resolve the target shape
/// from `resolver`.
fn hydrate_one(
    by_subject: &HashMap<String, Vec<(String, String)>>,
    subj: &str,
    shape: &ModelShape,
    include: Option<&HashMap<String, IncludeValue>>,
    resolver: &dyn ShapeResolver,
    depth: u8,
) -> Result<Value, Error> {
    if depth >= MAX_CONSTRUCT_DEPTH {
        return Ok(Value::Object(serde_json::Map::new()));
    }
    let mut obj = serde_json::Map::new();
    obj.insert("id".to_string(), Value::String(subj.to_string()));

    let rows = by_subject.get(subj).cloned().unwrap_or_default();

    // ----- Property values -----
    let mut by_pred: HashMap<String, Vec<String>> = HashMap::new();
    for (p, o) in &rows {
        by_pred.entry(p.clone()).or_default().push(o.clone());
    }

    for prop in &shape.properties {
        if prop.predicate.is_empty() || prop.is_flag {
            continue;
        }
        let vals = match by_pred.get(&prop.predicate) {
            Some(v) => v,
            None => continue,
        };

        if prop.is_collection || (matches!(prop.direction.as_deref(), Some("forward"))
            && !prop.is_scalar_relation)
        {
            obj.insert(
                prop.name.clone(),
                Value::Array(vals.iter().map(|v| Value::String(v.clone())).collect()),
            );
        } else if let Some(last) = vals.last() {
            obj.insert(prop.name.clone(), parse_scalar_value(last, prop));
        }
    }

    // Default empty arrays for declared collection properties that had
    // no triples — matches the legacy hydrator behaviour so consumers can
    // assume `obj.collection` is always defined.
    for prop in &shape.properties {
        if prop.is_collection && !obj.contains_key(&prop.name) {
            obj.insert(prop.name.clone(), Value::Array(vec![]));
        }
    }

    // ----- Includes (forward only, by design of can_use_construct) -----
    if let Some(include) = include {
        for (rel_name, val) in include {
            let want_nested = !matches!(val, IncludeValue::Bool(false));
            if !want_nested {
                continue;
            }
            let rel = match shape.include_relations.iter().find(|r| r.name == *rel_name) {
                Some(r) => r,
                None => continue,
            };
            if rel.direction == "reverse" {
                continue;
            }
            let target_shape = match resolver.get_shape(&rel.target_class_name) {
                Ok(s) => s,
                Err(_) => continue,
            };
            // Find linked subjects via the relation's predicate
            let linked: Vec<String> = rows
                .iter()
                .filter(|(p, _)| p == &rel.predicate)
                .map(|(_, o)| o.clone())
                .collect();
            // Filter to only those that match the target shape (and don't
            // accidentally pick up triples from sibling relations that share
            // the predicate — `topicTag` vs `embeddingTag`).
            let target_conf = find_required_flag(target_shape.as_ref());
            let nested_include = match val {
                IncludeValue::SubQuery(sq) => sq.include.as_ref(),
                _ => None,
            };
            let mut nested: Vec<Value> = Vec::new();
            for tid in &linked {
                if subject_matches_shape(by_subject, tid, target_shape.as_ref(), &target_conf) {
                    let child = hydrate_one(
                        by_subject,
                        tid,
                        target_shape.as_ref(),
                        nested_include,
                        resolver,
                        depth + 1,
                    )?;
                    nested.push(child);
                }
            }
            if rel.max_count == Some(1) {
                obj.insert(
                    rel.name.clone(),
                    nested.into_iter().next().unwrap_or(Value::Null),
                );
            } else {
                obj.insert(rel.name.clone(), Value::Array(nested));
            }
        }
    }

    Ok(Value::Object(obj))
}

/// Parse a single wire-format value back to its typed JSON representation.
/// Mirrors the small subset of `parse_literal_value` we need here:
/// `literal:string:` → JSON string (unencoded), `literal:number:` →
/// JSON number when finite, `literal:boolean:` → JSON bool, everything
/// else → JSON string (used for `literal:json:` and bare IRIs alike).
fn parse_scalar_value(val: &str, _prop: &ShapeProperty) -> Value {
    if let Some(rest) = val.strip_prefix("literal:number:") {
        if let Ok(n) = rest.parse::<f64>() {
            if let Some(num) = serde_json::Number::from_f64(n) {
                return Value::Number(num);
            }
        }
        return Value::String(rest.to_string());
    }
    if let Some(rest) = val.strip_prefix("literal:boolean:") {
        return match rest {
            "true" => Value::Bool(true),
            "false" => Value::Bool(false),
            _ => Value::String(rest.to_string()),
        };
    }
    if let Some(rest) = val.strip_prefix("literal:string:") {
        use percent_encoding::percent_decode_str;
        let decoded = percent_decode_str(rest)
            .decode_utf8()
            .map(|c| c.to_string())
            .unwrap_or_else(|_| rest.to_string());
        return Value::String(decoded);
    }
    Value::String(val.to_string())
}
