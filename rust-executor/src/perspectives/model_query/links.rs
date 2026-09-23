//! Per-link provenance on request: the `links` query option.
//!
//! Hydration folds an instance's links into one flat object — last write wins
//! for a scalar, one array of targets for a collection, and `createdAt` /
//! `updatedAt` / `author` computed across *every* link. Two questions do not
//! survive that fold (#1046 §3, §4):
//!
//! - **Annotation links.** A link on the instance whose predicate the shape does
//!   not declare is never fetched (the instance query binds `VALUES ?predicate`
//!   to the shape's predicates) and would be ignored by hydration anyway. A
//!   role-revocation tombstone (`ad4m://flow/role_grant_revoked`) is exactly
//!   such a link, so a class-layer read could not see a revocation — the
//!   fail-open direction for role gating.
//! - **When one link was written.** `createdAt` is the instance's earliest link.
//!   A member added to an existing role instance must be dated from its own
//!   `instance --didProperty--> did` link, not from the instance.
//!
//! `links: ["owner", "ad4m://flow/role_grant_revoked"]` answers both. Each entry
//! is a property or relation **name** the shape declares, or an absolute
//! predicate IRI. The result gains one key, [`LINKS_KEY`], mapping each
//! requested entry — spelled as requested — to every link on that predicate,
//! shaped as a [`LinkExpression`](crate::types::LinkExpression) (author,
//! timestamp, data, proof) so a consumer can deserialize it directly and verify
//! the signature itself.
//!
//! The read is one extra query over the page that is actually returned, run
//! after hydration. That placement is deliberate: the instance query and its
//! hydration are left byte-for-byte as they were, so an annotation link cannot
//! move `updatedAt`, `author` or a property value, and both instance-query plans
//! (single and two-phase) are covered by the same code instead of by two copies
//! of a predicate list. A query that does not ask for `links` issues no extra
//! query and returns exactly the JSON it returned before.

use deno_core::anyhow::{anyhow, Error};
use serde_json::{json, Map, Value};
use std::collections::BTreeMap;

use super::sparql_builder::local_status_filter;
use super::types::ModelShape;
use super::utils::{emittable_iri, values_or_str_filter};
use crate::perspectives::sparql_store::SparqlStore;

/// Instance key carrying the requested per-link rows. Reserved as a property
/// name in `shacl_parser`, so a class cannot declare a property it would
/// overwrite.
pub(crate) const LINKS_KEY: &str = "__links";

/// Resolve each requested entry to the predicate it reads, as
/// `(requested spelling, predicate)`.
///
/// A property or relation name wins over the IRI reading, mirroring how the
/// flow engine resolves `didProperty`. An entry that is neither is an error,
/// not an empty list: a caller that misspells `role_grant_revoked` must not be
/// told there are no revocations. A reverse relation (`belongsToOne` /
/// `belongsToMany`) is an error for the same reason: its links are incoming,
/// and reading outgoing ones would always find none.
pub(super) fn resolve_link_keys(
    shape: &ModelShape,
    requested: &[String],
) -> Result<Vec<(String, String)>, Error> {
    let mut out: Vec<(String, String)> = Vec::new();
    for key in requested {
        if out.iter().any(|(k, _)| k == key) {
            continue;
        }
        // (predicate, is_reverse)
        let by_name = shape
            .properties
            .iter()
            .find(|p| p.name == *key && !p.predicate.is_empty())
            .map(|p| {
                (
                    p.predicate.clone(),
                    p.direction.as_deref() == Some("reverse"),
                )
            })
            .or_else(|| {
                shape
                    .include_relations
                    .iter()
                    .find(|r| r.name == *key && !r.predicate.is_empty())
                    .map(|r| (r.predicate.clone(), r.direction == "reverse"))
            });
        let predicate = match by_name {
            // A reverse relation's links point at the instance
            // (`other --predicate--> this`); `attach_links` reads outgoing
            // links only, so resolving it would answer `[]` for a question
            // the store was never asked.
            Some((_, true)) => {
                return Err(anyhow!(
                    "links: `{key}` is a reverse relation of class `{}`; its links point at the \
                     instance and `links` reads outgoing links only. Ask for it on the class at \
                     the other end",
                    shape.target_class
                ))
            }
            Some((p, false)) => p,
            None if emittable_iri(key) => key.clone(),
            None => {
                return Err(anyhow!(
                    "links: `{key}` is neither a property or relation of class `{}` nor an \
                     absolute predicate IRI",
                    shape.target_class
                ))
            }
        };
        if !emittable_iri(&predicate) {
            return Err(anyhow!(
                "links: `{key}` resolves to predicate `{predicate}`, which is not an IRI the \
                 store can be queried with"
            ));
        }
        out.push((key.clone(), predicate));
    }
    Ok(out)
}

/// Attach [`LINKS_KEY`] to every instance in `instances`.
///
/// Every requested key is present on every instance, as an empty array when the
/// instance has no such link — "asked, none found" must read differently from
/// "not asked". Rows are ordered by timestamp, then target, the same string
/// order `createdAt` and collection hydration use.
///
/// `local: true` properties get the same `LinkStatus::Local` restriction the
/// instance query applies, so a gossiped Shared link on a local predicate is not
/// reintroduced through this side door.
///
/// A link stored without a proof comes back as `"proof": {"key": "",
/// "signature": ""}`. That is *not* "unsigned but valid":
/// `LinkExpression::compute_proof_valid` returns `false` for it, and so must
/// any verdict layered on these rows.
pub(super) async fn attach_links(
    store: &SparqlStore,
    shape: &ModelShape,
    keys: &[(String, String)],
    instances: &mut [Value],
) -> Result<(), Error> {
    if keys.is_empty() || instances.is_empty() {
        return Ok(());
    }
    let ids: Vec<String> = instances
        .iter()
        .filter_map(|i| i["id"].as_str().map(str::to_string))
        .collect();

    let mut predicates: Vec<&str> = keys.iter().map(|(_, p)| p.as_str()).collect();
    predicates.sort();
    predicates.dedup();

    // source -> predicate -> rows
    let mut found: BTreeMap<String, BTreeMap<String, Vec<Value>>> = BTreeMap::new();
    if !ids.is_empty() {
        let source_constraint = values_or_str_filter("source", &ids);
        let predicate_values = predicates
            .iter()
            .map(|p| format!("<{p}>"))
            .collect::<Vec<_>>()
            .join(" ");
        let local_status = local_status_filter(shape);
        let sparql = format!(
            r#"SELECT ?source ?predicate ?target ?author ?timestamp ?proofKey ?proofSig WHERE {{
    {source_constraint}
    VALUES ?predicate {{ {predicate_values} }}
    ?source ?predicate ?target .
    ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
    OPTIONAL {{ ?_reifier <ad4m://ontology/proofKey> ?proofKey . }}
    OPTIONAL {{ ?_reifier <ad4m://ontology/proofSignature> ?proofSig . }}
{local_status}}}"#
        );
        let rows: Vec<Value> = serde_json::from_str(&store.query_async(&sparql).await?)?;
        let s = |row: &Value, var: &str| row[var].as_str().unwrap_or("").to_string();
        for row in &rows {
            let source = s(row, "source");
            let predicate = s(row, "predicate");
            let link = json!({
                "author": s(row, "author"),
                "timestamp": s(row, "timestamp"),
                "data": {
                    "source": source,
                    "predicate": predicate,
                    "target": s(row, "target"),
                },
                "proof": {
                    "key": s(row, "proofKey"),
                    "signature": s(row, "proofSig"),
                },
            });
            found
                .entry(source)
                .or_default()
                .entry(predicate)
                .or_default()
                .push(link);
        }
        for by_predicate in found.values_mut() {
            for rows in by_predicate.values_mut() {
                rows.sort_by(|a, b| {
                    let key = |v: &Value| {
                        (
                            v["timestamp"].as_str().unwrap_or("").to_string(),
                            v["data"]["target"].as_str().unwrap_or("").to_string(),
                        )
                    };
                    key(a).cmp(&key(b))
                });
            }
        }
    }

    for inst in instances.iter_mut() {
        let id = inst["id"].as_str().unwrap_or("").to_string();
        let mut out = Map::new();
        for (key, predicate) in keys {
            let rows = found
                .get(&id)
                .and_then(|p| p.get(predicate))
                .cloned()
                .unwrap_or_default();
            out.insert(key.clone(), Value::Array(rows));
        }
        if let Value::Object(obj) = inst {
            obj.insert(LINKS_KEY.to_string(), Value::Object(out));
        }
    }
    Ok(())
}
