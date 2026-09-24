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
//!
//! The price of that placement: `__links` is a second read, not the same
//! snapshot as the instance. A member added or removed between the two can be
//! in the hydrated collection without a row here, or the reverse. A consumer
//! dating members by their own link (#1112, #1103) must treat "in the
//! collection, no row in `__links`" as an error, never fall back to
//! `createdAt`: that is the earlier date, so for role eligibility it widens
//! the window.

use deno_core::anyhow::{anyhow, Error};
use serde_json::{json, Map, Value};
use std::collections::BTreeMap;

use super::sparql_builder::local_status_filter;
use super::types::{IncludeValue, ModelQueryInput, ModelShape, ShapeResolver};
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
/// "not asked". Rows are ordered by timestamp, then target (the same string
/// order `createdAt` and collection hydration use), then author and signature
/// so the order is total — see [`sort_link_rows`].
///
/// `local: true` properties get the same `LinkStatus::Local` restriction the
/// instance query applies, so a gossiped Shared link on a local predicate is not
/// reintroduced through this side door.
///
/// A link stored without a proof comes back as `"proof": {"key": "",
/// "signature": ""}`. That is *not* "unsigned but valid":
/// `LinkExpression::compute_proof_valid` returns `false` for it, and so must
/// any verdict layered on these rows.
///
/// Not viewer-scoped: like the rest of `model_query` on `dev`, this read does
/// not take a `viewer_did`. When #1058 threads `viewer_author_filter` through
/// `model_query`, this query needs it too, after `{local_status}`. It accepts
/// arbitrary predicate IRIs, so it is the widest read to leave unscoped.
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
                sort_link_rows(rows);
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

/// Order one predicate's rows by timestamp, then target, author and
/// signature. The last two make the order total: two agents can write the same
/// triple in the same millisecond, and the store returns those rows in no fixed
/// order. Anything hashing `__links` (an evidence seal, a receipt) needs every
/// replica to produce the same array, and `canonical_json` sorts keys, not
/// arrays.
fn sort_link_rows(rows: &mut [Value]) {
    rows.sort_by_cached_key(|v| {
        let s = |x: &Value| x.as_str().unwrap_or("").to_string();
        (
            s(&v["timestamp"]),
            s(&v["data"]["target"]),
            s(&v["author"]),
            s(&v["proof"]["signature"]),
        )
    });
}

/// Every predicate a `links` request reads, at every `include` depth, for a
/// subscription's trigger set.
///
/// The trigger set is otherwise built from the shape's predicates, and an IRI
/// entry is by definition one the shape does not declare. Without this, a
/// subscription asking for a revocation tombstone would never re-run when the
/// tombstone lands and would keep reporting `[]`: "not revoked", fail-open.
///
/// Entries are resolved the way [`resolve_link_keys`] resolves them, against
/// the class the (sub-)query reads; an entry that does not resolve is left out
/// here, since the query itself rejects it. An include whose target class
/// cannot be resolved (a polymorphic relation that declares none) still
/// contributes its IRI entries.
pub(crate) fn links_trigger_predicates(
    shape: &ModelShape,
    query: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
) -> Vec<String> {
    let mut out = Vec::new();
    collect_links_predicates(Some(shape), query, resolver, &mut out);
    out.sort();
    out.dedup();
    out
}

fn collect_links_predicates(
    shape: Option<&ModelShape>,
    query: &ModelQueryInput,
    resolver: &dyn ShapeResolver,
    out: &mut Vec<String>,
) {
    for key in query.links.iter().flatten() {
        let by_name = shape.and_then(|s| {
            s.properties
                .iter()
                .find(|p| p.name == *key && !p.predicate.is_empty())
                .map(|p| p.predicate.clone())
                .or_else(|| {
                    s.include_relations
                        .iter()
                        .find(|r| r.name == *key && !r.predicate.is_empty())
                        .map(|r| r.predicate.clone())
                })
        });
        match by_name {
            Some(p) => out.push(p),
            None if emittable_iri(key) => out.push(key.clone()),
            None => {}
        }
    }
    for (name, value) in query.include.iter().flatten() {
        let IncludeValue::SubQuery(sub) = value else {
            continue;
        };
        let target = shape
            .and_then(|s| s.include_relations.iter().find(|r| r.name == *name))
            .filter(|r| !r.target_class_name.is_empty())
            .and_then(|r| resolver.get_shape(&r.target_class_name).ok());
        collect_links_predicates(target.as_deref(), sub, resolver, out);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn row(author: &str, timestamp: &str, target: &str, signature: &str) -> Value {
        json!({
            "author": author,
            "timestamp": timestamp,
            "data": { "source": "we://i", "predicate": "we://p", "target": target },
            "proof": { "key": "k", "signature": signature },
        })
    }

    /// `links` names resolve against the class each (sub-)query reads, at every
    /// include depth; IRI entries are taken as they are; an entry that is
    /// neither contributes nothing (the query itself rejects it).
    #[test]
    fn links_trigger_predicates_walk_every_include_depth() {
        use super::super::test_helpers::{prop, relation, shape, StaticShapeResolver};
        use super::super::types::ShapeRelation;
        use std::collections::HashMap;

        let rel = |name: &str, predicate: &str, target: &str| ShapeRelation {
            name: name.into(),
            predicate: predicate.into(),
            direction: "forward".into(),
            kind: "hasMany".into(),
            max_count: None,
            target_class_name: target.into(),
            target_class_uri: String::new(),
        };
        let mut top = shape("Top", vec![relation("mids", "we://mids")]);
        top.include_relations = vec![rel("mids", "we://mids", "Mid")];
        let mut mid = shape("Mid", vec![relation("leaves", "we://leaves")]);
        mid.include_relations = vec![rel("leaves", "we://leaves", "Leaf")];
        let leaf = shape("Leaf", vec![prop("note", "we://leaf_note")]);
        let resolver = StaticShapeResolver::new();
        resolver.register("Mid", mid);
        resolver.register("Leaf", leaf);

        let sub = |links: &[&str], include: Option<(&str, ModelQueryInput)>| ModelQueryInput {
            links: Some(links.iter().map(|s| s.to_string()).collect()),
            include: include.map(|(k, q)| {
                HashMap::from([(k.to_string(), IncludeValue::SubQuery(Box::new(q)))])
            }),
            ..Default::default()
        };
        let query = sub(
            &["we://top_tombstone"],
            Some((
                "mids",
                sub(
                    &["we://mid_tombstone", "not-a-key"],
                    Some(("leaves", sub(&["note"], None))),
                ),
            )),
        );
        assert_eq!(
            links_trigger_predicates(&top, &query, &resolver),
            vec!["we://leaf_note", "we://mid_tombstone", "we://top_tombstone"]
        );
    }

    /// Two agents write the same triple in the same millisecond. The store
    /// returns the two rows in whatever order it holds them, so a sort that
    /// stops at `(timestamp, target)` hands two replicas the same rows in
    /// different orders -- and anything hashing `__links` (an evidence seal, a
    /// receipt) then disagrees between them. The order must be total.
    #[test]
    fn row_order_is_total_and_independent_of_store_order() {
        let t = "2026-09-23T10:00:00.000Z";
        let rows = vec![
            row("did:key:b", t, "we://x", "s2"),
            row("did:key:a", t, "we://x", "s3"),
            row("did:key:a", t, "we://x", "s1"),
        ];
        let mut forward = rows.clone();
        let mut reversed: Vec<Value> = rows.into_iter().rev().collect();
        sort_link_rows(&mut forward);
        sort_link_rows(&mut reversed);
        assert_eq!(forward, reversed, "row order depends on store order");
        let order: Vec<(&str, &str)> = forward
            .iter()
            .map(|r| {
                (
                    r["author"].as_str().unwrap(),
                    r["proof"]["signature"].as_str().unwrap(),
                )
            })
            .collect();
        assert_eq!(
            order,
            vec![
                ("did:key:a", "s1"),
                ("did:key:a", "s3"),
                ("did:key:b", "s2")
            ]
        );
    }
}
