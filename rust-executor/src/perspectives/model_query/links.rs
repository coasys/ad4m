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
//! shaped as a [`DecoratedLinkExpression`](crate::types::DecoratedLinkExpression)
//! (author, timestamp, data, proof with the stored signature verdict, status) so a
//! consumer can deserialize it directly, gate on the verdict, or verify the
//! signature itself.
//!
//! For a collection that is per-item provenance (#1046 §6, #1115):
//! `links: ["members"]` gives each member its own author, timestamp and
//! verdict next to the plain `members` array, which stays as it is.
//!
//! How the rows are read depends on the predicate. A `links` entry that
//! names a predicate the instance query already fetches (a property of the
//! shape) is served from the hydration rows themselves: the instance query
//! then also selects the reifier's provenance columns, and [`attach_links`]
//! decorates them instead of re-reading. That is one snapshot, not two — a
//! scalar's `__links` rows are the same links its value was picked from, so
//! a consumer reading "the author of the link this value came from" cannot
//! get another link's author, and it costs no second query.
//!
//! Everything else — an annotation predicate the shape does not declare, or
//! a getter-backed collection the instance query does not fetch — is one
//! extra query over the requested predicates, run after hydration and
//! restricted to the returned page by a sub-SELECT on the instance ids
//! (`utils::values_subselect`: a bare `VALUES` next to the triple pattern
//! joined inside the for-loop and was re-iterated per link row; the
//! sub-SELECT drives seeks for a small page and hash-joins one predicate
//! scan for a large one).
//!
//! The price of that read: for those keys `__links` is a second read, not
//! the same snapshot as the instance. A member added or removed between the
//! two can be in the hydrated collection without a row here, or the reverse.
//! A consumer dating members by their own link (#1112, #1103) must treat
//! "in the collection, no row in `__links`" as an error, never fall back to
//! `createdAt`: that is the earlier date, so for role eligibility it widens
//! the window.

use deno_core::anyhow::{anyhow, Error};
use serde::de::DeserializeOwned;
use serde_json::{json, Map, Value};
use std::collections::{BTreeMap, HashMap, HashSet};

use super::sparql_builder::{
    instance_link_predicates, link_status_filter, local_status_filter, proof_valid_filter,
};
use super::types::{IncludeValue, InstanceLinks, ModelQueryInput, ModelShape, ShapeResolver};
use super::utils::{emittable_iri, values_subselect};
use crate::perspectives::sparql_store::{status_from_str, EvalStop, SparqlStore};
use crate::types::LinkStatus;

/// Instance key carrying the requested per-link rows. Reserved as a property
/// name in `shacl_parser`, so a class cannot declare a property it would
/// overwrite.
pub(crate) const LINKS_KEY: &str = "__links";

/// The rows of one requested `links` key on one result instance, each read as
/// a `T`. A key missing from [`LINKS_KEY`], or a row that is not a `T`, is an
/// `Err`, never an empty list: read as "no links", a missing `acceptedBy` or
/// grant key would silently drop every vote or grant.
pub(crate) fn links_rows<T: DeserializeOwned>(
    instance: &Value,
    key: &str,
) -> Result<Vec<T>, Error> {
    instance
        .get(LINKS_KEY)
        .and_then(|links| links.get(key))
        .and_then(Value::as_array)
        .ok_or_else(|| anyhow!("no `{LINKS_KEY}.{key}` rows"))?
        .iter()
        .map(|row| {
            serde_json::from_value(row.clone())
                .map_err(|e| anyhow!("a `{key}` row is not a link ({e}): {row}"))
        })
        .collect()
}

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

/// The predicates of `keys` the hydration rows answer: those the instance
/// query fetches. An empty predicate list means it fetches every predicate.
fn served_predicates<'a>(shape: &ModelShape, keys: &'a [(String, String)]) -> HashSet<&'a str> {
    let needed = instance_link_predicates(shape);
    if needed.is_empty() {
        return keys.iter().map(|(_, p)| p.as_str()).collect();
    }
    let needed: HashSet<&str> = needed.into_iter().collect();
    keys.iter()
        .filter(|(_, p)| needed.contains(p.as_str()))
        .map(|(_, p)| p.as_str())
        .collect()
}

/// Whether the instance query must carry the `__links` provenance columns:
/// any `links` key that is served from the hydration rows needs them (see
/// [`attach_links`]). `keys` is the already-resolved `links` request.
pub(super) fn hydration_carries_links(shape: &ModelShape, keys: &[(String, String)]) -> bool {
    !served_predicates(shape, keys).is_empty()
}

/// Attach [`LINKS_KEY`] to every instance in `instances`.
///
/// Every requested key is present on every instance, as an empty array when the
/// instance has no such link — "asked, none found" must read differently from
/// "not asked". Rows are ordered by timestamp, then target (the same string
/// order `createdAt` and collection hydration use), then author and signature
/// so the order is total — see [`sort_link_rows`].
///
/// Keys whose predicate the instance query fetches are served from
/// `hydrated` — the same rows hydration folded, so on a timestamp tie the
/// last `__links` row is the link the scalar's value came from
/// ([`InstanceLinkRow::rank`](super::types::InstanceLinkRow::rank) is the
/// order both pick by) — and no second query runs for them. `hydrated` is
/// `Some` only when the instance query selected the provenance columns,
/// which `hydration_carries_links` decides. Keys on any other predicate are
/// read with one query over just those predicates, restricted to the page.
///
/// `local: true` properties get the same `LinkStatus::Local` restriction the
/// instance query applies, so a gossiped Shared link on a local predicate is not
/// reintroduced through this side door. The query's `linkStatus` (#1116)
/// applies here too, so a Shared-only read lists only Shared links.
///
/// Links whose signature did not verify are withheld by default, the same
/// [`proof_valid_filter`] the instance query applies (#1113), so a forged
/// annotation or member link is not reintroduced here either. With
/// `include_unverified` they are returned, and the consumer gates on the
/// verdict below or verifies `proof` itself.
///
/// Each row's `proof` carries the store's signature verdict as `valid` /
/// `invalid` (#1115), recorded at insert time from the signature itself, so a
/// row deserializes as a [`DecoratedLinkExpression`](crate::types::DecoratedLinkExpression)
/// — the shape `perspective.get` returns — and a consumer can gate on the
/// verdict without redoing the crypto. Only a stored `"true"` reads as valid;
/// a missing annotation reads `valid: false`, never "unsigned but valid". So
/// under the default filter every row reads `valid: true`; only
/// `include_unverified` surfaces `valid: false` rows. A link stored without a
/// proof comes back as `{"key": "", "signature": "", "valid": false,
/// "invalid": true}`: `LinkExpression::compute_proof_valid` returns `false`
/// for it.
///
/// Each row carries the link's `status` (`"SHARED"` / `"LOCAL"`), so a row is
/// the whole [`LinkExpression`](crate::types::LinkExpression) that
/// `perspective.get` returns (`LinkRow.status` in the SDK), not one missing a
/// field. It is not signed: it says where this executor holds the link, not who
/// wrote it.
///
/// `data.target` is the target the link was signed over (the store's
/// `wireTarget` annotation when a literal was written in another encoding
/// than the store's canonical one), and the verdict was computed over those
/// same bytes, so the verdict and a consumer's own re-verification agree.
///
/// Not viewer-scoped: like the rest of `model_query` on `dev`, this read does
/// not take a `viewer_did`. When #1058 threads `viewer_author_filter` through
/// `model_query`, this query needs it too, after `{local_status}`. It accepts
/// arbitrary predicate IRIs, so it is the widest read to leave unscoped.
#[allow(clippy::too_many_arguments)]
pub(super) async fn attach_links(
    stop: &EvalStop,
    store: &SparqlStore,
    shape: &ModelShape,
    keys: &[(String, String)],
    link_status: Option<&LinkStatus>,
    include_unverified: Option<bool>,
    hydrated: Option<&[InstanceLinks]>,
    instances: &mut [Value],
) -> Result<(), Error> {
    if keys.is_empty() || instances.is_empty() {
        return Ok(());
    }
    let mut ids: Vec<String> = instances
        .iter()
        .filter_map(|i| i["id"].as_str().map(str::to_string))
        .collect();
    ids.sort();
    ids.dedup();

    // source -> predicate -> rows
    let mut found: BTreeMap<String, BTreeMap<String, Vec<Value>>> = BTreeMap::new();

    // The predicates a `links` request is answered for out of the hydration
    // rows — `None` when the instance query did not carry the provenance
    // columns, which is whenever no key is served this way.
    let served: Option<HashSet<&str>> = hydrated.map(|_| served_predicates(shape, keys));
    if let (Some(hydrated), Some(served)) = (hydrated, served.as_ref()) {
        let by_source: HashMap<&str, &InstanceLinks> =
            hydrated.iter().map(|i| (i.source.as_str(), i)).collect();
        for id in &ids {
            let Some(inst) = by_source.get(id.as_str()) else {
                continue;
            };
            for row in &inst.links {
                let (Some(provenance), true) = (
                    row.provenance.as_ref(),
                    served.contains(row.predicate.as_str()),
                ) else {
                    continue;
                };
                let mut link = decorated_link(&json!({
                    "source": inst.source,
                    "predicate": row.predicate,
                    "target": row.target,
                    "wireTarget": provenance.wire_target,
                    "author": row.author,
                    "timestamp": row.timestamp,
                    "proofKey": provenance.proof_key,
                    "proofSig": provenance.proof_signature,
                    "proofValid": provenance.proof_valid,
                }));
                if let Some(status) = status_from_str(&provenance.status) {
                    link["status"] = serde_json::to_value(status)?;
                }
                found
                    .entry(id.clone())
                    .or_default()
                    .entry(row.predicate.clone())
                    .or_default()
                    .push(link);
            }
        }
    }

    let mut predicates: Vec<&str> = keys
        .iter()
        .map(|(_, p)| p.as_str())
        .filter(|p| served.as_ref().is_none_or(|s| !s.contains(p)))
        .collect();
    predicates.sort();
    predicates.dedup();

    if !ids.is_empty() && !predicates.is_empty() {
        // The page ids constrain `?source` through a sub-SELECT
        // (`values_subselect`), not a bare `VALUES ?source { ... }` and not a
        // predicate scan filtered here. Beside `?source ?predicate ?target` a
        // bare VALUES lands inside the for-loop join and is re-iterated once
        // per link row (~70 s for a 50k-id page on a 50k-link predicate); a
        // predicate-only scan pays the whole predicate for a one-instance
        // page (~600 ms here). The sub-SELECT keeps the ids out of the loop:
        // they drive index seeks when the page is small and hash-join a
        // single predicate scan when it is large.
        let predicate_values = predicates
            .iter()
            .map(|p| format!("<{p}>"))
            .collect::<Vec<_>>()
            .join(" ");
        let source_constraint = values_subselect(
            "source",
            &ids,
            &format!("VALUES ?predicate {{ {predicate_values} }} ?source ?predicate ?target ."),
        );
        let local_status = local_status_filter(shape);
        let link_status = link_status_filter(link_status);
        let proof_valid = proof_valid_filter(include_unverified);
        // Required patterns before the first OPTIONAL: oxigraph does not reorder joins inside
        // an OPTIONAL chain that a required pattern follows, so they run as cartesian products.
        let sparql = format!(
            r#"SELECT ?source ?predicate ?target ?wireTarget ?author ?timestamp ?proofKey ?proofSig ?proofValid ?rowStatus WHERE {{
    {source_constraint}
    VALUES ?predicate {{ {predicate_values} }}
    ?source ?predicate ?target .
    ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
{link_status}{proof_valid}    OPTIONAL {{ ?_reifier <ad4m://ontology/proofKey> ?proofKey . }}
    OPTIONAL {{ ?_reifier <ad4m://ontology/proofSignature> ?proofSig . }}
    OPTIONAL {{ ?_reifier <ad4m://ontology/proofValid> ?proofValid . }}
    OPTIONAL {{ ?_reifier <ad4m://ontology/wireTarget> ?wireTarget . }}
    OPTIONAL {{ ?_reifier <ad4m://ontology/status> ?rowStatus . }}
{local_status}}}"#
        );
        let rows: Vec<Value> = serde_json::from_str(&store.query_async(stop, &sparql).await?)?;
        for row in &rows {
            let s = |var: &str| row[var].as_str().unwrap_or("").to_string();
            let source = s("source");
            let predicate = s("predicate");
            let mut link = decorated_link(row);
            // Spelled as `perspective.get` spells it (`"SHARED"` / `"LOCAL"`),
            // and absent when the store recorded none, like `status` there.
            // `decorated_link` does not set it: this query is the one that
            // selects `rowStatus`.
            if let Some(status) = status_from_str(&s("rowStatus")) {
                link["status"] = serde_json::to_value(status)?;
            }
            found
                .entry(source)
                .or_default()
                .entry(predicate)
                .or_default()
                .push(link);
        }
    }
    // The order is total (see `sort_link_rows`) however the rows arrived —
    // queried or served from hydration, whose own order is the store's.
    for by_predicate in found.values_mut() {
        for rows in by_predicate.values_mut() {
            sort_link_rows(rows);
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

/// One `__links` row from a result row binding `source`, `predicate`,
/// `target`, `wireTarget`, `author`, `timestamp`, `proofKey`, `proofSig` and
/// `proofValid`, shaped as a `DecoratedLinkExpression`.
pub(super) fn decorated_link(row: &Value) -> Value {
    let s = |var: &str| row[var].as_str().unwrap_or("").to_string();
    // Same decoding as `sparql_store::decode_proof_valid`: only a
    // stored "true" is valid; an absent annotation is not. As in
    // `VerifiedExpression::from`, `invalid` is `!valid`, so it covers
    // a failed signature, a missing verdict and a missing proof alike;
    // an empty `signature` is what tells an unsigned link apart.
    let valid = s("proofValid") == "true";
    json!({
        "author": s("author"),
        "timestamp": s("timestamp"),
        "data": {
            "source": s("source"),
            "predicate": s("predicate"),
            // The signed bytes when the store keeps them apart
            // from its canonical literal rendering.
            "target": match s("wireTarget") {
                w if w.is_empty() => s("target"),
                w => w,
            },
        },
        "proof": {
            "key": s("proofKey"),
            "signature": s("proofSig"),
            "valid": valid,
            "invalid": !valid,
        },
    })
}

/// Order one predicate's rows by timestamp, then target (the signed bytes,
/// as `data.target` reports them), then author and signature — the order
/// [`InstanceLinkRow::rank`](super::types::InstanceLinkRow::rank) ranks the
/// same links in, so `__links`' last row is the link hydration's scalar
/// pick came from. The last two make the order total: two agents can write
/// the same triple in the same millisecond, and the store returns those rows
/// in no fixed order. Anything hashing `__links` (an evidence seal, a
/// receipt) needs every replica to produce the same array, and
/// `canonical_json` sorts keys, not arrays.
pub(super) fn sort_link_rows(rows: &mut [Value]) {
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

    /// A `__links` read whose page is large: the whole-transcript gather
    /// asks for every in-scope instance's text link at once. Restricting the
    /// read with `VALUES ?source { ... }` joined as a nested loop — one scan
    /// of the predicate per id — and a 20k-id page took ~10 s; scanning the
    /// predicate once and filtering the sources on the rows is ~200 ms.
    #[tokio::test]
    async fn links_for_a_large_page_stay_fast() {
        use super::super::test_helpers::{prop, shape};
        use crate::agent::signatures::TestSigner;
        use crate::types::{Link, LinkExpression};

        let signer = TestSigner::generate();
        let store = SparqlStore::new(None).unwrap();
        for i in 0..20_000 {
            let e = signer.sign(
                Link {
                    source: format!("we://s{i}"),
                    predicate: Some("we://name".into()),
                    target: format!("literal:string:v{i}"),
                }
                .normalize(),
            );
            store
                .add_link(&LinkExpression {
                    author: e.author,
                    timestamp: e.timestamp,
                    data: e.data,
                    proof: e.proof,
                    status: Some(LinkStatus::Shared),
                })
                .unwrap();
        }
        let shape = shape("Thing", vec![prop("name", "we://name")]);
        let keys: Vec<(String, String)> = vec![("name".into(), "we://name".into())];
        let mut instances: Vec<Value> = (0..20_000)
            .map(|i| json!({ "id": format!("we://s{i}") }))
            .collect();
        let started = std::time::Instant::now();
        attach_links(
            &crate::perspectives::sparql_store::EvalStop::none(),
            &store,
            &shape,
            &keys,
            None,
            None,
            None,
            &mut instances,
        )
        .await
        .unwrap();
        let elapsed = started.elapsed();
        assert!(
            elapsed < std::time::Duration::from_secs(5),
            "__links for a 20k-instance page took {elapsed:?}"
        );
        let row = &instances[17][LINKS_KEY]["name"][0];
        assert_eq!(row["data"]["target"], "literal:string:v17");
        assert_eq!(row["author"], signer.did.as_str());
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

    /// A `__links` row is documented as a signed link a consumer can verify
    /// itself, so its target must be the signed bytes, not the store's
    /// canonical rendering of a literal written in another encoding.
    #[tokio::test]
    async fn a_links_row_carries_the_signed_target() {
        use super::super::test_helpers::execute_model_query_from_json;
        use crate::agent::signatures::TestSigner;
        use crate::types::{Link, LinkExpression, LinkStatus};

        let signer = TestSigner::generate();
        let store = SparqlStore::new(None).unwrap();
        let sign = |predicate: &str, target: &str| {
            let e = signer.sign(Link {
                source: "we://i".into(),
                predicate: Some(predicate.into()),
                target: target.into(),
            });
            LinkExpression {
                author: e.author,
                timestamp: e.timestamp,
                data: e.data,
                proof: e.proof,
                status: Some(LinkStatus::Shared),
            }
        };
        let raw = "literal:string:Write the guide";
        store.add_link(&sign("ad4m://type", "we://Note")).unwrap();
        store.add_link(&sign("we://name", raw)).unwrap();

        let shape = r#"{
            "className": "Note",
            "properties": {
                "type": { "predicate": "ad4m://type", "required": true, "flag": true,
                          "initial": "we://Note" },
                "name": { "predicate": "we://name", "required": false }
            }
        }"#;
        let result = execute_model_query_from_json(
            &store,
            "Note",
            &ModelQueryInput {
                links: Some(vec!["name".into()]),
                ..Default::default()
            },
            shape,
        )
        .await
        .unwrap();
        let rows = result.instances[0][LINKS_KEY]["name"].as_array().unwrap();
        assert_eq!(rows.len(), 1);
        let row = &rows[0];
        assert_eq!(row["data"]["target"], raw);
        assert_eq!(row["proof"]["valid"], true, "the stored verdict");
        assert_eq!(row["proof"]["invalid"], false);
        let link = LinkExpression {
            author: row["author"].as_str().unwrap().into(),
            timestamp: row["timestamp"].as_str().unwrap().into(),
            data: serde_json::from_value(row["data"].clone()).unwrap(),
            proof: serde_json::from_value(row["proof"].clone()).unwrap(),
            status: None,
        };
        assert!(link.compute_proof_valid(), "the row verifies as returned");
    }

    /// The `__links` read for one instance stays an indexed lookup on a large
    /// perspective. With its required reifier patterns after the `OPTIONAL`s,
    /// oxigraph joined the triple and reifier patterns without keys, as a
    /// cartesian product: seconds on a few thousand links, which pushed the
    /// flow engine's proposal reads past the 30 s RPC limit. Each required
    /// pattern after the `OPTIONAL`s (the default `proofValid` cut, a
    /// `linkStatus`) triggered it on its own, so both are checked.
    #[tokio::test]
    async fn links_for_one_instance_stay_fast_on_a_large_perspective() {
        use super::super::test_helpers::{prop, shape};
        use crate::agent::signatures::TestSigner;
        use crate::types::{Link, LinkExpression};

        // 400 instances with 10 links each over 40 distinct predicates, written
        // through the normal add-link path (one reifier and its annotations per
        // link).
        let signer = TestSigner::generate();
        let store = SparqlStore::new(None).unwrap();
        for i in 0..4000 {
            let e = signer.sign(
                Link {
                    source: format!("we://s{}", i / 10),
                    predicate: Some(format!("we://p{}", i % 40)),
                    target: format!("we://v{i}"),
                }
                .normalize(),
            );
            store
                .add_link(&LinkExpression {
                    author: e.author,
                    timestamp: e.timestamp,
                    data: e.data,
                    proof: e.proof,
                    status: Some(LinkStatus::Shared),
                })
                .unwrap();
        }

        let shape = shape("Thing", vec![prop("name", "we://name")]);
        // we://s7 holds links 70..=79, on predicates p30..=p39.
        let keys: Vec<(String, String)> = ["we://p31", "we://p35", "we://p0"]
            .iter()
            .map(|p| (p.to_string(), p.to_string()))
            .collect();
        for (status, include_unverified) in [(None, None), (Some(LinkStatus::Shared), Some(true))] {
            let mut instances = vec![json!({ "id": "we://s7" })];
            let started = std::time::Instant::now();
            attach_links(
                &crate::perspectives::sparql_store::EvalStop::none(),
                &store,
                &shape,
                &keys,
                status.as_ref(),
                include_unverified,
                None,
                &mut instances,
            )
            .await
            .unwrap();
            let elapsed = started.elapsed();
            assert!(
                elapsed < std::time::Duration::from_secs(1),
                "__links for one instance took {elapsed:?} (status {status:?})"
            );

            let links = &instances[0][LINKS_KEY];
            let targets = |key: &str| -> Vec<String> {
                links[key]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|r| r["data"]["target"].as_str().unwrap().to_string())
                    .collect()
            };
            assert_eq!(targets("we://p31"), vec!["we://v71"], "{links}");
            assert_eq!(targets("we://p35"), vec!["we://v75"], "{links}");
            assert!(targets("we://p0").is_empty(), "{links}");
            let row = &links["we://p31"][0];
            assert_eq!(row["data"]["source"], "we://s7");
            assert_eq!(row["author"], signer.did.as_str());
            assert_eq!(row["proof"]["valid"], true);
            assert_eq!(row["status"], "SHARED");
        }
    }

    /// Measures the `__links` read's SPARQL shapes against a 50k-link
    /// predicate, for a page of 1, 100 and 50,000 instance ids:
    ///
    /// - `values`: the pre-#1795 form, `VALUES ?source` + `VALUES ?predicate`
    ///   before the triple pattern;
    /// - `scan`: the interim form, `VALUES ?predicate` alone, sources filtered
    ///   in Rust (adds ~0 ms to the number printed);
    /// - `values+filter-pred`: `VALUES ?source` beside the pattern, the
    ///   predicate set as a `FILTER(?predicate IN (...))`;
    /// - `const-pred`: `VALUES ?source` with the predicate a constant;
    /// - `filter-in`: the scan plus a `FILTER(?source IN (...))`;
    /// - `subselect`: `VALUES ?source` inside a sub-SELECT — the shipped form
    ///   (`values_subselect`). `attach-links` below runs the real function.
    ///
    /// On this store (oxigraph, in-memory): `values`, `values+filter-pred`
    /// and `const-pred` are quadratic through the for-loop join (~68 s at a
    /// 50k page), `scan` pays the whole predicate even for a 1-id page
    /// (~600 ms), `filter-in` matches ids row-wise (~15 s at 50k), `subselect`
    /// is ~1 ms at 1, ~30 ms at 100 and ~600 ms at 50,000. Runs ~4 minutes:
    /// the quadratic references are kept in the table on purpose.
    ///
    /// `cargo test --release --offline -p ad4m-executor links_page_cost -- --ignored --nocapture`
    #[tokio::test]
    #[ignore]
    async fn links_page_cost() {
        use super::super::sparql_builder::{local_status_filter, proof_valid_filter};
        use super::super::test_helpers::{prop, shape};
        use crate::agent::signatures::TestSigner;
        use crate::types::{Link, LinkExpression, LinkStatus};

        const N: usize = 50_000;
        let signer = TestSigner::generate();
        let store = SparqlStore::new(None).unwrap();
        let started = std::time::Instant::now();
        for i in 0..N {
            let e = signer.sign(
                Link {
                    source: format!("we://s{i}"),
                    predicate: Some("we://name".into()),
                    target: format!("literal:string:v{i}"),
                }
                .normalize(),
            );
            store
                .add_link(&LinkExpression {
                    author: e.author,
                    timestamp: e.timestamp,
                    data: e.data,
                    proof: e.proof,
                    status: Some(LinkStatus::Shared),
                })
                .unwrap();
        }
        println!("seeded {N} links in {:?}", started.elapsed());

        let shape = shape("Thing", vec![prop("name", "we://name")]);
        let local_status = local_status_filter(&shape);
        let proof_valid = proof_valid_filter(None);
        let tail = |triple: &str, term: &str| {
            format!(
                r#"SELECT ?source ?target ?wireTarget ?author ?timestamp ?proofKey ?proofSig ?proofValid ?rowStatus WHERE {{
{triple}    ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( {term} )>> .
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
{proof_valid}    OPTIONAL {{ ?_reifier <ad4m://ontology/proofKey> ?proofKey . }}
    OPTIONAL {{ ?_reifier <ad4m://ontology/proofSignature> ?proofSig . }}
    OPTIONAL {{ ?_reifier <ad4m://ontology/proofValid> ?proofValid . }}
    OPTIONAL {{ ?_reifier <ad4m://ontology/wireTarget> ?wireTarget . }}
    OPTIONAL {{ ?_reifier <ad4m://ontology/status> ?rowStatus . }}
{local_status}}}"#
            )
        };

        let stop = crate::perspectives::sparql_store::EvalStop::none();
        for page in [1usize, 100, 50_000] {
            let ids: Vec<String> = (0..page).map(|i| format!("we://s{i}")).collect();
            let values_src = format!(
                "VALUES ?source {{ {} }}",
                ids.iter()
                    .map(|i| format!("<{i}>"))
                    .collect::<Vec<_>>()
                    .join(" ")
            );
            let filter_in = format!(
                "FILTER(?source IN ({}))",
                ids.iter()
                    .map(|i| format!("<{i}>"))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            let variants: Vec<(&str, String)> = vec![
                (
                    "values",
                    tail(
                        &format!(
                            "    {values_src}\n    VALUES ?predicate {{ <we://name> }}\n    ?source ?predicate ?target .\n"
                        ),
                        "?source ?predicate ?target",
                    ),
                ),
                (
                    "scan",
                    tail(
                        "    VALUES ?predicate { <we://name> }\n    ?source ?predicate ?target .\n",
                        "?source ?predicate ?target",
                    ),
                ),
                (
                    "values+filter-pred",
                    tail(
                        &format!(
                            "    {values_src}\n    ?source ?predicate ?target .\n    FILTER(?predicate IN (<we://name>))\n"
                        ),
                        "?source ?predicate ?target",
                    ),
                ),
                (
                    "const-pred",
                    tail(
                        &format!("    {values_src}\n    ?source <we://name> ?target .\n"),
                        "?source <we://name> ?target",
                    ),
                ),
                (
                    "filter-in",
                    tail(
                        &format!(
                            "    VALUES ?predicate {{ <we://name> }}\n    ?source ?predicate ?target .\n    {filter_in}\n"
                        ),
                        "?source ?predicate ?target",
                    ),
                ),
                (
                    "subselect",
                    tail(
                        &format!(
                            "    {{ SELECT ?source WHERE {{ {values_src} }} }}\n    VALUES ?predicate {{ <we://name> }}\n    ?source ?predicate ?target .\n"
                        ),
                        "?source ?predicate ?target",
                    ),
                ),
            ];
            for (name, sparql) in variants {
                let t = std::time::Instant::now();
                let rows: Vec<Value> =
                    serde_json::from_str(&store.query_async(&stop, &sparql).await.unwrap())
                        .unwrap();
                println!(
                    "page {page:>6} {name:>18}: {:>8?} ({} rows)",
                    t.elapsed(),
                    rows.len()
                );
            }
            let keys: Vec<(String, String)> = vec![("name".into(), "we://name".into())];
            let mut instances: Vec<Value> = ids.iter().map(|id| json!({ "id": id })).collect();
            let t = std::time::Instant::now();
            attach_links(
                &stop,
                &store,
                &shape,
                &keys,
                None,
                None,
                None,
                &mut instances,
            )
            .await
            .unwrap();
            println!("page {page:>6} {:>18}: {:>8?}", "attach-links", t.elapsed());
        }
    }
}
