use super::*;
use crate::perspectives::interpretation::types::{
    ExistingInstances, ExistingLinks, InstanceContext,
};
use crate::perspectives::model_query::types::{ModelShape, ParentScope};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use std::collections::{BTreeMap, HashMap, HashSet};

/// minimal transcript gatherer. Reads links `source ⇒ predicate ⇒ literal`
/// from a perspective where `predicate` matches `message_predicate` and the
/// target is a `literal:string:` URI (i.e., a message body). Returns turns in
/// the order the store returned them. Speaker is the link author.
///
/// Kept intentionally small — flows/channel-aware traversal is deferred to a
/// later PR. Callers that already have a curated `Vec<(speaker, text)>` should
/// pass it straight to [`run_interpretation`] and skip this helper.
pub async fn gather_transcript(
    perspective: &PerspectiveInstance,
    source: &str,
    message_predicate: &str,
) -> anyhow::Result<Vec<(String, String)>> {
    use crate::types::LinkQuery;
    let query = LinkQuery {
        source: Some(source.to_string()),
        predicate: Some(message_predicate.to_string()),
        ..Default::default()
    };
    let links = perspective
        .get_links(&query)
        .await
        .map_err(|e| anyhow::anyhow!("gather_transcript: get_links failed: {e:#}"))?;
    let mut out = Vec::with_capacity(links.len());
    for l in links {
        if let Some(body) = decode_literal_string(&l.data.target) {
            out.push((l.author, body));
        }
    }
    Ok(out)
}

/// Decode a `literal:` URI to a plain `String`, but only when it holds a
/// string value. Reuses `ad4m-client`'s `Literal` (the canonical
/// parse/serialize type); non-string literals (number/bool/json) and
/// non-literal URIs yield `None`.
pub(crate) fn decode_literal_string(uri: &str) -> Option<String> {
    use ad4m_client::literal::{Literal, LiteralValue};
    match Literal::from_url(uri.to_string()).ok()?.get().ok()? {
        LiteralValue::String(s) => Some(s),
        _ => None,
    }
}

/// Assemble an interpretation transcript from an arbitrary SPARQL SELECT
/// against the perspective. The query MUST bind `?speaker` and `?text` in
/// its solutions — each row becomes a `(speaker, text)` turn in the order
/// the store returned them. `?text` may be either a plain string literal
/// (returned as-is) or a `literal:string:...` URI (decoded via
/// [`decode_literal_string`]); rows that decode to non-string literals or
/// bind neither cleanly are skipped rather than failing the whole gather.
///
/// This is the generic counterpart to [`gather_transcript`]: a caller supplies
/// an arbitrary scope query (the AutoProcessor reads one off its config; tests
/// pass one directly) so interpretation can run over just the relevant subset
/// of the perspective instead of the full channel. Callers that just want the
/// flat "all messages on this source under this predicate" view should keep
/// using the source+predicate wrapper.
pub async fn gather_transcript_sparql(
    perspective: &PerspectiveInstance,
    sparql: &str,
) -> anyhow::Result<Vec<(String, String)>> {
    let rows_json = perspective
        .sparql_query(sparql.to_string())
        .map_err(|e| anyhow::anyhow!("gather_transcript_sparql: SPARQL query failed: {e:#}"))?;
    let rows: Vec<serde_json::Value> = serde_json::from_str(&rows_json)
        .map_err(|e| anyhow::anyhow!("gather_transcript_sparql: bad SPARQL result JSON: {e:#}"))?;

    let mut out = Vec::with_capacity(rows.len());
    for row in rows {
        let Some(speaker) = row.get("speaker").and_then(|v| v.as_str()) else {
            continue;
        };
        let Some(raw_text) = row.get("text").and_then(|v| v.as_str()) else {
            continue;
        };
        let text = if raw_text.starts_with("literal:") {
            match decode_literal_string(raw_text) {
                Some(s) => s,
                None => continue, // non-string literal — not a message body
            }
        } else {
            raw_text.to_string()
        };
        out.push((speaker.to_string(), text));
    }
    Ok(out)
}

/// read the instances already present in the perspective for each target class
/// into the id-keyed [`ExistingInstances`] map. Each [`InstanceContext`] carries
/// the instance's `id` (base URI) alongside its declared identity value and
/// secondary scalars.
///
/// This one map serves both halves of the interpretation contract; consumers
/// project what they need from it (see [`instances_by_class`] /
/// [`identity_values_by_class`]):
///   * the prompt ([`build_interpretation_input`]) shows `id` + identity so the
///     model can steer away from re-proposing known items *and* emit an `id` to
///     upsert one, or reference it as a relation target;
///   * the identity values feed the deterministic dedup safety net
///     ([`filter_already_present`]), and the id key-set bounds which existing-id
///     relation refs are accepted ([`plan_interpretation_ops_with_context`]).
///
/// The dedup/display key is whichever property the class declares as its
/// `identity` (via [`identity_property`]), not a hard-coded `title`. Classes
/// with no identity property are skipped entirely — no identity ⇒ no dedup, and
/// nothing meaningful to show the model.
///
/// Instances are read through the model-query API (`PerspectiveInstance::
/// model_query`) — the symmetric counterpart to writing them via
/// `create_subject` — so class conformance and field decoding go through the
/// class's own shape/getters rather than hand-matched type-flag links.
///
/// A per-class `model_query` failure is propagated. Silently treating it as
/// "no existing instances" would break [`filter_already_present`]'s deterministic
/// dedup guarantee: an empty `known` set for the failing class lets the LLM's
/// re-proposal of an existing item slip through and mint a duplicate subject.
/// Callers can exclude affected classes upstream (via `interpretation_classes`)
/// if they want a soft-skip.
pub async fn existing_instance_context(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
    scope: Option<&ParentScope>,
) -> anyhow::Result<ExistingInstances> {
    let mut out: ExistingInstances = HashMap::new();
    for shape in shapes {
        // No declared identity property ⇒ no dedup key ⇒ skip.
        let Some(idp) = identity_property(shape) else {
            continue;
        };
        let idp_name = idp.name.clone();
        let class = class_local_name(&shape.target_class);

        // Secondary scalars: everything the LLM can meaningfully see about an
        // existing instance beyond its identity — excluding the class type
        // flag (setter-managed) and every relation (link-typed, hydrated
        // separately and not useful in the compact `existing` prompt view).
        // Kept as `Vec<String>` (not a set) so property order is stable across
        // calls, feeding a deterministic prompt.
        let rel_preds = relation_predicates(shape);
        let scalar_names: Vec<String> = shape
            .properties
            .iter()
            .filter(|p| {
                p.name != idp_name && !p.is_flag && !rel_preds.contains(p.predicate.as_str())
            })
            .map(|p| p.name.clone())
            .collect();

        let mut requested = Vec::with_capacity(1 + scalar_names.len());
        requested.push(idp_name.clone());
        requested.extend(scalar_names.iter().cloned());
        // Build the model-query as structured JSON so an optional `scope`
        // (a `ParentScope`) can be spliced in as the query's `parent` filter —
        // constraining the existing-instance set to a sub-graph/tree rather
        // than every instance of the class in the perspective. `None` keeps the
        // pre-scope behaviour (all instances). The actual scope is supplied at
        // runtime by the caller (the AutoProcessor wires it in #885); here we
        // only thread the plumbing and exercise it in tests.
        let mut query_obj = serde_json::json!({ "properties": requested });
        if let Some(scope) = scope {
            query_obj["parent"] = serde_json::to_value(scope).map_err(|e| {
                anyhow::anyhow!("existing_instance_context: serialize parent scope: {e:#}")
            })?;
        }
        let query = query_obj.to_string();
        let result_json = perspective.model_query(class, &query).await.map_err(|e| {
            anyhow::anyhow!(
                "existing_instance_context: model_query({class}) failed — refusing to \
                 proceed because an empty existing-set here would silently break dedup: {e:#}"
            )
        })?;
        let result: serde_json::Value = serde_json::from_str(&result_json).map_err(|e| {
            anyhow::anyhow!("existing_instance_context: bad model_query result for {class}: {e:#}")
        })?;

        let rows: Vec<InstanceContext> = result
            .get("instances")
            .and_then(|v| v.as_array())
            .map(|rows| {
                rows.iter()
                    .filter_map(|inst| {
                        // `id` is the instance's base URI, injected by the
                        // model-query hydration for every row.
                        let id = inst.get("id").and_then(|v| v.as_str())?;
                        let title = inst.get(&idp_name).and_then(|v| v.as_str())?;
                        let mut properties: BTreeMap<String, String> = BTreeMap::new();
                        for name in &scalar_names {
                            // Present only when hydrated to a non-empty
                            // string. Numeric/bool scalars are stringified so
                            // the prompt stays JSON-object-of-strings for the
                            // LLM (matches how the model already renders
                            // interpreted field values on the way in).
                            let v = inst.get(name);
                            let rendered = match v {
                                Some(serde_json::Value::String(s)) if !s.is_empty() => {
                                    Some(s.clone())
                                }
                                Some(serde_json::Value::Number(n)) => Some(n.to_string()),
                                Some(serde_json::Value::Bool(b)) => Some(b.to_string()),
                                _ => None,
                            };
                            if let Some(rendered) = rendered {
                                properties.insert(name.clone(), rendered);
                            }
                        }
                        Some(InstanceContext {
                            id: id.to_string(),
                            title: title.to_string(),
                            class: class.to_string(),
                            properties,
                        })
                    })
                    .collect()
            })
            .unwrap_or_default();

        for row in rows {
            out.entry(row.id.clone()).or_default().push(row);
        }
    }
    Ok(out)
}

/// Read the relation edges already present in the graph for the given shapes'
/// **forward** relations, as canonical `(source, predicate, target)` triples.
///
/// Feeds the planner's idempotency guard ([`ExistingLinks`] threaded into
/// [`plan_interpretation_ops_resolved`]): a repeated continuous interpretation
/// pass must not re-emit a relation link that already exists, which would grow a
/// duplicate edge and — because a stored link's reifier IRI hashes in its
/// timestamp — a duplicate reifier node too (James #883 #4).
///
/// Only forward relations are read: reverse relations (`belongsTo*`) store their
/// edge on the other class and the planner does not emit them yet (Phase 3).
/// Each distinct predicate is queried once even when several shapes share it. A
/// query failure propagates — an empty set would silently defeat the guard and
/// let duplicates back in.
pub async fn existing_relation_links(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
) -> anyhow::Result<ExistingLinks> {
    use crate::types::LinkQuery;
    let mut out: ExistingLinks = HashSet::new();
    let mut queried: HashSet<String> = HashSet::new();
    for shape in shapes {
        for rel in &shape.include_relations {
            if rel.direction != "forward" {
                continue;
            }
            if !queried.insert(rel.predicate.clone()) {
                continue;
            }
            let links = perspective
                .get_links(&LinkQuery {
                    predicate: Some(rel.predicate.clone()),
                    ..Default::default()
                })
                .await
                .map_err(|e| {
                    anyhow::anyhow!(
                        "existing_relation_links: get_links({}) failed: {e:#}",
                        rel.predicate
                    )
                })?;
            for l in links {
                if let Some(pred) = &l.data.predicate {
                    out.insert((l.data.source.clone(), pred.clone(), l.data.target.clone()));
                }
            }
        }
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::perspectives::interpretation_test_support::*;

    #[tokio::test]
    async fn existing_instance_context_reads_id_and_identity() {
        // The context snapshot the prompt + relation resolver rely on: one row per
        // persisted instance, carrying the base URI as `id` and the class's declared
        // identity value as `title`.
        let (mut perspective, shapes, ctx) = setup_perspective_no_llm(&[("Task", TASK_SDNA)]).await;
        seed_instance(
            &mut perspective,
            &ctx,
            &shapes[0],
            "soa://existing/task/1",
            "Migrate the SHACL parser",
        )
        .await;

        let ctx_map = existing_instance_context(&perspective, &shapes, None)
            .await
            .expect("existing_instance_context");
        assert_eq!(ctx_map.len(), 1, "one seeded instance; got {ctx_map:#?}");
        // Id-keyed single source: look the instance up by its base URI.
        let entries = ctx_map
            .get("soa://existing/task/1")
            .expect("instance keyed by its id");
        assert_eq!(entries.len(), 1);
        let inst = &entries[0];
        assert_eq!(inst.id, "soa://existing/task/1");
        assert_eq!(inst.title, "Migrate the SHACL parser");
        assert_eq!(inst.class, "Task");
        assert!(
            inst.properties.is_empty(),
            "task with only identity set must carry no secondary scalars; got {:?}",
            inst.properties
        );

        // The projections consumers derive from this one map: per-class identity
        // values (dedup net) and the id key-set (relation resolver).
        assert_eq!(
            identity_values_by_class(&ctx_map).get("Task"),
            Some(&vec!["Migrate the SHACL parser".to_string()])
        );
        assert!(ctx_map.contains_key("soa://existing/task/1"));
    }

    #[tokio::test]
    async fn existing_instance_context_scope_constrains_to_subtree() {
        // Plumbing for tree-scoped extraction (#883): the existing-instance snapshot
        // that feeds dedup must be constrainable to a sub-graph, not every instance
        // of the class in the perspective. The runner (#885) supplies the scope; here
        // we prove the parameter actually narrows the set. Two Tasks, each linked
        // under a different parent node.
        use crate::perspectives::model_query::types::ParentScope;
        use crate::types::{Link, LinkStatus};

        let (mut perspective, shapes, ctx) = setup_perspective_no_llm(&[("Task", TASK_SDNA)]).await;
        seed_instance(
            &mut perspective,
            &ctx,
            &shapes[0],
            "soa://tree-a/task/1",
            "Task under tree A",
        )
        .await;
        seed_instance(
            &mut perspective,
            &ctx,
            &shapes[0],
            "soa://tree-b/task/1",
            "Task under tree B",
        )
        .await;
        // Parent edges: <parent> ns://contains <task-base>.
        for (parent, task) in [
            ("soa://parent/a", "soa://tree-a/task/1"),
            ("soa://parent/b", "soa://tree-b/task/1"),
        ] {
            perspective
                .add_link(
                    Link {
                        source: parent.into(),
                        predicate: Some("ns://contains".into()),
                        target: task.into(),
                    },
                    LinkStatus::Local,
                    None,
                    &ctx,
                )
                .await
                .expect("parent link");
        }

        // Unscoped: both instances are visible (pre-scope behaviour).
        let all = existing_instance_context(&perspective, &shapes, None)
            .await
            .expect("unscoped context");
        assert_eq!(
            all.len(),
            2,
            "unscoped context must see both tasks; got {all:#?}"
        );

        // Scoped to parent A: only the task under tree A survives.
        let scope = ParentScope::Raw {
            id: "soa://parent/a".into(),
            predicate: "ns://contains".into(),
        };
        let scoped = existing_instance_context(&perspective, &shapes, Some(&scope))
            .await
            .expect("scoped context");
        assert_eq!(
            scoped.len(),
            1,
            "parent-scoped context must exclude tree B; got {scoped:#?}"
        );
        let entries = scoped
            .get("soa://tree-a/task/1")
            .expect("tree-A task present");
        assert_eq!(entries[0].id, "soa://tree-a/task/1");
        assert_eq!(entries[0].title, "Task under tree A");
    }

    #[tokio::test]
    async fn existing_instance_context_populates_secondary_scalars() {
        // With the design-fork (i) enrichment, `existing_instance_context` must
        // return each instance's currently-set non-identity scalar values in the
        // `properties` map, so the prompt can show the LLM the full state of an
        // existing entry (its rolling summary etc.) — not just the identity
        // label. This is what unblocks the topic-shift discrimination on smaller
        // local models.
        let (mut perspective, shapes, ctx) =
            setup_perspective_no_llm(&[("ConversationSubgroup", CONVERSATION_SUBGROUP_SDNA)]).await;
        seed_instance_with_props(
        &mut perspective,
        &ctx,
        &shapes[0],
        "soa://existing/subgroup/payments",
        serde_json::json!({
            "name": "Payments infrastructure",
            "summary": "The team discussed dropped webhook retries during a recent payments outage."
        }),
    )
    .await;

        let ctx_map = existing_instance_context(&perspective, &shapes, None)
            .await
            .expect("existing_instance_context");
        assert_eq!(ctx_map.len(), 1, "one seeded instance; got {ctx_map:#?}");
        let entries = ctx_map
            .get("soa://existing/subgroup/payments")
            .expect("subgroup keyed by its id");
        let inst = &entries[0];
        assert_eq!(inst.title, "Payments infrastructure");
        assert_eq!(
            inst.properties.get("summary").map(String::as_str),
            Some("The team discussed dropped webhook retries during a recent payments outage."),
            "summary scalar must be populated; got {:?}",
            inst.properties
        );
        assert!(!inst.properties.contains_key("name"));
        assert!(!inst.properties.contains_key("type"));
    }
}
