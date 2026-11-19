use super::*;
use crate::perspectives::interpretation::types::{
    ExistingInstances, ExistingLinks, InstanceContext, TranscriptTurn,
};
use crate::perspectives::model_query::types::{ModelShape, Scope};
use crate::perspectives::model_query::ModelQueryInput;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use std::collections::{BTreeMap, HashMap, HashSet};

/// minimal transcript gatherer. Reads links `source ⇒ predicate ⇒ literal`
/// from a perspective where `predicate` matches `message_predicate` and the
/// target is a `literal:string:` URI (i.e., a message body). Returns turns in
/// the order the store returned them. Speaker is the link author.
///
/// Kept intentionally small — flows/channel-aware traversal is deferred to a
/// later PR. Callers that already have a curated [`TranscriptTurn`] list should
/// pass it straight to [`run_interpretation`] and skip this helper.
pub async fn gather_transcript(
    perspective: &PerspectiveInstance,
    source: &str,
    message_predicate: &str,
) -> anyhow::Result<Vec<TranscriptTurn>> {
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
        if let Some(body) = decode_transcript_body(&l.data.target) {
            out.push(TranscriptTurn {
                speaker: l.author,
                text: body,
                timestamp: l.timestamp,
            });
        }
    }
    Ok(out)
}

/// Decode a transcript body from a `literal:` URI, resolving an **expression
/// envelope** as well as a bare string.
///
/// A property declared with `resolveLanguage` — which `@Property` sets by
/// default, so *every* ORM-declared body has it — is not stored as
/// `literal:string:…`. Setting it creates an expression in the Literal
/// language, so the stored target is a signed envelope:
///
/// ```json
/// literal:json:{"author":"did:key:…","timestamp":"…","data":"the words","proof":{…}}
/// ```
///
/// A decoder that only accepts `LiteralValue::String` answers `None` for
/// that (it is `LiteralValue::Json`, not `String`), which made both gathers
/// silently skip every row and report an empty transcript —
/// indistinguishable from a conversation with nothing in it. The fixtures
/// write `ns://body` as a bare literal, so the path had only ever been
/// exercised against text no ORM model produces.
///
/// So: a string literal is the body; a JSON envelope carrying a string `data` is
/// the body inside it; anything else is still not a message body.
pub(crate) fn decode_transcript_body(uri: &str) -> Option<String> {
    use ad4m_client::literal::{Literal, LiteralValue};
    match Literal::from_url(uri.to_string()).ok()?.get().ok()? {
        LiteralValue::String(s) => Some(s),
        // An expression envelope. `data` is the payload the author signed; the
        // envelope's own `author`/`timestamp` are richer than the link
        // reifier's and a caller could prefer them, but reading the body is
        // what unblocks the gather.
        LiteralValue::Json(v) => v
            .get("data")
            .and_then(|d| d.as_str())
            .map(|s| s.to_string()),
        _ => None,
    }
}

/// The transcript read: the turns, beside the model-query instances the
/// watch loop feeds to its wake trigger.
pub struct GatheredTranscript {
    /// The model-query instances as returned — the trigger's `near`/`via`
    /// rules are defined over the nodes of the last read, so an edit to an
    /// in-scope instance's links wakes it.
    pub instances: Vec<serde_json::Value>,
    /// Turns whose required text resolved.
    pub turns: Vec<TranscriptTurn>,
    /// At least one in-scope instance's `text_property` failed its
    /// `resolveLanguage` fetch on this read: such a turn is dropped — the
    /// expression URL is not the words — but it is not consumed either. An
    /// expression arriving is not a store write, so the generation does not
    /// move and a caller keeping reads by generation must not treat this
    /// one as final.
    pub has_unresolved: bool,
}

/// Assemble an interpretation transcript as a model query over the
/// processor's message class (#1794). One hydrated instance is one turn:
/// `text` is the `text_property` value as hydration decoded it
/// (language-resolved, literal-decoded), and `speaker`/`timestamp` are the
/// **text link's** — read off its `__links` row — not the instance's, whose
/// `author` is the earliest link's and would attribute an edit written by
/// another signer to the instance's creator. Ordering by the text link's
/// timestamp places an edit where its words were written, not where the
/// instance was created. Reading through the model query is the point: only
/// shape-conforming instances become turns, and links whose signature does
/// not verify never hydrate (#1113, default provenance — `includeUnverified`
/// is never set here).
///
/// `query_json` is the restricted scope merged with the fields a turn needs
/// — see `AutoProcessorConfig::transcript_query_json`; `text_property` is
/// added to `links` so the selected link's provenance comes back with it.
pub async fn gather_transcript_model(
    perspective: &PerspectiveInstance,
    class: &str,
    text_property: &str,
    query_json: &str,
) -> anyhow::Result<GatheredTranscript> {
    gather_transcript_model_within(
        perspective,
        class,
        text_property,
        query_json,
        crate::perspectives::perspective_instance::MODEL_QUERY_SHAPE_WAIT,
    )
    .await
}

/// [`gather_transcript_model`] with the shape-wait budget as a parameter:
/// the auto-processor tick passes `Duration::ZERO`, as a subscription
/// recheck does — a missing shape fails the read, and the write that could
/// deliver it wakes the loop.
pub(crate) async fn gather_transcript_model_within(
    perspective: &PerspectiveInstance,
    class: &str,
    text_property: &str,
    query_json: &str,
    shape_wait: std::time::Duration,
) -> anyhow::Result<GatheredTranscript> {
    // The names the query's `where` reads on an instance's row: a row that
    // holds one of them in `__unresolved` was kept by `matches_where` on a
    // verdict it could not see, so its turn must be held like an
    // unresolved text — not built on an undecided condition (#1795).
    let where_names = where_property_names(query_json);
    let query_json = with_text_provenance(query_json, text_property)?;
    let result = perspective
        .model_query_one_result_within(class, &query_json, shape_wait)
        .await
        .map_err(|e| anyhow::anyhow!("gather_transcript_model: model_query failed: {e:#}"))?;

    let mut out = Vec::new();
    let mut has_unresolved = false;
    for instance in &result.instances {
        // The resolver marks a `resolveLanguage` value it could not fetch:
        // the raw expression URL sits where the text should be, so there is
        // no turn to take — and none to retire, since the same target may
        // resolve once the language serves it. A `where`-referenced
        // property counts the same: the row was kept on a condition the
        // read could not decide.
        let held = |names: &Vec<serde_json::Value>| {
            names.iter().any(|n| {
                n.as_str()
                    .is_some_and(|name| name == text_property || where_names.contains(name))
            })
        };
        if instance
            .get(crate::perspectives::model_query::UNRESOLVED_KEY)
            .and_then(|v| v.as_array())
            .is_some_and(&held)
        {
            has_unresolved = true;
            continue;
        }
        let Some(text) = scalar_text(instance.get(text_property)) else {
            continue;
        };
        // Rows are ranked the way hydration's scalar pick ranks (see
        // `attach_links`) and — for the shape's own predicates, which the
        // text property always is — served from the same hydration rows, so
        // the last is the link whose target became the text: one read, one
        // rank, no window for a newer write to split text from speaker.
        // Skipping a scalar with no row stays the safe direction, never
        // fall back to the instance's author.
        let Some(link) = instance
            .get(crate::perspectives::model_query::LINKS_KEY)
            .and_then(|links| links.get(text_property))
            .and_then(|rows| rows.as_array())
            .and_then(|rows| rows.last())
        else {
            continue;
        };
        let (Some(speaker), Some(timestamp)) = (
            link.get("author").and_then(|v| v.as_str()),
            link.get("timestamp").and_then(|v| v.as_str()),
        ) else {
            continue;
        };
        out.push(TranscriptTurn {
            speaker: speaker.to_string(),
            text,
            timestamp: timestamp.to_string(),
        });
    }
    Ok(GatheredTranscript {
        instances: result.instances,
        turns: out,
        has_unresolved,
    })
}

/// Add `text_property` to the query's `links` selection: the per-link
/// provenance (`__links`) a turn's speaker and timestamp are read from. The
/// restricted scope cannot name `links` (`validate_source_query`), so the
/// key is always absent or already ours.
fn with_text_provenance(query_json: &str, text_property: &str) -> anyhow::Result<String> {
    let mut query: serde_json::Value = serde_json::from_str(query_json)
        .map_err(|e| anyhow::anyhow!("gather_transcript_model: bad query JSON: {e:#}"))?;
    let obj = query
        .as_object_mut()
        .ok_or_else(|| anyhow::anyhow!("gather_transcript_model: query must be an object"))?;
    let entry = obj
        .entry("links".to_string())
        .or_insert_with(|| serde_json::Value::Array(Vec::new()));
    if let serde_json::Value::Array(keys) = entry {
        if !keys.iter().any(|k| k.as_str() == Some(text_property)) {
            keys.push(serde_json::Value::String(text_property.to_string()));
        }
    }
    Ok(query.to_string())
}

/// The instance properties the query's `where` reads (see
/// [`crate::perspectives::model_query::where_property_names`]). An
/// unparsable query yields none: the model query refuses it anyway.
fn where_property_names(query_json: &str) -> HashSet<String> {
    match serde_json::from_str::<ModelQueryInput>(query_json) {
        Ok(ModelQueryInput {
            where_clause: Some(clause),
            ..
        }) => crate::perspectives::model_query::where_property_names(&clause),
        _ => HashSet::new(),
    }
}

/// The turn text off a hydrated scalar (`parse_literal_value` output): a
/// string as-is, a number or bool stringified. Anything else — an absent
/// property, a node target, an array — is not a message body.
fn scalar_text(value: Option<&serde_json::Value>) -> Option<String> {
    match value? {
        serde_json::Value::String(s) => Some(s.clone()),
        serde_json::Value::Number(n) => Some(n.to_string()),
        serde_json::Value::Bool(b) => Some(b.to_string()),
        _ => None,
    }
}

/// Build a DID → display-name map for every speaker DID in `turns`.
///
/// Resolution order:
/// 1. Multi-user db — looks up `username` by `did` in the `users` table.
/// 2. Fallback — the raw DID string (no entry added to the map; callers treat
///    a missing key as "use the DID as-is").
///
/// Exactly one lookup per **unique** DID, resolved or not: the common
/// multi-agent transcript is the one where *no* speaker is in the users
/// table (agent DIDs aren't registered users), so memoising only successes
/// would pay a lock + query per *turn* in precisely the case #1006 is about.
///
/// A stored username that is empty (or whitespace) counts as unresolved —
/// rendering an empty speaker label would be strictly worse for extraction
/// than the raw DID it replaces.
///
/// The map is built once per interpretation pass and threaded into
/// [`build_interpretation_input`], which renders the display name and keeps
/// the raw DID alongside it only where disambiguation needs it (see there).
pub fn build_speaker_name_map(
    turns: &[TranscriptTurn],
) -> std::collections::HashMap<String, String> {
    let unique_dids: std::collections::HashSet<&str> =
        turns.iter().map(|t| t.speaker.as_str()).collect();
    let mut map = std::collections::HashMap::new();
    for did in unique_dids {
        let resolved = crate::db_backend::db_backend()
            .get_username_by_did(did)
            .ok()
            .flatten();
        if let Some(name) = resolved {
            if !name.trim().is_empty() {
                map.insert(did.to_string(), name);
            }
        }
    }
    map
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
    scope: Option<&Scope>,
) -> anyhow::Result<ExistingInstances> {
    let mut out: ExistingInstances = HashMap::new();
    for shape in shapes {
        // No declared identity property ⇒ no dedup key ⇒ skip.
        let Some(idp) = identity_property(shape) else {
            continue;
        };
        let idp_name = idp.name.clone();
        // Disambiguated class identifier (full URI on a cross-namespace local-name
        // clash) — the same key the prompt and routing use, and a valid class name
        // for `model_query` (`load_shape` resolves a full URI by exact match).
        let class = class_label(&shape.target_class, shapes);

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
        // (a `Scope`) can be spliced in as the query's `parent` filter —
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
        let result_json = perspective.model_query(&class, &query).await.map_err(|e| {
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
                            class: class.clone(),
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

    /// A body stored by a property with `resolveLanguage` — which `@Property` sets by default — is a
    /// signed expression envelope, not a bare string. Reading only the bare form made both gathers
    /// skip every row and report an empty transcript, which is indistinguishable from a conversation
    /// with nothing in it: no error, no warning, no event.
    #[test]
    fn transcript_body_reads_an_expression_envelope() {
        use ad4m_client::literal::Literal;

        // What `@Optional` (no resolveLanguage) stores.
        let bare = Literal::from_string("the words".to_string())
            .to_url()
            .unwrap();
        assert_eq!(decode_transcript_body(&bare).as_deref(), Some("the words"));

        // What `@Property` stores: an expression created in the Literal language.
        let envelope = Literal::from_json(serde_json::json!({
            "author": "did:key:z6Mkabc",
            "timestamp": "2026-08-18T16:18:21.287Z",
            "data": "the words",
            "proof": { "key": "did:key:z6Mkabc#z6Mkabc", "signature": "deadbeef" },
        }))
        .to_url()
        .unwrap();
        assert_eq!(
            decode_transcript_body(&envelope).as_deref(),
            Some("the words")
        );

        // Still not a message body: JSON with no string `data`.
        let other = Literal::from_json(serde_json::json!({ "count": 3 }))
            .to_url()
            .unwrap();
        assert_eq!(decode_transcript_body(&other), None);
    }

    /// Link-based gather over an envelope-shaped body — the shape any @Property
    /// declaration produces. Complements the unit test above by proving the
    /// callsite in gather_transcript itself decodes envelope targets end to end.
    /// The pre-fix code returned an empty vec here, silently, which is the exact
    /// symptom James hit in WE.
    #[tokio::test]
    async fn gather_transcript_reads_expression_envelope_body() {
        use crate::types::{Link, LinkStatus};
        use ad4m_client::literal::Literal;

        let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[]).await;

        let envelope = Literal::from_json(serde_json::json!({
            "author": "did:key:z6Mkalice",
            "timestamp": "2026-08-18T16:18:21.287Z",
            "data": "hello from the envelope",
            "proof": { "key": "did:key:z6Mkalice#z6Mkalice", "signature": "deadbeef" },
        }))
        .to_url()
        .unwrap();

        p.add_link(
            Link {
                source: "msg://envelope-1".into(),
                predicate: Some("ns://body".into()),
                target: envelope,
            },
            LinkStatus::Local,
            None,
            &ctx,
        )
        .await
        .expect("seed envelope body link");

        let turns = gather_transcript(&p, "msg://envelope-1", "ns://body")
            .await
            .expect("gather");
        assert_eq!(turns.len(), 1, "envelope body must yield a turn");
        assert_eq!(turns[0].text, "hello from the envelope");
    }

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
        use crate::perspectives::model_query::types::Scope;
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
        let scope = Scope::Raw {
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

    /// Before #1795 the watch tick read the transcript with the
    /// `source_scope_query` reifier pattern — `?speaker`/`?text`/`?timestamp`
    /// off the body link's reifier — and the processed set stored
    /// `turn_id(speaker, text, timestamp)` per row. The model-query read
    /// takes speaker and timestamp off the same link (its `__links` row),
    /// so it mints the same id: an upgraded processor keeps its processed
    /// set because the id inputs did not change. Pinned here so the
    /// provenance change can never silently orphan it.
    #[tokio::test]
    async fn a_turn_keeps_the_id_the_sparql_scope_made_for_it() {
        use crate::agent::signatures::TestSigner;
        use crate::perspectives::auto_processor::watcher::{turn_id, PendingTurn};
        use crate::perspectives::sparql_store::EvalStop;
        use crate::types::{Link, LinkExpression, LinkStatus};

        // The known-good pre-#1795 scope query —
        // BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY at c0441ac4c~1 — verbatim.
        const OLD_SCOPE: &str = "SELECT ?speaker ?text ?timestamp WHERE {
  ?m <ns://body> ?text .
  ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?m <ns://body> ?text )>> .
  ?r <ad4m://ontology/author> ?speaker .
  ?r <ad4m://ontology/timestamp> ?timestamp .
}
ORDER BY ?timestamp";

        let (p, _shapes, _ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        let signer = TestSigner::generate();
        let at = chrono::Utc::now();
        let signed = |predicate: &str, target: &str| {
            let mut l = LinkExpression::from(
                signer.sign_at(
                    Link {
                        source: "msg://1".into(),
                        predicate: Some(predicate.into()),
                        target: target.into(),
                    }
                    .normalize(),
                    at,
                ),
            );
            l.status = Some(LinkStatus::Shared);
            l
        };
        p.sparql_store
            .add_link(&signed("ns://type", "ns://message"))
            .expect("flag");
        p.sparql_store
            .add_link(&signed("ns://body", "literal:string:the words"))
            .expect("body");

        // The read the watcher makes now.
        let model = gather_transcript_model(&p, "ns://Message", "body", "{}")
            .await
            .expect("model read")
            .turns;
        assert_eq!(model.len(), 1, "got {model:#?}");

        // The read it made before, with the row→turn conversion
        // `gather_transcript_sparql` applied verbatim.
        let json = p
            .sparql_store
            .query_arbitrary(&EvalStop::none(), OLD_SCOPE)
            .expect("sparql read");
        let rows: Vec<serde_json::Value> = serde_json::from_str(&json).expect("rows json");
        assert_eq!(rows.len(), 1, "got {rows:#?}");
        let row = &rows[0];
        let raw_text = row["text"].as_str().expect("?text");
        let old_turn = TranscriptTurn {
            speaker: row["speaker"].as_str().expect("?speaker").to_string(),
            text: if raw_text.starts_with("literal:") {
                decode_transcript_body(raw_text).expect("a string literal body")
            } else if let Some(data) = serde_json::from_str::<serde_json::Value>(raw_text)
                .ok()
                .and_then(|v| v.get("data").and_then(|d| d.as_str()).map(String::from))
            {
                data
            } else {
                raw_text.to_string()
            },
            timestamp: row["timestamp"].as_str().expect("?timestamp").to_string(),
        };

        assert_eq!(model[0].speaker, old_turn.speaker);
        assert_eq!(model[0].text, old_turn.text);
        assert_eq!(model[0].timestamp, old_turn.timestamp);
        assert_eq!(
            PendingTurn::from_transcript(&model[0]).id,
            turn_id(&old_turn.speaker, &old_turn.text, &old_turn.timestamp),
            "the processed-set id must be the one the SPARQL scope minted"
        );
    }

    /// Two signers writing a body at the SAME timestamp: the turn must take
    /// text and speaker from ONE link — the one the deterministic tie rule
    /// (timestamp, then target, author, signature) ranks last. Written in both
    /// insertion orders, because a read that keeps the first row it sees
    /// answers differently per order — and one that reads the value from
    /// hydration but the author from `__links`' last row can pair one signer's
    /// words with the other's name.
    #[tokio::test]
    async fn a_body_written_twice_at_one_instant_keeps_text_and_speaker_together() {
        use crate::agent::signatures::TestSigner;
        use crate::types::{Link, LinkExpression, LinkStatus};

        let (p, _shapes, _ctx) = setup_perspective_no_llm(&[("ns://Message", MESSAGE_SDNA)]).await;
        let alice = TestSigner::generate();
        let bob = TestSigner::generate();
        let at = chrono::Utc::now();

        let signed = |signer: &TestSigner, msg: &str, predicate: &str, target: &str| {
            let mut l = LinkExpression::from(
                signer.sign_at(
                    Link {
                        source: msg.into(),
                        predicate: Some(predicate.into()),
                        target: target.into(),
                    }
                    .normalize(),
                    at,
                ),
            );
            l.status = Some(LinkStatus::Shared);
            l
        };

        // The same two bodies on each message, inserted in opposite orders.
        for (msg, alice_first) in [("msg://tie/a", true), ("msg://tie/b", false)] {
            p.sparql_store
                .add_link(&signed(&alice, msg, "ns://type", "ns://message"))
                .expect("flag");
            let alice_body = signed(&alice, msg, "ns://body", "literal:string:aaa");
            let bob_body = signed(&bob, msg, "ns://body", "literal:string:bbb");
            let ordered = if alice_first {
                [alice_body, bob_body]
            } else {
                [bob_body, alice_body]
            };
            for link in ordered {
                p.sparql_store.add_link(&link).expect("body");
            }
        }

        let turns = gather_transcript_model(&p, "ns://Message", "body", "{}")
            .await
            .expect("model read")
            .turns;
        assert_eq!(turns.len(), 2, "got {turns:#?}");
        for turn in &turns {
            assert_eq!(
                (turn.text.as_str(), turn.speaker.as_str()),
                ("bbb", bob.did.as_str()),
                "text and speaker must name one link, the same in both orders: {turn:#?}"
            );
        }
    }
}
