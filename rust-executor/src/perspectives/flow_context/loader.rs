//! Perspective-side reading for the Model C flow-aware extraction path.
//!
//! Two independent I/O layers plus a composer:
//! - [`load_flow_instances`] — hydrated [`FlowInstanceRecord`] rows via
//!   [`PerspectiveInstance::model_query`].
//! - [`load_shacl_flows`] — parsed [`SHACLFlow`] catalogue keyed by
//!   name via targeted `get_links` queries.
//! - [`gather_active_flow_contexts`] — composes the two + hands to
//!   [`build_flow_contexts`], the entry point the extraction pass calls
//!   after `apply_with_overlay`.
//!
//! Silent-fallback discipline: any I/O failure inside
//! [`gather_active_flow_contexts`] downgrades to an empty result with a
//! warning log — extraction MUST NOT break because one perspective
//! couldn't enumerate flows.

use super::render::summarize_flow_instance;
use super::types::{FlowContext, FlowInstanceRecord};
use crate::perspectives::flow_classes::FLOW_INSTANCE_CLASS;
use crate::perspectives::model_query::types::Scope;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::{parse_flow_from_links, SHACLFlow};
use crate::types::{Link, LinkQuery};
use std::collections::{HashMap, HashSet};

/// Parse one hydrated `FlowInstance` JSON object (as returned by
/// [`PerspectiveInstance::model_query`]) into a [`FlowInstanceRecord`].
///
/// Returns `None` when any of `id` / `flow` / `subject` / `currentState`
/// is missing — an untyped or half-written FlowInstance is silently
/// skipped rather than failing the whole extraction pass. The typical
/// cause is a mid-mint crash between constructor and setter writes; the
/// half-record shows up in the next model_query result and we don't
/// want that to poison every future extraction until it's hand-cleaned.
pub fn parse_flow_instance_from_hydrated(v: &serde_json::Value) -> Option<FlowInstanceRecord> {
    let instance_uri = v.get("id").and_then(|x| x.as_str())?.to_string();
    let flow_name = v.get("flow").and_then(|x| x.as_str())?.to_string();
    let subject = v.get("subject").and_then(|x| x.as_str())?.to_string();
    let current_state = v.get("currentState").and_then(|x| x.as_str())?.to_string();
    // Ad4mModel synthesises `createdAt` from the earliest link timestamp
    // on hydration (`rust-executor/src/perspectives/model_query/hydration.rs`).
    // When present it's an RFC3339 string; we keep it opaque here.
    let created_at = v
        .get("createdAt")
        .and_then(|x| x.as_str())
        .map(str::to_string);
    Some(FlowInstanceRecord {
        flow_name,
        instance_uri,
        subject,
        current_state,
        created_at,
    })
}

/// Load live `FlowInstance`s on the perspective whose `subject`
/// matches any URI in `subjects`. Empty `subjects` returns `Ok(vec![])`
/// without touching the store — the extraction pass has no batch bases
/// to scope against, so surfacing every FlowInstance would be
/// unbounded and violate Model C scope discipline (James PR #929 J#1).
///
/// Single-URI queries push the filter down to `model_query`
/// (`where.subject == uri`) so the existing typed-in-store path is
/// preserved. Multi-URI batches load-all and filter in-Rust because
/// `model_query` has no `in` operator today; the batch is bounded by
/// the auto-processor's drained item count.
///
/// Silently returns `Ok(vec![])` when the `FlowInstance` class hasn't
/// been registered yet on this perspective — a freshly-created
/// perspective simply has no live flows, and treating that as a
/// perspective-wide error would poison the extraction pass on every
/// call before the first flow is ever spawned.
pub async fn load_flow_instances(
    perspective: &PerspectiveInstance,
    subjects: &[String],
) -> anyhow::Result<Vec<FlowInstanceRecord>> {
    if subjects.is_empty() {
        return Ok(vec![]);
    }
    let query = if subjects.len() == 1 {
        serde_json::json!({ "where": { "subject": subjects[0].clone() } })
    } else {
        serde_json::json!({})
    };
    let json = match perspective
        .model_query(FLOW_INSTANCE_CLASS, &query.to_string())
        .await
    {
        Ok(j) => j,
        Err(e) => {
            // Absent-class case: no FlowInstances have ever been minted
            // on this perspective, so the SHACL shape isn't registered
            // yet. That's a valid steady state — return empty rather
            // than propagating an error that would break Model C's
            // extraction pass on every call.
            let msg = format!("{e:#}");
            if msg.contains("Shape not found") || msg.contains("shape not found") {
                return Ok(vec![]);
            }
            return Err(anyhow::anyhow!(
                "load_flow_instances: model_query failed: {msg}"
            ));
        }
    };
    let parsed: serde_json::Value = serde_json::from_str(&json)
        .map_err(|e| anyhow::anyhow!("load_flow_instances: response not JSON: {e:#}"))?;
    let instances = parsed
        .get("instances")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    let subject_set: HashSet<&str> = subjects.iter().map(String::as_str).collect();
    Ok(instances
        .iter()
        .filter_map(parse_flow_instance_from_hydrated)
        .filter(|r| subject_set.contains(r.subject.as_str()))
        .collect())
}

/// Pair each active [`FlowInstanceRecord`] with its parsed
/// [`SHACLFlow`] definition and produce [`FlowContext`]s for prompt
/// insertion.
///
/// Records whose `flow_name` is absent from `flows_by_name` are
/// silently skipped — a stale FlowInstance (its flow was
/// unregistered) shouldn't fail the whole extraction pass. Order is
/// preserved from `records`.
pub fn build_flow_contexts(
    records: &[FlowInstanceRecord],
    flows_by_name: &HashMap<String, SHACLFlow>,
) -> Vec<FlowContext> {
    records
        .iter()
        .filter_map(|r| {
            let flow = flows_by_name.get(&r.flow_name)?;
            Some(summarize_flow_instance(
                flow,
                r.instance_uri.clone(),
                r.subject.clone(),
                r.current_state.clone(),
            ))
        })
        .collect()
}

/// Slice 10.3c — compose the two loaders + [`build_flow_contexts`] into
/// one call that the extraction pass (`run.rs`) can use directly.
///
/// Returns the (possibly empty) list of [`FlowContext`]s. Any I/O
/// failure is logged and downgraded to an empty result — the extraction
/// pass MUST NOT break because one perspective couldn't enumerate flows;
/// the fallback is "extract without flow-aware prompting", which is
/// exactly the pre-slice-10.2 behaviour.
///
/// `subjects` is the set of base-expression URIs the extraction pass is
/// operating on — the drained batch bases in the auto-processor path
/// (see `InterpretationRunCursor.sources`). A FlowInstance whose
/// `subject` matches one of these is running on the same expression the
/// pass is interpreting and is included in the prompt.
///
/// Empty `subjects` returns an empty result rather than the whole
/// perspective's FlowInstance set — the pre-fix behaviour (loading
/// every FlowInstance on the perspective when scope was `None`) was
/// unbounded and violated Model C prompt discipline (James PR #929 J#1).
/// Callers that legitimately want every active flow (e.g. component
/// tests) can query [`load_flow_instances`] directly with the full
/// subject set.
pub async fn gather_active_flow_contexts(
    perspective: &PerspectiveInstance,
    subjects: &[String],
) -> Vec<FlowContext> {
    let flows_by_name = match load_shacl_flows(perspective).await {
        Ok(m) => m,
        Err(e) => {
            log::warn!("gather_active_flow_contexts: load_shacl_flows failed, using empty: {e:#}");
            return Vec::new();
        }
    };
    if flows_by_name.is_empty() {
        return Vec::new();
    }
    let records = match load_flow_instances(perspective, subjects).await {
        Ok(r) => r,
        Err(e) => {
            log::warn!(
                "gather_active_flow_contexts: load_flow_instances failed, using empty: {e:#}"
            );
            return Vec::new();
        }
    };
    build_flow_contexts(&records, &flows_by_name)
}

/// Derive the flow-filter subject key from an extraction pass `Scope`.
/// Kept for the strategy-path call site that has no drained-batch cursor
/// available yet (it still uses the dedup scope's anchor URI). New code
/// should prefer wiring `InterpretationRunCursor.sources` through and
/// calling [`gather_active_flow_contexts`] with the batch bases.
pub fn scope_subject(scope: &Scope) -> &str {
    match scope {
        Scope::Model { id, .. } => id.as_str(),
        Scope::Raw { id, .. } => id.as_str(),
    }
}

/// Discover every `SHACLFlow` definition present in a flat bag of links
/// and return them keyed by [`SHACLFlow::name`].
///
/// Flow discovery is anchored on `?flow_uri rdf://type ad4m://Flow`. For
/// each such source, the loader gathers:
///
/// - all links whose `source == flow_uri` (the flow-level metadata), and
/// - all links whose `source` matches a `hasState` or `hasTransition`
///   child target of that flow (the per-state / per-transition rows).
///
/// The gathered slice is then handed to
/// [`parse_flow_from_links`]. Flows whose parse fails are skipped with a
/// warning rather than failing the whole load — a single malformed flow
/// definition on the perspective shouldn't blind the extraction pass to
/// every other flow.
///
/// This is the pure, in-memory half; [`load_shacl_flows`] is the
/// [`PerspectiveInstance`] wrapper that fetches the link set with
/// targeted queries and delegates here.
pub fn parse_flows_from_bag(links: &[Link]) -> HashMap<String, SHACLFlow> {
    let mut flows = HashMap::new();

    let flow_uris: Vec<String> = links
        .iter()
        .filter(|l| l.predicate.as_deref() == Some("rdf://type") && l.target == "ad4m://Flow")
        .map(|l| l.source.clone())
        .filter(|s| !s.is_empty())
        .collect();

    for flow_uri in flow_uris {
        let child_uris: HashSet<String> = links
            .iter()
            .filter(|l| {
                l.source == flow_uri
                    && matches!(
                        l.predicate.as_deref(),
                        Some("ad4m://hasState") | Some("ad4m://hasTransition")
                    )
            })
            .map(|l| l.target.clone())
            .collect();

        let related: Vec<Link> = links
            .iter()
            .filter(|l| l.source == flow_uri || child_uris.contains(&l.source))
            .cloned()
            .collect();

        match parse_flow_from_links(&related, &flow_uri) {
            Ok(flow) => {
                flows.insert(flow.name.clone(), flow);
            }
            Err(e) => {
                log::warn!("parse_flows_from_bag: skipping flow {flow_uri} (parse failed): {e:#}");
            }
        }
    }

    flows
}

/// Load every `SHACLFlow` definition off a live perspective and return
/// them keyed by name — the flow-side companion to
/// [`load_flow_instances`].
///
/// Uses targeted [`PerspectiveInstance::get_links`] calls (rather than a
/// full-perspective scan) to keep the hot-path cost proportional to
/// `F * (S+T)`, where `F` is the flow count and `S+T` is the average
/// state + transition count per flow. On a perspective with millions of
/// links this is what keeps the auto-processor's per-batch flow load
/// bounded.
///
/// Query plan:
///
/// 1. `(predicate = rdf://type, target = ad4m://Flow)` — enumerate every
///    live `flow_uri` on the perspective.
/// 2. Per `flow_uri`: `(source = flow_uri)` — pull the flow-level
///    metadata + `hasState`/`hasTransition` edges.
/// 3. Per state/transition child URI: `(source = child_uri)` — pull the
///    per-state / per-transition rows.
///
/// Then delegates to [`parse_flows_from_bag`] for the actual
/// [`SHACLFlow`] reconstruction, so the parse-side behaviour is
/// exercised by the module's fixture-driven tests without needing a
/// live perspective.
///
/// Errors from the outer type-index query propagate; per-flow child
/// query failures are logged and skipped so one broken flow can't blind
/// the extraction pass to every other flow on the perspective.
pub async fn load_shacl_flows(
    perspective: &PerspectiveInstance,
) -> anyhow::Result<HashMap<String, SHACLFlow>> {
    let type_query = LinkQuery {
        source: None,
        predicate: Some("rdf://type".to_string()),
        target: Some("ad4m://Flow".to_string()),
        from_date: None,
        until_date: None,
        limit: None,
    };
    let type_links = perspective
        .get_links(&type_query)
        .await
        .map_err(|e| anyhow::anyhow!("load_shacl_flows: get_links(type) failed: {e:#}"))?;

    let mut bag: Vec<Link> = Vec::with_capacity(type_links.len());
    for tl in &type_links {
        bag.push(tl.data.clone());
    }

    for tl in type_links {
        let flow_uri = tl.data.source;
        if flow_uri.is_empty() {
            continue;
        }
        let flow_query = LinkQuery {
            source: Some(flow_uri.clone()),
            predicate: None,
            target: None,
            from_date: None,
            until_date: None,
            limit: None,
        };
        let flow_links = match perspective.get_links(&flow_query).await {
            Ok(l) => l,
            Err(e) => {
                log::warn!(
                    "load_shacl_flows: get_links(source={flow_uri}) failed, skipping: {e:#}"
                );
                continue;
            }
        };
        let child_uris: Vec<String> = flow_links
            .iter()
            .filter(|d| {
                matches!(
                    d.data.predicate.as_deref(),
                    Some("ad4m://hasState") | Some("ad4m://hasTransition")
                )
            })
            .map(|d| d.data.target.clone())
            .collect();
        bag.extend(flow_links.into_iter().map(|d| d.data));

        for child in child_uris {
            let child_query = LinkQuery {
                source: Some(child),
                predicate: None,
                target: None,
                from_date: None,
                until_date: None,
                limit: None,
            };
            match perspective.get_links(&child_query).await {
                Ok(child_links) => bag.extend(child_links.into_iter().map(|d| d.data)),
                Err(e) => log::warn!(
                    "load_shacl_flows: get_links(child of {flow_uri}) failed, skipping child: {e:#}"
                ),
            }
        }
    }

    Ok(parse_flows_from_bag(&bag))
}

// ============================================================================
// Tests — pure hydrated-JSON parsing + fixture-driven bag parsing. The
// live-perspective composition lives in the sibling `e2e_tests` module.
// ============================================================================
#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::shacl_parser::parse_flow_to_links;

    // ------------- parse_flow_instance_from_hydrated (slice 10.1b) -------------

    #[test]
    fn parse_flow_instance_happy_path() {
        let v = serde_json::json!({
            "id": "ad4m://flow/instance/inst-1",
            "baseExpression": "ad4m://flow/instance/inst-1",
            "flow": "Delivery",
            "subject": "ad4m://task/foo",
            "currentState": "scoped",
            // `createdAt` is Ad4mModel's synthesised earliest-link
            // timestamp — the value the record now sources from.
            "createdAt": "2026-08-26T09:00:00Z",
            "author": "did:key:z6Mk…",
            "timestamp": "2026-08-26T09:00:00.001Z"
        });
        let r = parse_flow_instance_from_hydrated(&v).expect("required scalars present");
        assert_eq!(r.instance_uri, "ad4m://flow/instance/inst-1");
        assert_eq!(r.flow_name, "Delivery");
        assert_eq!(r.subject, "ad4m://task/foo");
        assert_eq!(r.current_state, "scoped");
        assert_eq!(r.created_at.as_deref(), Some("2026-08-26T09:00:00Z"));
    }

    #[test]
    fn parse_flow_instance_extra_fields_ignored() {
        // The Ad4mModel hydration path emits synthetic fields (createdAt,
        // updatedAt, author, timestamp) alongside the class properties. The
        // parser must ignore anything it doesn't need — accepting a superset
        // of keys is how the reader stays forward-compatible with new
        // properties (see the reserved-field fix in `e6362e5ca`).
        let v = serde_json::json!({
            "id": "ad4m://flow/instance/inst-x",
            "flow": "Deliberation",
            "subject": "ad4m://proposal/bar",
            "currentState": "perspectives",
            "createdAt": "2026-08-26T10:00:00Z",
            "updatedAt": "2026-08-26T10:00:00.001Z",
            "author": "did:key:zzzz",
            "some_future_property_the_llm_added": "hello",
        });
        let r = parse_flow_instance_from_hydrated(&v).expect("required scalars present");
        assert_eq!(r.flow_name, "Deliberation");
        assert_eq!(r.current_state, "perspectives");
        assert_eq!(r.created_at.as_deref(), Some("2026-08-26T10:00:00Z"));
    }

    #[test]
    fn parse_flow_instance_missing_required_returns_none() {
        // Missing `flow` → mid-mint crash between constructor and setter
        // writes. Skip the record rather than pollute Vec<FlowContext>
        // with a half-typed instance.
        let v = serde_json::json!({
            "id": "ad4m://flow/instance/half",
            "subject": "ad4m://task/foo",
            "currentState": "identified",
        });
        assert!(parse_flow_instance_from_hydrated(&v).is_none());
    }

    #[test]
    fn parse_flow_instance_missing_created_at_still_parses() {
        // `createdAt` (synthesised on hydration) is optional in the
        // record — the extraction pass renders "start time unknown"
        // rather than skipping when a hydration path elides it.
        let v = serde_json::json!({
            "id": "ad4m://flow/instance/no-ts",
            "flow": "Delivery",
            "subject": "ad4m://task/foo",
            "currentState": "identified",
        });
        let r = parse_flow_instance_from_hydrated(&v).expect("required scalars present");
        assert!(r.created_at.is_none());
    }

    #[test]
    fn parse_flow_instance_non_string_required_returns_none() {
        // A scalar that's on-graph as a non-string (e.g. flow name mistakenly
        // stored as a number) is treated the same as missing — one half-typed
        // record must not block extraction of the well-typed rest.
        let v = serde_json::json!({
            "id": "ad4m://flow/instance/bad",
            "flow": 42, // wrong type
            "subject": "ad4m://task/foo",
            "currentState": "identified",
        });
        assert!(parse_flow_instance_from_hydrated(&v).is_none());
    }

    // ------------- build_flow_contexts (slice 10.1b) -------------

    fn record(flow: &str, uri: &str, subject: &str, state: &str) -> FlowInstanceRecord {
        FlowInstanceRecord {
            flow_name: flow.to_string(),
            instance_uri: uri.to_string(),
            subject: subject.to_string(),
            current_state: state.to_string(),
            created_at: Some("2026-08-26T09:00:00Z".to_string()),
        }
    }

    // Minimal fixture flow so build_flow_contexts has something to pair
    // records against without pulling the whole render-side fixture
    // set. Local to this module because it exercises the loader-side
    // joining, not the rendering surface.
    fn delivery_flow() -> SHACLFlow {
        use crate::perspectives::shacl_parser::{
            AD4MAction, ConsensusRule, FlowState, FlowTransition, LinkPattern,
        };
        let empty_pattern = LinkPattern {
            source: None,
            predicate: String::new(),
            target: String::new(),
        };
        let state_named = |name: &str| FlowState {
            name: name.to_string(),
            value: 0.0,
            state_check: empty_pattern.clone(),
            interpretation_hint: None,
            requires: None,
            semantic_check: None,
            consensus_rule: None,
        };
        let transition = |from: &str, to: &str| FlowTransition {
            action_name: format!("{from}->{to}"),
            from_state: from.to_string(),
            to_state: to.to_string(),
            actions: Vec::<AD4MAction>::new(),
        };
        SHACLFlow {
            name: "Delivery".to_string(),
            namespace: "ad4m://".to_string(),
            start_action: vec![],
            states: vec![
                state_named("identified"),
                state_named("scoped"),
                state_named("in_progress"),
                state_named("review"),
                state_named("done"),
            ],
            transitions: vec![
                transition("identified", "scoped"),
                transition("scoped", "in_progress"),
                transition("in_progress", "review"),
                transition("in_progress", "identified"),
                transition("review", "done"),
                transition("review", "in_progress"),
            ],
            interpretation_hint: Some(
                "A team-scale unit of work moving from identification to done.".to_string(),
            ),
            input_types: vec!["ad4m://Task".to_string()],
            output_types: vec![],
            creation_hint: None,
            context: None,
            consensus_rule: Some(ConsensusRule {
                n: 1,
                from_role: None,
            }),
        }
    }

    #[test]
    fn build_flow_contexts_pairs_records_with_flows() {
        let flow = delivery_flow();
        let mut catalogue = HashMap::new();
        catalogue.insert("Delivery".to_string(), flow);
        let records = vec![record(
            "Delivery",
            "ad4m://flow/instance/inst-1",
            "ad4m://task/foo",
            "in_progress",
        )];
        let ctxs = build_flow_contexts(&records, &catalogue);
        assert_eq!(ctxs.len(), 1);
        assert_eq!(ctxs[0].flow_name, "Delivery");
        assert_eq!(ctxs[0].current_state, "in_progress");
        // Wiring through summarize_flow_instance — reachable states from
        // `in_progress` on the delivery flow are `review` + `identified`.
        let names: Vec<&str> = ctxs[0]
            .reachable_next_states
            .iter()
            .map(|s| s.name.as_str())
            .collect();
        assert_eq!(names, vec!["review", "identified"]);
    }

    #[test]
    fn build_flow_contexts_skips_records_with_unknown_flow() {
        // A stale FlowInstance whose flow was unregistered must not fail
        // extraction — silently skip and keep processing the rest.
        let records = vec![
            record("Ghost", "ad4m://flow/instance/g", "ad4m://x", "s0"),
            record(
                "Delivery",
                "ad4m://flow/instance/d",
                "ad4m://task/y",
                "identified",
            ),
        ];

        // Empty catalogue → all skipped, empty result (not error).
        let empty: HashMap<String, SHACLFlow> = HashMap::new();
        assert!(build_flow_contexts(&records, &empty).is_empty());

        // Only Delivery known → Ghost skipped, Delivery kept.
        let mut catalogue_with_delivery: HashMap<String, SHACLFlow> = HashMap::new();
        catalogue_with_delivery.insert("Delivery".to_string(), delivery_flow());
        let ctxs = build_flow_contexts(&records, &catalogue_with_delivery);
        assert_eq!(ctxs.len(), 1);
        assert_eq!(ctxs[0].flow_name, "Delivery");
    }

    #[test]
    fn build_flow_contexts_preserves_record_order() {
        // Order matters — the prompt-builder inserts flows in the order
        // the caller supplies. A HashMap-based iteration would be
        // non-deterministic; we iterate `records`.
        let mut catalogue = HashMap::new();
        catalogue.insert("Delivery".to_string(), delivery_flow());
        let records = vec![
            record(
                "Delivery",
                "ad4m://flow/instance/a",
                "ad4m://x",
                "identified",
            ),
            record("Delivery", "ad4m://flow/instance/b", "ad4m://y", "scoped"),
            record("Delivery", "ad4m://flow/instance/c", "ad4m://z", "review"),
        ];
        let ctxs = build_flow_contexts(&records, &catalogue);
        let uris: Vec<&str> = ctxs.iter().map(|c| c.instance_uri.as_str()).collect();
        assert_eq!(
            uris,
            vec![
                "ad4m://flow/instance/a",
                "ad4m://flow/instance/b",
                "ad4m://flow/instance/c",
            ]
        );
    }

    // ------------- parse_flows_from_bag -------------

    fn mk_link(source: &str, predicate: &str, target: &str) -> Link {
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        }
    }

    fn ready_done_flow_links(name: &str, namespace: &str) -> Vec<Link> {
        let flow_json = format!(
            r#"{{
                "name": "{name}",
                "namespace": "{namespace}",
                "start_action": [],
                "states": [
                    {{"name": "ready", "value": 0.0, "state_check": {{"predicate": "{namespace}state", "target": "{namespace}ready"}}}},
                    {{"name": "done",  "value": 1.0, "state_check": {{"predicate": "{namespace}state", "target": "{namespace}done"}}}}
                ],
                "transitions": [
                    {{"action_name": "Complete", "from_state": "ready", "to_state": "done", "actions": []}}
                ]
            }}"#
        );
        parse_flow_to_links(&flow_json, name).expect("writer builds flow links")
    }

    #[test]
    fn parse_flows_from_bag_empty_returns_empty() {
        let flows = parse_flows_from_bag(&[]);
        assert!(flows.is_empty());
    }

    #[test]
    fn parse_flows_from_bag_no_flow_type_links_returns_empty() {
        // Bag with random unrelated links but no `rdf://type → ad4m://Flow`.
        let bag = vec![
            mk_link("ad4m://task/foo", "rdf://type", "ad4m://Task"),
            mk_link(
                "ad4m://task/foo",
                "ad4m://title",
                "literal:string:Ship%20it",
            ),
        ];
        let flows = parse_flows_from_bag(&bag);
        assert!(flows.is_empty());
    }

    #[test]
    fn parse_flows_from_bag_discovers_single_flow_via_writer_output() {
        let bag = ready_done_flow_links("TODO", "todo://");
        let flows = parse_flows_from_bag(&bag);
        assert_eq!(flows.len(), 1, "expected exactly one flow");
        let flow = flows.get("TODO").expect("flow keyed by name");
        assert_eq!(flow.namespace, "todo://");
        assert_eq!(flow.states.len(), 2);
        assert_eq!(flow.transitions.len(), 1);
        assert_eq!(flow.transitions[0].action_name, "Complete");
        assert_eq!(flow.transitions[0].from_state, "ready");
        assert_eq!(flow.transitions[0].to_state, "done");
    }

    #[test]
    fn parse_flows_from_bag_discovers_multiple_flows_side_by_side() {
        let mut bag = ready_done_flow_links("TODO", "todo://");
        bag.extend(ready_done_flow_links("Approval", "gov://"));
        let flows = parse_flows_from_bag(&bag);
        assert_eq!(flows.len(), 2);
        assert!(flows.contains_key("TODO"));
        assert!(flows.contains_key("Approval"));
        // Namespaces stay isolated — cross-contamination would mean the
        // reader crossed hasState/hasTransition child gathering between
        // sibling flows.
        assert_eq!(flows["TODO"].namespace, "todo://");
        assert_eq!(flows["Approval"].namespace, "gov://");
        assert_eq!(flows["TODO"].states.len(), 2);
        assert_eq!(flows["Approval"].states.len(), 2);
    }

    #[test]
    fn parse_flows_from_bag_ignores_unrelated_noise_links() {
        // Real flow + unrelated task/proposal noise. The noise MUST NOT
        // widen the flow's link set, and MUST NOT stop the flow from
        // being discovered.
        let mut bag = ready_done_flow_links("TODO", "todo://");
        bag.extend(vec![
            mk_link("ad4m://task/foo", "rdf://type", "ad4m://Task"),
            mk_link(
                "ad4m://task/foo",
                "ad4m://title",
                "literal:string:Something",
            ),
            mk_link("ad4m://proposal/bar", "rdf://type", "ad4m://Proposal"),
        ]);
        let flows = parse_flows_from_bag(&bag);
        assert_eq!(flows.len(), 1);
        assert!(flows.contains_key("TODO"));
    }

    #[test]
    fn parse_flows_from_bag_skips_flow_uri_without_flow_suffix() {
        // Manufactured type-index row whose source doesn't end in
        // "Flow" — parse_flow_from_links returns Err, loader must skip
        // it rather than propagate.
        let mut bag = ready_done_flow_links("TODO", "todo://");
        bag.push(mk_link("malformed://Broken", "rdf://type", "ad4m://Flow"));
        let flows = parse_flows_from_bag(&bag);
        assert_eq!(flows.len(), 1, "well-formed flow still discovered");
        assert!(flows.contains_key("TODO"));
    }

    #[test]
    fn parse_flows_from_bag_ignores_type_link_with_empty_source() {
        // A `rdf://type → ad4m://Flow` link with an empty source has no
        // flow URI to hang gathering off — must not be enumerated.
        let mut bag = ready_done_flow_links("TODO", "todo://");
        bag.push(mk_link("", "rdf://type", "ad4m://Flow"));
        let flows = parse_flows_from_bag(&bag);
        assert_eq!(flows.len(), 1);
        assert!(flows.contains_key("TODO"));
    }

    #[test]
    fn parse_flows_from_bag_child_uri_collision_across_flows_stays_isolated() {
        // Two flows that both happen to reference a state named
        // "shared" — but the state URIs are namespaced by flow name, so
        // gathering must not cross the boundary.
        let mut bag = ready_done_flow_links("Alpha", "ex://");
        bag.extend(ready_done_flow_links("Beta", "ex://"));
        let flows = parse_flows_from_bag(&bag);
        assert_eq!(flows.len(), 2);
        // Each flow's state count must remain 2 — a leak would push
        // one flow's states into the other.
        for name in ["Alpha", "Beta"] {
            let f = flows.get(name).expect("flow present");
            assert_eq!(f.states.len(), 2, "flow {name} states leaked from sibling");
        }
    }

    #[test]
    fn parse_flows_from_bag_key_is_shaclflow_name_not_uri() {
        // The HashMap MUST key on `SHACLFlow.name` (matches
        // FlowInstance.flow discriminator + build_flow_contexts lookup).
        let bag = ready_done_flow_links("TODO", "todo://");
        let flows = parse_flows_from_bag(&bag);
        assert!(
            !flows.contains_key("todo://TODOFlow"),
            "keyed on URI, not name"
        );
        assert!(flows.contains_key("TODO"), "must key on flow.name");
    }

    // ------------- scope_subject (slice 10.3c) -------------

    #[test]
    fn scope_subject_extracts_id_from_both_variants() {
        // Both Scope variants carry the pass anchor URI in `id`. The
        // extraction pass filters FlowInstances by subject == this URI,
        // so a mismatch here would silently drop every scope-scoped
        // active flow.
        let m = Scope::Model {
            model: "Task".to_string(),
            id: "ad4m://task/some-task".to_string(),
            field: None,
        };
        assert_eq!(scope_subject(&m), "ad4m://task/some-task");

        let r = Scope::Raw {
            id: "literal://string:channel-x".to_string(),
            predicate: "ad4m://hasChild".to_string(),
        };
        assert_eq!(scope_subject(&r), "literal://string:channel-x");
    }
}
