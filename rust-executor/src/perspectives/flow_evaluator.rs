//! Deterministic post-processing over active flows.
//!
//! After an extraction pass has committed its writes, every live
//! `FlowInstance` is checked against the `requires` guards of the states
//! reachable from its current one. Each fully satisfied (instance,
//! next-state) pair becomes an on-graph `FlowTransitionProposal` written
//! on behalf of the acting DID. This is what keeps a flow moving when the
//! LLM forgets to propose a transition it has just produced the evidence
//! for.
//!
//! A `requires` guard is an array of `ModelQuery`s with AND semantics.
//! Each query is translated to a `model_query` input, run against the
//! perspective and checked against its `count` cardinality. The matched
//! instance IDs form the proposal's evidence bag, sealed by an
//! order-independent SHA256 in [`evidence_hash`] so a later verification
//! can detect evidence that no longer resolves.
//!
//! Two optional refinements sit between evaluation and the write:
//!
//! - A state with a `semanticCheck` hint is confirmed by a second, small
//!   LLM call (see `flow_semantic_check`); anything but a clear YES
//!   discards the transition.
//! - The LLM's own flow proposals (from the strategy path's JSON output or
//!   the harness's `{Flow}_propose_transition` tool) never fire a
//!   transition on their own. When one names a transition the guard has
//!   already satisfied, its `reason` becomes the proposal's `rationale`;
//!   otherwise it is dropped.
//!
//! Every failure here is a skip, never an error: a broken flow
//! definition, an unregistered class or a transient query failure drops
//! one transition and the extraction pass carries on.

use crate::agent::AgentContext;
use crate::perspectives::flow_classes::write_flow_transition_proposal;
use crate::perspectives::flow_context::{
    load_all_flow_instances, load_flow_instances, load_shacl_flows, reachable_next_states,
    scope_subject, FlowInstanceRecord,
};
use crate::perspectives::flow_semantic_check::{
    build_semantic_check_prompt, semantic_check_passed, SemanticCheckLlm,
};
use crate::perspectives::interpretation::LlmFlowProposal;
use crate::perspectives::model_query::types::Scope;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::{
    ModelQuery, ModelQueryCount, PropertyCondition, SHACLFlow,
};
use anyhow::{anyhow, bail, Result};
use async_trait::async_trait;
use serde_json::{json, Map, Value};
use sha2::{Digest, Sha256};
use std::collections::HashMap;

/// One (flow instance, next-state) pair whose `requires` guard is fully
/// satisfied on the committed graph.
#[derive(Debug, Clone, PartialEq)]
pub struct SatisfiedTransition {
    pub flow_name: String,
    pub instance_uri: String,
    pub from_state: String,
    pub to_state: String,
    /// Every matched instance ID across the state's `requires`, deduplicated.
    pub evidence_ids: Vec<String>,
    /// The same instances as `evidence_ids`, hydrated with the JSON
    /// `model_query` returned for each — the semantic check reasons over
    /// this content, not over bare identifiers. Deliberately NOT part of
    /// [`evidence_hash`]: the seal stays a function of class names + IDs, so
    /// a content edit to an already-sealed instance doesn't re-open a guard.
    pub evidence: Vec<EvidenceItem>,
    /// See [`evidence_hash`].
    pub evidence_hash: String,
    /// The target state's `semanticCheck` hint, if it declares one.
    pub semantic_check: Option<String>,
}

/// One hydrated piece of guard evidence: an instance a `requires` query
/// matched, carried with its full `model_query` JSON so downstream LLM
/// passes can evaluate content ("was this agreed?") rather than rubber-stamp
/// a URI list.
#[derive(Debug, Clone, PartialEq)]
pub struct EvidenceItem {
    pub id: String,
    /// SHACL class the matching guard queried for.
    pub class_name: String,
    /// Compact JSON of the matched instance exactly as `model_query`
    /// returned it (id + properties).
    pub content: String,
}

/// Order-independent seal over a satisfied guard's evidence: SHA256 of the
/// class names joined by `|`, a NUL, and the sorted evidence IDs joined by
/// newlines. Two evaluations of the same guard against the same graph
/// produce the same hash regardless of result order.
pub fn evidence_hash(class_names: &[String], evidence_ids: &[String]) -> String {
    let mut sorted_ids = evidence_ids.to_vec();
    sorted_ids.sort();
    let mut hasher = Sha256::new();
    hasher.update(class_names.join("|"));
    hasher.update(b"\0");
    hasher.update(sorted_ids.join("\n"));
    hex::encode(hasher.finalize())
}

/// `count.{min,max}` check with inclusive bounds. An unset `count` means
/// "at least one match"; `{ max: 0 }` is a valid negative guard.
fn cardinality_satisfied(count: Option<&ModelQueryCount>, actual: usize) -> bool {
    match count {
        None => actual >= 1,
        Some(c) => {
            c.min.is_none_or(|min| actual >= min as usize)
                && c.max.is_none_or(|max| actual <= max as usize)
        }
    }
}

/// Translate a flow-side `ModelQuery` into the JSON input `model_query`
/// accepts. `didProperty` becomes `where.<prop> = acting_did`; `or`
/// alternatives become an `OR` list of sub-clauses.
///
/// Scalars, `equals` and `in` map directly onto `WhereCondition`.
/// `exists` and `matches` have no `model_query` counterpart yet, so a
/// guard using them fails translation and is skipped instead of being
/// evaluated against a wrong query.
fn requires_query_input(query: &ModelQuery, acting_did: &str) -> Result<Value> {
    let where_clause = requires_where(query, acting_did)?;
    Ok(if where_clause.is_empty() {
        json!({})
    } else {
        json!({ "where": where_clause })
    })
}

fn requires_where(query: &ModelQuery, acting_did: &str) -> Result<Map<String, Value>> {
    let mut out = Map::new();
    for (field, cond) in query.r#where.iter().flatten() {
        out.insert(field.clone(), where_condition(field, cond)?);
    }
    if let Some(prop) = &query.did_property {
        out.insert(prop.clone(), Value::String(acting_did.to_string()));
    }
    if let Some(alts) = query.or.as_ref().filter(|a| !a.is_empty()) {
        let branches = alts
            .iter()
            .map(|alt| requires_where(alt, acting_did).map(Value::Object))
            .collect::<Result<Vec<_>>>()?;
        out.insert("OR".to_string(), Value::Array(branches));
    }
    Ok(out)
}

fn where_condition(field: &str, cond: &PropertyCondition) -> Result<Value> {
    Ok(match cond {
        PropertyCondition::Str(s) => json!(s),
        PropertyCondition::Num(n) => json!(n),
        PropertyCondition::Bool(b) => json!(b),
        PropertyCondition::Equals { equals } => equals.clone(),
        PropertyCondition::In { one_of } => Value::Array(one_of.clone()),
        PropertyCondition::Exists { .. } => {
            bail!("`{field}`: `exists` is not supported by model_query")
        }
        PropertyCondition::Matches { .. } => {
            bail!("`{field}`: `matches` is not supported by model_query")
        }
    })
}

/// The one perspective call the evaluator needs, behind a trait so the
/// composition below can be unit-tested against a stub.
#[async_trait]
pub trait RequiresQueryable: Send + Sync {
    async fn model_query(&self, class_name: &str, query_json: &str) -> Result<String>;
}

#[async_trait]
impl RequiresQueryable for PerspectiveInstance {
    async fn model_query(&self, class_name: &str, query_json: &str) -> Result<String> {
        PerspectiveInstance::model_query(self, class_name, query_json).await
    }
}

/// Run one guard query. Returns whether its cardinality is satisfied and
/// the matched instances, hydrated with the JSON `model_query` already
/// returned for each (no second read).
async fn evaluate_query<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    query: &ModelQuery,
    acting_did: &str,
) -> Result<(bool, Vec<EvidenceItem>)> {
    let input = requires_query_input(query, acting_did)?;
    let raw = perspective
        .model_query(&query.class_name, &input.to_string())
        .await?;
    let result: Value = serde_json::from_str(&raw)?;
    let matched: Vec<EvidenceItem> = result
        .get("instances")
        .and_then(Value::as_array)
        .ok_or_else(|| {
            anyhow!(
                "model_query for `{}` returned no `instances` array",
                query.class_name
            )
        })?
        .iter()
        .filter_map(|inst| {
            let id = inst.get("id").and_then(Value::as_str)?;
            Some(EvidenceItem {
                id: id.to_string(),
                class_name: query.class_name.clone(),
                content: inst.to_string(),
            })
        })
        .collect();
    Ok((
        cardinality_satisfied(query.count.as_ref(), matched.len()),
        matched,
    ))
}

/// AND across a state's `requires`. `Ok(None)` as soon as one guard
/// misses; `Ok(Some((class_names, evidence)))`, both deduplicated by
/// first-seen order (evidence by instance ID), when every guard holds.
async fn evaluate_requires<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    requires: &[ModelQuery],
    acting_did: &str,
) -> Result<Option<(Vec<String>, Vec<EvidenceItem>)>> {
    let mut class_names: Vec<String> = Vec::new();
    let mut evidence: Vec<EvidenceItem> = Vec::new();
    for query in requires {
        let (satisfied, matched) = evaluate_query(perspective, query, acting_did).await?;
        if !satisfied {
            return Ok(None);
        }
        if !class_names.contains(&query.class_name) {
            class_names.push(query.class_name.clone());
        }
        for item in matched {
            if !evidence.iter().any(|e| e.id == item.id) {
                evidence.push(item);
            }
        }
    }
    Ok(Some((class_names, evidence)))
}

/// Walk every record's reachable next-states and collect the ones whose
/// `requires` guard holds. Records whose flow is unknown and states without
/// a guard are skipped; a query error skips that one transition and is
/// logged at debug level.
pub async fn evaluate_flow_transitions<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    records: &[FlowInstanceRecord],
    flows_by_uri: &HashMap<String, SHACLFlow>,
    acting_did: &str,
) -> Vec<SatisfiedTransition> {
    let mut out = Vec::new();
    for record in records {
        let Some(flow) = flows_by_uri.get(&record.flow_uri) else {
            continue;
        };
        for state in reachable_next_states(flow, &record.current_state) {
            let requires = state.requires.as_deref().unwrap_or_default();
            if requires.is_empty() {
                continue;
            }
            match evaluate_requires(perspective, requires, acting_did).await {
                Ok(Some((class_names, evidence))) => {
                    let evidence_ids: Vec<String> = evidence.iter().map(|e| e.id.clone()).collect();
                    out.push(SatisfiedTransition {
                        flow_name: flow.name.clone(),
                        instance_uri: record.instance_uri.clone(),
                        from_state: record.current_state.clone(),
                        to_state: state.name.clone(),
                        evidence_hash: evidence_hash(&class_names, &evidence_ids),
                        evidence_ids,
                        evidence,
                        semantic_check: state.semantic_check.clone(),
                    })
                }
                Ok(None) => {}
                Err(e) => log::debug!(
                    "flow evaluator: skipping {}.{} on {}: {e:#}",
                    flow.name,
                    state.name,
                    record.instance_uri
                ),
            }
        }
    }
    out
}

/// Load → evaluate → (confirm) → write, called by the extraction pass once
/// its own writes are committed. `scope` narrows the FlowInstance load to
/// the pass's anchor; `None` sweeps every live instance.
///
/// `llm_proposals` are the LLM's own transition proposals: one that names a
/// satisfied transition contributes its `reason` as the proposal's
/// `rationale`, the rest are ignored. `semantic_check`, when given, runs
/// the confirmation LLM for every transition whose target state has a
/// `semanticCheck` hint; only a YES lets it through.
///
/// Returns the URIs of the proposals minted. Never fails: loader errors
/// yield an empty result and a failed write drops only that proposal.
pub async fn run_engine_proposal_pass(
    perspective: &mut PerspectiveInstance,
    scope: Option<&Scope>,
    context: &AgentContext,
    llm_proposals: &[LlmFlowProposal],
    semantic_check: Option<&dyn SemanticCheckLlm>,
) -> Vec<String> {
    let loaded = async {
        let flows_by_uri = load_shacl_flows(perspective).await?;
        let records = match scope {
            Some(s) => load_flow_instances(perspective, &[scope_subject(s).to_string()]).await?,
            None => load_all_flow_instances(perspective).await?,
        };
        let acting_did = crate::agent::did_for_context(context)?;
        anyhow::Ok((flows_by_uri, records, acting_did))
    }
    .await;
    let (flows_by_uri, records, acting_did) = match loaded {
        Ok(loaded) => loaded,
        Err(e) => {
            log::warn!("run_engine_proposal_pass: {e:#}");
            return Vec::new();
        }
    };

    let satisfied =
        evaluate_flow_transitions(perspective, &records, &flows_by_uri, &acting_did).await;

    let mut minted = Vec::with_capacity(satisfied.len());
    for transition in &satisfied {
        let label = format!(
            "{}.{}→{}",
            transition.flow_name, transition.from_state, transition.to_state
        );

        if let (Some(llm), Some(hint)) = (semantic_check, transition.semantic_check.as_deref()) {
            // No hydrated evidence means there is no content the LLM could
            // evaluate the hint against — asking would be a rubber stamp on
            // identifiers. Same fail-closed disposition as an LLM error.
            if transition.evidence.is_empty() {
                log::warn!(
                    "run_engine_proposal_pass: {label} has semanticCheck {hint:?} but no \
                     hydrated evidence to evaluate it against; discarding (fail-closed)"
                );
                continue;
            }
            let flow_hint = records
                .iter()
                .find(|r| r.instance_uri == transition.instance_uri)
                .and_then(|r| flows_by_uri.get(&r.flow_uri))
                .and_then(|f| f.interpretation_hint.as_deref());
            let prompt = build_semantic_check_prompt(transition, hint, flow_hint);
            match llm.confirm(&prompt).await {
                Ok(answer) if semantic_check_passed(&answer) => {}
                Ok(answer) => {
                    log::debug!(
                        "run_engine_proposal_pass: {label} semantic check answered {answer:?}; discarding"
                    );
                    continue;
                }
                Err(e) => {
                    log::debug!(
                        "run_engine_proposal_pass: {label} semantic check failed: {e:#}; discarding"
                    );
                    continue;
                }
            }
        }

        if proposal_already_exists(perspective, transition).await {
            log::debug!("run_engine_proposal_pass: {label} already proposed; skipping");
            continue;
        }

        let rationale = llm_proposals
            .iter()
            .find(|p| p.instance == transition.instance_uri && p.to_state == transition.to_state)
            .and_then(|p| p.reason.as_deref())
            .map(str::trim)
            .filter(|r| !r.is_empty());

        match write_proposal(perspective, transition, &acting_did, rationale, context).await {
            Ok(uri) => minted.push(uri),
            Err(e) => log::debug!("run_engine_proposal_pass: {label} not written: {e:#}"),
        }
    }
    minted
}

/// Check whether a proposal with the same evidence hash already exists for
/// the same flow instance AND target state. Keeps the pass idempotent:
/// minting does not advance `currentState`, so without this check every
/// later pass re-proposes each satisfied-unconsumed transition — and a
/// consensus rule counting proposals rather than distinct DIDs could then
/// be gamed by one agent re-running its own pass. The `to_state` check
/// matters because two distinct transitions can share identical `requires`
/// guards and therefore identical evidence hashes.
async fn proposal_already_exists<S: ProposalLookup + ?Sized>(
    store: &S,
    transition: &SatisfiedTransition,
) -> bool {
    use crate::types::LinkQuery;
    let literal = |s: &str| format!("literal:string:{}", urlencoding::encode(s));
    // Every lookup below fails CLOSED (treat as already-proposed): a missed
    // mint on a transient store error is recovered on the next pass, while a
    // duplicate mint is exactly what this function exists to prevent — see
    // the invariant above.
    let hash_links = match store
        .get_proposal_links(&LinkQuery {
            predicate: Some("ad4m://flow/evidence_hashes".into()),
            target: Some(literal(&transition.evidence_hash)),
            ..Default::default()
        })
        .await
    {
        Ok(links) => links,
        Err(e) => {
            log::warn!(
                "proposal_already_exists: evidence-hash lookup failed ({e:#}); \
                 treating as already-proposed (fail-closed, skipping mint)"
            );
            return true;
        }
    };
    for link in &hash_links {
        let proposal_uri = &link.data.source;
        let links_to = |predicate: &'static str, want: String| async move {
            store
                .get_proposal_links(&LinkQuery {
                    source: Some(proposal_uri.clone()),
                    predicate: Some(predicate.into()),
                    ..Default::default()
                })
                .await
                .map(|links| links.iter().any(|l| l.data.target == want))
        };
        let fail_closed = |e: anyhow::Error| {
            log::warn!(
                "proposal_already_exists: candidate lookup on {proposal_uri} failed \
                 ({e:#}); treating as already-proposed (fail-closed, skipping mint)"
            );
        };
        match links_to("ad4m://flow/instance", transition.instance_uri.clone()).await {
            Ok(false) => continue,
            Ok(true) => {}
            Err(e) => {
                fail_closed(e);
                return true;
            }
        }
        match links_to("ad4m://flow/to_state", literal(&transition.to_state)).await {
            Ok(true) => return true,
            Ok(false) => {}
            Err(e) => {
                fail_closed(e);
                return true;
            }
        }
    }
    false
}

/// The one perspective call the idempotency check needs, behind a trait so
/// its fail-closed error path can be unit-tested against a stub.
#[async_trait]
pub trait ProposalLookup: Send + Sync {
    async fn get_proposal_links(
        &self,
        query: &crate::types::LinkQuery,
    ) -> Result<Vec<crate::types::DecoratedLinkExpression>>;
}

#[async_trait]
impl ProposalLookup for PerspectiveInstance {
    async fn get_proposal_links(
        &self,
        query: &crate::types::LinkQuery,
    ) -> Result<Vec<crate::types::DecoratedLinkExpression>> {
        self.get_links(query).await
    }
}

/// Write one proposal inside its own batch, so readers never see a
/// half-written proposal and one failed write does not roll back the rest.
async fn write_proposal(
    perspective: &mut PerspectiveInstance,
    transition: &SatisfiedTransition,
    proposer_did: &str,
    rationale: Option<&str>,
    context: &AgentContext,
) -> Result<String> {
    let batch_id = perspective.create_batch().await;
    let written = write_flow_transition_proposal(
        perspective,
        &uuid::Uuid::new_v4().to_string(),
        proposer_did,
        &transition.instance_uri,
        &transition.from_state,
        &transition.to_state,
        &transition.evidence_ids,
        &transition.evidence_hash,
        rationale,
        Some(batch_id.clone()),
        context,
    )
    .await;
    let committed = match written {
        Ok(uri) => perspective
            .commit_batch(batch_id.clone(), context)
            .await
            .map(|_| uri)
            .map_err(|e| anyhow!("commit_batch failed: {e:#}")),
        Err(e) => Err(e),
    };
    if committed.is_err() {
        perspective.discard_batch(&batch_id).await;
    }
    committed
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::shacl_parser::{FlowState, FlowTransition};
    use std::collections::BTreeMap;
    use std::sync::Mutex;

    fn mq(class: &str) -> ModelQuery {
        ModelQuery {
            class_name: class.to_string(),
            ..Default::default()
        }
    }

    fn with_where(mut q: ModelQuery, pairs: Vec<(&str, PropertyCondition)>) -> ModelQuery {
        q.r#where = Some(
            pairs
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect::<BTreeMap<_, _>>(),
        );
        q
    }

    fn count(min: Option<u32>, max: Option<u32>) -> Option<ModelQueryCount> {
        Some(ModelQueryCount { min, max })
    }

    #[test]
    fn evidence_hash_is_order_independent_and_content_sensitive() {
        let classes = vec!["ns://A".to_string()];
        let a = evidence_hash(&classes, &["b".into(), "a".into(), "c".into()]);
        let b = evidence_hash(&classes, &["c".into(), "a".into(), "b".into()]);
        assert_eq!(a, b);
        assert_eq!(a.len(), 64, "hex-encoded SHA256");
        assert_ne!(a, evidence_hash(&classes, &["a".into(), "b".into()]));
        assert_ne!(
            a,
            evidence_hash(&["ns://B".into()], &["a".into(), "b".into(), "c".into()])
        );
    }

    #[test]
    fn cardinality_bounds_are_inclusive_and_default_to_at_least_one() {
        assert!(!cardinality_satisfied(None, 0));
        assert!(cardinality_satisfied(None, 1));
        let range = count(Some(1), Some(3));
        assert!(!cardinality_satisfied(range.as_ref(), 0));
        assert!(cardinality_satisfied(range.as_ref(), 1));
        assert!(cardinality_satisfied(range.as_ref(), 3));
        assert!(!cardinality_satisfied(range.as_ref(), 4));
        let negative = count(None, Some(0));
        assert!(cardinality_satisfied(negative.as_ref(), 0));
        assert!(!cardinality_satisfied(negative.as_ref(), 1));
        assert!(cardinality_satisfied(count(None, None).as_ref(), 0));
    }

    #[test]
    fn query_input_translates_scalars_operators_and_did_property() {
        assert_eq!(
            requires_query_input(&mq("ns://T"), "did:key:x").unwrap(),
            json!({}),
            "bare class → no filter"
        );
        let mut q = with_where(
            mq("ns://T"),
            vec![
                ("state", PropertyCondition::Str("done".into())),
                ("priority", PropertyCondition::Num(3.0)),
                ("archived", PropertyCondition::Bool(false)),
                (
                    "owner",
                    PropertyCondition::Equals {
                        equals: json!("alice"),
                    },
                ),
                (
                    "tag",
                    PropertyCondition::In {
                        one_of: vec![json!("a"), json!("b")],
                    },
                ),
            ],
        );
        q.did_property = Some("author".into());
        assert_eq!(
            requires_query_input(&q, "did:key:acting").unwrap(),
            json!({ "where": {
                "state": "done",
                "priority": 3.0,
                "archived": false,
                "owner": "alice",
                "tag": ["a", "b"],
                "author": "did:key:acting",
            }})
        );
    }

    #[test]
    fn query_input_nests_or_branches() {
        let leaf = |role: &str| {
            with_where(
                mq("ns://M"),
                vec![("role", PropertyCondition::Str(role.into()))],
            )
        };
        let mut inner = mq("ns://M");
        inner.or = Some(vec![leaf("admin")]);
        let mut outer = with_where(
            mq("ns://M"),
            vec![("channel", PropertyCondition::Str("c".into()))],
        );
        outer.or = Some(vec![leaf("owner"), inner]);
        assert_eq!(
            requires_query_input(&outer, "did:key:x").unwrap(),
            json!({ "where": {
                "channel": "c",
                "OR": [ { "role": "owner" }, { "OR": [ { "role": "admin" } ] } ],
            }})
        );
        let mut empty_or = mq("ns://M");
        empty_or.or = Some(vec![]);
        assert_eq!(
            requires_query_input(&empty_or, "did:key:x").unwrap(),
            json!({})
        );
    }

    #[test]
    fn query_input_rejects_conditions_model_query_cannot_express() {
        let exists = with_where(
            mq("ns://T"),
            vec![("deletedAt", PropertyCondition::Exists { exists: false })],
        );
        assert!(requires_query_input(&exists, "did:key:x").is_err());
        let matches = with_where(
            mq("ns://T"),
            vec![(
                "title",
                PropertyCondition::Matches {
                    matches: "^Q".into(),
                },
            )],
        );
        assert!(requires_query_input(&matches, "did:key:x").is_err());
    }

    /// Canned `model_query` keyed by class name; records every call.
    #[derive(Default)]
    struct StubPerspective {
        calls: Mutex<Vec<(String, String)>>,
        responses: HashMap<String, Result<Vec<Value>, String>>,
    }

    impl StubPerspective {
        fn with_instances(mut self, class: &str, ids: &[&str]) -> Self {
            self.with_instance_objects(class, ids.iter().map(|id| json!({ "id": id })).collect())
        }
        fn with_instance_objects(mut self, class: &str, objects: Vec<Value>) -> Self {
            self.responses.insert(class.into(), Ok(objects));
            self
        }
        fn with_error(mut self, class: &str, msg: &str) -> Self {
            self.responses.insert(class.into(), Err(msg.into()));
            self
        }
        fn calls_for(&self, class: &str) -> Vec<String> {
            self.calls
                .lock()
                .unwrap()
                .iter()
                .filter(|(c, _)| c == class)
                .map(|(_, q)| q.clone())
                .collect()
        }
    }

    #[async_trait]
    impl RequiresQueryable for StubPerspective {
        async fn model_query(&self, class_name: &str, query_json: &str) -> Result<String> {
            self.calls
                .lock()
                .unwrap()
                .push((class_name.to_string(), query_json.to_string()));
            match self.responses.get(class_name) {
                Some(Ok(objects)) => Ok(json!({
                    "instances": objects,
                    "totalCount": objects.len(),
                })
                .to_string()),
                Some(Err(msg)) => Err(anyhow!(msg.clone())),
                None => Err(anyhow!("no canned response for `{class_name}`")),
            }
        }
    }

    fn state(name: &str, requires: Option<Vec<ModelQuery>>) -> FlowState {
        FlowState {
            name: name.to_string(),
            value: 0.0,
            interpretation_hint: None,
            requires,
            semantic_check: None,
            consensus_rule: None,
        }
    }

    /// `from → to` flow whose `to` state carries `requires`.
    fn flow(name: &str, from: &str, to: &str, requires: Option<Vec<ModelQuery>>) -> SHACLFlow {
        SHACLFlow {
            name: name.to_string(),
            namespace: format!("{}://", name.to_lowercase()),
            states: vec![state(from, None), state(to, requires)],
            transitions: vec![FlowTransition {
                action_name: format!("{from}To{to}"),
                from_state: from.to_string(),
                to_state: to.to_string(),
                actions: Vec::new(),
            }],
            interpretation_hint: None,
            input_types: Vec::new(),
            output_types: Vec::new(),
            creation_hint: None,
            context: None,
            consensus_rule: None,
        }
    }

    fn record(flow_uri: &str, instance: &str, state: &str) -> FlowInstanceRecord {
        FlowInstanceRecord {
            flow_uri: flow_uri.into(),
            instance_uri: instance.into(),
            subject: "ad4m://subject".into(),
            current_state: state.into(),
            created_at: None,
        }
    }

    const DELIVERY: &str = "delivery://DeliveryFlow";

    fn delivery(requires: Vec<ModelQuery>) -> HashMap<String, SHACLFlow> {
        HashMap::from([(
            DELIVERY.to_string(),
            flow("Delivery", "identified", "scoped", Some(requires)),
        )])
    }

    #[tokio::test]
    async fn satisfied_guard_yields_one_transition_with_sealed_evidence() {
        let mut flows = delivery(vec![mq("ns://Task")]);
        flows.get_mut(DELIVERY).unwrap().states[1].semantic_check = Some("Agreed?".into());
        let recs = vec![record(DELIVERY, "ad4m://flow/instance/1", "identified")];
        let stub = StubPerspective::default().with_instances("ns://Task", &["ad4m://task/1"]);
        let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:x").await;
        assert_eq!(
            out,
            vec![SatisfiedTransition {
                flow_name: "Delivery".into(),
                instance_uri: "ad4m://flow/instance/1".into(),
                from_state: "identified".into(),
                to_state: "scoped".into(),
                evidence_ids: vec!["ad4m://task/1".into()],
                evidence: vec![EvidenceItem {
                    id: "ad4m://task/1".into(),
                    class_name: "ns://Task".into(),
                    content: json!({ "id": "ad4m://task/1" }).to_string(),
                }],
                evidence_hash: evidence_hash(&["ns://Task".into()], &["ad4m://task/1".into()]),
                semantic_check: Some("Agreed?".into()),
            }]
        );
    }

    /// The hydration contract: whatever JSON `model_query` returned for a
    /// matched instance rides along on the transition, so the semantic
    /// check can reason over property values instead of bare URIs.
    #[tokio::test]
    async fn evidence_is_hydrated_with_instance_content() {
        let flows = delivery(vec![mq("ns://Task")]);
        let recs = vec![record(DELIVERY, "ad4m://flow/instance/1", "identified")];
        let stub = StubPerspective::default().with_instance_objects(
            "ns://Task",
            vec![json!({
                "id": "ad4m://task/1",
                "title": "Ship parser",
                "body": "We agreed on the scope."
            })],
        );
        let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:x").await;
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].evidence.len(), 1);
        assert_eq!(out[0].evidence[0].id, "ad4m://task/1");
        assert_eq!(out[0].evidence[0].class_name, "ns://Task");
        assert!(out[0].evidence[0]
            .content
            .contains("We agreed on the scope."));
        // Hash contract untouched: still a function of class names + IDs.
        assert_eq!(
            out[0].evidence_hash,
            evidence_hash(&["ns://Task".into()], &["ad4m://task/1".into()])
        );
    }

    #[tokio::test]
    async fn unsatisfied_guardless_and_unknown_flow_yield_nothing() {
        let unsatisfied = delivery(vec![mq("ns://Task")]);
        let stub = StubPerspective::default().with_instances("ns://Task", &[]);
        let recs = vec![record(DELIVERY, "ad4m://flow/instance/1", "identified")];
        assert!(
            evaluate_flow_transitions(&stub, &recs, &unsatisfied, "did:key:x")
                .await
                .is_empty()
        );

        let guardless = HashMap::from([(
            DELIVERY.to_string(),
            flow("Delivery", "identified", "scoped", None),
        )]);
        let stub = StubPerspective::default();
        assert!(
            evaluate_flow_transitions(&stub, &recs, &guardless, "did:key:x")
                .await
                .is_empty()
        );
        assert!(stub.calls.lock().unwrap().is_empty(), "no guard → no query");

        let unknown = vec![record("unknown://Flow", "ad4m://flow/instance/2", "x")];
        assert!(
            evaluate_flow_transitions(&stub, &unknown, &guardless, "did:key:x")
                .await
                .is_empty()
        );
    }

    #[tokio::test]
    async fn requires_is_an_and_that_short_circuits_and_dedups_evidence() {
        let flows = delivery(vec![mq("ns://A"), mq("ns://B"), mq("ns://A")]);
        let recs = vec![record(DELIVERY, "ad4m://flow/instance/1", "identified")];

        let stub = StubPerspective::default()
            .with_instances("ns://A", &["x/1", "x/2"])
            .with_instances("ns://B", &["x/2", "x/3"]);
        let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:x").await;
        assert_eq!(out[0].evidence_ids, vec!["x/1", "x/2", "x/3"]);
        assert_eq!(
            out[0].evidence_hash,
            evidence_hash(&["ns://A".into(), "ns://B".into()], &out[0].evidence_ids)
        );

        let stub = StubPerspective::default()
            .with_instances("ns://A", &["x/1"])
            .with_instances("ns://B", &[]);
        assert!(evaluate_flow_transitions(&stub, &recs, &flows, "did:key:x")
            .await
            .is_empty());
        assert_eq!(
            stub.calls_for("ns://A").len(),
            1,
            "third guard never runs after B misses"
        );
    }

    #[tokio::test]
    async fn cardinality_and_translated_query_are_applied_per_guard() {
        let mut q = with_where(
            mq("ns://T"),
            vec![("author", PropertyCondition::Str("did:key:a".into()))],
        );
        q.count = count(Some(2), Some(3));
        let flows = delivery(vec![q]);
        let recs = vec![record(DELIVERY, "ad4m://flow/instance/1", "identified")];

        let stub = StubPerspective::default().with_instances("ns://T", &["a", "b", "c", "d"]);
        assert!(evaluate_flow_transitions(&stub, &recs, &flows, "did:key:x")
            .await
            .is_empty());
        let sent: Value = serde_json::from_str(&stub.calls_for("ns://T")[0]).unwrap();
        assert_eq!(sent, json!({ "where": { "author": "did:key:a" } }));

        let stub = StubPerspective::default().with_instances("ns://T", &["a", "b"]);
        assert_eq!(
            evaluate_flow_transitions(&stub, &recs, &flows, "did:key:x")
                .await
                .len(),
            1
        );
    }

    #[tokio::test]
    async fn a_failing_guard_skips_only_its_own_transition() {
        let mut flows = delivery(vec![mq("ns://Broken")]);
        flows.insert(
            "deliberation://Flow".into(),
            flow(
                "Deliberation",
                "proposal",
                "tension",
                Some(vec![mq("ns://Perspective")]),
            ),
        );
        let recs = vec![
            record(DELIVERY, "ad4m://flow/instance/1", "identified"),
            record("deliberation://Flow", "ad4m://flow/instance/2", "proposal"),
        ];
        let stub = StubPerspective::default()
            .with_error("ns://Broken", "unregistered class")
            .with_instances("ns://Perspective", &["p/1"]);
        let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:x").await;
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].flow_name, "Deliberation");

        let untranslatable = delivery(vec![with_where(
            mq("ns://T"),
            vec![(
                "title",
                PropertyCondition::Matches {
                    matches: "^Q".into(),
                },
            )],
        )]);
        let stub = StubPerspective::default().with_instances("ns://T", &["t/1"]);
        assert!(
            evaluate_flow_transitions(&stub, &recs, &untranslatable, "did:key:x")
                .await
                .is_empty()
        );
        assert!(
            stub.calls.lock().unwrap().is_empty(),
            "untranslatable guard never reaches model_query"
        );
    }

    /// Idempotency must fail CLOSED: a store error during the
    /// already-proposed lookup means "skip the mint", never "mint another".
    /// A missed mint is recovered on the next pass; a duplicate mint is the
    /// bug this check exists to prevent (proposal-count consensus gaming).
    mod proposal_lookup_fail_closed {
        use super::*;
        use crate::types::{DecoratedLinkExpression, LinkQuery};

        /// Scripted store: `None` for a predicate = that lookup errors.
        struct ScriptedStore {
            by_predicate: HashMap<String, Option<Vec<DecoratedLinkExpression>>>,
        }

        #[async_trait]
        impl ProposalLookup for ScriptedStore {
            async fn get_proposal_links(
                &self,
                query: &LinkQuery,
            ) -> Result<Vec<DecoratedLinkExpression>> {
                let predicate = query.predicate.clone().unwrap_or_default();
                match self.by_predicate.get(&predicate) {
                    Some(Some(links)) => Ok(links.clone()),
                    Some(None) => Err(anyhow!("transient store error")),
                    None => Ok(Vec::new()),
                }
            }
        }

        fn transition() -> SatisfiedTransition {
            SatisfiedTransition {
                flow_name: "Delivery".into(),
                instance_uri: "ad4m://flow/instance/1".into(),
                from_state: "identified".into(),
                to_state: "scoped".into(),
                evidence_ids: vec!["ad4m://task/1".into()],
                evidence: Vec::new(),
                evidence_hash: "hash".into(),
                semantic_check: None,
            }
        }

        #[tokio::test]
        async fn hash_lookup_error_reports_already_proposed() {
            let store = ScriptedStore {
                by_predicate: HashMap::from([("ad4m://flow/evidence_hashes".to_string(), None)]),
            };
            assert!(
                proposal_already_exists(&store, &transition()).await,
                "a failed evidence-hash lookup must skip the mint, not duplicate it"
            );
        }

        fn link(source: &str, predicate: &str, target: &str) -> DecoratedLinkExpression {
            DecoratedLinkExpression {
                author: "did:key:test".into(),
                timestamp: "2026-01-01T00:00:00Z".into(),
                data: crate::types::Link {
                    source: source.into(),
                    predicate: Some(predicate.into()),
                    target: target.into(),
                },
                proof: crate::types::DecoratedExpressionProof {
                    key: String::new(),
                    signature: String::new(),
                    valid: None,
                    invalid: None,
                },
                status: None,
            }
        }

        #[tokio::test]
        async fn candidate_lookup_error_reports_already_proposed() {
            let hash_link = link(
                "proposal://1",
                "ad4m://flow/evidence_hashes",
                "literal:string:hash",
            );
            let store = ScriptedStore {
                by_predicate: HashMap::from([
                    (
                        "ad4m://flow/evidence_hashes".to_string(),
                        Some(vec![hash_link]),
                    ),
                    // Candidate instance lookup errors.
                    ("ad4m://flow/instance".to_string(), None),
                ]),
            };
            assert!(proposal_already_exists(&store, &transition()).await);
        }

        #[tokio::test]
        async fn empty_store_reports_not_proposed() {
            let store = ScriptedStore {
                by_predicate: HashMap::new(),
            };
            assert!(!proposal_already_exists(&store, &transition()).await);
        }
    }
}
