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
//! String values in `where` are substituted at evaluation time:
//! `$flow.base` → the instance's subject, `$flow.uri` / `$flow.instance`
//! → the instance URI, `$did` → the acting DID. `linkedTo` compiles to a
//! `parent` scope; the `"base"`/`"flow"` shorthand uses
//! `ad4m://has_child`, and `{ via, to }` names the predicate.
//!
//! Every failure here is a skip, never an error: a broken flow
//! definition, an unregistered class or a transient query failure drops
//! one transition and the extraction pass carries on. Untranslatable
//! guards (`exists`/`matches`, a colliding `didProperty`, a malformed
//! `linkedTo`) log at warn — they will never start working on retry.

use crate::agent::AgentContext;
use crate::perspectives::flow_classes::write_flow_transition_proposal;
use crate::perspectives::flow_context::{
    load_flow_instances, load_shacl_flows, reachable_next_states, FlowInstanceRecord, FlowTokens,
};
use crate::perspectives::model_query::ModelQueryInput;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::{
    ModelQuery, ModelQueryCount, PropertyCondition, SHACLFlow,
};
use crate::types::LinkQuery;
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
    /// See [`evidence_hash`].
    pub evidence_hash: String,
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

/// Default collection predicate used when `linkedTo` is the `"base"` /
/// `"flow"` shorthand. Authors who need a different edge write
/// `{ via, to }` instead.
const LINKED_TO_DEFAULT_PREDICATE: &str = "ad4m://has_child";

/// Substitute `$flow.base`, `$flow.uri` / `$flow.instance`, and `$did`
/// in a `where` string. Delegates to [`FlowTokens::substitute`] — the
/// single definition of the token set.
fn substitute_tokens(s: &str, record: &FlowInstanceRecord, acting_did: &str) -> String {
    let tokens = FlowTokens {
        subject: &record.subject,
        instance_uri: &record.instance_uri,
        did: acting_did,
    };
    tokens.substitute(s)
}

fn substitute_json(value: &Value, record: &FlowInstanceRecord, acting_did: &str) -> Value {
    match value {
        Value::String(s) => Value::String(substitute_tokens(s, record, acting_did)),
        Value::Array(items) => Value::Array(
            items
                .iter()
                .map(|v| substitute_json(v, record, acting_did))
                .collect(),
        ),
        other => other.clone(),
    }
}

/// Translate a flow-side `ModelQuery` into the JSON input `model_query`
/// accepts. `didProperty` becomes `where.<prop> = acting_did`; `or`
/// alternatives become an `OR` list of sub-clauses; `linkedTo` becomes
/// a `parent` scope.
///
/// Scalars, `equals` and `in` map directly onto `WhereCondition`.
/// `exists` and `matches` have no `model_query` counterpart yet, so a
/// guard using them fails translation and is skipped instead of being
/// evaluated against a wrong query.
fn requires_query_input(
    query: &ModelQuery,
    record: &FlowInstanceRecord,
    acting_did: &str,
) -> Result<Value> {
    let where_clause = requires_where(query, record, acting_did, false)?;
    let mut out = Map::new();
    if !where_clause.is_empty() {
        out.insert("where".into(), Value::Object(where_clause));
    }
    if let Some(linked) = &query.linked_to {
        out.insert("parent".into(), linked_to_parent(linked, record)?);
    }
    let out = Value::Object(out);
    serde_json::from_value::<ModelQueryInput>(out.clone())
        .map_err(|e| anyhow!("translated query is not a valid ModelQueryInput: {e}"))?;
    Ok(out)
}

fn requires_where(
    query: &ModelQuery,
    record: &FlowInstanceRecord,
    acting_did: &str,
    nested: bool,
) -> Result<Map<String, Value>> {
    if nested && query.linked_to.is_some() {
        bail!("`linkedTo` on an `or` branch is not supported by model_query");
    }
    let mut out = Map::new();
    for (field, cond) in query.r#where.iter().flatten() {
        out.insert(
            field.clone(),
            where_condition(field, cond, record, acting_did)?,
        );
    }
    if let Some(prop) = &query.did_property {
        if out.contains_key(prop) {
            bail!("`didProperty` `{prop}` collides with an existing `where` field");
        }
        out.insert(prop.clone(), Value::String(acting_did.to_string()));
    }
    if let Some(alts) = query.or.as_ref().filter(|a| !a.is_empty()) {
        let branches = alts
            .iter()
            .map(|alt| {
                if alt.class_name != query.class_name {
                    bail!(
                        "`or` branch class `{}` must match the outer class `{}`",
                        alt.class_name,
                        query.class_name
                    );
                }
                if alt.count.is_some() {
                    bail!("`count` on an `or` branch is not supported");
                }
                requires_where(alt, record, acting_did, true).map(Value::Object)
            })
            .collect::<Result<Vec<_>>>()?;
        out.insert("OR".to_string(), Value::Array(branches));
    }
    Ok(out)
}

fn where_condition(
    field: &str,
    cond: &PropertyCondition,
    record: &FlowInstanceRecord,
    acting_did: &str,
) -> Result<Value> {
    Ok(match cond {
        PropertyCondition::Str(s) => json!(substitute_tokens(s, record, acting_did)),
        PropertyCondition::Num(n) => json!(n),
        PropertyCondition::Bool(b) => json!(b),
        PropertyCondition::Equals { equals } => substitute_json(equals, record, acting_did),
        PropertyCondition::In { one_of } => Value::Array(
            one_of
                .iter()
                .map(|v| substitute_json(v, record, acting_did))
                .collect(),
        ),
        PropertyCondition::Exists { .. } => {
            bail!("`{field}`: `exists` is not supported by model_query")
        }
        PropertyCondition::Matches { .. } => {
            bail!("`{field}`: `matches` is not supported by model_query")
        }
    })
}

fn linked_to_parent(linked: &Value, record: &FlowInstanceRecord) -> Result<Value> {
    let (id, predicate) = match linked {
        Value::String(s) => {
            let id = match s.as_str() {
                "base" => record.subject.as_str(),
                "flow" => record.instance_uri.as_str(),
                other => bail!("`linkedTo` `{other}` is not `base` or `flow`"),
            };
            (id, LINKED_TO_DEFAULT_PREDICATE)
        }
        Value::Object(obj) => {
            let via = obj
                .get("via")
                .and_then(Value::as_str)
                .ok_or_else(|| anyhow!("`linkedTo` object needs a string `via` predicate"))?;
            let to = obj
                .get("to")
                .and_then(Value::as_str)
                .ok_or_else(|| anyhow!("`linkedTo` object needs `to` of `base` or `flow`"))?;
            let id = match to {
                "base" => record.subject.as_str(),
                "flow" => record.instance_uri.as_str(),
                other => bail!("`linkedTo.to` `{other}` is not `base` or `flow`"),
            };
            (id, via)
        }
        _ => bail!("`linkedTo` must be \"base\", \"flow\", or {{ via, to }}"),
    };
    if id.is_empty() {
        bail!("`linkedTo` anchor resolved to an empty string");
    }
    Ok(json!({ "id": id, "predicate": predicate }))
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

/// Outcome of AND-ing a state's `requires`. Translation failures are
/// split from query failures so the composer can `warn!` the former
/// (persistent misconfig) and `debug!` the latter (transient).
enum RequiresResult {
    Satisfied(Vec<String>, Vec<String>),
    Unmet,
    Untranslatable(anyhow::Error),
    QueryFailed(anyhow::Error),
}

/// Run one already-translated guard query. Returns the matched IDs.
async fn run_query<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    class_name: &str,
    input: &Value,
) -> Result<Vec<String>> {
    let raw = perspective
        .model_query(class_name, &input.to_string())
        .await?;
    let result: Value = serde_json::from_str(&raw)?;
    let ids = result
        .get("instances")
        .and_then(Value::as_array)
        .ok_or_else(|| anyhow!("model_query for `{class_name}` returned no `instances` array"))?
        .iter()
        .filter_map(|inst| inst.get("id").and_then(Value::as_str))
        .map(str::to_string)
        .collect();
    Ok(ids)
}

/// AND across a state's `requires`. Unmet as soon as one guard misses;
/// `Satisfied` (class names and evidence IDs, both deduplicated in
/// first-seen order) when every guard holds.
async fn evaluate_requires<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    requires: &[ModelQuery],
    record: &FlowInstanceRecord,
    acting_did: &str,
) -> RequiresResult {
    let mut class_names: Vec<String> = Vec::new();
    let mut evidence_ids: Vec<String> = Vec::new();
    for query in requires {
        let input = match requires_query_input(query, record, acting_did) {
            Ok(v) => v,
            Err(e) => return RequiresResult::Untranslatable(e),
        };
        let ids = match run_query(perspective, &query.class_name, &input).await {
            Ok(ids) => ids,
            Err(e) => return RequiresResult::QueryFailed(e),
        };
        if !cardinality_satisfied(query.count.as_ref(), ids.len()) {
            return RequiresResult::Unmet;
        }
        if !class_names.contains(&query.class_name) {
            class_names.push(query.class_name.clone());
        }
        for id in ids {
            if !evidence_ids.contains(&id) {
                evidence_ids.push(id);
            }
        }
    }
    RequiresResult::Satisfied(class_names, evidence_ids)
}

/// Walk every record's reachable next-states and collect the ones whose
/// `requires` guard holds. Records whose flow is unknown and states without
/// a guard are skipped. A query error skips that one transition (`debug!`);
/// an untranslatable guard is the same skip at `warn!`.
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
            match evaluate_requires(perspective, requires, record, acting_did).await {
                RequiresResult::Satisfied(class_names, evidence_ids) => {
                    out.push(SatisfiedTransition {
                        flow_name: flow.name.clone(),
                        instance_uri: record.instance_uri.clone(),
                        from_state: record.current_state.clone(),
                        to_state: state.name.clone(),
                        evidence_hash: evidence_hash(&class_names, &evidence_ids),
                        evidence_ids,
                    })
                }
                RequiresResult::Unmet => {}
                RequiresResult::Untranslatable(e) => log::warn!(
                    "flow evaluator: untranslatable {}.{} on {}: {e:#}",
                    flow.name,
                    state.name,
                    record.instance_uri
                ),
                RequiresResult::QueryFailed(e) => log::debug!(
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

/// Load → evaluate → write, called by the extraction pass once its own
/// writes are committed. `subjects` narrows the FlowInstance load to the
/// given base URIs; an empty slice returns immediately — the extraction
/// pass wrote nothing, so there are no flow instances to re-evaluate.
/// Returns the URIs of the proposals minted. Never fails: loader errors
/// yield an empty result and a failed write drops only that proposal.
pub async fn run_engine_proposal_pass(
    perspective: &mut PerspectiveInstance,
    subjects: &[String],
    context: &AgentContext,
) -> Vec<String> {
    if subjects.is_empty() {
        return Vec::new();
    }
    let loaded = async {
        let flows_by_uri = load_shacl_flows(perspective).await?;
        let records = load_flow_instances(perspective, subjects).await?;
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
        if proposal_already_exists(perspective, transition).await {
            log::debug!(
                "run_engine_proposal_pass: {}.{}→{} already proposed, skipping",
                transition.flow_name,
                transition.from_state,
                transition.to_state
            );
            continue;
        }
        match write_proposal(perspective, transition, &acting_did, context).await {
            Ok(uri) => minted.push(uri),
            Err(e) => log::debug!(
                "run_engine_proposal_pass: {}.{}→{} not written: {e:#}",
                transition.flow_name,
                transition.from_state,
                transition.to_state
            ),
        }
    }
    minted
}

/// Check whether a proposal with the same evidence hash already exists for the
/// same flow instance AND the same target state. Uses link queries: finds
/// proposals carrying the evidence hash, then confirms one links to the same
/// flow instance with the same `to_state`. Without the `to_state` check, two
/// distinct transitions sharing identical requires guards would collide.
async fn proposal_already_exists(
    perspective: &PerspectiveInstance,
    transition: &SatisfiedTransition,
) -> bool {
    let hash_literal = format!(
        "literal:string:{}",
        urlencoding::encode(&transition.evidence_hash)
    );
    let hash_links = match perspective
        .get_links(&LinkQuery {
            predicate: Some("ad4m://flow/evidence_hashes".into()),
            target: Some(hash_literal),
            ..Default::default()
        })
        .await
    {
        Ok(links) => links,
        Err(_) => return false,
    };
    let to_state_literal = format!(
        "literal:string:{}",
        urlencoding::encode(&transition.to_state)
    );
    for link in &hash_links {
        let proposal_uri = &link.data.source;
        let instance_links = match perspective
            .get_links(&LinkQuery {
                source: Some(proposal_uri.clone()),
                predicate: Some("ad4m://flow/instance".into()),
                ..Default::default()
            })
            .await
        {
            Ok(links) => links,
            Err(_) => continue,
        };
        let matches_instance = instance_links
            .iter()
            .any(|l| l.data.target == transition.instance_uri);
        if !matches_instance {
            continue;
        }
        let to_state_links = match perspective
            .get_links(&LinkQuery {
                source: Some(proposal_uri.clone()),
                predicate: Some("ad4m://flow/to_state".into()),
                ..Default::default()
            })
            .await
        {
            Ok(links) => links,
            Err(_) => continue,
        };
        if to_state_links
            .iter()
            .any(|l| l.data.target == to_state_literal)
        {
            return true;
        }
    }
    false
}

/// Write one proposal inside its own batch, so readers never see a
/// half-written proposal and one failed write does not roll back the rest.
async fn write_proposal(
    perspective: &mut PerspectiveInstance,
    transition: &SatisfiedTransition,
    proposer_did: &str,
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
        None,
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

    fn inst() -> FlowInstanceRecord {
        FlowInstanceRecord {
            flow_uri: "delivery://DeliveryFlow".into(),
            instance_uri: "ad4m://flow/instance/1".into(),
            subject: "ad4m://task/onboarding".into(),
            current_state: "identified".into(),
            created_at: None,
        }
    }

    fn qin(q: &ModelQuery, did: &str) -> Value {
        requires_query_input(q, &inst(), did).unwrap()
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
            qin(&mq("ns://T"), "did:key:x"),
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
            qin(&q, "did:key:acting"),
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
            qin(&outer, "did:key:x"),
            json!({ "where": {
                "channel": "c",
                "OR": [ { "role": "owner" }, { "OR": [ { "role": "admin" } ] } ],
            }})
        );
        let mut empty_or = mq("ns://M");
        empty_or.or = Some(vec![]);
        assert_eq!(qin(&empty_or, "did:key:x"), json!({}));
    }

    #[test]
    fn query_input_rejects_conditions_model_query_cannot_express() {
        let exists = with_where(
            mq("ns://T"),
            vec![("deletedAt", PropertyCondition::Exists { exists: false })],
        );
        assert!(requires_query_input(&exists, &inst(), "did:key:x").is_err());
        let matches = with_where(
            mq("ns://T"),
            vec![(
                "title",
                PropertyCondition::Matches {
                    matches: "^Q".into(),
                },
            )],
        );
        assert!(requires_query_input(&matches, &inst(), "did:key:x").is_err());
    }

    #[test]
    fn query_input_substitutes_flow_and_did_tokens() {
        let rec = inst();
        let q = with_where(
            mq("ns://T"),
            vec![
                ("about", PropertyCondition::Str("$flow.base".into())),
                (
                    "on",
                    PropertyCondition::Equals {
                        equals: json!("$flow.uri"),
                    },
                ),
                (
                    "alsoOn",
                    PropertyCondition::In {
                        one_of: vec![json!("$flow.instance"), json!("other")],
                    },
                ),
                ("author", PropertyCondition::Str("$did".into())),
            ],
        );
        assert_eq!(
            requires_query_input(&q, &rec, "did:key:acting").unwrap(),
            json!({ "where": {
                "about": "ad4m://task/onboarding",
                "on": "ad4m://flow/instance/1",
                "alsoOn": ["ad4m://flow/instance/1", "other"],
                "author": "did:key:acting",
            }})
        );
    }

    #[test]
    fn query_input_bails_when_did_property_collides_with_where() {
        let mut q = with_where(
            mq("ns://T"),
            vec![("author", PropertyCondition::Str("alice".into()))],
        );
        q.did_property = Some("author".into());
        let err = requires_query_input(&q, &inst(), "did:key:x").unwrap_err();
        assert!(err.to_string().contains("collides"), "got {err:#}");
    }

    #[test]
    fn query_input_compiles_linked_to_into_parent_scope() {
        let rec = inst();
        let mut base = mq("ns://T");
        base.linked_to = Some(json!("base"));
        assert_eq!(
            requires_query_input(&base, &rec, "did:key:x").unwrap(),
            json!({ "parent": {
                "id": "ad4m://task/onboarding",
                "predicate": "ad4m://has_child",
            }})
        );
        let mut flow = mq("ns://T");
        flow.linked_to = Some(json!({ "via": "ns://about", "to": "flow" }));
        assert_eq!(
            requires_query_input(&flow, &rec, "did:key:x").unwrap(),
            json!({ "parent": {
                "id": "ad4m://flow/instance/1",
                "predicate": "ns://about",
            }})
        );
        let mut bad = mq("ns://T");
        bad.linked_to = Some(json!(42));
        assert!(requires_query_input(&bad, &rec, "did:key:x").is_err());
        let mut nested = mq("ns://T");
        nested.or = Some(vec![{
            let mut branch = mq("ns://T");
            branch.linked_to = Some(json!("base"));
            branch
        }]);
        assert!(requires_query_input(&nested, &rec, "did:key:x").is_err());
    }

    #[test]
    fn or_branch_with_different_class_is_rejected() {
        let branch = mq("ns://Other");
        let mut q = mq("ns://T");
        q.or = Some(vec![branch]);
        let err = requires_query_input(&q, &inst(), "did:key:x").unwrap_err();
        assert!(
            err.to_string().contains("must match the outer class"),
            "got {err:#}"
        );
    }

    #[test]
    fn or_branch_with_own_count_is_rejected() {
        let mut branch = mq("ns://T");
        branch.count = count(Some(2), None);
        let mut q = mq("ns://T");
        q.or = Some(vec![branch]);
        let err = requires_query_input(&q, &inst(), "did:key:x").unwrap_err();
        assert!(err.to_string().contains("count"), "got {err:#}");
    }

    #[test]
    fn linked_to_with_empty_subject_is_rejected() {
        let rec = FlowInstanceRecord {
            subject: "".into(),
            ..inst()
        };
        let mut q = mq("ns://T");
        q.linked_to = Some(json!("base"));
        let err = requires_query_input(&q, &rec, "did:key:x").unwrap_err();
        assert!(err.to_string().contains("empty"), "got {err:#}");
    }

    #[test]
    fn query_input_deserialises_as_model_query_input() {
        let mut q = with_where(
            mq("ns://T"),
            vec![
                ("title", PropertyCondition::Str("Onboard Ana".into())),
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
        q.linked_to = Some(json!({ "via": "ns://about", "to": "base" }));
        let value = qin(&q, "did:key:acting");
        let parsed: crate::perspectives::model_query::ModelQueryInput =
            serde_json::from_value(value).expect("translated JSON must be a ModelQueryInput");
        assert!(parsed.where_clause.is_some());
        assert!(parsed.parent.is_some());
    }

    /// Canned `model_query` keyed by class name; records every call.
    #[derive(Default)]
    struct StubPerspective {
        calls: Mutex<Vec<(String, String)>>,
        responses: HashMap<String, Result<Vec<String>, String>>,
    }

    impl StubPerspective {
        fn with_instances(mut self, class: &str, ids: &[&str]) -> Self {
            self.responses.insert(
                class.into(),
                Ok(ids.iter().map(|s| s.to_string()).collect()),
            );
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
                Some(Ok(ids)) => Ok(json!({
                    "instances": ids.iter().map(|id| json!({ "id": id })).collect::<Vec<_>>(),
                    "totalCount": ids.len(),
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
        let flows = delivery(vec![mq("ns://Task")]);
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
                evidence_hash: evidence_hash(&["ns://Task".into()], &["ad4m://task/1".into()]),
            }]
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
}
