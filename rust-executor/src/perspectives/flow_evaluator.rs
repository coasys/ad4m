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
//! instances (IDs + canonicalized content) form the proposal's evidence
//! bag, sealed by an order-independent SHA256 in [`evidence_hash`] so the
//! consensus pass can detect evidence that no longer resolves — or was
//! edited — before firing.
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
    scope_subject, FlowInstanceRecord, FlowTokens,
};
use crate::perspectives::flow_semantic_check::{
    build_semantic_check_prompt, semantic_check_passed, SemanticCheckLlm,
};
use crate::perspectives::interpretation::LlmFlowProposal;
use crate::perspectives::model_query::types::Scope;
use crate::perspectives::model_query::ModelQueryInput;
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
    /// this content, not over bare identifiers, and its canonicalized form
    /// is sealed into [`evidence_hash`]. Editing a cited instance therefore
    /// re-opens the guard under a NEW hash: the mint-side dedup misses and
    /// mints a fresh proposal for the current evidence, while the consensus
    /// pass's pre-fire re-verify invalidates the stale-sealed one — the two
    /// halves of spec §4.2's edit detection.
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

/// Serialize a JSON value with recursively-sorted object keys — a stable
/// form independent of the key order `model_query` (or any serde
/// `preserve_order` setting) happens to produce. Non-JSON content is
/// hashed verbatim rather than dropped.
fn canonical_json(v: &Value) -> String {
    match v {
        Value::Object(map) => {
            let mut keys: Vec<&String> = map.keys().collect();
            keys.sort();
            let body: Vec<String> = keys
                .iter()
                .map(|k| {
                    format!(
                        "{}:{}",
                        Value::String((*k).clone()),
                        canonical_json(&map[*k])
                    )
                })
                .collect();
            format!("{{{}}}", body.join(","))
        }
        Value::Array(items) => {
            let body: Vec<String> = items.iter().map(canonical_json).collect();
            format!("[{}]", body.join(","))
        }
        other => other.to_string(),
    }
}

/// Order-independent seal over a satisfied guard's evidence: SHA256 of the
/// class names joined by `|`, a NUL, then one line per evidence item —
/// `class\0id\0canonical-content` — sorted. Two evaluations of the same
/// guard against the same graph produce the same hash regardless of result
/// order; **editing a cited instance changes the hash** (spec §4.2), which
/// is what lets the consensus pass detect a stale seal before firing.
pub fn evidence_hash(class_names: &[String], evidence: &[EvidenceItem]) -> String {
    let mut lines: Vec<String> = evidence
        .iter()
        .map(|e| {
            let canonical = serde_json::from_str::<Value>(&e.content)
                .map(|v| canonical_json(&v))
                .unwrap_or_else(|_| e.content.clone());
            format!("{}\0{}\0{}", e.class_name, e.id, canonical)
        })
        .collect();
    lines.sort();
    let mut hasher = Sha256::new();
    hasher.update(class_names.join("|"));
    hasher.update(b"\0");
    hasher.update(lines.join("\n"));
    hex::encode(hasher.finalize())
}

/// `count.{min,max}` check with inclusive bounds. An unset `count` means
/// "at least one match"; `{ max: 0 }` is a valid negative guard.
pub(crate) fn cardinality_satisfied(count: Option<&ModelQueryCount>, actual: usize) -> bool {
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
pub(crate) fn requires_query_input(
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

    /// Is this base still carrying an unaccepted interpretation overlay?
    /// Design principle #5: overlays don't count as evidence — only
    /// committed (accepted or human-written) graph state satisfies
    /// `requires`, grants role eligibility, or enters an evidence hash.
    /// Defaults to `false` so pure stubs keep their pre-overlay behavior.
    async fn has_pending_overlay(&self, _base: &str) -> Result<bool> {
        Ok(false)
    }
}

#[async_trait]
impl RequiresQueryable for PerspectiveInstance {
    async fn model_query(&self, class_name: &str, query_json: &str) -> Result<String> {
        PerspectiveInstance::model_query(self, class_name, query_json).await
    }

    async fn has_pending_overlay(&self, base: &str) -> Result<bool> {
        let links = self
            .get_links(&crate::types::LinkQuery {
                source: Some(base.to_string()),
                predicate: Some(
                    crate::perspectives::interpretation::overlay::OVERLAY_KIND_PRED.to_string(),
                ),
                ..Default::default()
            })
            .await?;
        Ok(!links.is_empty())
    }
}

/// Outcome of AND-ing a state's `requires`. Translation failures are
/// split from query failures so the composer can `warn!` the former
/// (persistent misconfig) and `debug!` the latter (transient).
enum RequiresResult {
    Satisfied(Vec<String>, Vec<EvidenceItem>),
    Unmet,
    Untranslatable(anyhow::Error),
    QueryFailed(anyhow::Error),
}

/// Run one already-translated guard query. Returns the matched instances,
/// hydrated with the JSON `model_query` already returned for each (no
/// second read).
pub(crate) async fn run_query<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    class_name: &str,
    input: &Value,
) -> Result<Vec<EvidenceItem>> {
    let raw = perspective
        .model_query(class_name, &input.to_string())
        .await?;
    let result: Value = serde_json::from_str(&raw)?;
    let candidates: Vec<EvidenceItem> = result
        .get("instances")
        .and_then(Value::as_array)
        .ok_or_else(|| anyhow!("model_query for `{class_name}` returned no `instances` array"))?
        .iter()
        .filter_map(|inst| {
            let id = inst.get("id").and_then(Value::as_str)?;
            Some(EvidenceItem {
                id: id.to_string(),
                class_name: class_name.to_string(),
                content: inst.to_string(),
            })
        })
        .collect();
    // Overlay exclusion (design principle #5): an instance still carrying an
    // unaccepted interpretation overlay is not committed graph state — it
    // neither satisfies a guard nor enters the evidence hash. A lookup error
    // propagates (fail closed at the caller) rather than silently counting.
    let mut matched = Vec::with_capacity(candidates.len());
    for item in candidates {
        if perspective.has_pending_overlay(&item.id).await? {
            log::debug!(
                "run_query: excluding `{}` from `{class_name}` evidence — pending overlay",
                item.id
            );
            continue;
        }
        matched.push(item);
    }
    Ok(matched)
}

/// AND across a state's `requires`. Unmet as soon as one guard misses;
/// `Satisfied` (class names and hydrated evidence, both deduplicated in
/// first-seen order, evidence by instance ID) when every guard holds.
async fn evaluate_requires<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    requires: &[ModelQuery],
    record: &FlowInstanceRecord,
    acting_did: &str,
) -> RequiresResult {
    let mut class_names: Vec<String> = Vec::new();
    let mut evidence: Vec<EvidenceItem> = Vec::new();
    for query in requires {
        let input = match requires_query_input(query, record, acting_did) {
            Ok(v) => v,
            Err(e) => return RequiresResult::Untranslatable(e),
        };
        let matched = match run_query(perspective, &query.class_name, &input).await {
            Ok(items) => items,
            Err(e) => return RequiresResult::QueryFailed(e),
        };
        if !cardinality_satisfied(query.count.as_ref(), matched.len()) {
            return RequiresResult::Unmet;
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
    RequiresResult::Satisfied(class_names, evidence)
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
            match evaluate_requires(perspective, requires, record, acting_did).await {
                RequiresResult::Satisfied(class_names, evidence) => {
                    let evidence_ids: Vec<String> = evidence.iter().map(|e| e.id.clone()).collect();
                    out.push(SatisfiedTransition {
                        flow_name: flow.name.clone(),
                        instance_uri: record.instance_uri.clone(),
                        from_state: record.current_state.clone(),
                        to_state: state.name.clone(),
                        evidence_hash: evidence_hash(&class_names, &evidence),
                        evidence_ids,
                        evidence,
                        semantic_check: state.semantic_check.clone(),
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

/// Re-run one target state's `requires` against the CURRENT graph and
/// return the freshly-computed evidence hash — the consensus pass's
/// pre-fire re-verification (firing-engine design §2 step 6).
///
/// - `Ok(Some(hash))` — guard still satisfied; the caller compares with the
///   proposal's sealed hash (mismatch ⇒ a cited instance changed ⇒
///   invalidate, spec §11's hash-verify demo).
/// - `Ok(None)` — nothing verifiable remains: guard no longer satisfied, or
///   the flow/state/guard definition changed out from under the proposal
///   (unknown state, guard now absent, untranslatable). Caller invalidates.
/// - `Err` — transient query/store failure: the caller skips firing this
///   pass (fail closed) WITHOUT invalidating.
///
/// `acting_did` must be the PROPOSER's DID: `$did`-substituted guards
/// resolved against the proposer at mint time, so re-verification must
/// substitute the same identity or the hash could never match.
pub(crate) async fn recompute_evidence_hash<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    flow: &SHACLFlow,
    record: &FlowInstanceRecord,
    to_state: &str,
    acting_did: &str,
) -> Result<Option<String>> {
    let Some(state) = flow.states.iter().find(|s| s.name == to_state) else {
        log::warn!(
            "recompute_evidence_hash: state `{to_state}` no longer exists on flow `{}`",
            flow.name
        );
        return Ok(None);
    };
    let requires = state.requires.as_deref().unwrap_or_default();
    if requires.is_empty() {
        log::warn!(
            "recompute_evidence_hash: `{}.{to_state}` no longer carries a `requires` guard",
            flow.name
        );
        return Ok(None);
    }
    match evaluate_requires(perspective, requires, record, acting_did).await {
        RequiresResult::Satisfied(class_names, evidence) => {
            Ok(Some(evidence_hash(&class_names, &evidence)))
        }
        RequiresResult::Unmet => Ok(None),
        RequiresResult::Untranslatable(e) => {
            log::warn!(
                "recompute_evidence_hash: `{}.{to_state}` became untranslatable: {e:#}",
                flow.name
            );
            Ok(None)
        }
        RequiresResult::QueryFailed(e) => Err(e),
    }
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
    flow_filter: Option<&[String]>,
) -> Vec<String> {
    let loaded = async {
        let mut flows_by_uri = load_shacl_flows(perspective).await?;
        crate::perspectives::flow_context::retain_selected_flows(&mut flows_by_uri, flow_filter);
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

        // Idempotency BEFORE the semantic gate: minting doesn't advance
        // `currentState`, so a satisfied-but-unconsumed transition reappears
        // on every pass — checking duplicates first means it costs two link
        // queries per pass instead of one LLM call per pass.
        if proposal_already_exists(perspective, transition, &acting_did).await {
            log::debug!("run_engine_proposal_pass: {label} already proposed; skipping");
            continue;
        }

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

/// Check whether **this DID's** proposal with the same evidence hash already
/// exists for the same flow instance AND target state. Keeps the pass
/// idempotent: minting does not advance `currentState`, so without this
/// check every later pass re-proposes each satisfied-unconsumed transition.
/// The `to_state` check matters because two distinct transitions can share
/// identical `requires` guards and therefore identical evidence hashes.
///
/// The dedup key carries the **proposer dimension** (firing-engine design
/// §6, from Lal's #932 thread): a proposal synced from another agent's
/// replica must NOT suppress this agent's own mint, or a multi-DID quorum
/// could never grow past 1 — the consensus counter counts distinct DIDs, so
/// each DID needs its own row (or an accept-link) to be countable. Re-mints
/// by the *same* DID stay impossible, which is all idempotency requires.
async fn proposal_already_exists<S: ProposalLookup + ?Sized>(
    store: &S,
    transition: &SatisfiedTransition,
    acting_did: &str,
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
            Ok(true) => {}
            Ok(false) => continue,
            Err(e) => {
                fail_closed(e);
                return true;
            }
        }
        // Same (hash, instance, to_state) — but only OUR OWN row suppresses
        // the mint (see the proposer-dimension invariant above). DIDs are
        // URIs, so the setter stores the raw value — no `literal:` wrapper
        // (same as `ad4m://flow/instance` above; the e2e locks this shape).
        match links_to("ad4m://flow/proposer", acting_did.to_string()).await {
            Ok(true) => return true,
            Ok(false) => continue,
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

    fn count(min: Option<u32>, max: Option<u32>) -> Option<ModelQueryCount> {
        Some(ModelQueryCount { min, max })
    }

    #[test]
    fn evidence_hash_is_order_independent_and_content_sensitive() {
        let classes = vec!["ns://A".to_string()];
        let item = |id: &str, content: &str| EvidenceItem {
            id: id.into(),
            class_name: "ns://A".into(),
            content: content.into(),
        };
        let abc = vec![
            item("a", r#"{"id":"a","title":"one"}"#),
            item("b", r#"{"id":"b","title":"two"}"#),
            item("c", r#"{"id":"c","title":"three"}"#),
        ];
        let cab = vec![abc[2].clone(), abc[0].clone(), abc[1].clone()];
        let a = evidence_hash(&classes, &abc);
        assert_eq!(a, evidence_hash(&classes, &cab), "order-independent");
        assert_eq!(a.len(), 64, "hex-encoded SHA256");
        assert_ne!(a, evidence_hash(&classes, &abc[..2]), "id-set-sensitive");
        assert_ne!(
            a,
            evidence_hash(&["ns://B".into()], &abc),
            "class-sensitive"
        );

        // Same ids, edited content → different hash. This is what lets the
        // consensus pass catch an instance edited between mint and firing.
        let mut edited = abc.clone();
        edited[1].content = r#"{"id":"b","title":"two EDITED"}"#.into();
        assert_ne!(a, evidence_hash(&classes, &edited), "content-sensitive");

        // JSON key order does not matter — content is canonicalized.
        let mut reordered = abc.clone();
        reordered[1].content = r#"{"title":"two","id":"b"}"#.into();
        assert_eq!(
            a,
            evidence_hash(&classes, &reordered),
            "key order is canonicalized away"
        );
    }

    #[test]
    fn canonical_json_sorts_keys_recursively() {
        let v: Value = serde_json::from_str(r#"{"b":{"y":2,"x":[{"q":1,"p":0}]},"a":1}"#).unwrap();
        assert_eq!(
            canonical_json(&v),
            r#"{"a":1,"b":{"x":[{"p":0,"q":1}],"y":2}}"#
        );
    }

    #[tokio::test]
    async fn recompute_reproduces_the_minted_hash_and_detects_edits() {
        let f = flow(
            "Delivery",
            "identified",
            "scoped",
            Some(vec![mq("ns://Vote")]),
        );
        let flows = HashMap::from([(
            f.flow_uri(),
            flow(
                "Delivery",
                "identified",
                "scoped",
                Some(vec![mq("ns://Vote")]),
            ),
        )]);

        let stub = StubPerspective::default()
            .with_instance_objects("ns://Vote", vec![json!({"id": "v1", "value": "yes"})]);
        let minted = evaluate_flow_transitions(&stub, &[inst()], &flows, "did:key:me").await;
        assert_eq!(minted.len(), 1);

        // Unchanged graph → recompute reproduces the sealed hash.
        let same = recompute_evidence_hash(&stub, &f, &inst(), "scoped", "did:key:me")
            .await
            .unwrap();
        assert_eq!(same.as_deref(), Some(minted[0].evidence_hash.as_str()));

        // Same instance id, edited content → different hash: the stale-seal
        // detection the consensus pass fires on (spec §11).
        let edited = StubPerspective::default()
            .with_instance_objects("ns://Vote", vec![json!({"id": "v1", "value": "no"})]);
        let changed = recompute_evidence_hash(&edited, &f, &inst(), "scoped", "did:key:me")
            .await
            .unwrap()
            .expect("guard still satisfied");
        assert_ne!(changed, minted[0].evidence_hash);
    }

    #[tokio::test]
    async fn recompute_is_none_when_unverifiable_and_err_on_store_failure() {
        let f = flow(
            "Delivery",
            "identified",
            "scoped",
            Some(vec![mq("ns://Vote")]),
        );

        // Guard no longer satisfied.
        let empty = StubPerspective::default().with_instances("ns://Vote", &[]);
        assert_eq!(
            recompute_evidence_hash(&empty, &f, &inst(), "scoped", "did:key:me")
                .await
                .unwrap(),
            None
        );

        // Target state vanished from the flow definition.
        assert_eq!(
            recompute_evidence_hash(&empty, &f, &inst(), "shipped", "did:key:me")
                .await
                .unwrap(),
            None
        );

        // Guard-less state: nothing to verify.
        let unguarded = flow("Delivery", "identified", "scoped", None);
        assert_eq!(
            recompute_evidence_hash(&empty, &unguarded, &inst(), "scoped", "did:key:me")
                .await
                .unwrap(),
            None
        );

        // Store error → Err, so the caller skips firing without invalidating.
        let broken = StubPerspective::default().with_error("ns://Vote", "store down");
        assert!(
            recompute_evidence_hash(&broken, &f, &inst(), "scoped", "did:key:me")
                .await
                .is_err()
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

    /// Canned `model_query` keyed by class name; records every call.
    #[derive(Default)]
    struct StubPerspective {
        calls: Mutex<Vec<(String, String)>>,
        responses: HashMap<String, Result<Vec<Value>, String>>,
        /// Instance IDs `has_pending_overlay` reports as overlay-pending.
        pending: Vec<String>,
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
        fn with_pending(mut self, id: &str) -> Self {
            self.pending.push(id.to_string());
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

        async fn has_pending_overlay(&self, base: &str) -> Result<bool> {
            Ok(self.pending.iter().any(|p| p == base))
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
                evidence_hash: evidence_hash(
                    &["ns://Task".into()],
                    &[EvidenceItem {
                        id: "ad4m://task/1".into(),
                        class_name: "ns://Task".into(),
                        content: json!({ "id": "ad4m://task/1" }).to_string(),
                    }],
                ),
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
        // Hash seals class names + IDs + canonicalized content.
        assert_eq!(
            out[0].evidence_hash,
            evidence_hash(&["ns://Task".into()], &out[0].evidence)
        );
    }

    #[tokio::test]
    async fn overlay_pending_matches_neither_satisfy_nor_enter_evidence() {
        // Guard needs one Task; the only match is overlay-pending → Unmet.
        let flows = delivery(vec![mq("ns://Task")]);
        let recs = vec![record(DELIVERY, "ad4m://flow/instance/1", "identified")];
        let stub = StubPerspective::default()
            .with_instances("ns://Task", &["ad4m://task/pending"])
            .with_pending("ad4m://task/pending");
        assert!(
            evaluate_flow_transitions(&stub, &recs, &flows, "did:key:x")
                .await
                .is_empty(),
            "design principle #5: a pending overlay is not evidence"
        );

        // Mixed: the pending match is excluded from evidence AND the hash.
        let stub = StubPerspective::default()
            .with_instances("ns://Task", &["ad4m://task/pending", "ad4m://task/real"])
            .with_pending("ad4m://task/pending");
        let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:x").await;
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].evidence_ids, vec!["ad4m://task/real"]);
        assert_eq!(
            out[0].evidence_hash,
            evidence_hash(&["ns://Task".into()], &out[0].evidence),
            "hash seals only the committed subset"
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
            evidence_hash(&["ns://A".into(), "ns://B".into()], &out[0].evidence)
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
                proposal_already_exists(&store, &transition(), "did:key:me").await,
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
            assert!(proposal_already_exists(&store, &transition(), "did:key:me").await);
        }

        fn full_candidate_store(proposer: &str) -> ScriptedStore {
            ScriptedStore {
                by_predicate: HashMap::from([
                    (
                        "ad4m://flow/evidence_hashes".to_string(),
                        Some(vec![link(
                            "proposal://1",
                            "ad4m://flow/evidence_hashes",
                            "literal:string:hash",
                        )]),
                    ),
                    (
                        "ad4m://flow/instance".to_string(),
                        Some(vec![link(
                            "proposal://1",
                            "ad4m://flow/instance",
                            "ad4m://flow/instance/1",
                        )]),
                    ),
                    (
                        "ad4m://flow/to_state".to_string(),
                        Some(vec![link(
                            "proposal://1",
                            "ad4m://flow/to_state",
                            "literal:string:scoped",
                        )]),
                    ),
                    (
                        "ad4m://flow/proposer".to_string(),
                        // Raw DID target — DIDs are URIs, the setter never
                        // literal-wraps them (locked by the e2e's
                        // `assert_has_target(.., "ad4m://flow/proposer", &acting_did)`).
                        Some(vec![link("proposal://1", "ad4m://flow/proposer", proposer)]),
                    ),
                ]),
            }
        }

        #[tokio::test]
        async fn own_matching_proposal_suppresses_the_mint() {
            let store = full_candidate_store("did:key:me");
            assert!(
                proposal_already_exists(&store, &transition(), "did:key:me").await,
                "re-running the pass must not duplicate this DID's proposal"
            );
        }

        #[tokio::test]
        async fn another_dids_matching_proposal_does_not_suppress() {
            // The multi-agent quorum case (design §6): agent A's synced
            // proposal must not block agent B's own — the consensus counter
            // counts distinct DIDs, so B needs its own countable row.
            let store = full_candidate_store("did:key:agent-a");
            assert!(
                !proposal_already_exists(&store, &transition(), "did:key:agent-b").await,
                "a foreign proposal must not suppress this DID's mint"
            );
        }

        #[tokio::test]
        async fn proposer_lookup_error_reports_already_proposed() {
            let mut store = full_candidate_store("did:key:me");
            store
                .by_predicate
                .insert("ad4m://flow/proposer".to_string(), None);
            assert!(
                proposal_already_exists(&store, &transition(), "did:key:me").await,
                "a failed proposer lookup must fail closed like the other lookups"
            );
        }

        #[tokio::test]
        async fn empty_store_reports_not_proposed() {
            let store = ScriptedStore {
                by_predicate: HashMap::new(),
            };
            assert!(!proposal_already_exists(&store, &transition(), "did:key:me").await);
        }
    }
}
