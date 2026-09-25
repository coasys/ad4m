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
//! bag, sealed by an order-independent SHA256 in [`evidence_hash`] so a
//! voter can detect evidence that no longer resolves — or was edited —
//! before co-signing.
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
    load_flow_instances, load_shacl_flows, reachable_next_states, FlowInstanceRecord, FlowTokens,
};
use crate::perspectives::flow_instance::atom::ROLE_GRANT_REVOKED_PREDICATE;
use crate::perspectives::flow_instance::receipt::FlowReceipt;
use crate::perspectives::flow_semantic_check::{
    build_semantic_check_prompt, semantic_check_passed, SemanticCheckLlm,
};
use crate::perspectives::interpretation::LlmFlowProposal;
use crate::perspectives::model_query::ModelQueryInput;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::{
    ModelQuery, ModelQueryCount, PropertyCondition, SHACLFlow,
};
use crate::types::LinkExpression;
use anyhow::{anyhow, bail, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
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
    /// re-opens the guard under a NEW hash: the mint side misses its dedup
    /// and mints a fresh proposal for the current evidence, while a voter
    /// asked to co-sign the stale-sealed one refuses
    /// (`flow_instance::accept`).
    pub evidence: Vec<EvidenceItem>,
    /// See [`evidence_hash`].
    pub evidence_hash: String,
    /// The target state's `semanticCheck` hint, if it declares one.
    pub semantic_check: Option<String>,
    /// `Some` exactly when `to_state` is terminal: the instances the
    /// proposer names as the run's outputs, each with the content this
    /// replica's `model_query` returned for it. The proposal names their
    /// `(class, id)` and signs `outputs_hash` over the content next to the
    /// evidence seal (#1104); see
    /// `flow_instance::atom::check_outputs_commitment` for what a voter
    /// checks.
    pub outputs: Option<Vec<EvidenceItem>>,
}

/// One hydrated piece of guard evidence: an instance a `requires` query
/// matched, carried with its full `model_query` JSON so downstream LLM
/// passes can evaluate content ("was this agreed?") rather than rubber-stamp
/// a URI list.
///
/// `Serialize` on purpose: the preimage behind a proposal's `evidence_hash`
/// is what turns "this hash matches nothing I can see" into an inspectable
/// object a reader can re-hash with [`evidence_hash`] and compare against the
/// seal every voter verified before co-signing.
///
/// `Deserialize` for the other end of that trip: a receipt that arrived from
/// elsewhere is read back before it is verified
/// (`flow_instance::receipt::EvidencePreimage`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EvidenceItem {
    pub id: String,
    /// SHACL class the matching guard queried for.
    pub class_name: String,
    /// Compact JSON of the matched instance exactly as `model_query`
    /// returned it (id + properties).
    pub content: String,
}

/// Serialize a JSON value with recursively-sorted object keys — a stable
/// form independent of the key order `model_query` happens to produce.
/// Non-JSON content is hashed verbatim rather than dropped.
pub(crate) fn canonical_json(v: &Value) -> String {
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

/// Order-independent seal over a satisfied guard's evidence: SHA256 over the
/// *number* of class names, then the class names, then each evidence item's
/// `(class, id, canonical-content)` triple, sorted. Every field is
/// length-prefixed before hashing, so no field's *content* can shift bytes
/// across a boundary.
///
/// # Why the class-name count is framed too
///
/// Framing every field makes the digest input decode to a unique flat
/// sequence of strings. That is not enough: fields cannot shift, but
/// **sections can**. With no count, nothing marks where the class names end
/// and the items begin, so any `k` with `(total - k) % 3 == 0` reads back as
/// a valid `(class_names, items)` split and distinct preimages share a seal.
///
/// The dangerous direction is unfolding class names *into* an item. A
/// negative guard (`count: { max: 0 }`) contributes a class name with zero
/// items, so a re-partition can move names into the `class` / `id` /
/// `content` slots of a fabricated item. Before the count was framed,
/// `["Reviewer", "Blocker", "task://99", "{…}"] + []` and
/// `["Reviewer"] + [("Blocker", "task://99", "{…}")]` hashed identically —
/// and the second asserts a cited instance that never existed while still
/// satisfying `EvidencePreimage::rehashes_to_seal` against a seal the real
/// voters computed. Framing the count pins the boundary, so one digest has
/// one preimage partition.
///
/// Two evaluations of the same guard against the same graph produce the same
/// hash regardless of result order; **editing a cited instance changes the
/// hash**, which is what lets a voter detect a stale seal before co-signing.
pub fn evidence_hash(class_names: &[String], evidence: &[EvidenceItem]) -> String {
    let mut hasher = Sha256::new();
    hasher.update((class_names.len() as u64).to_le_bytes());
    for name in class_names {
        frame(&mut hasher, name);
    }
    frame_items(&mut hasher, evidence);
    hex::encode(hasher.finalize())
}

/// Length-prefix one field, so no field's content can shift bytes across a
/// boundary. The one framing [`evidence_hash`], [`tagged_items_hash`] and
/// [`super::content_address::content_address`] share.
pub(crate) fn frame(hasher: &mut Sha256, field: &str) {
    hasher.update((field.len() as u64).to_le_bytes());
    hasher.update(field.as_bytes());
}

/// Each item's `(class, id, canonical_json(content))` triple, sorted and
/// framed. Sorting makes the result independent of the order `model_query`
/// returned the instances in; [`canonical_json`] makes it independent of
/// their key order. Non-JSON content is framed verbatim.
fn frame_items(hasher: &mut Sha256, evidence: &[EvidenceItem]) {
    let mut items: Vec<(String, String, String)> = evidence
        .iter()
        .map(|e| {
            let canonical = serde_json::from_str::<Value>(&e.content)
                .map(|v| canonical_json(&v))
                .unwrap_or_else(|_| e.content.clone());
            (e.class_name.clone(), e.id.clone(), canonical)
        })
        .collect();
    items.sort();
    for (class, id, content) in &items {
        frame(hasher, class);
        frame(hasher, id);
        frame(hasher, content);
    }
}

/// The [`evidence_hash`] item framing under a domain `tag`, for a hash that
/// must never be read as an evidence seal (the flow outputs commitment,
/// `flow_instance::atom::outputs_hash`).
///
/// The digest input opens with `u64::MAX` where an evidence seal has its
/// class-name count. No evidence seal can open that way: it would need
/// 2^64 − 1 class names. The framed `tag` follows, then the items exactly as
/// [`evidence_hash`] frames them. So the two hashes share one
/// canonicalisation and one item framing, and cannot collide by
/// construction.
pub(crate) fn tagged_items_hash(tag: &str, items: &[EvidenceItem]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(u64::MAX.to_le_bytes());
    frame(&mut hasher, tag);
    frame_items(&mut hasher, items);
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
/// The keys of an object `linkedTo`, `{ via, to }`. The role-gate reader
/// refuses any other key (`shacl_parser::role_gate_keys`), so both read this.
const LINKED_TO_VIA: &str = "via";
const LINKED_TO_TO: &str = "to";
pub(crate) const LINKED_TO_KEYS: &[&str] = &[LINKED_TO_VIA, LINKED_TO_TO];

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

/// Substitute tokens everywhere a string can appear in a condition value.
///
/// Objects are recursed into, not cloned. `WhereCondition` admits object
/// shapes — `Ops` (`{"not": …}`, `{"contains": …}`) and `SubClause` — so a
/// rule may legitimately carry `$did` inside one, e.g.
/// `{"members": {"equals": {"not": "$did"}}}`. Cloning such an object left
/// the literal `"$did"` in the query handed to `model_query`, which produces
/// the **same** query for every candidate: the role then stops discriminating
/// between DIDs. Under a negating operator that fails *open* — no instance
/// has a property equal to the literal `"$did"`, so `not` matches everyone
/// and every candidate is granted the role.
///
/// The `did_dependent` guard in `resolve_role_grants` does not catch this:
/// it tests the serialised *rule* for `$did`, which is present, rather than
/// the generated *input*, where it was never resolved.
fn substitute_json(value: &Value, record: &FlowInstanceRecord, acting_did: &str) -> Value {
    match value {
        Value::String(s) => Value::String(substitute_tokens(s, record, acting_did)),
        Value::Array(items) => Value::Array(
            items
                .iter()
                .map(|v| substitute_json(v, record, acting_did))
                .collect(),
        ),
        Value::Object(fields) => Value::Object(
            fields
                .iter()
                .map(|(k, v)| (k.clone(), substitute_json(v, record, acting_did)))
                .collect(),
        ),
        other => other.clone(),
    }
}

/// Whether any string anywhere in `value` still contains an unresolved
/// `$`-token. Used as a post-substitution assertion: substitution is total
/// over the JSON tree, so a surviving token means a shape it does not know
/// how to walk, and a query that cannot discriminate must fail closed rather
/// than run.
fn has_unresolved_token(value: &Value) -> bool {
    match value {
        Value::String(s) => FlowTokens::contains_token(s),
        Value::Array(items) => items.iter().any(has_unresolved_token),
        Value::Object(fields) => fields.values().any(has_unresolved_token),
        _ => false,
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
    let where_clause = requires_where(query, record, acting_did, None, None, false)?;
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
    // Substitution is total over the JSON tree, so a surviving token means a
    // shape it could not walk. Refuse rather than run: a role query still
    // carrying `$did` is byte-identical for every candidate, and under a
    // negating operator it matches all of them.
    if has_unresolved_token(&out) {
        bail!(
            "translated query still contains an unresolved token — it would be identical for \
             every candidate and cannot discriminate between DIDs: {out}"
        );
    }
    Ok(out)
}

/// Translate one level of a `ModelQuery` (the top, or an `or` branch).
///
/// `author` is a per-link condition here: a role rule's `author` names who
/// may grant the role, so every link the rule filters on at that level must be
/// one that author wrote. It is emitted **nested** under every field the level
/// emits, the DID property included (see `model_query::link_author`):
/// `{ didProperty: agent, where: { forTask: T, author: A } }` becomes
/// `{ forTask: { eq: T, author: A }, agent: { eq: did, author: A } }`, and an
/// operator object takes it as one more key (`{ not: v, author: A }`). Nesting
/// it under the DID property alone let anyone else's link satisfy the other
/// fields (#1114). A field with no link behind it (a getter, `timestamp`) then
/// makes `model_query` return an `Err`, which fails closed.
///
/// The DID property is the level's own `didProperty`, or for an `or` branch
/// without one, the enclosing level's. `or` branches that each name only a
/// granter (`where: { author: X }`) with plain DIDs collapse into one author
/// list on the outer DID property: `{ agent: { eq: did, author: [A, B] } }`.
/// Branches that say more keep their own `OR` arm, with the author nested
/// under each of the arm's fields. An arm that inherits an author does not
/// collapse: the granters would become its own author and replace the
/// inherited one on its fields, where they only scope the DID property.
///
/// A level's `where` and its `or` are ANDed, so an arm's fields are links the
/// rule filters on too. An arm with no `author` of its own therefore takes the
/// enclosing level's, nested under each of its fields:
/// `{ didProperty: agent, where: { author: A }, or: [{ where: { rank: lead } }] }`
/// becomes `{ agent: { eq: did, author: A }, OR: [{ rank: { eq: lead, author: A } }] }`.
/// An arm with its own `author` keeps its own.
///
/// An arm's `author` reaches its level's fields in one case only: when every
/// arm names only a granter, the collapse makes their union the level's
/// `author`, and it scopes the level's fields like an own one. A level with
/// fields but no `author` (own, inherited or collapsed) whose arms name one
/// anywhere below is refused: its fields would be emitted bare, so anyone's
/// link could satisfy them beside a granter's arm, and one more condition in
/// an arm (which stops the collapse) would widen the rule. The rule author puts
/// the `author` on the level, or writes the level's fields into each arm.
///
/// A level with no field to nest under keeps its own `author` top level, where
/// `model_query` reads it as the instance's author; an inherited one has
/// nothing to scope there and is dropped.
///
/// `author` covers the `where` fields only. The `linkedTo` link (the `parent`
/// scope) is matched from any author: `model_query` has no author condition
/// on it yet (#1139).
fn requires_where(
    query: &ModelQuery,
    record: &FlowInstanceRecord,
    acting_did: &str,
    inherited_did_property: Option<&str>,
    inherited_author: Option<&Value>,
    nested: bool,
) -> Result<Map<String, Value>> {
    if nested && query.linked_to.is_some() {
        bail!("`linkedTo` on an `or` branch is not supported by model_query");
    }
    let did_property = query.did_property.as_deref().or(inherited_did_property);
    let mut out = Map::new();
    let mut author = None;
    for (field, cond) in query.r#where.iter().flatten() {
        let value = where_condition(field, cond, record, acting_did)?;
        // A DID property named `author` is a (hand-built) property, and the
        // `where.author` beside it collides with it below.
        if field == "author" && did_property != Some("author") {
            author = Some(value);
        } else {
            out.insert(field.clone(), value);
        }
    }

    let alts = query.or.as_ref().filter(|a| !a.is_empty());
    if let Some(alts) = alts {
        for alt in alts {
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
        }
    }

    // `or` branches that only name a granter fold into one author list,
    // unless that list would replace an author this level inherits.
    let granters = match (did_property, &author, alts) {
        (Some(_), None, Some(alts))
            if query.did_property.is_some() && inherited_author.is_none() =>
        {
            granter_only_branches(alts, record, acting_did)?
        }
        _ => None,
    };
    let collapsed = granters.is_some();
    if collapsed {
        author = granters;
    }
    if author.is_none()
        && inherited_author.is_none()
        && !out.is_empty()
        && alts.is_some_and(|alts| alts.iter().any(names_author))
    {
        let fields = out.keys().map(|f| format!("`{f}`")).collect::<Vec<_>>();
        bail!(
            "an `or` arm names an `author` but its level has none, so the level's {} would \
             match a link anyone wrote: put the `author` on the level, or write those fields \
             into each `or` arm",
            fields.join(", ")
        );
    }

    // The level's own DID property, or an `or` branch granter's enclosing one.
    let did_here = query
        .did_property
        .as_deref()
        .or(did_property.filter(|_| author.is_some()));
    if let Some(prop) = did_here {
        if out.contains_key(prop) {
            bail!("`didProperty` `{prop}` collides with an existing `where` field");
        }
        out.insert(prop.to_string(), Value::String(acting_did.to_string()));
    }

    // The level's own `author`, else the one its enclosing level passed down.
    let scope = author.as_ref().or(inherited_author);
    if let Some(scope) = scope {
        for (field, value) in out.iter_mut() {
            *value = with_author(field, value.take(), scope)?;
        }
    }
    if out.is_empty() {
        if let Some(author) = &author {
            out.insert("author".to_string(), author.clone());
        }
    }

    if let Some(alts) = alts.filter(|_| !collapsed) {
        let branches = alts
            .iter()
            .map(|alt| {
                requires_where(alt, record, acting_did, did_property, scope, true)
                    .map(Value::Object)
            })
            .collect::<Result<Vec<_>>>()?;
        out.insert("OR".to_string(), Value::Array(branches));
    }
    Ok(out)
}

/// Nest `author` into one field's condition: `v` becomes `{ eq: v, author }`,
/// and an operator object takes `author` as one more key.
///
/// Only value operators take it. `equals` passes any JSON through, and beside
/// a relation quantifier `author` would mean something else: `{ none: {…},
/// author: A }` is "A wrote no such link", which `model_query` deliberately
/// does not scope side by side, and `{ some: {…}, author: A }` scopes only the
/// relation link, not the linked record's fields. A sub-clause object has no
/// value to scope at all. Each is refused, so the rule fails closed.
fn with_author(field: &str, condition: Value, author: &Value) -> Result<Value> {
    const VALUE_OPS: [&str; 8] = ["eq", "not", "contains", "between", "lt", "lte", "gt", "gte"];
    match condition {
        Value::Object(mut ops) => {
            if ops.contains_key("author") {
                bail!("`{field}` already carries an `author` beside the rule's `author`");
            }
            if let Some(key) = ops.keys().find(|k| !VALUE_OPS.contains(&k.as_str())) {
                bail!(
                    "`{field}`: the rule's `author` cannot be nested beside `{key}`; only value \
                     operators ({}) take it",
                    VALUE_OPS.join(", ")
                );
            }
            ops.insert("author".to_string(), author.clone());
            Ok(Value::Object(ops))
        }
        value => Ok(json!({ "eq": value, "author": author })),
    }
}

/// Whether `query`, or any `or` arm below it, has a `where.author`.
fn names_author(query: &ModelQuery) -> bool {
    query
        .r#where
        .as_ref()
        .is_some_and(|w| w.contains_key("author"))
        || query.or.iter().flatten().any(names_author)
}

/// The union of the granters when every `or` branch is only
/// `{ className, where: { author: <DID or DIDs> } }`, else `None`.
fn granter_only_branches(
    alts: &[ModelQuery],
    record: &FlowInstanceRecord,
    acting_did: &str,
) -> Result<Option<Value>> {
    let mut dids: Vec<Value> = Vec::new();
    for alt in alts {
        let only_author = alt.did_property.is_none()
            && alt.linked_to.is_none()
            && alt.or.as_ref().is_none_or(|o| o.is_empty())
            && alt
                .r#where
                .as_ref()
                .is_some_and(|w| w.len() == 1 && w.contains_key("author"));
        if !only_author {
            return Ok(None);
        }
        let cond = &alt.r#where.as_ref().expect("checked above")["author"];
        let found = match where_condition("author", cond, record, acting_did)? {
            did @ Value::String(_) => vec![did],
            Value::Array(items) if items.iter().all(Value::is_string) => items,
            _ => return Ok(None),
        };
        for did in found {
            if !dids.contains(&did) {
                dids.push(did);
            }
        }
    }
    Ok(Some(match dids.len() {
        1 => dids.remove(0),
        _ => Value::Array(dids),
    }))
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
                .get(LINKED_TO_VIA)
                .and_then(Value::as_str)
                .ok_or_else(|| anyhow!("`linkedTo` object needs a string `via` predicate"))?;
            let to = obj
                .get(LINKED_TO_TO)
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

/// One signed revocation tombstone: who wrote it and when.
///
/// A **computed view** over a carried tombstone link, not a carried value:
/// `RoleGrantEvidence::resolve` derives it from the link's own author and
/// timestamp after applying the authority rule. Nothing serialises it into a
/// read-set, so nobody can assert a revocation that no link witnesses.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RoleRevocation {
    /// The tombstone's author — the DID that signed it.
    pub by: String,
    /// The tombstone's link timestamp: the moment the grant stopped counting.
    pub at: String,
}

/// The links behind one role instance's history for one DID: the
/// `instance --didProperty--> did` grant links, and the tombstones on the
/// instance naming the DID — **as links, not as conclusions**.
///
/// The store returns the material; nothing here is collapsed into
/// `granted_at` / `revoked_at`. That derivation is
/// [`RoleGrantEvidence::resolve`](crate::perspectives::flow_instance::roles::RoleGrantEvidence::resolve),
/// which is pure and therefore re-runnable by a reader holding only a
/// serialised read-set. Carrying links rather than verdicts is what lets that
/// reader check the chronology instead of believing the minter's summary.
///
/// Carried as plain [`LinkExpression`], not `DecoratedLinkExpression`: the
/// decorated form's `proof.valid` / `status` are one executor's read-model
/// flags for its own clients, and on material that travels they would be
/// claims. Every signature check on these links —
/// [`revocation_link_counts_for_did`], here and in the pure reader — computes
/// the verdict from the signature itself, so there is deliberately no carried
/// verdict to read (review r4076927995).
///
/// What is carried differs by kind, and the asymmetry is on purpose *for now*:
///
/// - **Tombstones** must be signed ([`revocation_link_counts_for_did`]),
///   exactly as `flow_instance::atom::signed_by` requires it for votes: a
///   link whose verdict is not `valid` is not a link anyone wrote. This is
///   the rule the pre-#1027 code already applied, carried over unchanged.
/// - **Grant links** are carried on target match alone
///   ([`grant_link_names_did`]), also unchanged from pre-#1027.
///
/// Signature-filtering grant links too is the obvious next step and is
/// deliberately **not** taken here: `granted_at` falls back to the instance's
/// own timestamp when no grant link qualifies, and that fallback is normally
/// *earlier* than the assignment link — so dropping links can land the window
/// wider than leaving them in. Closing that hole needs the fallback, the
/// `proof.valid` tri-state (#1046, which conflates never-evaluated with
/// evaluated-and-failed) and the missing author filter to move together.
/// Tracked as <https://github.com/coasys/ad4m/issues/1063>.
///
/// **Authority is deliberately not checked here** — whether a tombstone's
/// author may revoke depends on the role query, so the reader applies
/// [`revocation_authorised`](crate::perspectives::flow_instance::roles::evidence::revocation_authorised)
/// itself against the definition it holds, and cannot be handed a
/// pre-filtered set to trust.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct RoleGrantLinks {
    /// Every `instance --didProperty--> did` link, earliest first.
    /// Empty when the role query has no `didProperty` or no such link exists;
    /// the reader then dates the grant from the instance itself, never from
    /// "always".
    pub grant_links: Vec<LinkExpression>,
    /// Every signed tombstone on this instance naming this DID, any author,
    /// **before** the authority filter. Never truncated: dropping a tombstone
    /// can only widen a window.
    pub revocation_links: Vec<LinkExpression>,
}

/// At most this many grant links are carried per `(instance, DID)`, earliest
/// first. Only the earliest ever dates a grant, so the rest are redundancy;
/// a cap can therefore only move `granted_at` later — fail-closed — while
/// bounding what a receipt has to carry.
///
/// The cap is applied **after every collection-side filter** and to a
/// signature-preferred ordering, so that a reader-side filter added later
/// (#1063) cannot be inverted by forged links evicting a genuine one at
/// collection time — see the comment at the truncation site.
pub(crate) const MAX_GRANT_LINKS: usize = 8;

/// Whether a **grant** link speaks about this DID's membership: its target
/// names the DID. No signature check — see [`RoleGrantLinks`] and #1063.
///
/// Shared by the store boundary above and by the pure reader in
/// `roles::RoleGrantEvidence::resolve`, on purpose. An asymmetric filter is
/// not a style problem: a link the minter counts and the verifier drops
/// mints receipts that fail their own verification, and — the direction that
/// actually costs — a link the verifier counts and the minter dropped lets
/// forged material widen a window. One predicate per kind, both sites.
pub(crate) fn grant_link_names_did(link: &LinkExpression, did: &str, did_literal: &str) -> bool {
    target_names_did(&link.data.target, did, did_literal)
}

/// Whether a **revocation** tombstone counts for this DID: its signature
/// verifies, and its target names the DID. Same both-sites rule as
/// [`grant_link_names_did`], plus the vote-layer signature check.
///
/// The verdict is **computed here from the signature**
/// ([`LinkExpression::compute_proof_valid`]), never read from a carried flag
/// — the carried form has none. That makes the both-sites symmetry exact:
/// collection boundary and pure reader run the same crypto on the same
/// material, so a verdict cannot differ between them and cannot be forged
/// into either.
pub(crate) fn revocation_link_counts_for_did(
    link: &LinkExpression,
    did: &str,
    did_literal: &str,
) -> bool {
    link.compute_proof_valid() && grant_link_names_did(link, did, did_literal)
}

/// `literal:string:`-encode a DID for target matching — the form the SDNA
/// setters write. Shared with the pure reader for the same reason
/// [`grant_link_names_did`] is.
pub(crate) fn did_literal_url(did: &str) -> anyhow::Result<String> {
    use ad4m_client::literal::Literal;
    Literal::from_string(did.to_string())
        .to_url()
        .map_err(|e| anyhow::anyhow!("literal encode DID `{did}`: {e}"))
}

impl RoleGrantLinks {
    /// The `links` entries a role query asks `model_query` for, so that
    /// [`Self::from_instance`] can read each match's history off the result:
    /// the `didProperty` **name** when there is one, and the tombstone
    /// predicate.
    ///
    /// The name is passed as the role query spells it. `model_query` resolves
    /// it through the class shape (`model_query::links`), the same translation
    /// its `where` applies to the membership condition, so the grant links and
    /// the match cannot disagree about which predicate carries the DID. Before
    /// #1065 they did: the name went verbatim into a raw `get_links` as a
    /// predicate, matched nothing, and every `didProperty` grant fell back to
    /// the instance's earlier timestamp. A name the class does not declare is a
    /// `model_query` error, never an empty list.
    pub(crate) fn query_keys(did_property: Option<&str>) -> Vec<String> {
        did_property
            .into_iter()
            .chain(std::iter::once(ROLE_GRANT_REVOKED_PREDICATE))
            .map(str::to_string)
            .collect()
    }

    /// Collect one matched role instance's history for one DID from the
    /// `__links` rows `model_query` attached to it (#1046 §3 + §4, #1103).
    ///
    /// `instance` — one entry of the role query's `instances`, run with
    ///   `links: `[`Self::query_keys`]`(did_property)`.
    /// `did_property` — the role query's `didProperty`, if any; `None` for
    ///   `$did`-style queries, where no single property carries the DID.
    /// `did` — the candidate's plain DID.
    ///
    /// A requested key missing from the instance, or a row that is not a
    /// [`LinkExpression`], is an `Err`. Reading either as "no links" fails
    /// open: no tombstone means "not revoked", and no grant link means dating
    /// from the instance, which is normally earlier than the assignment.
    ///
    /// The filters are the store boundary's, unchanged from the raw read this
    /// replaces: grant links on target match and a parseable timestamp,
    /// signature-preferred under [`MAX_GRANT_LINKS`]; tombstones on
    /// [`revocation_link_counts_for_did`]. The verdicts are computed here from
    /// the signatures, not read from the query (#1046 §2 is not in yet).
    pub(crate) fn from_instance(
        instance: &Value,
        did_property: Option<&str>,
        did: &str,
    ) -> anyhow::Result<Self> {
        use crate::perspectives::flow_instance::time::parse_link_timestamp;
        use crate::perspectives::model_query::LINKS_KEY;

        let instance_id = instance["id"].as_str().unwrap_or("<no id>");
        let rows = |key: &str| -> anyhow::Result<Vec<LinkExpression>> {
            let Some(rows) = instance[LINKS_KEY][key].as_array() else {
                bail!(
                    "role grant links: role instance `{instance_id}` came back without \
                     `{LINKS_KEY}.{key}`, so its grant history for `{did}` cannot be read; \
                     refusing to gate (fail-closed)"
                );
            };
            rows.iter()
                .map(|row| {
                    serde_json::from_value::<LinkExpression>(row.clone()).map_err(|e| {
                        anyhow!(
                            "role grant links: role instance `{instance_id}`: a `{key}` row is \
                             not a link ({e}): {row}"
                        )
                    })
                })
                .collect()
        };

        let did_literal =
            did_literal_url(did).map_err(|e| anyhow!("role grant links: {instance_id}: {e}"))?;
        let grant_counts = |l: &LinkExpression| grant_link_names_did(l, did, &did_literal);
        let revocation_counts =
            |l: &LinkExpression| revocation_link_counts_for_did(l, did, &did_literal);

        // Sorted by parsed instant, not by string — grant links are
        // client-stamped and clients disagree on RFC 3339 flavour (#1000).
        // A link whose timestamp does not parse can never date a grant, so it
        // is not worth carrying; when none parses this stays empty and the
        // reader falls back to the instance's own timestamp or fails closed.
        let raw_grant_links = match did_property {
            Some(prop) => rows(prop)?,
            None => Vec::new(),
        };
        let mut grant_links: Vec<LinkExpression> = raw_grant_links
            .iter()
            .filter(|l| grant_counts(l) && parse_link_timestamp(&l.timestamp).is_some())
            .cloned()
            .collect();
        // A `didProperty` role whose assignment links cannot be found is not a
        // neutral outcome: `granted_at` falls back to
        // `asserted_instance_timestamp`, which is the instance's *earliest*
        // link and therefore normally EARLIER than the assignment — the
        // widest possible window, not the narrow one the rule asked for. That
        // is the shape of the #1027 hole #1065 fixed, and what made it survive
        // a fully green suite was that nothing distinguished "this query names
        // no didProperty" from "this didProperty resolves to nothing". Warn so
        // the next drift announces itself instead of silently widening
        // eligibility.
        if let Some(prop) = did_property {
            if grant_links.is_empty() {
                log::warn!(
                    "role grant links: role instance `{instance_id}`: no `{prop}` assignment \
                     link for `{did}` survived (model_query returned {} link(s) on that \
                     property). `granted_at` will fall back to the instance's own timestamp, \
                     which is normally EARLIER than the assignment — a WIDER eligibility window \
                     than the rule intends. Check that the assignment link carries a parseable \
                     RFC 3339 timestamp.",
                    raw_grant_links.len(),
                );
            }
        }
        grant_links.sort_by(|a, b| {
            (parse_link_timestamp(&a.timestamp), &a.timestamp)
                .cmp(&(parse_link_timestamp(&b.timestamp), &b.timestamp))
        });
        // Every filter runs before the cap, and the cap runs last. That order
        // is load-bearing, not tidiness: the cap keeps the EARLIEST links, and
        // a reader-side filter that only ever removes links (#1063 adds a
        // signature one) then sees whatever survived collection. Cap first and
        // the two invert — N forged early links evict the genuine later one
        // here, the reader drops all N, `resolve` finds no grant link at all
        // and falls back to `asserted_instance_timestamp`, which is *earlier*
        // than the grant the filter existed to protect. Fail-open, from a
        // filter meant to fail closed.
        //
        // So the cap is applied to a signature-preferred ordering: links whose
        // signature verifies claim slots first — computed from the signature
        // here, since the carried form deliberately has no verdict flag to
        // read. This is a *preference*, never a filter — an unverified link
        // is still carried while there is room, which is what keeps #1064 and
        // #1063 out of this PR. Under the cap nothing changes; at the cap a
        // forger cannot evict a genuine link, and dropping an unverified
        // *earlier* link can only move `granted_at` later, which is the
        // fail-closed direction.
        grant_links.sort_by_cached_key(|l| !l.compute_proof_valid());
        grant_links.truncate(MAX_GRANT_LINKS);
        grant_links.sort_by(|a, b| {
            (parse_link_timestamp(&a.timestamp), &a.timestamp)
                .cmp(&(parse_link_timestamp(&b.timestamp), &b.timestamp))
        });

        let revocation_links: Vec<LinkExpression> = rows(ROLE_GRANT_REVOKED_PREDICATE)?
            .into_iter()
            .filter(revocation_counts)
            .collect();

        Ok(RoleGrantLinks {
            grant_links,
            revocation_links,
        })
    }
}

/// The one perspective call the evaluator needs, behind a trait so the
/// composition below can be unit-tested against a stub.
///
/// Role grant histories come through `model_query` too: the role query asks
/// for [`RoleGrantLinks::query_keys`] and [`RoleGrantLinks::from_instance`]
/// reads them off each match (#1103). There is no second link read to stub.
#[async_trait]
pub trait RequiresQueryable: Send + Sync {
    async fn model_query(&self, class_name: &str, query_json: &str) -> Result<String>;

    /// Every receipt filed under `flow_uri`'s index, read whole so the caller
    /// can verify them itself.
    ///
    /// Called only for a role query that declares `producedByFlow`. The index
    /// is writable by anyone and this call trusts nothing in it: collecting a
    /// receipt decides nothing, and every check that matters runs in the
    /// caller (see
    /// [`flow_instance::grant`](crate::perspectives::flow_instance::grant)).
    ///
    /// Over [`MAX_FLOW_RECEIPTS`](crate::perspectives::flow_instance::produced::MAX_FLOW_RECEIPTS)
    /// this is an `Err` carrying
    /// [`ReceiptBudgetExceeded`](crate::perspectives::flow_instance::produced::ReceiptBudgetExceeded),
    /// never a shorter list.
    ///
    /// The default knows nothing, which is the fail-closed answer here: no
    /// receipts means no grant, so a stub that stays on this default never
    /// turns into "granted by something I could not see".
    async fn flow_receipts(&self, _flow_uri: &str) -> anyhow::Result<Vec<FlowReceipt>> {
        Ok(Vec::new())
    }

    /// This replica's flow definitions, keyed by `flow_uri()` — what a
    /// `producedByFlow` gate verifies the granting flow's receipts against.
    ///
    /// The default is empty, which is fail-closed: a gate whose flow is not
    /// in the catalogue is an error, never "not a member".
    async fn flow_catalogue(&self) -> anyhow::Result<HashMap<String, SHACLFlow>> {
        Ok(HashMap::new())
    }
}

/// Does a link target name this DID? Accepts the raw DID (the flow's own
/// `proposer` links), the `literal:string:`-encoded form the SDNA setters
/// write, and the legacy `literal://string:` spelling still minted by Flux's
/// TypeScript `Literal` and by pre-normalisation peers (#1014).
///
/// The legacy spelling matters more here than anywhere else in this file:
/// every other malformed input in this read path fails *closed* (undated instance
/// → error, non-discriminating query → error), but an unrecognised
/// *tombstone* spelling would fail open — the revocation simply is not seen.
fn target_names_did(target: &str, did: &str, did_literal: &str) -> bool {
    target == did || crate::utils::normalize_legacy_literal(target).as_ref() == did_literal
}

#[async_trait]
impl RequiresQueryable for PerspectiveInstance {
    async fn model_query(&self, class_name: &str, query_json: &str) -> Result<String> {
        PerspectiveInstance::model_query(self, class_name, query_json).await
    }

    /// `produced`'s loader, unchanged: scoped to the flow before it is
    /// budgeted, and an error over budget. One reader of F's receipts for
    /// every consumer, so the role gate cannot drift from `flowValidOutputs`.
    async fn flow_receipts(&self, flow_uri: &str) -> anyhow::Result<Vec<FlowReceipt>> {
        crate::perspectives::flow_instance::produced::load_flow_receipts(self, flow_uri).await
    }

    async fn flow_catalogue(&self) -> anyhow::Result<HashMap<String, SHACLFlow>> {
        load_shacl_flows(self).await
    }
}

/// Tri-state seal result for one target state's guard, used by the manual
/// proposal path and by `accept.rs` when re-verifying a co-sign.
///
/// - `Sealed(hash)` — guard is present and currently satisfied; hash is
///   the SHA256 over the evidence, exactly as [`evidence_hash`] produces.
/// - `NoGuard` — the target state carries no `requires` guard; the seal is
///   defined as `evidence_hash(&[], &[])` (the hash of an empty bag), so
///   proposer and voter always agree on it by construction.
/// - `Unmet` — the guard exists but is not currently satisfied, or the flow
///   / state definition changed since the proposal was minted.  The caller
///   must refuse its own action and write nothing.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum EvidenceSeal {
    Sealed(String),
    NoGuard,
    Unmet,
}

impl EvidenceSeal {
    /// Canonical hash string: `Sealed(h) → h`, `NoGuard → hash of empty bag`.
    /// Returns `None` for `Unmet` so callers can treat it as "unverifiable".
    pub(crate) fn hash(&self) -> Option<String> {
        match self {
            EvidenceSeal::Sealed(h) => Some(h.clone()),
            EvidenceSeal::NoGuard => Some(evidence_hash(&[], &[])),
            EvidenceSeal::Unmet => None,
        }
    }
}

/// Outcome of AND-ing a state's `requires`. Translation failures are
/// split from query failures so the composer can `warn!` the former
/// (persistent misconfig) and `debug!` the latter (transient).
pub(crate) enum RequiresResult {
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
    let matched = result
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
    Ok(matched)
}

/// AND across a state's `requires`. Unmet as soon as one guard misses;
/// `Satisfied` (class names and hydrated evidence, both deduplicated in
/// first-seen order, evidence by instance ID) when every guard holds.
pub(crate) async fn evaluate_requires<Q: RequiresQueryable + ?Sized>(
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
                    // The engine has no user to ask what a run produced, so
                    // into a terminal state it names what the guard matched.
                    // That is this proposer's choice, not a rule: a receipt
                    // is checked against the signed commitment, never
                    // against `requires` (#1104).
                    let outputs = crate::perspectives::flow_instance::receipt::is_terminal_state(
                        flow,
                        &state.name,
                    )
                    .then(|| evidence.clone());
                    out.push(SatisfiedTransition {
                        flow_name: flow.name.clone(),
                        instance_uri: record.instance_uri.clone(),
                        from_state: record.current_state.clone(),
                        to_state: state.name.clone(),
                        evidence_hash: evidence_hash(&class_names, &evidence),
                        evidence_ids,
                        evidence,
                        semantic_check: state.semantic_check.clone(),
                        outputs,
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

/// A recomputed [`EvidenceSeal`] together with the evidence it was computed
/// over, for the one caller that has to *write* that evidence (the mint).
///
/// `evidence` is empty for `NoGuard` (the canonical seal is over an empty
/// bag, by definition) and for `Unmet` (there is nothing to cite).
pub(crate) struct SealedEvidence {
    pub seal: EvidenceSeal,
    /// The guard's class names, in the order [`evidence_hash`] framed them
    /// into the seal. Carried because they are not recoverable from the
    /// items: a negative guard contributes a class name and no item, so a
    /// preimage of items alone could not be re-hashed (see
    /// `flow_instance::receipt::EvidencePreimage`).
    pub class_names: Vec<String>,
    pub evidence: Vec<EvidenceItem>,
}

/// Re-run one target state's `requires` against the CURRENT graph and return
/// an [`EvidenceSeal`] that a voter can compare with the proposal's stored
/// hash before co-signing.
///
/// - `Ok(Sealed(hash))` — guard satisfied; compare with `atom.evidence_hash`.
/// - `Ok(NoGuard)` — the target state carries no guard; the canonical seal
///   is `evidence_hash(&[], &[])` (see [`EvidenceSeal::hash`]).
/// - `Ok(Unmet)` — guard exists but is not currently satisfied, or the
///   flow / state definition changed.  The caller must refuse its own action.
/// - `Err` — transient query / store failure.
///
/// A caller may only ever refuse its own action on any of these; none of
/// them is grounds for touching somebody else's proposal.
///
/// `acting_did` must be the PROPOSER's DID: `$did`-substituted guards
/// resolved against the proposer at mint time, so re-verification must
/// substitute the same identity or the hash could never match.
///
/// **This is the one definition of "the seal for state S".** Both places a
/// vote comes into existence go through it — the co-sign in
/// `flow_instance::accept` via [`recompute_evidence_hash`], and the mint in
/// `flow_instance::propose` via this function — so the proposer's own vote is
/// sealed by exactly the code every other replica re-runs against it.
pub(crate) async fn recompute_evidence_seal<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    flow: &SHACLFlow,
    record: &FlowInstanceRecord,
    to_state: &str,
    acting_did: &str,
) -> Result<SealedEvidence> {
    let unmet = |seal| SealedEvidence {
        seal,
        class_names: Vec::new(),
        evidence: Vec::new(),
    };
    let Some(state) = flow.states.iter().find(|s| s.name == to_state) else {
        log::warn!(
            "recompute_evidence_hash: state `{to_state}` no longer exists on flow `{}`",
            flow.name
        );
        return Ok(unmet(EvidenceSeal::Unmet));
    };
    let requires = state.requires.as_deref().unwrap_or_default();
    if requires.is_empty() {
        return Ok(unmet(EvidenceSeal::NoGuard));
    }
    match evaluate_requires(perspective, requires, record, acting_did).await {
        RequiresResult::Satisfied(class_names, evidence) => Ok(SealedEvidence {
            seal: EvidenceSeal::Sealed(evidence_hash(&class_names, &evidence)),
            class_names,
            evidence,
        }),
        RequiresResult::Unmet => Ok(unmet(EvidenceSeal::Unmet)),
        RequiresResult::Untranslatable(e) => {
            log::warn!(
                "recompute_evidence_hash: `{}.{to_state}` became untranslatable: {e:#}",
                flow.name
            );
            Ok(unmet(EvidenceSeal::Unmet))
        }
        RequiresResult::QueryFailed(e) => Err(e),
    }
}

/// [`recompute_evidence_seal`] without the evidence — what a voter needs.
pub(crate) async fn recompute_evidence_hash<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    flow: &SHACLFlow,
    record: &FlowInstanceRecord,
    to_state: &str,
    acting_did: &str,
) -> Result<EvidenceSeal> {
    Ok(
        recompute_evidence_seal(perspective, flow, record, to_state, acting_did)
            .await?
            .seal,
    )
}

/// Load → evaluate → (confirm) → write, called by the extraction pass once
/// its own writes are committed. `subjects` bounds the FlowInstance load to
/// what this pass actually touched or showed the LLM (written bases ∪
/// cursor sources); an empty slice returns immediately — there is never a
/// whole-perspective sweep from this entry point (that unbounded path was
/// removed once and regressed once; see PR #940 review).
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
    subjects: &[String],
    context: &AgentContext,
    llm_proposals: &[LlmFlowProposal],
    semantic_check: Option<&dyn SemanticCheckLlm>,
    flow_filter: Option<&[String]>,
) -> Vec<String> {
    if subjects.is_empty() {
        return Vec::new();
    }
    let loaded = async {
        let mut flows_by_uri = load_shacl_flows(perspective).await?;
        crate::perspectives::flow_context::retain_selected_flows(&mut flows_by_uri, flow_filter);
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
    // Mint path: always derive, never use the `currentState` cache.  This is
    // deliberate even though the cache is `Local`-verified since #987: minting
    // from a stale cache (debounce gap, role-change lag) would issue proposals
    // on the wrong edge and each one costs paid LLM tokens.  The evaluator
    // runs rarely and needs the freshest fold immediately before minting, so
    // the derive cost here is acceptable and correct.
    let derived =
        crate::perspectives::flow_instance::derive_states(perspective, &records, &flows_by_uri)
            .await;
    // Honour the invariant from issue #998: a contested flow is irreversibly
    // stalled — two edges already carry quorum, so more proposals cannot resolve
    // it. Proposing into such a flow wastes LLM tokens and misleads governance.
    let records: Vec<_> = derived
        .into_iter()
        .filter_map(|df| {
            if let Some(ref c) = df.contested {
                log::info!(
                    "run_engine_proposal_pass: {} is contested in state {:?}; skipping mint (issue #998)",
                    df.record.instance_uri,
                    c.from_state,
                );
                None
            } else {
                Some(df.record)
            }
        })
        .collect();

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
        if proposal_already_exists(perspective, transition).await {
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

        // The sweep after this pass reports what the proposal settles; see
        // `catch_up_before_voting` for why the proposer derives first.
        for outcome in crate::perspectives::flow_instance::pass::catch_up_before_voting(
            perspective,
            &transition.instance_uri,
            context,
        )
        .await
        {
            log::info!(
                "run_engine_proposal_pass: recorded {} {} → {} before proposing",
                outcome.instance_uri,
                outcome.from_state,
                outcome.to_state
            );
        }
        match write_proposal(perspective, transition, &acting_did, rationale, context).await {
            Ok(uri) => minted.push(uri),
            Err(e) => log::debug!("run_engine_proposal_pass: {label} not written: {e:#}"),
        }
    }
    minted
}

/// **Every** live proposal carrying this transition's dedup key —
/// `(evidence_hash, instance_uri, to_state)` — sorted by URI; empty when
/// there is none.
///
/// *Live* excludes any proposal carrying `resolved_as`: that is the recorded
/// history of a consensus event, not an open proposal, and it must not
/// suppress a re-mint. Without the exclusion a cyclic flow wedges — same
/// graph → same seal → the already-settled proposal matches the whole key,
/// so the mint is skipped and the edge can never fire on the next visit.
///
/// The key deliberately carries **no proposer**. That is what lets a second
/// agent find the one open proposal for an edge and co-sign it instead of
/// minting an unreachable twin (`flow_instance::propose`). `to_state` is in
/// the key because two distinct transitions can share identical `requires`
/// guards and therefore identical evidence hashes.
///
/// **All matches, not the first.** The key carries no `from_state` either, so
/// in a flow with two transitions into one state under identical guards a
/// proposal on the *other* edge shares the key. Nothing orders these links,
/// so a first-match lookup is a coin flip: the manual path would classify a
/// foreign candidate, miss the joinable one sitting behind it, and mint — and
/// mint *again* on the next press, splitting the vote it exists to gather and
/// breaking invariant 4 (`flow_instance::propose`). Returning the whole set
/// lets that caller prefer its own edge. The engine pass only asks whether the
/// set is non-empty, which is what it asked before.
///
/// **Sorted by URI**, because the store's own iteration order is arbitrary —
/// `query_links` walks matched quads and never orders them, so the same graph
/// can hand back the same two candidates in either order on two runs. Two
/// things need that not to be true. The manual path picks the first
/// `Joinable` among twins, and unordered input makes that pick vary run to
/// run and replica to replica for no reason (harmless to quorum, since
/// `fold::settle_edge` pools across twins, but it scatters co-signs). And the
/// ordering-dependent bug above cannot be tested through the store at all
/// unless the order is fixed. A total order over URIs is enough for both.
///
/// Every store failure is an `Err`, with no disposition chosen here: the two
/// callers need opposite ones. The engine pass fails closed
/// ([`proposal_already_exists`]) because it retries on the next pass; the
/// manual path surfaces the error, because a user's click has no next pass.
pub(crate) async fn find_live_proposals<S: ProposalLookup + ?Sized>(
    store: &S,
    transition: &SatisfiedTransition,
) -> Result<Vec<String>> {
    use crate::types::LinkQuery;
    let mut found = Vec::new();
    let literal = |s: &str| format!("literal:string:{}", urlencoding::encode(s));
    let hash_links = store
        .get_proposal_links(&LinkQuery {
            predicate: Some("ad4m://flow/evidence_hashes".into()),
            target: Some(literal(&transition.evidence_hash)),
            ..Default::default()
        })
        .await
        .map_err(|e| anyhow!("evidence-hash lookup failed ({e:#})"))?;
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
                .map_err(|e| anyhow!("candidate lookup on {proposal_uri} failed ({e:#})"))
        };
        let resolved = store
            .get_proposal_links(&LinkQuery {
                source: Some(proposal_uri.clone()),
                predicate: Some(
                    crate::perspectives::flow_instance::atom::RESOLVED_AS_PREDICATE.to_string(),
                ),
                ..Default::default()
            })
            .await
            .map_err(|e| anyhow!("candidate lookup on {proposal_uri} failed ({e:#})"))?;
        if !resolved.is_empty() {
            continue;
        }
        if !links_to("ad4m://flow/instance", transition.instance_uri.clone()).await? {
            continue;
        }
        if links_to("ad4m://flow/to_state", literal(&transition.to_state)).await? {
            found.push(proposal_uri.clone());
        }
    }
    found.sort();
    Ok(found)
}

/// Idempotency for the **engine pass**: has a proposal with this transition's
/// evidence hash already been minted for the same instance AND target state?
///
/// Minting does not advance `currentState`, so without this check every later
/// pass re-proposes each satisfied-unconsumed transition — and a consensus
/// rule counting proposals rather than distinct DIDs could then be gamed by
/// one agent re-running its own pass.
///
/// **Both guarantees are about the engine pass**, and the fail-closed
/// disposition below is too: it assumes a caller that runs again shortly and
/// whose acting DID is this replica's. A caller that runs once, on a human's
/// click, satisfies neither — it must call [`find_live_proposals`] and choose
/// its own disposition. `flow_instance::propose` does exactly that; this
/// wrapper exists so the engine's behaviour is unchanged by that split.
///
/// **The `from_state`-free key bites here, and only here.** Asking whether the
/// set is non-empty cannot tell an `A→C` proposal from a `B→C` one, so a
/// stranded proposal on one edge suppresses the engine ever proposing the
/// other, silently, for as long as it stays open. The manual path discriminates
/// and recovers; this one has nowhere to put the distinction — skipping is its
/// whole contract — so it does not. Narrow (it needs two guard-identical edges
/// into one state) and pre-existing, but real: prefer widening the key here
/// over re-flattening the manual path onto it.
///
/// **The validity-blind match bites the same way.** A live terminal proposal
/// no voter could sign — a bad or missing outputs commitment, an output that
/// does not load — still matches the key, so it suppresses the engine mint
/// on that edge for as long as it stays open. The manual path validates the
/// atom as a co-signer would and mints past it (`live_proposal_role`); doing
/// the same here needs `load_outputs`, which [`ProposalLookup`] cannot
/// answer. Same remedy when it matters: a human proposes manually, or the
/// invalid proposal's author rejects it.
pub(crate) async fn proposal_already_exists<S: ProposalLookup + ?Sized>(
    store: &S,
    transition: &SatisfiedTransition,
) -> bool {
    match find_live_proposals(store, transition).await {
        Ok(found) => !found.is_empty(),
        // Fail CLOSED: a missed mint on a transient store error is recovered
        // on the next pass, while a duplicate mint is exactly what this
        // function exists to prevent — see the invariant above.
        Err(e) => {
            log::warn!(
                "proposal_already_exists: {e:#}; treating as already-proposed \
                 (fail-closed, skipping mint)"
            );
            true
        }
    }
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
pub(crate) async fn write_proposal(
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
        transition.outputs.as_deref(),
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

    #[test]
    fn target_names_did_accepts_raw_and_both_literal_spellings() {
        let did = "did:key:zAlice";
        let lit = "literal:string:did%3Akey%3AzAlice";
        assert!(target_names_did(did, did, lit));
        assert!(target_names_did(lit, did, lit));
        // The legacy double-slash spelling names the same DID — a tombstone
        // written in it must be seen, or revocation fails open.
        assert!(target_names_did(
            "literal://string:did%3Akey%3AzAlice",
            did,
            lit
        ));
        assert!(!target_names_did(
            "literal:string:did%3Akey%3AzBob",
            did,
            lit
        ));
        assert!(!target_names_did("literal://", did, lit));
    }

    #[test]
    fn to_url_emits_the_single_colon_spelling_target_names_did_expects() {
        use ad4m_client::literal::Literal;
        let url = Literal::from_string("did:key:zAlice".to_string())
            .to_url()
            .expect("literal encode");
        assert!(url.starts_with("literal:") && !url.starts_with("literal://"));
        assert!(target_names_did(&url, "did:key:zAlice", &url));
    }

    /// The role query asks for the `didProperty` name as spelled, plus the
    /// tombstone predicate — and only the tombstone when there is no
    /// `didProperty`. Dropping the tombstone key would leave `from_instance`
    /// nothing to read revocations from.
    #[test]
    fn role_grant_query_keys_are_the_did_property_and_the_tombstone() {
        assert_eq!(
            RoleGrantLinks::query_keys(Some("owner")),
            vec![
                "owner".to_string(),
                ROLE_GRANT_REVOKED_PREDICATE.to_string()
            ]
        );
        assert_eq!(
            RoleGrantLinks::query_keys(None),
            vec![ROLE_GRANT_REVOKED_PREDICATE.to_string()]
        );
    }

    /// `from_instance` reads what `model_query` attached under `__links`. A
    /// requested key that is not there, or a row that is not a link, is an
    /// error: read as "no links", a missing tombstone key is "not revoked"
    /// and a missing grant key dates the grant from the earlier instance
    /// timestamp — both fail open.
    #[test]
    #[rustfmt::skip]
    fn role_grant_links_missing_or_malformed_rows_are_errors_not_no_links() {
        let did = "did:key:zAlice";
        let tomb = ROLE_GRANT_REVOKED_PREDICATE;
        let cases: Vec<(&str, Value, Option<&str>)> = vec![
            ("no `__links` at all", json!({ "id": "r0" }), Some("owner")),
            ("the tombstone key is missing", json!({ "id": "r0", "__links": { "owner": [] } }), Some("owner")),
            ("the didProperty key is missing", json!({ "id": "r0", "__links": { tomb: [] } }), Some("owner")),
            ("no didProperty, and the tombstone key is missing", json!({ "id": "r0", "__links": {} }), None),
            ("a tombstone row that is not a link", json!({ "id": "r0", "__links": { "owner": [], tomb: [{ "nope": 1 }] } }), Some("owner")),
            ("a grant row that is not a link", json!({ "id": "r0", "__links": { "owner": [{ "nope": 1 }], tomb: [] } }), Some("owner")),
        ];
        for (name, instance, did_property) in cases {
            assert!(
                RoleGrantLinks::from_instance(&instance, did_property, did).is_err(),
                "{name}: must be an Err, not an empty history"
            );
        }
        // Asked and answered with nothing is the one empty history.
        let answered = json!({ "id": "r0", "__links": { "owner": [], tomb: [] } });
        assert_eq!(
            RoleGrantLinks::from_instance(&answered, Some("owner"), did).expect("answered"),
            RoleGrantLinks::default()
        );
    }

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

    /// Test 18. The seal is order-independent (two evaluations of the same
    /// guard agree however the store ordered the instances) and
    /// content-sensitive (an edit that keeps the same IDs changes it) — the
    /// second half is what lets a voter refuse to co-sign edited evidence.
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

        let mut edited = abc.clone();
        edited[1].content = r#"{"id":"b","title":"two EDITED"}"#.into();
        assert_ne!(a, evidence_hash(&classes, &edited), "content-sensitive");

        // JSON key order is canonicalized away, so the seal does not drift
        // with whatever order `model_query` happened to serialise.
        let mut reordered = abc.clone();
        reordered[1].content = r#"{"title":"two","id":"b"}"#.into();
        assert_eq!(a, evidence_hash(&classes, &reordered), "key order ignored");
    }

    /// Framing every *field* stops a field's content from shifting bytes
    /// across a boundary. It does not stop a *section* from shifting: without
    /// the `class_names` count in the digest, the hash input decodes to a
    /// unique flat sequence of strings but not to a unique
    /// `(class_names, items)` split — any `k` with `(total - k) % 3 == 0`
    /// reads back as a valid partition.
    ///
    /// The second pair is the direction that matters. A negative guard
    /// contributes a class name with zero items, so re-partitioning is
    /// *productive*: `["Reviewer"] + [("Blocker", "task://99", …)]` asserts a
    /// cited instance that never existed, while still satisfying
    /// `EvidencePreimage::rehashes_to_seal` against a seal the real voters
    /// computed.
    ///
    /// Red if the `class_names.len()` framing is removed from
    /// `evidence_hash`: the pairs then collide on `de92f359…` and
    /// `e34627f9…` respectively.
    #[test]
    fn evidence_hash_pins_where_the_class_names_end_and_the_items_begin() {
        let names = |v: &[&str]| v.iter().map(|s| s.to_string()).collect::<Vec<String>>();
        let item = |class: &str, id: &str, content: &str| EvidenceItem {
            id: id.into(),
            class_name: class.into(),
            content: content.into(),
        };

        // An item folded forward into the class-name list.
        assert_ne!(
            evidence_hash(
                &names(&["Task"]),
                &[item("Task", "task://1", r#"{"title":"x"}"#)],
            ),
            evidence_hash(
                &names(&["Task", "Task", "task://1", r#"{"title":"x"}"#]),
                &[]
            ),
            "a guard over one matched Task must not share a seal with a guard \
             whose class list swallowed that match"
        );

        // Class names unfolded backwards into an item: the preimage that
        // invents cited evidence under a seal real voters signed.
        assert_ne!(
            evidence_hash(
                &names(&["Reviewer", "Blocker", "task://99", r#"{"approved":true}"#]),
                &[],
            ),
            evidence_hash(
                &names(&["Reviewer"]),
                &[item("Blocker", "task://99", r#"{"approved":true}"#)],
            ),
            "satisfied negative guards must not share a seal with a guard that \
             cites an instance"
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

    /// The two predicates both sides of the wire share, pinned together so a
    /// change to one that should have touched the other is visible here.
    ///
    /// Target matching is identical for both and must accept every spelling
    /// the graph uses for a DID target — an unrecognised *tombstone* spelling
    /// would fail open (#1014).
    ///
    /// They differ on signatures, and that difference is the tested contract,
    /// not an accident: a tombstone needs a signature that verifies, a grant
    /// link does not (yet — #1063). The verdict is **computed** from the
    /// signature — the carried form has no verdict flag a fixture could set —
    /// so a fixture that wants a tombstone to count has to sign it for real,
    /// and a forged one is one signed by a key that is not its stated
    /// author's.
    #[test]
    fn grant_and_revocation_predicates_differ_only_on_the_signature_check() {
        use crate::agent::signatures::TestSigner;
        use ad4m_client::literal::Literal;
        let did = "did:key:alice";
        let did_literal = did_literal_url(did).unwrap();
        let other = Literal::from_string("did:key:bob".to_string())
            .to_url()
            .unwrap();
        let legacy = did_literal.replace("literal:string:", "literal://string:");
        let admin = TestSigner::generate();
        let forger = TestSigner::generate();
        let at = chrono::DateTime::parse_from_rfc3339("2026-01-01T00:00:00.000Z")
            .unwrap()
            .with_timezone(&chrono::Utc);
        // A valid link is signed by its stated author's own key; a forged one
        // states the same author but carries somebody else's signature.
        let signed = |target: &str, valid: bool| -> LinkExpression {
            let signing_key = if valid { &admin } else { &forger };
            let mut expr = signing_key.sign_at(
                crate::types::Link {
                    source: "r0".into(),
                    predicate: Some("agent".into()),
                    target: target.into(),
                }
                .normalize(),
                at,
            );
            expr.author = admin.did.clone();
            expr.proof.key = admin.key_id.clone();
            LinkExpression::from(expr)
        };

        for target in [did, did_literal.as_str(), legacy.as_str()] {
            assert!(
                revocation_link_counts_for_did(&signed(target, true), did, &did_literal),
                "a signed tombstone naming the DID as `{target}` counts"
            );
            assert!(
                !revocation_link_counts_for_did(&signed(target, false), did, &did_literal),
                "a tombstone whose signature does not verify never counts, whatever it names"
            );
            for valid in [true, false] {
                assert!(
                    grant_link_names_did(&signed(target, valid), did, &did_literal),
                    "a grant link is carried on target match alone (valid={valid}); \
                     signature-filtering it is #1063, not this PR"
                );
            }
        }
        for valid in [true, false] {
            assert!(
                !revocation_link_counts_for_did(&signed(&other, valid), did, &did_literal),
                "a tombstone naming another DID is not this DID's history"
            );
            assert!(
                !grant_link_names_did(&signed(&other, valid), did, &did_literal),
                "a grant link naming another DID is not this DID's history"
            );
        }
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

    /// A role rule's `author` names who may grant the role, so it is nested
    /// under every field the level emits, the DID property included
    /// (`model_query` reads that per link, #1114), and `or` branches that only
    /// name a granter collapse into one author list. An `or` arm with no
    /// `author` of its own takes its level's. What these queries match
    /// is pinned in `model_query::link_author_tests::flow_rules`; this table
    /// only pins the shape.
    #[test]
    fn role_author_is_nested_under_every_field_of_its_level() {
        let did = "did:key:cand";
        let task = inst().subject;
        let role = |v: Value| -> ModelQuery { serde_json::from_value(v).unwrap() };
        for (rule, expected) in [
            (
                json!({ "className": "ns://R", "didProperty": "agent", "where": { "author": "did:key:admin" } }),
                json!({ "agent": { "eq": did, "author": "did:key:admin" } }),
            ),
            (
                json!({ "className": "ns://R", "didProperty": "agent",
                        "where": { "author": { "in": ["did:key:admin", "did:key:lead"] } } }),
                json!({ "agent": { "eq": did, "author": ["did:key:admin", "did:key:lead"] } }),
            ),
            (
                json!({ "className": "ns://R", "didProperty": "agent", "where": { "author": "$did" } }),
                json!({ "agent": { "eq": did, "author": did } }),
            ),
            (
                json!({ "className": "ns://R", "didProperty": "agent",
                        "where": { "forTask": "$flow.base", "author": "did:key:admin" } }),
                json!({ "forTask": { "eq": task, "author": "did:key:admin" },
                        "agent": { "eq": did, "author": "did:key:admin" } }),
            ),
            (
                json!({ "className": "ns://R", "didProperty": "agent",
                        "where": { "tag": { "in": ["a", "b"] }, "state": { "equals": { "not": "archived" } },
                                   "author": "did:key:admin" } }),
                json!({ "tag": { "eq": ["a", "b"], "author": "did:key:admin" },
                        "state": { "not": "archived", "author": "did:key:admin" },
                        "agent": { "eq": did, "author": "did:key:admin" } }),
            ),
            (
                json!({ "className": "ns://R", "didProperty": "agent",
                        "or": [ { "className": "ns://R", "where": { "author": "did:key:admin" } },
                                { "className": "ns://R", "where": { "author": { "in": ["did:key:lead", "did:key:admin"] } } } ] }),
                json!({ "agent": { "eq": did, "author": ["did:key:admin", "did:key:lead"] } }),
            ),
            (
                json!({ "className": "ns://R", "didProperty": "agent",
                        "or": [ { "className": "ns://R", "where": { "role": "lead", "author": "did:key:admin" } },
                                { "className": "ns://R", "where": { "author": "did:key:lead" } } ] }),
                json!({ "agent": did, "OR": [
                    { "role": { "eq": "lead", "author": "did:key:admin" },
                      "agent": { "eq": did, "author": "did:key:admin" } },
                    { "agent": { "eq": did, "author": "did:key:lead" } },
                ] }),
            ),
            (
                json!({ "className": "ns://R", "didProperty": "agent", "where": { "author": "did:key:admin" },
                        "or": [ { "className": "ns://R", "where": { "rank": "lead" } },
                                { "className": "ns://R", "where": { "rank": { "equals": { "not": "junior" } } } } ] }),
                json!({ "agent": { "eq": did, "author": "did:key:admin" }, "OR": [
                    { "rank": { "eq": "lead", "author": "did:key:admin" } },
                    { "rank": { "not": "junior", "author": "did:key:admin" } },
                ] }),
            ),
            (
                json!({ "className": "ns://R", "didProperty": "agent", "where": { "author": "did:key:admin" },
                        "or": [ { "className": "ns://R", "where": { "rank": "lead", "author": "did:key:lead" } },
                                { "className": "ns://R", "where": {} } ] }),
                json!({ "agent": { "eq": did, "author": "did:key:admin" }, "OR": [
                    { "rank": { "eq": "lead", "author": "did:key:lead" },
                      "agent": { "eq": did, "author": "did:key:lead" } },
                    {},
                ] }),
            ),
            (
                json!({ "className": "ns://R", "didProperty": "agent", "where": { "role": "lead" } }),
                json!({ "role": "lead", "agent": did }),
            ),
            (
                json!({ "className": "ns://R", "where": { "reviewer": "$did", "author": "did:key:admin" } }),
                json!({ "reviewer": { "eq": did, "author": "did:key:admin" } }),
            ),
            (
                json!({ "className": "ns://R", "linkedTo": "base", "where": { "author": "$did" } }),
                json!({ "author": did }),
            ),
        ] {
            let q = role(rule.clone());
            let got = qin(&q, did);
            assert_eq!(got["where"], expected, "{rule}");
        }
    }

    /// `equals` passes any JSON through. Beside a relation quantifier or in a
    /// sub-clause, a rule's `author` would not mean "the author wrote this
    /// link", so the translator refuses it instead of nesting it; the same
    /// condition without an `author` still translates.
    #[test]
    fn role_author_is_refused_beside_a_quantifier_or_sub_clause() {
        let role = |v: Value| -> ModelQuery { serde_json::from_value(v).unwrap() };
        for cond in [
            json!({ "none": { "verdict": "rejected" } }),
            json!({ "some": { "verdict": "approved" } }),
            json!({ "verdict": "approved" }),
        ] {
            let rule = |author: Option<&str>| {
                let mut w = json!({ "reviews": { "equals": cond } });
                if let Some(a) = author {
                    w["author"] = json!(a);
                }
                role(json!({ "className": "ns://R", "didProperty": "agent", "where": w }))
            };
            let err = requires_query_input(&rule(Some("did:key:admin")), &inst(), "did:key:x")
                .expect_err(&format!("author beside {cond}"))
                .to_string();
            assert!(err.contains("cannot be nested beside"), "{cond}: {err}");
            requires_query_input(&rule(None), &inst(), "did:key:x")
                .unwrap_or_else(|e| panic!("control without author, {cond}: {e}"));

            // An inherited author is refused the same way inside an `or` arm.
            let arm = role(json!({ "className": "ns://R", "didProperty": "agent",
                "where": { "author": "did:key:admin" },
                "or": [ { "className": "ns://R", "where": { "reviews": { "equals": cond } } } ] }));
            assert!(
                requires_query_input(&arm, &inst(), "did:key:x").is_err(),
                "inherited author beside {cond}"
            );
        }
    }

    /// A level with fields but no `author` of its own (nor an inherited or
    /// collapsed one) is refused when an `or` arm names one anywhere below:
    /// its fields would be emitted bare beside the granter's arm (#1114).
    /// Controls: the same fields under the collapse, under the level's own
    /// `author`, written into each arm, and with no `author` anywhere.
    #[test]
    fn a_level_without_an_author_is_refused_beside_author_arms() {
        let role = |v: Value| -> ModelQuery { serde_json::from_value(v).unwrap() };
        let arm = |w: Value| json!({ "className": "ns://R", "where": w });
        let admin = "did:key:admin";
        let lead = "did:key:lead";
        let for_task = json!({ "forTask": "$flow.base" });
        for rule in [
            // Arms that name granters but do not collapse.
            json!({ "className": "ns://R", "didProperty": "agent", "where": for_task,
                    "or": [ arm(json!({ "author": admin })), arm(json!({ "author": lead, "rank": "senior" })) ] }),
            // An arm with no `author` beside a granter arm.
            json!({ "className": "ns://R", "didProperty": "agent", "where": for_task,
                    "or": [ arm(json!({ "rank": "senior" })), arm(json!({ "author": admin })) ] }),
            // One level down: an arm with fields whose own arm names a granter.
            json!({ "className": "ns://R", "didProperty": "agent",
                    "or": [ { "className": "ns://R", "where": for_task, "or": [ arm(json!({ "author": admin })) ] } ] }),
            // A wrapper arm with no `where` whose own arm names a granter:
            // only the recursion in `names_author` sees that `author`.
            json!({ "className": "ns://R", "didProperty": "agent", "where": for_task,
                    "or": [ { "className": "ns://R", "or": [ arm(json!({ "author": admin })) ] } ] }),
            // Without a `didProperty`.
            json!({ "className": "ns://R", "where": { "reviewer": "$did" },
                    "or": [ arm(json!({ "author": admin, "rank": "senior" })) ] }),
        ] {
            let err = requires_query_input(&role(rule.clone()), &inst(), "did:key:x")
                .expect_err(&rule.to_string())
                .to_string();
            assert!(
                err.contains("put the `author` on the level"),
                "{rule}: {err}"
            );
        }
        for rule in [
            json!({ "className": "ns://R", "didProperty": "agent", "where": for_task,
                    "or": [ arm(json!({ "author": admin })), arm(json!({ "author": lead })) ] }),
            json!({ "className": "ns://R", "didProperty": "agent", "where": { "forTask": "$flow.base", "author": admin },
                    "or": [ arm(json!({ "author": lead, "rank": "senior" })) ] }),
            json!({ "className": "ns://R", "didProperty": "agent",
                    "or": [ arm(json!({ "author": admin, "forTask": "$flow.base" })),
                            arm(json!({ "author": lead, "rank": "senior", "forTask": "$flow.base" })) ] }),
            json!({ "className": "ns://R", "didProperty": "agent", "where": for_task,
                    "or": [ arm(json!({ "rank": "lead" })), arm(json!({ "rank": "senior" })) ] }),
        ] {
            requires_query_input(&role(rule.clone()), &inst(), "did:key:x")
                .unwrap_or_else(|e| panic!("control {rule}: {e}"));
        }
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

    /// A `$did` inside an **object**-valued condition must be substituted like
    /// one inside a string or an array. `WhereCondition` admits object shapes
    /// (`Ops`, `SubClause`), so this is a legal rule — and before the fix
    /// `substitute_json` cloned objects wholesale, leaving the literal
    /// `"$did"` in the query. Every candidate then received a byte-identical
    /// query, so the role stopped discriminating between DIDs.
    #[test]
    fn query_input_substitutes_did_inside_an_object_valued_condition() {
        let q = with_where(
            mq("ns://Member"),
            vec![(
                "holder",
                PropertyCondition::Equals {
                    equals: json!({ "not": "$did" }),
                },
            )],
        );
        let out = qin(&q, "did:key:alice");
        assert_eq!(
            out,
            json!({ "where": { "holder": { "not": "did:key:alice" } } }),
            "object-valued conditions must be substituted, not cloned: {out}"
        );

        // Two different candidates must get two different queries — the
        // property the role gate depends on.
        assert_ne!(qin(&q, "did:key:alice"), qin(&q, "did:key:bob"));
    }

    /// Nested one level deeper, to pin that the walk is recursive rather than
    /// a single-level special case.
    #[test]
    fn query_input_substitutes_did_nested_in_arrays_inside_objects() {
        let q = with_where(
            mq("ns://Member"),
            vec![(
                "holder",
                PropertyCondition::Equals {
                    equals: json!({ "not": { "OR": [{ "did": "$did" }] } }),
                },
            )],
        );
        let out = qin(&q, "did:key:alice");
        assert!(
            !serde_json::to_string(&out).unwrap().contains("$did"),
            "no token may survive at any depth: {out}"
        );
    }

    /// Fail-closed backstop: if substitution ever misses a shape, the query
    /// must be refused rather than run. A role query that still carries a
    /// token is identical for every candidate, and under a negating operator
    /// it matches all of them — the failure direction is *open*, which is why
    /// this is an error rather than a warning.
    #[test]
    fn query_input_refuses_a_query_with_an_unresolved_token() {
        // An empty acting DID makes `FlowTokens::substitute` a deliberate
        // no-op for `$did` (empty field = "not set"), which is the cheapest
        // way to reach the post-substitution assertion.
        let q = with_where(
            mq("ns://Member"),
            vec![("holder", PropertyCondition::Str("$did".into()))],
        );
        let err = requires_query_input(&q, &inst(), "")
            .unwrap_err()
            .to_string();
        assert!(err.contains("unresolved token"), "{err}");
        assert!(err.contains("cannot discriminate"), "{err}");
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
                "about": { "eq": "ad4m://task/onboarding", "author": "did:key:acting" },
                "on": { "eq": "ad4m://flow/instance/1", "author": "did:key:acting" },
                "alsoOn": { "eq": ["ad4m://flow/instance/1", "other"], "author": "did:key:acting" },
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
    }

    impl StubPerspective {
        fn with_instances(self, class: &str, ids: &[&str]) -> Self {
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
            consensus_rule_malformed: false,
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
            consensus_rule_malformed: false,
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

    /// `linked_to_parent` reads every key in `LINKED_TO_KEYS`: without any
    /// one of them the object is refused. With the role-gate reader refusing
    /// every key outside the list (#1144), a stale name left in the list, or
    /// a key read here but not listed, fails one of the two.
    #[test]
    fn linked_to_parent_reads_every_listed_key() {
        let rec = record("ns://F", "ns://i", "s");
        let full: serde_json::Map<String, Value> = LINKED_TO_KEYS
            .iter()
            .map(|k| {
                (
                    k.to_string(),
                    json!(if *k == LINKED_TO_TO {
                        "base"
                    } else {
                        "ns://has"
                    }),
                )
            })
            .collect();
        assert_eq!(
            linked_to_parent(&Value::Object(full.clone()), &rec).expect("all listed keys"),
            json!({ "id": "ad4m://subject", "predicate": "ns://has" })
        );
        for key in LINKED_TO_KEYS {
            let mut without = full.clone();
            without.remove(*key);
            assert!(
                linked_to_parent(&Value::Object(without), &rec).is_err(),
                "`{key}` is listed but linked_to_parent does not need it"
            );
        }
    }

    const DELIVERY: &str = "delivery://DeliveryFlow";

    fn delivery(requires: Vec<ModelQuery>) -> HashMap<String, SHACLFlow> {
        HashMap::from([(
            DELIVERY.to_string(),
            flow("Delivery", "identified", "scoped", Some(requires)),
        )])
    }

    /// Test 19. The vote-time check's contract: on an unchanged graph the
    /// recomputed seal reproduces the minted one exactly, and an edit to the
    /// cited instance — same ID, different content — produces a different
    /// one. This is what a voter compares before deciding to co-sign.
    #[tokio::test]
    async fn recompute_reproduces_the_minted_hash_and_detects_edits() {
        // `SHACLFlow` is not `Clone`, so build one and borrow it back out of
        // the map the evaluator needs rather than constructing it twice.
        let built = flow(
            "Delivery",
            "identified",
            "scoped",
            Some(vec![mq("ns://Vote")]),
        );
        let flow_uri = built.flow_uri();
        let flows = HashMap::from([(flow_uri.clone(), built)]);
        let f = &flows[&flow_uri];

        let stub = StubPerspective::default()
            .with_instance_objects("ns://Vote", vec![json!({"id": "v1", "value": "yes"})]);
        let minted = evaluate_flow_transitions(&stub, &[inst()], &flows, "did:key:me").await;
        assert_eq!(minted.len(), 1);

        let same = recompute_evidence_hash(&stub, f, &inst(), "scoped", "did:key:me")
            .await
            .unwrap();
        assert_eq!(
            same.hash().as_deref(),
            Some(minted[0].evidence_hash.as_str()),
            "unchanged graph reproduces the minted hash"
        );

        let edited = StubPerspective::default()
            .with_instance_objects("ns://Vote", vec![json!({"id": "v1", "value": "no"})]);
        let changed = recompute_evidence_hash(&edited, f, &inst(), "scoped", "did:key:me")
            .await
            .unwrap()
            .hash()
            .expect("guard still satisfied even after edit — different hash, not Unmet");
        assert_ne!(changed, minted[0].evidence_hash);
    }

    /// `Unmet` for an unsatisfied/missing guard, `NoGuard` for a guard-less
    /// target state, and `Err` for a transient store failure. The voter uses
    /// `EvidenceSeal::hash()` which returns `None` for `Unmet` and `Some` for
    /// both `Sealed` and `NoGuard`, so the accept check catches only `Unmet`.
    #[tokio::test]
    async fn recompute_returns_unmet_or_noguard_and_err_on_store_failure() {
        let f = flow(
            "Delivery",
            "identified",
            "scoped",
            Some(vec![mq("ns://Vote")]),
        );
        let empty = StubPerspective::default().with_instances("ns://Vote", &[]);
        let unguarded = flow("Delivery", "identified", "scoped", None);

        // Unmet cases: guard exists but is not satisfied, or state is gone.
        for (name, store, flow, to_state) in [
            ("guard no longer satisfied", &empty, &f, "scoped"),
            (
                "target state vanished from the flow definition",
                &empty,
                &f,
                "shipped",
            ),
        ] {
            assert_eq!(
                recompute_evidence_hash(store, flow, &inst(), to_state, "did:key:me")
                    .await
                    .unwrap(),
                EvidenceSeal::Unmet,
                "{name}"
            );
        }

        // Guard-less state: commit A's fix — returns NoGuard, not Unmet.
        assert_eq!(
            recompute_evidence_hash(&empty, &unguarded, &inst(), "scoped", "did:key:me")
                .await
                .unwrap(),
            EvidenceSeal::NoGuard,
            "guard-less state must return NoGuard so co-signing can succeed"
        );

        let broken = StubPerspective::default().with_error("ns://Vote", "store down");
        assert!(
            recompute_evidence_hash(&broken, &f, &inst(), "scoped", "did:key:me")
                .await
                .is_err()
        );
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
                // `scoped` is terminal, so the engine names what the guard
                // matched as the run's outputs, with its content (#1104).
                outputs: Some(vec![EvidenceItem {
                    id: "ad4m://task/1".into(),
                    class_name: "ns://Task".into(),
                    content: json!({ "id": "ad4m://task/1" }).to_string(),
                }]),
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
        // The seal covers class names + IDs + canonicalized content.
        assert_eq!(
            out[0].evidence_hash,
            evidence_hash(&["ns://Task".into()], &out[0].evidence)
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
                outputs: None,
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

        /// The split the manual path needed. Both callers run the same lookup
        /// over the same failing store and must reach OPPOSITE dispositions:
        /// the engine pass fails closed because it retries on the next pass,
        /// and the manual path surfaces the error because a user's click has
        /// no next pass — silence there reports a lost vote as success.
        #[tokio::test]
        async fn a_store_failure_is_an_error_for_the_manual_path_and_fail_closed_for_the_pass() {
            let hash_lookup_fails = ScriptedStore {
                by_predicate: HashMap::from([("ad4m://flow/evidence_hashes".to_string(), None)]),
            };
            let err = find_live_proposals(&hash_lookup_fails, &transition())
                .await
                .expect_err("a failed evidence-hash lookup must be an Err, not Ok(None)");
            assert!(
                format!("{err:#}").contains("evidence-hash lookup failed"),
                "the error must name what failed: {err:#}"
            );
            assert!(
                proposal_already_exists(&hash_lookup_fails, &transition()).await,
                "the engine pass still fails closed on the very same store"
            );

            let candidate_lookup_fails = ScriptedStore {
                by_predicate: HashMap::from([
                    (
                        "ad4m://flow/evidence_hashes".to_string(),
                        Some(vec![link(
                            "proposal://1",
                            "ad4m://flow/evidence_hashes",
                            "literal:string:hash",
                        )]),
                    ),
                    ("ad4m://flow/instance".to_string(), None),
                ]),
            };
            let err = find_live_proposals(&candidate_lookup_fails, &transition())
                .await
                .expect_err("a failed candidate lookup must be an Err too");
            assert!(
                format!("{err:#}").contains("candidate lookup on proposal://1 failed"),
                "the error must name the candidate: {err:#}"
            );
            assert!(
                proposal_already_exists(&candidate_lookup_fails, &transition()).await,
                "the engine pass still fails closed here as well"
            );
        }

        /// The other half of the same split: on a clean store the two agree,
        /// and `find_live_proposals` hands back the URIs rather than a bool —
        /// which is what lets the manual path co-sign what it found.
        #[tokio::test]
        async fn a_live_match_yields_the_proposal_uri_and_a_settled_one_yields_none() {
            let matching = |extra: Vec<(String, Option<Vec<DecoratedLinkExpression>>)>| {
                let mut by_predicate = HashMap::from([
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
                ]);
                by_predicate.extend(extra);
                ScriptedStore { by_predicate }
            };

            let live = matching(vec![]);
            assert_eq!(
                find_live_proposals(&live, &transition())
                    .await
                    .expect("a clean store must not error"),
                vec!["proposal://1".to_string()],
                "the URI, not a bool — the manual path co-signs what it finds"
            );
            assert!(proposal_already_exists(&live, &transition()).await);

            // A `resolved_as` mark makes it history, not a live proposal: it
            // must not suppress a re-mint, or a cyclic flow wedges.
            let settled = matching(vec![(
                crate::perspectives::flow_instance::atom::RESOLVED_AS_PREDICATE.to_string(),
                Some(vec![link(
                    "proposal://1",
                    crate::perspectives::flow_instance::atom::RESOLVED_AS_PREDICATE,
                    "literal:string:fired",
                )]),
            )]);
            assert_eq!(
                find_live_proposals(&settled, &transition())
                    .await
                    .expect("no error"),
                Vec::<String>::new(),
                "a settled proposal is history and must not be found as live"
            );
            assert!(!proposal_already_exists(&settled, &transition()).await);
        }

        /// Two live proposals share the dedup key, because it carries no
        /// `from_state` and the flow has two guard-identical edges into one
        /// state. The lookup must hand back BOTH.
        ///
        /// Returning only the first is a coin flip on link order, and losing
        /// that flip is not cosmetic: `flow_instance::propose` would classify
        /// the foreign proposal, never see the joinable one behind it, and
        /// mint — then mint again on the next press, splitting the very vote
        /// the dedup key exists to gather. Only this caller can tell the two
        /// apart (it knows the acting DID and the derived `from_state`), so
        /// the lookup's whole job is to not decide for it.
        ///
        /// It does decide the ORDER, though: sorted by URI, not however the
        /// store happened to iterate. Two runs over one graph must classify
        /// the same candidate first.
        #[tokio::test]
        async fn every_proposal_sharing_the_dedup_key_is_returned_not_just_the_first() {
            let both = |uri: &str| {
                (
                    uri.to_string(),
                    link(uri, "ad4m://flow/instance", "ad4m://flow/instance/1"),
                )
            };
            let (a, link_a) = both("proposal://aaa");
            let (b, link_b) = both("proposal://bbb");
            // Fed to the store in DESCENDING order, which the real store is
            // free to do: `query_links` never orders its matches.
            let store = ScriptedStore {
                by_predicate: HashMap::from([
                    (
                        "ad4m://flow/evidence_hashes".to_string(),
                        Some(vec![
                            link(&b, "ad4m://flow/evidence_hashes", "literal:string:hash"),
                            link(&a, "ad4m://flow/evidence_hashes", "literal:string:hash"),
                        ]),
                    ),
                    (
                        "ad4m://flow/instance".to_string(),
                        Some(vec![link_b, link_a]),
                    ),
                    (
                        "ad4m://flow/to_state".to_string(),
                        Some(vec![
                            link(&b, "ad4m://flow/to_state", "literal:string:scoped"),
                            link(&a, "ad4m://flow/to_state", "literal:string:scoped"),
                        ]),
                    ),
                ]),
            };

            assert_eq!(
                find_live_proposals(&store, &transition())
                    .await
                    .expect("a clean store must not error"),
                vec![a, b],
                "both matches, sorted by URI rather than left in the store's arbitrary \
                 order — the caller picks its own edge, and it must pick the same one twice"
            );
            // The engine pass asked "is there one?" before and still does.
            assert!(proposal_already_exists(&store, &transition()).await);
        }
    }
}
