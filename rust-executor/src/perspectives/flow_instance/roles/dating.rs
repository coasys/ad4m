//! When a role grant starts (#1063): the rule stated in the
//! [`roles`](super) module doc (§ *Where the timestamps come from*), as code.
//!
//! [`GrantDating`] is built from the translated role query and the candidate,
//! and applied twice: where the evidence is collected
//! ([`RoleGrantLinks::from_instance`](crate::perspectives::flow_evaluator::RoleGrantLinks::from_instance),
//! before the cap) and where it is read
//! ([`RoleGrantEvidence::resolve`](super::RoleGrantEvidence::resolve)). Change
//! the rule there, not here.

use super::evidence::granter_authorised;
use crate::perspectives::flow_evaluator::{did_literal_url, target_names_did};
use crate::perspectives::flow_instance::time::parse_link_timestamp;
use crate::perspectives::shacl_parser::ModelQuery;
use crate::types::LinkExpression;
use chrono::{DateTime, Utc};
use serde_json::{Map, Value};

/// How one candidate's grants of one role are dated: read once off the
/// rule, then applied to each matched instance.
///
/// Built from the same inputs on both sides, the translated role query and
/// the candidate, so collection and [`resolve`](super::RoleGrantEvidence::resolve)
/// cannot disagree on which link counts.
#[derive(Debug, Clone)]
pub struct GrantDating {
    /// The candidate.
    pub did: String,
    did_literal: String,
    /// The role query with `$did` and the flow tokens substituted for this
    /// candidate: what [`granter_authorised`] reads.
    translated_query: Value,
    /// The DID fields, by property name, sorted.
    pub fields: Vec<String>,
    /// Whether the rule's `author` is `$did`, so the grantee's own links
    /// date the grant.
    pub by_grantee: bool,
}

impl GrantDating {
    /// `role` is the rule as written, `translated_query` the same rule
    /// translated for `did` ([`requires_query_input`](crate::perspectives::flow_evaluator::requires_query_input)).
    pub fn new(role: &ModelQuery, translated_query: &Value, did: &str) -> anyhow::Result<Self> {
        let mut fields = Vec::new();
        if let Some(Value::Object(where_clause)) = translated_query.get("where") {
            did_fields(where_clause, did, &mut fields);
        }
        fields.sort();
        fields.dedup();
        Ok(Self {
            did: did.to_string(),
            did_literal: did_literal_url(did)?,
            translated_query: translated_query.clone(),
            fields,
            by_grantee: authored_by_grantee(role, None),
        })
    }

    /// Whether any link could date a grant under this rule. `false` means
    /// the rule grants nothing.
    pub fn is_datable(&self) -> bool {
        !self.fields.is_empty() || self.by_grantee
    }

    /// Whether `link` is a grant link for this candidate on `instance_id`:
    /// on the instance, naming the DID, from an accepted granter, signed, and
    /// placeable in time. The caller has already chosen the links on the
    /// DID fields' predicates.
    ///
    /// The signature verdict is computed here, never read from a carried
    /// flag: the carried form has none.
    pub fn is_grant_link(&self, link: &LinkExpression, instance_id: &str) -> bool {
        link.data.source == instance_id
            && target_names_did(&link.data.target, &self.did, &self.did_literal)
            && parse_link_timestamp(&link.timestamp).is_some()
            && granter_authorised(&self.translated_query, &link.author)
            && link.compute_proof_valid()
    }

    /// Whether `link` is one of the grantee's own links on `instance_id`:
    /// on the instance, written by the candidate, signed, and placeable in
    /// time.
    pub fn is_grantees_own_link(&self, link: &LinkExpression, instance_id: &str) -> bool {
        link.data.source == instance_id
            && link.author == self.did
            && parse_link_timestamp(&link.timestamp).is_some()
            && link.compute_proof_valid()
    }

    /// When the grant on `instance_id` starts, from the carried links; `None`
    /// when nothing dates it, which is no grant.
    pub fn granted_at(
        &self,
        instance_id: &str,
        grant_links: &[LinkExpression],
        grantees_own_links: &[LinkExpression],
    ) -> Option<String> {
        if !self.is_datable() {
            return None;
        }
        let mut starts = Vec::with_capacity(2);
        if !self.fields.is_empty() {
            starts.push(earliest(
                grant_links
                    .iter()
                    .filter(|l| self.is_grant_link(l, instance_id)),
            )?);
        }
        if self.by_grantee {
            starts.push(earliest(
                grantees_own_links
                    .iter()
                    .filter(|l| self.is_grantees_own_link(l, instance_id)),
            )?);
        }
        starts.into_iter().max().map(|(_, at)| at)
    }
}

/// The earliest of `links` by parsed instant, never by string: timestamps are
/// client-asserted and clients disagree on RFC 3339 flavour (#1000).
fn earliest<'a>(
    links: impl Iterator<Item = &'a LinkExpression>,
) -> Option<(DateTime<Utc>, String)> {
    links
        .filter_map(|l| parse_link_timestamp(&l.timestamp).map(|at| (at, l.timestamp.clone())))
        .min()
}

/// Earliest first, at most `cap`: what collection carries of one kind. Only
/// the earliest link dates a grant, and every link here already qualifies,
/// so the cap only drops redundancy.
pub(crate) fn earliest_first(links: &mut Vec<LinkExpression>, cap: usize) {
    links.sort_by(|a, b| {
        (parse_link_timestamp(&a.timestamp), &a.timestamp)
            .cmp(&(parse_link_timestamp(&b.timestamp), &b.timestamp))
    });
    links.dedup();
    links.truncate(cap);
}

/// The `where` fields, at this level and in its `OR` arms, whose condition is
/// `did` itself: the bare value, or `{ eq: did }` with at most the `author`
/// the translator nests beside it. `author` is never a field here. The
/// translator emits no `AND` or `NOT`.
fn did_fields(where_clause: &Map<String, Value>, did: &str, out: &mut Vec<String>) {
    for (key, condition) in where_clause {
        match key.as_str() {
            "author" => {}
            "OR" => {
                for arm in condition.as_array().into_iter().flatten() {
                    if let Some(arm) = arm.as_object() {
                        did_fields(arm, did, out);
                    }
                }
            }
            _ => {
                let is_did = match condition {
                    Value::String(s) => s == did,
                    Value::Object(ops) => {
                        ops.get("eq").and_then(Value::as_str) == Some(did)
                            && ops.keys().all(|k| k == "eq" || k == "author")
                    }
                    _ => false,
                };
                if is_did {
                    out.push(key.clone());
                }
            }
        }
    }
}

/// Whether the rule's `author`, at any level, names `$did`. `didProperty:
/// "author"` says the same thing (the translator writes it as
/// `author: <did>`); a `where.author` beside it is a field, not an author
/// condition.
fn authored_by_grantee(query: &ModelQuery, inherited_did_property: Option<&str>) -> bool {
    let did_property = query.did_property.as_deref().or(inherited_did_property);
    let own = query.did_property.as_deref() == Some("author")
        || (did_property != Some("author")
            && query
                .r#where
                .as_ref()
                .and_then(|w| w.get("author"))
                .and_then(|c| serde_json::to_string(c).ok())
                .is_some_and(|c| c.contains("$did")));
    own || query
        .or
        .iter()
        .flatten()
        .any(|alt| authored_by_grantee(alt, did_property))
}
