//! Who may vote: resolving a rule's `fromRole` gate, **as of each vote's own
//! timestamp** (#1027).
//!
//! This is the only step of a state read that touches the store, which is
//! why it happens in the loader and not in [`fold`](super::fold). It fails
//! closed in every direction: a store error aborts the read rather than
//! mis-counting a vote; a role query that cannot tell one DID from another
//! is an error rather than "everybody passes"; a role instance that cannot be
//! placed in time is an error rather than "granted since forever".
//!
//! ## What a grant is, and what a revocation is
//!
//! A grant is a role instance — whatever class the flow author chose — that the
//! rule's query matches for a DID. A revocation is a signed tombstone link
//! on that instance, [`ROLE_GRANT_REVOKED_PREDICATE`](super::atom::ROLE_GRANT_REVOKED_PREDICATE)
//! naming the revoked DID,
//! **never a deletion**. Role instances stay in the graph for good; the tombstone
//! ends the instance's validity for that DID from the tombstone's own timestamp.
//! Both are Shared links on the same trust substrate as the votes.
//!
//! This puts one convention on social DNA authors: **role instances are add-only,
//! and membership ends only through tombstones.** The role query runs against
//! the *current* graph — only the instance's existence is windowed in time — so a
//! query keyed on a mutable property (`where: { active: true }`) reopens the
//! deletion problem through the side door: flipping the property makes the
//! instance vanish from historical verdicts too, un-settling edges its votes once
//! settled. State that changes belongs in new instances, not in edited ones.
//!
//! ## As-of gating
//!
//! Every vote is gated against the graph *as it stood at the vote's
//! timestamp*: an instance counts toward the rule's `count` for a vote at `t` iff
//! it was granted at or before `t` and no authorised revocation of it
//! existed at or before `t` — `granted_at <= t < revoked_at`. Both bounds
//! are deliberate: a grant written in the same instant as the vote counts,
//! and a revocation written in the same instant as the vote already gates
//! it ([`RoleGrantWindow::open_at`]). Eligibility is therefore a function of
//! graph *content* alone. A newcomer replica deriving from scratch reaches
//! the state everyone else holds, an edge settled by a then-eligible voter
//! stays settled after the voter is revoked, and a revocation that syncs in
//! late heals exactly like a late vote — the fold re-runs and re-derives.
//!
//! ## Where the timestamps come from
//!
//! - `granted_at` is the earliest `instance --didProperty--> did` link, when
//!   the query names a `didProperty` and that link exists — there the grant is
//!   dated from the assignment itself; otherwise the instance's own timestamp
//!   (its earliest link, as `model_query` reports it). Only that fallback is
//!   coarse: in it, membership acquired on a pre-existing instance dates from the
//!   instance, not the acquisition. Neither branch is ever "unknown, so always":
//!   an instance with no timestamp at all is an error.
//!
//!   **This paragraph described the intent from #1027 until #1065; it did not
//!   describe the code.** The `didProperty` branch had never once executed:
//!   the role's `didProperty` is a property *name* and the lookup passed it to
//!   `get_links` as an RDF *predicate*, so it matched nothing and every
//!   `didProperty` role fell through to the fallback — which is normally
//!   EARLIER than the assignment link, making the branch advertised as the
//!   precise one the widest one available. Every such grant was therefore
//!   retroactive to the instance's creation, and votes cast between then and
//!   the DID actually being assigned counted toward quorum. `didProperty` is
//!   resolved through the class shape at the store boundary since #1065
//!   (`flow_evaluator::PerspectiveInstance::did_property_predicate`); the
//!   sentence above is true from that commit forward and from no earlier one.
//!   Receipts minted before it date their grants from the instance.
//! - `revoked_at` is the tombstone's link timestamp.
//!
//! Timestamps are compared as **parsed instants** ([`super::time`]), never
//! as strings: they are client-asserted RFC 3339 and clients disagree on
//! flavour (`+00:00` vs `Z`, fractional digits, non-UTC offsets), so string
//! order diverges from instant order inside a second (#1000). Anything that
//! cannot be placed in time fails closed — see [`RoleGrantWindow::open_at`]
//! and [`fold`](super::fold) § *Ordering and time*.
//!
//! ## Authority
//!
//! A tombstone counts only when its signature verifies **and** its author
//! is one the grant's own rule would accept as a granter: the role query's
//! `author` condition (top level, and any `or` branch) is applied to the
//! tombstone's author through `model_query`'s own condition evaluator
//! ([`revocation_authorised`]). The condition is read from the *translated*
//! query — `$did` is already substituted to the candidate whose membership
//! is being tested, and never stands for anyone's author — so
//! `where: { author: "did:…admin" }` makes grants *and* revocations
//! admin-only, while `author: "$did"` requires the author to be that
//! candidate: a role that can only be self-granted is therefore also only
//! self-revoked. A rule with no author condition lets anyone grant — and,
//! symmetrically, anyone revoke — which is exactly the authority that social
//! DNA already declares.
//!
//! ## Accepted caveat: author-asserted timestamps
//!
//! A tombstone's timestamp is asserted by its author, so an admin could
//! back-date a revocation and retroactively un-settle an edge. That is an
//! escalation of *timing* only, within an authority the social DNA already
//! grants: the admin controls membership, and a back-dated revocation
//! achieves nothing a genuinely earlier one would not have. Documented, not
//! engineered around, in v1 (#1027). Roles granted as flow outputs — the
//! planned recursive composition — will carry a quorum-fixed time no single
//! party can back-date, shrinking the residual to the admin-authored base
//! case.
//!
//! ## History
//!
//! Before #1027 roles were re-derived live against the current graph and
//! revocation meant deleting the instance, so revoking someone later un-settled
//! an edge their vote had settled and replicas disagreed depending on when
//! they first derived. Windows fixed that; they were then *asserted* by the
//! replica that read them. Now the read-set carries the
//! [`RoleGrantEvidence`] — the grant links and tombstones themselves — and
//! [`RoleGrantEvidence::resolve`] recomputes the [`RoleGrantWindow`]s from
//! them, so a minted token's backing states no chronology of its own: it
//! hands over the signed material and lets the reader do the arithmetic.
//!
//! ## Same filter at collection and at read
//!
//! Whatever decides that a link counts toward a DID's membership must run
//! identically at the store boundary (when evidence is collected) and in
//! [`RoleGrantEvidence::resolve`] (when it is read) — otherwise the minter
//! and the verifier disagree, which either mints receipts that fail their own
//! verification or lets material the minter dropped widen a window. Hence one
//! predicate per link kind, exported from `flow_evaluator` and called from
//! both sites: [`grant_link_names_did`] and [`revocation_link_counts_for_did`].
//!
//! The two kinds are filtered *differently*, unchanged from pre-#1027:
//! tombstones must carry a verified signature, grant links need only name the
//! DID. Making grant links signature-filtered too is a real hole but not a
//! one-line one — `granted_at` falls back to the instance timestamp, which is
//! normally earlier than the assignment link, so dropping links can widen the
//! window rather than narrow it, and the bigger half of the hole is a missing
//! author filter. See <https://github.com/coasys/ad4m/issues/1063>, which
//! also waits on the `proof.valid` tri-state (#1046).

use super::atom::{TransitionAtom, Vote};
use super::time::parse_link_timestamp;
use crate::perspectives::flow_context::FlowInstanceRecord;
pub use crate::perspectives::flow_evaluator::RoleRevocation;
use crate::perspectives::flow_evaluator::{
    cardinality_satisfied, did_literal_url, grant_link_names_did, requires_query_input,
    revocation_link_counts_for_did, run_query, EvidenceItem, RequiresQueryable,
};
use crate::perspectives::model_query::{matches_condition, WhereCondition};
use crate::perspectives::shacl_parser::{ConsensusRule, ModelQuery, ModelQueryCount};
use crate::types::LinkExpression;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

/// One matched role instance's history for one DID: when it started to count and
/// every authorised tombstone that ended it.
///
/// A **computed view**, never a carried value. It is what
/// [`RoleGrantEvidence::resolve`] derives from the signed links in the
/// read-set, so the chronology a verdict rests on is re-derived by whoever
/// reads it rather than asserted by whoever wrote it. `Serialize` stays only
/// because surfaces render this shape; nothing in the read-set holds one.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RoleGrantWindow {
    /// The instance the role query matched for this DID.
    pub instance_id: String,
    /// When the instance started to count — see the module doc.
    pub granted_at: String,
    /// The authorised, signed tombstones on this instance naming this DID,
    /// earliest first. Usually none or one.
    pub revocations: Vec<RoleRevocation>,
}

impl RoleGrantWindow {
    /// The moment the instance stopped counting, if it has: the earliest
    /// revocation **by parsed instant** — string `min()` would pick by
    /// client format inside a sub-second collision (#1000).
    ///
    /// Fail-closed like [`Self::open_at`]: a revocation that cannot be
    /// placed in time sorts *first*, never last. `open_at` treats such a
    /// tombstone as closing the window outright, so any surface rendering
    /// this answer must show that tombstone — a later, parseable one
    /// presenting itself as the earliest (or, with no other revocations,
    /// the instance reading as "not revoked") would be the "revocation ignored"
    /// direction this module cannot afford.
    pub fn revoked_at(&self) -> Option<&str> {
        self.revocations
            .iter()
            .min_by_key(|r| {
                let parsed = parse_link_timestamp(&r.at);
                (
                    parsed.is_some(),
                    parsed.unwrap_or(chrono::DateTime::<chrono::Utc>::MIN_UTC),
                    r.at.as_str(),
                )
            })
            .map(|r| r.at.as_str())
    }

    /// Whether the instance counted for a vote cast at `at`:
    /// `granted_at <= at < revoked_at`, compared as parsed instants —
    /// timestamps are client-asserted and clients disagree on RFC 3339
    /// flavour, so string comparison would gate same-second votes by client
    /// library (#1000).
    ///
    /// **Anything that cannot be placed in time fails closed.** An
    /// unparseable `at` or `granted_at` means the window never opens for
    /// that question; an unparseable revocation timestamp closes the window
    /// outright — an authorised tombstone exists, so the safe reading is
    /// "revoked", never "revocation ignored". A revocation stamped with the
    /// vote's own timestamp already gates it.
    pub fn open_at(&self, at: &str) -> bool {
        let (Some(at), Some(granted)) = (
            parse_link_timestamp(at),
            parse_link_timestamp(&self.granted_at),
        ) else {
            return false;
        };
        if granted > at {
            return false;
        }
        let mut revoked: Option<chrono::DateTime<chrono::Utc>> = None;
        for r in &self.revocations {
            match parse_link_timestamp(&r.at) {
                None => return false,
                Some(instant) => revoked = Some(revoked.map_or(instant, |m| m.min(instant))),
            }
        }
        revoked.is_none_or(|revoked| at < revoked)
    }
}

/// The role instances behind one `(target state, DID)` pair, with their history.
///
/// A **computed view** over [`RoleGrantEvidence`], produced by
/// [`RoleGrantEvidence::resolve`] and consumed by [`eligible_votes`]. It is
/// what makes a verdict readable after the fact — "Bob counted toward
/// `approved` at 10:02 because instance r1 said he was a Reviewer from 09:00
/// and its tombstone is from 11:00" — but it is not what travels: the
/// read-set carries the links those sentences were derived from, so a reader
/// re-derives the sentence instead of believing it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RoleGrant {
    /// The state whose rule asked the question — rules are per target state,
    /// so the same DID can be eligible for one edge and not another.
    pub to_state: String,
    /// The role query's class, for readability of a serialised read-set.
    pub role_class: String,
    pub did: String,
    /// IDs of the instances the role query matched for this DID (`windows`, by
    /// ID).
    pub instances: Vec<String>,
    /// One history per matched instance, sorted by `(granted_at, instance_id)`.
    pub windows: Vec<RoleGrantWindow>,
}

impl RoleGrant {
    /// How many of the matched instances counted at `at`.
    pub fn instances_open_at(&self, at: &str) -> usize {
        self.windows.iter().filter(|w| w.open_at(at)).count()
    }

    /// Whether this DID satisfied the rule's `count` at `at` — the number of
    /// instances open then, under the same cardinality rule a `requires` guard
    /// uses (`None` = at least one).
    pub fn eligible_at(&self, at: &str, count: Option<&ModelQueryCount>) -> bool {
        cardinality_satisfied(count, self.instances_open_at(at))
    }
}

/// The link-level history of one role instance for one DID: the links
/// themselves, not a summary of them.
///
/// Nothing derived is stored here — `granted_at`, `revoked_at` and the
/// authority filter are all computed by the reader
/// ([`RoleGrantEvidence::resolve`]). That is what retires the "audit record,
/// not proof" caveat for the chronology: a minter can no longer state when a
/// grant began, only hand over the links that say so.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RoleInstanceHistory {
    /// URI of the instance the role query matched for this DID.
    pub instance_id: String,
    /// Every `instance --<didProperty>--> did` link, as read from the store —
    /// carried as plain [`LinkExpression`]: the decorated form's
    /// `proof.valid` / `status` are one executor's read-model flags, and on
    /// carried material they would be the minter's claims, so the type
    /// refuses to carry them (see
    /// [`RoleGrantLinks`](crate::perspectives::flow_evaluator::RoleGrantLinks)).
    /// Not signature-filtered — see the module header and #1063. May be empty:
    /// non-`didProperty` queries, or membership acquired without a dated
    /// assignment link.
    pub grant_links: Vec<LinkExpression>,
    /// Every signed tombstone on the instance naming this DID, carried
    /// **before** authority filtering so the reader applies
    /// [`revocation_authorised`] itself against the flow definition it holds.
    /// Never truncated: dropping a tombstone can only widen a window.
    pub revocation_links: Vec<LinkExpression>,
    /// Fallback dating when `grant_links` yields nothing: the instance's
    /// hydrated timestamp. **Asserted** by the minter — a hydration product
    /// with no single link behind it, and the one field here that stays
    /// audit-grade until `model_query` results carry per-link signatures.
    /// Flows whose role queries use `didProperty` never need it.
    pub asserted_instance_timestamp: Option<String>,
}

/// The evidence behind one `(target state, candidate DID)` pair: same keying
/// as the [`RoleGrant`] view it resolves to, links instead of derived fields.
///
/// This is what rides in [`ReadSet::role_grants`](super::ReadSet::role_grants).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RoleGrantEvidence {
    /// The state whose rule asked the question — rules are per target state,
    /// so the same DID can be eligible for one edge and not another.
    pub to_state: String,
    /// The role query's class, for readability of a serialised read-set.
    pub role_class: String,
    pub did: String,
    /// One history per instance the role query matched for this DID.
    pub instances: Vec<RoleInstanceHistory>,
}

impl RoleGrantEvidence {
    /// Recompute the [`RoleGrant`] view the fold consumes, from carried
    /// material only. **Pure**: no store, no clock, no network — so a reader
    /// holding a serialised read-set reaches the same windows this replica did.
    ///
    /// `translated_role_query` is the role's `ModelQuery` with `$did` /
    /// `$flow.base` / `$flow.instance` already substituted for this
    /// candidate ([`requires_query_input`]); the authority rule is read from
    /// it, so it must be the query for *this* `did`.
    ///
    /// Per instance:
    /// - `granted_at` = earliest RFC 3339-parseable timestamp among the grant
    ///   links naming this DID ([`grant_link_names_did`]). No qualifying link:
    ///   the asserted instance timestamp, if parseable. Neither: **`Err`**,
    ///   the same fail-closed rule the loader applies — a grant that cannot be
    ///   placed in time gates nothing rather than gating everything.
    /// - revocations = the carried tombstones that count for this DID
    ///   ([`revocation_link_counts_for_did`] — signed, and naming the DID) and
    ///   whose author [`revocation_authorised`] accepts, as `(by, at)`.
    ///
    /// # Signatures
    ///
    /// Grant links are **not** signature-filtered here, matching the
    /// collection side and the pre-#1027 behaviour; see the module header and
    /// <https://github.com/coasys/ad4m/issues/1063>.
    ///
    /// A tombstone counts only when its signature verifies, and that verdict
    /// is **computed, never carried**: the evidence types hold plain
    /// [`LinkExpression`], whose proof has no verdict field, so a read-set
    /// cannot even state one — and [`revocation_link_counts_for_did`] runs
    /// the check itself, from the signature, on every call. A forged
    /// `"valid": true` on a tombstone would have ended a grant early,
    /// changing who counted toward quorum; that attack is now unrepresentable
    /// rather than filtered. Doing the check inside the shared predicate
    /// makes it **unskippable**: there is no path from carried evidence to a
    /// [`RoleGrantWindow`] that does not go through it, so a future ingest
    /// seam cannot forget the step.
    ///
    /// This keeps `resolve` pure — the check is SHA256 plus an Ed25519
    /// verification against the author's own `did:key`: no store, no clock,
    /// no network — at a cost bounded by the number of tombstones carried.
    ///
    /// Grant links get no such treatment because they are not
    /// signature-filtered on either side; see above and #1063.
    pub fn resolve(&self, translated_role_query: &Value) -> anyhow::Result<RoleGrant> {
        let did_literal = did_literal_url(&self.did)?;
        let grant_counts = |l: &&LinkExpression| grant_link_names_did(l, &self.did, &did_literal);

        let mut windows = Vec::with_capacity(self.instances.len());
        for instance in &self.instances {
            // Earliest by parsed instant, never by string: timestamps are
            // client-asserted and clients disagree on RFC 3339 flavour, so
            // string order diverges from instant order inside a second (#1000).
            let from_links = instance
                .grant_links
                .iter()
                .filter(grant_counts)
                .filter_map(|l| parse_link_timestamp(&l.timestamp).map(|dt| (dt, &l.timestamp)))
                .min()
                .map(|(_, ts)| ts.clone());
            let Some(granted_at) = from_links
                .or_else(|| instance.asserted_instance_timestamp.clone())
                .filter(|t| parse_link_timestamp(t).is_some())
            else {
                anyhow::bail!(
                    "RoleGrantEvidence::resolve: role instance `{}` (`{}`) for `{}` carries no RFC 3339-parseable grant link and no parseable instance timestamp, so the grant cannot be placed in time; refusing to gate (fail-closed)",
                    instance.instance_id,
                    self.role_class,
                    self.did
                );
            };

            // Sorted here, not by the store: the view must not depend on the
            // order a store happens to return links in.
            let mut revocations: Vec<RoleRevocation> = instance
                .revocation_links
                .iter()
                .filter(|l| revocation_link_counts_for_did(l, &self.did, &did_literal))
                .filter(|l| revocation_authorised(translated_role_query, &l.author))
                .map(|l| RoleRevocation {
                    by: l.author.clone(),
                    at: l.timestamp.clone(),
                })
                .collect();
            revocations.sort_by(|a, b| {
                (parse_link_timestamp(&a.at), &a.at, &a.by).cmp(&(
                    parse_link_timestamp(&b.at),
                    &b.at,
                    &b.by,
                ))
            });
            revocations.dedup();

            windows.push(RoleGrantWindow {
                instance_id: instance.instance_id.clone(),
                granted_at,
                revocations,
            });
        }
        windows.sort_by(|a, b| {
            (
                parse_link_timestamp(&a.granted_at),
                &a.granted_at,
                &a.instance_id,
            )
                .cmp(&(
                    parse_link_timestamp(&b.granted_at),
                    &b.granted_at,
                    &b.instance_id,
                ))
        });

        Ok(RoleGrant {
            to_state: self.to_state.clone(),
            role_class: self.role_class.clone(),
            did: self.did.clone(),
            instances: windows.iter().map(|w| w.instance_id.clone()).collect(),
            windows,
        })
    }
}

/// Whether `author` could have written a grant the translated role query
/// accepts — and may therefore revoke one.
///
/// Reads only the `author` conditions of the translated `where` (the top
/// level, and each `OR` branch, of which at least one must accept), and
/// evaluates each with `model_query`'s own [`matches_condition`], so the
/// condition means exactly what it means for the instances. No author condition
/// anywhere accepts everyone, as the query itself does. A condition that
/// cannot be read is a refusal, never a pass.
///
/// Top-level keys plus `OR` is not a simplification: SHACL `ModelQuery`
/// grammar has exactly a flat `where` and an `OR` of flat branches — no
/// `AND`, no `NOT` — and the translator emits the same uppercase `OR` key
/// `matches_where` reads. A reader who knows `matches_where` also handles
/// `AND`/`NOT` on other paths should not wait for them here: the role-query
/// translator cannot produce them.
pub fn revocation_authorised(translated_query: &Value, author: &str) -> bool {
    fn accepted(where_clause: &Map<String, Value>, author: &str) -> bool {
        let own = match where_clause.get("author") {
            None => true,
            Some(cond) => serde_json::from_value::<WhereCondition>(cond.clone())
                .map(|c| matches_condition(&Value::String(author.to_string()), &c))
                .unwrap_or(false),
        };
        let branches = match where_clause.get("OR") {
            None => true,
            Some(Value::Array(alts)) => alts
                .iter()
                .any(|alt| alt.as_object().is_some_and(|w| accepted(w, author))),
            Some(_) => false,
        };
        own && branches
    }
    match translated_query.get("where") {
        None => true,
        Some(Value::Object(w)) => accepted(w, author),
        Some(_) => false,
    }
}

/// The `timestamp` `model_query` reports for a hydrated instance: its earliest
/// link. Absent only for an instance that hydration could not date.
fn instance_timestamp(item: &EvidenceItem) -> Option<String> {
    serde_json::from_str::<Value>(&item.content)
        .ok()?
        .get("timestamp")?
        .as_str()
        .filter(|s| !s.is_empty())
        .map(str::to_string)
}

/// Ask a `fromRole` gate about each candidate and collect the links behind
/// each matched instance's history, so the fold can gate every vote as of its
/// own timestamp — and so a reader can re-derive that gating from the links
/// rather than from our summary of them.
///
/// Both role-query shapes collapse into one per-candidate membership check,
/// because the fold only ever intersects the role set with the DIDs that
/// actually voted:
///
/// - a `$did`-templated query substitutes the candidate's DID directly;
/// - a `didProperty` query becomes `where.<prop> = candidate`, the
///   membership form of "extract the DIDs from the role instances".
///
/// A query that references the DID in neither way cannot discriminate
/// between candidates, and "I cannot determine membership" must never
/// degrade to "everyone is a member" — so it is an `Err`, as is any store or
/// translation failure. Timing is **not** decided here any more: an instance
/// that cannot be placed in time fails closed in [`RoleGrantEvidence::resolve`],
/// on both sides of the wire, rather than only on this one.
///
/// Per matched instance there is one [`RequiresQueryable::role_grant_links`]
/// call for the grant links and the signed tombstones — two `get_links`
/// queries under the live impl, so the store fan-out is
/// candidates × instances × 2. Authority is deliberately **not** applied
/// here: the tombstones travel unfiltered and the reader applies
/// [`revocation_authorised`] itself, so a minter cannot silently mis-apply
/// the rule. Nothing here is queried again by the fold.
pub async fn resolve_role_grants<Q: RequiresQueryable + ?Sized>(
    perspective: &Q,
    to_state: &str,
    role: &ModelQuery,
    record: &FlowInstanceRecord,
    candidates: &[String],
) -> anyhow::Result<Vec<RoleGrantEvidence>> {
    // `unwrap_or(false)` routes a serde failure into the same `Err` below —
    // "couldn't even inspect the rule" is the purest can't-determine case.
    let did_dependent = role.did_property.is_some()
        || serde_json::to_string(role)
            .map(|s| s.contains("$did"))
            .unwrap_or(false);
    if !did_dependent {
        anyhow::bail!(
            "resolve_role_grants: fromRole query on `{}` references neither `didProperty` nor `$did` — it cannot discriminate between DIDs, so membership is undeterminable; refusing to gate (fail-closed)",
            role.class_name
        );
    }

    // A property NAME, not a predicate. The store boundary resolves it
    // through the class's shape before querying links — see
    // `flow_evaluator::PerspectiveInstance::did_property_predicate`.
    let did_property = role.did_property.as_deref();
    let mut evidence = Vec::with_capacity(candidates.len());
    for did in candidates {
        let input = requires_query_input(role, record, did)?;
        let matched = run_query(perspective, &role.class_name, &input).await?;

        let mut instances = Vec::with_capacity(matched.len());
        for item in &matched {
            let links = perspective
                .role_grant_links(&role.class_name, &item.id, did_property, did)
                .await?;
            instances.push(RoleInstanceHistory {
                instance_id: item.id.clone(),
                grant_links: links.grant_links,
                revocation_links: links.revocation_links,
                asserted_instance_timestamp: instance_timestamp(item),
            });
        }
        // Stable by instance URI: the read-set must not depend on the order a
        // store happens to return matches in. The *view*'s order is
        // `resolve`'s business (by grant time); this is only the carrier.
        instances.sort_by(|a, b| a.instance_id.cmp(&b.instance_id));

        evidence.push(RoleGrantEvidence {
            to_state: to_state.to_string(),
            role_class: role.class_name.clone(),
            did: did.clone(),
            instances,
        });
    }
    Ok(evidence)
}

/// The votes on `atom` that its rule admits — pure, so the fold's inputs can
/// be rebuilt from a serialised read-set. A rule without a `fromRole` admits
/// every vote; with one, a vote counts only when the grant for that atom's
/// target state and that voter satisfied the rule's `count` **at the vote's
/// own timestamp** ([`RoleGrant::eligible_at`]). No grant, no vote.
pub fn eligible_votes(
    atom: &TransitionAtom,
    rule: &ConsensusRule,
    grants: &[RoleGrant],
) -> Vec<Vote> {
    let Some(role) = rule.from_role.as_ref() else {
        return atom.votes.clone();
    };
    atom.votes
        .iter()
        .filter(|vote| {
            grants.iter().any(|g| {
                g.to_state == atom.to_state
                    && g.did == vote.did
                    && g.eligible_at(&vote.at, role.count.as_ref())
            })
        })
        .cloned()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::signatures::TestSigner;
    use crate::perspectives::flow_evaluator::RoleGrantLinks;
    use async_trait::async_trait;
    use serde_json::json;
    use std::collections::HashMap;
    use std::sync::{LazyLock, Mutex};

    /// Test personas hold **real** Ed25519 keypairs, not `did:key:alice`
    /// placeholders, because [`RoleGrantEvidence::resolve`] recomputes every
    /// tombstone's signature rather than reading its carried `proof.valid`.
    /// A fixture that merely *claims* `valid: true` is precisely the minter's
    /// word the reader no longer takes, so a fixture that wants a tombstone to
    /// count has to sign it for real.
    ///
    /// Leaked on first use so the DIDs are `&'static str` and read like the
    /// constants they replaced. One keypair per persona per process.
    fn persona(name: &str) -> &'static TestSigner {
        static SIGNERS: LazyLock<Mutex<HashMap<String, &'static TestSigner>>> =
            LazyLock::new(|| Mutex::new(HashMap::new()));
        *SIGNERS
            .lock()
            .expect("persona registry")
            .entry(name.to_string())
            .or_insert_with(|| Box::leak(Box::new(TestSigner::generate())))
    }

    /// The signer behind a DID one of the fixtures produced, for re-signing.
    fn persona_for_did(did: &str) -> Option<&'static TestSigner> {
        SIGNER_NAMES
            .iter()
            .map(|n| persona(n))
            .find(|s| s.did == did)
    }

    const SIGNER_NAMES: [&str; 5] = ["alice", "bob", "admin", "lead", "mallory"];

    #[allow(non_snake_case)]
    fn ALICE() -> &'static str {
        &persona("alice").did
    }
    #[allow(non_snake_case)]
    fn BOB() -> &'static str {
        &persona("bob").did
    }
    #[allow(non_snake_case)]
    fn ADMIN() -> &'static str {
        &persona("admin").did
    }
    #[allow(non_snake_case)]
    fn LEAD() -> &'static str {
        &persona("lead").did
    }
    #[allow(non_snake_case)]
    fn MALLORY() -> &'static str {
        &persona("mallory").did
    }
    const T0: &str = "2026-01-01T00:00:00.000Z";
    const T1: &str = "2026-01-02T00:00:00.000Z";
    const T2: &str = "2026-01-03T00:00:00.000Z";
    const T3: &str = "2026-01-04T00:00:00.000Z";
    const T4: &str = "2026-01-05T00:00:00.000Z";
    /// Later than any grant or tombstone a test writes: "eligible now".
    const NOW: &str = "2030-01-01T00:00:00.000Z";

    fn record() -> FlowInstanceRecord {
        FlowInstanceRecord {
            flow_uri: "delivery://DeliveryFlow".into(),
            instance_uri: "ad4m://flow/instance/1".into(),
            subject: "ad4m://task/onboarding".into(),
            current_state: "review".into(),
            created_at: None,
        }
    }

    fn role(v: Value) -> ModelQuery {
        serde_json::from_value(v).expect("role query deserializes")
    }

    fn dids(names: &[&str]) -> Vec<String> {
        names.iter().map(|s| s.to_string()).collect()
    }

    fn revocation(by: &str, at: &str) -> RoleRevocation {
        RoleRevocation {
            by: by.into(),
            at: at.into(),
        }
    }

    /// One link as the evidence types carry it. Author, target, signature
    /// validity and timestamp are all inputs the filters read, so every
    /// fixture states them explicitly.
    ///
    /// `valid` is honoured **cryptographically** — the carried form has no
    /// `proof.valid` a fixture could set: a valid link is signed by
    /// `author`'s own key over its own data and timestamp, and a forged one
    /// carries a signature from a key that is not `author`'s. The filters
    /// compute the verdict from the signature, so that is the only lever a
    /// fixture has.
    fn role_link(
        predicate: &str,
        target: &str,
        author: &str,
        valid: bool,
        timestamp: &str,
    ) -> LinkExpression {
        use crate::types::Link as CoreLink;
        let at = chrono::DateTime::parse_from_rfc3339(timestamp)
            .unwrap_or_else(|e| panic!("fixture timestamp `{timestamp}`: {e}"))
            .with_timezone(&chrono::Utc);
        let signer = persona_for_did(author)
            .unwrap_or_else(|| panic!("fixture author `{author}` is not a known persona"));
        // A forged link states `author` but is signed by somebody else's key —
        // exactly what a link whose signature does not check out looks like.
        let signing_key = if valid { signer } else { persona("forger") };
        let mut expr = signing_key.sign_at(
            CoreLink {
                source: "r0".to_string(),
                predicate: Some(predicate.to_string()),
                target: target.to_string(),
            }
            .normalize(),
            at,
        );
        expr.author = author.to_string();
        expr.proof.key = format!("{author}#key");
        LinkExpression::from(expr)
    }

    /// A signed `instance --agent--> did` grant link at `at`.
    fn grant_link(did: &str, at: &str) -> LinkExpression {
        role_link("agent", did, ADMIN(), true, at)
    }

    /// A signed tombstone by `by` at `at`.
    fn tombstone(did: &str, by: &str, at: &str) -> LinkExpression {
        role_link(
            crate::perspectives::flow_instance::atom::ROLE_GRANT_REVOKED_PREDICATE,
            did,
            by,
            true,
            at,
        )
    }

    /// The store's answer for one DID: a grant link at `granted_at` (when
    /// given) plus one signed tombstone per `(by, at)`.
    fn history(
        did: &str,
        granted_at: Option<&str>,
        revocations: &[(&str, &str)],
    ) -> RoleGrantLinks {
        RoleGrantLinks {
            grant_links: granted_at
                .map(|at| grant_link(did, at))
                .into_iter()
                .collect(),
            revocation_links: revocations
                .iter()
                .map(|(by, at)| tombstone(did, by, at))
                .collect(),
        }
    }

    /// The translated role query for `did` — what `resolve` reads the
    /// authority rule from, exactly as `fold_read_set` builds it.
    fn translated(role: &ModelQuery, did: &str) -> Value {
        requires_query_input(role, &record(), did).expect("role query translates")
    }

    /// Resolve every candidate's evidence into the view the gate consumes.
    fn views(evidence: &[RoleGrantEvidence], role: &ModelQuery) -> Vec<RoleGrant> {
        evidence
            .iter()
            .map(|e| {
                e.resolve(&translated(role, &e.did))
                    .unwrap_or_else(|err| panic!("evidence for {} resolves: {err:#}", e.did))
            })
            .collect()
    }

    /// The DIDs whose grants satisfy `count` at [`NOW`].
    fn eligible_now<'g>(grants: &'g [RoleGrant], count: Option<&ModelQueryCount>) -> Vec<&'g str> {
        grants
            .iter()
            .filter(|g| g.eligible_at(NOW, count))
            .map(|g| g.did.as_str())
            .collect()
    }

    /// Query-aware stub: a call whose JSON mentions one of `member_dids`
    /// returns `rows_per_match` instances (`r0`, `r1`, …, each dated [`T0`] unless
    /// `undated_instances`); `unconditional_instances` (for DID-independent queries)
    /// wins over matching when set; `error` fails every call. `histories`
    /// is what the store says about each DID's instances.
    #[derive(Default)]
    struct RoleStub {
        member_dids: Vec<String>,
        rows_per_match: usize,
        unconditional_instances: Option<usize>,
        undated_instances: bool,
        error: Option<String>,
        calls: Mutex<Vec<String>>,
        histories: HashMap<String, RoleGrantLinks>,
    }

    #[async_trait]
    impl RequiresQueryable for RoleStub {
        async fn model_query(&self, _class: &str, query_json: &str) -> anyhow::Result<String> {
            self.calls.lock().unwrap().push(query_json.to_string());
            if let Some(msg) = &self.error {
                return Err(anyhow::anyhow!(msg.clone()));
            }
            let n = self.unconditional_instances.unwrap_or_else(|| {
                if self
                    .member_dids
                    .iter()
                    .any(|d| query_json.contains(d.as_str()))
                {
                    self.rows_per_match
                } else {
                    0
                }
            });
            let instances: Vec<Value> = (0..n)
                .map(|i| {
                    if self.undated_instances {
                        json!({ "id": format!("r{i}") })
                    } else {
                        json!({ "id": format!("r{i}"), "timestamp": T0, "author": ADMIN() })
                    }
                })
                .collect();
            Ok(json!({ "instances": instances, "totalCount": n }).to_string())
        }

        async fn role_grant_links(
            &self,
            _role_class: &str,
            _instance_id: &str,
            _did_property: Option<&str>,
            did: &str,
        ) -> anyhow::Result<RoleGrantLinks> {
            Ok(self.histories.get(did).cloned().unwrap_or_default())
        }
    }

    fn members(member_dids: &[&str]) -> RoleStub {
        RoleStub {
            member_dids: dids(member_dids),
            rows_per_match: 1,
            ..Default::default()
        }
    }

    /// `(name, role query, member DIDs, candidates, expected eligible set,
    /// expected query count)`.
    #[tokio::test]
    #[rustfmt::skip]
    async fn both_role_query_shapes_resolve_per_candidate() {
        let cases: Vec<(&str, Value, Vec<&str>, Vec<&str>, Vec<&str>, Option<usize>)> = vec![
            ("shape 1: didProperty filters candidates",
             json!({ "className": "ns://Reviewer", "didProperty": "agent" }),
             vec![ALICE()], vec![ALICE(), BOB()], vec![ALICE()], Some(2)),
            ("shape 2: $did token substitutes per candidate",
             json!({ "className": "ns://Member", "where": { "member": "$did" } }),
             vec![BOB()], vec![ALICE(), BOB()], vec![BOB()], None),
            ("one role instance does not satisfy count.min = 2",
             json!({ "className": "ns://Reviewer", "didProperty": "agent", "count": { "min": 2 } }),
             vec![ALICE()], vec![ALICE()], vec![], None),
        ];

        for (name, role_json, member_dids, candidates, expected, expect_calls) in cases {
            let stub = members(&member_dids);
            let role = role(role_json);
            let evidence =
                resolve_role_grants(&stub, "approved", &role, &record(), &dids(&candidates))
                    .await
                    .unwrap();
            let grants = views(&evidence, &role);
            assert_eq!(eligible_now(&grants, role.count.as_ref()), expected.to_vec(), "{name}");
            assert_eq!(grants.len(), candidates.len(), "{name}: one verdict per candidate");
            for g in &grants {
                assert_eq!(g.to_state, "approved", "{name}: verdicts are per target state");
                assert_eq!(g.instances, g.windows.iter().map(|w| w.instance_id.clone()).collect::<Vec<_>>(),
                           "{name}: `instances` is the windows' IDs");
                for w in &g.windows {
                    assert_eq!(w.granted_at, T0, "{name}: an undated grant link dates from the instance");
                    assert!(w.revocations.is_empty(), "{name}: no tombstones were reported");
                }
            }

            if let Some(n) = expect_calls {
                let calls = stub.calls.lock().unwrap();
                assert_eq!(calls.len(), n, "{name}: one membership query per candidate");
                for (call, candidate) in calls.iter().zip(candidates) {
                    assert!(call.contains(candidate),
                            "{name}: each query carries its candidate's DID: {calls:?}");
                }
            }
        }
    }

    /// Every failure mode fails CLOSED — error out, never degrade to
    /// "everyone passes". A `fromRole` is a security rule, and a
    /// misconfigured security rule admits nobody.
    #[tokio::test]
    #[rustfmt::skip]
    async fn every_role_failure_is_an_error_not_allow_all() {
        let cases: Vec<(&str, RoleStub, Value, &str)> = vec![
            ("a query that cannot discriminate between DIDs",
             RoleStub { unconditional_instances: Some(1), ..Default::default() },
             json!({ "className": "ns://Quorum", "where": { "open": true } }), "cannot discriminate"),
            ("a store error",
             RoleStub { error: Some("store down".into()), ..Default::default() },
             json!({ "className": "ns://Reviewer", "didProperty": "agent" }), "store down"),
            ("a role query model_query cannot express",
             RoleStub::default(),
             json!({ "className": "ns://Reviewer", "didProperty": "agent",
                     "where": { "status": { "matches": ".*" } } }), ""),
        ];

        for (name, stub, role_json, expect_contains) in cases {
            let err = resolve_role_grants(&stub, "approved", &role(role_json), &record(), &dids(&[ALICE()]))
                .await
                .expect_err(name);
            assert!(err.to_string().contains(expect_contains), "{name}: got {err:#}");
        }

        // Timing fails closed one step later, in the pure resolve, so it fails
        // the same way for a reader off-perspective: an instance with no signed
        // grant link and no instance timestamp cannot be placed in time, and
        // an unplaceable grant gates nothing rather than gating everything.
        let stub = RoleStub { undated_instances: true, ..members(&[ALICE()]) };
        let undated = role(json!({ "className": "ns://Reviewer", "didProperty": "agent" }));
        let evidence = resolve_role_grants(&stub, "approved", &undated, &record(), &dids(&[ALICE()]))
            .await
            .expect("collecting the links themselves cannot fail on timing");
        let err = evidence[0].resolve(&translated(&undated, ALICE())).expect_err("undated instance");
        assert!(err.to_string().contains("cannot be placed in time"), "got {err:#}");

        // And the undeterminable rule never even runs a query.
        let stub = RoleStub { unconditional_instances: Some(1), ..Default::default() };
        let _ = resolve_role_grants(
            &stub, "approved",
            &role(json!({ "className": "ns://Quorum", "where": { "open": true } })),
            &record(), &dids(&[ALICE()]),
        ).await;
        assert!(stub.calls.lock().unwrap().is_empty(), "no query may run for an undeterminable rule");
    }

    fn atom_with_votes(votes: &[(&str, &str)]) -> TransitionAtom {
        let votes: Vec<Vote> = votes
            .iter()
            .map(|(did, at)| Vote {
                did: did.to_string(),
                at: at.to_string(),
            })
            .collect();
        TransitionAtom {
            uri: "p1".into(),
            from_state: "review".into(),
            to_state: "approved".into(),
            proposer: votes.first().map(|v| v.did.clone()).unwrap_or_default(),
            proposed_at: votes.first().map(|v| v.at.clone()).unwrap_or_default(),
            evidence_hash: "seal".into(),
            votes,
        }
    }

    fn gated_rule(count: Option<Value>) -> ConsensusRule {
        let mut q = json!({ "className": "ns://Reviewer", "didProperty": "agent" });
        if let Some(c) = count {
            q["count"] = c;
        }
        ConsensusRule {
            n: 1,
            from_role: Some(role(q)),
        }
    }

    fn window(
        instance_id: &str,
        granted_at: &str,
        revocations: &[(&str, &str)],
    ) -> RoleGrantWindow {
        RoleGrantWindow {
            instance_id: instance_id.into(),
            granted_at: granted_at.into(),
            revocations: revocations
                .iter()
                .map(|(by, at)| revocation(by, at))
                .collect(),
        }
    }

    fn grant(to_state: &str, did: &str, windows: Vec<RoleGrantWindow>) -> RoleGrant {
        RoleGrant {
            to_state: to_state.into(),
            role_class: "ns://Reviewer".into(),
            did: did.into(),
            instances: windows.iter().map(|w| w.instance_id.clone()).collect(),
            windows,
        }
    }

    #[test]
    fn a_rule_without_a_role_admits_every_vote_and_one_with_a_role_admits_only_grantees() {
        let atom = atom_with_votes(&[(ALICE(), T1), (BOB(), T2)]);
        let open = ConsensusRule {
            n: 1,
            from_role: None,
        };
        assert_eq!(eligible_votes(&atom, &open, &[]).len(), 2);

        let gated = gated_rule(None);
        assert!(
            eligible_votes(&atom, &gated, &[]).is_empty(),
            "no grant, no vote — an unresolved gate admits nobody"
        );
        let grants = vec![
            grant("approved", BOB(), vec![window("r0", T0, &[])]),
            grant("approved", ALICE(), vec![]),
            grant("shipped", ALICE(), vec![window("r1", T0, &[])]),
        ];
        assert_eq!(
            eligible_votes(&atom, &gated, &grants)
                .into_iter()
                .map(|v| v.did)
                .collect::<Vec<_>>(),
            vec![BOB()],
            "a grant with no instances admits nobody, and a grant for another target state is not a grant for this one"
        );
    }

    /// The as-of rule, including both boundaries: an instance counts for a vote at
    /// `t` iff `granted_at <= t < revoked_at`. `(name, vote at, granted at,
    /// revoked at, counts?)`.
    #[test]
    #[rustfmt::skip]
    fn a_vote_counts_only_while_the_grant_was_open_at_its_own_timestamp() {
        let cases: Vec<(&str, &str, &str, Option<&str>, bool)> = vec![
            ("vote after grant, never revoked",             T2, T1, None,     true),
            ("vote before the grant existed",               T1, T2, None,     false),
            ("vote at exactly the grant's timestamp",       T1, T1, None,     true),
            ("vote before the revocation",                  T2, T1, Some(T3), true),
            ("vote after the revocation",                   T4, T1, Some(T3), false),
            ("vote at exactly the revocation's timestamp",  T3, T1, Some(T3), false),
            ("revocation back-dated before the grant",      T2, T1, Some(T0), false),
        ];
        for (name, vote_at, granted_at, revoked_at, counts) in cases {
            let revocations: Vec<(&str, &str)> = revoked_at.map(|at| (ADMIN(), at)).into_iter().collect();
            let grants = vec![grant("approved", ALICE(), vec![window("r0", granted_at, &revocations)])];
            let atom = atom_with_votes(&[(ALICE(), vote_at)]);
            assert_eq!(eligible_votes(&atom, &gated_rule(None), &grants).len(), usize::from(counts), "{name}");
        }
    }

    /// The gate is per vote, not per atom or per DID: on one atom, the same
    /// agent's vote before the tombstone counts and their vote after it does
    /// not — which is what keeps an edge they settled settled.
    #[test]
    fn a_revocation_gates_only_votes_cast_after_it() {
        let atom = atom_with_votes(&[(ALICE(), T2), (ALICE(), T4)]);
        let before = vec![grant("approved", ALICE(), vec![window("r0", T1, &[])])];
        let after = vec![grant(
            "approved",
            ALICE(),
            vec![window("r0", T1, &[(ADMIN(), T3)])],
        )];
        assert_eq!(eligible_votes(&atom, &gated_rule(None), &before).len(), 2);
        let eligible = eligible_votes(&atom, &gated_rule(None), &after);
        assert_eq!(
            eligible.iter().map(|v| v.at.as_str()).collect::<Vec<_>>(),
            vec![T2],
            "the pre-revocation vote survives the revocation; the later one is gated"
        );
    }

    /// `count` is evaluated as of the vote too: with `min: 2`, a DID whose
    /// second instance was revoked at T3 still had two instances at T2 and one at T4.
    #[test]
    fn the_rules_count_is_evaluated_as_of_the_vote() {
        let grants = vec![grant(
            "approved",
            ALICE(),
            vec![window("r0", T0, &[]), window("r1", T1, &[(ADMIN(), T3)])],
        )];
        let rule = gated_rule(Some(json!({ "min": 2 })));
        assert_eq!(
            eligible_votes(&atom_with_votes(&[(ALICE(), T2)]), &rule, &grants).len(),
            1
        );
        assert!(eligible_votes(&atom_with_votes(&[(ALICE(), T4)]), &rule, &grants).is_empty());
    }

    /// A tombstone counts only from an author the grant's own rule accepts:
    /// the role query's `author` condition is applied to the tombstone's
    /// author. `(name, role query, accepted revokers, rejected revokers)` —
    /// every revoker writes a tombstone at [`T2`] and the window keeps only
    /// the accepted ones.
    #[tokio::test]
    #[rustfmt::skip]
    async fn a_tombstone_counts_only_from_an_author_the_grants_rule_accepts() {
        let cases: Vec<(&str, Value, Vec<&str>, Vec<&str>)> = vec![
            ("no author condition: anyone may grant, so anyone may revoke",
             json!({ "className": "ns://Reviewer", "didProperty": "agent" }),
             vec![ADMIN(), MALLORY(), ALICE()], vec![]),
            ("admin-only grants are admin-only revocations",
             json!({ "className": "ns://Reviewer", "didProperty": "agent", "where": { "author": ADMIN() } }),
             vec![ADMIN()], vec![MALLORY(), ALICE()]),
            ("a set of authorised granters",
             json!({ "className": "ns://Reviewer", "didProperty": "agent", "where": { "author": { "in": [ADMIN(), LEAD()] } } }),
             vec![ADMIN(), LEAD()], vec![MALLORY(), ALICE()]),
            ("`or` branches: a revoker any branch would accept as granter",
             json!({ "className": "ns://Reviewer", "didProperty": "agent",
                     "or": [ { "className": "ns://Reviewer", "where": { "author": ADMIN() } },
                             { "className": "ns://Reviewer", "where": { "author": LEAD() } } ] }),
             vec![ADMIN(), LEAD()], vec![MALLORY(), ALICE()]),
            ("`author: $did`: a self-granted role is self-revoked",
             json!({ "className": "ns://Reviewer", "didProperty": "agent", "where": { "author": "$did" } }),
             vec![ALICE()], vec![ADMIN(), MALLORY()]),
        ];
        for (name, role_json, accepted, rejected) in cases {
            let mut stub = members(&[ALICE()]);
            let revokers: Vec<(&str, &str)> =
                accepted.iter().chain(rejected.iter()).map(|by| (*by, T2)).collect();
            stub.histories.insert(ALICE().into(), history(ALICE(), Some(T1), &revokers));
            let role = role(role_json);
            let evidence = resolve_role_grants(&stub, "approved", &role, &record(), &dids(&[ALICE()]))
                .await
                .unwrap();
            assert_eq!(
                evidence[0].instances[0].revocation_links.len(), revokers.len(),
                "{name}: every signed tombstone travels — the reader, not the minter, applies authority"
            );
            let grants = views(&evidence, &role);
            let kept: Vec<&str> = grants[0].windows[0].revocations.iter().map(|r| r.by.as_str()).collect();
            let mut expected: Vec<&str> = accepted.to_vec();
            expected.sort();
            assert_eq!(kept, expected, "{name}");
            assert_eq!(grants[0].eligible_at(T3, None), accepted.is_empty(), "{name}: revoked iff someone authorised did it");
            assert!(grants[0].eligible_at(T1, None), "{name}: the vote before any tombstone still counts");
        }
    }

    /// The window is derived from the carried links, in a deterministic order:
    /// the grant link's timestamp when there is one, else the instance's own;
    /// the authorised tombstones earliest first.
    #[tokio::test]
    async fn windows_are_derived_from_the_carried_links() {
        let mut stub = RoleStub {
            rows_per_match: 2,
            ..members(&[ALICE(), BOB()])
        };
        stub.histories.insert(
            ALICE().into(),
            history(ALICE(), Some(T1), &[(MALLORY(), T3), (ADMIN(), T2)]),
        );
        // Bob's instances have no `didProperty` link the store could date: they
        // date from the instances themselves (T0), and nothing revoked them.
        let role = role(json!({ "className": "ns://Reviewer", "didProperty": "agent" }));
        let evidence = resolve_role_grants(
            &stub,
            "approved",
            &role,
            &record(),
            &dids(&[ALICE(), BOB()]),
        )
        .await
        .unwrap();
        assert_eq!(
            evidence[1].instances[0].asserted_instance_timestamp.as_deref(),
            Some(T0),
            "with no grant link to carry, the instance's hydrated timestamp is what travels — asserted, and named as such"
        );
        let grants = views(&evidence, &role);
        let alice = &grants[0];
        assert_eq!(alice.did, ALICE());
        assert_eq!(alice.instances, vec!["r0", "r1"]);
        for w in &alice.windows {
            assert_eq!(w.granted_at, T1);
            assert_eq!(
                w.revocations,
                vec![revocation(ADMIN(), T2), revocation(MALLORY(), T3)],
                "no author condition, so both tombstones count, earliest first"
            );
            assert_eq!(w.revoked_at(), Some(T2));
        }
        let bob = &grants[1];
        assert!(bob
            .windows
            .iter()
            .all(|w| w.granted_at == T0 && w.revocations.is_empty()));
        assert!(bob.eligible_at(NOW, None));
        assert!(!alice.eligible_at(NOW, None));
    }

    /// A tombstone whose signature does not check out is not a revocation —
    /// the vote-layer rule (`atom::signed_by`: a link whose verdict is not
    /// `valid` is not a link anyone wrote), applied to tombstones by
    /// [`revocation_link_counts_for_did`]. Pinned here because the reshape
    /// moved the filter from the store boundary into the pure reader, and this
    /// is the assertion that says it survived the move.
    ///
    /// This test used to have a second half: the same bad signature with
    /// `"valid": true` written on it, pinning that `resolve` re-derives the
    /// verdict instead of reading the carried one. That attack is now
    /// **unrepresentable**: the evidence carries plain [`LinkExpression`],
    /// whose proof has no verdict field, so a read-set cannot state a verdict
    /// at all — the signature is the only thing a sender controls, and it is
    /// exactly what this test forges (r4076927995).
    ///
    /// Grant links deliberately have no such check yet; see #1063.
    #[test]
    fn a_tombstone_whose_signature_fails_does_not_revoke_whatever_it_claims() {
        let role = role(json!({ "className": "ns://Reviewer", "didProperty": "agent" }));
        let query = translated(&role, ALICE());

        // Signature genuinely does not verify: stated author ADMIN, signed by
        // the forger persona's key.
        let forged = role_link(
            crate::perspectives::flow_instance::atom::ROLE_GRANT_REVOKED_PREDICATE,
            ALICE(),
            ADMIN(),
            false,
            T2,
        );
        let grant = RoleGrantEvidence {
            to_state: "approved".into(),
            role_class: "ns://Reviewer".into(),
            did: ALICE().into(),
            instances: vec![RoleInstanceHistory {
                instance_id: "r0".into(),
                grant_links: vec![grant_link(ALICE(), T1)],
                revocation_links: vec![forged],
                asserted_instance_timestamp: None,
            }],
        }
        .resolve(&query)
        .expect("resolves");
        assert!(
            grant.windows[0].revocations.is_empty(),
            "an unsigned tombstone is not a link anyone wrote"
        );
        assert!(
            grant.eligible_at(NOW, None),
            "a forged tombstone must not shrink the window"
        );
    }

    /// A tombstone naming *someone else* is not this DID's revocation, and a
    /// grant link naming someone else does not date this DID's grant — the
    /// target half of the same predicate.
    #[test]
    fn links_naming_another_did_are_not_this_dids_history() {
        let role = role(json!({ "className": "ns://Reviewer", "didProperty": "agent" }));
        let grant = RoleGrantEvidence {
            to_state: "approved".into(),
            role_class: "ns://Reviewer".into(),
            did: ALICE().into(),
            instances: vec![RoleInstanceHistory {
                instance_id: "r0".into(),
                grant_links: vec![grant_link(BOB(), T0), grant_link(ALICE(), T3)],
                revocation_links: vec![tombstone(BOB(), ADMIN(), T4)],
                asserted_instance_timestamp: None,
            }],
        }
        .resolve(&translated(&role, ALICE()))
        .expect("resolves");
        assert_eq!(
            grant.windows[0].granted_at, T3,
            "Bob's earlier grant link does not date Alice's grant"
        );
        assert!(
            grant.windows[0].revocations.is_empty(),
            "Bob's tombstone does not revoke Alice"
        );
    }

    /// `revoked_at` must share `open_at`'s fail direction: a revocation that
    /// cannot be placed in time closes the window (`open_at`), so it is also
    /// the one `revoked_at` surfaces — never out-ranked by a later,
    /// parseable tombstone, and never dropped into "not revoked".
    #[test]
    fn an_unparseable_revocation_is_surfaced_never_ignored() {
        let w = RoleGrantWindow {
            instance_id: "r0".into(),
            granted_at: T0.into(),
            revocations: vec![
                revocation(ADMIN(), "not-a-timestamp"),
                revocation(ADMIN(), T2),
            ],
        };
        assert_eq!(w.revoked_at(), Some("not-a-timestamp"));
        assert!(!w.open_at(NOW), "unparseable revocation closes the window");

        let only_garbage = RoleGrantWindow {
            instance_id: "r0".into(),
            granted_at: T0.into(),
            revocations: vec![revocation(ADMIN(), "not-a-timestamp")],
        };
        assert_eq!(only_garbage.revoked_at(), Some("not-a-timestamp"));
    }
}
