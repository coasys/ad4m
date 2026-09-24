//! The carried role evidence — grant links and tombstones — and how a
//! window is derived from it by whoever reads it.

use super::{RoleGrant, RoleGrantWindow, RoleRevocation};
use crate::perspectives::flow_evaluator::{
    cardinality_satisfied, did_literal_url, grant_link_names_did, revocation_link_counts_for_did,
    EvidenceItem,
};
use crate::perspectives::flow_instance::time::parse_link_timestamp;
use crate::perspectives::model_query::{matches_condition, WhereCondition};
use crate::perspectives::shacl_parser::ModelQuery;
use crate::types::LinkExpression;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
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
    /// For a role query that declares `producedByFlow`: when the granting
    /// flow first produced this instance — the earliest verified receipt's
    /// quorum time, as the collecting replica checked it against its own
    /// graph ([`grant`](super::super::grant)). `None` when no verified
    /// receipt names it, and for every other role. Omitted from the
    /// serialised form when `None`.
    ///
    /// **Carried, not re-derived.** The receipt behind it does not travel, so
    /// a reader of a serialised read-set trusts this date the way it trusts
    /// that the carried links are all the links there were. See
    /// [`grant`](super::super::grant) § *What a receipt of a gated flow
    /// proves*.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub produced_at: Option<String>,
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
    ///
    /// # `producedByFlow`
    ///
    /// `role` is the role query **from the reader's own flow definition**,
    /// never from the carried evidence: a minter who could name the gate
    /// would be naming the rule its own receipt is judged by.
    ///
    /// When it declares `producedByFlow`, the instance counts only when it
    /// carries a [`RoleInstanceHistory::produced_at`], and **that date
    /// replaces every other dating**: the assignment links and
    /// `asserted_instance_timestamp` are not consulted, not even as a
    /// fallback. Were they, writing a plain assignment link would grant the
    /// role with no receipt at all and the gate would be decorative.
    ///
    /// An instance with no `produced_at` contributes no window, exactly as if
    /// the role query had not matched it. That is an ordinary "not a member",
    /// distinct from the `Err` above — which exists for a grant that *is*
    /// claimed and cannot be placed in time. Tombstones still apply, and are
    /// the only way such a grant ever ends; see [`grant`](super::super::grant)
    /// § *Revocation*.
    ///
    /// `count` is here only to refuse one shape outright: a `count` that is
    /// satisfied by **zero** matching instances, such as `{ max: 0 }`. Paired
    /// with `producedByFlow` that inverts the gate into "eligible while no
    /// verified receipt exists", so every reason a receipt might fail to
    /// verify — an un-synced flow definition, a broken signature — becomes a
    /// reason to *grant*. Fail-closed has to mean the same thing at both ends
    /// of the rule, so this combination is an error rather than a subtlety.
    pub fn resolve(
        &self,
        translated_role_query: &Value,
        role: &ModelQuery,
    ) -> anyhow::Result<RoleGrant> {
        let produced_by = role.produced_by_flow.as_ref();
        if produced_by.is_some() && cardinality_satisfied(role.count.as_ref(), 0) {
            anyhow::bail!(
                "RoleGrantEvidence::resolve: the `{}` role gate combines `producedByFlow` with a `count` satisfied by zero instances, so a receipt that FAILS to verify would make `{}` eligible rather than ineligible; refusing to gate (fail-closed)",
                role.class_name,
                self.did
            );
        }
        let did_literal = did_literal_url(&self.did)?;
        let grant_counts = |l: &&LinkExpression| grant_link_names_did(l, &self.did, &did_literal);

        let mut windows = Vec::with_capacity(self.instances.len());
        for instance in &self.instances {
            // A `producedByFlow` gate dates the grant from the granting run's
            // quorum and from nothing else — see § producedByFlow above. An
            // instance no receipt produced is simply not a member, so it is
            // skipped rather than raised.
            if produced_by.is_some() {
                match &instance.produced_at {
                    Some(granted_at) => windows.push(RoleGrantWindow {
                        instance_id: instance.instance_id.clone(),
                        granted_at: granted_at.clone(),
                        revocations: self.revocations_on(
                            instance,
                            translated_role_query,
                            &did_literal,
                        ),
                    }),
                    None => log::debug!(
                        "producedByFlow: `{}` does not count toward `{}` for `{}`: no verified \
                         receipt produced it",
                        instance.instance_id,
                        role.class_name,
                        self.did
                    ),
                }
                continue;
            }

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

            windows.push(RoleGrantWindow {
                instance_id: instance.instance_id.clone(),
                granted_at,
                revocations: self.revocations_on(instance, translated_role_query, &did_literal),
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

    /// The authorised, signed tombstones on one instance naming this DID,
    /// earliest first — the `revocations` half of a [`RoleGrantWindow`].
    ///
    /// Shared by both dating branches on purpose. A `producedByFlow` grant is
    /// dated by a quorum instead of by a link, but it ends exactly the way
    /// every other role grant ends: a new signed tombstone from an author
    /// [`revocation_authorised`] accepts. Splitting the two branches'
    /// revocation handling is how that would quietly stop being true.
    fn revocations_on(
        &self,
        instance: &RoleInstanceHistory,
        translated_role_query: &Value,
        did_literal: &str,
    ) -> Vec<RoleRevocation> {
        // Sorted here, not by the store: the view must not depend on the
        // order a store happens to return links in.
        let mut revocations: Vec<RoleRevocation> = instance
            .revocation_links
            .iter()
            .filter(|l| revocation_link_counts_for_did(l, &self.did, did_literal))
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
        revocations
    }
}

/// Whether `author` could have written a grant the translated role query
/// accepts — and may therefore revoke one.
///
/// Reads only the `author` conditions of the translated `where`, level by
/// level (the top, and each `OR` branch, of which at least one must accept),
/// and evaluates each with `model_query`'s own [`matches_condition`], so the
/// condition means exactly what it means for the instances. At a level, every
/// author condition must accept: the ones the translator nests under each
/// field of the level (`{ agent: { eq: did, author: A } }`, the grant link's
/// author, and the same `author` under every other field, an `or` arm's fields
/// included when the arm inherits its level's), and a top-level
/// `author` (emitted when the level has no field to nest it under). These are
/// the conditions the grant query itself
/// requires, so grants and revocations stay symmetric. No author condition
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
        let conditions = where_clause
            .iter()
            .filter_map(|(key, value)| match key.as_str() {
                "OR" | "AND" | "NOT" => None,
                "author" => Some(value),
                _ => value.as_object().and_then(|ops| ops.get("author")),
            });
        let own = conditions.into_iter().all(|cond| {
            serde_json::from_value::<WhereCondition>(cond.clone())
                .map(|c| matches_condition(&Value::String(author.to_string()), &c))
                .unwrap_or(false)
        });
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
pub(super) fn instance_timestamp(item: &EvidenceItem) -> Option<String> {
    serde_json::from_str::<Value>(&item.content)
        .ok()?
        .get("timestamp")?
        .as_str()
        .filter(|s| !s.is_empty())
        .map(str::to_string)
}

#[cfg(test)]
mod tests {
    use super::super::resolve_role_grants;
    use super::super::test_support::*;
    use super::*;
    use crate::perspectives::flow_evaluator::RequiresQueryable;
    use crate::perspectives::flow_evaluator::requires_query_input;
    use crate::perspectives::flow_evaluator::RoleGrantLinks;
    use crate::perspectives::shacl_parser::ModelQuery;
    use serde_json::json;
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
            stub.histories.insert("r0".into(), history(ALICE(), Some(T1), &revokers));
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

    /// The authority rule reads the author where the translator now puts it,
    /// nested under every field of its level, and still ignores a revocation
    /// by anyone the grant rule would not accept as a granter (#1114).
    #[test]
    fn a_revocation_by_a_non_admin_is_ignored_under_the_nested_translation() {
        let role = role(
            json!({ "className": "ns://Reviewer", "didProperty": "agent", "where": { "author": ADMIN() } }),
        );
        let translated = requires_query_input(&role, &record(), ALICE()).unwrap();
        assert_eq!(
            translated["where"]["agent"],
            json!({ "eq": ALICE(), "author": ADMIN() }),
            "the grant is admin's `agent` link, per link"
        );
        assert!(revocation_authorised(&translated, ADMIN()));
        for revoker in [MALLORY(), ALICE(), LEAD()] {
            assert!(!revocation_authorised(&translated, revoker), "{revoker}");
        }
        // With more fields the same author sits under each, in the plain form,
        // inside an `or` arm, and inside arms that inherit the level's author;
        // the reader still accepts admin alone there.
        for rule in [
            json!({ "className": "ns://Reviewer", "didProperty": "agent",
                    "where": { "forTask": "$flow.base", "author": ADMIN() } }),
            json!({ "className": "ns://Reviewer", "didProperty": "agent",
                    "or": [ { "className": "ns://Reviewer", "where": { "rank": "lead", "author": ADMIN() } } ] }),
            json!({ "className": "ns://Reviewer", "didProperty": "agent", "where": { "author": ADMIN() },
                    "or": [ { "className": "ns://Reviewer", "where": { "rank": "lead" } },
                            { "className": "ns://Reviewer", "where": { "rank": "senior" } } ] }),
        ] {
            let translated = requires_query_input(
                &serde_json::from_value::<ModelQuery>(rule.clone()).unwrap(),
                &record(),
                ALICE(),
            )
            .unwrap();
            assert!(revocation_authorised(&translated, ADMIN()), "{rule}");
            for revoker in [MALLORY(), ALICE(), LEAD()] {
                assert!(
                    !revocation_authorised(&translated, revoker),
                    "{revoker}: {rule}"
                );
            }
        }
        // Every author condition at a level must accept, whichever key holds it.
        let both = json!({ "where": { "agent": { "eq": ALICE(), "author": [ADMIN(), LEAD()] }, "author": LEAD() } });
        assert!(revocation_authorised(&both, LEAD()));
        assert!(!revocation_authorised(&both, ADMIN()));
    }

    /// A level with fields and no `author`, beside `or` arms that name granters
    /// without collapsing, is refused at translation, so it grants nobody and
    /// has no revocation rule to read: resolving the role fails for grant and
    /// revocation alike. The same rule with the fields written into each arm
    /// accepts a revocation from each arm's granter and nobody else, as it
    /// accepts a grant.
    #[tokio::test]
    async fn a_refused_level_grants_and_revokes_nothing_and_its_distributed_form_is_symmetric() {
        let refused = role(
            json!({ "className": "ns://Reviewer", "didProperty": "agent",
            "where": { "forTask": "$flow.base" },
            "or": [ { "className": "ns://Reviewer", "where": { "author": ADMIN() } },
                    { "className": "ns://Reviewer", "where": { "author": LEAD(), "rank": "senior" } } ] }),
        );
        assert!(requires_query_input(&refused, &record(), ALICE()).is_err());
        let mut stub = members(&[ALICE()]);
        stub.histories
            .insert(ALICE().into(), history(ALICE(), Some(T1), &[(ADMIN(), T2)]));
        assert!(
            resolve_role_grants(&stub, "approved", &refused, &record(), &dids(&[ALICE()]))
                .await
                .is_err(),
            "no grant evidence, and so no revocation, for a refused rule"
        );

        let distributed = role(
            json!({ "className": "ns://Reviewer", "didProperty": "agent",
            "or": [ { "className": "ns://Reviewer", "where": { "author": ADMIN(), "forTask": "$flow.base" } },
                    { "className": "ns://Reviewer",
                      "where": { "author": LEAD(), "rank": "senior", "forTask": "$flow.base" } } ] }),
        );
        let translated = requires_query_input(&distributed, &record(), ALICE()).unwrap();
        for revoker in [ADMIN(), LEAD()] {
            assert!(revocation_authorised(&translated, revoker), "{revoker}");
        }
        for revoker in [MALLORY(), ALICE()] {
            assert!(!revocation_authorised(&translated, revoker), "{revoker}");
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
        for id in ["r0", "r1"] {
            stub.histories.insert(
                id.into(),
                history(ALICE(), Some(T1), &[(MALLORY(), T3), (ADMIN(), T2)]),
            );
        }
        // Bob's instances are the same two, and every link on them is about
        // Alice. None of it speaks for Bob, so he has no `didProperty` link the
        // store could date: his windows date from the instances themselves
        // (T0), and nothing revoked them.
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

    /// Two instances of one role, each with its own history: Alice's grant
    /// on `r0` was revoked at T2, and she was granted again on `r1` at T3.
    /// Each window is dated and closed by its own instance's links, and the
    /// live one keeps her eligible.
    ///
    /// The stub could not express this before #1129's review. It handed every
    /// matched instance the same `__links`, so both windows came out revoked.
    #[tokio::test]
    async fn each_instance_is_resolved_from_its_own_history() {
        let mut stub = RoleStub {
            rows_per_match: 2,
            ..members(&[ALICE()])
        };
        stub.histories
            .insert("r0".into(), history(ALICE(), Some(T1), &[(ADMIN(), T2)]));
        stub.histories
            .insert("r1".into(), history(ALICE(), Some(T3), &[]));
        let role = role(json!({ "className": "ns://Reviewer", "didProperty": "agent" }));
        let evidence = resolve_role_grants(&stub, "approved", &role, &record(), &dids(&[ALICE()]))
            .await
            .unwrap();
        let carried: Vec<(&str, usize, usize)> = evidence[0]
            .instances
            .iter()
            .map(|i| {
                (
                    i.instance_id.as_str(),
                    i.grant_links.len(),
                    i.revocation_links.len(),
                )
            })
            .collect();
        assert_eq!(
            carried,
            vec![("r0", 1, 1), ("r1", 1, 0)],
            "each instance carries its own links"
        );

        let grants = views(&evidence, &role);
        let mut windows: Vec<(&str, Option<&str>)> = grants[0]
            .windows
            .iter()
            .map(|w| (w.granted_at.as_str(), w.revoked_at()))
            .collect();
        windows.sort();
        assert_eq!(windows, vec![(T1, Some(T2)), (T3, None)]);
        assert!(
            grants[0].eligible_at(NOW, None),
            "the live grant on r1 keeps her a member"
        );
    }

    /// The stub is total over `links` keys: a key it has no links for
    /// answers `[]`, as the real store does for a predicate nobody wrote.
    /// Before #1129's review, any key that was not the tombstone predicate got
    /// grant links, so a misspelled key in `query_keys` passed every test.
    #[tokio::test]
    async fn the_stub_answers_an_unknown_links_key_with_nothing() {
        let mut stub = members(&[ALICE()]);
        stub.histories
            .insert("r0".into(), history(ALICE(), Some(T1), &[(ADMIN(), T2)]));
        let tomb = crate::perspectives::flow_instance::atom::ROLE_GRANT_REVOKED_PREDICATE;
        let query =
            json!({ "where": { "agent": ALICE() }, "links": [STUB_GRANT_KEY, tomb, "agnet"] });
        let raw = stub
            .model_query("ns://Reviewer", &query.to_string())
            .await
            .unwrap();
        let links = &serde_json::from_str::<Value>(&raw).unwrap()["instances"][0]["__links"];
        assert_eq!(links[STUB_GRANT_KEY].as_array().map(Vec::len), Some(1));
        assert_eq!(links[tomb].as_array().map(Vec::len), Some(1));
        assert_eq!(links["agnet"], json!([]), "a key nobody wrote has no links");
    }

    /// What `resolve_role_grants` carries for Alice's one `agent` role
    /// instance when the store's `__links` rows are `history`.
    async fn carried_for_alice(history: RoleGrantLinks) -> RoleInstanceHistory {
        let mut stub = members(&[ALICE()]);
        stub.histories.insert("r0".into(), history);
        let role = role(json!({ "className": "ns://Reviewer", "didProperty": "agent" }));
        let mut evidence =
            resolve_role_grants(&stub, "approved", &role, &record(), &dids(&[ALICE()]))
                .await
                .expect("resolve_role_grants");
        evidence.remove(0).instances.remove(0)
    }

    /// The collection-side filters run on the `__links` rows (#1103), and
    /// what they drop never reaches the read-set.
    ///
    /// These assert on the **carried** evidence, not on a verdict: `resolve`
    /// re-applies the target and signature filters itself, so a verdict test
    /// passes whether or not collection filtered. Before #1103 the same
    /// filters sat in the raw `get_links` impl, which no test reached.
    #[tokio::test]
    async fn collection_carries_only_links_that_speak_for_the_candidate() {
        let genuine = grant_link(ALICE(), T1);
        let mut undatable = grant_link(ALICE(), T0);
        undatable.timestamp = "not a time".into();
        let about_bob = grant_link(BOB(), T0);
        let forged_tombstone = role_link(
            crate::perspectives::flow_instance::atom::ROLE_GRANT_REVOKED_PREDICATE,
            ALICE(),
            ADMIN(),
            false,
            T2,
        );
        let signed_tombstone = tombstone(ALICE(), ADMIN(), T3);
        let tombstone_for_bob = tombstone(BOB(), ADMIN(), T2);

        let carried = carried_for_alice(RoleGrantLinks {
            grant_links: vec![undatable, about_bob, genuine.clone()],
            revocation_links: vec![
                forged_tombstone,
                tombstone_for_bob,
                signed_tombstone.clone(),
            ],
        })
        .await;
        assert_eq!(
            carried.grant_links,
            vec![genuine],
            "a grant link with no parseable timestamp can date nothing, and one naming Bob \
             says nothing about Alice: neither travels"
        );
        assert_eq!(
            carried.revocation_links,
            vec![signed_tombstone],
            "a tombstone whose signature fails, or that names Bob, does not travel"
        );
    }

    /// At most [`MAX_GRANT_LINKS`] grant links travel, the earliest — and a
    /// link whose signature verifies claims a slot before one that does not,
    /// so forged early links cannot evict the genuine assignment. Evicted,
    /// the reader would fall back to the instance's own, earlier timestamp
    /// once #1063 drops unverified grant links: fail-open.
    #[tokio::test]
    async fn the_grant_link_cap_keeps_the_earliest_and_prefers_verified_links() {
        use crate::perspectives::flow_evaluator::MAX_GRANT_LINKS;

        let early = |i: usize| format!("2025-12-31T00:00:{i:02}.000Z");
        let genuine = grant_link(ALICE(), T1);
        let mut grant_links: Vec<LinkExpression> = (0..MAX_GRANT_LINKS)
            .map(|i| role_link("agent", ALICE(), ADMIN(), false, &early(i)))
            .collect();
        grant_links.push(genuine.clone());
        let carried = carried_for_alice(RoleGrantLinks {
            grant_links,
            revocation_links: Vec::new(),
        })
        .await;
        assert_eq!(carried.grant_links.len(), MAX_GRANT_LINKS, "capped");
        assert!(
            carried.grant_links.contains(&genuine),
            "{MAX_GRANT_LINKS} forged earlier links must not evict the signed assignment"
        );

        let signed: Vec<LinkExpression> = (0..=MAX_GRANT_LINKS)
            .map(|i| grant_link(ALICE(), &early(i)))
            .collect();
        let carried = carried_for_alice(RoleGrantLinks {
            grant_links: signed.clone(),
            revocation_links: Vec::new(),
        })
        .await;
        assert_eq!(
            carried.grant_links,
            signed[..MAX_GRANT_LINKS].to_vec(),
            "all verified: the earliest {MAX_GRANT_LINKS}, earliest first"
        );
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
                produced_at: None,
            }],
        }
        .resolve(&query, &role)
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
                produced_at: None,
            }],
        }
        .resolve(&translated(&role, ALICE()), &role)
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
}
