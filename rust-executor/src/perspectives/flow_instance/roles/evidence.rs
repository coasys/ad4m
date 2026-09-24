//! The carried role evidence — grant links and tombstones — and how a
//! window is derived from it by whoever reads it.

use super::{RoleGrant, RoleGrantWindow, RoleRevocation};
use crate::perspectives::flow_evaluator::{
    did_literal_url, grant_link_names_did, revocation_link_counts_for_did, EvidenceItem,
};
use crate::perspectives::flow_instance::time::parse_link_timestamp;
use crate::perspectives::model_query::{matches_condition, WhereCondition};
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
}
