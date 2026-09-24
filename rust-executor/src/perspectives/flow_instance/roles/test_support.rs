//! Fixtures shared by the roles tests: signed personas, links, histories
//! and the query-aware store stub.

use super::{RoleGrant, RoleGrantEvidence, RoleRevocation};
use crate::agent::signatures::TestSigner;
use crate::perspectives::flow_context::FlowInstanceRecord;
use crate::perspectives::flow_evaluator::{
    requires_query_input, RequiresQueryable, RoleGrantLinks,
};
use crate::perspectives::shacl_parser::{ModelQuery, ModelQueryCount};
use crate::types::LinkExpression;
use async_trait::async_trait;
use serde_json::{json, Value};
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
pub(super) fn persona(name: &str) -> &'static TestSigner {
    static SIGNERS: LazyLock<Mutex<HashMap<String, &'static TestSigner>>> =
        LazyLock::new(|| Mutex::new(HashMap::new()));
    *SIGNERS
        .lock()
        .expect("persona registry")
        .entry(name.to_string())
        .or_insert_with(|| Box::leak(Box::new(TestSigner::generate())))
}

/// The signer behind a DID one of the fixtures produced, for re-signing.
pub(super) fn persona_for_did(did: &str) -> Option<&'static TestSigner> {
    SIGNER_NAMES
        .iter()
        .map(|n| persona(n))
        .find(|s| s.did == did)
}

pub(super) const SIGNER_NAMES: [&str; 5] = ["alice", "bob", "admin", "lead", "mallory"];

#[allow(non_snake_case)]
pub(super) fn ALICE() -> &'static str {
    &persona("alice").did
}
#[allow(non_snake_case)]
pub(super) fn BOB() -> &'static str {
    &persona("bob").did
}
#[allow(non_snake_case)]
pub(super) fn ADMIN() -> &'static str {
    &persona("admin").did
}
#[allow(non_snake_case)]
pub(super) fn LEAD() -> &'static str {
    &persona("lead").did
}
#[allow(non_snake_case)]
pub(super) fn MALLORY() -> &'static str {
    &persona("mallory").did
}
pub(super) const T0: &str = "2026-01-01T00:00:00.000Z";
pub(super) const T1: &str = "2026-01-02T00:00:00.000Z";
pub(super) const T2: &str = "2026-01-03T00:00:00.000Z";
pub(super) const T3: &str = "2026-01-04T00:00:00.000Z";
pub(super) const T4: &str = "2026-01-05T00:00:00.000Z";
/// Later than any grant or tombstone a test writes: "eligible now".
pub(super) const NOW: &str = "2030-01-01T00:00:00.000Z";

pub(super) fn record() -> FlowInstanceRecord {
    FlowInstanceRecord {
        flow_uri: "delivery://DeliveryFlow".into(),
        instance_uri: "ad4m://flow/instance/1".into(),
        subject: "ad4m://task/onboarding".into(),
        current_state: "review".into(),
        created_at: None,
    }
}

pub(super) fn role(v: Value) -> ModelQuery {
    serde_json::from_value(v).expect("role query deserializes")
}

pub(super) fn dids(names: &[&str]) -> Vec<String> {
    names.iter().map(|s| s.to_string()).collect()
}

pub(super) fn revocation(by: &str, at: &str) -> RoleRevocation {
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
pub(super) fn role_link(
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
pub(super) fn grant_link(did: &str, at: &str) -> LinkExpression {
    role_link("agent", did, ADMIN(), true, at)
}

/// A signed tombstone by `by` at `at`.
pub(super) fn tombstone(did: &str, by: &str, at: &str) -> LinkExpression {
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
pub(super) fn history(
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
pub(super) fn translated(role: &ModelQuery, did: &str) -> Value {
    requires_query_input(role, &record(), did).expect("role query translates")
}

/// Resolve every candidate's evidence into the view the gate consumes.
pub(super) fn views(evidence: &[RoleGrantEvidence], role: &ModelQuery) -> Vec<RoleGrant> {
    evidence
        .iter()
        .map(|e| {
            e.resolve(&translated(role, &e.did))
                .unwrap_or_else(|err| panic!("evidence for {} resolves: {err:#}", e.did))
        })
        .collect()
}

/// The DIDs whose grants satisfy `count` at [`NOW`].
pub(super) fn eligible_now<'g>(
    grants: &'g [RoleGrant],
    count: Option<&ModelQueryCount>,
) -> Vec<&'g str> {
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
pub(super) struct RoleStub {
    pub(super) member_dids: Vec<String>,
    pub(super) rows_per_match: usize,
    pub(super) unconditional_instances: Option<usize>,
    pub(super) undated_instances: bool,
    pub(super) error: Option<String>,
    pub(super) calls: Mutex<Vec<String>>,
    pub(super) histories: HashMap<String, RoleGrantLinks>,
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

pub(super) fn members(member_dids: &[&str]) -> RoleStub {
    RoleStub {
        member_dids: dids(member_dids),
        rows_per_match: 1,
        ..Default::default()
    }
}
