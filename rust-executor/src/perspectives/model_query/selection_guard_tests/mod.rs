//! #1120: a link that is withheld from hydration must not select an instance
//! either.
//!
//! #1113 withholds unverified links from the rows that hydrate an instance, and
//! #1116's `linkStatus` withholds links of the other status. The patterns that
//! decide *which* instances match (pushed `where`, the class's flags, `COUNT` /
//! `totalCount`, the two-phase plan's id phase and order keys, `$` projections)
//! must read the same links. Otherwise a forged `agent` link with an admin as
//! its claimed author satisfies `{ agent: { eq: X, author: admin } }`, which is
//! the role check #1103 and #1063 build on.
//!
//! Every fixture signs with real [`TestSigner`] keys. A forged link claims a
//! signer as its author and carries that signer's signature over a different
//! target, which is what a peer without the key can gossip. Each test also runs
//! the query with `includeUnverified` (or without `linkStatus`) and asserts the
//! old answer, so none of them can pass because the withheld link never
//! mattered.
//!
//! | File | Covers |
//! |---|---|
//! | `where_and_count.rs` | `where` (per-link and side-by-side `author`, membership, quantifiers, `NOT`), conformance, `count` / `totalCount`, the page, scopes, a projection's `where`, a typed relation's target flags, deduplication |
//! | `link_status.rs` | the same under `linkStatus`, and one reifier for status and verdict |
//! | `walks.rs` | transitive projections and scopes, walked one guarded step at a time; the guard's cost |
//! | `viewer.rs` | another agent's `Local` link, for a viewer (#1024): with the verdict on one reifier, and under `linkStatus` |

mod link_status;
mod viewer;
mod walks;
mod where_and_count;

use super::test_helpers::execute_model_query_from_json;
use super::types::{ModelQueryInput, ModelQueryResult};
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{Link, LinkExpression, LinkStatus};
use serde_json::{json, Value};

pub(super) const SG_SHAPE_JSON: &str = r#"{
    "className": "Grant",
    "properties": {
        "type": {"predicate":"ad4m://type","required":true,"flag":true,"initial":"sg://Grant"},
        "agent": {"predicate":"sg://agent","required":false},
        "name": {"predicate":"sg://name","required":false}
    },
    "relations": {
        "members": { "predicate": "sg://member", "kind": "hasMany", "targetClassName": "" },
        "replies": { "predicate": "sg://reply", "kind": "hasMany", "targetClassName": "" }
    }
}"#;

pub(super) fn sg_at(second: u32) -> chrono::DateTime<chrono::Utc> {
    use chrono::TimeZone;
    chrono::Utc
        .with_ymd_and_hms(2026, 9, 25, 12, 0, second)
        .unwrap()
}

pub(super) fn sg_link(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    second: u32,
    status: LinkStatus,
) -> LinkExpression {
    let mut l = LinkExpression::from(signer.sign_at(
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        },
        sg_at(second),
    ));
    l.status = Some(status);
    l
}

/// A Shared link signed by `signer`.
pub(super) fn signed(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    second: u32,
) -> LinkExpression {
    sg_link(
        signer,
        source,
        predicate,
        target,
        second,
        LinkStatus::Shared,
    )
}

/// A Shared link claiming `signer` as its author, carrying the signer's
/// signature over another target.
pub(super) fn forged(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    second: u32,
) -> LinkExpression {
    let mut l = signed(
        signer,
        source,
        predicate,
        "literal:string:what-was-signed",
        second,
    );
    l.data.target = target.to_string();
    assert!(!l.compute_proof_valid(), "the fixture must not verify");
    l
}

pub(super) fn add(store: &SparqlStore, links: impl IntoIterator<Item = LinkExpression>) {
    for l in links {
        store.add_link(&l).unwrap();
    }
}

pub(super) async fn run(store: &SparqlStore, query: Value) -> ModelQueryResult {
    let query: ModelQueryInput = serde_json::from_value(query.clone())
        .unwrap_or_else(|e| panic!("query {query} does not parse: {e}"));
    execute_model_query_from_json(store, "Grant", &query, SG_SHAPE_JSON)
        .await
        .unwrap()
}

pub(super) fn ids(result: &ModelQueryResult) -> Vec<String> {
    result
        .instances
        .iter()
        .map(|i| i["id"].as_str().unwrap().to_string())
        .collect()
}

/// `query` with `includeUnverified: true` added.
pub(super) fn opted_in(mut query: Value) -> Value {
    query["includeUnverified"] = json!(true);
    query
}
