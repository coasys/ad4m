//! Fixtures for the scope index tests: signed links at a stated millisecond,
//! a forged link, a flagged shape, and a view of the index that two stores
//! can be compared by.

use chrono::{TimeZone, Utc};
use std::collections::BTreeMap;

use super::{MemberKey, COUNT, ENTRY, FLAG, MEMBER};
use crate::agent::signatures::TestSigner;
use crate::perspectives::model_query::shape::parse_shape_from_json;
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{Link, LinkExpression, LinkStatus};

/// The flag predicate of the test classes.
pub const TYPE: &str = "ad4m://type";
/// The flag value of class `Message`.
pub const MESSAGE: &str = "si://Message";
/// The relation from a channel to its messages.
pub const IN_CHANNEL: &str = "si://message";
/// A time in 2026, in milliseconds, that test times count from.
pub const T0: u64 = 1_790_000_000_000;

/// `Message`: flag `ad4m://type = si://Message`, a body, and nothing else
/// required.
pub fn message_shape() -> ModelShape {
    parse_shape_from_json(
        &format!(
            r#"{{
    "className": "Message",
    "properties": {{
        "type": {{"predicate":"{TYPE}","required":true,"flag":true,"initial":"{MESSAGE}"}},
        "body": {{"predicate":"si://body","required":false}}
    }},
    "relations": {{}}
}}"#
        ),
        "Message",
    )
    .expect("the test shape parses")
}

/// A link `source predicate target` signed by `signer` at `ms` (milliseconds
/// since the epoch), stored as `Shared`.
pub fn signed(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    ms: u64,
) -> LinkExpression {
    let at = Utc
        .timestamp_millis_opt(ms as i64)
        .single()
        .expect("a valid test time");
    let mut l = LinkExpression::from(signer.sign_at(
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        },
        at,
    ));
    l.status = Some(LinkStatus::Shared);
    l
}

/// A link that names `signer` as its author but whose signature covers
/// another target, so it does not verify.
pub fn forged(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    ms: u64,
) -> LinkExpression {
    let mut l = signed(signer, source, predicate, "literal:string:signed", ms);
    l.data.target = target.to_string();
    assert!(
        !l.compute_proof_valid(),
        "the forged fixture must not verify"
    );
    l
}

/// The flag link of message `id` at `ms`.
pub fn flag_link(signer: &TestSigner, id: &str, ms: u64) -> LinkExpression {
    signed(signer, id, TYPE, MESSAGE, ms)
}

/// The link from `channel` to message `id` at `ms`.
pub fn channel_link(signer: &TestSigner, channel: &str, id: &str, ms: u64) -> LinkExpression {
    signed(signer, channel, IN_CHANNEL, id, ms)
}

/// The index of a store, without the parts that depend on the order work was
/// done in (the `seq` of each key, and counters at 0), so a store built one
/// way can be compared with a store built another way.
#[derive(Debug, PartialEq, Eq)]
pub struct IndexView {
    /// Per scope: `(t_ms, member)`, sorted.
    pub entries: BTreeMap<Vec<u8>, Vec<(u64, String)>>,
    /// `(member hash, scope)` → `(t_ms, class)`.
    pub members: BTreeMap<Vec<u8>, (u64, Vec<u8>)>,
    /// `(member hash, class)` → `(t_ms, noncanonical)`.
    pub flags: BTreeMap<Vec<u8>, (u64, bool)>,
    /// Counters other than 0.
    pub counts: BTreeMap<Vec<u8>, i64>,
}

pub fn index_view(store: &SparqlStore) -> IndexView {
    let raw = store.raw_store();
    let mut view = IndexView {
        entries: BTreeMap::new(),
        members: BTreeMap::new(),
        flags: BTreeMap::new(),
        counts: BTreeMap::new(),
    };
    for item in raw.aux_scan(&[], &[], false) {
        let (key, value) = item.expect("aux scan");
        let key_t_ms = |bytes: &[u8]| MemberKey::from_bytes(&bytes[..8]).unwrap().t_ms();
        match key[0] {
            ENTRY => {
                let t_ms = MemberKey::from_bytes(&key[17..25]).unwrap().t_ms();
                view.entries
                    .entry(key[1..17].to_vec())
                    .or_default()
                    .push((t_ms, String::from_utf8(value).unwrap()));
            }
            MEMBER => {
                view.members
                    .insert(key[1..].to_vec(), (key_t_ms(&value), value[8..].to_vec()));
            }
            FLAG => {
                view.flags
                    .insert(key[1..].to_vec(), (key_t_ms(&value), value[8] == 1));
            }
            COUNT => {
                let n = raw.aux_count(&key).expect("aux count");
                if n != 0 {
                    view.counts.insert(key[1..].to_vec(), n);
                }
            }
            _ => {}
        }
    }
    for list in view.entries.values_mut() {
        list.sort();
    }
    view
}

/// Every entry of the index is consistent with the rest: each `E` has its
/// `M`, each `M` its `E`, each scope's counter equals its entries, and no
/// member has two entries in one scope.
pub fn assert_consistent(store: &SparqlStore) {
    let raw = store.raw_store();
    let all: Vec<(Vec<u8>, Vec<u8>)> = raw
        .aux_scan(&[], &[], false)
        .collect::<Result<_, _>>()
        .expect("aux scan");
    let mut per_scope: BTreeMap<Vec<u8>, i64> = BTreeMap::new();
    let mut seen: std::collections::HashSet<(Vec<u8>, String)> = Default::default();
    for (key, value) in &all {
        match key[0] {
            ENTRY => {
                let scope = key[1..17].to_vec();
                *per_scope.entry(scope.clone()).or_default() += 1;
                let member = String::from_utf8(value.clone()).unwrap();
                assert!(
                    seen.insert((scope.clone(), member.clone())),
                    "{member} has two entries in one scope"
                );
                let mut mkey = vec![MEMBER];
                mkey.extend_from_slice(&super::member_hash(&member));
                mkey.extend_from_slice(&scope);
                let m = raw
                    .aux_get(&mkey)
                    .unwrap()
                    .expect("an entry has its member key");
                assert_eq!(
                    &m[..8],
                    &key[17..25],
                    "the member key names the entry's key"
                );
            }
            MEMBER => {
                let mut ekey = vec![ENTRY];
                ekey.extend_from_slice(&key[17..33]);
                ekey.extend_from_slice(&value[..8]);
                assert!(
                    raw.aux_get(&ekey).unwrap().is_some(),
                    "a member key has its entry"
                );
            }
            _ => {}
        }
    }
    for (scope, n) in per_scope {
        let mut ckey = vec![COUNT];
        ckey.extend_from_slice(&scope);
        assert_eq!(
            raw.aux_count(&ckey).unwrap(),
            n,
            "a scope's count equals its entries"
        );
    }
}
