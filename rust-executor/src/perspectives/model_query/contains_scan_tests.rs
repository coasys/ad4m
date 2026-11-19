//! #1735: a `contains` on `ad4m://TextContent` text, and the same filter twice
//! under `OR`, the way `get_mention_waker_config` asks.
//!
//! A WE neighbourhood is many classes that each carry a text expression
//! (a signed literal envelope), a type flag and a parent link, plus links
//! that are not text. The waker reads them as one abstract class, so the
//! query here is the union over those classes with no per-class page: the
//! same path `modelQuery(..., "ad4m://TextContent", ...)` takes. One term
//! on a space of this size is a few seconds (#1735 measured 3.98 s and 40
//! rows). Two terms must cost about the same, not the 30 s model-query
//! timeout.

use super::test_helpers::{flag, prop, shape, StaticShapeResolver};
use super::types::{ModelQueryInput, ModelShape};
use super::union::execute_union_query_with_views;
use super::utils::literal_percent_encode;
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::{EvalStop, SparqlStore};
use crate::types::{Link, LinkExpression, LinkStatus};
use chrono::{DateTime, Utc};
use serde_json::{json, Value};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::{Duration, Instant};

/// The issue's page size.
const LIMIT: usize = 50;

/// Planted rows. Low indexes, so a space smaller than a WE neighbourhood
/// still contains them as long as each class is at least this big.
const HEX_MESSAGES: usize = 25;
const DID_COMMENTS: usize = 8;
const BOTH_BLOCKS: usize = 4;
const CASE_NOTES: usize = 3;
const SELF_HEX: usize = 6;

struct ClassSpec {
    name: &'static str,
    type_pred: &'static str,
    type_iri: &'static str,
    text_pred: &'static str,
    /// Records of this class.
    count: usize,
}

struct Space {
    store: SparqlStore,
    views: HashMap<String, Arc<ModelShape>>,
    classes: Vec<String>,
    own_did: String,
    /// Link expressions in the store, the unit #1735 counted.
    links: usize,
    /// Ids whose text contains "Hex" (any case) and was not written by us.
    hex: HashSet<String>,
    /// Ids the two-term waker query must return: `hex` plus texts that
    /// contain the DID and were not written by us.
    either: HashSet<String>,
}

fn classes_of(message: usize, comment: usize, block: usize, note: usize) -> Vec<ClassSpec> {
    vec![
        ClassSpec {
            name: "Message",
            type_pred: "we://message/type",
            type_iri: "we://Message",
            text_pred: "we://message/body",
            count: message,
        },
        ClassSpec {
            name: "Comment",
            type_pred: "we://comment/type",
            type_iri: "we://Comment",
            text_pred: "we://comment/body",
            count: comment,
        },
        ClassSpec {
            name: "Block",
            type_pred: "we://block/type",
            type_iri: "we://Block",
            text_pred: "we://block/text",
            count: block,
        },
        ClassSpec {
            name: "Note",
            type_pred: "we://note/type",
            type_iri: "we://Note",
            text_pred: "we://note/content",
            count: note,
        },
    ]
}

/// What one record's text is, and whether the waker query should see it.
struct Plant {
    extra: String,
    by_self: bool,
    /// Matches the name term ("Hex").
    hex: bool,
    /// Matches the DID term.
    did: bool,
}

fn plant(class: &str, i: usize, n: usize, own_did: &str) -> Plant {
    if class == "Message" && i < HEX_MESSAGES {
        return Plant {
            extra: "Hex".into(),
            by_self: false,
            hex: true,
            did: false,
        };
    }
    if class == "Message" && n >= HEX_MESSAGES + SELF_HEX && i >= n - SELF_HEX {
        return Plant {
            extra: "Hex".into(),
            by_self: true,
            hex: false,
            did: false,
        };
    }
    if class == "Comment" && i < DID_COMMENTS {
        return Plant {
            extra: own_did.to_string(),
            by_self: false,
            hex: false,
            did: true,
        };
    }
    if class == "Block" && i < BOTH_BLOCKS {
        return Plant {
            extra: format!("Hex mentioned {own_did}"),
            by_self: false,
            hex: true,
            did: true,
        };
    }
    if class == "Note" && i < CASE_NOTES {
        return Plant {
            extra: "See HEX in the title".into(),
            by_self: false,
            hex: true,
            did: false,
        };
    }
    Plant {
        extra: String::new(),
        by_self: false,
        hex: false,
        did: false,
    }
}

fn envelope(author: &str, ts: &str, data: &str) -> String {
    let json_str = serde_json::to_string(&json!({
        "author": author,
        "timestamp": ts,
        "data": data,
        "proof": { "key": "k", "signature": "s", "valid": true, "invalid": false }
    }))
    .unwrap();
    format!("literal:json:{}", literal_percent_encode(&json_str))
}

fn at(offset_ms: i64) -> DateTime<Utc> {
    DateTime::from_timestamp_millis(1_700_000_000_000 + offset_ms).expect("timestamp in range")
}

fn signed(
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    when: DateTime<Utc>,
) -> LinkExpression {
    let mut link = LinkExpression::from(signer.sign_at(
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        },
        when,
    ));
    link.status = Some(LinkStatus::Shared);
    link
}

fn text_view(spec: &ClassSpec) -> ModelShape {
    let mut text = prop("text", spec.text_pred);
    // A WE text property is a signed literal envelope, not a plain xsd:string.
    text.resolve_language = Some("literal".into());
    shape(
        spec.name,
        vec![flag("type", spec.type_pred, spec.type_iri), text],
    )
}

/// `message` / `comment` / `block` / `note` records, plus `noise` reaction
/// links that are not text. Every link is signed, so the default
/// `proofValid` guard keeps it, the way a real neighbourhood reads.
/// A DID whose spelling does not contain "hex", so a name-term `contains`
/// cannot accidentally match a planted DID.
fn signer_without_hex() -> TestSigner {
    loop {
        let signer = TestSigner::generate();
        if !signer.did.to_ascii_lowercase().contains("hex") {
            return signer;
        }
    }
}

fn build_space(message: usize, comment: usize, block: usize, note: usize, noise: usize) -> Space {
    let own = signer_without_hex();
    let others: Vec<TestSigner> = (0..3).map(|_| signer_without_hex()).collect();
    let specs = classes_of(message, comment, block, note);
    let store = SparqlStore::new(None).unwrap();
    let mut links = Vec::new();
    let mut hex = HashSet::new();
    let mut either = HashSet::new();

    for (ci, spec) in specs.iter().enumerate() {
        for i in 0..spec.count {
            let id = format!("we://{}/{}", spec.name.to_ascii_lowercase(), i);
            let planted = plant(spec.name, i, spec.count, &own.did);
            let signer = if planted.by_self {
                &own
            } else {
                &others[i % others.len()]
            };
            // Filler avoids the substring "hex" and the DID.
            let body = format!(
                "Weekly note {i}. The plan covers the roadmap, the review and the next milestone. {} End of note {i}.",
                planted.extra
            );
            let when = at((ci as i64) * 1_000_000 + i as i64);
            let text = envelope(&signer.did, &signer.did, &body);
            links.push(signed(signer, &id, spec.type_pred, spec.type_iri, when));
            links.push(signed(signer, &id, spec.text_pred, &text, when));
            links.push(signed(
                signer,
                "we://channel/general",
                "ad4m://has_child",
                &id,
                when,
            ));
            if planted.hex {
                hex.insert(id.clone());
            }
            if planted.hex || planted.did {
                either.insert(id);
            }
        }
    }

    let noise_signer = &others[0];
    for i in 0..noise {
        let when = at(50_000_000 + i as i64);
        links.push(signed(
            noise_signer,
            &format!("we://actor/{}", i % 64),
            "we://reacts",
            &format!("we://message/{}", i % message.max(1)),
            when,
        ));
    }

    let link_count = links.len();
    for chunk in links.chunks(2_000) {
        store.apply_diff(&[], chunk).unwrap();
    }

    let mut views = HashMap::new();
    let mut classes = Vec::new();
    for spec in &specs {
        classes.push(spec.name.to_string());
        views.insert(spec.name.to_string(), Arc::new(text_view(spec)));
    }

    Space {
        store,
        views,
        classes,
        own_did: own.did,
        links: link_count,
        hex,
        either,
    }
}

/// The waker's text query. `terms` is one name, or the name and the DID.
fn waker_query(own_did: &str, terms: &[&str]) -> ModelQueryInput {
    let not_me = json!({ "not": own_did });
    let branches: Vec<Value> = terms
        .iter()
        .map(|term| json!({ "text": { "contains": term, "author": not_me } }))
        .collect();
    let where_value = if branches.len() == 1 {
        branches.into_iter().next().unwrap()
    } else {
        json!({ "OR": branches })
    };
    serde_json::from_value(json!({
        "where": where_value,
        "order": { "timestamp": "DESC" },
        "limit": LIMIT,
        "properties": ["text"],
        "links": ["text"],
    }))
    .unwrap()
}

async fn query(space: &Space, terms: &[&str]) -> (std::time::Duration, Vec<String>) {
    let input = waker_query(&space.own_did, terms);
    let started = Instant::now();
    let result = execute_union_query_with_views(
        &EvalStop::none(),
        &space.store,
        &space.classes,
        &input,
        &StaticShapeResolver::new(),
        &space.views,
    )
    .await
    .unwrap();
    let elapsed = started.elapsed();
    let ids = result
        .instances
        .iter()
        .filter_map(|row| row["id"].as_str().map(str::to_string))
        .collect();
    (elapsed, ids)
}

fn assert_ids(got: &[String], expect: &HashSet<String>, label: &str) {
    let got_set: HashSet<&str> = got.iter().map(String::as_str).collect();
    let missing: Vec<_> = expect
        .iter()
        .filter(|id| !got_set.contains(id.as_str()))
        .cloned()
        .collect();
    let extra: Vec<_> = got_set
        .iter()
        .filter(|id| !expect.contains(**id))
        .copied()
        .collect();
    assert!(
        missing.is_empty() && extra.is_empty(),
        "{label}: missing {missing:?}, extra {extra:?}, got {} rows",
        got.len()
    );
}

/// The planted rows, on a space small enough to be about the answer and not
/// the clock. Self-authored "Hex" is absent; a case difference still matches.
#[tokio::test(flavor = "multi_thread")]
async fn ored_text_contains_matches_other_authors_only() {
    let space = build_space(40, 12, 8, 6, 30);
    assert!(space.hex.len() > 5, "planted name hits");
    assert!(space.either.len() > space.hex.len(), "the DID adds hits");

    let (_one_t, one) = query(&space, &["Hex"]).await;
    assert_ids(&one, &space.hex, "one term");

    let (_two_t, two) = query(&space, &["Hex", &space.own_did]).await;
    assert_ids(&two, &space.either, "two terms");
}

/// #1735's shape and its bound. One term on this space is the scan the
/// issue measured at about 4 s. Two ORed terms must finish in a few
/// seconds, not trip the 30 s model-query timeout, and must cost about
/// what one term costs.
#[tokio::test(flavor = "multi_thread")]
async fn ored_text_contains_on_a_large_space_stays_within_a_few_seconds() {
    // ~46k link expressions: four text-bearing classes (type, text, parent)
    // and reaction links that are not text. Same order of magnitude as the
    // 39,218-link and 74,265-link neighbourhoods in the issue.
    let space = build_space(8_000, 2_000, 1_000, 500, 12_000);
    assert_eq!(space.hex.len(), HEX_MESSAGES + BOTH_BLOCKS + CASE_NOTES);
    assert_eq!(
        space.either.len(),
        HEX_MESSAGES + DID_COMMENTS + BOTH_BLOCKS + CASE_NOTES
    );
    eprintln!(
        "#1735 space: {} links, {} name hits, {} either-term hits",
        space.links,
        space.hex.len(),
        space.either.len()
    );

    let (one, one_ids) = query(&space, &["Hex"]).await;
    eprintln!("#1735 one term {one:?} ({} rows)", one_ids.len());
    assert_ids(&one_ids, &space.hex, "one term");
    let (two, two_ids) = query(&space, &["Hex", &space.own_did]).await;
    eprintln!("#1735 two terms {two:?} ({} rows)", two_ids.len());
    assert_ids(&two_ids, &space.either, "two terms");
    eprintln!(
        "#1735 timings: one term {one:?} ({} rows), two terms {two:?} ({} rows), {} links",
        one_ids.len(),
        two_ids.len(),
        space.links
    );

    // "A few seconds", under the 30 s timeout the issue hit, and about the
    // cost of the one-term scan (the issue: 3.98 s vs a timeout).
    let bound = Duration::from_secs(12);
    assert!(
        two < bound,
        "two-term contains OR took {two:?} on {} links (one term {one:?}, {} rows); \
         #1735: one term was 3.98 s and two terms timed out at 30 s, so two terms \
         must finish within {bound:?} and cost about the same as one",
        space.links,
        one_ids.len()
    );
    assert!(
        two < one.saturating_mul(3).max(Duration::from_secs(1)),
        "two-term contains OR took {two:?}, one term {one:?}; the OR must share \
         the text scan (#1735) rather than cost several times one term"
    );
}
