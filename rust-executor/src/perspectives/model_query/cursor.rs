//! Keyset cursors for `model_query` (`after` in, `nextCursor` out).
//!
//! Opt-in: a query that sends no `after` gets no `nextCursor` and runs
//! exactly as before. `after: ""` asks for the first page in cursor mode;
//! each reply's `nextCursor` (present only while a full page came back)
//! continues from the last row of that page.
//!
//! Supported for the timestamp order only: no `order`, or a single
//! `timestamp` / `createdAt` / `updatedAt` key, either direction, with a
//! `where` the store can evaluate and no per-anchor limit or walk. The page is
//! then selected in SPARQL by `(first timestamp, id)` — rows inserted before
//! the cursor position cannot shift later pages, unlike `offset`. Any other
//! query with a non-empty `after` is refused; with `after: ""` it runs as a
//! plain query and returns no `nextCursor`.

use base64::Engine;
use deno_core::anyhow::{anyhow, Error};
use serde::{Deserialize, Serialize};

use super::types::OrderDirection;

/// Position after the last row of a page: its first-link timestamp and id,
/// plus the direction the page was read in.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(super) struct KeysetCursor {
    pub(super) ts: String,
    pub(super) id: String,
    pub(super) desc: bool,
}

const ENGINE: base64::engine::GeneralPurpose = base64::engine::general_purpose::URL_SAFE_NO_PAD;

impl KeysetCursor {
    pub(super) fn new(ts: String, id: String, direction: OrderDirection) -> Self {
        Self {
            ts,
            id,
            desc: matches!(direction, OrderDirection::DESC),
        }
    }

    pub(super) fn encode(&self) -> String {
        ENGINE.encode(serde_json::to_vec(self).expect("cursor serializes"))
    }

    pub(super) fn decode(s: &str) -> Result<Self, Error> {
        let bytes = ENGINE
            .decode(s)
            .map_err(|_| anyhow!("Invalid `after` cursor"))?;
        serde_json::from_slice(&bytes).map_err(|_| anyhow!("Invalid `after` cursor"))
    }

    pub(super) fn direction(&self) -> OrderDirection {
        if self.desc {
            OrderDirection::DESC
        } else {
            OrderDirection::ASC
        }
    }
}

/// Is `order` one the cursor supports (the timestamp order)?
pub(super) fn is_timestamp_order(order: &Option<Vec<(String, OrderDirection)>>) -> bool {
    match order {
        None => true,
        Some(keys) => {
            keys.len() == 1
                && matches!(
                    keys[0].0.as_str(),
                    "timestamp" | "createdAt" | "updatedAt"
                )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips_and_keeps_direction() {
        let c = KeysetCursor::new(
            "2026-01-01T00:00:00.000Z".into(),
            "ns://a\"b".into(),
            OrderDirection::DESC,
        );
        let back = KeysetCursor::decode(&c.encode()).unwrap();
        assert_eq!(back, c);
        assert!(matches!(back.direction(), OrderDirection::DESC));
    }

    #[test]
    fn rejects_garbage() {
        assert!(KeysetCursor::decode("not a cursor!").is_err());
        assert!(KeysetCursor::decode(&ENGINE.encode(b"{}")).is_err());
    }

    #[test]
    fn timestamp_order_detection() {
        assert!(is_timestamp_order(&None));
        assert!(is_timestamp_order(&Some(vec![(
            "createdAt".into(),
            OrderDirection::DESC
        )])));
        assert!(!is_timestamp_order(&Some(vec![(
            "title".into(),
            OrderDirection::ASC
        )])));
        assert!(!is_timestamp_order(&Some(vec![
            ("timestamp".into(), OrderDirection::ASC),
            ("title".into(), OrderDirection::ASC)
        ])));
    }
}

/// End to end through `execute_model_query` on a real store.
#[cfg(test)]
mod pipeline_tests {
    use super::super::query::execute_model_query;
    use super::super::test_helpers::StaticShapeResolver;
    use super::super::types::{ModelQueryInput, ModelQueryResult, ModelShape};
    use super::super::utils::literal_percent_encode;
    use crate::perspectives::sparql_store::SparqlStore;
    use crate::types::{ExpressionProof, Link, LinkExpression, LinkStatus};
    use serde_json::{json, Value};

    const SHAPE_JSON: &str = r#"{
        "className": "Todo",
        "properties": {
            "kind": { "predicate": "ns://kind", "required": true, "flag": true, "initial": "ns://todo" },
            "title": { "predicate": "ns://title", "required": false, "resolveLanguage": "literal" }
        }
    }"#;

    fn link(source: &str, predicate: &str, target: &str, ts: &str) -> LinkExpression {
        LinkExpression {
            author: "did:key:zA".into(),
            timestamp: ts.into(),
            data: Link {
                source: source.into(),
                predicate: Some(predicate.into()),
                target: target.into(),
            },
            proof: ExpressionProof {
                key: "key".into(),
                signature: "sig".into(),
            },
            status: Some(LinkStatus::Shared),
        }
    }

    fn add(store: &SparqlStore, id: &str, second: u32) {
        let ts = format!("2026-01-01T00:00:{second:02}.000Z");
        store.add_link(&link(id, "ns://kind", "ns://todo", &ts)).unwrap();
        let title = format!("literal:string:{}", literal_percent_encode(id));
        store.add_link(&link(id, "ns://title", &title, &ts)).unwrap();
    }

    /// Ten todos; `ns://t03` and `ns://t04` share a timestamp (a tie the
    /// cursor must break by id).
    fn store() -> SparqlStore {
        let store = SparqlStore::new(None).unwrap();
        for (i, second) in [(0, 10), (1, 11), (2, 12), (3, 13), (4, 13), (5, 15), (6, 16), (7, 17), (8, 18), (9, 19)] {
            add(&store, &format!("ns://t{i:02}"), second);
        }
        store
    }

    async fn run(store: &SparqlStore, query: Value) -> Result<ModelQueryResult, deno_core::anyhow::Error> {
        let (resolver, shape) = StaticShapeResolver::from_json("Todo", SHAPE_JSON).unwrap();
        let shape: ModelShape = (*shape).clone();
        let mut input: ModelQueryInput = serde_json::from_value(query).unwrap();
        input.include_unverified = Some(true);
        execute_model_query(store, &shape, &input, &resolver).await
    }

    fn ids(r: &ModelQueryResult) -> Vec<String> {
        r.instances
            .iter()
            .map(|i| i["id"].as_str().unwrap().to_string())
            .collect()
    }

    /// Walk every page; returns the concatenated ids and the page count.
    async fn walk(store: &SparqlStore, mut query: Value, limit: usize) -> (Vec<String>, usize) {
        let mut all = vec![];
        let mut after = String::new();
        for pages in 1..50 {
            query["after"] = json!(after);
            query["limit"] = json!(limit);
            let r = run(store, query.clone()).await.unwrap();
            assert!(r.instances.len() <= limit);
            all.extend(ids(&r));
            match r.next_cursor {
                Some(c) => after = c,
                None => return (all, pages),
            }
        }
        panic!("cursor never ended");
    }

    #[tokio::test]
    async fn cursor_pages_cover_every_row_once_in_order() {
        let store = store();
        let full = ids(&run(&store, json!({ "order": { "timestamp": "ASC" } })).await.unwrap());
        assert_eq!(full.len(), 10);
        let (paged, pages) = walk(&store, json!({}), 3).await;
        assert_eq!(paged.len(), 10, "no row repeated or skipped: {paged:?}");
        let mut sorted = paged.clone();
        sorted.sort();
        sorted.dedup();
        assert_eq!(sorted.len(), 10);
        assert_eq!(pages, 4, "3+3+3+1");
        // Same order as the plain timestamp order, apart from the tie.
        assert_eq!(paged[..3], full[..3]);
        assert_eq!(paged[5..], full[5..]);
    }

    #[tokio::test]
    async fn cursor_pages_descending() {
        let store = store();
        let (paged, _) = walk(&store, json!({ "order": { "createdAt": "DESC" } }), 4).await;
        assert_eq!(paged.len(), 10);
        assert_eq!(paged[0], "ns://t09");
        assert_eq!(paged[9], "ns://t00");
    }

    #[tokio::test]
    async fn exact_multiple_ends_with_an_empty_page() {
        let store = store();
        let (paged, pages) = walk(&store, json!({}), 5).await;
        assert_eq!(paged.len(), 10);
        assert_eq!(pages, 3, "5 + 5 + empty");
    }

    #[tokio::test]
    async fn an_insert_before_the_cursor_does_not_shift_the_next_page() {
        let store = store();
        let first = run(&store, json!({ "after": "", "limit": 3 })).await.unwrap();
        assert_eq!(ids(&first), vec!["ns://t00", "ns://t01", "ns://t02"]);
        // A row that sorts before the cursor arrives between the two reads.
        add(&store, "ns://early", 1);
        let second = run(
            &store,
            json!({ "after": first.next_cursor.unwrap(), "limit": 3 }),
        )
        .await
        .unwrap();
        assert_eq!(ids(&second)[0], "ns://t03", "offset 3 would now repeat ns://t02");
        assert_eq!(second.total_count, 11);
    }

    #[tokio::test]
    async fn without_after_the_reply_is_unchanged() {
        let store = store();
        let r = run(&store, json!({ "limit": 3 })).await.unwrap();
        assert!(r.next_cursor.is_none());
        let wire = serde_json::to_value(&r).unwrap();
        assert!(wire.get("nextCursor").is_none(), "no new key: {wire}");
        assert_eq!(
            wire.as_object().unwrap().keys().collect::<Vec<_>>(),
            vec!["instances", "totalCount"]
        );
    }

    #[tokio::test]
    async fn unsupported_queries_refuse_a_cursor_but_accept_an_empty_after() {
        let store = store();
        let by_title = json!({ "order": { "title": "ASC" }, "limit": 3 });
        let mut q = by_title.clone();
        q["after"] = json!("");
        let r = run(&store, q).await.unwrap();
        assert!(r.next_cursor.is_none(), "unsupported order: no cursor");
        assert_eq!(r.instances.len(), 3);

        let first = run(&store, json!({ "after": "", "limit": 3 })).await.unwrap();
        let cursor = first.next_cursor.unwrap();
        let mut q = by_title;
        q["after"] = json!(cursor.clone());
        assert!(run(&store, q).await.is_err());
        assert!(
            run(&store, json!({ "after": cursor.clone(), "limit": 3, "offset": 1 }))
                .await
                .is_err(),
            "after and offset do not combine"
        );
        assert!(
            run(&store, json!({ "after": cursor, "limit": 3, "order": { "timestamp": "DESC" } }))
                .await
                .is_err(),
            "cursor from the other direction"
        );
        assert!(run(&store, json!({ "after": "garbage", "limit": 3 })).await.is_err());
    }
}
