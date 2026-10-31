//! An include's `limit` / `offset` page each parent's own related rows (BUG-104).
//!
//! `include: { comments: { limit: 2 } }` on a page of posts reads as "each post
//! with its two latest comments", and that is what the SDK documents. The include
//! used to run one sub-query for the whole page with the caller's `limit`, so the
//! two comments went to whichever post owned them and every other post got `[]`.

use super::shape::parse_shape_from_json;
use super::test_helpers::StaticShapeResolver;
use super::types::{IncludeValue, ModelQueryInput, OrderDirection};
use super::utils::literal_percent_encode;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{ExpressionProof, Link, LinkExpression};
use std::collections::HashMap;

fn link(source: &str, predicate: &str, target: &str, ts: u64) -> LinkExpression {
    LinkExpression {
        author: "did:key:test123".to_string(),
        timestamp: ts.to_string(),
        data: Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        },
        proof: ExpressionProof {
            key: "key".to_string(),
            signature: "sig".to_string(),
        },
        status: Some(crate::types::LinkStatus::Shared),
    }
}

const POST_SHAPE: &str = r#"{
    "className": "Post",
    "properties": {
        "flag": { "predicate": "we://flag", "required": true, "flag": true, "initial": "we://post" }
    },
    "relations": {
        "comments": { "predicate": "we://comment", "kind": "hasMany", "targetClassName": "Comment" },
        "replies": {
            "predicate": "we://reply_to",
            "kind": "belongsToMany",
            "targetClassName": "Comment",
            "direction": "reverse"
        }
    }
}"#;

const COMMENT_SHAPE: &str = r#"{
    "className": "Comment",
    "properties": {
        "cflag": { "predicate": "we://cflag", "required": true, "flag": true, "initial": "we://comment" },
        "body": { "predicate": "we://body", "resolveLanguage": "literal" }
    },
    "relations": {}
}"#;

/// Two posts, three comments each. A post's comments are created in order, so
/// `c/<post>/3` is its latest. Every comment is linked to its post both ways:
/// `post -comment-> c` (forward `comments`) and `c -reply_to-> post` (reverse
/// `replies`). Bodies run against creation order, so `body DESC` picks the
/// earliest comments first.
fn store_with_two_posts() -> SparqlStore {
    let store = SparqlStore::new(None).unwrap();
    for (p, post) in ["we://post/1", "we://post/2"].iter().enumerate() {
        let base = 1_700_000_000_000 + (p as u64) * 100;
        store
            .add_link(&link(post, "we://flag", "we://post", base))
            .unwrap();
        for i in 1..=3u64 {
            let c = comment(p + 1, i);
            let ts = base + 10 + i;
            store
                .add_link(&link(&c, "we://cflag", "we://comment", ts))
                .unwrap();
            let body = format!("body-{}", 4 - i);
            store
                .add_link(&link(
                    &c,
                    "we://body",
                    &format!("literal:string:{}", literal_percent_encode(&body)),
                    ts,
                ))
                .unwrap();
            store.add_link(&link(post, "we://comment", &c, ts)).unwrap();
            store
                .add_link(&link(&c, "we://reply_to", post, ts))
                .unwrap();
        }
    }
    store
}

fn comment(post: usize, i: u64) -> String {
    format!("we://c/{post}/{i}")
}

fn resolver() -> (
    StaticShapeResolver,
    std::sync::Arc<super::types::ModelShape>,
) {
    let (resolver, post_shape) = StaticShapeResolver::from_json("Post", POST_SHAPE).unwrap();
    resolver.register(
        "Comment",
        parse_shape_from_json(COMMENT_SHAPE, "Comment").unwrap(),
    );
    (resolver, post_shape)
}

/// Each post's included ids for `relation`, keyed by post id.
async fn included(
    store: &SparqlStore,
    relation: &str,
    sub: ModelQueryInput,
) -> HashMap<String, Vec<String>> {
    let (resolver, post_shape) = resolver();
    let query = ModelQueryInput {
        include: Some(HashMap::from([(
            relation.to_string(),
            IncludeValue::SubQuery(Box::new(sub)),
        )])),
        // The fixture's proofs do not verify; these tests are about paging.
        include_unverified: Some(true),
        ..Default::default()
    };
    let result = super::query::execute_model_query(store, post_shape.as_ref(), &query, &resolver)
        .await
        .unwrap();
    assert_eq!(result.instances.len(), 2, "both posts are on the page");
    result
        .instances
        .iter()
        .map(|post| {
            let ids = post[relation]
                .as_array()
                .unwrap_or_else(|| panic!("`{relation}` is an array: {post}"))
                .iter()
                .map(|c| {
                    c["id"]
                        .as_str()
                        .unwrap_or_else(|| panic!("an included row is hydrated: {c}"))
                        .to_string()
                })
                .collect();
            (post["id"].as_str().unwrap().to_string(), ids)
        })
        .collect()
}

fn latest_first() -> Option<Vec<(String, OrderDirection)>> {
    Some(vec![("timestamp".to_string(), OrderDirection::DESC)])
}

fn expect(pairs: [(&str, Vec<String>); 2]) -> HashMap<String, Vec<String>> {
    pairs.into_iter().map(|(k, v)| (k.to_string(), v)).collect()
}

#[tokio::test]
async fn a_forward_include_limit_applies_per_parent() {
    let store = store_with_two_posts();
    let got = included(
        &store,
        "comments",
        ModelQueryInput {
            limit: Some(2),
            order: latest_first(),
            ..Default::default()
        },
    )
    .await;
    assert_eq!(
        got,
        expect([
            ("we://post/1", vec![comment(1, 3), comment(1, 2)]),
            ("we://post/2", vec![comment(2, 3), comment(2, 2)]),
        ]),
        "each post gets its own two latest comments"
    );
}

#[tokio::test]
async fn a_reverse_include_limit_applies_per_parent() {
    let store = store_with_two_posts();
    let got = included(
        &store,
        "replies",
        ModelQueryInput {
            limit: Some(2),
            order: latest_first(),
            ..Default::default()
        },
    )
    .await;
    assert_eq!(
        got,
        expect([
            ("we://post/1", vec![comment(1, 3), comment(1, 2)]),
            ("we://post/2", vec![comment(2, 3), comment(2, 2)]),
        ])
    );
}

#[tokio::test]
async fn an_include_offset_applies_per_parent() {
    let store = store_with_two_posts();
    let got = included(
        &store,
        "comments",
        ModelQueryInput {
            limit: Some(1),
            offset: Some(1),
            order: latest_first(),
            ..Default::default()
        },
    )
    .await;
    assert_eq!(
        got,
        expect([
            ("we://post/1", vec![comment(1, 2)]),
            ("we://post/2", vec![comment(2, 2)]),
        ])
    );
}

/// No `order`: the N are chosen oldest first (a limited read with no order has
/// always meant timestamp ascending) and keep the relation's link order.
#[tokio::test]
async fn an_include_limit_without_order_keeps_the_oldest_per_parent() {
    let store = store_with_two_posts();
    let got = included(
        &store,
        "comments",
        ModelQueryInput {
            limit: Some(2),
            ..Default::default()
        },
    )
    .await;
    assert_eq!(
        got,
        expect([
            ("we://post/1", vec![comment(1, 1), comment(1, 2)]),
            ("we://post/2", vec![comment(2, 1), comment(2, 2)]),
        ])
    );
}

/// An order the store cannot sort by (two keys) cannot pick the N in the store,
/// so the include reads each parent's rows whole and pages them afterwards —
/// still per parent, still in the requested order.
#[tokio::test]
async fn an_include_limit_with_an_order_the_store_cannot_express_still_applies_per_parent() {
    let store = store_with_two_posts();
    let got = included(
        &store,
        "comments",
        ModelQueryInput {
            limit: Some(2),
            order: Some(vec![
                ("body".to_string(), OrderDirection::DESC),
                ("timestamp".to_string(), OrderDirection::ASC),
            ]),
            ..Default::default()
        },
    )
    .await;
    assert_eq!(
        got,
        expect([
            ("we://post/1", vec![comment(1, 1), comment(1, 2)]),
            ("we://post/2", vec![comment(2, 1), comment(2, 2)]),
        ])
    );
}

/// A comment linked from both posts counts toward each post's limit on its own
/// terms: post/1's two latest include the shared one, and post/2 still gets two.
#[tokio::test]
async fn a_row_shared_by_two_parents_is_paged_for_each() {
    let store = store_with_two_posts();
    // Newer than everything else, linked from both posts.
    let shared = "we://c/shared";
    let ts = 1_700_000_001_000;
    store
        .add_link(&link(shared, "we://cflag", "we://comment", ts))
        .unwrap();
    for post in ["we://post/1", "we://post/2"] {
        store
            .add_link(&link(post, "we://comment", shared, ts))
            .unwrap();
    }
    let got = included(
        &store,
        "comments",
        ModelQueryInput {
            limit: Some(2),
            order: latest_first(),
            ..Default::default()
        },
    )
    .await;
    assert_eq!(
        got,
        expect([
            ("we://post/1", vec![shared.to_string(), comment(1, 3)]),
            ("we://post/2", vec![shared.to_string(), comment(2, 3)]),
        ])
    );
}

/// An include's `where` still filters before the page is cut, per parent.
#[tokio::test]
async fn an_include_where_filters_before_the_per_parent_limit() {
    let store = store_with_two_posts();
    let got = included(
        &store,
        "comments",
        ModelQueryInput {
            limit: Some(1),
            order: latest_first(),
            where_clause: Some(std::collections::BTreeMap::from([(
                "body".to_string(),
                super::types::WhereCondition::StringArray(vec![
                    "body-3".to_string(),
                    "body-2".to_string(),
                ]),
            )])),
            ..Default::default()
        },
    )
    .await;
    // body-1 is each post's latest comment, so the filter leaves the second.
    assert_eq!(
        got,
        expect([
            ("we://post/1", vec![comment(1, 2)]),
            ("we://post/2", vec![comment(2, 2)]),
        ])
    );
}

/// A `where` the store cannot evaluate (a bare `author` is matched after
/// hydration) must not let the store cut each parent's rows first: the cut
/// would keep rows the filter then drops, and leave the parent short.
#[tokio::test]
async fn an_include_where_matched_after_hydration_is_not_cut_in_the_store() {
    let store = store_with_two_posts();
    for (p, post) in ["we://post/1", "we://post/2"].iter().enumerate() {
        let c = comment(p + 1, 0);
        let ts = 1_700_000_000_001 + (p as u64) * 100;
        for (s, pred, t) in [
            (c.as_str(), "we://cflag", "we://comment"),
            (*post, "we://comment", c.as_str()),
        ] {
            let mut l = link(s, pred, t, ts);
            l.author = "did:key:other".to_string();
            store.add_link(&l).unwrap();
        }
    }
    let got = included(
        &store,
        "comments",
        ModelQueryInput {
            limit: Some(1),
            order: latest_first(),
            where_clause: Some(std::collections::BTreeMap::from([(
                "author".to_string(),
                super::types::WhereCondition::String("did:key:other".to_string()),
            )])),
            ..Default::default()
        },
    )
    .await;
    // Each post's latest comment is by test123; the only match is its oldest.
    assert_eq!(
        got,
        expect([
            ("we://post/1", vec![comment(1, 0)]),
            ("we://post/2", vec![comment(2, 0)]),
        ])
    );
}

/// Without `limit` / `offset` every parent keeps every related row, in the
/// relation's link order.
#[tokio::test]
async fn an_include_without_limit_returns_every_row_per_parent() {
    let store = store_with_two_posts();
    let got = included(&store, "comments", ModelQueryInput::default()).await;
    assert_eq!(
        got,
        expect([
            ("we://post/1", (1..=3).map(|i| comment(1, i)).collect()),
            ("we://post/2", (1..=3).map(|i| comment(2, i)).collect()),
        ])
    );
}

/// A relation filled by a getter of the model author's own lists something
/// other than its links, so the store cannot cut it by those links: here the
/// getter leaves out each post's latest comment. A cut over the links would
/// keep `{3, 2}` and then lose `3` to the getter, leaving one comment where
/// the relation has two to give.
#[tokio::test]
async fn an_include_limit_on_a_getter_relation_pages_what_the_getter_lists() {
    let store = store_with_two_posts();
    let shape_json = r#"{
        "className": "Post",
        "properties": {
            "flag": { "predicate": "we://flag", "required": true, "flag": true, "initial": "we://post" }
        },
        "relations": {
            "comments": {
                "predicate": "we://comment",
                "kind": "hasMany",
                "targetClassName": "Comment",
                "getter": "SELECT ?target WHERE { <Base> <we://comment> ?target . FILTER(?target != <we://c/1/3> && ?target != <we://c/2/3>) }"
            }
        }
    }"#;
    let (resolver, post_shape) = StaticShapeResolver::from_json("Post", shape_json).unwrap();
    resolver.register(
        "Comment",
        parse_shape_from_json(COMMENT_SHAPE, "Comment").unwrap(),
    );
    let query = ModelQueryInput {
        include: Some(HashMap::from([(
            "comments".to_string(),
            IncludeValue::SubQuery(Box::new(ModelQueryInput {
                limit: Some(2),
                order: latest_first(),
                ..Default::default()
            })),
        )])),
        include_unverified: Some(true),
        ..Default::default()
    };
    let result = super::query::execute_model_query(&store, post_shape.as_ref(), &query, &resolver)
        .await
        .unwrap();
    let got: HashMap<String, Vec<String>> = result
        .instances
        .iter()
        .map(|post| {
            let ids = post["comments"]
                .as_array()
                .unwrap()
                .iter()
                .map(|c| c["id"].as_str().unwrap().to_string())
                .collect();
            (post["id"].as_str().unwrap().to_string(), ids)
        })
        .collect();
    assert_eq!(
        got,
        expect([
            ("we://post/1", vec![comment(1, 2), comment(1, 1)]),
            ("we://post/2", vec![comment(2, 2), comment(2, 1)]),
        ])
    );
}
