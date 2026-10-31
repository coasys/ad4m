//! Which properties a query resolves through their language: only those it hands back or still
//! reads. A property resolved through a language is a fetch, often a network round trip, and one the
//! query did not ask for is stripped before the result leaves — so resolving it was the whole cost of
//! a record's file for a query that wanted its name.
//!
//! What the `where` and `order` read is resolved for every row before the window, whether the query
//! returns it or not; the page pass resolves the rest of what it returns.

use super::page_before_resolve_tests::{
    data_uri, file_type, id, ids, link, run, seed, shape_json, ts, ts_ms, FileLanguage,
};
use super::query::{resolve_language_transforms, returned};
use super::test_helpers::{prop, shape};
use super::types::{ModelQueryInput, TransformExpression};
use crate::perspectives::sparql_store::SparqlStore;
use serde_json::{json, Value};
use std::time::Duration;

fn sorted(keys: Option<Vec<String>>) -> Option<Vec<String>> {
    keys.map(|mut v| {
        v.sort();
        v
    })
}

fn strings(names: &[&str]) -> Vec<String> {
    names.iter().map(|s| s.to_string()).collect()
}

#[test]
fn returns_everything_when_no_properties_are_named() {
    assert_eq!(returned(&ModelQueryInput::default()), None);
}

#[test]
fn returns_only_what_was_asked_for() {
    let query = ModelQueryInput {
        properties: Some(strings(&["name"])),
        ..Default::default()
    };
    assert_eq!(
        sorted(returned(&query)),
        Some(strings(&["baseExpression", "id", "name"]))
    );
}

#[test]
fn returns_what_an_include_hands_back() {
    let query: ModelQueryInput = serde_json::from_value(json!({
        "properties": ["name"],
        "include": { "body": true }
    }))
    .unwrap();
    assert_eq!(
        sorted(returned(&query)),
        Some(strings(&["baseExpression", "body", "id", "name"]))
    );
}

/// A deterministic property with a transform is transformed here without any language, so whether
/// it was left alone says whether the selection reached it.
#[tokio::test]
async fn leaves_a_property_it_was_not_asked_for_as_it_was() {
    let mut label = prop("label", "test://label");
    label.transform = Some(TransformExpression::Literal {
        value: json!("transformed"),
    });
    let shape = shape("Thing", vec![prop("name", "test://name"), label]);

    let mut skipped = vec![json!({ "id": "a", "name": "A", "label": "stored" })];
    resolve_language_transforms(&shape, &mut skipped, |name| name == "name")
        .await
        .unwrap();
    assert_eq!(skipped[0]["label"], json!("stored"));

    let mut resolved = vec![json!({ "id": "a", "name": "A", "label": "stored" })];
    resolve_language_transforms(&shape, &mut resolved, |_| true)
        .await
        .unwrap();
    assert_eq!(resolved[0]["label"], json!("transformed"));
}

/// No `properties`: every row of the page comes back with its file decoded.
#[tokio::test]
async fn a_query_without_properties_resolves_every_file_on_its_page() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    let store = seed(10, Some(&files));
    let shape = shape_json(Some(&files));

    let (rows, _, _) = run(&store, &shape, json!({ "limit": 4 })).await;

    assert_eq!(rows.len(), 4);
    for row in &rows {
        let i: usize = row["id"].as_str().unwrap()[7..].parse().unwrap();
        assert_eq!(row["file"], data_uri(i), "{row}");
    }
    assert_eq!(files.gets(), 4);
    files.stop().await;
}

/// `properties` that leave the file out: the file is stripped, so it is not fetched.
#[tokio::test]
async fn a_query_naming_its_properties_fetches_no_file_it_leaves_out() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    let store = seed(10, Some(&files));
    let shape = shape_json(Some(&files));

    let (rows, _, _) = run(
        &store,
        &shape,
        json!({ "properties": ["title"], "limit": 4 }),
    )
    .await;

    assert_eq!(rows.len(), 4);
    for row in &rows {
        assert!(row.get("file").is_none(), "{row}");
        assert!(row["title"].is_string(), "{row}");
    }
    assert_eq!(files.gets(), 0, "fetched a file the query left out");
    files.stop().await;
}

/// `properties` that name the file, with a filter that runs after hydration: the page's files.
#[tokio::test]
async fn a_query_naming_the_file_fetches_it_for_the_page() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    let store = seed(30, Some(&files));
    let shape = shape_json(Some(&files));

    let (rows, total, _) = run(
        &store,
        &shape,
        json!({
            "properties": ["title", "file"],
            "where": { "kind": "post", "timestamp": { "gt": ts_ms(10) } },
            "limit": 3,
        }),
    )
    .await;

    let page: Vec<usize> = vec![12, 14, 16];
    assert_eq!(ids(&rows), page.iter().map(|&i| id(i)).collect::<Vec<_>>());
    assert_eq!(total, 9);
    for (row, &i) in rows.iter().zip(&page) {
        assert_eq!(row["file"], data_uri(i), "{row}");
    }
    assert_eq!(files.gets(), page.len());
    files.stop().await;
}

/// The strip keeps `include` keys beside `properties`, so a value it hands back under one is
/// resolved too.
#[tokio::test]
async fn an_include_key_is_resolved_as_the_strip_keeps_it() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    let store = seed(10, Some(&files));
    let shape = shape_json(Some(&files));

    let (rows, _, _) = run(
        &store,
        &shape,
        json!({ "properties": ["title"], "include": { "file": true }, "limit": 3 }),
    )
    .await;

    assert_eq!(rows.len(), 3);
    for row in &rows {
        let i: usize = row["id"].as_str().unwrap()[7..].parse().unwrap();
        assert_eq!(row["file"], data_uri(i), "{row}");
    }
    assert_eq!(files.gets(), 3);
    files.stop().await;
}

/// A `where` on the file that `properties` leaves out still reads every row's decoded file, and
/// the file is then stripped: the filter needs it, the result does not.
#[tokio::test]
async fn a_where_on_a_file_left_out_of_properties_still_reads_the_decoded_file() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    let n = 30;
    let store = seed(n, Some(&files));
    let shape = shape_json(Some(&files));

    // The `timestamp` arm keeps the whole `OR` out of the store.
    let (rows, total, _) = run(
        &store,
        &shape,
        json!({
            "properties": ["title"],
            "where": { "OR": [
                { "file": { "contains": "image/png" } },
                { "timestamp": { "gt": ts_ms(25) } }
            ] },
            "limit": 4,
            "offset": 2,
            "order": { "timestamp": "DESC" },
        }),
    )
    .await;

    let matching: Vec<usize> = (0..n).rev().filter(|&i| i % 3 == 0 || i > 25).collect();
    let page: Vec<usize> = matching.iter().copied().skip(2).take(4).collect();
    assert_eq!(ids(&rows), page.iter().map(|&i| id(i)).collect::<Vec<_>>());
    assert_eq!(total, matching.len());
    for row in &rows {
        assert!(row.get("file").is_none(), "{row}");
    }
    assert_eq!(files.gets(), n, "each file is fetched once, for the filter");
    files.stop().await;
}

/// A sort on a field of the file that `properties` leaves out picks the page from decoded values.
#[tokio::test]
async fn a_sort_on_a_file_left_out_of_properties_picks_the_page_from_decoded_values() {
    let files = FileLanguage::serve(Duration::ZERO).await;
    // `rank` = 7i mod 13 is distinct for each of 12 items, so the order has no ties.
    let n = 12;
    let store = seed(n, Some(&files));
    // `focus` keeps the fetched expression data, so `file` decodes to an object.
    let shape = format!(
        r#"{{
        "className": "Item",
        "properties": {{
            "type": {{ "predicate": "ad4m://type", "required": true, "flag": true, "initial": "item://Item" }},
            "rank": {{ "predicate": "item://rank" }},
            "file": {{ "predicate": "item://file", "resolveLanguage": "{}", "transform": {{ "type": "focus" }} }}
        }},
        "relations": {{}}
    }}"#,
        files.address()
    );

    let (rows, _, _) = run(
        &store,
        &shape,
        json!({
            "properties": ["rank"],
            "order": [["file.file_type", "DESC"], ["rank", "ASC"]],
            "limit": 5,
            "offset": 2,
        }),
    )
    .await;

    let mut expected: Vec<usize> = (0..n).collect();
    expected.sort_by(|&a, &b| {
        file_type(b)
            .cmp(file_type(a))
            .then(((a * 7) % 13).cmp(&((b * 7) % 13)))
    });
    let page: Vec<usize> = expected.into_iter().skip(2).take(5).collect();
    assert_eq!(ids(&rows), page.iter().map(|&i| id(i)).collect::<Vec<_>>());
    for row in &rows {
        assert!(row.get("file").is_none(), "{row}");
    }
    assert_eq!(files.gets(), n, "each file is fetched once, for the sort");
    files.stop().await;
}

// Getters and reverse relations follow the same rule: each is a store read per page (a getter
// for every typed relation, since the SDK generates a conformance getter for each), so one the
// query neither returns nor filters on is not read at all.

/// Items with a typed `comments` and `owner` (getter-backed, as the SDK generates them) and a
/// reverse `markedBy`.
const RELATED_SHAPE: &str = r#"{
    "className": "Item",
    "properties": {
        "type": { "predicate": "ad4m://type", "required": true, "flag": true, "initial": "item://Item" },
        "kind": { "predicate": "item://kind" },
        "title": { "predicate": "item://title" },
        "rank": { "predicate": "item://rank" }
    },
    "relations": {
        "comments": { "predicate": "item://comment", "kind": "hasMany", "targetClassName": "Comment",
            "getter": "SELECT ?target WHERE { <Base> <item://comment> ?target . ?target <ad4m://type> <item://Comment> . }" },
        "owner": { "predicate": "item://owner", "kind": "hasOne", "targetClassName": "Owner",
            "getter": "SELECT ?target WHERE { <Base> <item://owner> ?target . ?target <ad4m://type> <item://Owner> . } LIMIT 1" },
        "markedBy": { "predicate": "item://marks", "kind": "belongsToMany", "targetClassName": "", "direction": "reverse" }
    }
}"#;

fn comment(i: usize) -> String {
    format!("comment://{i:05}")
}

fn owner(i: usize) -> String {
    format!("owner://{i:05}")
}

fn mark(i: usize) -> String {
    format!("mark://{i:05}")
}

/// Item `i`'s `markedBy`: `mark(i)` for an even item, none for an odd one.
fn marked_by(i: usize) -> Value {
    if i.is_multiple_of(2) {
        json!([mark(i)])
    } else {
        json!([])
    }
}

/// `seed`'s items, each with a comment and an owner; the even ones marked by `mark(i)`.
fn seed_related(n: usize) -> SparqlStore {
    let store = seed(n, None);
    for i in 0..n {
        let t = ts(i);
        let mut links = vec![
            link(&id(i), "item://comment", &comment(i), &t),
            link(&comment(i), "ad4m://type", "item://Comment", &t),
            link(&id(i), "item://owner", &owner(i), &t),
            link(&owner(i), "ad4m://type", "item://Owner", &t),
        ];
        if i.is_multiple_of(2) {
            links.push(link(&mark(i), "item://marks", &id(i), &t));
        }
        for l in links {
            store.add_link(&l).unwrap();
        }
    }
    store
}

/// The relation reads among `queries`: each getter's and the reverse relation's, by name.
fn relation_reads(queries: &[String]) -> Vec<(&'static str, &String)> {
    let reads = [
        ("comments", "?target <ad4m://type> <item://Comment>"),
        ("owner", "?target <ad4m://type> <item://Owner>"),
        ("markedBy", "?source <item://marks> ?target"),
    ];
    queries
        .iter()
        .flat_map(|q| {
            reads
                .iter()
                .filter(|(_, marker)| q.contains(marker))
                .map(move |(name, _)| (*name, q))
        })
        .collect()
}

/// What `f` returned and the store queries it ran.
async fn logged<T>(
    store: &SparqlStore,
    f: impl std::future::Future<Output = T>,
) -> (T, Vec<String>) {
    let before = store.queries_run().len();
    let out = f.await;
    (out, store.queries_run().split_off(before))
}

fn names(reads: &[(&'static str, &String)]) -> Vec<&'static str> {
    let mut names: Vec<&str> = reads.iter().map(|(name, _)| *name).collect();
    names.sort();
    names
}

fn item_index(row: &Value) -> usize {
    row["id"].as_str().unwrap()[7..].parse().unwrap()
}

/// No `properties`: every relation is read once, for the page, and handed back as before.
#[tokio::test]
async fn a_query_without_properties_reads_every_relation() {
    let store = seed_related(10);

    let ((rows, _, _), queries) =
        logged(&store, run(&store, RELATED_SHAPE, json!({ "limit": 4 }))).await;

    assert_eq!(rows.len(), 4);
    for row in &rows {
        let i = item_index(row);
        assert_eq!(row["comments"], json!([comment(i)]), "{row}");
        assert_eq!(row["owner"], json!(owner(i)), "{row}");
        assert_eq!(row["markedBy"], marked_by(i), "{row}");
    }
    assert_eq!(
        names(&relation_reads(&queries)),
        vec!["comments", "markedBy", "owner"]
    );
}

/// `properties: ['title']`: no getter and no reverse relation is read, with or without
/// `deepQuery` (relation getters run under `deepQuery: false` too).
#[tokio::test]
async fn a_query_naming_its_properties_reads_no_relation_it_leaves_out() {
    let store = seed_related(10);

    for deep_query in [true, false] {
        let ((rows, _, _), queries) = logged(
            &store,
            run(
                &store,
                RELATED_SHAPE,
                json!({ "properties": ["title"], "deepQuery": deep_query, "limit": 4 }),
            ),
        )
        .await;

        assert_eq!(rows.len(), 4);
        for row in &rows {
            let keys: Vec<&String> = row.as_object().unwrap().keys().collect();
            assert_eq!(keys.len(), 3, "id, baseExpression and title only: {row}");
            assert!(row["title"].is_string(), "{row}");
        }
        assert_eq!(
            names(&relation_reads(&queries)),
            Vec::<&str>::new(),
            "deepQuery: {deep_query}"
        );
    }
}

/// A relation named in `properties` is read, and only it.
#[tokio::test]
async fn a_query_naming_one_relation_reads_only_that_one() {
    let store = seed_related(10);

    let ((rows, _, _), queries) = logged(
        &store,
        run(
            &store,
            RELATED_SHAPE,
            json!({ "properties": ["title", "owner"], "limit": 4 }),
        ),
    )
    .await;

    for row in &rows {
        assert_eq!(row["owner"], json!(owner(item_index(row))), "{row}");
        assert!(row.get("comments").is_none(), "{row}");
    }
    assert_eq!(names(&relation_reads(&queries)), vec!["owner"]);
}

/// A `where` and an `order` on a getter-backed relation that `properties` leaves out still filter
/// and sort on its hydrated value; its getter, which runs on the page after both and whose value
/// the strip would drop, is not run.
#[tokio::test]
async fn a_where_and_order_on_a_getter_relation_left_out_of_properties_still_apply() {
    let store = seed_related(10);

    let ((rows, total, _), queries) = logged(&store, run(
        &store,
        RELATED_SHAPE,
        json!({
            "properties": ["title"],
            "where": { "OR": [{ "owner": owner(3) }, { "owner": owner(7) }, { "owner": owner(5) }] },
            "order": { "owner": "DESC" },
            "limit": 2,
        }),
    ))
    .await;

    assert_eq!(ids(&rows), vec![id(7), id(5)]);
    assert_eq!(total, 3);
    for row in &rows {
        assert!(row.get("owner").is_none(), "{row}");
    }
    assert_eq!(names(&relation_reads(&queries)), Vec::<&str>::new());
}

/// A `where` on a reverse relation that `properties` leaves out still filters on its value: it is
/// read for every row before the filter, then stripped. The getters, neither read nor returned,
/// are not run.
#[tokio::test]
async fn a_where_on_a_reverse_relation_left_out_of_properties_still_filters() {
    let n = 10;
    let store = seed_related(n);

    // The `timestamp` arm keeps the whole `OR` out of the store.
    let ((rows, total, _), queries) = logged(
        &store,
        run(
            &store,
            RELATED_SHAPE,
            json!({
                "properties": ["title"],
                "where": { "OR": [
                    { "markedBy": mark(4) },
                    { "timestamp": { "gt": ts_ms(7) } }
                ] },
                "limit": 2,
            }),
        ),
    )
    .await;

    assert_eq!(ids(&rows), vec![id(4), id(8)]);
    assert_eq!(total, 3, "items 4, 8 and 9");
    for row in &rows {
        assert!(row.get("markedBy").is_none(), "{row}");
    }
    let reads = relation_reads(&queries);
    assert_eq!(names(&reads), vec!["markedBy"]);
    assert!(
        reads[0].1.contains(&id(1)),
        "read for every row before the filter: {}",
        reads[0].1
    );
}

/// A reverse relation the query returns but does not filter on is read for the page only.
#[tokio::test]
async fn a_returned_reverse_relation_is_read_for_the_page_only() {
    let n = 20;
    let store = seed_related(n);

    let ((rows, total, _), queries) = logged(
        &store,
        run(
            &store,
            RELATED_SHAPE,
            json!({
                "properties": ["title", "markedBy"],
                "where": { "timestamp": { "gt": ts_ms(5) } },
                "limit": 3,
            }),
        ),
    )
    .await;

    assert_eq!(ids(&rows), vec![id(6), id(7), id(8)]);
    assert_eq!(total, n - 6);
    for row in &rows {
        let i = item_index(row);
        assert_eq!(row["markedBy"], marked_by(i), "{row}");
    }
    let reads = relation_reads(&queries);
    assert_eq!(names(&reads), vec!["markedBy"]);
    let read = reads[0].1;
    for i in 6..9 {
        assert!(read.contains(&id(i)), "{read}");
    }
    assert!(!read.contains(&id(9)), "read beyond the page: {read}");
}
