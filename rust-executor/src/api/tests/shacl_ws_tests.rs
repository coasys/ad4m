//! Integration tests for the four SHACL WS-RPC endpoints
//! (`perspective.getShaclNames`, `.getShaclTargetClass`, `.getShacl`,
//! `.getAllShacl`).
//!
//! These pin the central correctness claim of PR #935 — that the Rust
//! walk returns the same triple set the old TS client walk did — by
//! seeding a real `PerspectiveInstance` with a known SHACL corpus and
//! asserting on the extracted names / target class / triples.
//!
//! The tests exercise the `pub(crate)` helpers directly (rather than the
//! WS handler entry points) so we can drive them from a `PerspectiveInstance`
//! without needing to bootstrap the full `RequestContext` / capability
//! plumbing — the handler bodies are thin wrappers over these helpers.

use std::collections::HashSet;

use uuid::Uuid;

use crate::agent::{AgentContext, AgentService};
use crate::api::perspectives_ws::{
    resolve_shacl_links, resolve_shacl_names, resolve_shacl_target_class, ShaclLinkTriple,
};
use crate::db::Ad4mDb;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::prolog_service::init_prolog_service;
use crate::test_utils::setup_wallet;
use crate::types::{
    Link, LinkExpression, LinkQuery, LinkStatus, PerspectiveHandle, PerspectiveState,
};

// ── Setup ────────────────────────────────────────────────────────────────────

async fn setup_perspective() -> PerspectiveInstance {
    setup_wallet();
    // Global DB is single-init; safe to call repeatedly under `Once` inside.
    let _ = Ad4mDb::init_global_instance(":memory:");
    AgentService::init_global_test_instance();
    init_prolog_service().await;

    let perspective = PerspectiveInstance::new(
        PerspectiveHandle {
            uuid: Uuid::new_v4().to_string(),
            name: Some("shacl-ws-test".into()),
            shared_url: None,
            neighbourhood: None,
            state: PerspectiveState::Private,
            owners: None,
        },
        None,
    );
    // SHACL walk uses `get_links` only (no prolog). Skip prolog pool setup.
    perspective
}

/// Seed the perspective with one SHACL shape following the on-store
/// convention the executor and the SDK share:
///
/// * `ad4m://self  ad4m://has_shacl        literal:string:shacl://<name>`
/// * `literal:string:shacl://<name>  ad4m://shacl_shape_uri  <shape_uri>`
/// * `<shape_uri>  sh://targetClass  <target_class>`
/// * `<shape_uri>  sh://property     <prop_uri>` (one per property)
/// * `<prop_uri>   sh://path         <path>`
/// * `<prop_uri>   sh://datatype     <datatype>`
/// * `<prop_uri>   sh://minCount     literal:number:<min>`
/// * `<prop_uri>   sh://maxCount     literal:number:<max>`
#[derive(Clone)]
struct SeedProp {
    name: &'static str,
    path: &'static str,
    datatype: &'static str,
    min_count: u32,
    max_count: u32,
}

async fn seed_shape(
    perspective: &mut PerspectiveInstance,
    ctx: &AgentContext,
    name: &str,
    shape_uri: &str,
    target_class: &str,
    props: &[SeedProp],
) {
    let literal_name = format!("literal:string:shacl://{name}");

    // has_shacl edge
    perspective
        .add_link(
            Link {
                source: "ad4m://self".into(),
                predicate: Some("ad4m://has_shacl".into()),
                target: literal_name.clone(),
            },
            LinkStatus::Local,
            None,
            ctx,
        )
        .await
        .expect("seed has_shacl");

    // name → shape uri
    perspective
        .add_link(
            Link {
                source: literal_name,
                predicate: Some("ad4m://shacl_shape_uri".into()),
                target: shape_uri.into(),
            },
            LinkStatus::Local,
            None,
            ctx,
        )
        .await
        .expect("seed shacl_shape_uri");

    // shape → target class
    perspective
        .add_link(
            Link {
                source: shape_uri.into(),
                predicate: Some("sh://targetClass".into()),
                target: target_class.into(),
            },
            LinkStatus::Local,
            None,
            ctx,
        )
        .await
        .expect("seed targetClass");

    for p in props {
        let prop_uri = format!("{shape_uri}.{name}", name = p.name);
        // shape → property
        perspective
            .add_link(
                Link {
                    source: shape_uri.into(),
                    predicate: Some("sh://property".into()),
                    target: prop_uri.clone(),
                },
                LinkStatus::Local,
                None,
                ctx,
            )
            .await
            .expect("seed sh://property");

        // property → path
        perspective
            .add_link(
                Link {
                    source: prop_uri.clone(),
                    predicate: Some("sh://path".into()),
                    target: p.path.into(),
                },
                LinkStatus::Local,
                None,
                ctx,
            )
            .await
            .expect("seed sh://path");

        // property → datatype
        perspective
            .add_link(
                Link {
                    source: prop_uri.clone(),
                    predicate: Some("sh://datatype".into()),
                    target: p.datatype.into(),
                },
                LinkStatus::Local,
                None,
                ctx,
            )
            .await
            .expect("seed sh://datatype");

        // cardinalities
        perspective
            .add_link(
                Link {
                    source: prop_uri.clone(),
                    predicate: Some("sh://minCount".into()),
                    target: format!("literal:number:{}", p.min_count),
                },
                LinkStatus::Local,
                None,
                ctx,
            )
            .await
            .expect("seed sh://minCount");

        perspective
            .add_link(
                Link {
                    source: prop_uri,
                    predicate: Some("sh://maxCount".into()),
                    target: format!("literal:number:{}", p.max_count),
                },
                LinkStatus::Local,
                None,
                ctx,
            )
            .await
            .expect("seed sh://maxCount");
    }
}

fn triple_set(triples: &[ShaclLinkTriple]) -> HashSet<(String, String, String)> {
    triples
        .iter()
        .map(|t| (t.source.clone(), t.predicate.clone(), t.target.clone()))
        .collect()
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[tokio::test]
async fn shacl_names_returns_seeded_names() {
    let mut perspective = setup_perspective().await;
    let ctx = AgentContext::main_agent();

    let props = vec![
        SeedProp {
            name: "body",
            path: "flux://body",
            datatype: "xsd:string",
            min_count: 1,
            max_count: 1,
        },
        SeedProp {
            name: "author",
            path: "flux://author",
            datatype: "xsd:string",
            min_count: 1,
            max_count: 1,
        },
    ];
    seed_shape(
        &mut perspective,
        &ctx,
        "Message",
        "flux://MessageShape",
        "flux://Message",
        &props,
    )
    .await;
    seed_shape(
        &mut perspective,
        &ctx,
        "Channel",
        "flux://ChannelShape",
        "flux://Channel",
        &[SeedProp {
            name: "name",
            path: "flux://name",
            datatype: "xsd:string",
            min_count: 1,
            max_count: 1,
        }],
    )
    .await;

    let names = resolve_shacl_names(&perspective).await.expect("names");
    let got: HashSet<String> = names.into_iter().collect();
    let want: HashSet<String> = ["Message", "Channel"]
        .iter()
        .map(|s| s.to_string())
        .collect();
    assert_eq!(got, want, "shape names should match the seeded set exactly");
}

#[tokio::test]
async fn shacl_target_class_resolves_and_missing_returns_none() {
    let mut perspective = setup_perspective().await;
    let ctx = AgentContext::main_agent();

    seed_shape(
        &mut perspective,
        &ctx,
        "Message",
        "flux://MessageShape",
        "flux://Message",
        &[SeedProp {
            name: "body",
            path: "flux://body",
            datatype: "xsd:string",
            min_count: 1,
            max_count: 1,
        }],
    )
    .await;

    let tc = resolve_shacl_target_class(&perspective, "Message")
        .await
        .expect("target class");
    assert_eq!(tc.as_deref(), Some("flux://Message"));

    // Pins the wire contract: unknown name → `None` (serialized as JSON
    // `null` at the handler boundary, mapped to `undefined` at the TS
    // proxy). Guards against a future refactor silently changing this to
    // an empty string or an error.
    let missing = resolve_shacl_target_class(&perspective, "NonExistent")
        .await
        .expect("target class for missing");
    assert!(
        missing.is_none(),
        "unknown shape name must resolve to None, got {:?}",
        missing
    );
}

#[tokio::test]
async fn shacl_get_returns_all_property_triples() {
    let mut perspective = setup_perspective().await;
    let ctx = AgentContext::main_agent();

    let props = vec![
        SeedProp {
            name: "body",
            path: "flux://body",
            datatype: "xsd:string",
            min_count: 1,
            max_count: 1,
        },
        SeedProp {
            name: "timestamp",
            path: "flux://timestamp",
            datatype: "xsd:dateTime",
            min_count: 0,
            max_count: 1,
        },
    ];
    seed_shape(
        &mut perspective,
        &ctx,
        "Message",
        "flux://MessageShape",
        "flux://Message",
        &props,
    )
    .await;

    let (shape_uri, triples) = resolve_shacl_links(&perspective, "Message")
        .await
        .expect("resolve")
        .expect("shape exists");
    assert_eq!(shape_uri, "flux://MessageShape");

    let ts = triple_set(&triples);

    // targetClass
    assert!(ts.contains(&(
        "flux://MessageShape".into(),
        "sh://targetClass".into(),
        "flux://Message".into(),
    )));
    // both property URIs are linked from the shape
    assert!(ts.contains(&(
        "flux://MessageShape".into(),
        "sh://property".into(),
        "flux://MessageShape.body".into(),
    )));
    assert!(ts.contains(&(
        "flux://MessageShape".into(),
        "sh://property".into(),
        "flux://MessageShape.timestamp".into(),
    )));
    // each property has its full quintet (path, datatype, minCount, maxCount)
    for p in &props {
        let prop_uri = format!("flux://MessageShape.{}", p.name);
        assert!(
            ts.contains(&(prop_uri.clone(), "sh://path".into(), p.path.to_string(),)),
            "missing sh://path for {}",
            p.name
        );
        assert!(
            ts.contains(&(
                prop_uri.clone(),
                "sh://datatype".into(),
                p.datatype.to_string(),
            )),
            "missing sh://datatype for {}",
            p.name
        );
        assert!(
            ts.contains(&(
                prop_uri.clone(),
                "sh://minCount".into(),
                format!("literal:number:{}", p.min_count),
            )),
            "missing sh://minCount for {}",
            p.name
        );
        assert!(
            ts.contains(&(
                prop_uri,
                "sh://maxCount".into(),
                format!("literal:number:{}", p.max_count),
            )),
            "missing sh://maxCount for {}",
            p.name
        );
    }

    // resolve_shacl_links de-duplicates identical triples (see step 5 of
    // the walk). Confirm no duplicates leaked into the response.
    assert_eq!(
        triples.len(),
        ts.len(),
        "resolve_shacl_links must not return duplicate triples"
    );

    // No shape exists under this name.
    let missing = resolve_shacl_links(&perspective, "NonExistent")
        .await
        .expect("resolve missing");
    assert!(missing.is_none());
}

#[tokio::test]
async fn shacl_get_all_returns_every_seeded_shape() {
    let mut perspective = setup_perspective().await;
    let ctx = AgentContext::main_agent();

    seed_shape(
        &mut perspective,
        &ctx,
        "Message",
        "flux://MessageShape",
        "flux://Message",
        &[
            SeedProp {
                name: "body",
                path: "flux://body",
                datatype: "xsd:string",
                min_count: 1,
                max_count: 1,
            },
            SeedProp {
                name: "author",
                path: "flux://author",
                datatype: "xsd:string",
                min_count: 1,
                max_count: 1,
            },
        ],
    )
    .await;
    seed_shape(
        &mut perspective,
        &ctx,
        "Channel",
        "flux://ChannelShape",
        "flux://Channel",
        &[
            SeedProp {
                name: "name",
                path: "flux://name",
                datatype: "xsd:string",
                min_count: 1,
                max_count: 1,
            },
            SeedProp {
                name: "owner",
                path: "flux://owner",
                datatype: "xsd:anyURI",
                min_count: 1,
                max_count: 1,
            },
        ],
    )
    .await;

    // Reproduce the outer walk (name enumeration + per-shape resolution)
    // so the test exercises the exact composition `get_all_shacl` uses.
    let names = resolve_shacl_names(&perspective).await.expect("names");
    let mut resolved: Vec<(String, String, Vec<ShaclLinkTriple>)> = Vec::new();
    for name in &names {
        if let Some((uri, links)) = resolve_shacl_links(&perspective, name)
            .await
            .expect("resolve")
        {
            resolved.push((name.clone(), uri, links));
        }
    }

    let got: HashSet<String> = resolved.iter().map(|(n, _, _)| n.clone()).collect();
    let want: HashSet<String> = ["Message", "Channel"]
        .iter()
        .map(|s| s.to_string())
        .collect();
    assert_eq!(got, want);

    for (name, uri, links) in &resolved {
        let ts = triple_set(links);
        let expected_target = match name.as_str() {
            "Message" => "flux://Message",
            "Channel" => "flux://Channel",
            other => panic!("unexpected shape: {other}"),
        };
        assert!(
            ts.contains(&(
                uri.clone(),
                "sh://targetClass".into(),
                expected_target.into(),
            )),
            "shape {name} missing its targetClass triple"
        );
        // Each seeded shape has 2 property URIs → 2 `sh://property` edges.
        let prop_edges = links
            .iter()
            .filter(|t| t.source == *uri && t.predicate == "sh://property")
            .count();
        assert_eq!(prop_edges, 2, "shape {name} should have 2 property edges");
    }
}

#[tokio::test]
async fn shacl_get_all_handles_target_class_unlinked_mid_walk() {
    // Concurrency race: after we've enumerated names but before we resolve
    // shape_a's triples, drop shape_a's `sh://targetClass` link. The walk
    // still finds the shape URI (via `ad4m://shacl_shape_uri`), so shape_a
    // is included in the result — but with no `sh://targetClass` triple.
    //
    // Documents current behaviour: partial shape (no targetClass triple)
    // is included; the missing edge silently vanishes from the response.
    // If this ever flips to a hard error / drop, this assert must be
    // updated intentionally.
    let mut perspective = setup_perspective().await;
    let ctx = AgentContext::main_agent();

    seed_shape(
        &mut perspective,
        &ctx,
        "ShapeA",
        "app://ShapeA",
        "app://TargetA",
        &[SeedProp {
            name: "x",
            path: "app://x",
            datatype: "xsd:string",
            min_count: 1,
            max_count: 1,
        }],
    )
    .await;

    // Step 1: enumerate names (as `get_all_shacl` would).
    let names = resolve_shacl_names(&perspective).await.expect("names");
    assert_eq!(names, vec!["ShapeA".to_string()]);

    // Step 1.5 (race): unlink the targetClass edge before we resolve it.
    // `remove_link` looks up by (source, predicate, target, author, timestamp)
    // so we have to fetch the actual expression first.
    let tc_links = perspective
        .get_links(&LinkQuery {
            source: Some("app://ShapeA".into()),
            predicate: Some("sh://targetClass".into()),
            target: Some("app://TargetA".into()),
            ..Default::default()
        })
        .await
        .expect("lookup targetClass");
    assert_eq!(
        tc_links.len(),
        1,
        "expected exactly one targetClass edge to unlink"
    );
    let tc_expr = LinkExpression::from(tc_links.into_iter().next().unwrap());
    perspective
        .remove_link(tc_expr, None)
        .await
        .expect("remove targetClass mid-walk");

    // Step 2: resolve shape.
    let (uri, links) = resolve_shacl_links(&perspective, "ShapeA")
        .await
        .expect("resolve")
        .expect("shape uri still resolvable via shacl_shape_uri");
    assert_eq!(uri, "app://ShapeA");

    let ts = triple_set(&links);
    let has_target_class = ts.iter().any(|(_, p, _)| p == "sh://targetClass");
    assert!(
        !has_target_class,
        // documents current behaviour: silent drop on targetClass unlinked mid-walk
        "current wire contract: partial shape (no targetClass triple) is returned when the edge disappears between name enumeration and shape resolution"
    );

    // The property sub-walk still yields the property triples — only the
    // shape-level targetClass edge was removed.
    assert!(ts.contains(&(
        "app://ShapeA".into(),
        "sh://property".into(),
        "app://ShapeA.x".into(),
    )));
}
