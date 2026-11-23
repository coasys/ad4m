//! What a mount owns, and what it leaves alone: links someone else wrote
//! under `<lang>://`, whatever their timestamp, the schema a newer sync
//! installed, another mount's shape nodes, and a mirror an older build
//! wrote.

use super::mount_gates::gates;
use super::mount_gates::MountPoint;
use super::mount_lifecycle_tests::{links_at, project};
use super::mounts_tests::{create, mounted};
use super::*;
use crate::agent::{did_for_context, sign_for_context, signing_key_id_for_context};
use crate::languages::mount_tests::load_mount_test_language;
use serde_json::json;

const EPOCH: &str = "1970-01-01T00:00:00.000Z";
/// The mount ledger's mark (`SparqlStore::add_mount_links`).
const MOUNTED_BY: &str = "ad4m://ontology/mountedBy";

/// `link` signed by the main agent at the Unix epoch, the timestamp A9's
/// mount used as its marker.
fn main_agent_link_at_epoch(link: Link) -> LinkExpression {
    let context = AgentContext::main_agent();
    let data = link.normalize();
    let signature = hex::encode(
        sign_for_context(
            &crate::agent::signatures::hash_data_and_timestamp(
                &data,
                &chrono::DateTime::<chrono::Utc>::UNIX_EPOCH,
            ),
            &context,
        )
        .unwrap(),
    );
    LinkExpression::from(Expression {
        author: did_for_context(&context).unwrap(),
        timestamp: EPOCH.to_string(),
        data,
        proof: ExpressionProof {
            key: signing_key_id_for_context(&context).unwrap(),
            signature,
        },
    })
}

/// Whether the store holds a link with exactly this author and timestamp
/// on `source -[predicate]-> target`.
async fn holds(p: &PerspectiveInstance, expected: &LinkExpression) -> bool {
    links_at(
        p,
        &expected.data.source,
        expected.data.predicate.as_deref().unwrap(),
    )
    .await
    .iter()
    .any(|l| {
        l.author == expected.author
            && l.timestamp == expected.timestamp
            && l.data.target == expected.data.target
    })
}

// --- Only the mount's own links are the mount's ---------------------------

#[tokio::test]
async fn an_epoch_dated_link_of_a_foreign_author_survives_sync_and_unmount() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _ctx) = mounted(&lang).await;
    let foreign = LinkExpression::from(Expression {
        author: "did:key:z6MkForeignAuthorOfAnAnnotation".to_string(),
        timestamp: EPOCH.to_string(),
        data: Link {
            source: format!("{lang}://t1"),
            predicate: Some("ex://note".to_string()),
            target: "literal:string:theirs".to_string(),
        },
        proof: ExpressionProof {
            key: "did:key:z6MkForeignAuthorOfAnAnnotation#key".to_string(),
            signature: "00".to_string(),
        },
    });
    p.add_link_expression(foreign.clone(), LinkStatus::Local, None)
        .await
        .unwrap();

    p.sync_mount(&lang).await.unwrap();
    assert!(holds(&p, &foreign).await, "sync removed it");
    p.unmount(&lang).await.unwrap();
    assert!(holds(&p, &foreign).await, "unmount removed it");
}

#[tokio::test]
async fn an_epoch_dated_link_of_the_main_agent_survives_sync_and_unmount() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _ctx) = mounted(&lang).await;
    // The same triple the mount wrote for t1's title, through the ordinary
    // `perspective.addLinkExpression` path.
    let mine = main_agent_link_at_epoch(Link {
        source: format!("{lang}://t1"),
        predicate: Some("mounttest://title".to_string()),
        target: "literal:string:a".to_string(),
    });
    p.add_link_expression(mine.clone(), LinkStatus::Local, None)
        .await
        .unwrap();

    project(
        &lang,
        "outsideChange",
        json!({"address": "t1", "data": {"title": "b"}, "emit": false}),
    )
    .await;
    p.sync_mount(&lang).await.unwrap();
    assert!(holds(&p, &mine).await, "sync removed it");
    p.unmount(&lang).await.unwrap();
    assert!(holds(&p, &mine).await, "unmount removed it");
}

// --- No caller can set the mount mark ---------------------------------------

/// The link that would mark `link`'s reifier as written by the mount of
/// `lang`: `<link:…> -[mountedBy]-> "<lang>"`, as the ledger stores it.
fn forged_mark(link: &LinkExpression, lang: &str) -> Link {
    let reifier = crate::perspectives::sparql_store::SparqlStore::reifier_of(link);
    Link {
        source: reifier
            .trim_start_matches('<')
            .trim_end_matches('>')
            .to_string(),
        predicate: Some(MOUNTED_BY.to_string()),
        target: format!("literal:string:{lang}"),
    }
}

/// A user's Local note on `t1`, as stored.
async fn users_note(p: &mut PerspectiveInstance, lang: &str) -> LinkExpression {
    let mut note = LinkExpression::from(
        p.add_links(
            vec![Link {
                source: format!("{lang}://t1"),
                predicate: Some("ex://note".to_string()),
                target: "literal:string:mine".to_string(),
            }],
            LinkStatus::Local,
            None,
            &AgentContext::main_agent(),
        )
        .await
        .unwrap()
        .remove(0),
    );
    note.status = Some(LinkStatus::Local);
    note
}

#[tokio::test]
async fn a_link_stating_the_mount_mark_is_never_stored() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted(&lang).await;
    let note = users_note(&mut p, &lang).await;
    let forged = forged_mark(&note, &lang);

    // Each path either refuses the link or stores nothing: the store
    // refuses the mark (`refuse_link_metadata`), and `write_link_diff`
    // logs a store refusal instead of returning it.
    let stored = |p: &PerspectiveInstance| {
        let p = p.clone();
        let source = forged.source.clone();
        async move { !links_at(&p, &source, MOUNTED_BY).await.is_empty() }
    };

    // `perspective.addLink` / `addLinks`.
    let _ = p
        .add_links(vec![forged.clone()], LinkStatus::Local, None, &ctx)
        .await;
    assert!(!stored(&p).await, "add_links stored the mount mark");

    // `perspective.addLinkExpression`: a caller-signed expression.
    let mut expression =
        LinkExpression::from(crate::agent::create_signed_expression(forged.clone(), &ctx).unwrap());
    let _ = p
        .add_link_expression(expression.clone(), LinkStatus::Local, None)
        .await;
    assert!(
        !stored(&p).await,
        "add_link_expression stored the mount mark"
    );

    // A batch commit, an import and a link-language diff all write through
    // `apply_diff`, which refuses the whole diff.
    expression.status = Some(LinkStatus::Local);
    assert!(
        p.sparql_store.apply_diff(&[], &[expression]).is_err(),
        "apply_diff took the mount mark"
    );
    assert!(
        links_at(&p, &forged.source, MOUNTED_BY).await.is_empty(),
        "a mount mark link is in the store"
    );
}

#[tokio::test]
async fn a_caller_cannot_make_its_link_mount_written() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, ctx) = mounted(&lang).await;
    let note = users_note(&mut p, &lang).await;

    // Try to mark the note as the mount's through each write path; the
    // outcome of each try does not matter, only what the mount thinks.
    let forged = forged_mark(&note, &lang);
    let _ = p
        .add_links(vec![forged.clone()], LinkStatus::Local, None, &ctx)
        .await;
    let mut expression =
        LinkExpression::from(crate::agent::create_signed_expression(forged, &ctx).unwrap());
    let _ = p
        .add_link_expression(expression.clone(), LinkStatus::Local, None)
        .await;
    expression.status = Some(LinkStatus::Local);
    let _ = p.sparql_store.apply_diff(&[], &[expression]);

    let decorated = DecoratedLinkExpression::from((note.clone(), LinkStatus::Local));
    assert!(
        !p.mount_written(&lang).unwrap().contains(&decorated),
        "the caller's note counts as written by the mount"
    );
    p.sync_mount(&lang).await.unwrap();
    assert!(holds(&p, &note).await, "sync removed the caller's note");
    p.unmount(&lang).await.unwrap();
    assert!(holds(&p, &note).await, "unmount removed the caller's note");
}

// --- A stale sync never rolls the schema back -------------------------------

#[tokio::test]
async fn an_older_sync_does_not_roll_back_newer_shapes() {
    let lang = load_mount_test_language().await;
    let (p, _ctx) = mounted(&lang).await;

    // Sync A reads version 2 and stops; sync B reads version 3 and applies.
    let gate = gates::arm(&lang, MountPoint::SnapshotFetched);
    let mut older = p.clone();
    let l = lang.clone();
    let a = tokio::spawn(async move { older.sync_mount(&l).await });
    gate.arrived().await;
    project(&lang, "setVersion", json!({"version": "3"})).await;
    p.clone().sync_mount(&lang).await.unwrap();
    gate.release();
    let _ = a.await.unwrap();

    let versions: Vec<String> = links_at(
        &p,
        &format!("{lang}://project"),
        "ad4m://mount_shapes_version",
    )
    .await
    .into_iter()
    .map(|l| l.data.target)
    .collect();
    assert_eq!(
        versions,
        vec!["literal:string:3".to_string()],
        "the older sync reinstalled its shapes"
    );
}

// --- Shape nodes and declared names are owned too ---------------------------

#[tokio::test]
async fn a_shape_node_of_another_mount_is_refused() {
    let lang = load_mount_test_language().await;
    let lang2 = load_mount_test_language().await;
    project(
        &lang,
        "setShapes",
        json!({"shapes": [{"name": "XFoo", "target": "a://XFoo"}]}),
    )
    .await;
    // Name, basename and IRI all differ; only the shape node is `lang`'s.
    project(
        &lang2,
        "setShapes",
        json!({"shapes": [{"name": "Foo", "target": "b://Foo", "nodeShapeUri": "a://XFooShape"}]}),
    )
    .await;
    let (mut p, _ctx) = mounted(&lang).await;

    let error = p.mount(&lang2).await.unwrap_err();
    assert!(error.to_string().contains("a://XFooShape"), "{error}");
}

#[tokio::test]
async fn one_shape_set_naming_a_class_twice_is_refused() {
    let lang = load_mount_test_language().await;
    project(
        &lang,
        "setShapes",
        json!({"shapes": [
            {"name": "Dup", "target": "mounttest://DupA"},
            {"name": "Dup", "target": "mounttest://DupB"}
        ]}),
    )
    .await;
    let (mut p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;

    let error = p.mount(&lang).await.unwrap_err();
    assert!(error.to_string().contains("Dup"), "{error}");
}

// --- A mirror an older build wrote is not adopted ---------------------------

/// A perspective holding what a pre-A10 build left of a mount of `lang`:
/// the mount link, and a version link and a record link written as
/// ordinary Local links. (Those builds wrote the mount link Shared; it is
/// Local here so a restore reaches the older-build check.)
async fn mirror_from_an_older_build(lang: &str) -> PerspectiveInstance {
    let (mut p, _shapes, ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    p.add_links(
        vec![Link {
            source: "ad4m://self".to_string(),
            predicate: Some("ad4m://mount".to_string()),
            target: format!("{lang}://project"),
        }],
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .unwrap();
    p.add_links(
        vec![
            Link {
                source: format!("{lang}://project"),
                predicate: Some("ad4m://mount_shapes_version".to_string()),
                target: "literal:string:2".to_string(),
            },
            Link {
                source: format!("{lang}://t1"),
                predicate: Some("mounttest://title".to_string()),
                target: "literal:string:old".to_string(),
            },
        ],
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .unwrap();
    p
}

#[tokio::test]
async fn a_restore_refuses_a_mirror_from_an_older_build() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "new"})).await;
    let mut p = mirror_from_an_older_build(&lang).await;

    p.restore_mounts().await;
    let registered = crate::perspectives::perspectives_for_mount(&lang);
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    assert!(registered.is_empty(), "the old mirror was activated");
    let titles = links_at(&p, &format!("{lang}://t1"), "mounttest://title").await;
    assert_eq!(titles.len(), 1, "the old mirror was synced: {titles:?}");
}

#[tokio::test]
async fn a_mount_refuses_a_mirror_from_an_older_build() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "new"})).await;
    let mut p = mirror_from_an_older_build(&lang).await;

    let error = p.mount(&lang).await.unwrap_err();
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    assert!(error.to_string().contains("fresh perspective"), "{error}");
}

// --- The ownership check and the install are one SDNA change ----------------

/// A `register_shapes` that arrives after a mount's class ownership check
/// waits for the mount's install: it cannot land in between and have its
/// class replaced by a check that no longer holds.
#[tokio::test]
async fn register_shapes_waits_for_a_mount_between_check_and_install() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (p, _shapes, ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;

    let gate = gates::arm(&lang, MountPoint::OwnershipChecked);
    let mut mounting = p.clone();
    let l = lang.clone();
    let mount = tokio::spawn(async move { mounting.mount(&l).await });
    gate.arrived().await;

    let thing: crate::perspectives::shacl_parser::SHACLShape = serde_json::from_value(json!({
        "target_class": "app://Gadget",
        "properties": [{"path": "app://title", "name": "title", "datatype": "xsd:string", "max_count": 1}]
    }))
    .unwrap();
    let mut registering = p.clone();
    let register =
        tokio::spawn(async move { registering.register_shapes(vec![thing], &ctx).await });
    tokio::time::sleep(Duration::from_millis(500)).await;
    let landed_between = register.is_finished();
    gate.release();

    let mounted = tokio::time::timeout(Duration::from_secs(10), mount)
        .await
        .expect("the mount deadlocked")
        .unwrap();
    let registered = tokio::time::timeout(Duration::from_secs(10), register)
        .await
        .expect("register_shapes deadlocked")
        .unwrap();
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    assert!(
        !landed_between,
        "register_shapes ran between the mount's ownership check and its install"
    );
    mounted.unwrap();
    registered.unwrap();
}

// --- A record names only this mount's classes -------------------------------

/// A record whose `class` is a class the app registered, not one the mount
/// installed, writes nothing: no flag or property links of the app's
/// class under the language's address.
#[tokio::test]
async fn a_record_of_a_class_the_mount_did_not_install_writes_nothing() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, ctx) = mounted(&lang).await;
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    p.add_sdna(
        "AppNote".to_string(),
        String::new(),
        SdnaType::SubjectClass,
        Some(
            json!({
                "target_class": "app://AppNote",
                "properties": [
                    {"path": "rdf://type", "name": "type", "has_value": "app://AppNote", "min_count": 1, "max_count": 1},
                    {"path": "app://title", "name": "title", "max_count": 1}
                ]
            })
            .to_string(),
        ),
        &ctx,
    )
    .await
    .unwrap();

    let generation = p.active_mount_generation(&lang).await.unwrap();
    for class in ["AppNote", "app://AppNote"] {
        let delta = crate::languages::mount_types::MountDelta {
            address: Some("t9".to_string()),
            changed: vec![crate::languages::mount_types::MountEntry {
                r#class: class.to_string(),
                address: "t9".to_string(),
                data: json!({"title": "forged"}),
            }],
            removed: vec![],
        };
        let result = p
            .apply_mount_delta(
                &lang,
                &delta,
                generation,
                super::mount_lifecycle::next_mount_seq(),
            )
            .await;
        assert!(
            result.is_err(),
            "a record of {class} was applied: {result:?}"
        );
    }
    let forged = p
        .get_links(&LinkQuery {
            source: Some(format!("{lang}://t9")),
            ..Default::default()
        })
        .await
        .unwrap();
    assert!(
        forged.is_empty(),
        "links of another class were written: {forged:?}"
    );
}
