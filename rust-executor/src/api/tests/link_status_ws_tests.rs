//! The `status` a link write names decides whether the link reaches the neighbourhood. A
//! write must land with the status it asked for in any casing, and a value that is neither
//! `shared` nor `local` must be refused (400) before anything is written: silently storing it
//! as Local kept the link on one executor with no error.

use serde_json::{json, Value};

use crate::api::perspectives_ws::parse_link_status;
use crate::api::tests::support::{admin_ctx, registered_perspective};
use crate::api::ws_handler::build_handler_map;
use crate::types::{LinkQuery, LinkStatus};

#[test]
fn the_known_statuses_parse_in_any_casing() {
    for s in ["shared", "SHARED", "Shared", "sHaReD"] {
        assert_eq!(
            parse_link_status(Some(s)).unwrap(),
            LinkStatus::Shared,
            "{s}"
        );
    }
    for s in ["local", "LOCAL", "Local", "lOcAl"] {
        assert_eq!(
            parse_link_status(Some(s)).unwrap(),
            LinkStatus::Local,
            "{s}"
        );
    }
}

#[test]
fn no_status_is_shared() {
    assert_eq!(parse_link_status(None).unwrap(), LinkStatus::Shared);
}

#[test]
fn an_unknown_status_is_refused_naming_it_and_the_accepted_ones() {
    for s in ["sharde", "", " shared", "private", "LOCALL"] {
        let e = parse_link_status(Some(s)).expect_err(s);
        assert_eq!(e.code, 400, "{s:?}: {}", e.message);
        assert!(
            e.message.contains(&format!("{s:?}")),
            "{s:?}: {}",
            e.message
        );
        assert!(
            e.message.contains("\"shared\"") && e.message.contains("\"local\""),
            "{s:?}: {}",
            e.message
        );
    }
}

async fn call(method: &str, params: Value) -> Result<Value, (u16, String)> {
    build_handler_map()
        .dispatch(method, params, admin_ctx())
        .await
        .map_err(|e| (e.code, e.message))
}

/// The status of every stored link whose source is `source`.
async fn stored_statuses(uuid: &str, source: &str) -> Vec<Option<LinkStatus>> {
    let perspective = crate::perspectives::get_perspective(uuid).expect("registered perspective");
    perspective
        .get_links(&LinkQuery {
            source: Some(source.to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links")
        .into_iter()
        .map(|l| l.status)
        .collect()
}

fn link(source: &str) -> Value {
    json!({ "source": source, "predicate": "status://p", "target": "status://t" })
}

#[tokio::test]
async fn a_write_with_status_spelled_as_the_enum_lands_shared() {
    let p = registered_perspective(&[]).await;

    for (i, status) in ["Shared", "shared", "SHARED", "sHaReD"].iter().enumerate() {
        let source = format!("status://shared/{i}");
        let added = call(
            "perspective.addLink",
            json!({ "uuid": p.0, "link": link(&source), "status": status }),
        )
        .await
        .unwrap_or_else(|e| panic!("addLink with {status:?}: {e:?}"));
        assert_eq!(added["status"], json!("SHARED"), "{status:?}: {added}");
        assert_eq!(
            stored_statuses(&p.0, &source).await,
            vec![Some(LinkStatus::Shared)],
            "{status:?}"
        );
    }

    for (i, status) in ["Local", "local", "LOCAL"].iter().enumerate() {
        let source = format!("status://local/{i}");
        call(
            "perspective.addLinks",
            json!({ "uuid": p.0, "links": [link(&source)], "status": status }),
        )
        .await
        .unwrap_or_else(|e| panic!("addLinks with {status:?}: {e:?}"));
        assert_eq!(
            stored_statuses(&p.0, &source).await,
            vec![Some(LinkStatus::Local)],
            "{status:?}"
        );
    }
}

#[tokio::test]
async fn a_write_with_an_unknown_status_is_refused_and_writes_nothing() {
    let p = registered_perspective(&[]).await;
    let source = "status://typo";

    let writes = [
        (
            "perspective.addLink",
            json!({ "uuid": p.0, "link": link(source), "status": "sharde" }),
        ),
        (
            "perspective.addLinks",
            json!({ "uuid": p.0, "links": [link(source)], "status": "sharde" }),
        ),
        (
            "perspective.linkMutations",
            json!({
                "uuid": p.0,
                "mutations": { "additions": [link(source)], "removals": [] },
                "status": "sharde",
            }),
        ),
    ];
    for (method, params) in writes {
        let (code, message) = call(method, params)
            .await
            .expect_err(&format!("{method} with status \"sharde\" must be refused"));
        assert_eq!(code, 400, "{method}: {message}");
        assert!(message.contains("sharde"), "{method}: {message}");
        assert!(
            message.contains("shared") && message.contains("local"),
            "{method} names the accepted values: {message}"
        );
        assert!(
            stored_statuses(&p.0, source).await.is_empty(),
            "{method} wrote a link"
        );
    }
}
