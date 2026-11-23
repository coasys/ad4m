//! `perspective.mount` and `perspective.syncMount` over RPC. A mount reads
//! the language as the perspective's owner (#1775), so a managed user mounts
//! and syncs its own perspectives as itself.

use std::sync::Arc;

use serde_json::{json, Value};

use crate::agent::capabilities::perspective_update_capability;
use crate::api::tests::support::registered_perspective;
use crate::api::ws_handler::build_handler_map;
use crate::languages::mount_tests::load_mount_test_language;
use crate::types::RequestContext;

async fn call_as(ctx: RequestContext, method: &str, params: Value) -> Result<Value, (u16, String)> {
    build_handler_map()
        .dispatch(method, params, Arc::new(ctx))
        .await
        .map_err(|e| (e.code, e.message))
}

fn update_everywhere() -> Vec<crate::agent::capabilities::Capability> {
    vec![perspective_update_capability(vec!["*".to_string()])]
}

#[tokio::test]
async fn a_managed_users_mount_and_sync_read_the_language_as_that_user() {
    let lang = load_mount_test_language().await;
    let p = registered_perspective(&[]).await;
    let alice = "did:key:z6MkAliceMountUser";
    crate::perspectives::get_perspective(&p.0)
        .unwrap()
        .persisted
        .lock()
        .await
        .add_owner(alice);
    let params = json!({ "uuid": p.0, "languageAddress": lang });

    for method in ["perspective.mount", "perspective.syncMount"] {
        call_as(
            RequestContext::user_for_test(alice, update_everywhere()),
            method,
            params.clone(),
        )
        .await
        .unwrap_or_else(|e| panic!("{method}: {e:?}"));
    }
    let perspective = crate::perspectives::get_perspective(&p.0).unwrap();
    assert_eq!(perspective.mounts().await.unwrap(), vec![lang.clone()]);
    crate::perspectives::unregister_mount(&lang, &p.0);

    let agents =
        crate::languages::mount_tests::interact(&format!("{lang}://project"), "agents", json!({}))
            .await
            .unwrap();
    let snapshots: Vec<&Value> = agents
        .as_array()
        .unwrap()
        .iter()
        .filter(|a| a["what"] == "mountSnapshot")
        .collect();
    assert_eq!(snapshots.len(), 2, "{agents}");
    for read in snapshots {
        assert_eq!(read["agent"], json!(alice), "{agents}");
    }
}

#[tokio::test]
async fn the_main_agent_may_mount_and_sync() {
    let lang = load_mount_test_language().await;
    let p = registered_perspective(&[]).await;
    let params = json!({ "uuid": p.0, "languageAddress": lang });

    let mounted = call_as(
        RequestContext::app_for_test(update_everywhere()),
        "perspective.mount",
        params.clone(),
    )
    .await
    .unwrap();
    assert_eq!(mounted, json!(true));
    call_as(
        RequestContext::app_for_test(update_everywhere()),
        "perspective.syncMount",
        params,
    )
    .await
    .unwrap();

    let perspective = crate::perspectives::get_perspective(&p.0).unwrap();
    assert_eq!(perspective.mounts().await.unwrap(), vec![lang.clone()]);
    crate::perspectives::unregister_mount(&lang, &p.0);
}

#[test]
fn a_mount_and_a_sync_get_the_long_timeout() {
    // A first mount fetches and writes the whole snapshot inside the call,
    // like a sync: on the short timeout a client gives up while it runs.
    let map = build_handler_map();
    for method in ["perspective.mount", "perspective.syncMount"] {
        let spec = map
            .specs()
            .into_iter()
            .find(|s| s.name == method)
            .unwrap_or_else(|| panic!("{method} is not registered"));
        assert!(spec.long, "{method} has the short timeout");
    }
}
