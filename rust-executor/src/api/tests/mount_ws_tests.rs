//! `perspective.mount` and `perspective.syncMount` over RPC. Until ad4m#1775
//! a mount reads the language as the main agent, so only the main agent may
//! mount or sync one.

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
async fn a_managed_user_may_not_mount_or_sync() {
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
        let err = call_as(
            RequestContext::user_for_test(alice, update_everywhere()),
            method,
            params.clone(),
        )
        .await
        .unwrap_err();
        assert_eq!(err.0, 403, "{method}: {err:?}");
        assert!(err.1.contains("main agent"), "{method}: {err:?}");
    }
    let perspective = crate::perspectives::get_perspective(&p.0).unwrap();
    assert!(perspective.mounts().await.unwrap().is_empty());
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
