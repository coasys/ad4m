use super::*;
use crate::agent::capabilities::AGENT_UPDATE_CAPABILITY;
use crate::db::Ad4mDb;
use serde_json::json;

fn init_db() {
    if Ad4mDb::global_instance().lock().unwrap().is_none() {
        Ad4mDb::init_global_instance(":memory:").unwrap();
    }
}

fn map() -> HandlerMap {
    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    map
}

async fn call(method: &str, params: Value, ctx: RequestContext) -> Result<Value, WsRpcError> {
    map().dispatch(method, params, Arc::new(ctx)).await
}

#[tokio::test]
async fn only_the_operator_calls_index_methods() {
    init_db();
    for (method, params) in [
        ("index.configure", json!({ "url": "", "enabled": false })),
        ("index.status", json!({})),
        (
            "index.publish",
            json!({ "name": "n", "description": "", "location": "l", "url": "https://h.example" }),
        ),
        ("index.withdraw", json!({})),
    ] {
        let err = call(
            method,
            params,
            RequestContext::app_for_test(vec![AGENT_UPDATE_CAPABILITY.clone()]),
        )
        .await
        .unwrap_err();
        assert_eq!(err.code, 403, "{method}: {}", err.message);
    }
}

#[tokio::test]
async fn an_index_that_is_off_makes_no_call() {
    init_db();
    let off = call(
        "index.configure",
        json!({ "url": "", "enabled": false }),
        RequestContext::operator_for_test(),
    )
    .await
    .unwrap();
    assert_eq!(off, json!({ "url": "", "enabled": false }));

    let status = call(
        "index.status",
        json!({}),
        RequestContext::operator_for_test(),
    )
    .await
    .unwrap();
    assert_eq!(status["config"]["enabled"], false);
    assert!(status["allowed"].is_null() && status["listing"].is_null());

    for (method, params) in [
        (
            "index.publish",
            json!({ "name": "n", "description": "", "location": "l", "url": "https://h.example" }),
        ),
        ("index.withdraw", json!({})),
    ] {
        let err = call(method, params, RequestContext::operator_for_test())
            .await
            .unwrap_err();
        assert_eq!(err.code, 400, "{method}: {}", err.message);
        assert!(err.message.contains("off"), "{}", err.message);
    }
}

#[tokio::test]
async fn configure_refuses_a_url_the_client_would_not_use() {
    init_db();
    let err = call(
        "index.configure",
        json!({ "url": "http://index.example.org/v1/index", "enabled": true }),
        RequestContext::operator_for_test(),
    )
    .await
    .unwrap_err();
    assert!(err.message.contains("https"), "{}", err.message);
    // Nothing was stored.
    assert!(!load_config().unwrap().enabled);
}

#[tokio::test]
async fn host_info_answers_not_a_host_with_multi_user_mode_off() {
    init_db();
    crate::db_backend::db_backend()
        .set_multi_user_enabled(false)
        .unwrap();
    // Open: an anonymous caller (the index reads it with no credentials) gets the answer.
    let err = call(
        "host.info",
        json!({}),
        RequestContext::for_test(crate::agent::session::Principal::Anonymous),
    )
    .await
    .unwrap_err();
    assert_eq!(err.data.unwrap()["code"], "not-a-host");
}

#[test]
fn host_info_names_the_operator_and_the_executor() {
    let operator = "did:scid:coasys:1:QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG";
    let executor = "did:key:z6MkvDqGT54cXesYGvABpF1UapVNwjCqRcafi4Px6Thv5T3Z";
    let info = serde_json::to_value(host_info_of(operator.into(), executor.into(), true)).unwrap();
    // The fields the hosting index's `fetchHostFacts` reads, with its types.
    assert_eq!(info["operatorDid"], operator);
    assert_eq!(info["executorDid"], executor);
    assert_eq!(info["signup"], "anyone");
    assert_eq!(info["admitsGuests"], true);
    assert_eq!(info["freeAccess"], true);
    assert_eq!(info["full"], false);
    assert!(info["version"].as_str().is_some_and(|v| !v.is_empty()));
    assert!(!host_info_of(operator.into(), executor.into(), false).free_access);
}
