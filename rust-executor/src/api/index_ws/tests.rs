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

fn own_listing(state: &str, reason: Option<&str>) -> OwnListing {
    OwnListing {
        listing_id: "l1".into(),
        name: "Host".into(),
        description: "d".into(),
        location: "AU".into(),
        url: "https://h.example".into(),
        executor_did: "did:key:z6Mk".into(),
        operator_did: "did:scid:coasys:1:Qm".into(),
        run_by_coasys: false,
        ai_models: vec![ListedModel {
            name: "m".into(),
            kind: "llm".into(),
        }],
        rates: vec![],
        signup: "anyone".into(),
        admits_guests: true,
        free_access: true,
        full: false,
        approval: json!({ "kind": "approved", "since": "2026-11-01" }),
        last_seen_at: "2026-11-20T00:00:00Z".into(),
        version: "0.1".into(),
        state: state.into(),
        reason: reason.map(str::to_string),
    }
}

#[tokio::test]
async fn the_index_answering_a_new_state_updates_the_record_once() {
    init_db();
    // A record from an earlier publish, in another state than the index now answers.
    store_local(&LocalListing {
        listing_id: "l1".into(),
        draft: ListingText {
            name: "Host".into(),
            description: "d".into(),
            location: "AU".into(),
            url: "https://h.example".into(),
        },
        state: "unreachable".into(),
        reason: None,
        published_at: 5,
        rates_hash: "sent".into(),
    })
    .unwrap();
    assert!(observe(&own_listing("listed", None), false).unwrap());
    let first = load_local().unwrap().unwrap();
    assert_eq!((first.state.as_str(), first.published_at), ("listed", 5));
    assert_eq!(first.draft.name, "Host");
    // The same answer again: nothing changed, nothing sent.
    assert!(!observe(&own_listing("listed", None), false).unwrap());
    // The index delisted it: the record follows, and keeps the text and the last send.
    assert!(observe(&own_listing("delisted", Some("charging")), false).unwrap());
    let after = load_local().unwrap().unwrap();
    assert_eq!(after.state, "delisted");
    assert_eq!(after.reason.as_deref(), Some("charging"));
    assert_eq!(after.draft, first.draft);
    assert_eq!(after.rates_hash, first.rates_hash);
    assert_eq!(after.published_at, first.published_at);
    // A publish or a withdraw is a change whatever the record says.
    assert!(observe(&own_listing("delisted", Some("charging")), true).unwrap());
}

#[tokio::test]
async fn status_answers_from_the_record_while_the_index_cannot_be_asked() {
    init_db();
    let backend = crate::db_backend::db_backend();
    backend
        .set_setting(
            LISTING_KEY,
            &serde_json::to_string(&LocalListing {
                listing_id: "l1".into(),
                draft: ListingText {
                    name: "Host".into(),
                    description: String::new(),
                    location: "AU".into(),
                    url: "https://h.example".into(),
                },
                state: "listed".into(),
                reason: None,
                published_at: 1,
                rates_hash: "h".into(),
            })
            .unwrap(),
        )
        .unwrap();
    // An index nothing listens on.
    call(
        "index.configure",
        json!({ "url": "http://127.0.0.1:1/v1/index", "enabled": true }),
        RequestContext::operator_for_test(),
    )
    .await
    .unwrap();
    let status = call(
        "index.status",
        json!({}),
        RequestContext::operator_for_test(),
    )
    .await
    .unwrap();
    assert_eq!(status["local"]["state"], "listed");
    assert_eq!(status["local"]["draft"]["name"], "Host");
    assert!(
        status["indexError"].as_str().is_some_and(|e| !e.is_empty()),
        "{status}"
    );
    assert!(status["listing"].is_null() && status["allowed"].is_null());
    call(
        "index.configure",
        json!({ "url": "", "enabled": false }),
        RequestContext::operator_for_test(),
    )
    .await
    .unwrap();
}

#[tokio::test]
async fn the_republisher_makes_no_call_while_the_index_is_off() {
    init_db();
    call(
        "index.configure",
        json!({ "url": "", "enabled": false }),
        RequestContext::operator_for_test(),
    )
    .await
    .unwrap();
    let mut state = RepublishState {
        pending: Some(PendingChange {
            hash: "x".into(),
            since: 1,
        }),
        ..Default::default()
    };
    republish_tick(&mut state, 10_000_000).await;
    assert_eq!(state.pending, None);
    assert_eq!(state.hold_until, 0);
}

#[test]
fn the_config_files_index_is_the_runtime_config_at_start() {
    init_db();
    seed_config(&IndexConfig {
        url: " https://api.example.org/v1/index ".into(),
        enabled: true,
    })
    .unwrap();
    assert_eq!(
        load_config().unwrap(),
        IndexConfig {
            url: "https://api.example.org/v1/index".into(),
            enabled: true,
        }
    );
    // A URL the client would not use stops the start with a reason.
    let err = seed_config(&IndexConfig {
        url: "http://index.example.org".into(),
        enabled: true,
    })
    .unwrap_err();
    assert!(err.contains("https"), "{err}");
    seed_config(&IndexConfig::default()).unwrap();
    assert!(!load_config().unwrap().enabled);
}
