//! `perspective.createSubject` on a class whose constructor is a `call`
//! answers the URL the language minted, not `true` (design §3.3).

use serde_json::{json, Value};

use crate::api::tests::support::{admin_ctx, registered_perspective};
use crate::api::ws_handler::build_handler_map;
use crate::languages::mount_tests::{load_mount_test_language, mount_thing_shape};

async fn call(method: &str, params: Value) -> Result<Value, (u16, String)> {
    build_handler_map()
        .dispatch(method, params, admin_ctx())
        .await
        .map_err(|e| (e.code, e.message))
}

#[tokio::test]
async fn create_subject_rpc_returns_the_url_for_a_call_constructor() {
    let lang = load_mount_test_language().await;
    let shape = mount_thing_shape(&lang).await;
    let p = registered_perspective(&[("MountThing", shape.as_str())]).await;

    let result = call(
        "perspective.createSubject",
        json!({
            "uuid": p.0,
            "subjectClass": "MountThing",
            "expressionAddress": "ignored",
            "initialValues": "{\"title\":\"a\"}",
        }),
    )
    .await
    .unwrap();

    assert_eq!(result, json!(format!("{lang}://t1")));
}
