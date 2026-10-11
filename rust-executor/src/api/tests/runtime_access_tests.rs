//! `runtime.exportData` of one perspective on a multi-user executor. The method is an
//! executor operation (`Guard::Executor`): user sessions never reach it. Apps do, and a perspective export
//! follows the perspective rule: an app exports the main agent's and unowned perspectives,
//! never a user's. The operator exports any.

use serde_json::{json, Value};

use super::multi_user::{expect_code, Executor, RegisteredPerspective};

#[tokio::test]
async fn only_the_operator_exports_a_users_perspective() {
    let executor = Executor::start().await;
    let alice = executor.user("alice");
    let hers = RegisteredPerspective::private(Some(vec![alice.did.clone()]));
    let main_agents = RegisteredPerspective::private(Some(vec![executor.main_agent_did()]));
    executor
        .call(
            "perspective.addLink",
            json!({
                "uuid": hers.uuid,
                "link": {
                    "source": "ad4m://self",
                    "predicate": "ad4m://has_note",
                    "target": "literal:string:alices-private-note",
                },
            }),
            &alice.session,
        )
        .await
        .expect("alice writes to her perspective");

    let dir = tempfile::tempdir().expect("temp dir");
    let path = |file: &str| dir.path().join(file);
    let export = |uuid: &str, file: &str| -> Value {
        json!({
            "type": "perspective",
            "perspectiveUuid": uuid,
            "filePath": path(file).to_str().expect("utf-8 path"),
        })
    };
    let app = executor.main_agent_app();

    expect_code(
        executor
            .call("runtime.exportData", export(&hers.uuid, "app.json"), &app)
            .await,
        403,
        "a main-agent app exporting alice's perspective",
    );
    assert!(!path("app.json").exists());
    expect_code(
        executor
            .call(
                "runtime.exportData",
                export(&hers.uuid, "alice.json"),
                &alice.session,
            )
            .await,
        403,
        "a user session calling an executor operation",
    );
    assert!(!path("alice.json").exists());

    executor
        .call(
            "runtime.exportData",
            export(&main_agents.uuid, "main.json"),
            &app,
        )
        .await
        .expect("a main-agent app exports the main agent's perspective");
    assert!(path("main.json").exists());

    executor
        .call(
            "runtime.exportData",
            export(&hers.uuid, "operator.json"),
            &executor.admin(),
        )
        .await
        .expect("the operator exports alice's perspective");
    let exported = std::fs::read_to_string(path("operator.json")).expect("export file");
    assert!(exported.contains("alices-private-note"), "{exported}");
}
