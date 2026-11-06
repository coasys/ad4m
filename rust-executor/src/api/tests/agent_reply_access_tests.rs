//! `perspective.agentReply` on a multi-user executor: the perspective the agent works in.
//!
//! The method's guard checks the perspective that keeps the conversation (`uuid`). The handler
//! runs the same perspective guard on `workIn` itself, and it runs before the handler's own
//! checks (compute credits, the session's settings), so a call that fails both gets the access
//! answer: 404 for another user's perspective, as for a missing one.

use serde_json::json;

use super::multi_user::{expect_code, Executor, RegisteredPerspective};
use crate::db_backend::db_backend;

/// Free hosting off until dropped (it is on by default).
struct PaidHosting;

impl PaidHosting {
    fn on() -> Self {
        db_backend()
            .set_free_hosting_enabled(false)
            .expect("turn free hosting off");
        PaidHosting
    }
}

impl Drop for PaidHosting {
    fn drop(&mut self) {
        let _ = db_backend().set_free_hosting_enabled(true);
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn a_user_cannot_have_the_agent_work_in_another_users_perspective() {
    let executor = Executor::start().await;
    let alice = executor.user("alice");
    let bob = executor.user("bob");
    let alices = RegisteredPerspective::private(Some(vec![alice.did.clone()]));
    let bobs = RegisteredPerspective::private(Some(vec![bob.did.clone()]));

    let call = |work_in: &str, write_tools: serde_json::Value| {
        json!({
            "uuid": alices.uuid,
            "container": "app://chat",
            "workIn": work_in,
            "writeTools": write_tools,
        })
    };

    // With free hosting off, a new user has no credits: a call that passes every access check
    // stops at the credits.
    let _paid = PaidHosting::on();
    expect_code(
        executor
            .call(
                "perspective.agentReply",
                call(&alices.uuid, json!([])),
                &alice.session,
            )
            .await,
        403,
        "Alice's own perspective, no credits",
    );
    // Bob's perspective is refused before the credits are looked at, reading or writing.
    for write_tools in [json!([]), json!(["instance_update"])] {
        expect_code(
            executor
                .call(
                    "perspective.agentReply",
                    call(&bobs.uuid, write_tools.clone()),
                    &alice.session,
                )
                .await,
            404,
            &format!("Bob's perspective as workIn, writeTools {write_tools}"),
        );
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn a_user_cannot_name_another_users_perspective_as_the_library() {
    let executor = Executor::start().await;
    let alice = executor.user("alice");
    let bob = executor.user("bob");
    let alices = RegisteredPerspective::private(Some(vec![alice.did.clone()]));
    let alices_library = RegisteredPerspective::private(Some(vec![alice.did.clone()]));
    let bobs = RegisteredPerspective::private(Some(vec![bob.did.clone()]));

    let _paid = PaidHosting::on();
    for write_tools in [json!([]), json!(["template_create"])] {
        // Alice's own library passes the access checks and stops at the credits.
        for (method, extra, code) in [
            (
                "perspective.agentReply",
                json!({ "container": "app://chat", "workIn": alices.uuid }),
                403,
            ),
            ("perspective.runAgent", json!({ "messages": [] }), 403),
        ] {
            for (library, expected) in [(&alices_library.uuid, code), (&bobs.uuid, 404)] {
                let mut params = extra.clone();
                params["uuid"] = json!(alices.uuid);
                params["library"] = json!(library);
                params["writeTools"] = write_tools.clone();
                expect_code(
                    executor.call(method, params, &alice.session).await,
                    expected,
                    &format!("{method}, library {library}, writeTools {write_tools}"),
                );
            }
        }
        // A session is refused another user's library when it is made, not only per reply.
        expect_code(
            executor
                .call(
                    "perspective.agentSessionCreate",
                    json!({ "uuid": alices.uuid, "title": "t", "workIn": alices.uuid,
                            "library": bobs.uuid, "writeTools": write_tools }),
                    &alice.session,
                )
                .await,
            404,
            &format!("agentSessionCreate, Bob's library, writeTools {write_tools}"),
        );
    }
}
