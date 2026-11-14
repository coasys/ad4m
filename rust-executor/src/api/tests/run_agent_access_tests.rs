//! `perspective.runAgent`'s library: the second perspective the template tools may act in.
//!
//! The method's guard checks the perspective the session works in (`uuid`). The handler runs the
//! same perspective guard on `library` itself — every run, since it is a request field, not a
//! stored setting — and it runs before the handler's own checks (compute credits), so a call that
//! fails both gets the access answer: 404 for another user's perspective, as for a missing one,
//! and 403 for a caller whose capability does not cover the library.

use serde_json::json;

use super::multi_user::{expect_code, Executor, RegisteredPerspective};
use crate::agent::capabilities::{perspective_query_capability, perspective_update_capability};
use crate::db_backend::db_backend;
use crate::types::RequestContext;
use std::sync::Arc;

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
async fn a_user_cannot_name_another_users_perspective_as_the_library() {
    let executor = Executor::start().await;
    let alice = executor.user("alice");
    let bob = executor.user("bob");
    let alices = RegisteredPerspective::private(Some(vec![alice.did.clone()]));
    let alices_library = RegisteredPerspective::private(Some(vec![alice.did.clone()]));
    let bobs = RegisteredPerspective::private(Some(vec![bob.did.clone()]));

    // With free hosting off, a new user has no credits: a call that passes every access check
    // stops at the credits.
    let _paid = PaidHosting::on();
    for write_tools in [json!([]), json!(["template_create"])] {
        for (library, expected) in [(&alices_library.uuid, 403), (&bobs.uuid, 404)] {
            expect_code(
                executor
                    .call(
                        "perspective.runAgent",
                        json!({
                            "uuid": alices.uuid,
                            "library": library,
                            "messages": [],
                            "writeTools": write_tools,
                        }),
                        &alice.session,
                    )
                    .await,
                expected,
                &format!("library {library}, writeTools {write_tools}"),
            );
        }
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn a_caller_without_update_capability_on_the_library_is_refused() {
    let executor = Executor::start().await;
    // Carol's did, so the users row the credits check reads exists; scoped sessions below.
    let carol = executor.user("carol");
    let did = carol.did;
    // The caller owns both perspectives and may write the space; the library it may only read.
    let space = RegisteredPerspective::private(Some(vec![did.clone()]));
    let library = RegisteredPerspective::private(Some(vec![did.clone()]));
    let session = Arc::new(RequestContext::user_for_test(
        &did,
        vec![
            perspective_update_capability(vec![space.uuid.clone()]),
            perspective_query_capability(vec![library.uuid.clone()]),
        ],
    ));

    let _paid = PaidHosting::on();
    let refused = executor
        .call(
            "perspective.runAgent",
            json!({
                "uuid": space.uuid,
                "library": library.uuid,
                "messages": [],
                "writeTools": ["template_create"],
            }),
            &session,
        )
        .await;
    // 403, not the credits refusal the same code would be without the library check.
    let err = expect_code(refused, 403, "runAgent with a read-only library");
    assert!(err.message.contains("Capability is not matched"), "{err:?}");

    // Held to the library the call passes access and stops at the credits.
    let session = Arc::new(RequestContext::user_for_test(
        &did,
        vec![
            perspective_update_capability(vec![space.uuid.clone()]),
            perspective_update_capability(vec![library.uuid.clone()]),
        ],
    ));
    let credits = executor
        .call(
            "perspective.runAgent",
            json!({
                "uuid": space.uuid,
                "library": library.uuid,
                "messages": [],
                "writeTools": ["template_create"],
            }),
            &session,
        )
        .await;
    let err = expect_code(credits, 403, "runAgent with a writable library");
    assert!(
        err.message.contains("Insufficient compute credits"),
        "{err:?}"
    );
}
