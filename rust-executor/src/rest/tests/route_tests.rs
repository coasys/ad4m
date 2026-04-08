//! Route registration tests — verify all routes are registered with expected methods.
//!
//! These tests use axum's Router + tower::ServiceExt to send requests and verify
//! that the route matching works correctly (e.g. correct method, correct path).
//! We don't test handler logic here — just that routes are wired up.

use axum::http::{Method, Request, StatusCode};
use tower::ServiceExt;

use crate::rest::auth::AppState;
use crate::rest::rest_router;

fn test_state() -> AppState {
    AppState {
        admin_credential: Some("test-admin-token".to_string()),
        auto_permit_cap_requests: false,
    }
}

fn test_router() -> axum::Router {
    rest_router(test_state())
}

/// Send a request to the router and return the status code.
/// We expect handler failures (since no executor is running), but we should NOT
/// get 404/405 for registered routes — those indicate missing route registration.
async fn route_status(method: Method, uri: &str) -> StatusCode {
    let app = test_router();
    let body = if method == Method::POST || method == Method::PUT || method == Method::PATCH {
        axum::body::Body::from("{}")
    } else {
        axum::body::Body::empty()
    };
    let request = Request::builder()
        .method(method)
        .uri(uri)
        .header("content-type", "application/json")
        .header("authorization", "test-admin-token")
        .body(body)
        .unwrap();

    let response = app.oneshot(request).await.unwrap();
    response.status()
}

/// A registered route should NOT return 404 (Not Found) or 405 (Method Not Allowed).
/// It may return 500 (because the executor isn't running) or 400/401/403, which is fine.
fn is_route_registered(status: StatusCode) -> bool {
    status != StatusCode::NOT_FOUND && status != StatusCode::METHOD_NOT_ALLOWED
}

// ── Root ──

#[tokio::test]
async fn root_info_endpoint() {
    let status = route_status(Method::GET, "/").await;
    assert_eq!(status, StatusCode::OK);
}

// ── Agent routes ──

#[tokio::test]
async fn route_get_agent() {
    let s = route_status(Method::GET, "/api/v1/agent").await;
    assert!(is_route_registered(s), "GET /agent returned {}", s);
}

#[tokio::test]
async fn route_get_agent_status() {
    let s = route_status(Method::GET, "/api/v1/agent/status").await;
    assert!(is_route_registered(s), "GET /agent/status returned {}", s);
}

#[tokio::test]
async fn route_agent_is_locked() {
    let s = route_status(Method::GET, "/api/v1/agent/is-locked").await;
    assert!(
        is_route_registered(s),
        "GET /agent/is-locked returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_apps() {
    let s = route_status(Method::GET, "/api/v1/agent/apps").await;
    assert!(is_route_registered(s), "GET /agent/apps returned {}", s);
}

#[tokio::test]
async fn route_get_agent_by_did() {
    let s = route_status(Method::GET, "/api/v1/agent/by-did/did:test:123").await;
    assert!(
        is_route_registered(s),
        "GET /agent/by-did/:did returned {}",
        s
    );
}

#[tokio::test]
async fn route_update_profile() {
    let s = route_status(Method::PATCH, "/api/v1/agent/profile").await;
    assert!(
        is_route_registered(s),
        "PATCH /agent/profile returned {}",
        s
    );
}

#[tokio::test]
async fn route_generate_agent() {
    let s = route_status(Method::POST, "/api/v1/agent/generate").await;
    assert!(
        is_route_registered(s),
        "POST /agent/generate returned {}",
        s
    );
}

#[tokio::test]
async fn route_lock_agent() {
    let s = route_status(Method::POST, "/api/v1/agent/lock").await;
    assert!(is_route_registered(s), "POST /agent/lock returned {}", s);
}

#[tokio::test]
async fn route_unlock_agent() {
    let s = route_status(Method::POST, "/api/v1/agent/unlock").await;
    assert!(is_route_registered(s), "POST /agent/unlock returned {}", s);
}

#[tokio::test]
async fn route_sign_message() {
    let s = route_status(Method::POST, "/api/v1/agent/sign").await;
    assert!(is_route_registered(s), "POST /agent/sign returned {}", s);
}

#[tokio::test]
async fn route_remove_app() {
    let s = route_status(Method::DELETE, "/api/v1/agent/apps/test-id").await;
    assert!(
        is_route_registered(s),
        "DELETE /agent/apps/:id returned {}",
        s
    );
}

#[tokio::test]
async fn route_request_capability() {
    let s = route_status(Method::POST, "/api/v1/agent/auth/request").await;
    assert!(
        is_route_registered(s),
        "POST /agent/auth/request returned {}",
        s
    );
}

#[tokio::test]
async fn route_permit_capability() {
    let s = route_status(Method::POST, "/api/v1/agent/auth/permit").await;
    assert!(
        is_route_registered(s),
        "POST /agent/auth/permit returned {}",
        s
    );
}

#[tokio::test]
async fn route_generate_jwt() {
    let s = route_status(Method::POST, "/api/v1/agent/auth/jwt").await;
    assert!(
        is_route_registered(s),
        "POST /agent/auth/jwt returned {}",
        s
    );
}

#[tokio::test]
async fn route_revoke_token() {
    let s = route_status(Method::DELETE, "/api/v1/agent/auth/token/test-token").await;
    assert!(
        is_route_registered(s),
        "DELETE /agent/auth/token/:token returned {}",
        s
    );
}

#[tokio::test]
async fn route_trusted_agents_get() {
    let s = route_status(Method::GET, "/api/v1/agent/trusted").await;
    assert!(is_route_registered(s), "GET /agent/trusted returned {}", s);
}

#[tokio::test]
async fn route_trusted_agents_put() {
    let s = route_status(Method::PUT, "/api/v1/agent/trusted").await;
    assert!(is_route_registered(s), "PUT /agent/trusted returned {}", s);
}

#[tokio::test]
async fn route_trusted_agents_delete() {
    let s = route_status(Method::DELETE, "/api/v1/agent/trusted").await;
    assert!(
        is_route_registered(s),
        "DELETE /agent/trusted returned {}",
        s
    );
}

#[tokio::test]
async fn route_entanglement_proofs_get() {
    let s = route_status(Method::GET, "/api/v1/agent/entanglement-proofs").await;
    assert!(
        is_route_registered(s),
        "GET /agent/entanglement-proofs returned {}",
        s
    );
}

#[tokio::test]
async fn route_entanglement_proofs_post() {
    let s = route_status(Method::POST, "/api/v1/agent/entanglement-proofs").await;
    assert!(
        is_route_registered(s),
        "POST /agent/entanglement-proofs returned {}",
        s
    );
}

#[tokio::test]
async fn route_entanglement_proofs_delete() {
    let s = route_status(Method::DELETE, "/api/v1/agent/entanglement-proofs").await;
    assert!(
        is_route_registered(s),
        "DELETE /agent/entanglement-proofs returned {}",
        s
    );
}

// ── Language routes ──

#[tokio::test]
async fn route_list_languages() {
    let s = route_status(Method::GET, "/api/v1/languages").await;
    assert!(is_route_registered(s), "GET /languages returned {}", s);
}

#[tokio::test]
async fn route_publish_language() {
    let s = route_status(Method::POST, "/api/v1/languages/publish").await;
    assert!(
        is_route_registered(s),
        "POST /languages/publish returned {}",
        s
    );
}

#[tokio::test]
async fn route_apply_template() {
    let s = route_status(Method::POST, "/api/v1/languages/apply-template").await;
    assert!(
        is_route_registered(s),
        "POST /languages/apply-template returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_language() {
    let s = route_status(Method::GET, "/api/v1/languages/Qm12345").await;
    assert!(
        is_route_registered(s),
        "GET /languages/:address returned {}",
        s
    );
}

#[tokio::test]
async fn route_remove_language() {
    let s = route_status(Method::DELETE, "/api/v1/languages/Qm12345").await;
    assert!(
        is_route_registered(s),
        "DELETE /languages/:address returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_language_meta() {
    let s = route_status(Method::GET, "/api/v1/languages/Qm12345/meta").await;
    assert!(
        is_route_registered(s),
        "GET /languages/:address/meta returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_language_source() {
    let s = route_status(Method::GET, "/api/v1/languages/Qm12345/source").await;
    assert!(
        is_route_registered(s),
        "GET /languages/:address/source returned {}",
        s
    );
}

#[tokio::test]
async fn route_write_settings() {
    let s = route_status(Method::PUT, "/api/v1/languages/Qm12345/settings").await;
    assert!(
        is_route_registered(s),
        "PUT /languages/:address/settings returned {}",
        s
    );
}

// ── Perspective routes ──

#[tokio::test]
async fn route_list_perspectives() {
    let s = route_status(Method::GET, "/api/v1/perspectives").await;
    assert!(is_route_registered(s), "GET /perspectives returned {}", s);
}

#[tokio::test]
async fn route_create_perspective() {
    let s = route_status(Method::POST, "/api/v1/perspectives").await;
    assert!(is_route_registered(s), "POST /perspectives returned {}", s);
}

#[tokio::test]
async fn route_get_perspective() {
    let s = route_status(Method::GET, "/api/v1/perspectives/test-uuid").await;
    assert!(
        is_route_registered(s),
        "GET /perspectives/:uuid returned {}",
        s
    );
}

#[tokio::test]
async fn route_update_perspective() {
    let s = route_status(Method::PUT, "/api/v1/perspectives/test-uuid").await;
    assert!(
        is_route_registered(s),
        "PUT /perspectives/:uuid returned {}",
        s
    );
}

#[tokio::test]
async fn route_delete_perspective() {
    let s = route_status(Method::DELETE, "/api/v1/perspectives/test-uuid").await;
    assert!(
        is_route_registered(s),
        "DELETE /perspectives/:uuid returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_snapshot() {
    let s = route_status(Method::GET, "/api/v1/perspectives/test-uuid/snapshot").await;
    assert!(
        is_route_registered(s),
        "GET /perspectives/:uuid/snapshot returned {}",
        s
    );
}

#[tokio::test]
async fn route_query_links() {
    let s = route_status(Method::GET, "/api/v1/perspectives/test-uuid/links").await;
    assert!(
        is_route_registered(s),
        "GET /perspectives/:uuid/links returned {}",
        s
    );
}

#[tokio::test]
async fn route_mutate_links() {
    let s = route_status(Method::POST, "/api/v1/perspectives/test-uuid/links").await;
    assert!(
        is_route_registered(s),
        "POST /perspectives/:uuid/links returned {}",
        s
    );
}

#[tokio::test]
async fn route_query_perspective() {
    let s = route_status(Method::POST, "/api/v1/perspectives/test-uuid/query").await;
    assert!(
        is_route_registered(s),
        "POST /perspectives/:uuid/query returned {}",
        s
    );
}

#[tokio::test]
async fn route_add_sdna() {
    let s = route_status(Method::POST, "/api/v1/perspectives/test-uuid/sdna").await;
    assert!(
        is_route_registered(s),
        "POST /perspectives/:uuid/sdna returned {}",
        s
    );
}

#[tokio::test]
async fn route_execute_commands() {
    let s = route_status(Method::POST, "/api/v1/perspectives/test-uuid/commands").await;
    assert!(
        is_route_registered(s),
        "POST /perspectives/:uuid/commands returned {}",
        s
    );
}

// ── Neighbourhood routes ──

#[tokio::test]
async fn route_join_neighbourhood() {
    let s = route_status(Method::POST, "/api/v1/neighbourhoods/join").await;
    assert!(
        is_route_registered(s),
        "POST /neighbourhoods/join returned {}",
        s
    );
}

#[tokio::test]
async fn route_publish_neighbourhood() {
    let s = route_status(Method::POST, "/api/v1/neighbourhoods/publish").await;
    assert!(
        is_route_registered(s),
        "POST /neighbourhoods/publish returned {}",
        s
    );
}

#[tokio::test]
async fn route_send_broadcast() {
    let s = route_status(Method::POST, "/api/v1/neighbourhoods/test-uuid/broadcast").await;
    assert!(
        is_route_registered(s),
        "POST /neighbourhoods/:uuid/broadcast returned {}",
        s
    );
}

#[tokio::test]
async fn route_send_signal() {
    let s = route_status(Method::POST, "/api/v1/neighbourhoods/test-uuid/signal").await;
    assert!(
        is_route_registered(s),
        "POST /neighbourhoods/:uuid/signal returned {}",
        s
    );
}

#[tokio::test]
async fn route_set_online_status() {
    let s = route_status(
        Method::PUT,
        "/api/v1/neighbourhoods/test-uuid/online-status",
    )
    .await;
    assert!(
        is_route_registered(s),
        "PUT /neighbourhoods/:uuid/online-status returned {}",
        s
    );
}

#[tokio::test]
async fn route_online_agents() {
    let s = route_status(
        Method::GET,
        "/api/v1/neighbourhoods/test-uuid/online-agents",
    )
    .await;
    assert!(
        is_route_registered(s),
        "GET /neighbourhoods/:uuid/online-agents returned {}",
        s
    );
}

#[tokio::test]
async fn route_other_agents() {
    let s = route_status(Method::GET, "/api/v1/neighbourhoods/test-uuid/other-agents").await;
    assert!(
        is_route_registered(s),
        "GET /neighbourhoods/:uuid/other-agents returned {}",
        s
    );
}

#[tokio::test]
async fn route_has_telepresence() {
    let s = route_status(
        Method::GET,
        "/api/v1/neighbourhoods/test-uuid/has-telepresence",
    )
    .await;
    assert!(
        is_route_registered(s),
        "GET /neighbourhoods/:uuid/has-telepresence returned {}",
        s
    );
}

// ── Expression routes ──

#[tokio::test]
async fn route_create_expression() {
    let s = route_status(Method::POST, "/api/v1/expressions").await;
    assert!(is_route_registered(s), "POST /expressions returned {}", s);
}

#[tokio::test]
async fn route_get_many_expressions() {
    let s = route_status(Method::POST, "/api/v1/expressions/many").await;
    assert!(
        is_route_registered(s),
        "POST /expressions/many returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_expression() {
    let s = route_status(Method::GET, "/api/v1/expressions/lang://Qm123%2F%2Fhash123").await;
    assert!(
        is_route_registered(s),
        "GET /expressions/:url returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_interactions() {
    let s = route_status(Method::GET, "/api/v1/expressions/lang://Qm123/interactions").await;
    assert!(
        is_route_registered(s),
        "GET /expressions/:url/interactions returned {}",
        s
    );
}

#[tokio::test]
async fn route_interact_expression() {
    let s = route_status(Method::POST, "/api/v1/expressions/lang://Qm123/interact").await;
    assert!(
        is_route_registered(s),
        "POST /expressions/:url/interact returned {}",
        s
    );
}

// ── Runtime routes ──

#[tokio::test]
async fn route_runtime_info() {
    let s = route_status(Method::GET, "/api/v1/runtime/info").await;
    assert!(is_route_registered(s), "GET /runtime/info returned {}", s);
}

#[tokio::test]
async fn route_quit_runtime() {
    let s = route_status(Method::POST, "/api/v1/runtime/quit").await;
    assert!(is_route_registered(s), "POST /runtime/quit returned {}", s);
}

#[tokio::test]
async fn route_set_status() {
    let s = route_status(Method::PUT, "/api/v1/runtime/status").await;
    assert!(is_route_registered(s), "PUT /runtime/status returned {}", s);
}

#[tokio::test]
async fn route_open_link() {
    let s = route_status(Method::POST, "/api/v1/runtime/open-link").await;
    assert!(
        is_route_registered(s),
        "POST /runtime/open-link returned {}",
        s
    );
}

#[tokio::test]
async fn route_export_data() {
    let s = route_status(Method::POST, "/api/v1/runtime/export").await;
    assert!(
        is_route_registered(s),
        "POST /runtime/export returned {}",
        s
    );
}

#[tokio::test]
async fn route_import_data() {
    let s = route_status(Method::POST, "/api/v1/runtime/import").await;
    assert!(
        is_route_registered(s),
        "POST /runtime/import returned {}",
        s
    );
}

#[tokio::test]
async fn route_restart_holochain() {
    let s = route_status(Method::POST, "/api/v1/runtime/holochain/restart").await;
    assert!(
        is_route_registered(s),
        "POST /runtime/holochain/restart returned {}",
        s
    );
}

#[tokio::test]
async fn route_verify_signature() {
    let s = route_status(Method::POST, "/api/v1/runtime/verify-signature").await;
    assert!(
        is_route_registered(s),
        "POST /runtime/verify-signature returned {}",
        s
    );
}

#[tokio::test]
async fn route_list_friends() {
    let s = route_status(Method::GET, "/api/v1/runtime/friends").await;
    assert!(
        is_route_registered(s),
        "GET /runtime/friends returned {}",
        s
    );
}

#[tokio::test]
async fn route_add_friends() {
    let s = route_status(Method::PUT, "/api/v1/runtime/friends").await;
    assert!(
        is_route_registered(s),
        "PUT /runtime/friends returned {}",
        s
    );
}

#[tokio::test]
async fn route_remove_friends() {
    let s = route_status(Method::DELETE, "/api/v1/runtime/friends").await;
    assert!(
        is_route_registered(s),
        "DELETE /runtime/friends returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_friend_status() {
    let s = route_status(Method::GET, "/api/v1/runtime/friends/did:test:123").await;
    assert!(
        is_route_registered(s),
        "GET /runtime/friends/:did returned {}",
        s
    );
}

#[tokio::test]
async fn route_send_friend_message() {
    let s = route_status(Method::POST, "/api/v1/runtime/friends/did:test:123/message").await;
    assert!(
        is_route_registered(s),
        "POST /runtime/friends/:did/message returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_inbox() {
    let s = route_status(Method::GET, "/api/v1/runtime/messages/inbox").await;
    assert!(
        is_route_registered(s),
        "GET /runtime/messages/inbox returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_outbox() {
    let s = route_status(Method::GET, "/api/v1/runtime/messages/outbox").await;
    assert!(
        is_route_registered(s),
        "GET /runtime/messages/outbox returned {}",
        s
    );
}

#[tokio::test]
async fn route_list_notifications() {
    let s = route_status(Method::GET, "/api/v1/runtime/notifications").await;
    assert!(
        is_route_registered(s),
        "GET /runtime/notifications returned {}",
        s
    );
}

#[tokio::test]
async fn route_create_notification() {
    let s = route_status(Method::POST, "/api/v1/runtime/notifications").await;
    assert!(
        is_route_registered(s),
        "POST /runtime/notifications returned {}",
        s
    );
}

#[tokio::test]
async fn route_update_notification() {
    let s = route_status(Method::PATCH, "/api/v1/runtime/notifications/test-id").await;
    assert!(
        is_route_registered(s),
        "PATCH /runtime/notifications/:id returned {}",
        s
    );
}

#[tokio::test]
async fn route_delete_notification() {
    let s = route_status(Method::DELETE, "/api/v1/runtime/notifications/test-id").await;
    assert!(
        is_route_registered(s),
        "DELETE /runtime/notifications/:id returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_link_language_templates() {
    let s = route_status(Method::GET, "/api/v1/runtime/link-language-templates").await;
    assert!(
        is_route_registered(s),
        "GET /runtime/link-language-templates returned {}",
        s
    );
}

#[tokio::test]
async fn route_add_link_language_templates() {
    let s = route_status(Method::PUT, "/api/v1/runtime/link-language-templates").await;
    assert!(
        is_route_registered(s),
        "PUT /runtime/link-language-templates returned {}",
        s
    );
}

#[tokio::test]
async fn route_remove_link_language_templates() {
    let s = route_status(Method::DELETE, "/api/v1/runtime/link-language-templates").await;
    assert!(
        is_route_registered(s),
        "DELETE /runtime/link-language-templates returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_hc_agent_infos() {
    let s = route_status(Method::GET, "/api/v1/runtime/hc/agent-infos").await;
    assert!(
        is_route_registered(s),
        "GET /runtime/hc/agent-infos returned {}",
        s
    );
}

#[tokio::test]
async fn route_add_hc_agent_infos() {
    let s = route_status(Method::POST, "/api/v1/runtime/hc/agent-infos").await;
    assert!(
        is_route_registered(s),
        "POST /runtime/hc/agent-infos returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_network_metrics() {
    let s = route_status(Method::GET, "/api/v1/runtime/network-metrics").await;
    assert!(
        is_route_registered(s),
        "GET /runtime/network-metrics returned {}",
        s
    );
}

#[tokio::test]
async fn route_get_free_hosting_enabled() {
    let s = route_status(Method::GET, "/api/v1/runtime/free-hosting-enabled").await;
    assert!(
        is_route_registered(s),
        "GET /runtime/free-hosting-enabled returned {}",
        s
    );
}

#[tokio::test]
async fn route_set_free_hosting_enabled() {
    let s = route_status(Method::PUT, "/api/v1/runtime/free-hosting-enabled").await;
    assert!(
        is_route_registered(s),
        "PUT /runtime/free-hosting-enabled returned {}",
        s
    );
}

// ── User routes ──

#[tokio::test]
async fn route_get_multi_user_enabled() {
    let s = route_status(Method::GET, "/api/v1/users/multi-user-enabled").await;
    assert!(
        is_route_registered(s),
        "GET /users/multi-user-enabled returned {}",
        s
    );
}

#[tokio::test]
async fn route_set_multi_user_enabled() {
    let s = route_status(Method::PUT, "/api/v1/users/multi-user-enabled").await;
    assert!(
        is_route_registered(s),
        "PUT /users/multi-user-enabled returned {}",
        s
    );
}

#[tokio::test]
async fn route_list_users() {
    let s = route_status(Method::GET, "/api/v1/users").await;
    assert!(is_route_registered(s), "GET /users returned {}", s);
}

#[tokio::test]
async fn route_create_user() {
    let s = route_status(Method::POST, "/api/v1/users").await;
    assert!(is_route_registered(s), "POST /users returned {}", s);
}

#[tokio::test]
async fn route_login_user() {
    let s = route_status(Method::POST, "/api/v1/users/login").await;
    assert!(is_route_registered(s), "POST /users/login returned {}", s);
}

#[tokio::test]
async fn route_verify_email() {
    let s = route_status(Method::POST, "/api/v1/users/verify-email").await;
    assert!(
        is_route_registered(s),
        "POST /users/verify-email returned {}",
        s
    );
}

#[tokio::test]
async fn route_email_test() {
    let s = route_status(Method::POST, "/api/v1/dev/email-test").await;
    assert!(
        is_route_registered(s),
        "POST /dev/email-test returned {}",
        s
    );
}

// ── Hosting routes ──

#[tokio::test]
async fn route_get_hosting_info() {
    let s = route_status(Method::GET, "/api/v1/hosting").await;
    assert!(is_route_registered(s), "GET /hosting returned {}", s);
}

#[tokio::test]
async fn route_get_hosting_wallet() {
    let s = route_status(Method::GET, "/api/v1/hosting/wallet").await;
    assert!(is_route_registered(s), "GET /hosting/wallet returned {}", s);
}

#[tokio::test]
async fn route_get_hosting_wallet_history() {
    let s = route_status(Method::GET, "/api/v1/hosting/wallet/history").await;
    assert!(
        is_route_registered(s),
        "GET /hosting/wallet/history returned {}",
        s
    );
}

// ── AI routes ──

#[tokio::test]
async fn route_list_models() {
    let s = route_status(Method::GET, "/api/v1/ai/models").await;
    assert!(is_route_registered(s), "GET /ai/models returned {}", s);
}

#[tokio::test]
async fn route_add_model() {
    let s = route_status(Method::POST, "/api/v1/ai/models").await;
    assert!(is_route_registered(s), "POST /ai/models returned {}", s);
}

#[tokio::test]
async fn route_update_model() {
    let s = route_status(Method::PUT, "/api/v1/ai/models/test-id").await;
    assert!(is_route_registered(s), "PUT /ai/models/:id returned {}", s);
}

#[tokio::test]
async fn route_remove_model() {
    let s = route_status(Method::DELETE, "/api/v1/ai/models/test-id").await;
    assert!(
        is_route_registered(s),
        "DELETE /ai/models/:id returned {}",
        s
    );
}

#[tokio::test]
async fn route_set_default_model() {
    let s = route_status(Method::PUT, "/api/v1/ai/models/test-id/default").await;
    assert!(
        is_route_registered(s),
        "PUT /ai/models/:id/default returned {}",
        s
    );
}

#[tokio::test]
async fn route_list_tasks() {
    let s = route_status(Method::GET, "/api/v1/ai/tasks").await;
    assert!(is_route_registered(s), "GET /ai/tasks returned {}", s);
}

#[tokio::test]
async fn route_add_task() {
    let s = route_status(Method::POST, "/api/v1/ai/tasks").await;
    assert!(is_route_registered(s), "POST /ai/tasks returned {}", s);
}

#[tokio::test]
async fn route_update_task() {
    let s = route_status(Method::PUT, "/api/v1/ai/tasks/test-id").await;
    assert!(is_route_registered(s), "PUT /ai/tasks/:id returned {}", s);
}

#[tokio::test]
async fn route_remove_task() {
    let s = route_status(Method::DELETE, "/api/v1/ai/tasks/test-id").await;
    assert!(
        is_route_registered(s),
        "DELETE /ai/tasks/:id returned {}",
        s
    );
}

#[tokio::test]
async fn route_ai_prompt() {
    let s = route_status(Method::POST, "/api/v1/ai/prompt").await;
    assert!(is_route_registered(s), "POST /ai/prompt returned {}", s);
}

#[tokio::test]
async fn route_ai_embed() {
    let s = route_status(Method::POST, "/api/v1/ai/embed").await;
    assert!(is_route_registered(s), "POST /ai/embed returned {}", s);
}

// ── Event (SSE) routes ──

#[tokio::test]
async fn route_agent_events() {
    let s = route_status(Method::GET, "/api/v1/events/agent").await;
    assert!(is_route_registered(s), "GET /events/agent returned {}", s);
}

#[tokio::test]
async fn route_perspective_lifecycle_events() {
    let s = route_status(Method::GET, "/api/v1/events/perspectives").await;
    assert!(
        is_route_registered(s),
        "GET /events/perspectives returned {}",
        s
    );
}

#[tokio::test]
async fn route_perspective_link_events() {
    let s = route_status(Method::GET, "/api/v1/events/perspectives/test-uuid/links").await;
    assert!(
        is_route_registered(s),
        "GET /events/perspectives/:uuid/links returned {}",
        s
    );
}

#[tokio::test]
async fn route_neighbourhood_signal_events() {
    let s = route_status(
        Method::GET,
        "/api/v1/events/neighbourhoods/test-uuid/signals",
    )
    .await;
    assert!(
        is_route_registered(s),
        "GET /events/neighbourhoods/:uuid/signals returned {}",
        s
    );
}

#[tokio::test]
async fn route_runtime_events() {
    let s = route_status(Method::GET, "/api/v1/events/runtime").await;
    assert!(is_route_registered(s), "GET /events/runtime returned {}", s);
}

#[tokio::test]
async fn route_ai_events() {
    let s = route_status(Method::GET, "/api/v1/events/ai").await;
    assert!(is_route_registered(s), "GET /events/ai returned {}", s);
}

// ── Negative: wrong method should return 405 ──

#[tokio::test]
async fn wrong_method_returns_405() {
    // GET on a POST-only endpoint
    let s = route_status(Method::GET, "/api/v1/agent/generate").await;
    assert_eq!(
        s,
        StatusCode::METHOD_NOT_ALLOWED,
        "GET /agent/generate should be 405"
    );
}

#[tokio::test]
async fn nonexistent_route_returns_404() {
    let s = route_status(Method::GET, "/api/v1/nonexistent").await;
    assert_eq!(s, StatusCode::NOT_FOUND);
}

// ── Root response format ──

#[tokio::test]
async fn root_returns_expected_json() {
    let app = test_router();
    let request = Request::builder()
        .method(Method::GET)
        .uri("/")
        .body(axum::body::Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();
    assert_eq!(response.status(), StatusCode::OK);

    let body = http_body_util::BodyExt::collect(response.into_body())
        .await
        .unwrap()
        .to_bytes();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert_eq!(json["name"], "AD4M Executor");
    assert!(json["api"].as_str().unwrap().contains("v1"));
    assert!(json["endpoints"].is_array());
}
