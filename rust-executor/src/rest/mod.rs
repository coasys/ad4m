//! REST API module — `/api/v1/*`
//!
//! Axum-based REST API running alongside the existing warp GraphQL server.
//! This is the harmonised REST surface that will eventually replace GraphQL.

pub mod agent;
pub mod ai;
pub mod auth;
pub mod errors;
pub mod events;
pub mod expressions;
pub mod hosting;
pub mod languages;
pub mod neighbourhoods;
pub mod perspectives;
pub mod runtime;
pub mod types;
pub mod users;

use auth::AppState;
use axum::{
    http::{HeaderValue, Method},
    routing::{delete, get, patch, post, put},
    Router,
};
use deno_core::error::AnyError;
use std::net::SocketAddr;
use tower_http::cors::{Any, CorsLayer};

/// Build the full REST API router.
pub fn rest_router(state: AppState) -> Router {
    let cors = CorsLayer::new()
        .allow_origin(Any)
        .allow_methods([
            Method::GET,
            Method::POST,
            Method::PUT,
            Method::PATCH,
            Method::DELETE,
            Method::OPTIONS,
        ])
        .allow_headers(Any);

    Router::new()
        // ── Agent (19 endpoints) ──
        .route("/agent", get(agent::get_agent))
        .route("/agent/apps", get(agent::get_apps))
        .route("/agent/by-did/{did}", get(agent::get_agent_by_did))
        .route("/agent/profile", patch(agent::update_profile))
        .route("/agent/generate", post(agent::generate_agent))
        .route("/agent/lock", post(agent::lock_agent))
        .route("/agent/unlock", post(agent::unlock_agent))
        .route("/agent/sign", post(agent::sign_message))
        .route("/agent/apps/{id}", delete(agent::remove_app))
        .route("/agent/auth/request", post(agent::request_capability))
        .route("/agent/auth/permit", post(agent::permit_capability))
        .route("/agent/auth/jwt", post(agent::generate_jwt))
        .route("/agent/auth/token/{token}", delete(agent::revoke_token))
        .route(
            "/agent/trusted",
            get(agent::get_trusted_agents)
                .put(agent::add_trusted_agents)
                .delete(agent::delete_trusted_agents),
        )
        .route(
            "/agent/entanglement-proofs",
            get(agent::get_entanglement)
                .post(agent::add_entanglement)
                .delete(agent::delete_entanglement),
        )
        // ── Languages (6 endpoints) ──
        .route("/languages", get(languages::list_languages))
        .route("/languages/publish", post(languages::publish_language))
        .route("/languages/apply-template", post(languages::apply_template_and_publish))
        .route("/languages/{address}", get(languages::get_language).delete(languages::remove_language))
        .route("/languages/{address}/settings", put(languages::write_settings))
        // ── Perspectives (10 endpoints) ──
        .route("/perspectives", get(perspectives::list_perspectives).post(perspectives::create_perspective))
        .route(
            "/perspectives/{uuid}",
            get(perspectives::get_perspective_handler)
                .put(perspectives::update_perspective_handler)
                .delete(perspectives::delete_perspective),
        )
        .route("/perspectives/{uuid}/links", post(perspectives::mutate_links))
        .route("/perspectives/{uuid}/query", post(perspectives::query_perspective))
        .route("/perspectives/{uuid}/sdna", post(perspectives::add_sdna))
        .route("/perspectives/{uuid}/commands", post(perspectives::execute_commands))
        // ── Neighbourhoods (7 endpoints) ──
        .route("/neighbourhoods/join", post(neighbourhoods::join_neighbourhood))
        .route("/neighbourhoods/publish", post(neighbourhoods::publish_neighbourhood))
        .route("/neighbourhoods/{uuid}/broadcast", post(neighbourhoods::send_broadcast))
        .route("/neighbourhoods/{uuid}/signal", post(neighbourhoods::send_signal))
        .route("/neighbourhoods/{uuid}/online-status", put(neighbourhoods::set_online_status))
        .route("/neighbourhoods/{uuid}/agents", get(neighbourhoods::list_agents))
        .route("/neighbourhoods/{uuid}/telepresence", get(neighbourhoods::has_telepresence))
        // ── Expressions (5 endpoints) ──
        .route("/expressions", post(expressions::create_expression))
        .route("/expressions/many", post(expressions::get_many_expressions))
        .route("/expressions/{url}", get(expressions::get_expression))
        .route("/expressions/{url}/interactions", get(expressions::get_interactions))
        .route("/expressions/{url}/interact", post(expressions::interact_expression))
        // ── Runtime (17 endpoints) ──
        .route("/runtime/info", get(runtime::get_runtime_info))
        .route("/runtime/quit", post(runtime::quit_runtime))
        .route("/runtime/status", put(runtime::set_status))
        .route("/runtime/open-link", post(runtime::open_link))
        .route("/runtime/export", post(runtime::export_data))
        .route("/runtime/import", post(runtime::import_data))
        .route("/runtime/holochain/restart", post(runtime::restart_holochain))
        .route("/runtime/verify-signature", post(runtime::verify_signature))
        .route("/runtime/friends", get(runtime::list_friends).put(runtime::add_friends).delete(runtime::remove_friends))
        .route("/runtime/friends/{did}", get(runtime::get_friend_status))
        .route("/runtime/friends/{did}/message", post(runtime::send_friend_message))
        .route("/runtime/messages/inbox", get(runtime::get_inbox))
        .route("/runtime/messages/outbox", get(runtime::get_outbox))
        .route("/runtime/notifications", get(runtime::list_notifications).post(runtime::create_notification))
        .route("/runtime/notifications/{id}", patch(runtime::update_notification).delete(runtime::delete_notification))
        .route("/runtime/link-language-templates", get(runtime::get_link_language_templates).put(runtime::add_link_language_templates).delete(runtime::remove_link_language_templates))
        .route("/runtime/hc/agent-infos", get(runtime::get_hc_agent_infos).post(runtime::add_hc_agent_infos))
        .route("/runtime/network-metrics", get(runtime::get_network_metrics))
        // ── Users (7 endpoints + dev email test) ──
        .route("/users/multi-user-enabled", get(users::get_multi_user_enabled).put(users::set_multi_user_enabled))
        .route("/users", get(users::list_users).post(users::create_user))
        .route("/users/{email}/wallet", get(users::get_user_wallet))
        .route("/users/login", post(users::login_user))
        .route("/users/verify-email", post(users::verify_email))
        .route("/dev/email-test", post(users::email_test))
        // ── Hosting (3 endpoints) ──
        .route("/hosting", get(hosting::get_hosting_info))
        .route("/hosting/wallet", get(hosting::get_hosting_wallet))
        .route("/hosting/wallet/history", get(hosting::get_hosting_wallet_history))
        // ── AI (8 endpoints) ──
        .route("/ai/models", get(ai::list_models).post(ai::add_model))
        .route("/ai/models/{id}", put(ai::update_model).delete(ai::remove_model))
        .route("/ai/tasks", get(ai::list_tasks).post(ai::manage_task))
        .route("/ai/prompt", post(ai::ai_prompt))
        .route("/ai/embed", post(ai::ai_embed))
        // ── SSE Events (6 endpoints) ──
        .route("/events/agent", get(events::agent_events))
        .route("/events/perspectives", get(events::perspective_lifecycle_events))
        .route("/events/perspectives/{uuid}/links", get(events::perspective_link_events))
        .route("/events/neighbourhoods/{uuid}/signals", get(events::neighbourhood_signal_events))
        .route("/events/runtime", get(events::runtime_events))
        .route("/events/ai", get(events::ai_events))
        // ── State + Middleware ──
        .with_state(state)
        .layer(cors)
}

/// Start the REST API server on gql_port + 1 (transitional: separate port).
pub async fn start_rest_server(
    port: u16,
    admin_credential: Option<String>,
    auto_permit_cap_requests: bool,
) -> Result<(), AnyError> {
    let state = AppState {
        admin_credential,
        auto_permit_cap_requests,
    };

    let app = rest_router(state).into_make_service();

    let addr = SocketAddr::from(([127, 0, 0, 1], port));
    log::info!("REST API server starting on http://{}/api/v1", addr);

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}
