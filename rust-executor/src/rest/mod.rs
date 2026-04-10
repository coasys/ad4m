//! REST API module — `/api/v1/*`
//!
//! Axum-based REST API server.

pub mod agent;
pub mod ai;
pub mod audio_ws;
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

#[cfg(test)]
mod tests;

use crate::Ad4mConfig;
use auth::AppState;
use axum::{
    http::Method,
    response::Json,
    routing::{delete, get, patch, post, put},
    Router,
};
use deno_core::error::AnyError;
use serde_json::json;
use std::net::SocketAddr;
use tower_http::cors::{AllowOrigin, CorsLayer};

/// Build the full REST API router.
pub fn rest_router(state: AppState) -> Router {
    let cors = CorsLayer::new()
        .allow_origin(AllowOrigin::any())
        .allow_methods([
            Method::GET,
            Method::POST,
            Method::PUT,
            Method::PATCH,
            Method::DELETE,
            Method::OPTIONS,
        ])
        .allow_headers(tower_http::cors::Any)
        .expose_headers([
            "Cross-Origin-Embedder-Policy".parse().unwrap(),
            "Cross-Origin-Resource-Policy".parse().unwrap(),
            "Cross-Origin-Opener-Policy".parse().unwrap(),
        ]);

    // Root info endpoint
    let root = Router::new()
        .route(
            "/",
            get(|| async {
                Json(json!({
                    "name": "AD4M Executor",
                    "version": *crate::globals::AD4M_VERSION,
                    "api": "/api/v1",
                    "endpoints": [
                        "/api/v1/agent", "/api/v1/languages", "/api/v1/perspectives",
                        "/api/v1/neighbourhoods", "/api/v1/expressions", "/api/v1/runtime",
                        "/api/v1/users", "/api/v1/hosting", "/api/v1/ai", "/api/v1/events"
                    ]
                }))
            }),
        )
        .route("/health", get(|| async { Json(json!({"status": "ok"})) }));

    root.nest(
        "/api/v1",
        Router::new()
            // ── Agent (19 endpoints) ──
            .route("/agent", get(agent::get_agent))
            .route("/agent/status", get(agent::get_agent_status))
            .route("/agent/is-locked", get(agent::is_locked))
            .route("/agent/apps", get(agent::get_apps))
            .route("/agent/by-did/{did}", get(agent::get_agent_by_did))
            .route("/agent/profile", patch(agent::update_profile))
            .route("/agent/generate", post(agent::generate_agent))
            .route("/agent/import", post(agent::import_agent))
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
            .route(
                "/agent/entanglement-proof-preflight",
                post(agent::entanglement_proof_preflight),
            )
            // ── Languages (6 endpoints) ──
            .route("/languages", get(languages::list_languages))
            .route("/languages/publish", post(languages::publish_language))
            .route(
                "/languages/apply-template",
                post(languages::apply_template_and_publish),
            )
            .route(
                "/languages/{address}",
                get(languages::get_language).delete(languages::remove_language),
            )
            .route(
                "/languages/{address}/meta",
                get(languages::get_language_meta),
            )
            .route(
                "/languages/{address}/source",
                get(languages::get_language_source),
            )
            .route(
                "/languages/{address}/settings",
                put(languages::write_settings),
            )
            // ── Perspectives (15+ endpoints) ──
            .route(
                "/perspectives",
                get(perspectives::list_perspectives).post(perspectives::create_perspective),
            )
            .route(
                "/perspectives/{uuid}",
                get(perspectives::get_perspective_handler)
                    .put(perspectives::update_perspective_handler)
                    .delete(perspectives::delete_perspective),
            )
            .route(
                "/perspectives/{uuid}/snapshot",
                get(perspectives::get_snapshot),
            )
            .route(
                "/perspectives/{uuid}/publish-snapshot",
                post(perspectives::publish_snapshot),
            )
            .route(
                "/perspectives/{uuid}/links",
                get(perspectives::query_links)
                    .post(perspectives::add_link)
                    .put(perspectives::update_link)
                    .delete(perspectives::remove_link),
            )
            .route(
                "/perspectives/{uuid}/links/bulk",
                post(perspectives::add_links_bulk),
            )
            .route(
                "/perspectives/{uuid}/links/remove-bulk",
                post(perspectives::remove_links_bulk),
            )
            .route(
                "/perspectives/{uuid}/links/mutations",
                post(perspectives::link_mutations),
            )
            .route(
                "/perspectives/{uuid}/links/expression",
                post(perspectives::add_link_expression),
            )
            .route(
                "/perspectives/{uuid}/query",
                post(perspectives::query_perspective),
            )
            .route("/perspectives/{uuid}/sdna", post(perspectives::add_sdna))
            .route(
                "/perspectives/{uuid}/commands",
                post(perspectives::execute_commands),
            )
            .route(
                "/perspectives/{uuid}/batch",
                post(perspectives::create_batch),
            )
            .route(
                "/perspectives/{uuid}/batch/commit",
                post(perspectives::commit_batch),
            )
            .route(
                "/perspectives/{uuid}/subscribe-query",
                post(perspectives::subscribe_query),
            )
            .route(
                "/perspectives/{uuid}/subscribe-surreal-query",
                post(perspectives::subscribe_surreal_query),
            )
            .route(
                "/perspectives/{uuid}/keep-alive-query",
                post(perspectives::keep_alive_query),
            )
            .route(
                "/perspectives/{uuid}/keep-alive-surreal-query",
                post(perspectives::keep_alive_surreal_query),
            )
            .route(
                "/perspectives/{uuid}/dispose-query-subscription",
                post(perspectives::dispose_query_subscription),
            )
            .route(
                "/perspectives/{uuid}/dispose-surreal-query-subscription",
                post(perspectives::dispose_surreal_query_subscription),
            )
            .route(
                "/perspectives/{uuid}/create-subject",
                post(perspectives::create_subject),
            )
            .route(
                "/perspectives/{uuid}/get-subject-data",
                post(perspectives::get_subject_data),
            )
            // ── Neighbourhoods (7 endpoints) ──
            .route(
                "/neighbourhoods/join",
                post(neighbourhoods::join_neighbourhood),
            )
            .route(
                "/neighbourhoods/publish",
                post(neighbourhoods::publish_neighbourhood),
            )
            .route(
                "/neighbourhoods/{uuid}/broadcast",
                post(neighbourhoods::send_broadcast),
            )
            .route(
                "/neighbourhoods/{uuid}/signal",
                post(neighbourhoods::send_signal),
            )
            .route(
                "/neighbourhoods/{uuid}/online-status",
                put(neighbourhoods::set_online_status),
            )
            .route(
                "/neighbourhoods/{uuid}/online-agents",
                get(neighbourhoods::online_agents),
            )
            .route(
                "/neighbourhoods/{uuid}/other-agents",
                get(neighbourhoods::other_agents),
            )
            .route(
                "/neighbourhoods/{uuid}/has-telepresence",
                get(neighbourhoods::has_telepresence),
            )
            // ── Expressions (5 endpoints) ──
            .route("/expressions", post(expressions::create_expression))
            .route("/expressions/many", post(expressions::get_many_expressions))
            .route("/expressions/{url}", get(expressions::get_expression))
            .route(
                "/expressions/{url}/interactions",
                get(expressions::get_interactions),
            )
            .route(
                "/expressions/{url}/interact",
                post(expressions::interact_expression),
            )
            // ── Runtime (17+ endpoints) ──
            .route("/runtime/info", get(runtime::get_runtime_info))
            .route("/runtime/quit", post(runtime::quit_runtime))
            .route("/runtime/status", put(runtime::set_status))
            .route("/runtime/open-link", post(runtime::open_link))
            .route("/runtime/export", post(runtime::export_data))
            .route("/runtime/import", post(runtime::import_data))
            .route("/runtime/tls-domain", get(runtime::get_tls_domain))
            .route("/runtime/compute-log", get(runtime::get_compute_log))
            .route(
                "/runtime/holochain/restart",
                post(runtime::restart_holochain),
            )
            .route("/runtime/verify-signature", post(runtime::verify_signature))
            .route(
                "/runtime/friends",
                get(runtime::list_friends)
                    .put(runtime::add_friends)
                    .delete(runtime::remove_friends),
            )
            .route("/runtime/friends/{did}", get(runtime::get_friend_status))
            .route(
                "/runtime/friends/{did}/message",
                post(runtime::send_friend_message),
            )
            .route("/runtime/messages/inbox", get(runtime::get_inbox))
            .route("/runtime/messages/outbox", get(runtime::get_outbox))
            .route(
                "/runtime/notifications",
                get(runtime::list_notifications).post(runtime::create_notification),
            )
            .route(
                "/runtime/notifications/{id}",
                patch(runtime::update_notification).delete(runtime::delete_notification),
            )
            .route(
                "/runtime/link-language-templates",
                get(runtime::get_link_language_templates)
                    .put(runtime::add_link_language_templates)
                    .delete(runtime::remove_link_language_templates),
            )
            .route(
                "/runtime/hc/agent-infos",
                get(runtime::get_hc_agent_infos).post(runtime::add_hc_agent_infos),
            )
            .route(
                "/runtime/network-metrics",
                get(runtime::get_network_metrics),
            )
            .route(
                "/runtime/free-hosting-enabled",
                get(runtime::get_free_hosting_enabled).put(runtime::set_free_hosting_enabled),
            )
            // ── Users (7 endpoints + dev email test) ──
            .route(
                "/users/multi-user-enabled",
                get(users::get_multi_user_enabled).put(users::set_multi_user_enabled),
            )
            .route("/users", get(users::list_users).post(users::create_user))
            .route("/users/{email}/wallet", get(users::get_user_wallet))
            .route("/users/login", post(users::login_user))
            .route("/users/verify-email", post(users::verify_email))
            .route(
                "/users/request-verification",
                post(users::request_verification),
            )
            .route("/dev/email-test", post(users::email_test))
            // ── Hosting (3 endpoints) ──
            .route("/hosting", get(hosting::get_hosting_info))
            .route("/hosting/wallet", get(hosting::get_hosting_wallet))
            .route(
                "/hosting/wallet/history",
                get(hosting::get_hosting_wallet_history),
            )
            .route(
                "/hosting/wallet/hot-wallet-address",
                put(hosting::set_hot_wallet_address),
            )
            .route("/hosting/request-payment", post(hosting::request_payment))
            // ── AI (8+ endpoints) ──
            .route("/ai/models", get(ai::list_models).post(ai::add_model))
            .route(
                "/ai/models/{id}",
                put(ai::update_model).delete(ai::remove_model),
            )
            .route("/ai/models/{id}/default", put(ai::set_default_model))
            .route("/ai/models/default", get(ai::get_default_model))
            .route(
                "/ai/model-loading-status",
                get(ai::get_model_loading_status),
            )
            .route("/ai/tasks", get(ai::list_tasks).post(ai::add_task))
            .route(
                "/ai/tasks/{id}",
                put(ai::update_task).delete(ai::remove_task),
            )
            .route("/ai/prompt", post(ai::ai_prompt))
            .route("/ai/embed", post(ai::ai_embed))
            // ── AI Transcription (3 endpoints) ──
            .route(
                "/ai/transcription/open",
                post(ai::open_transcription_stream),
            )
            .route(
                "/ai/transcription/feed",
                post(ai::feed_transcription_stream),
            )
            .route(
                "/ai/transcription/close",
                post(ai::close_transcription_stream),
            )
            // ── WebSocket (1 endpoint) ──
            .route("/ws/audio", get(audio_ws::audio_websocket))
            // ── SSE Events (6 endpoints) ──
            .route("/events/agent", get(events::agent_events))
            .route(
                "/events/perspectives",
                get(events::perspective_lifecycle_events),
            )
            .route(
                "/events/perspectives/{uuid}/links",
                get(events::perspective_link_events),
            )
            .route(
                "/events/neighbourhoods/{uuid}/signals",
                get(events::neighbourhood_signal_events),
            )
            .route("/events/runtime", get(events::runtime_events))
            .route("/events/ai", get(events::ai_events)),
    )
    // ── State + Middleware ──
    .with_state(state)
    .layer(cors)
}

/// Start the REST API server.
pub async fn start_server(config: Ad4mConfig) -> Result<(), AnyError> {
    // Set global SMTP config for email verification
    crate::config::set_smtp_config(config.smtp_config.clone())?;

    let port = config.gql_port.expect("Did not get port");
    let admin_credential = config.admin_credential.clone();
    let auto_permit = config.auto_permit_cap_requests.unwrap_or(false);

    let state = AppState {
        admin_credential: admin_credential.clone(),
        auto_permit_cap_requests: auto_permit,
    };

    let app = rest_router(state);

    if let Some(tls_config) = &config.tls {
        let tls_port = tls_config.tls_port;
        let cert_path = tls_config.cert_file_path.clone();
        let key_path = tls_config.key_file_path.clone();

        log::info!("Starting REST API (HTTP) on 127.0.0.1:{}", port);
        log::info!("Starting REST API (HTTPS) on 0.0.0.0:{}", tls_port);

        let tls_state = AppState {
            admin_credential: admin_credential.clone(),
            auto_permit_cap_requests: auto_permit,
        };
        let tls_app = rest_router(tls_state);

        let rustls_config =
            axum_server::tls_rustls::RustlsConfig::from_pem_file(&cert_path, &key_path)
                .await
                .map_err(|e| deno_core::anyhow::anyhow!("TLS config error: {}", e))?;

        tokio::spawn(async move {
            axum_server::bind_rustls(SocketAddr::from(([0, 0, 0, 0], tls_port)), rustls_config)
                .serve(tls_app.into_make_service())
                .await
                .unwrap();
        });

        let listener =
            tokio::net::TcpListener::bind(SocketAddr::from(([127, 0, 0, 1], port))).await?;
        axum::serve(listener, app.into_make_service()).await?;
    } else {
        let address: [u8; 4] = if config.localhost.unwrap_or(true) {
            [127, 0, 0, 1]
        } else {
            [0, 0, 0, 0]
        };

        let addr = SocketAddr::from((address, port));
        log::info!("REST API server starting on http://{}/api/v1", addr);

        let listener = tokio::net::TcpListener::bind(addr).await?;
        axum::serve(listener, app.into_make_service()).await?;
    }

    Ok(())
}
