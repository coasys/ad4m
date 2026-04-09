//! Runtime REST endpoints: /api/v1/runtime/*
//!
//! 17 harmonised endpoints covering runtime info, friends, messages, notifications,
//! link language templates, holochain, and import/export.

use axum::{
    extract::{Path, State},
    Json,
};

use crate::agent::capabilities::*;
use crate::agent::AgentService;
use crate::db::Ad4mDb;
use crate::globals::AD4M_VERSION;
use crate::holochain_service::get_holochain_service;
use crate::runtime_service::RuntimeService;
use crate::types::Notification;
use crate::types::*;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;
// Disambiguate: use REST's NotificationInput (has Option<String> for app_icon_path)
use super::types::NotificationInput;

/// GET /runtime/info — combined info + readiness + TLS domain
pub async fn get_runtime_info(
    State(_state): State<AppState>,
    _auth: AuthContext,
) -> Result<Json<RuntimeInfo>, ApiError> {
    let info = AgentService::with_global_instance(|agent_service| {
        agent_service
            .agent
            .clone()
            .ok_or(ApiError::NotFound("Agent not found".into()))?;

        Ok::<RuntimeInfo, ApiError>(RuntimeInfo {
            is_initialized: agent_service.is_initialized(),
            is_unlocked: agent_service.is_unlocked(),
            ad4m_executor_version: AD4M_VERSION.clone(),
        })
    })?;
    Ok(Json(info))
}

/// POST /runtime/quit — quit runtime
pub async fn quit_runtime(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_QUIT_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    // Spawn quit in background so response can be sent
    tokio::spawn(async {
        tokio::time::sleep(std::time::Duration::from_millis(500)).await;
        std::process::exit(0);
    });

    Ok(Json(true))
}

/// PUT /runtime/status — set runtime status
pub async fn set_status(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<SetStatusRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_MY_STATUS_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    // Runtime status setting is not currently supported
    let _ = body;
    Err(ApiError::Internal(
        "Runtime status update not implemented".into(),
    ))
}

/// POST /runtime/open-link — open URL in system browser
pub async fn open_link(
    State(_state): State<AppState>,
    _auth: AuthContext,
    Json(body): Json<OpenLinkRequest>,
) -> Result<Json<bool>, ApiError> {
    #[cfg(target_os = "macos")]
    std::process::Command::new("open")
        .arg(&body.url)
        .spawn()
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    #[cfg(target_os = "linux")]
    std::process::Command::new("xdg-open")
        .arg(&body.url)
        .spawn()
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    #[cfg(target_os = "windows")]
    std::process::Command::new("cmd")
        .args(["/C", "start", &body.url])
        .spawn()
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    Ok(Json(true))
}

/// POST /runtime/export — export db or perspective
pub async fn export_data(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<ExportRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    match body.export_type.as_str() {
        "db" => {
            let json_data = Ad4mDb::with_global_instance(|db| db.export_all_to_json())
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            // SECURITY TODO: constrain file paths to ad4m data directory.
            // This is pre-existing behaviour from the GraphQL mutation.
            let data = std::fs::read_to_string(&body.file_path)
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            let json_data: serde_json::Value =
                serde_json::from_str(&data).map_err(|e| ApiError::BadRequest(e.to_string()))?;
            Ad4mDb::with_global_instance(|db| db.import_from_json(json_data))
                .map_err(|e| ApiError::Internal(e.to_string()))?;
        }
        "perspective" => {
            let data = std::fs::read_to_string(&body.file_path)
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            let instance: crate::perspectives::SerializedPerspective =
                serde_json::from_str(&data).map_err(|e| ApiError::BadRequest(e.to_string()))?;
            crate::perspectives::import_perspective(instance)
                .await
                .map_err(|e| ApiError::Internal(e.to_string()))?;
        }
        other => {
            return Err(ApiError::BadRequest(format!(
                "Unknown import type: {}",
                other
            )))
        }
    }

    Ok(Json(true))
}

/// POST /runtime/holochain/restart — restart Holochain
pub async fn restart_holochain(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    // Only admin can restart holochain
    if !context.is_admin_credential {
        return Err(ApiError::Forbidden("Admin credential required".into()));
    }

    // Restart HC
    let _ = get_holochain_service().await;
    Ok(Json(true))
}

/// GET /runtime/verify-signature — verify signed string
pub async fn verify_signature(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<VerifySignatureRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let result = crate::agent::signatures::verify_string_signed_by_did(
        &body.did,
        &body.data,
        &body.signed_data,
    )
    .map_err(|e| ApiError::Internal(e.to_string()))?;
    Ok(Json(result))
}

// ── Friends & Messages ──

/// GET /friends — list friends
pub async fn list_friends(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_FRIENDS_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let friends = RuntimeService::with_global_instance(|runtime| {
        Ok::<Vec<String>, ApiError>(runtime.get_friends())
    })?;
    Ok(Json(friends))
}

/// GET /friends/:did — friend status
pub async fn get_friend_status(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(did): Path<String>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_FRIEND_STATUS_READ_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    // friend_status not implemented in RuntimeService
    let _ = did;
    Err(ApiError::Internal("Friend status not implemented".into()))
}

/// PUT /friends — add friends
pub async fn add_friends(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<FriendsListRequest>,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_FRIENDS_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let friends = RuntimeService::with_global_instance(|runtime| {
        runtime.add_friend(body.dids);
        Ok::<Vec<String>, ApiError>(runtime.get_friends())
    })?;
    Ok(Json(friends))
}

/// DELETE /friends — remove friends
pub async fn remove_friends(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<FriendsListRequest>,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_FRIENDS_DELETE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let friends = RuntimeService::with_global_instance(|runtime| {
        runtime.remove_friend(body.dids);
        Ok::<Vec<String>, ApiError>(runtime.get_friends())
    })?;
    Ok(Json(friends))
}

/// POST /friends/:did/message — send message to friend
pub async fn send_friend_message(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(did): Path<String>,
    Json(body): Json<FriendSendMessageRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_MESSAGES_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let message_expr: PerspectiveExpression = serde_json::from_str(&body.message)
        .map_err(|e| ApiError::BadRequest(format!("Invalid message format: {}", e)))?;
    RuntimeService::with_global_instance(|runtime| {
        runtime.add_message_to_outbox(SentMessage {
            message: message_expr,
            recipient: did.clone(),
        });
    });

    Ok(Json(true))
}

/// GET /messages/inbox
pub async fn get_inbox(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_MESSAGES_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    // No inbox in RuntimeService; return empty
    Ok(Json(serde_json::json!([])))
}

/// GET /messages/outbox
pub async fn get_outbox(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_MESSAGES_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let outbox = RuntimeService::with_global_instance(|runtime| runtime.get_outbox());

    Ok(Json(serde_json::to_value(outbox).unwrap_or_default()))
}

// ── Notifications ──

/// GET /notifications
pub async fn list_notifications(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<Notification>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let notifications = Ad4mDb::with_global_instance(|db| db.get_notifications())
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(notifications))
}

/// POST /notifications — request install notification
pub async fn create_notification(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<NotificationInput>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let domain_input = crate::types::domain::NotificationInput {
        description: body.description,
        app_name: body.app_name,
        app_url: body.app_url,
        app_icon_path: body.app_icon_path.unwrap_or_default(),
        trigger: body.trigger,
        perspective_ids: body.perspective_ids,
        webhook_url: body.webhook_url,
        webhook_auth: body.webhook_auth,
    };

    Ad4mDb::with_global_instance(|db| db.add_notification(domain_input, None))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// PATCH /notifications/:id — update (including grant)
pub async fn update_notification(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
    Json(body): Json<NotificationInput>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let notification = Notification {
        id: id.clone(),
        description: body.description,
        app_name: body.app_name,
        app_url: body.app_url,
        app_icon_path: body.app_icon_path.unwrap_or_default(),
        trigger: body.trigger,
        perspective_ids: body.perspective_ids,
        webhook_url: body.webhook_url,
        webhook_auth: body.webhook_auth,
        granted: false,
        user_email: None,
    };

    Ad4mDb::with_global_instance(|db| db.update_notification(id, &notification))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// DELETE /notifications/:id — remove
pub async fn delete_notification(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(id): Path<String>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    Ad4mDb::with_global_instance(|db| db.remove_notification(id))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

// ── Link Language Templates ──

/// GET /runtime/link-language-templates
pub async fn get_link_language_templates(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_KNOWN_LINK_LANGUAGES_READ_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let templates = RuntimeService::with_global_instance(|runtime| {
        Ok::<Vec<String>, ApiError>(runtime.get_know_link_languages())
    })?;
    Ok(Json(templates))
}

/// PUT /runtime/link-language-templates
pub async fn add_link_language_templates(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<LinkLanguageTemplatesRequest>,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_KNOWN_LINK_LANGUAGES_CREATE_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let templates = RuntimeService::with_global_instance(|runtime| {
        runtime.add_know_link_language(body.addresses);
        Ok::<Vec<String>, ApiError>(runtime.get_know_link_languages())
    })?;
    Ok(Json(templates))
}

/// DELETE /runtime/link-language-templates
pub async fn remove_link_language_templates(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<LinkLanguageTemplatesRequest>,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_KNOWN_LINK_LANGUAGES_DELETE_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let templates = RuntimeService::with_global_instance(|runtime| {
        runtime.remove_know_link_language(body.addresses);
        Ok::<Vec<String>, ApiError>(runtime.get_know_link_languages())
    })?;
    Ok(Json(templates))
}

// ── Holochain ──

/// GET /runtime/hc/agent-infos
pub async fn get_hc_agent_infos(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_HC_AGENT_INFO_READ_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let hc = get_holochain_service().await;

    let infos = hc
        .agent_infos()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(infos))
}

/// POST /runtime/hc/agent-infos
pub async fn add_hc_agent_infos(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<AddAgentInfosRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_HC_AGENT_INFO_CREATE_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let hc = get_holochain_service().await;

    hc.add_agent_infos(vec![body.agent_infos])
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// GET /runtime/network-metrics
pub async fn get_network_metrics(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(
        &context.capabilities,
        &RUNTIME_HC_AGENT_INFO_READ_CAPABILITY,
    )
    .map_err(|e| ApiError::Forbidden(e))?;

    let hc = get_holochain_service().await;

    let metrics = hc
        .get_network_metrics()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(metrics))
}

pub async fn get_free_hosting_enabled(
    State(_state): State<AppState>,
    _auth: AuthContext,
) -> Result<Json<bool>, ApiError> {
    let enabled = Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled())
        .map_err(|e| ApiError::Internal(e.to_string()))?;
    Ok(Json(enabled))
}

pub async fn set_free_hosting_enabled(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<serde_json::Value>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_QUIT_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let enabled = body["enabled"]
        .as_bool()
        .ok_or_else(|| ApiError::BadRequest("'enabled' boolean required".into()))?;

    Ad4mDb::with_global_instance(|db| db.set_free_hosting_enabled(enabled))
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(enabled))
}

/// POST /runtime/import — import data
pub async fn import_data(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<ImportRequest>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    match body.import_type.as_str() {
        "db" => {
            let data = std::fs::read_to_string(&body.file_path)
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            let json_data: serde_json::Value =
                serde_json::from_str(&data).map_err(|e| ApiError::BadRequest(e.to_string()))?;
            Ad4mDb::with_global_instance(|db| db.import_from_json(json_data))
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            Ok(Json(serde_json::json!({"success": true})))
        }
        "perspective" => {
            let data = std::fs::read_to_string(&body.file_path)
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            let snapshot: serde_json::Value =
                serde_json::from_str(&data).map_err(|e| ApiError::BadRequest(e.to_string()))?;
            // TODO: implement perspective import from snapshot
            Ok(Json(serde_json::json!({"success": true, "snapshot": snapshot})))
        }
        other => Err(ApiError::BadRequest(format!(
            "Unknown import type: {}. Use 'db' or 'perspective'.",
            other
        ))),
    }
}

/// GET /runtime/tls-domain
pub async fn get_tls_domain(
    State(_state): State<AppState>,
    _auth: AuthContext,
) -> Result<Json<Option<String>>, ApiError> {
    let config = crate::config::get_global_config();
    let domain = config.tls.as_ref().map(|t| t.domain.clone());
    Ok(Json(domain))
}

/// GET /runtime/compute-log
pub async fn get_compute_log(
    State(_state): State<AppState>,
    auth: AuthContext,
    axum::extract::Query(params): axum::extract::Query<std::collections::HashMap<String, String>>,
) -> Result<Json<Vec<serde_json::Value>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let user_email = params.get("userEmail").or_else(|| {
        user_email_from_token(context.auth_token.clone()).as_ref().map(|s| s)
    }).cloned();

    let since = params.get("since").cloned();
    let limit = params.get("limit").and_then(|l| l.parse::<usize>().ok());

    let logs = Ad4mDb::with_global_instance(|db| {
        db.get_compute_log(user_email.as_deref(), since.as_deref(), limit)
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(logs))
}
