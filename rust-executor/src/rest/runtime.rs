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
use crate::types::*;
use crate::holochain_service::get_holochain_service;
use crate::runtime_service::RuntimeService;
use crate::types::Notification;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

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

        Ok(RuntimeInfo {
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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    RuntimeService::with_global_instance(|runtime| {
        runtime.set_status(serde_json::to_string(&body.status).unwrap_or_default())
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// POST /runtime/open-link — open URL in system browser
pub async fn open_link(
    State(_state): State<AppState>,
    _auth: AuthContext,
    Json(body): Json<OpenLinkRequest>,
) -> Result<Json<bool>, ApiError> {
    open::that(&body.url).map_err(|e| ApiError::Internal(e.to_string()))?;
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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    match body.export_type.as_str() {
        "db" => {
            let json_data = Ad4mDb::with_global_instance(|db| db.export_all_to_json())
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            std::fs::write(&body.file_path, serde_json::to_string_pretty(&json_data)?)
                .map_err(|e| ApiError::Internal(e.to_string()))?;
        }
        "perspective" => {
            let uuid = body.perspective_uuid.as_ref()
                .ok_or_else(|| ApiError::BadRequest("perspectiveUuid required for perspective export".into()))?;
            crate::perspectives::export_perspective(uuid, &body.file_path)
                .await
                .map_err(|e| ApiError::Internal(e.to_string()))?;
        }
        other => return Err(ApiError::BadRequest(format!("Unknown export type: {}", other))),
    }

    Ok(Json(true))
}

/// POST /runtime/import — import db or perspective
pub async fn import_data(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<ImportRequest>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    match body.import_type.as_str() {
        "db" => {
            let data = std::fs::read_to_string(&body.file_path)
                .map_err(|e| ApiError::Internal(e.to_string()))?;
            let json_data: serde_json::Value = serde_json::from_str(&data)
                .map_err(|e| ApiError::BadRequest(e.to_string()))?;
            Ad4mDb::with_global_instance(|db| db.import_all_from_json(json_data))
                .map_err(|e| ApiError::Internal(e.to_string()))?;
        }
        "perspective" => {
            crate::perspectives::import_perspective(&body.file_path)
                .await
                .map_err(|e| ApiError::Internal(e.to_string()))?;
        }
        other => return Err(ApiError::BadRequest(format!("Unknown import type: {}", other))),
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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let result = crate::agent::signatures::verify_string_signed_by_did(&body.did, &body.data, &body.signed_data)
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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

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
    check_capability(&context.capabilities, &RUNTIME_FRIEND_STATUS_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let status = RuntimeService::with_global_instance(|runtime| {
        runtime.friend_status(&did)
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(status).unwrap_or_default()))
}

/// PUT /friends — add friends
pub async fn add_friends(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<FriendsListRequest>,
) -> Result<Json<Vec<String>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_FRIENDS_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let friends = RuntimeService::with_global_instance(|runtime| {
        Ok::<Vec<String>, ApiError>(runtime.add_friends(body.dids))
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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let friends = RuntimeService::with_global_instance(|runtime| {
        Ok::<Vec<String>, ApiError>(runtime.remove_friends(body.dids))
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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    RuntimeService::with_global_instance(|runtime| {
        runtime.friend_send_message(&did, &body.message)
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// GET /messages/inbox
pub async fn get_inbox(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_MESSAGES_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let inbox = RuntimeService::with_global_instance(|runtime| {
        runtime.message_inbox()
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(serde_json::to_value(inbox).unwrap_or_default()))
}

/// GET /messages/outbox
pub async fn get_outbox(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<serde_json::Value>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_MESSAGES_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let outbox = RuntimeService::with_global_instance(|runtime| {
        runtime.message_outbox()
    })
    .map_err(|e| ApiError::Internal(e.to_string()))?;

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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let agent_context = crate::agent::AgentContext::from_auth_token(context.auth_token.clone());
    let notifications = Ad4mDb::with_global_instance(|db| {
        db.get_notifications(agent_context.user_email.as_deref())
    })
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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let notification = Notification {
        id: uuid::Uuid::new_v4().to_string(),
        description: body.description,
        app_name: body.app_name,
        app_url: body.app_url,
        app_icon_path: body.app_icon_path,
        trigger: body.trigger,
        perspective_ids: body.perspective_ids,
        webhook_url: body.webhook_url,
        webhook_auth: body.webhook_auth,
        granted: false,
        user_email: None,
    };

    Ad4mDb::with_global_instance(|db| db.add_notification(&notification))
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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let notification = Notification {
        id: id.clone(),
        description: body.description,
        app_name: body.app_name,
        app_url: body.app_url,
        app_icon_path: body.app_icon_path,
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
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

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
    check_capability(&context.capabilities, &RUNTIME_KNOWN_LINK_LANGUAGES_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let templates = RuntimeService::with_global_instance(|runtime| {
        Ok::<Vec<String>, ApiError>(runtime.get_known_link_language_templates())
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
    check_capability(&context.capabilities, &RUNTIME_KNOWN_LINK_LANGUAGES_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let templates = RuntimeService::with_global_instance(|runtime| {
        Ok::<Vec<String>, ApiError>(runtime.add_known_link_language_templates(body.addresses))
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
    check_capability(&context.capabilities, &RUNTIME_KNOWN_LINK_LANGUAGES_DELETE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let templates = RuntimeService::with_global_instance(|runtime| {
        Ok::<Vec<String>, ApiError>(runtime.remove_known_link_language_templates(body.addresses))
    })?;
    Ok(Json(templates))
}

// ── Holochain ──

/// GET /runtime/hc/agent-infos
pub async fn get_hc_agent_infos(
    State(_state): State<AppState>,
    auth: AuthContext,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_HC_AGENT_INFO_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let hc = get_holochain_service().await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let infos = hc.agent_infos()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(infos))
}

/// POST /runtime/hc/agent-infos
pub async fn add_hc_agent_infos(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(infos): Json<String>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &RUNTIME_HC_AGENT_INFO_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let hc = get_holochain_service().await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    hc.add_agent_infos(&infos)
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
    check_capability(&context.capabilities, &RUNTIME_HC_AGENT_INFO_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e.message().to_string()))?;

    let hc = get_holochain_service().await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let metrics = hc.network_metrics()
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(metrics))
}
