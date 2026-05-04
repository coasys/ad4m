//! Runtime WS-native handlers.

use serde_json::Value;
use std::sync::Arc;

use crate::agent::capabilities::*;
use crate::agent::AgentService;
use crate::db::Ad4mDb;
use crate::globals::AD4M_VERSION;
use crate::holochain_service::get_holochain_service;
use crate::runtime_service::RuntimeService;
use crate::types::Notification;
use crate::types::{PerspectiveExpression, RequestContext, RuntimeInfo, SentMessage};

use super::types::{
    AddAgentInfosRequest, ExportRequest, FriendSendMessageRequest, FriendsListRequest,
    ImportRequest, LinkLanguageTemplatesRequest, NotificationGrantRequest, NotificationInput,
    OpenLinkRequest, VerifySignatureRequest,
};
use super::ws_handler::{HandlerMap, ParamExt, WsRpcError};

async fn get_runtime_info(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let info = AgentService::with_global_instance(|agent_service| {
        agent_service
            .agent
            .clone()
            .ok_or(WsRpcError::not_found("Agent not found"))?;

        Ok::<RuntimeInfo, WsRpcError>(RuntimeInfo {
            is_initialized: agent_service.is_initialized(),
            is_unlocked: agent_service.is_unlocked(),
            ad4m_executor_version: AD4M_VERSION.clone(),
        })
    })?;
    Ok(serde_json::to_value(info)?)
}

async fn quit_runtime(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_QUIT_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    tokio::spawn(async {
        tokio::time::sleep(std::time::Duration::from_millis(500)).await;
        std::process::exit(0);
    });

    Ok(Value::Bool(true))
}

async fn set_status(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_MY_STATUS_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let _ = params;
    Err(WsRpcError::internal(
        "Runtime status update not implemented",
    ))
}

async fn open_link(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    if !ctx.is_admin_credential {
        return Err(WsRpcError::forbidden("Admin credential required"));
    }

    let body: OpenLinkRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    if !body.url.starts_with("http://") && !body.url.starts_with("https://") {
        return Err(WsRpcError::bad_request(
            "URL must start with http:// or https://",
        ));
    }

    // Block opening URLs pointing to localhost/private IPs to prevent SSRF
    let url_lower = body.url.to_lowercase();
    let host_part = url_lower
        .trim_start_matches("http://")
        .trim_start_matches("https://");
    let host = host_part.split('/').next().unwrap_or("");
    let host_no_port = host.split(':').next().unwrap_or("");
    if host_no_port == "localhost"
        || host_no_port == "127.0.0.1"
        || host_no_port == "0.0.0.0"
        || host_no_port == "::1"
        || host_no_port.starts_with("10.")
        || host_no_port.starts_with("192.168.")
        || host_no_port.starts_with("169.254.")
    {
        return Err(WsRpcError::bad_request(
            "Cannot open URLs pointing to localhost or private networks",
        ));
    }

    #[cfg(target_os = "macos")]
    std::process::Command::new("open")
        .arg(&body.url)
        .spawn()
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    #[cfg(target_os = "linux")]
    std::process::Command::new("xdg-open")
        .arg(&body.url)
        .spawn()
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    #[cfg(target_os = "windows")]
    std::process::Command::new("cmd")
        .args(["/C", "start", &body.url])
        .spawn()
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn export_data(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: ExportRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    match body.export_type.as_str() {
        "db" => {
            let json_data = Ad4mDb::with_global_instance(|db| db.export_all_to_json())
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
            let json_string = serde_json::to_string_pretty(&json_data)
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
            std::fs::write(&body.file_path, json_string)
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
        }
        "perspective" => {
            let uuid = body.perspective_uuid.as_deref().ok_or_else(|| {
                WsRpcError::bad_request("perspective_uuid required for perspective export")
            })?;
            let perspective = crate::perspectives::get_perspective(uuid)
                .ok_or_else(|| WsRpcError::not_found(format!("Perspective {} not found", uuid)))?;
            let links = perspective
                .get_links(&crate::types::LinkQuery {
                    source: None,
                    target: None,
                    predicate: None,
                    from_date: None,
                    until_date: None,
                    limit: None,
                })
                .await
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
            let snapshot = crate::types::domain::Perspective { links };
            let json_string = serde_json::to_string_pretty(&snapshot)
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
            std::fs::write(&body.file_path, json_string)
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
        }
        other => {
            return Err(WsRpcError::bad_request(format!(
                "Unknown export type: {}",
                other
            )))
        }
    }

    Ok(Value::Bool(true))
}

async fn import_data(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: ImportRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    match body.import_type.as_str() {
        "db" => {
            let data = std::fs::read_to_string(&body.file_path)
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
            let json_data: serde_json::Value =
                serde_json::from_str(&data).map_err(|e| WsRpcError::bad_request(e.to_string()))?;
            let result = Ad4mDb::with_global_instance(|db| db.import_from_json(json_data))
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
            Ok(serde_json::to_value(result).map_err(|e| WsRpcError::internal(e.to_string()))?)
        }
        "perspective" => {
            let data = std::fs::read_to_string(&body.file_path)
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
            let snapshot: serde_json::Value =
                serde_json::from_str(&data).map_err(|e| WsRpcError::bad_request(e.to_string()))?;
            Ok(serde_json::json!({"success": true, "snapshot": snapshot}))
        }
        other => Err(WsRpcError::bad_request(format!(
            "Unknown import type: {}. Use 'db' or 'perspective'.",
            other
        ))),
    }
}

async fn restart_holochain(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    if !ctx.is_admin_credential {
        return Err(WsRpcError::forbidden("Admin credential required"));
    }
    let _ = get_holochain_service().await;
    Ok(Value::Bool(true))
}

async fn verify_signature(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: VerifySignatureRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let result = crate::agent::signatures::verify_string_signed_by_did(
        &body.did,
        &body.data,
        &body.signed_data,
    )
    .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(Value::Bool(result))
}

// ── Friends & Messages ──

async fn list_friends(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_FRIENDS_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let friends = RuntimeService::with_global_instance(|runtime| {
        Ok::<Vec<String>, WsRpcError>(runtime.get_friends())
    })?;
    Ok(serde_json::to_value(friends)?)
}

async fn get_friend_status(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_FRIEND_STATUS_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let _ = params.require_str("did")?;
    Err(WsRpcError::not_implemented("Friend status not implemented"))
}

async fn add_friends(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_FRIENDS_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: FriendsListRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let friends = RuntimeService::with_global_instance(|runtime| {
        runtime.add_friend(body.dids);
        Ok::<Vec<String>, WsRpcError>(runtime.get_friends())
    })?;
    Ok(serde_json::to_value(friends)?)
}

async fn remove_friends(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_FRIENDS_DELETE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: FriendsListRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let friends = RuntimeService::with_global_instance(|runtime| {
        runtime.remove_friend(body.dids);
        Ok::<Vec<String>, WsRpcError>(runtime.get_friends())
    })?;
    Ok(serde_json::to_value(friends)?)
}

async fn send_friend_message(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_MESSAGES_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let did = params.require_str("did")?;
    let body: FriendSendMessageRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let message_expr: PerspectiveExpression = serde_json::from_value(body.message)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid message format: {}", e)))?;
    RuntimeService::with_global_instance(|runtime| {
        runtime.add_message_to_outbox(SentMessage {
            message: message_expr,
            recipient: did.clone(),
        });
    });

    Ok(Value::Bool(true))
}

async fn get_inbox(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_MESSAGES_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;
    Ok(serde_json::json!([]))
}

async fn get_outbox(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_MESSAGES_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let outbox = RuntimeService::with_global_instance(|runtime| runtime.get_outbox());
    Ok(serde_json::to_value(outbox).unwrap_or_default())
}

// ── Notifications ──

async fn list_notifications(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let user_email = user_email_from_token(ctx.auth_token.clone());
    let notifications =
        Ad4mDb::with_global_instance(|db| db.get_notifications_for_user(user_email))
            .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(notifications)?)
}

async fn create_notification(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: NotificationInput = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

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

    let user_email = user_email_from_token(ctx.auth_token.clone());
    let id = RuntimeService::request_install_notification(domain_input, user_email)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String(id))
}

async fn update_notification(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let id = params.require_str("id")?;
    let body: NotificationInput = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

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
        granted: body.granted.unwrap_or(false),
        user_email: None,
    };

    Ad4mDb::with_global_instance(|db| db.update_notification(id, &notification))
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn grant_notification(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    if user_email_from_token(ctx.auth_token.clone()).is_some() {
        return Err(WsRpcError::forbidden(
            "Permission denied: managed users cannot call grantNotification",
        ));
    }

    let id = params.require_str("id")?;
    let body: NotificationGrantRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let notifications = Ad4mDb::with_global_instance(|db| db.get_notifications())
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let existing = notifications
        .iter()
        .find(|n| n.id == id)
        .ok_or_else(|| WsRpcError::not_found(format!("Notification {} not found", id)))?;

    let mut updated = existing.clone();
    updated.granted = body.granted;

    Ad4mDb::with_global_instance(|db| db.update_notification(id, &updated))
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn delete_notification(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let id = params.require_str("id")?;
    Ad4mDb::with_global_instance(|db| db.remove_notification(id))
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

// ── Link Language Templates ──

async fn get_link_language_templates(
    _params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(
        &ctx.capabilities,
        &RUNTIME_KNOWN_LINK_LANGUAGES_READ_CAPABILITY,
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let templates = RuntimeService::with_global_instance(|runtime| {
        Ok::<Vec<String>, WsRpcError>(runtime.get_know_link_languages())
    })?;
    Ok(serde_json::to_value(templates)?)
}

async fn add_link_language_templates(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(
        &ctx.capabilities,
        &RUNTIME_KNOWN_LINK_LANGUAGES_CREATE_CAPABILITY,
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: LinkLanguageTemplatesRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let templates = RuntimeService::with_global_instance(|runtime| {
        runtime.add_know_link_language(body.addresses);
        Ok::<Vec<String>, WsRpcError>(runtime.get_know_link_languages())
    })?;
    Ok(serde_json::to_value(templates)?)
}

async fn remove_link_language_templates(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(
        &ctx.capabilities,
        &RUNTIME_KNOWN_LINK_LANGUAGES_DELETE_CAPABILITY,
    )
    .map_err(|e| WsRpcError::forbidden(e))?;

    let body: LinkLanguageTemplatesRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let templates = RuntimeService::with_global_instance(|runtime| {
        runtime.remove_know_link_language(body.addresses);
        Ok::<Vec<String>, WsRpcError>(runtime.get_know_link_languages())
    })?;
    Ok(serde_json::to_value(templates)?)
}

// ── Holochain ──

async fn get_hc_agent_infos(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_HC_AGENT_INFO_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let hc = get_holochain_service().await;
    let infos = hc
        .agent_infos()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(serde_json::to_value(infos)?)
}

async fn add_hc_agent_infos(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_HC_AGENT_INFO_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: AddAgentInfosRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let hc = get_holochain_service().await;
    hc.add_agent_infos(body.agent_infos)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn get_network_metrics(
    _params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_HC_AGENT_INFO_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let hc = get_holochain_service().await;
    let metrics = hc
        .get_network_metrics()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(Value::String(metrics))
}

async fn get_free_hosting_enabled(
    _params: Value,
    _ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let enabled = Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled())
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    Ok(Value::Bool(enabled))
}

async fn set_free_hosting_enabled(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_QUIT_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let enabled = params
        .get("enabled")
        .and_then(|v| v.as_bool())
        .ok_or_else(|| WsRpcError::bad_request("'enabled' boolean required"))?;

    Ad4mDb::with_global_instance(|db| db.set_free_hosting_enabled(enabled))
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(enabled))
}

async fn get_tls_domain(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    Ok(Value::Null)
}

async fn get_compute_log(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let user_email = params
        .opt_str("userEmail")
        .or_else(|| user_email_from_token(ctx.auth_token.clone()));

    let email = user_email.unwrap_or_default();
    let since = params.opt_str("since");
    let limit = params.get("limit").and_then(|l| l.as_i64()).unwrap_or(100);

    let logs =
        Ad4mDb::with_global_instance(|db| db.get_compute_log(&email, since.as_deref(), limit))
            .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(logs).unwrap_or_default())
}

async fn set_host_rates(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &RUNTIME_QUIT_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;
    let _ = params;
    Err(WsRpcError::not_implemented(
        "PUT /runtime/host-rates is not yet implemented on the server",
    ))
}

async fn get_host_rates(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    Err(WsRpcError::not_implemented(
        "GET /runtime/host-rates is not yet implemented on the server",
    ))
}

// ── Stubs for unyt endpoints ──

async fn stub_not_impl(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    Err(WsRpcError::not_implemented(
        "Not yet implemented on the server",
    ))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.register("runtime.info", get_runtime_info);
    map.register("runtime.quit", quit_runtime);
    map.register("runtime.setStatus", set_status);
    map.register("runtime.openLink", open_link);
    map.register("runtime.exportData", export_data);
    map.register("runtime.importData", import_data);
    map.register("runtime.restartHolochain", restart_holochain);
    map.register("runtime.verifySignature", verify_signature);
    map.register("runtime.tlsDomain", get_tls_domain);
    map.register("runtime.computeLog", get_compute_log);
    // Friends & messages
    map.register("runtime.friends", list_friends);
    map.register("runtime.addFriends", add_friends);
    map.register("runtime.removeFriends", remove_friends);
    map.register("runtime.friendStatus", get_friend_status);
    map.register("runtime.sendFriendMessage", send_friend_message);
    map.register("runtime.inbox", get_inbox);
    map.register("runtime.outbox", get_outbox);
    // Notifications
    map.register("runtime.notifications", list_notifications);
    map.register("runtime.createNotification", create_notification);
    map.register("runtime.updateNotification", update_notification);
    map.register("runtime.grantNotification", grant_notification);
    map.register("runtime.deleteNotification", delete_notification);
    // Link language templates
    map.register("runtime.linkLanguageTemplates", get_link_language_templates);
    map.register(
        "runtime.addLinkLanguageTemplates",
        add_link_language_templates,
    );
    map.register(
        "runtime.removeLinkLanguageTemplates",
        remove_link_language_templates,
    );
    // Holochain
    map.register("runtime.hcAgentInfos", get_hc_agent_infos);
    map.register("runtime.addHcAgentInfos", add_hc_agent_infos);
    map.register("runtime.networkMetrics", get_network_metrics);
    // Hosting flags
    map.register("runtime.freeHostingEnabled", get_free_hosting_enabled);
    map.register("runtime.setFreeHostingEnabled", set_free_hosting_enabled);
    map.register("runtime.hostRates", get_host_rates);
    map.register("runtime.setHostRates", set_host_rates);
    // Unyt stubs
    map.register("runtime.unytAgentKey", stub_not_impl);
    map.register("runtime.unytSendHot", stub_not_impl);
    map.register("runtime.unytWalletBalance", stub_not_impl);
    map.register("runtime.unytWalletHistory", stub_not_impl);
    map.register("runtime.unytVersionInfo", stub_not_impl);
    map.register("runtime.unytHotAgentPubkey", stub_not_impl);
    map.register("runtime.unytMembraneProof", stub_not_impl);
    map.register("runtime.unytReinstallDna", stub_not_impl);
}
