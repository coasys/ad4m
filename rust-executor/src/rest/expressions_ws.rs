//! Expression WS-native handlers.

use serde_json::Value;
use std::sync::Arc;

use crate::agent::capabilities::*;
use crate::agent::AgentContext;
use crate::helpers::build_expression_rendered;
use crate::languages::LanguageController;
use crate::types::*;

use super::types::*;
use super::ws_handler::{HandlerMap, ParamExt, WsRpcError};

async fn get_expression(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &EXPRESSION_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let url = params.require_str("url")?;
    let raw = params.get("raw").and_then(|v| v.as_bool()).unwrap_or(false);

    let decoded_url = urlencoding::decode(&url)
        .map(|s| s.into_owned())
        .unwrap_or(url);

    let controller = LanguageController::global_instance();
    let parsed = LanguageController::parse_expr_url(&decoded_url);

    if let Ok((lang_address, expression_address)) = parsed {
        let is_literal = lang_address == "literal";
        let is_loaded = is_literal || controller.is_language_loaded(&lang_address).await;

        if is_loaded {
            match controller
                .get_expression(&lang_address, &expression_address)
                .await
            {
                Ok(Some(expr_json)) => {
                    if raw {
                        let json_string = serde_json::to_string(&expr_json).unwrap_or_default();
                        return Ok(Value::String(json_string));
                    } else {
                        let rendered = build_expression_rendered(&expr_json, &lang_address);
                        return Ok(serde_json::to_value(rendered)?);
                    }
                }
                Ok(None) => return Ok(Value::Null),
                Err(e) => {
                    return Err(WsRpcError::internal(format!(
                        "Failed to get expression {}: {}",
                        decoded_url, e
                    )));
                }
            }
        }
    }

    Ok(Value::Null)
}

async fn get_many_expressions(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &EXPRESSION_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: ExpressionManyRequest =
        serde_json::from_value(params).map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let controller = LanguageController::global_instance();
    let mut results: Vec<Option<ExpressionRendered>> = Vec::new();

    for url in &body.urls {
        if let Ok((lang_address, expression_address)) = LanguageController::parse_expr_url(url) {
            let is_literal = lang_address == "literal";
            let is_loaded = is_literal || controller.is_language_loaded(&lang_address).await;

            if is_loaded {
                match controller
                    .get_expression(&lang_address, &expression_address)
                    .await
                {
                    Ok(Some(expr_json)) => {
                        results.push(Some(build_expression_rendered(&expr_json, &lang_address)));
                    }
                    Ok(None) => results.push(None),
                    Err(_) => results.push(None),
                }
            } else {
                results.push(None);
            }
        } else {
            results.push(None);
        }
    }

    Ok(serde_json::to_value(results)?)
}

async fn create_expression(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &EXPRESSION_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: CreateExpressionRequest =
        serde_json::from_value(params).map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let controller = LanguageController::global_instance();
    let content_json: serde_json::Value =
        serde_json::from_str(&body.content).unwrap_or(Value::String(body.content));
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());

    let url = controller
        .expression_create(&body.language_address, content_json, &agent_context)
        .await
        .map_err(|e| {
            WsRpcError::internal(format!(
                "Failed to create expression on {}: {}",
                body.language_address, e
            ))
        })?;

    Ok(Value::String(url))
}

async fn get_interactions(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &EXPRESSION_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let url = params.require_str("url")?;
    let decoded_url = urlencoding::decode(&url)
        .map(|s| s.into_owned())
        .unwrap_or(url);

    let controller = LanguageController::global_instance();
    if let Ok((lang_address, _)) = LanguageController::parse_expr_url(&decoded_url) {
        if controller.is_language_loaded(&lang_address).await {
            let interactions = controller
                .expression_interactions(&decoded_url)
                .await
                .map_err(|e| {
                    WsRpcError::internal(format!(
                        "Failed to get expression interactions for {}: {}",
                        decoded_url, e
                    ))
                })?;
            return Ok(serde_json::to_value(interactions)?);
        }
    }

    Ok(serde_json::to_value(Vec::<InteractionMeta>::new())?)
}

async fn interact_expression(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &EXPRESSION_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let url = params.require_str("url")?;
    let body: InteractionCallWrapper = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let decoded_url = urlencoding::decode(&url)
        .map(|s| s.into_owned())
        .unwrap_or(url.clone());

    let controller = LanguageController::global_instance();
    if let Ok((lang_address, _)) = LanguageController::parse_expr_url(&decoded_url) {
        if controller.is_language_loaded(&lang_address).await {
            match controller
                .expression_interact(&decoded_url, &body.interaction_call)
                .await
            {
                Ok(Some(result)) => return Ok(Value::String(result)),
                Ok(None) => return Ok(Value::String("null".to_string())),
                Err(e) => {
                    return Err(WsRpcError::internal(format!(
                        "expression_interact failed for {}: {}",
                        decoded_url, e
                    )));
                }
            }
        }
    }

    Err(WsRpcError::not_found(format!(
        "Language not loaded for expression URL: {}",
        decoded_url
    )))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.register("expression.get", get_expression);
    map.register("expression.getMany", get_many_expressions);
    map.register("expression.create", create_expression);
    map.register("expression.interactions", get_interactions);
    map.register("expression.interact", interact_expression);
}
