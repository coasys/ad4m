//! Language WS-native handlers.

use serde_json::Value;
use std::sync::Arc;

use crate::agent::capabilities::*;
use crate::languages::LanguageController;
use crate::types::*;

use super::types::*;
use super::ws_handler::{HandlerMap, ParamExt, WsRpcError};

async fn list_languages(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &LANGUAGE_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let filter = params.opt_str("filter");
    let controller = LanguageController::global_instance();
    let refs = controller.get_installed_languages(filter.as_deref()).await;

    let mut handles = Vec::new();
    for lang_ref in refs {
        let settings = controller.get_settings_public(&lang_ref.address).await;
        let settings_str = if settings.is_null() {
            None
        } else {
            Some(serde_json::to_string(&settings).unwrap_or_default())
        };

        handles.push(LanguageHandle {
            address: lang_ref.address,
            name: lang_ref.name,
            settings: settings_str,
            constructor_icon: None,
            icon: None,
            settings_icon: None,
        });
    }
    Ok(serde_json::to_value(handles)?)
}

async fn get_language(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &LANGUAGE_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let address = params.require_str("address")?;
    let controller = LanguageController::global_instance();

    if !controller.is_language_loaded(&address).await {
        controller.language_by_ref(&address).await.map_err(|e| {
            let msg = match &e {
                crate::languages::error::LanguageError::LoadError { message, .. } => {
                    message.clone()
                }
                other => other.to_string(),
            };
            WsRpcError::internal(msg)
        })?;
    }

    if controller.is_language_loaded(&address).await {
        let name = controller.get_language_name(&address).await;
        let settings = controller.get_settings_public(&address).await;
        let settings_str = if settings.is_null() {
            None
        } else {
            Some(serde_json::to_string(&settings).unwrap_or_default())
        };

        let (constructor_icon_json, icon_json, settings_icon_json) =
            controller.get_language_icons(&address).await;

        let constructor_icon =
            constructor_icon_json.and_then(|j| serde_json::from_str::<Icon>(&j).ok());
        let icon = icon_json.and_then(|j| serde_json::from_str::<Icon>(&j).ok());
        let settings_icon = settings_icon_json.and_then(|j| serde_json::from_str::<Icon>(&j).ok());

        return Ok(serde_json::to_value(LanguageHandle {
            address,
            name,
            settings: settings_str,
            constructor_icon,
            icon,
            settings_icon,
        })?);
    }

    Err(WsRpcError::not_found(format!(
        "Language not loaded: {}",
        address
    )))
}

async fn get_language_meta(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &LANGUAGE_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let address = params.require_str("address")?;
    let controller = LanguageController::global_instance();
    let meta = controller
        .get_language_expression(&address)
        .await
        .map_err(|e| {
            WsRpcError::internal(format!(
                "Failed to get language meta for {}: {}",
                address, e
            ))
        })?;

    Ok(serde_json::to_value(meta)?)
}

async fn get_language_source(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &LANGUAGE_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let address = params.require_str("address")?;
    let controller = LanguageController::global_instance();
    let source = controller
        .get_language_source(&address)
        .await
        .map_err(|e| {
            WsRpcError::internal(format!(
                "Failed to get language source for {}: {}",
                address, e
            ))
        })?;

    Ok(Value::String(source))
}

async fn publish_language(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &LANGUAGE_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: PublishLanguageRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let controller = LanguageController::global_instance();

    let bundle = std::fs::read_to_string(&body.language_path)
        .map_err(|e| WsRpcError::internal(format!("Failed to read language bundle: {}", e)))?;

    let (hash, _bundle_path) = controller
        .save_language_bundle(&bundle, None)
        .map_err(|e| WsRpcError::internal(format!("Failed to save language bundle: {}", e)))?;

    let meta = LanguageMeta {
        name: body.language_meta.name.clone(),
        address: hash.clone(),
        description: Some(body.language_meta.description.clone()),
        possible_template_params: body.language_meta.possible_template_params.clone(),
        source_code_link: body.language_meta.source_code_link.clone(),
        ..LanguageMeta::default()
    };

    let language_language_address = {
        let sys = controller.system_addresses.lock().await;
        sys.language_language
            .clone()
            .ok_or_else(|| WsRpcError::internal("Language language not loaded"))?
    };

    let language_input = LanguageLanguageInput {
        bundle: bundle.clone(),
        meta: meta.clone(),
    };

    let content = serde_json::to_value(&language_input)
        .map_err(|e| WsRpcError::internal(format!("Failed to serialize language input: {}", e)))?;

    let agent_context = crate::agent::AgentContext::main_agent();
    controller
        .expression_create(&language_language_address, content, &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(format!("Failed to publish language: {}", e)))?;

    let bundle_on_disk = crate::utils::languages_directory()
        .join(&hash)
        .join("bundle.js");
    if bundle_on_disk.exists() {
        if let Err(e) = controller.load_language(bundle_on_disk, false).await {
            log::warn!("Failed to load published language into runtime: {}", e);
        }
    }

    let response_meta = LanguageMeta {
        author: crate::agent::did(),
        templated: Some(false),
        ..meta
    };

    Ok(serde_json::to_value(response_meta)?)
}

async fn apply_template(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &LANGUAGE_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: ApplyTemplateRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let controller = LanguageController::global_instance();

    let language_language_loaded = {
        let sys = controller.system_addresses.lock().await;
        sys.language_language.is_some()
    };

    if !language_language_loaded {
        return Err(WsRpcError::internal(
            "Language language not loaded - cannot apply template and publish",
        ));
    }

    let template_map: serde_json::Map<String, serde_json::Value> =
        serde_json::from_str(&body.template_data)
            .map_err(|e| WsRpcError::bad_request(format!("Invalid template_data JSON: {}", e)))?;

    let input = controller
        .language_apply_template_on_source(&body.source_language_hash, template_map)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let input_name = input.meta.name.clone();

    let (hash, _bundle_path) = controller
        .save_language_bundle(&input.bundle, None)
        .map_err(|e| {
            WsRpcError::internal(format!("Failed to save templated language bundle: {}", e))
        })?;

    let language_language_address = {
        let sys = controller.system_addresses.lock().await;
        sys.language_language
            .clone()
            .ok_or_else(|| WsRpcError::internal("Language language address not available"))?
    };

    let content = serde_json::to_value(&input)
        .map_err(|e| WsRpcError::internal(format!("Failed to serialize language input: {}", e)))?;

    let agent_context = crate::agent::AgentContext::main_agent();
    controller
        .expression_create(&language_language_address, content, &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(format!("Failed to publish language: {}", e)))?;

    let bundle_on_disk = crate::utils::languages_directory()
        .join(&hash)
        .join("bundle.js");
    if bundle_on_disk.exists() {
        if let Err(e) = controller.load_language(bundle_on_disk, false).await {
            log::warn!("Failed to load templated language into runtime: {}", e);
        }
    }

    controller.set_language_name(&hash, &input_name).await;

    Ok(serde_json::to_value(LanguageRef {
        address: hash,
        name: input_name,
    })?)
}

async fn remove_language(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &LANGUAGE_DELETE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let address = params.require_str("address")?;
    let mut controller = LanguageController::global_instance();
    controller
        .language_remove(&address)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn write_settings(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    check_capability(&ctx.capabilities, &LANGUAGE_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let address = params.require_str("address")?;
    let body: LanguageSettingsWrapper = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let controller = LanguageController::global_instance();
    if !controller.is_language_loaded(&address).await {
        return Err(WsRpcError::not_found(format!(
            "Language not loaded: {}",
            address
        )));
    }

    controller
        .write_settings(&address, body.settings)
        .await
        .map_err(|e| WsRpcError::internal(format!("Failed to write settings: {}", e)))?;

    controller.reload_language(&address).await.map_err(|e| {
        WsRpcError::internal(format!(
            "Failed to reload language after settings change: {}",
            e
        ))
    })?;

    Ok(Value::Bool(true))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.register("language.all", list_languages);
    map.register("language.get", get_language);
    map.register("language.meta", get_language_meta);
    map.register("language.source", get_language_source);
    map.register("language.publish", publish_language);
    map.register("language.applyTemplate", apply_template);
    map.register("language.remove", remove_language);
    map.register("language.writeSettings", write_settings);
}
