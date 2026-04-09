//! Language REST endpoints: /api/v1/languages/*
//!
//! 6 harmonised endpoints.

use axum::{
    extract::{Path, Query, State},
    Json,
};
use std::collections::HashMap;

use crate::agent::capabilities::*;
use crate::languages::LanguageController;
use crate::types::*;

use super::auth::{AppState, AuthContext};
use super::errors::ApiError;
use super::types::*;

/// GET /languages — list (with ?filter= param)
pub async fn list_languages(
    State(_state): State<AppState>,
    auth: AuthContext,
    Query(params): Query<HashMap<String, String>>,
) -> Result<Json<Vec<LanguageHandle>>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let filter = params.get("filter").cloned();
    let controller = LanguageController::global_instance();
    let refs = controller.get_installed_languages(filter.as_deref()).await;

    let mut handles = Vec::new();
    for lang_ref in refs {
        let settings = controller.get_settings_public(&lang_ref.address);
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
    Ok(Json(handles))
}

/// GET /languages/:address — get language handle (includes icons)
pub async fn get_language(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(address): Path<String>,
) -> Result<Json<LanguageHandle>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let controller = LanguageController::global_instance();

    // If not already loaded, try to install/load it
    if !controller.is_language_loaded(&address).await {
        controller.language_by_ref(&address).await.map_err(|e| {
            let msg = match &e {
                crate::languages::error::LanguageError::LoadError { message, .. } => {
                    message.clone()
                }
                other => other.to_string(),
            };
            ApiError::Internal(msg)
        })?;
    }

    if controller.is_language_loaded(&address).await {
        let name = controller.get_language_name(&address).await;
        let settings = controller.get_settings_public(&address);
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

        return Ok(Json(LanguageHandle {
            address,
            name,
            settings: settings_str,
            constructor_icon,
            icon,
            settings_icon,
        }));
    }

    Err(ApiError::NotFound(format!(
        "Language not loaded: {}",
        address
    )))
}

/// GET /languages/:address/meta — get language meta expression
pub async fn get_language_meta(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(address): Path<String>,
) -> Result<Json<LanguageMeta>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let controller = LanguageController::global_instance();
    let meta = controller
        .get_language_expression(&address)
        .await
        .map_err(|e| {
            ApiError::Internal(format!(
                "Failed to get language meta for {}: {}",
                address, e
            ))
        })?;

    Ok(Json(meta))
}

/// GET /languages/:address/source — get language source code
pub async fn get_language_source(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(address): Path<String>,
) -> Result<Json<String>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_READ_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let controller = LanguageController::global_instance();
    let source = controller
        .get_language_source(&address)
        .await
        .map_err(|e| {
            ApiError::Internal(format!(
                "Failed to get language source for {}: {}",
                address, e
            ))
        })?;

    Ok(Json(source))
}

/// POST /languages/publish — publish a language
pub async fn publish_language(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<PublishLanguageRequest>,
) -> Result<Json<LanguageMeta>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let controller = LanguageController::global_instance();

    // SECURITY TODO: validate language_path is within allowed directories (AD4M data dir or known language dirs).
    // This is pre-existing behaviour from the GraphQL mutation.
    let bundle = std::fs::read_to_string(&body.language_path)
        .map_err(|e| ApiError::Internal(format!("Failed to read language bundle: {}", e)))?;

    // Save the bundle locally
    let (hash, _bundle_path) = controller
        .save_language_bundle(&bundle, None)
        .map_err(|e| ApiError::Internal(format!("Failed to save language bundle: {}", e)))?;

    let meta = LanguageMeta {
        name: body.language_meta.name.clone(),
        address: hash.clone(),
        description: Some(body.language_meta.description.clone()),
        possible_template_params: body.language_meta.possible_template_params.clone(),
        source_code_link: body.language_meta.source_code_link.clone(),
        ..LanguageMeta::default()
    };

    // Publish to the language language
    let language_language_address = {
        let sys = controller.system_addresses.lock().await;
        sys.language_language
            .clone()
            .ok_or_else(|| ApiError::Internal("Language language not loaded".into()))?
    };

    let language_input = LanguageLanguageInput {
        bundle: bundle.clone(),
        meta: meta.clone(),
    };

    let content = serde_json::to_value(&language_input)
        .map_err(|e| ApiError::Internal(format!("Failed to serialize language input: {}", e)))?;

    let agent_context = crate::agent::AgentContext::main_agent();
    controller
        .expression_create(&language_language_address, content, &agent_context)
        .await
        .map_err(|e| ApiError::Internal(format!("Failed to publish language: {}", e)))?;

    // Load the language into a per-language runtime
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

    Ok(Json(response_meta))
}

/// POST /languages/apply-template — apply template and publish
pub async fn apply_template_and_publish(
    State(_state): State<AppState>,
    auth: AuthContext,
    Json(body): Json<ApplyTemplateRequest>,
) -> Result<Json<LanguageRef>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_CREATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let controller = LanguageController::global_instance();

    // Check if the language language is loaded
    let language_language_loaded = {
        let sys = controller.system_addresses.lock().await;
        sys.language_language.is_some()
    };

    if !language_language_loaded {
        return Err(ApiError::Internal(
            "Language language not loaded - cannot apply template and publish".into(),
        ));
    }

    let template_map: serde_json::Map<String, serde_json::Value> =
        serde_json::from_str(&body.template_data)
            .map_err(|e| ApiError::BadRequest(format!("Invalid template_data JSON: {}", e)))?;

    let input = controller
        .language_apply_template_on_source(&body.source_language_hash, template_map)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    let input_name = input.meta.name.clone();

    // Save the templated bundle locally
    if let Err(e) = controller.save_language_bundle(&input.bundle, None) {
        log::warn!("Failed to save templated language bundle locally: {}", e);
    }

    let language_language_address = {
        let sys = controller.system_addresses.lock().await;
        sys.language_language.clone().unwrap()
    };

    let input_json = serde_json::to_string(&input)
        .map_err(|e| ApiError::Internal(format!("Failed to serialize language input: {}", e)))?;

    let publish_script = format!(
        r#"await globalThis.__ad4m_language_instance__.expressionAdapter.putAdapter.createPublic({})"#,
        input_json
    );

    let address_raw = controller
        .execute_on_language(&language_language_address, &publish_script)
        .await
        .map_err(|e| ApiError::Internal(format!("Failed to publish language: {}", e)))?;

    let address = address_raw.trim().trim_matches('"').to_string();

    // Load the templated language into a per-language runtime
    let bundle_on_disk = crate::utils::languages_directory()
        .join(&address)
        .join("bundle.js");
    if bundle_on_disk.exists() {
        if let Err(e) = controller.load_language(bundle_on_disk, false).await {
            log::warn!("Failed to load templated language into runtime: {}", e);
        }
    }

    Ok(Json(LanguageRef {
        address,
        name: input_name,
    }))
}

/// DELETE /languages/:address — remove a language
pub async fn remove_language(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(address): Path<String>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_DELETE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let mut controller = LanguageController::global_instance();
    controller
        .language_remove(&address)
        .await
        .map_err(|e| ApiError::Internal(e.to_string()))?;

    Ok(Json(true))
}

/// PUT /languages/:address/settings — write settings
pub async fn write_settings(
    State(_state): State<AppState>,
    auth: AuthContext,
    Path(address): Path<String>,
    Json(body): Json<serde_json::Value>,
) -> Result<Json<bool>, ApiError> {
    let context = auth.to_request_context();
    check_capability(&context.capabilities, &LANGUAGE_UPDATE_CAPABILITY)
        .map_err(|e| ApiError::Forbidden(e))?;

    let controller = LanguageController::global_instance();
    if !controller.is_language_loaded(&address).await {
        return Err(ApiError::NotFound(format!(
            "Language not loaded: {}",
            address
        )));
    }

    let settings_json: serde_json::Value = body;
    controller
        .write_settings(&address, settings_json)
        .await
        .map_err(|e| ApiError::Internal(format!("Failed to write settings: {}", e)))?;

    controller.reload_language(&address).await.map_err(|e| {
        ApiError::Internal(format!(
            "Failed to reload language after settings change: {}",
            e
        ))
    })?;

    Ok(Json(true))
}
