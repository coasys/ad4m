//! `GET /v1/models` — list every model registered with the executor.

use std::time::SystemTime;

use axum::Json;

use super::errors::{OpenAIError, OpenAIResult};
use super::types::{ModelExtensions, ModelInfo, ModelListResponse};
use crate::agent::capabilities::check_capability;
use crate::api::auth::AuthContext;
use crate::types::Model;

pub async fn list_models(auth: AuthContext) -> OpenAIResult<Json<ModelListResponse>> {
    check_capability(
        &auth.capabilities,
        &crate::services::builtins::capability(
            crate::services::builtins::Builtin::AiModels,
            "READ",
        ),
    )
    .map_err(OpenAIError::forbidden)?;

    let ctx = crate::services::ServiceHost::context_for_request(&auth.to_request_context());
    let models: Vec<Model> = crate::services::builtins::call(
        crate::services::builtins::Builtin::AiModels,
        "models",
        serde_json::json!({}),
        &ctx,
    )
    .await
    .map_err(super::ai_error)?;

    let data = models.into_iter().map(model_to_info).collect();
    Ok(Json(ModelListResponse {
        object: "list",
        data,
    }))
}

/// Map an AD4M [`Model`] to an OpenAI-shaped [`ModelInfo`].  Local vs remote
/// backends are surfaced via the `ad4m.backend` extension so callers that
/// want to prefer local inference can filter on it.
fn model_to_info(m: Model) -> ModelInfo {
    let backend = if m.api.is_some() {
        Some("remote".to_string())
    } else if m.local.is_some() {
        Some("local".to_string())
    } else {
        None
    };
    let model_type = format!("{}", m.model_type).to_lowercase();
    ModelInfo {
        id: m.id,
        object: "model",
        created: SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs() as i64)
            .unwrap_or(0),
        owned_by: "ad4m",
        extensions: ModelExtensions {
            model_type,
            name: m.name,
            backend,
        },
    }
}
