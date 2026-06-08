//! `GET /v1/models` — list every model registered with the executor.

use axum::Json;

use super::errors::{OpenAIError, OpenAIResult};
use super::types::{ModelExtensions, ModelInfo, ModelListResponse};
use crate::agent::capabilities::{check_capability, AI_READ_CAPABILITY};
use crate::api::auth::AuthContext;
use crate::db::Ad4mDb;
use crate::types::Model;

pub async fn list_models(auth: AuthContext) -> OpenAIResult<Json<ModelListResponse>> {
    check_capability(&auth.capabilities, &AI_READ_CAPABILITY).map_err(OpenAIError::forbidden)?;

    let models = Ad4mDb::with_global_instance(|db| db.get_models())
        .map_err(|e| OpenAIError::internal(format!("Database error listing models: {e}")))?;

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
        created: 0,
        owned_by: "ad4m",
        extensions: ModelExtensions {
            model_type,
            name: m.name,
            backend,
        },
    }
}
