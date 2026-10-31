//! `GET /v1/models` — every model registered with the executor, across its endpoints.
//!
//! The list is the executor's model rows, not the upstreams' listings: a client can only
//! use a model that has a row (billing keys on its name). `id` is the name, `owned_by` the
//! endpoint's name, and the `ad4m` extension says the type, the endpoint, the id the
//! endpoint knows the model by, and where it stands by the endpoint's last health check.

use std::collections::HashMap;

use axum::Json;

use super::chat::epoch_seconds;
use super::errors::{OpenAIError, OpenAIResult};
use super::types::{ModelExtensions, ModelInfo, ModelListResponse};
use crate::agent::capabilities::AI_READ_CAPABILITY;
use crate::db::Ad4mDb;
use crate::types::{Endpoint, EndpointState, Model, ModelState, ModelStatus, RequestContext};

pub async fn list_models(auth: RequestContext) -> OpenAIResult<Json<ModelListResponse>> {
    auth.require(&AI_READ_CAPABILITY)
        .map_err(OpenAIError::forbidden)?;

    let (models, endpoints) = Ad4mDb::with_global_instance(|db| {
        Ok::<_, anyhow::Error>((db.get_models()?, db.get_endpoints()?))
    })
    .map_err(|e| OpenAIError::internal(format!("Database error listing models: {e}")))?;
    let endpoints: HashMap<String, Endpoint> =
        endpoints.into_iter().map(|e| (e.id.clone(), e)).collect();

    let created = epoch_seconds();
    let data = models
        .into_iter()
        .map(|m| {
            let endpoint = m.endpoint_id.as_ref().and_then(|id| endpoints.get(id));
            model_info(m, endpoint, created)
        })
        .collect();
    Ok(Json(ModelListResponse {
        object: "list",
        data,
    }))
}

fn model_info(m: Model, endpoint: Option<&Endpoint>, created: i64) -> ModelInfo {
    let remote_model = m.api.as_ref().map(|api| api.model.clone());
    let status = endpoint.and_then(|e| status_of(e, remote_model.as_deref()?));
    ModelInfo {
        id: m.name,
        object: "model",
        created,
        owned_by: endpoint.map_or_else(|| "ad4m".to_string(), |e| e.name.clone()),
        extensions: ModelExtensions {
            model_type: m.model_type,
            endpoint_id: m.endpoint_id,
            remote_model,
            status,
        },
    }
}

/// Where `remote_model` stands on `endpoint` by its last health check: down when the
/// endpoint is, missing when it answered and did not list the model (Ollama lists `m` as
/// `m:latest`). `None` before a check.
fn status_of(endpoint: &Endpoint, remote_model: &str) -> Option<ModelStatus> {
    let state = match endpoint.health.state {
        EndpointState::Unknown => return None,
        EndpointState::Unreachable | EndpointState::Unauthorized => ModelState::EndpointDown,
        EndpointState::Reachable
            if endpoint.health.served_models.iter().any(|served| {
                served == remote_model || served.strip_suffix(":latest") == Some(remote_model)
            }) =>
        {
            ModelState::Ready
        }
        EndpointState::Reachable => ModelState::Missing,
    };
    Some(ModelStatus {
        state,
        progress: None,
        detail: endpoint.health.detail.clone(),
    })
}
