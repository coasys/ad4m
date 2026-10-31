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
use crate::types::{Endpoint, Model, RequestContext};

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
            let endpoint = endpoints.get(&m.endpoint_id);
            model_info(m, endpoint, created)
        })
        .collect();
    Ok(Json(ModelListResponse {
        object: "list",
        data,
    }))
}

fn model_info(m: Model, endpoint: Option<&Endpoint>, created: i64) -> ModelInfo {
    let view = m.view(endpoint);
    ModelInfo {
        id: view.name,
        object: "model",
        created,
        owned_by: endpoint.map_or_else(String::new, |e| e.name.clone()),
        extensions: ModelExtensions {
            model_type: view.model_type,
            endpoint_id: view.endpoint_id,
            remote_model: view.remote_model,
            status: view.status,
        },
    }
}
