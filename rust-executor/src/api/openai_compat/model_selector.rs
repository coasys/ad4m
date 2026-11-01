//! Resolve an OpenAI-style `model` string to a registered AD4M model and the endpoint
//! that serves it.
//!
//! Order of resolution (plan §5.1):
//!
//! 1. Exact match against any registered model's `id`.
//! 2. Exact match against any registered model's `name`.
//! 3. The literal `"default"` — returns the configured default for the
//!    requested [`ModelType`].
//! 4. `"<endpoint name>/<remote model>"` — the registered model of that pair.
//!
//! When nothing matches we return a 404 so the client sees the OpenAI
//! `model_not_found` shape. A model is never registered on the fly: billing needs a row
//! with a name.

use super::errors::{OpenAIError, OpenAIResult};
use crate::db::Ad4mDb;
use crate::types::{Endpoint, Model, ModelType};

/// A registered model, with the endpoint that serves it. `endpoint` is `None` for a model
/// without one (a local model): the executor runs it.
pub struct Resolved {
    pub model: Model,
    pub endpoint: Option<Endpoint>,
}

impl Resolved {
    /// The id the endpoint knows the model by.
    pub fn remote_model(&self) -> Option<&str> {
        self.model.api.as_ref().map(|api| api.model.as_str())
    }
}

fn db_error(e: impl std::fmt::Display) -> OpenAIError {
    OpenAIError::internal(format!("Database error: {e}"))
}

pub async fn resolve(requested: &str, expected_type: ModelType) -> OpenAIResult<Resolved> {
    if requested.is_empty() {
        return Err(OpenAIError::invalid_request(
            "Missing required parameter: model",
        ));
    }

    let models = Ad4mDb::with_global_instance(|db| db.get_models()).map_err(db_error)?;

    let found = models
        .iter()
        .find(|m| m.id == requested)
        // Case-sensitive, as OpenAI's.
        .or_else(|| models.iter().find(|m| m.name == requested))
        .cloned();
    let model = match found {
        Some(model) => model,
        // After id and name, so a real model named "default" wins.
        None if requested == "default" => {
            let id = Ad4mDb::with_global_instance(|db| db.get_default_model(expected_type.clone()))
                .map_err(db_error)?
                .ok_or_else(|| {
                    OpenAIError::not_found(format!(
                        "No default {} model configured on this executor",
                        type_label(&expected_type)
                    ))
                })?;
            models
                .iter()
                .find(|m| m.id == id)
                .cloned()
                .ok_or_else(|| OpenAIError::not_found(format!("Default model {id} not found")))?
        }
        None => by_endpoint_and_remote(&models, requested)?.ok_or_else(|| {
            OpenAIError::not_found(format!(
                "Model '{requested}' not found. List registered models with GET /v1/models."
            ))
        })?,
    };

    if model.model_type != expected_type {
        return Err(OpenAIError::invalid_request(format!(
            "Model '{}' is a {} model; this endpoint requires a {}.",
            requested,
            type_label(&model.model_type),
            type_label(&expected_type),
        )));
    }

    let endpoint = match &model.endpoint_id {
        Some(id) => Ad4mDb::with_global_instance(|db| db.get_endpoint(id)).map_err(db_error)?,
        None => None,
    };
    Ok(Resolved { model, endpoint })
}

/// The id of the model `requested` names. See [`resolve`].
pub async fn resolve_model(requested: &str, expected_type: ModelType) -> OpenAIResult<String> {
    Ok(resolve(requested, expected_type).await?.model.id)
}

/// The model registered for `"<endpoint name>/<remote model>"`.
fn by_endpoint_and_remote(models: &[Model], requested: &str) -> OpenAIResult<Option<Model>> {
    let Some((endpoint_name, remote)) = requested.split_once('/') else {
        return Ok(None);
    };
    let Some(endpoint) = Ad4mDb::with_global_instance(|db| db.get_endpoint_by_name(endpoint_name))
        .map_err(db_error)?
    else {
        return Ok(None);
    };
    Ok(models
        .iter()
        .find(|m| {
            m.endpoint_id.as_deref() == Some(endpoint.id.as_str())
                && m.api.as_ref().is_some_and(|api| api.model == remote)
        })
        .cloned())
}

fn type_label(t: &ModelType) -> &'static str {
    match t {
        ModelType::Llm => "LLM",
        ModelType::Embedding => "embedding",
        ModelType::Transcription => "transcription",
        ModelType::Speech => "speech",
    }
}
