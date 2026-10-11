//! Resolve an OpenAI-style `model` string to a registered AD4M model id.
//!
//! Order of resolution (matches the spec §4.4):
//!
//! 1. Exact match against any registered model's `id`.
//! 2. Exact match against any registered model's `name`.
//! 3. The literal `"default"` — returns the configured default for the
//!    requested [`ModelType`].
//!
//! When nothing matches we return a 404 so the client sees the OpenAI
//! `model_not_found` shape.

use super::errors::{OpenAIError, OpenAIResult};
use crate::db::Ad4mDb;
use crate::types::ModelType;

pub async fn resolve_model(requested: &str, expected_type: ModelType) -> OpenAIResult<String> {
    if requested.is_empty() {
        return Err(OpenAIError::invalid_request(
            "Missing required parameter: model",
        ));
    }

    let models = Ad4mDb::with_global_instance(|db| db.get_models())
        .map_err(|e| OpenAIError::internal(format!("Database error listing models: {e}")))?;

    // Pass 1: exact id match.
    if let Some(m) = models.iter().find(|m| m.id == requested) {
        if m.model_type != expected_type {
            return Err(OpenAIError::invalid_request(format!(
                "Model '{}' is a {} model; this endpoint requires a {}.",
                requested,
                type_label(&m.model_type),
                type_label(&expected_type),
            )));
        }
        return Ok(m.id.clone());
    }

    // Pass 2: exact name match (case-sensitive — same as OpenAI).
    if let Some(m) = models.iter().find(|m| m.name == requested) {
        if m.model_type != expected_type {
            return Err(OpenAIError::invalid_request(format!(
                "Model '{}' is a {} model; this endpoint requires a {}.",
                requested,
                type_label(&m.model_type),
                type_label(&expected_type),
            )));
        }
        return Ok(m.id.clone());
    }

    // Pass 3: the `"default"` keyword resolves via the DB's per-type
    // pointer, after id/name so a real model named "default" wins.
    if requested == "default" {
        return Ad4mDb::with_global_instance(|db| db.get_default_model(expected_type.clone()))
            .map_err(|e| OpenAIError::internal(format!("Database error: {e}")))?
            .ok_or_else(|| {
                OpenAIError::not_found(format!(
                    "No default {} model configured on this executor",
                    type_label(&expected_type)
                ))
            });
    }

    Err(OpenAIError::not_found(format!(
        "Model '{requested}' not found. List registered models with GET /v1/models."
    )))
}

fn type_label(t: &ModelType) -> &'static str {
    match t {
        ModelType::Llm => "LLM",
        ModelType::Embedding => "embedding",
        ModelType::Transcription => "transcription",
    }
}
