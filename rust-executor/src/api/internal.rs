//! Internal API endpoints — authenticated via INTERNAL_API_TOKEN.
//!
//! These endpoints serve platform → executor communication only.
//! They do not use the normal user auth (AuthContext / capabilities).

use axum::{http::HeaderMap, response::Json};
use serde_json::json;

use super::errors::ApiError;

/// Verify the INTERNAL_API_TOKEN from the Authorization header.
///
/// Returns `Ok(())` if the token matches, `Err(Unauthorized)` otherwise.
fn verify_internal_token(headers: &HeaderMap) -> Result<(), ApiError> {
    let token = headers
        .get("Authorization")
        .and_then(|v| v.to_str().ok())
        .and_then(|s| s.strip_prefix("Bearer "))
        .unwrap_or("");

    let config = crate::config::get_global_config();
    let expected = config.internal_api_token.as_deref().unwrap_or("");

    if token.is_empty() || expected.is_empty() || token != expected {
        return Err(ApiError::Unauthorized(
            "Invalid internal API token".to_string(),
        ));
    }

    Ok(())
}

/// POST /internal/shutdown — graceful shutdown triggered by the platform.
///
/// Sequence:
/// 1. Verify INTERNAL_API_TOKEN
/// 2. Flush all SPARQL stores
/// 3. Run a final snapshot backup
/// 4. Return 200 to the caller
/// 5. Exit the process after a short delay
pub async fn internal_shutdown(headers: HeaderMap) -> Result<Json<serde_json::Value>, ApiError> {
    verify_internal_token(&headers)?;

    // Spawn the shutdown sequence so the HTTP response goes out first
    tokio::spawn(async {
        log::info!("Graceful shutdown initiated by platform");

        // Run backup in a blocking context (uses reqwest::blocking)
        let config = crate::config::get_global_config();
        let backup_result = tokio::task::spawn_blocking(move || {
            crate::perspective_snapshot::backup_perspectives(&config)
        })
        .await;

        match backup_result {
            Ok(Ok(())) => log::info!("Final snapshot completed"),
            Ok(Err(e)) => log::warn!("Final snapshot failed: {}", e),
            Err(e) => log::warn!("Final snapshot task panic: {}", e),
        }

        // Give the HTTP response time to flush
        tokio::time::sleep(std::time::Duration::from_millis(500)).await;

        log::info!("Exiting process after graceful shutdown");
        std::process::exit(0);
    });

    Ok(Json(json!({"status": "shutting_down"})))
}
