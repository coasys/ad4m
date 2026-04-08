//! Error response format tests — verify ApiError maps to correct HTTP status codes and JSON format.

use axum::response::IntoResponse;
use http_body_util::BodyExt;

use crate::rest::errors::ApiError;

async fn error_to_json(error: ApiError) -> (u16, serde_json::Value) {
    let response = error.into_response();
    let status = response.status().as_u16();
    let body = response.into_body().collect().await.unwrap().to_bytes();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    (status, json)
}

#[tokio::test]
async fn bad_request_returns_400() {
    let (status, json) = error_to_json(ApiError::BadRequest("invalid input".into())).await;
    assert_eq!(status, 400);
    assert_eq!(json["code"], 400);
    assert_eq!(json["error"], "invalid input");
}

#[tokio::test]
async fn unauthorized_returns_401() {
    let (status, json) = error_to_json(ApiError::Unauthorized("no token".into())).await;
    assert_eq!(status, 401);
    assert_eq!(json["code"], 401);
    assert_eq!(json["error"], "no token");
}

#[tokio::test]
async fn forbidden_returns_403() {
    let (status, json) = error_to_json(ApiError::Forbidden("not permitted".into())).await;
    assert_eq!(status, 403);
    assert_eq!(json["code"], 403);
    assert_eq!(json["error"], "not permitted");
}

#[tokio::test]
async fn not_found_returns_404() {
    let (status, json) = error_to_json(ApiError::NotFound("resource missing".into())).await;
    assert_eq!(status, 404);
    assert_eq!(json["code"], 404);
    assert_eq!(json["error"], "resource missing");
}

#[tokio::test]
async fn internal_returns_500() {
    let (status, json) = error_to_json(ApiError::Internal("something broke".into())).await;
    assert_eq!(status, 500);
    assert_eq!(json["code"], 500);
    assert_eq!(json["error"], "something broke");
}

#[tokio::test]
async fn from_string_produces_internal() {
    let error: ApiError = "database error".to_string().into();
    let (status, _) = error_to_json(error).await;
    assert_eq!(status, 500);
}

#[tokio::test]
async fn from_anyhow_produces_internal() {
    let anyhow_err = deno_core::anyhow::anyhow!("anyhow failure");
    let error: ApiError = anyhow_err.into();
    let (status, json) = error_to_json(error).await;
    assert_eq!(status, 500);
    assert!(json["error"].as_str().unwrap().contains("anyhow failure"));
}

#[tokio::test]
async fn error_response_is_json_content_type() {
    let response = ApiError::BadRequest("test".into()).into_response();
    let content_type = response
        .headers()
        .get("content-type")
        .unwrap()
        .to_str()
        .unwrap();
    assert!(content_type.contains("application/json"));
}

#[tokio::test]
async fn capability_err_to_api_forbidden() {
    use crate::rest::errors::capability_err_to_api;
    let err = capability_err_to_api("Capability not permitted for this resource");
    let (status, _) = error_to_json(err).await;
    assert_eq!(status, 403);
}

#[tokio::test]
async fn capability_err_to_api_unauthorized() {
    use crate::rest::errors::capability_err_to_api;
    let err = capability_err_to_api("Invalid token");
    let (status, _) = error_to_json(err).await;
    assert_eq!(status, 401);
}
