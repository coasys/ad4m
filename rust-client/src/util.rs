use anyhow::{anyhow, Result};
use serde::de::DeserializeOwned;

/// Helper to make authenticated GET requests to the REST API.
pub async fn get<R: DeserializeOwned>(
    executor_url: &str,
    cap_token: &str,
    path: &str,
) -> Result<R> {
    let url = format!("{}/api/v1{}", executor_url.trim_end_matches('/'), path);
    let response = reqwest::Client::new()
        .get(&url)
        .header("authorization", cap_token)
        .send()
        .await?;
    let status = response.status();
    if !status.is_success() {
        let body = response.text().await.unwrap_or_default();
        return Err(anyhow!("GET {} failed ({}): {}", path, status, body));
    }
    Ok(response.json().await?)
}

/// Helper to make authenticated POST requests to the REST API.
pub async fn post<B: serde::Serialize, R: DeserializeOwned>(
    executor_url: &str,
    cap_token: &str,
    path: &str,
    body: &B,
) -> Result<R> {
    let url = format!("{}/api/v1{}", executor_url.trim_end_matches('/'), path);
    let response = reqwest::Client::new()
        .post(&url)
        .header("authorization", cap_token)
        .json(body)
        .send()
        .await?;
    let status = response.status();
    if !status.is_success() {
        let body_text = response.text().await.unwrap_or_default();
        return Err(anyhow!("POST {} failed ({}): {}", path, status, body_text));
    }
    Ok(response.json().await?)
}

/// Helper to make authenticated PUT requests to the REST API.
pub async fn put<B: serde::Serialize, R: DeserializeOwned>(
    executor_url: &str,
    cap_token: &str,
    path: &str,
    body: &B,
) -> Result<R> {
    let url = format!("{}/api/v1{}", executor_url.trim_end_matches('/'), path);
    let response = reqwest::Client::new()
        .put(&url)
        .header("authorization", cap_token)
        .json(body)
        .send()
        .await?;
    let status = response.status();
    if !status.is_success() {
        let body_text = response.text().await.unwrap_or_default();
        return Err(anyhow!("PUT {} failed ({}): {}", path, status, body_text));
    }
    Ok(response.json().await?)
}

/// Helper to make authenticated PATCH requests to the REST API.
#[allow(dead_code)]
pub async fn patch<B: serde::Serialize, R: DeserializeOwned>(
    executor_url: &str,
    cap_token: &str,
    path: &str,
    body: &B,
) -> Result<R> {
    let url = format!("{}/api/v1{}", executor_url.trim_end_matches('/'), path);
    let response = reqwest::Client::new()
        .patch(&url)
        .header("authorization", cap_token)
        .json(body)
        .send()
        .await?;
    let status = response.status();
    if !status.is_success() {
        let body_text = response.text().await.unwrap_or_default();
        return Err(anyhow!("PATCH {} failed ({}): {}", path, status, body_text));
    }
    Ok(response.json().await?)
}

/// Helper to make authenticated DELETE requests to the REST API.
pub async fn delete<R: DeserializeOwned>(
    executor_url: &str,
    cap_token: &str,
    path: &str,
) -> Result<R> {
    let url = format!("{}/api/v1{}", executor_url.trim_end_matches('/'), path);
    let response = reqwest::Client::new()
        .delete(&url)
        .header("authorization", cap_token)
        .send()
        .await?;
    let status = response.status();
    if !status.is_success() {
        let body_text = response.text().await.unwrap_or_default();
        return Err(anyhow!(
            "DELETE {} failed ({}): {}",
            path,
            status,
            body_text
        ));
    }
    Ok(response.json().await?)
}

/// Helper to make authenticated DELETE requests with a JSON body.
pub async fn delete_with_body<B: serde::Serialize, R: DeserializeOwned>(
    executor_url: &str,
    cap_token: &str,
    path: &str,
    body: &B,
) -> Result<R> {
    let url = format!("{}/api/v1{}", executor_url.trim_end_matches('/'), path);
    let response = reqwest::Client::new()
        .delete(&url)
        .header("authorization", cap_token)
        .json(body)
        .send()
        .await?;
    let status = response.status();
    if !status.is_success() {
        let body_text = response.text().await.unwrap_or_default();
        return Err(anyhow!(
            "DELETE {} failed ({}): {}",
            path,
            status,
            body_text
        ));
    }
    Ok(response.json().await?)
}

/// Helper for POST requests that return no body (204 etc) — returns ()
#[allow(dead_code)]
pub async fn post_no_response<B: serde::Serialize>(
    executor_url: &str,
    cap_token: &str,
    path: &str,
    body: &B,
) -> Result<()> {
    let url = format!("{}/api/v1{}", executor_url.trim_end_matches('/'), path);
    let response = reqwest::Client::new()
        .post(&url)
        .header("authorization", cap_token)
        .json(body)
        .send()
        .await?;
    let status = response.status();
    if !status.is_success() {
        let body_text = response.text().await.unwrap_or_default();
        return Err(anyhow!("POST {} failed ({}): {}", path, status, body_text));
    }
    Ok(())
}

/// Helper for DELETE requests that return no body.
pub async fn delete_no_response(executor_url: &str, cap_token: &str, path: &str) -> Result<()> {
    let url = format!("{}/api/v1{}", executor_url.trim_end_matches('/'), path);
    let response = reqwest::Client::new()
        .delete(&url)
        .header("authorization", cap_token)
        .send()
        .await?;
    let status = response.status();
    if !status.is_success() {
        let body_text = response.text().await.unwrap_or_default();
        return Err(anyhow!(
            "DELETE {} failed ({}): {}",
            path,
            status,
            body_text
        ));
    }
    Ok(())
}
