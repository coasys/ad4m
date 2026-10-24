//! Expression WS-native handlers.

use serde_json::Value;
use std::future::Future;
use std::sync::Arc;

use crate::agent::capabilities::*;
use crate::helpers::build_expression_rendered;
use crate::languages::LanguageController;
use crate::types::*;

use super::types::*;
use super::ws_handler::{HandlerMap, ParamExt, WsRpcError};

/// `expression.get`: the rendered expression, or `null` when unknown.
async fn get_expression(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    read_expression(params, ctx, false).await
}

/// `expression.getRaw`: the stored expression as a JSON string, or `null` when unknown.
async fn get_raw_expression(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    read_expression(params, ctx, true).await
}

async fn read_expression(
    params: Value,
    ctx: Arc<RequestContext>,
    raw: bool,
) -> Result<Value, WsRpcError> {
    ctx.require(&EXPRESSION_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let url = params.require_str("url")?;
    Ok(
        match resolve_expression(url, loaded_language_expression).await? {
            Some((_, expr_json)) if raw => {
                Value::String(serde_json::to_string(&expr_json).unwrap_or_default())
            }
            Some((lang_address, expr_json)) => {
                serde_json::to_value(build_expression_rendered(&expr_json, &lang_address))?
            }
            None => Value::Null,
        },
    )
}

/// Most expression lookups one `expression.getMany` call runs at once.
const GET_MANY_CONCURRENCY: usize = 16;

/// `expression.getMany { urls }` → one entry per URL, in order: what
/// `expression.get` returns for it, or `{ code, message }` where it fails.
async fn get_many_expressions(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    ctx.require(&EXPRESSION_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: ExpressionManyRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    Ok(serde_json::to_value(
        get_many_entries(body.urls, loaded_language_expression).await,
    )?)
}

/// `get_many_expressions` over any `lookup`: entry `i` is `resolve_expression(urls[i])`.
// Owned URLs and a `Clone` lookup: a stream over borrowed URLs makes the
// handler future lifetime-generic, which the `Send` bound on handlers rejects.
async fn get_many_entries<F, Fut>(urls: Vec<String>, lookup: F) -> Vec<Option<ExpressionManyEntry>>
where
    F: Fn(String, String) -> Fut + Clone,
    Fut: Future<Output = Result<Option<Value>, String>>,
{
    use futures::stream::StreamExt;
    futures::stream::iter(urls.into_iter().map(|url| {
        let fut = resolve_expression(url, lookup.clone());
        async move {
            match fut.await {
                Ok(Some((lang_address, expr_json))) => Some(ExpressionManyEntry::Found(
                    build_expression_rendered(&expr_json, &lang_address),
                )),
                Ok(None) => None,
                Err(e) => Some(ExpressionManyEntry::Failed(ExpressionLookupError {
                    code: e.code,
                    message: e.message,
                })),
            }
        }
    }))
    .buffered(GET_MANY_CONCURRENCY)
    .collect()
    .await
}

/// The stored expression at `url` (URL-decoded first) with its language
/// address, read through `lookup(language, address)`; `None` when the URL does
/// not parse or `lookup` finds nothing. A failed lookup is a 500 naming the URL.
async fn resolve_expression<F, Fut>(
    url: String,
    lookup: F,
) -> Result<Option<(String, Value)>, WsRpcError>
where
    F: FnOnce(String, String) -> Fut,
    Fut: Future<Output = Result<Option<Value>, String>>,
{
    let decoded_url = urlencoding::decode(&url)
        .map(|s| s.into_owned())
        .unwrap_or(url);
    let Ok((lang_address, expression_address)) = LanguageController::parse_expr_url(&decoded_url)
    else {
        return Ok(None);
    };
    match lookup(lang_address.clone(), expression_address).await {
        Ok(found) => Ok(found.map(|expr_json| (lang_address, expr_json))),
        Err(e) => Err(WsRpcError::internal(format!(
            "Failed to get expression {}: {}",
            decoded_url, e
        ))),
    }
}

/// The expression at `address` in a loaded language (or a literal); `None`
/// when the language is not loaded.
async fn loaded_language_expression(
    lang_address: String,
    address: String,
) -> Result<Option<Value>, String> {
    let controller = LanguageController::global_instance();
    if lang_address != "literal" && !controller.is_language_loaded(&lang_address).await {
        return Ok(None);
    }
    controller
        .get_expression(&lang_address, &address)
        .await
        .map_err(|e| e.to_string())
}

async fn create_expression(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    ctx.require(&EXPRESSION_CREATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let body: CreateExpressionRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let controller = LanguageController::global_instance();
    let content_json: serde_json::Value =
        serde_json::from_str(&body.content).unwrap_or(Value::String(body.content));
    let agent_context = ctx.acts_as();

    let url = controller
        .expression_create(&body.language_address, content_json, &agent_context)
        .await
        .map_err(|e| {
            WsRpcError::internal(format!(
                "Failed to create expression on {}: {}",
                body.language_address, e
            ))
        })?;

    Ok(Value::String(url))
}

async fn get_interactions(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    ctx.require(&EXPRESSION_READ_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let url = params.require_str("url")?;
    let decoded_url = urlencoding::decode(&url)
        .map(|s| s.into_owned())
        .unwrap_or(url);

    let controller = LanguageController::global_instance();
    if let Ok((lang_address, _)) = LanguageController::parse_expr_url(&decoded_url) {
        if controller.is_language_loaded(&lang_address).await {
            let interactions = controller
                .expression_interactions(&decoded_url)
                .await
                .map_err(|e| {
                    WsRpcError::internal(format!(
                        "Failed to get expression interactions for {}: {}",
                        decoded_url, e
                    ))
                })?;
            return Ok(serde_json::to_value(interactions)?);
        }
    }

    Ok(serde_json::to_value(Vec::<InteractionMeta>::new())?)
}

async fn interact_expression(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    ctx.require(&EXPRESSION_UPDATE_CAPABILITY)
        .map_err(|e| WsRpcError::forbidden(e))?;

    let url = params.require_str("url")?;
    let body: InteractionCallWrapper = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let decoded_url = urlencoding::decode(&url)
        .map(|s| s.into_owned())
        .unwrap_or(url.clone());

    let controller = LanguageController::global_instance();
    if let Ok((lang_address, _)) = LanguageController::parse_expr_url(&decoded_url) {
        if controller.is_language_loaded(&lang_address).await {
            match controller
                .expression_interact(&decoded_url, &body.interaction_call)
                .await
            {
                Ok(Some(result)) => return Ok(Value::String(result)),
                Ok(None) => return Ok(Value::String("null".to_string())),
                Err(e) => {
                    return Err(WsRpcError::internal(format!(
                        "expression_interact failed for {}: {}",
                        decoded_url, e
                    )));
                }
            }
        }
    }

    Err(WsRpcError::not_found(format!(
        "Language not loaded for expression URL: {}",
        decoded_url
    )))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<ExpressionUrlRequest, Option<ExpressionRendered>>(
        "expression.get",
        get_expression,
    )
    .read();
    map.method::<ExpressionUrlRequest, Option<String>>("expression.getRaw", get_raw_expression)
        .read();
    map.method::<ExpressionManyRequest, Vec<Option<ExpressionManyEntry>>>(
        "expression.getMany",
        get_many_expressions,
    )
    .read();
    map.method::<CreateExpressionRequest, String>("expression.create", create_expression);
    map.method::<ExpressionUrlRequest, Vec<InteractionMeta>>(
        "expression.interactions",
        get_interactions,
    )
    .read();
    // The interaction's result, JSON-encoded (`"null"` when it returns nothing).
    map.method::<ExpressionInteractRequest, String>("expression.interact", interact_expression);
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// `good` stores one expression per address, `missing` stores none and
    /// `broken` fails every read.
    async fn fake_lookup(language: String, address: String) -> Result<Option<Value>, String> {
        match language.as_str() {
            "good" => Ok(Some(json!({
                "author": "did:key:alice",
                "timestamp": "2026-10-14T00:00:00Z",
                "data": { "address": address },
                "proof": { "key": "k", "signature": "s", "valid": true },
            }))),
            "missing" => Ok(None),
            _ => Err(format!("{language} is broken")),
        }
    }

    /// What one `expression.get` call answers for `url`, as a `getMany` entry.
    async fn get_one(url: &str) -> Value {
        match resolve_expression(url.to_string(), fake_lookup).await {
            Ok(Some((language, expr))) => {
                serde_json::to_value(build_expression_rendered(&expr, &language)).unwrap()
            }
            Ok(None) => Value::Null,
            Err(e) => json!({ "code": e.code, "message": e.message }),
        }
    }

    #[tokio::test]
    async fn get_many_answers_what_get_answers_per_url() {
        let urls: Vec<String> = [
            "good://a",
            "good%3A%2F%2Fencoded%20address",
            "missing://x",
            "broken://y",
            "broken%3A%2F%2Fz",
            "no-scheme",
            "good://a",
        ]
        .iter()
        .map(|u| u.to_string())
        .collect();

        let many = serde_json::to_value(get_many_entries(urls.clone(), fake_lookup).await).unwrap();
        let mut each = Vec::new();
        for url in &urls {
            each.push(get_one(url).await);
        }
        assert_eq!(many, Value::Array(each));

        // The encoded URL is decoded before the lookup, as `expression.get` does.
        assert_eq!(many[1]["data"], json!("{\"address\":\"encoded address\"}"));
        assert_eq!(many[2], Value::Null);
        assert_eq!(
            many[4],
            json!({ "code": 500, "message": "Failed to get expression broken://z: broken is broken" })
        );
        assert_eq!(many[5], Value::Null);
    }

    #[test]
    fn a_failed_entry_round_trips_as_failed() {
        let wire = json!([null, { "code": 500, "message": "m" }]);
        let entries: Vec<Option<ExpressionManyEntry>> = serde_json::from_value(wire).unwrap();
        assert!(entries[0].is_none());
        assert!(matches!(
            &entries[1],
            Some(ExpressionManyEntry::Failed(e)) if *e == ExpressionLookupError { code: 500, message: "m".into() }
        ));
    }
}
