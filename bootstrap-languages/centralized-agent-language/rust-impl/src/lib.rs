//! # Centralized Agent Expression Store — Rust ALDK port
//!
//! Expression language that stores agent expressions via a centralized
//! HTTP server (socket.ad4m.dev). Ports the JS implementation at
//! `bootstrap-languages/centralized-agent-language/index.ts` onto the
//! Rust ALDK: same endpoint, same content contract (an Agent object
//! with `did` + `perspective`).

use ad4m_ldk::imports as rt;
use ad4m_ldk::prelude::*;
use web_sys::console;
use wasm_bindgen::JsValue;

const ENDPOINT: &str = "https://socket.ad4m.dev/agent";

fn log(msg: &str) {
    console::log_1(&JsValue::from_str(msg));
}

fn warn(msg: &str) {
    console::warn_1(&JsValue::from_str(msg));
}

fn error(msg: &str) {
    console::error_1(&JsValue::from_str(msg));
}

pub struct CentralizedAgentLanguage;

impl Language for CentralizedAgentLanguage {
    fn name() -> &'static str { "centralized-agent-expression-store" }
    fn version() -> &'static str { "0.1.0" }
    fn is_public() -> bool { false }

    async fn init() -> LanguageResult<Self> {
        log("[centralized-agent-language] init: ready (socket.ad4m.dev)");
        Ok(CentralizedAgentLanguage)
    }
}

impl ExpressionCapability for CentralizedAgentLanguage {
    async fn expression_create(
        &mut self,
        mut content: serde_json::Value,
    ) -> LanguageResult<Address> {
        log("[centralized-agent-language] expressionCreate called");

        let did = content.get("did").and_then(|v| v.as_str())
            .ok_or_else(|| {
                error("[centralized-agent-language] expressionCreate: missing 'did' in content");
                LanguageError::invalid_input(
                    "Content must be an Agent object (missing did)"
                )
            })?
            .to_string();

        log(&format!("[centralized-agent-language] expressionCreate: did={}", did));

        let has_links = content.get("perspective")
            .and_then(|p| p.get("links"))
            .and_then(|l| l.as_array())
            .is_some();
        if !has_links {
            error(&format!("[centralized-agent-language] expressionCreate: missing perspective.links for did={}", did));
            return Err(LanguageError::invalid_input(
                "Content must be an Agent object (missing perspective.links)"
            ));
        }

        let my_did = rt::agent_did();
        if did != my_did {
            error(&format!("[centralized-agent-language] expressionCreate: DID mismatch. content.did={}, agentDid()={}", did, my_did));
            return Err(LanguageError::permission_denied(
                "Can't set Agent Expression for foreign DID - only for self"
            ));
        }

        if content.get("directMessageLanguage").is_none() {
            content.as_object_mut().unwrap()
                .insert("directMessageLanguage".to_string(), serde_json::Value::Null);
        }
        if let Some(links) = content.get_mut("perspective")
            .and_then(|p| p.get_mut("links"))
            .and_then(|l| l.as_array_mut())
        {
            for link in links.iter_mut() {
                if let Some(obj) = link.as_object_mut() {
                    if let Some(proof) = obj.get_mut("proof").and_then(|p| p.as_object_mut()) {
                        proof.remove("valid");
                        proof.remove("invalid");
                    }
                }
            }
        }

        let signed: serde_json::Value =
            serde_wasm_bindgen::from_value(rt::agent_create_signed_expression_typed(&content))
                .map_err(|e| {
                    error(&format!("[centralized-agent-language] expressionCreate: signed expression serialization failed: {}", e));
                    LanguageError::internal(format!(
                        "serializing signed expression: {}", e
                    ))
                })?;

        let body = serde_json::json!({
            "data": { "did": did, "expression": signed }
        });

        log(&format!("[centralized-agent-language] expressionCreate: POSTing to {} for did={}", ENDPOINT, did));
        match rt::http_post_json(ENDPOINT, &body).await {
            Ok(_) => log(&format!("[centralized-agent-language] expressionCreate: POST succeeded for did={}", did)),
            Err(e) => {
                error(&format!("[centralized-agent-language] expressionCreate: POST FAILED for did={}. Error: {:?}", did, e));
                return Err(LanguageError::internal(format!(
                    "POST {} failed: {:?}", ENDPOINT, e
                )));
            }
        }

        Ok(did)
    }

    async fn expression_get(
        &mut self,
        address: Address,
    ) -> LanguageResult<Option<Expression>> {
        let url = format!("{}?did={}", ENDPOINT, urlencode(&address));
        log(&format!("[centralized-agent-language] expressionGet: fetching {}", url));

        let body = match rt::http_get(&url).await {
            Ok(b) => b,
            Err(e) => {
                error(&format!("[centralized-agent-language] expressionGet: GET FAILED for did={}. Error: {:?}", address, e));
                return Err(LanguageError::internal(format!(
                    "GET {} failed: {:?}", url, e
                )));
            }
        };

        if body.is_empty() {
            warn(&format!("[centralized-agent-language] expressionGet: empty response for did={}", address));
            return Ok(None);
        }

        let parsed: serde_json::Value = serde_json::from_str(&body)
            .map_err(|e| {
                error(&format!("[centralized-agent-language] expressionGet: JSON parse failed for did={}. Body (first 500): {}. Error: {}", address, &body[..body.len().min(500)], e));
                LanguageError::internal(format!(
                    "parsing GET response: {}", e
                ))
            })?;

        // The server returns { "expression": {...} } as the response body.
        // (axiod's response.data IS the body — no extra "data" wrapper.)
        let exp_value = parsed
            .get("expression")
            .cloned();

        let exp_value = match exp_value {
            Some(v) if !v.is_null() => {
                log(&format!("[centralized-agent-language] expressionGet: found expression for did={}", address));
                v
            }
            _ => {
                let keys: Vec<&str> = parsed.as_object()
                    .map(|o| o.keys().map(|k| k.as_str()).collect())
                    .unwrap_or_default();
                warn(&format!("[centralized-agent-language] expressionGet: no 'expression' field in response for did={}. Response keys: {:?}", address, keys));
                return Ok(None);
            }
        };

        match serde_json::from_value::<Expression>(exp_value) {
            Ok(exp) => {
                log(&format!("[centralized-agent-language] expressionGet: deserialized OK for did={}. author={}", address, exp.author));
                Ok(Some(exp))
            }
            Err(e) => {
                error(&format!("[centralized-agent-language] expressionGet: DESERIALIZATION FAILED for did={}. Error: {}", address, e));
                Err(LanguageError::internal(format!(
                    "deserializing AgentExpression: {}", e
                )))
            }
        }
    }
}

fn urlencode(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for b in s.as_bytes() {
        let c = *b;
        if c.is_ascii_alphanumeric() || c == b'-' || c == b'_' || c == b'.' || c == b'~' {
            out.push(c as char);
        } else {
            out.push_str(&format!("%{:02X}", c));
        }
    }
    out
}

ad4m_language! {
    language: CentralizedAgentLanguage,
    capabilities: [expression],
    holochain_signal: false,
}
