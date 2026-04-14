//! # Centralized Agent Expression Store — Rust ALDK port
//!
//! Expression language that stores agent expressions via a centralized
//! HTTP server (socket.ad4m.dev). Ports the JS implementation at
//! `bootstrap-languages/centralized-agent-language/index.ts` onto the
//! Rust ALDK: same endpoint, same content contract (an Agent object
//! with `did` + `perspective`).

use ad4m_ldk::imports as rt;
use ad4m_ldk::prelude::*;

const ENDPOINT: &str = "https://socket.ad4m.dev/agent";

pub struct CentralizedAgentLanguage;

impl Language for CentralizedAgentLanguage {
    fn name() -> &'static str { "centralized-agent-expression-store" }
    fn version() -> &'static str { "0.1.0" }
    fn is_public() -> bool { false }

    async fn init() -> LanguageResult<Self> {
        Ok(CentralizedAgentLanguage)
    }
}

impl ExpressionCapability for CentralizedAgentLanguage {
    async fn expression_create(
        &mut self,
        mut content: serde_json::Value,
    ) -> LanguageResult<Address> {
        let did = content.get("did").and_then(|v| v.as_str())
            .ok_or_else(|| LanguageError::invalid_input(
                "Content must be an Agent object (missing did)"
            ))?
            .to_string();

        let has_links = content.get("perspective")
            .and_then(|p| p.get("links"))
            .and_then(|l| l.as_array())
            .is_some();
        if !has_links {
            return Err(LanguageError::invalid_input(
                "Content must be an Agent object (missing perspective.links)"
            ));
        }

        let my_did = rt::agent_did();
        if did != my_did {
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
                .map_err(|e| LanguageError::internal(format!(
                    "serializing signed expression: {}", e
                )))?;

        let body = serde_json::json!({
            "data": { "did": did, "expression": signed }
        });

        rt::http_post_json(ENDPOINT, &body)
            .await
            .map_err(|e| LanguageError::internal(format!(
                "POST {} failed: {:?}", ENDPOINT, e
            )))?;

        Ok(did)
    }

    async fn expression_get(
        &mut self,
        address: Address,
    ) -> LanguageResult<Option<Expression>> {
        let url = format!("{}?did={}", ENDPOINT, urlencode(&address));
        let body = rt::http_get(&url)
            .await
            .map_err(|e| LanguageError::internal(format!(
                "GET {} failed: {:?}", url, e
            )))?;

        if body.is_empty() {
            return Ok(None);
        }

        let parsed: serde_json::Value = serde_json::from_str(&body)
            .map_err(|e| LanguageError::internal(format!(
                "parsing GET response: {}", e
            )))?;

        // Server wraps the agent expression in { data: { expression: {...} } }
        let exp_value = parsed
            .get("data")
            .and_then(|d| d.get("expression"))
            .cloned();

        let exp_value = match exp_value {
            Some(v) if !v.is_null() => v,
            _ => return Ok(None),
        };

        let exp: Expression = serde_json::from_value(exp_value)
            .map_err(|e| LanguageError::internal(format!(
                "deserializing AgentExpression: {}", e
            )))?;
        Ok(Some(exp))
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
