//! # Agent Expression Store — Rust ALDK port
//!
//! Expression language that stores agent expressions via Holochain DNA.
//! Ports the JS implementation at `bootstrap-languages/agent-language/
//! index.ts` onto the Rust ALDK: same DNA (`agent-store`), same zome
//! functions (`create_agent_expression`, `get_agent_expression`), same
//! content contract (an Agent object with `did` + `perspective`).

use ad4m_ldk::imports as rt;
use ad4m_ldk::prelude::*;
use serde::Serialize;

const DNA_NICK: &str = "agent-store";
const ZOME_NAME: &str = "agent_store";

const HAPP_BUNDLE: &[u8] = include_bytes!(
    "../../hc-dna/workdir/ad4m-agent-language.happ"
);

#[derive(Serialize)]
struct DnaRegistration<'a> {
    nick: &'a str,
    bundle: &'a [u8],
}

pub struct AgentLanguage;

impl Language for AgentLanguage {
    fn name() -> &'static str { "agent-expression-store" }
    fn version() -> &'static str { "0.1.0" }
    fn is_public() -> bool { false }

    async fn init() -> LanguageResult<Self> {
        let dnas = [DnaRegistration { nick: DNA_NICK, bundle: HAPP_BUNDLE }];
        rt::holochain_register_dnas_typed(&dnas[..])
            .await
            .map_err(|e| LanguageError::internal(format!(
                "holochain_register_dnas failed: {:?}", e
            )))?;
        Ok(AgentLanguage)
    }
}

impl ExpressionCapability for AgentLanguage {
    async fn expression_create(
        &mut self,
        mut content: serde_json::Value,
    ) -> LanguageResult<Address> {
        // Validate that content looks like an Agent object: must have
        // `did` and `perspective.links`.
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

        // Only the agent themselves may author their own Agent expression.
        let my_did = rt::agent_did();
        if did != my_did {
            return Err(LanguageError::permission_denied(
                "Can't set Agent Expression for foreign DID - only for self"
            ));
        }

        // Default optional fields and strip link proof validity markers,
        // matching the JS implementation.
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
                    obj.remove("status");
                }
            }
        }

        // Wrap in an agent-signed expression envelope, then write it to
        // the agent store DHT.
        let signed = rt::agent_create_signed_expression_typed(&content);
        rt::holochain_call(DNA_NICK, ZOME_NAME, "create_agent_expression", signed)
            .await
            .map_err(|e| LanguageError::internal(format!(
                "create_agent_expression failed: {:?}", e
            )))?;

        Ok(did)
    }

    async fn expression_get(
        &mut self,
        address: Address,
    ) -> LanguageResult<Option<Expression>> {
        let result = rt::holochain_call_typed(DNA_NICK, ZOME_NAME, "get_agent_expression", &address)
            .await
            .map_err(|e| LanguageError::internal(format!(
                "get_agent_expression failed: {:?}", e
            )))?;

        if result.is_null() || result.is_undefined() {
            return Ok(None);
        }

        // The zome returns an AgentExpression whose shape matches the
        // generic Expression envelope (author + timestamp + data + proof).
        let exp: Expression = serde_wasm_bindgen::from_value(result)
            .map_err(|e| LanguageError::internal(format!(
                "deserializing AgentExpression: {}", e
            )))?;
        Ok(Some(exp))
    }
}

ad4m_language! {
    language: AgentLanguage,
    capabilities: [expression],
    holochain_signal: false,
}
