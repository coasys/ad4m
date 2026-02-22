use super::byte_array::ByteArray;
use super::LanguageController;
use crate::{
    graphql::graphql_types::{OnlineAgent, PerspectiveExpression},
    types::{Perspective, PerspectiveDiff},
};
use async_trait::async_trait;
use base64::prelude::*;
use deno_core::error::AnyError;

/// Trait abstracting link-language backends (JS or WASM).
/// All methods take `&mut self` so implementations can mutate internal state.
#[async_trait]
pub trait LanguageBackend: Send + Sync {
    async fn sync(&mut self) -> Result<(), AnyError>;
    async fn commit(&mut self, diff: PerspectiveDiff) -> Result<Option<String>, AnyError>;
    async fn current_revision(&mut self) -> Result<Option<String>, AnyError>;
    async fn render(&mut self) -> Result<Option<Perspective>, AnyError>;
    async fn others(&mut self) -> Result<Vec<String>, AnyError>;
    async fn has_telepresence_adapter(&mut self) -> Result<bool, AnyError>;
    async fn set_online_status(&mut self, status: PerspectiveExpression) -> Result<(), AnyError>;
    async fn get_online_agents(&mut self) -> Result<Vec<OnlineAgent>, AnyError>;
    async fn send_signal(
        &mut self,
        remote_agent_did: String,
        payload: PerspectiveExpression,
    ) -> Result<(), AnyError>;
    async fn send_broadcast(&mut self, payload: PerspectiveExpression) -> Result<(), AnyError>;
}

// ---------------------------------------------------------------------------
// JS (Deno) backend – the original `Language` implementation
// ---------------------------------------------------------------------------

#[derive(Clone)]
pub struct Language {
    address: String,
}

fn parse_revision(js_result: String) -> Result<Option<String>, AnyError> {
    if let Ok(maybe_revision) = serde_json::from_str::<Option<ByteArray>>(&js_result) {
        Ok(maybe_revision.map(|revision| {
            let vec: Vec<u8> = revision.into();
            BASE64_STANDARD.encode(vec)
        }))
    } else {
        Ok(serde_json::from_str::<Option<String>>(&js_result)?)
    }
}

impl Language {
    pub fn new(address: String) -> Self {
        Self { address }
    }

    pub fn address(&self) -> &str {
        &self.address
    }
}

    pub async fn sync(&mut self) -> Result<(), AnyError> {
        let controller = LanguageController::global_instance();
        let script = r#"
            language.linksAdapter ? await language.linksAdapter.sync() : null
        "#;

        controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }

    pub async fn commit(&mut self, diff: PerspectiveDiff) -> Result<Option<String>, AnyError> {
        let controller = LanguageController::global_instance();
        let diff_json = serde_json::to_string(&diff)?;
        let script = format!(
            r#"
            JSON.stringify(language.linksAdapter ? await language.linksAdapter.commit({}) : null)
            "#,
            diff_json
        );

        let result = controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        parse_revision(result)
    }

    pub async fn current_revision(&mut self) -> Result<Option<String>, AnyError> {
        let controller = LanguageController::global_instance();
        let script = r#"
            JSON.stringify(language.linksAdapter ? await language.linksAdapter.currentRevision() : null)
        "#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        parse_revision(result)
    }

    pub async fn render(&mut self) -> Result<Option<Perspective>, AnyError> {
        let controller = LanguageController::global_instance();
        let script = r#"
            JSON.stringify(language.linksAdapter ? await language.linksAdapter.render() : null)
        "#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        let maybe_value = serde_json::from_str(&result)?;
        Ok(maybe_value)
    }

    pub async fn others(&mut self) -> Result<Vec<String>, AnyError> {
        let controller = LanguageController::global_instance();
        let script = r#"
            JSON.stringify(language.linksAdapter ? await language.linksAdapter.others() : null)
        "#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        let others_vec = serde_json::from_str(&result)?;
        Ok(others_vec)
    }

    pub async fn has_telepresence_adapter(&mut self) -> Result<bool, AnyError> {
        let controller = LanguageController::global_instance();
        let script = r#"
            language.telepresenceAdapter ? true : false
        "#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;

        // The result should be "true" or "false" as a string
        Ok(result.trim() == "true")
    }

    async fn set_online_status(
        &mut self,
        status: PerspectiveExpression,
    ) -> Result<(), AnyError> {
        let controller = LanguageController::global_instance();
        let status_json = serde_json::to_string(&status)?;
        let script = format!(
            r#"
            language.telepresenceAdapter ? await language.telepresenceAdapter.setOnlineStatus({}) : null
            "#,
            status_json
        );

        controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }

    pub async fn get_online_agents(&mut self) -> Result<Vec<OnlineAgent>, AnyError> {
        let controller = LanguageController::global_instance();
        let script = r#"
            JSON.stringify(language.telepresenceAdapter ? await language.telepresenceAdapter.getOnlineAgents() : null)
        "#;

        let result = controller
            .execute_on_language(&self.address, script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        let online_agents = serde_json::from_str(&result)?;
        Ok(online_agents)
    }

    async fn send_signal(
        &mut self,
        remote_agent_did: String,
        payload: PerspectiveExpression,
    ) -> Result<(), AnyError> {
        let controller = LanguageController::global_instance();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"
            language.telepresenceAdapter ? await language.telepresenceAdapter.sendSignal("{}", {}) : null
            "#,
            remote_agent_did, payload_json
        );

        controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }

    pub async fn send_broadcast(&mut self, payload: PerspectiveExpression) -> Result<(), AnyError> {
        let controller = LanguageController::global_instance();
        let payload_json = serde_json::to_string(&payload)?;
        let script = format!(
            r#"
            language.telepresenceAdapter ? await language.telepresenceAdapter.sendBroadcast({}) : null
            "#,
            payload_json
        );

        controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        Ok(())
    }

    pub async fn set_local_agents(&mut self, agents: Vec<String>) -> Result<(), AnyError> {
        log::debug!("set_local_agents: agents: {:?}", agents);
        let controller = LanguageController::global_instance();
        let agents_json = serde_json::to_string(&agents)?;
        let script = format!(
            r#"
            (language.linksAdapter && language.linksAdapter.setLocalAgents) ? await language.linksAdapter.setLocalAgents({}) : null
            "#,
            agents_json
        );

        log::debug!("set_local_agents script: {}", script);
        let result = controller
            .execute_on_language(&self.address, &script)
            .await
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
        log::debug!("set_local_agents result: {}", result);
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// WASM backend
// ---------------------------------------------------------------------------

#[cfg(feature = "wasm-languages")]
pub mod wasm_backend {
    use super::*;
    use crate::wasm_core::WasmLanguageInstance;
    use std::sync::{Arc, Mutex};

    /// WASM-based language backend wrapping a `WasmLanguageInstance`.
    pub struct WasmLanguage {
        instance: Arc<Mutex<WasmLanguageInstance>>,
    }

    impl WasmLanguage {
        pub fn new(instance: Arc<Mutex<WasmLanguageInstance>>) -> Self {
            Self { instance }
        }
    }

    #[async_trait]
    impl LanguageBackend for WasmLanguage {
        async fn sync(&mut self) -> Result<(), AnyError> {
            let mut instance = self.instance.lock().unwrap();
            if !instance.capabilities().has_links_adapter {
                return Ok(());
            }
            instance.sync().map_err(|e| anyhow::anyhow!("{}", e))
        }

        async fn commit(&mut self, diff: PerspectiveDiff) -> Result<Option<String>, AnyError> {
            let mut instance = self.instance.lock().unwrap();
            if !instance.capabilities().has_links_adapter {
                return Ok(None);
            }
            let abi_diff = crate::wasm_core::abi::AbiPerspectiveDiff {
                additions: diff.additions.into_iter().map(|le| crate::wasm_core::abi::AbiLinkExpression {
                    author: le.author,
                    timestamp: le.timestamp,
                    data: crate::wasm_core::abi::AbiLink {
                        source: le.data.source,
                        target: le.data.target,
                        predicate: le.data.predicate,
                    },
                    proof: crate::wasm_core::abi::AbiExpressionProof {
                        key: le.proof.key,
                        signature: le.proof.signature,
                    },
                    status: le.status.map(|s| format!("{:?}", s).to_lowercase()),
                }).collect(),
                removals: diff.removals.into_iter().map(|le| crate::wasm_core::abi::AbiLinkExpression {
                    author: le.author,
                    timestamp: le.timestamp,
                    data: crate::wasm_core::abi::AbiLink {
                        source: le.data.source,
                        target: le.data.target,
                        predicate: le.data.predicate,
                    },
                    proof: crate::wasm_core::abi::AbiExpressionProof {
                        key: le.proof.key,
                        signature: le.proof.signature,
                    },
                    status: le.status.map(|s| format!("{:?}", s).to_lowercase()),
                }).collect(),
            };
            instance.commit(&abi_diff).map_err(|e| anyhow::anyhow!("{}", e))
        }

        async fn current_revision(&mut self) -> Result<Option<String>, AnyError> {
            let mut instance = self.instance.lock().unwrap();
            if !instance.capabilities().has_links_adapter {
                return Ok(None);
            }
            instance.current_revision().map_err(|e| anyhow::anyhow!("{}", e))
        }

        async fn render(&mut self) -> Result<Option<Perspective>, AnyError> {
            let mut instance = self.instance.lock().unwrap();
            if !instance.capabilities().has_links_adapter {
                return Ok(None);
            }
            match instance.render().map_err(|e| anyhow::anyhow!("{}", e))? {
                Some(links) => {
                    let link_exprs: Vec<crate::types::LinkExpression> = links.into_iter().map(|le| {
                        crate::types::LinkExpression {
                            author: le.author,
                            timestamp: le.timestamp,
                            data: crate::types::Link {
                                source: le.data.source,
                                target: le.data.target,
                                predicate: le.data.predicate,
                            },
                            proof: crate::types::ExpressionProof {
                                key: le.proof.key,
                                signature: le.proof.signature,
                            },
                            status: le.status.and_then(|s| serde_json::from_value(serde_json::Value::String(s)).ok()),
                        }
                    }).collect();
                    Ok(Some(Perspective { links: link_exprs }))
                }
                None => Ok(None),
            }
        }

        async fn others(&mut self) -> Result<Vec<String>, AnyError> {
            let mut instance = self.instance.lock().unwrap();
            if !instance.capabilities().has_links_adapter {
                return Ok(vec![]);
            }
            instance.others().map_err(|e| anyhow::anyhow!("{}", e))
        }

        async fn has_telepresence_adapter(&mut self) -> Result<bool, AnyError> {
            Ok(false)
        }

        async fn set_online_status(
            &mut self,
            _status: PerspectiveExpression,
        ) -> Result<(), AnyError> {
            Ok(())
        }

        async fn get_online_agents(&mut self) -> Result<Vec<OnlineAgent>, AnyError> {
            Ok(vec![])
        }

        async fn send_signal(
            &mut self,
            _remote_agent_did: String,
            _payload: PerspectiveExpression,
        ) -> Result<(), AnyError> {
            Ok(())
        }

        async fn send_broadcast(
            &mut self,
            _payload: PerspectiveExpression,
        ) -> Result<(), AnyError> {
            Ok(())
        }
    }
}
