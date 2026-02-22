use super::byte_array::ByteArray;
use crate::{
    graphql::graphql_types::{OnlineAgent, PerspectiveExpression},
    js_core::JsCoreHandle,
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
    js_core: JsCoreHandle,
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
    pub fn new(address: String, js_core: JsCoreHandle) -> Self {
        Self { address, js_core }
    }
}

#[async_trait]
impl LanguageBackend for Language {
    async fn sync(&mut self) -> Result<(), AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    ? 
                    await (await core.languageController.languageByRef({{address:"{}"}})).linksAdapter.sync() 
                    : 
                    null
                )
            "#,
            self.address, self.address,
        );
        let _result: String = self.js_core.execute(script).await?;
        Ok(())
    }

    async fn commit(&mut self, diff: PerspectiveDiff) -> Result<Option<String>, AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    ? 
                    await (await core.languageController.languageByRef({{address:"{}"}})).linksAdapter.commit({}) 
                    : 
                    null
                )
            "#,
            self.address,
            self.address,
            serde_json::to_string(&diff)?,
        );
        let result: String = self.js_core.execute(script).await?;
        parse_revision(result)
    }

    async fn current_revision(&mut self) -> Result<Option<String>, AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    ? 
                    await (await core.languageController.languageByRef({{address:"{}"}})).linksAdapter.currentRevision() 
                    : 
                    null
                )
            "#,
            self.address, self.address,
        );
        let result: String = self.js_core.execute(script).await?;
        parse_revision(result)
    }

    async fn render(&mut self) -> Result<Option<Perspective>, AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    ? 
                    await (await core.languageController.languageByRef({{address:"{}"}})).linksAdapter.render() 
                    : 
                    null
                )
            "#,
            self.address, self.address,
        );
        let result: String = self.js_core.execute(script).await?;
        let maybe_value = serde_json::from_str(&result)?;
        Ok(maybe_value)
    }

    async fn others(&mut self) -> Result<Vec<String>, AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    ? 
                    await (await core.languageController.languageByRef({{address:"{}"}})).linksAdapter.others() 
                    : 
                    null
                )
            "#,
            self.address, self.address,
        );
        let result: String = self.js_core.execute(script).await?;
        let others_vec = serde_json::from_str(&result)?;
        Ok(others_vec)
    }

    async fn has_telepresence_adapter(&mut self) -> Result<bool, AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    &&
                    await (await core.languageController.languageByRef({{address:"{}"}})).telepresenceAdapter
                    ? 
                    true
                    : 
                    false
                )
            "#,
            self.address, self.address,
        );
        let result: String = self.js_core.execute(script).await?;
        let has_telepresence_adapter = serde_json::from_str(&result)?;
        Ok(has_telepresence_adapter)
    }

    async fn set_online_status(
        &mut self,
        status: PerspectiveExpression,
    ) -> Result<(), AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    ? 
                    await (await core.languageController.languageByRef({{address:"{}"}})).telepresenceAdapter.setOnlineStatus({})
                    : 
                    null
                )
            "#,
            self.address,
            self.address,
            serde_json::to_string(&status)?,
        );
        let _result: String = self.js_core.execute(script).await?;
        Ok(())
    }

    async fn get_online_agents(&mut self) -> Result<Vec<OnlineAgent>, AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    ? 
                    await (await core.languageController.languageByRef({{address:"{}"}})).telepresenceAdapter.getOnlineAgents()
                    : 
                    null
                )
            "#,
            self.address, self.address,
        );
        let result: String = self.js_core.execute(script).await?;
        let online_agents = serde_json::from_str(&result)?;
        Ok(online_agents)
    }

    async fn send_signal(
        &mut self,
        remote_agent_did: String,
        payload: PerspectiveExpression,
    ) -> Result<(), AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    ? 
                    await (await core.languageController.languageByRef({{address:"{}"}})).telepresenceAdapter.sendSignal("{}", {})
                    : 
                    null
                )
            "#,
            self.address,
            self.address,
            remote_agent_did,
            serde_json::to_string(&payload)?,
        );
        let _result: String = self.js_core.execute(script).await?;
        Ok(())
    }

    async fn send_broadcast(&mut self, payload: PerspectiveExpression) -> Result<(), AnyError> {
        let script = format!(
            r#"
                JSON.stringify(
                    await core.languageController.languageByRef({{address:"{}"}}) 
                    ? 
                    await (await core.languageController.languageByRef({{address:"{}"}})).telepresenceAdapter.sendBroadcast({})
                    : 
                    null
                )
            "#,
            self.address,
            self.address,
            serde_json::to_string(&payload)?,
        );
        let _result: String = self.js_core.execute(script).await?;
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
