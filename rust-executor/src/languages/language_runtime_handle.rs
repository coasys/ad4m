use log::{debug, error, info};
use serde_json::Value as JsonValue;
use std::path::PathBuf;
use tokio::runtime::Builder;
use tokio::sync::{
    mpsc::{self, UnboundedSender},
    oneshot,
};

use crate::agent::AgentContext;
use crate::languages::LanguageContext;

use super::language_runtime::{LanguageOperation, LanguageRuntime, LanguageRuntimeRequest};

/// Handle to a per-language runtime running in its own thread.
/// Thin communication layer that sends requests to a LanguageRuntime
/// via a channel.
#[derive(Clone)]
pub struct LanguageRuntimeHandle {
    pub language_address: String,
    pub language_name: Option<String>,
    tx: UnboundedSender<LanguageRuntimeRequest>,
}

impl LanguageRuntimeHandle {
    /// Spawn a new LanguageRuntime in a dedicated thread.
    /// The storage_directory is the only filesystem path accessible to this sandbox.
    /// System/bootstrap languages additionally get CWD access.
    pub fn spawn(
        language_address: String,
        storage_directory: PathBuf,
        is_system_language: bool,
    ) -> Result<Self, String> {
        info!(
            "Spawning LanguageRuntime for: {} (system={})",
            language_address, is_system_language
        );

        let (tx, rx) = mpsc::unbounded_channel::<LanguageRuntimeRequest>();
        let addr = language_address.clone();
        let prefix_len = 8.min(addr.len());

        std::thread::Builder::new()
            .name(format!("lang-{}", &addr[..prefix_len]))
            .spawn(move || {
                let rt = Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .expect("Failed to create Tokio runtime for language");

                rt.block_on(async {
                    let runtime =
                        LanguageRuntime::new(addr.clone(), storage_directory, is_system_language);

                    // Execute bootstrap module to make Deno ops available
                    if let Err(e) = runtime.init().await {
                        error!("[lang:{}] Bootstrap failed: {}", addr, e);
                        return;
                    }

                    runtime.process_requests(rx).await;
                });
            })
            .map_err(|e| format!("Failed to spawn language thread: {}", e))?;

        info!("LanguageRuntimeHandle spawned for: {}", language_address);
        Ok(Self {
            language_address,
            language_name: None,
            tx,
        })
    }

    /// Send an operation and wait for the result.
    async fn send(&self, operation: LanguageOperation) -> Result<String, String> {
        let op_name = format!("{:?}", operation);
        let (response_tx, response_rx) = oneshot::channel();

        debug!(
            "[handle:{}] Sending operation: {}",
            self.language_address,
            &op_name[..op_name.len().min(100)]
        );

        self.tx
            .send(LanguageRuntimeRequest {
                operation,
                response_tx,
            })
            .map_err(|e| format!("Language runtime channel closed: {}", e))?;

        let result = response_rx
            .await
            .map_err(|e| format!("Language runtime dropped without responding: {}", e))?;

        match &result {
            Ok(_) => debug!("[handle:{}] Operation completed OK", self.language_address),
            Err(e) => error!("[handle:{}] Operation failed: {}", self.language_address, e),
        }

        result
    }

    pub async fn execute(&self, script: String) -> Result<String, String> {
        self.send(LanguageOperation::Execute(
            script,
            AgentContext::main_agent(),
        ))
        .await
    }

    pub async fn execute_with_context(
        &self,
        script: String,
        agent_context: AgentContext,
    ) -> Result<String, String> {
        self.send(LanguageOperation::Execute(script, agent_context))
            .await
    }

    pub async fn load_module(&self, path: String, context: LanguageContext) -> Result<(), String> {
        self.send(LanguageOperation::LoadModule(path, context))
            .await
            .map(|_| ())
    }

    pub async fn load_language(&self, context: JsonValue) -> Result<(), String> {
        self.send(LanguageOperation::LoadLanguage(context))
            .await
            .map(|_| ())
    }

    pub async fn register_callbacks(&self) -> Result<(bool, bool), String> {
        let result_str = self.send(LanguageOperation::RegisterCallbacks).await?;
        let v: serde_json::Value = serde_json::from_str(&result_str)
            .map_err(|e| format!("Failed to parse callback result: {}", e))?;
        Ok((
            v["links"].as_bool().unwrap_or(false),
            v["telepresence"].as_bool().unwrap_or(false),
        ))
    }

    pub async fn teardown(&self) -> Result<(), String> {
        self.send(LanguageOperation::Teardown).await.map(|_| ())
    }

    /// Query the language name from the JS runtime after initLanguage has run.
    pub async fn query_language_name(&mut self) -> Option<String> {
        // Wrap in JSON.stringify so the result round-trips through a
        // well-formed JSON string, not a raw v8 `to_rust_string_lossy`
        // capture. Also tolerate the Rust ALDK shape where `name` is an
        // exported zero-arg function (wasm-bindgen cannot export string
        // constants): call it when it's a function, read the value when
        // it's a string. The previous `trim().trim_matches('"')` path
        // mangled any name containing a `"` or leading/trailing
        // whitespace — rare for language names but trivial to fix
        // correctly via `serde_json::from_str`.
        let script = r#"
            JSON.stringify(
                (() => {
                    const l = globalThis.__ad4m_language_instance__;
                    if (!l) return "";
                    const n = l.name;
                    if (typeof n === "function") return String(n() ?? "");
                    return String(n ?? "");
                })()
            )
        "#
        .to_string();
        match self.execute(script).await {
            Ok(result) => {
                let name: String = serde_json::from_str(result.trim()).ok()?;
                if name.is_empty() {
                    None
                } else {
                    self.language_name = Some(name.clone());
                    Some(name)
                }
            }
            Err(_) => None,
        }
    }

    /// Return a human-readable label: "name (address)" or just "address"
    pub fn label(&self) -> String {
        match &self.language_name {
            Some(name) => format!("{} ({})", name, self.language_address),
            None => self.language_address.clone(),
        }
    }
}
