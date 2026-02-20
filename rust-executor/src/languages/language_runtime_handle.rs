use tokio::runtime::Builder;
use tokio::sync::{
    mpsc::{self, UnboundedSender},
    oneshot,
};
use serde_json::Value as JsonValue;
use log::{error, info};

use super::language_runtime::{LanguageOperation, LanguageRuntime, LanguageRuntimeRequest};

/// Handle to a per-language runtime running in its own thread.
/// Thin communication layer (like JsCoreHandle) that sends requests
/// to a LanguageRuntime via a channel.
#[derive(Clone)]
pub struct LanguageRuntimeHandle {
    pub language_address: String,
    tx: UnboundedSender<LanguageRuntimeRequest>,
}

impl LanguageRuntimeHandle {
    /// Spawn a new LanguageRuntime in a dedicated thread.
    pub fn spawn(language_address: String) -> Result<Self, String> {
        info!("Spawning LanguageRuntime for: {}", language_address);

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
                    let runtime = LanguageRuntime::new(addr.clone());

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
        Ok(Self { language_address, tx })
    }

    /// Send an operation and wait for the result.
    async fn send(&self, operation: LanguageOperation) -> Result<String, String> {
        let (response_tx, response_rx) = oneshot::channel();

        self.tx
            .send(LanguageRuntimeRequest { operation, response_tx })
            .map_err(|e| format!("Language runtime channel closed: {}", e))?;

        response_rx.await
            .map_err(|e| format!("Language runtime dropped without responding: {}", e))?
    }

    pub async fn execute(&self, script: String) -> Result<String, String> {
        self.send(LanguageOperation::Execute(script)).await
    }

    pub async fn load_module(&self, path: String) -> Result<(), String> {
        self.send(LanguageOperation::LoadModule(path)).await.map(|_| ())
    }

    pub async fn load_language(&self, context: JsonValue) -> Result<(), String> {
        self.send(LanguageOperation::LoadLanguage(context)).await.map(|_| ())
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
}
