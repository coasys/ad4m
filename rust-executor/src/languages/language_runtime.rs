use crate::js_core::JsCore;
use log::{debug, error, info, warn};
use serde_json::Value as JsonValue;
use tokio::sync::{mpsc::UnboundedReceiver, oneshot};

/// Request sent to a LanguageRuntime via its channel
#[derive(Debug)]
pub(crate) struct LanguageRuntimeRequest {
    pub operation: LanguageOperation,
    pub response_tx: oneshot::Sender<Result<String, String>>,
}

/// Operations that can be sent to a LanguageRuntime
#[derive(Debug)]
pub(crate) enum LanguageOperation {
    Execute(String),
    LoadModule(String),
    LoadLanguage(JsonValue),
    RegisterCallbacks,
    Teardown,
}

/// Per-language Deno runtime that encapsulates a single language instance.
/// Each LanguageRuntime has its own isolated JsCore/Deno worker.
///
/// This is NOT Send (contains V8/MainWorker). It lives in a dedicated thread
/// and processes requests from a channel. Mirrors the JsCore pattern -
/// communication happens via LanguageRuntimeHandle.
pub struct LanguageRuntime {
    language_address: String,
    js_core: JsCore,
}

impl LanguageRuntime {
    pub fn new(language_address: String) -> Self {
        info!("Creating LanguageRuntime for: {}", language_address);
        Self {
            language_address,
            js_core: JsCore::new_for_language(),
        }
    }

    /// Initialize the language runtime by executing the bootstrap module.
    /// Must be called before processing requests to make Deno ops available.
    pub async fn init(&self) -> Result<(), String> {
        self.js_core.init_for_language().await.map_err(|e| {
            format!(
                "Bootstrap failed for language {}: {}",
                self.language_address, e
            )
        })
    }

    /// Load a language bundle by dynamically importing it via the bootstrap's loadLanguageBundle()
    pub async fn load_module(&self, path: &str) -> Result<(), String> {
        let script = format!(r#"await loadLanguageBundle("{}")"#, path);
        self.js_core
            .execute(&script)
            .await
            .map_err(|e| format!("Failed to load language bundle {}: {}", path, e))?;
        Ok(())
    }

    /// Initialize the language with context via the bootstrap's initLanguage()
    pub async fn load_language(&self, language_context: JsonValue) -> Result<(), String> {
        info!("Initializing language: {}", self.language_address);

        let context_json = serde_json::to_string(&language_context)
            .map_err(|e| format!("Failed to serialize language context: {}", e))?;

        let script = format!("await initLanguage({})", context_json);

        self.js_core.execute(&script).await.map_err(|e| {
            format!(
                "Failed to call language constructor for {}: {}",
                self.language_address, e
            )
        })?;

        Ok(())
    }

    /// Execute a script in this language's runtime
    pub async fn execute(&self, script: &str) -> Result<String, String> {
        self.js_core.execute(script).await
    }

    /// Register callbacks for links and telepresence adapters
    pub async fn register_callbacks(&self) -> Result<(bool, bool), String> {
        let addr = &self.language_address;

        let links_script = format!(
            r#"
            (function() {{
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.linksAdapter) {{
                    language.linksAdapter.addCallback((diff) => {{
                        Deno.core.ops.perspective_diff_received(diff, "{addr}");
                    }});
                    if (language.linksAdapter.addSyncStateChangeCallback) {{
                        language.linksAdapter.addSyncStateChangeCallback((state) => {{
                            Deno.core.ops.sync_state_changed(state, "{addr}");
                        }});
                    }}
                    return true;
                }}
                return false;
            }})()
            "#,
        );

        let has_links = self.execute(&links_script).await?.trim() == "true";

        let telepresence_script = format!(
            r#"
            (function() {{
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.telepresenceAdapter) {{
                    language.telepresenceAdapter.registerSignalCallback((signal, recipientDid) => {{
                        Deno.core.ops.telepresence_signal_received(signal, "{addr}", recipientDid);
                    }});
                    return true;
                }}
                return false;
            }})()
            "#,
        );

        let has_telepresence = self.execute(&telepresence_script).await?.trim() == "true";

        info!(
            "Registered callbacks for language {}: links={}, telepresence={}",
            self.language_address, has_links, has_telepresence
        );

        Ok((has_links, has_telepresence))
    }

    /// Teardown and cleanup this language runtime
    pub async fn teardown(&self) -> Result<(), String> {
        info!("Tearing down language runtime: {}", self.language_address);

        let cleanup_script = r#"
            (async function() {
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.cleanup) {
                    await language.cleanup();
                }
            })()
        "#;

        if let Err(e) = self.execute(cleanup_script).await {
            error!(
                "Error during language cleanup for {}: {}",
                self.language_address, e
            );
        }

        let _ = self
            .execute("delete globalThis.__ad4m_language_instance__;")
            .await;

        info!("Tore down language runtime: {}", self.language_address);
        Ok(())
    }

    /// Main event loop: process requests from the channel until Teardown.
    ///
    /// Uses `tokio::select!` to concurrently poll the V8 event loop and
    /// receive requests, mirroring the pattern in `JsCore::start()`.
    /// Without event loop polling, JS async operations (timers, promises,
    /// pending ops) would stall between requests.
    pub(crate) async fn process_requests(self, mut rx: UnboundedReceiver<LanguageRuntimeRequest>) {
        let local_set = tokio::task::LocalSet::new();
        let js_core = self.js_core.clone();
        let addr = self.language_address.clone();

        local_set.run_until(async move {
            loop {
                tokio::select! {
                    biased;

                    // Poll requests from the channel
                    maybe_request = rx.recv() => {
                        match maybe_request {
                            Some(request) => {
                                let is_teardown = matches!(request.operation, LanguageOperation::Teardown);

                                let result = match request.operation {
                                    LanguageOperation::Execute(script) => self.execute(&script).await,
                                    LanguageOperation::LoadModule(path) => {
                                        self.load_module(&path).await.map(|_| String::new())
                                    }
                                    LanguageOperation::LoadLanguage(context) => {
                                        self.load_language(context).await.map(|_| String::new())
                                    }
                                    LanguageOperation::RegisterCallbacks => {
                                        self.register_callbacks().await.map(|(links, tp)| {
                                            format!("{{\"links\":{},\"telepresence\":{}}}", links, tp)
                                        })
                                    }
                                    LanguageOperation::Teardown => {
                                        self.teardown().await.map(|_| String::new())
                                    }
                                };

                                let _ = request.response_tx.send(result);

                                if is_teardown {
                                    debug!("Teardown complete for language: {}", addr);
                                    return;
                                }
                            }
                            None => {
                                // Channel closed (all senders dropped), exit
                                info!("[lang:{}] Request channel closed, shutting down", addr);
                                return;
                            }
                        }
                    }

                    // Continuously poll V8 event loop for pending JS tasks/ops
                    event_loop_result = js_core.event_loop() => {
                        match event_loop_result {
                            Ok(_) => {}
                            Err(err) => {
                                warn!("[lang:{}] Event loop error: {}", addr, err);
                            }
                        }
                    }
                }
            }
        }).await;
    }
}
