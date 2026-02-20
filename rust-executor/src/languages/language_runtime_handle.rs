use std::path::PathBuf;
use std::sync::Arc;
use tokio::runtime::Builder;
use tokio::sync::{
    broadcast::{self, Sender},
    mpsc::{self, UnboundedReceiver, UnboundedSender},
    oneshot, Mutex as TokioMutex,
};
use serde_json::Value as JsonValue;
use log::{debug, error, info};

use crate::js_core::JsCore;

/// Handle to a per-language runtime running in its own thread
pub struct LanguageRuntimeHandle {
    pub language_address: String,
    tx_execute: UnboundedSender<LanguageRuntimeRequest>,
    tx_module_load: UnboundedSender<LanguageRuntimeRequest>,
    broadcast_tx: Sender<LanguageRuntimeResponse>,
}

impl Clone for LanguageRuntimeHandle {
    fn clone(&self) -> Self {
        Self {
            language_address: self.language_address.clone(),
            tx_execute: self.tx_execute.clone(),
            tx_module_load: self.tx_module_load.clone(),
            broadcast_tx: self.broadcast_tx.clone(),
        }
    }
}

#[derive(Debug)]
struct LanguageRuntimeRequest {
    operation: LanguageOperation,
    response_tx: oneshot::Sender<LanguageRuntimeResponse>,
}

#[derive(Debug)]
enum LanguageOperation {
    Execute(String),
    LoadModule(String),
    Teardown,
}

#[derive(Debug, Clone)]
struct LanguageRuntimeResponse {
    result: Result<String, String>,
}

impl LanguageRuntimeHandle {
    /// Create a new LanguageRuntimeHandle and spawn a dedicated thread for this language
    pub async fn new(
        language_address: String,
        _bundle_path: PathBuf,
        _storage_directory: PathBuf,
        _custom_settings: Option<JsonValue>,
    ) -> Result<Self, String> {
        info!("Creating LanguageRuntimeHandle for: {}", language_address);

        let (tx_inside, _rx_outside) = broadcast::channel::<LanguageRuntimeResponse>(50);
        let (tx_execute, rx_execute) = mpsc::unbounded_channel::<LanguageRuntimeRequest>();
        let (tx_module_load, rx_module_load) = mpsc::unbounded_channel::<LanguageRuntimeRequest>();

        let tx_inside_clone = tx_inside.clone();
        let language_address_clone = language_address.clone();

        // Spawn dedicated thread for this language runtime
        std::thread::Builder::new()
            .name(format!("lang-{}", &language_address[..8]))
            .spawn(move || {
                let rt = Builder::new_current_thread()
                    .thread_name(format!("lang-{}", &language_address_clone[..8]))
                    .enable_all()
                    .build()
                    .expect("Failed to create Tokio runtime for language");

                let _guard = rt.enter();

                rt.block_on(async {
                    // Create dedicated JsCore for this language (minimal bootstrap, no main.js)
                    let js_core = JsCore::new_for_language();

                    info!("JsCore created for language: {}", language_address_clone);

                    // Signal initialization complete
                    let _ = tx_inside_clone.send(LanguageRuntimeResponse {
                        result: Ok("initialized".to_string()),
                    });

                    // Run the event loop
                    Self::run_event_loop(
                        js_core,
                        language_address_clone,
                        rx_execute,
                        rx_module_load,
                    ).await;
                });
            })
            .map_err(|e| format!("Failed to spawn language thread: {}", e))?;

        let handle = Self {
            language_address: language_address.clone(),
            tx_execute,
            tx_module_load,
            broadcast_tx: tx_inside.clone(),
        };

        // Wait for initialization
        let mut rx = tx_inside.subscribe();
        match rx.recv().await {
            Ok(response) => {
                response.result.map_err(|e| format!("Language runtime initialization failed: {}", e))?;
            }
            Err(e) => return Err(format!("Failed to receive initialization signal: {}", e)),
        }

        info!("LanguageRuntimeHandle created for: {}", language_address);
        Ok(handle)
    }

    /// Execute a script in this language's runtime
    pub async fn execute(&mut self, script: String) -> Result<String, String> {
        debug!("Executing on language {}: {}", self.language_address, &script[..script.len().min(100)]);

        let (response_tx, response_rx) = oneshot::channel();

        self.tx_execute
            .send(LanguageRuntimeRequest {
                operation: LanguageOperation::Execute(script),
                response_tx,
            })
            .map_err(|e| format!("Failed to send execute request: {}", e))?;

        let response = response_rx.await
            .map_err(|e| format!("Failed to receive execute response: {}", e))?;

        response.result
    }

    /// Load a module in this language's runtime
    pub async fn load_module(&mut self, path: String) -> Result<String, String> {
        debug!("Loading module for language {}: {}", self.language_address, path);

        let (response_tx, response_rx) = oneshot::channel();

        self.tx_module_load
            .send(LanguageRuntimeRequest {
                operation: LanguageOperation::LoadModule(path),
                response_tx,
            })
            .map_err(|e| format!("Failed to send load_module request: {}", e))?;

        let response = response_rx.await
            .map_err(|e| format!("Failed to receive load_module response: {}", e))?;

        response.result
    }

    /// Load and initialize the language bundle
    pub async fn load_language(&mut self, language_context: JsonValue) -> Result<(), String> {
        info!("Loading language: {}", self.language_address);

        // The bundle should already be loaded as a module at this point
        // Now we need to call the language constructor

        let context_json = serde_json::to_string(&language_context)
            .map_err(|e| format!("Failed to serialize language context: {}", e))?;

        let constructor_script = format!(
            "const language = await languageConstructor({});\n\
             globalThis.__ad4m_language_instance__ = language;\n\
             language",
            context_json
        );

        self.execute(constructor_script).await?;

        info!("Successfully loaded language: {}", self.language_address);
        Ok(())
    }

    /// Register callbacks for this language
    pub async fn register_callbacks(&mut self) -> Result<(bool, bool), String> {
        debug!("Registering callbacks for language: {}", self.language_address);

        let language_address = self.language_address.clone();

        // Register links adapter callbacks
        let links_callback_script = format!(
            r#"
            (function() {{
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.linksAdapter) {{
                    language.linksAdapter.addCallback((diff) => {{
                        Deno.core.ops.perspective_diff_received(diff, "{}");
                    }});

                    if (language.linksAdapter.addSyncStateChangeCallback) {{
                        language.linksAdapter.addSyncStateChangeCallback((state) => {{
                            Deno.core.ops.sync_state_changed(state, "{}");
                        }});
                    }}
                    return true;
                }}
                return false;
            }})()
            "#,
            language_address, language_address
        );

        let has_links = self.execute(links_callback_script).await?;
        let has_links_adapter = has_links.trim() == "true";

        // Register telepresence adapter callbacks
        let telepresence_callback_script = format!(
            r#"
            (function() {{
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.telepresenceAdapter) {{
                    language.telepresenceAdapter.registerSignalCallback((signal, recipientDid) => {{
                        Deno.core.ops.telepresence_signal_received(signal, "{}", recipientDid);
                    }});
                    return true;
                }}
                return false;
            }})()
            "#,
            language_address
        );

        let has_telepresence = self.execute(telepresence_callback_script).await?;
        let has_telepresence_adapter = has_telepresence.trim() == "true";

        info!(
            "Registered callbacks for language {}: links={}, telepresence={}",
            self.language_address,
            has_links_adapter,
            has_telepresence_adapter
        );

        Ok((has_links_adapter, has_telepresence_adapter))
    }

    /// Teardown this language runtime
    pub async fn teardown(&mut self) -> Result<(), String> {
        info!("Tearing down language runtime: {}", self.language_address);

        // Call language cleanup if it exists
        let cleanup_script = r#"
            (async function() {
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.cleanup) {
                    await language.cleanup();
                }
            })()
        "#;

        if let Err(e) = self.execute(cleanup_script.to_string()).await {
            error!("Error during language cleanup for {}: {}", self.language_address, e);
            // Continue with teardown even if cleanup fails
        }

        // Clear the language instance reference
        let _ = self.execute("delete globalThis.__ad4m_language_instance__;".to_string()).await;

        // Send teardown signal
        let (response_tx, response_rx) = oneshot::channel();
        let _ = self.tx_execute.send(LanguageRuntimeRequest {
            operation: LanguageOperation::Teardown,
            response_tx,
        });

        // Wait briefly for teardown acknowledgment
        let _ = response_rx.await;

        info!("Successfully tore down language runtime: {}", self.language_address);
        Ok(())
    }

    /// Event loop for processing language runtime requests
    async fn run_event_loop(
        js_core: JsCore,
        language_address: String,
        rx_execute: UnboundedReceiver<LanguageRuntimeRequest>,
        rx_module_load: UnboundedReceiver<LanguageRuntimeRequest>,
    ) {
        let rx_execute = Arc::new(TokioMutex::new(rx_execute));
        let rx_module_load = Arc::new(TokioMutex::new(rx_module_load));

        loop {
            let execute_fut = Self::process_execute_requests(
                rx_execute.clone(),
                js_core.clone(),
                language_address.clone(),
            );

            let module_load_fut = Self::process_module_load_requests(
                rx_module_load.clone(),
                js_core.clone(),
                language_address.clone(),
            );

            let local_set = tokio::task::LocalSet::new();
            let module_load_local_set = tokio::task::LocalSet::new();

            tokio::select! {
                _execute = local_set.run_until(execute_fut) => {
                    debug!("Execute fut completed for language: {}", language_address);
                }
                _module_load = module_load_local_set.run_until(module_load_fut) => {
                    debug!("Module load fut completed for language: {}", language_address);
                }
            }
        }
    }

    /// Process execute requests
    async fn process_execute_requests(
        rx: Arc<TokioMutex<UnboundedReceiver<LanguageRuntimeRequest>>>,
        js_core: JsCore,
        language_address: String,
    ) {
        loop {
            let mut receiver = rx.lock().await;
            if let Some(request) = receiver.recv().await {
                drop(receiver); // Release lock before processing

                match request.operation {
                    LanguageOperation::Execute(script) => {
                        let js_core_cloned = js_core.clone();
                        let response_tx = request.response_tx;
                        let lang_addr = language_address.clone();

                        tokio::task::spawn_local(async move {
                            match js_core_cloned.execute(&script).await {
                                Ok(result) => {
                                    let _ = response_tx.send(LanguageRuntimeResponse {
                                        result: Ok(result),
                                    });
                                }
                                Err(err) => {
                                    error!("Error executing script in language {}: {}", lang_addr, err);
                                    let _ = response_tx.send(LanguageRuntimeResponse {
                                        result: Err(err),
                                    });
                                }
                            }
                        });
                    }
                    LanguageOperation::Teardown => {
                        debug!("Teardown requested for language: {}", language_address);
                        let _ = request.response_tx.send(LanguageRuntimeResponse {
                            result: Ok("teardown".to_string()),
                        });
                        return; // Exit the loop
                    }
                    _ => {
                        error!("Unexpected operation in execute channel");
                    }
                }
            } else {
                tokio::time::sleep(tokio::time::Duration::from_millis(1)).await;
            }
        }
    }

    /// Process module load requests
    async fn process_module_load_requests(
        rx: Arc<TokioMutex<UnboundedReceiver<LanguageRuntimeRequest>>>,
        js_core: JsCore,
        language_address: String,
    ) {
        loop {
            let mut receiver = rx.lock().await;
            if let Some(request) = receiver.recv().await {
                drop(receiver); // Release lock before processing

                if let LanguageOperation::LoadModule(path) = request.operation {
                    let js_core_cloned = js_core.clone();
                    let response_tx = request.response_tx;
                    let lang_addr = language_address.clone();

                    tokio::task::spawn_local(async move {
                        match js_core_cloned.load_module(&path).await {
                            Ok(()) => {
                                info!("Module loaded for language {}: {}", lang_addr, path);
                                let _ = response_tx.send(LanguageRuntimeResponse {
                                    result: Ok(String::from("")),
                                });
                            }
                            Err(err) => {
                                error!("Error loading module for language {}: {}", lang_addr, err);
                                let _ = response_tx.send(LanguageRuntimeResponse {
                                    result: Err(err.to_string()),
                                });
                            }
                        }
                    });
                } else {
                    error!("Unexpected operation in module load channel");
                }
            } else {
                tokio::time::sleep(tokio::time::Duration::from_millis(1)).await;
            }
        }
    }
}
