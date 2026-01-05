use std::path::PathBuf;
use serde_json::Value as JsonValue;
use crate::js_core::JsCore;
use log::{debug, error, info};

/// Per-language Deno runtime that encapsulates a single language instance.
/// Each LanguageRuntime has its own isolated JsCore/Deno worker.
/// Note: This is NOT Send because it contains a Deno MainWorker.
pub struct LanguageRuntime {
    /// The IPFS hash identifying this language
    pub language_address: String,

    /// Dedicated Deno runtime instance
    #[allow(dead_code)]
    pub(crate) js_core: JsCore,

    /// Human-readable name (optional)
    pub language_name: Option<String>,

    /// Path to the language bundle
    bundle_path: PathBuf,

    /// Per-language storage path
    #[allow(dead_code)]
    storage_directory: PathBuf,

    /// Language-specific settings
    pub custom_settings: Option<JsonValue>,

    /// Whether the language has a links adapter with callbacks registered
    pub has_links_adapter: bool,

    /// Whether the language has a telepresence adapter with callbacks registered
    pub has_telepresence_adapter: bool,
}

impl LanguageRuntime {
    /// Create a new LanguageRuntime instance
    pub async fn new(
        language_address: String,
        bundle_path: PathBuf,
        storage_directory: PathBuf,
        custom_settings: Option<JsonValue>,
    ) -> Result<Self, String> {
        info!("Creating runtime for language: {}", language_address);

        // Create a new JsCore instance for this language
        let js_core = JsCore::new();

        let runtime = Self {
            language_address: language_address.clone(),
            js_core,
            language_name: None,
            bundle_path,
            storage_directory,
            custom_settings,
            has_links_adapter: false,
            has_telepresence_adapter: false,
        };

        // Initialize the Deno runtime
        runtime.js_core.init_engine().await
            .map_err(|e| format!("Failed to initialize Deno runtime for language {}: {}", language_address, e))?;

        info!("Successfully created runtime for language: {}", language_address);
        Ok(runtime)
    }

    /// Load and initialize the language bundle
    pub async fn load_language(&mut self, language_context: JsonValue) -> Result<(), String> {
        info!("Loading language from: {:?}", self.bundle_path);

        // Load the language bundle module
        let bundle_path_str = self.bundle_path.to_string_lossy().to_string();
        self.js_core.load_module(&bundle_path_str).await
            .map_err(|e| format!("Failed to load language bundle {}: {}", self.language_address, e))?;

        // Create language context and call constructor
        let context_json = serde_json::to_string(&language_context)
            .map_err(|e| format!("Failed to serialize language context: {}", e))?;

        let constructor_script = format!(
            "const language = await languageConstructor({});\n\
             globalThis.__ad4m_language_instance__ = language;\n\
             language",
            context_json
        );

        self.js_core.execute(&constructor_script).await
            .map_err(|e| format!("Failed to call language constructor for {}: {}", self.language_address, e))?;

        info!("Successfully loaded language: {}", self.language_address);
        Ok(())
    }

    /// Execute a script in this language's runtime
    pub async fn execute(&self, script: &str) -> Result<String, String> {
        debug!("Executing script in language runtime: {}", self.language_address);
        self.js_core.execute(script).await
    }

    /// Register callbacks for this language
    pub async fn register_callbacks(&mut self) -> Result<(), String> {
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

        let has_links = self.execute(&links_callback_script).await?;
        self.has_links_adapter = has_links.trim() == "true";

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

        let has_telepresence = self.execute(&telepresence_callback_script).await?;
        self.has_telepresence_adapter = has_telepresence.trim() == "true";

        info!(
            "Registered callbacks for language {}: links={}, telepresence={}",
            self.language_address,
            self.has_links_adapter,
            self.has_telepresence_adapter
        );

        Ok(())
    }

    /// Teardown and cleanup this language runtime
    pub async fn teardown(&self) -> Result<(), String> {
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

        if let Err(e) = self.execute(cleanup_script).await {
            error!("Error during language cleanup for {}: {}", self.language_address, e);
            // Continue with teardown even if cleanup fails
        }

        // Clear the language instance reference
        let _ = self.execute("delete globalThis.__ad4m_language_instance__;").await;

        info!("Successfully tore down language runtime: {}", self.language_address);
        Ok(())
    }
}

impl Drop for LanguageRuntime {
    fn drop(&mut self) {
        debug!("Dropping LanguageRuntime for: {}", self.language_address);
        // JsCore will be dropped automatically, cleaning up the Deno worker
    }
}
