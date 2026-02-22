mod byte_array;
pub mod error;
pub mod language;
pub mod language_context;
pub mod language_runtime;
pub mod language_runtime_handle;

use deno_core::error::AnyError;
use std::collections::HashSet;
use std::sync::{Arc, Mutex};

use crate::runtime_service::RuntimeService;
use crate::types::Address;
use crate::{
    agent::{did_for_context, signing_key_id_for_context, AgentContext},
    graphql::graphql_types::{DecoratedNeighbourhoodExpression, Neighbourhood},
    js_core::JsCoreHandle,
    utils::{language_storage_directory, languages_directory},
};
use error::LanguageError;
use language::Language;
use language_context::LanguageContext;
use language_runtime_handle::LanguageRuntimeHandle;
use log::{error, info, warn};
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use tokio::sync::Mutex as TokioMutex;

/// Tracks addresses of system languages (language language, agent, neighbourhood, perspective)
#[derive(Debug, Clone, Default)]
pub struct SystemLanguageAddresses {
    pub language_language: Option<String>,
    pub agent_language: Option<String>,
    pub neighbourhood_language: Option<String>,
    pub perspective_language: Option<String>,
    pub system_language_set: HashSet<String>,
}

lazy_static! {
    static ref LANGUAGE_CONTROLLER_INSTANCE: Arc<Mutex<Option<LanguageController>>> =
        Arc::new(Mutex::new(None));
}

#[derive(Clone)]
pub struct LanguageController {
    // Legacy field for backward compatibility (still used for neighbourhood operations)
    js_core: JsCoreHandle,

    // Per-language runtime handles (isolated Deno instances per language)
    runtimes: Arc<TokioMutex<HashMap<String, LanguageRuntimeHandle>>>,

    // System language address tracking
    system_addresses: Arc<TokioMutex<SystemLanguageAddresses>>,

    // Watch channel for signaling when all languages are ready
    languages_ready_tx: Arc<tokio::sync::watch::Sender<bool>>,
    languages_ready_rx: tokio::sync::watch::Receiver<bool>,
}

impl LanguageController {
    pub fn init_global_instance(js_core: JsCoreHandle) {
        let mut instance = LANGUAGE_CONTROLLER_INSTANCE.lock().unwrap();
        *instance = Some(LanguageController::new(js_core));
    }

    pub fn global_instance() -> LanguageController {
        LANGUAGE_CONTROLLER_INSTANCE
            .lock()
            .unwrap()
            .as_ref()
            .expect("LanguageController not initialized")
            .clone()
    }

    fn new(js_core: JsCoreHandle) -> Self {
        let (languages_ready_tx, languages_ready_rx) = tokio::sync::watch::channel(false);
        Self {
            js_core,
            runtimes: Arc::new(TokioMutex::new(HashMap::new())),
            system_addresses: Arc::new(TokioMutex::new(SystemLanguageAddresses::default())),
            languages_ready_tx: Arc::new(languages_ready_tx),
            languages_ready_rx,
        }
    }

    /// Load a language from a bundle path
    ///
    /// Creates a dedicated per-language runtime with isolated Deno worker
    pub async fn load_language(&self, bundle_path: PathBuf) -> Result<String, LanguageError> {
        info!("Loading language from bundle: {:?}", bundle_path);

        // Read bundle to calculate IPFS hash
        let bundle_content = fs::read_to_string(&bundle_path)?;
        let language_address = self.calculate_language_hash(&bundle_content);

        info!("Language address: {}", language_address);

        // Get language settings if they exist
        let custom_settings = self.get_settings(&language_address).ok();

        // Get storage directory for this language
        let storage_directory = language_storage_directory(&language_address);

        // Create storage directory if it doesn't exist
        fs::create_dir_all(&storage_directory)?;

        // Get agent information for language context
        let agent_context = AgentContext::main_agent();
        let agent_did = did_for_context(&agent_context).map_err(|e| LanguageError::LoadError {
            address: language_address.clone(),
            message: format!("Failed to get agent DID: {}", e),
        })?;
        let agent_signing_key_id =
            signing_key_id_for_context(&agent_context).map_err(|e| LanguageError::LoadError {
                address: language_address.clone(),
                message: format!("Failed to get signing key ID: {}", e),
            })?;

        // Create language context
        let language_context = LanguageContext::new(
            agent_did,
            agent_signing_key_id,
            custom_settings.clone(),
            storage_directory.clone(),
            language_address.clone(),
        );

        // Spawn dedicated runtime in its own thread
        let runtime_handle =
            LanguageRuntimeHandle::spawn(language_address.clone()).map_err(|e| {
                LanguageError::LoadError {
                    address: language_address.clone(),
                    message: e,
                }
            })?;

        // Load the language bundle module
        let bundle_path_str = bundle_path.to_string_lossy().to_string();
        info!("Loading module for language {}", language_address);
        runtime_handle
            .load_module(bundle_path_str)
            .await
            .map_err(|e| LanguageError::LoadError {
                address: language_address.clone(),
                message: format!("Failed to load language module: {}", e),
            })?;
        info!("Module loaded for language {}", language_address);

        // Initialize the language with context
        info!("Calling load_language for {}", language_address);
        runtime_handle
            .load_language(language_context.to_json())
            .await
            .map_err(|e| LanguageError::LoadError {
                address: language_address.clone(),
                message: format!("Failed to initialize language: {}", e),
            })?;
        info!("load_language completed for {}", language_address);

        // Register callbacks for adapters
        info!("Registering callbacks for {}", language_address);
        runtime_handle
            .register_callbacks()
            .await
            .map_err(|e| LanguageError::LoadError {
                address: language_address.clone(),
                message: format!("Failed to register callbacks: {}", e),
            })?;
        info!("Callbacks registered for {}", language_address);

        // Store the runtime handle
        let mut runtimes = self.runtimes.lock().await;
        runtimes.insert(language_address.clone(), runtime_handle);

        info!("Successfully loaded language: {}", language_address);
        Ok(language_address)
    }

    /// Unload a language and clean up
    pub async fn unload_language(&self, language_address: &str) -> Result<(), LanguageError> {
        info!("Unloading language: {}", language_address);

        let mut runtimes = self.runtimes.lock().await;
        if let Some(runtime) = runtimes.remove(language_address) {
            // Teardown the runtime (cleanup language instance, drop thread)
            runtime
                .teardown()
                .await
                .map_err(|e| LanguageError::RuntimeError {
                    address: language_address.to_string(),
                    message: format!("Failed to teardown runtime: {}", e),
                })?;
        }

        info!("Successfully unloaded language: {}", language_address);
        Ok(())
    }

    /// Check if a language is loaded
    pub async fn is_language_loaded(&self, language_address: &str) -> bool {
        let runtimes = self.runtimes.lock().await;
        runtimes.contains_key(language_address)
    }

    /// Execute a script on a specific language runtime
    ///
    /// First tries dedicated per-language Rust runtime with proper thread isolation.
    /// Falls back to JS-side LanguageController for dynamically installed languages
    /// that don't have their own Rust runtime.
    pub async fn execute_on_language(
        &self,
        language_address: &str,
        script: &str,
    ) -> Result<String, LanguageError> {
        // Try Rust-side per-language runtime first
        let handle = {
            let runtimes = self.runtimes.lock().await;
            runtimes.get(language_address).cloned()
        };

        if let Some(handle) = handle {
            return handle
                .execute(script.to_string())
                .await
                .map_err(|e| LanguageError::RuntimeError {
                    address: language_address.to_string(),
                    message: e,
                });
        }

        // Fall back to JS-side execution for dynamically installed languages.
        // The script references `language` as a bare variable, so we wrap it
        // to get the language from the JS LanguageController first.
        // Note: We wrap the script in parentheses for the return statement to
        // avoid JavaScript ASI (automatic semicolon insertion) when the script
        // starts with a newline.
        let js_script = format!(
            r#"
            (async () => {{
                const language = await core.languageController.languageByRef({{ address: "{}", name: "" }});
                if (!language) {{
                    throw new Error("Language not loaded on JS side: {}");
                }}
                return ({});
            }})()
            "#,
            language_address, language_address, script
        );

        log::info!(
            "execute_on_language JS fallback for {}: script = {}",
            language_address,
            &script[..script.len().min(200)]
        );
        let mut js_core = self.js_core.clone();
        let result = js_core
            .execute(js_script)
            .await
            .map_err(|e| LanguageError::RuntimeError {
                address: language_address.to_string(),
                message: e.to_string(),
            });
        log::info!(
            "execute_on_language JS fallback result for {}: {:?}",
            language_address,
            result.as_ref().map(|r| &r[..r.len().min(200)])
        );
        result
    }

    /// Calculate IPFS hash for a language bundle using the same algorithm as utils_extension::hash()
    fn calculate_language_hash(&self, bundle_content: &str) -> String {
        use cid::Cid;
        use multibase::Base;
        use multihash::{Code, MultihashDigest};

        // Compute the SHA-256 multihash
        let multihash = Code::Sha2_256.digest(bundle_content.as_bytes());

        // Create a CID with version 1, raw codec (0x00)
        let cid = Cid::new_v1(0x00, multihash);

        // Encode the CID in base58btc
        let encoded_cid = multibase::encode(Base::Base58Btc, cid.to_bytes());

        format!("Qm{}", encoded_cid)
    }

    /// Get language settings from storage
    fn get_settings(&self, language_address: &str) -> Result<JsonValue, LanguageError> {
        let settings_path = languages_directory()
            .join(language_address)
            .join("settings.json");

        if !settings_path.exists() {
            return Ok(JsonValue::Null);
        }

        let content = fs::read_to_string(&settings_path)?;
        serde_json::from_str(&content).map_err(|e| e.into())
    }

    /// Write language settings to storage
    pub async fn write_settings(
        &self,
        language_address: &str,
        settings: JsonValue,
    ) -> Result<(), LanguageError> {
        let language_dir = languages_directory().join(language_address);
        fs::create_dir_all(&language_dir)?;

        let settings_path = language_dir.join("settings.json");
        let content = serde_json::to_string_pretty(&settings)?;
        fs::write(&settings_path, content)?;

        Ok(())
    }

    /// Shutdown all language runtimes
    pub async fn shutdown(&self) -> Result<(), LanguageError> {
        info!("Shutting down language controller");

        let mut runtimes = self.runtimes.lock().await;

        // Teardown all language runtimes
        for (address, runtime) in runtimes.drain() {
            info!("Shutting down language runtime: {}", address);
            if let Err(e) = runtime.teardown().await {
                error!("Error shutting down language {}: {}", address, e);
            }
        }

        info!("Language controller shut down");
        Ok(())
    }

    /// Save a language bundle to disk, returning (hash, bundle_path)
    pub fn save_language_bundle(
        &self,
        bundle: &str,
        meta: Option<&JsonValue>,
    ) -> Result<(String, PathBuf), LanguageError> {
        let hash = self.calculate_language_hash(bundle);
        let language_dir = languages_directory().join(&hash);
        fs::create_dir_all(&language_dir)?;

        let bundle_path = language_dir.join("bundle.js");
        fs::write(&bundle_path, bundle)?;

        if let Some(meta_value) = meta {
            let meta_path = language_dir.join("meta.json");
            let meta_content = serde_json::to_string_pretty(meta_value)?;
            fs::write(&meta_path, meta_content)?;
        }

        Ok((hash, bundle_path))
    }

    /// Ensure a language bundle is saved on disk, fetching from the language language if needed.
    /// Does NOT spawn a per-language runtime — during the transition period, language operations
    /// are handled by the JS-side LanguageController.
    async fn install_language_from_address(&self, address: &str) -> Result<(), LanguageError> {
        let bundle_path = languages_directory().join(address).join("bundle.js");

        if bundle_path.exists() {
            // Bundle already on disk
            info!("Language bundle already on disk: {}", address);
            return Ok(());
        }

        // Fetch from the language language
        let language_language_address = {
            let sys = self.system_addresses.lock().await;
            sys.language_language
                .clone()
                .ok_or_else(|| LanguageError::LoadError {
                    address: address.to_string(),
                    message: "Language language not loaded yet".to_string(),
                })?
        };

        // Get meta from expressionAdapter.get()
        let meta_script = format!(
            r#"JSON.stringify(await globalThis.__ad4m_language_instance__.expressionAdapter.get("{}"))"#,
            address
        );

        let meta_result = self
            .execute_on_language(&language_language_address, &meta_script)
            .await?;

        let meta_expression: JsonValue =
            serde_json::from_str(&meta_result).map_err(|e| LanguageError::LoadError {
                address: address.to_string(),
                message: format!("Failed to parse language expression: {}", e),
            })?;

        let meta = meta_expression.get("data");

        // Get bundle source from languageAdapter.getLanguageSource()
        let source_script = format!(
            r#"await globalThis.__ad4m_language_instance__.languageAdapter.getLanguageSource("{}")"#,
            address
        );

        let bundle_source = self
            .execute_on_language(&language_language_address, &source_script)
            .await
            .map_err(|e| LanguageError::LoadError {
                address: address.to_string(),
                message: format!("Failed to get language source: {}", e),
            })?;

        if bundle_source.is_empty() {
            return Err(LanguageError::LoadError {
                address: address.to_string(),
                message: "Language source is empty".to_string(),
            });
        }

        let (_hash, _saved_bundle_path) = self.save_language_bundle(&bundle_source, meta)?;
        info!("Saved language bundle for: {}", address);

        Ok(())
    }

    /// Load system languages (language language first, then agent/neighbourhood/perspective)
    pub async fn load_system_languages(
        &self,
        language_language_only: bool,
    ) -> Result<(), LanguageError> {
        // Step 1: Load the language language from the bootstrap seed bundle
        let language_language_bundle =
            RuntimeService::with_global_instance(|rs| rs.get_language_language_bundle());

        let (hash, bundle_path) = self.save_language_bundle(&language_language_bundle, None)?;
        info!("Saved language language bundle, hash={}, loading...", hash);
        self.load_language(bundle_path).await?;
        info!("load_language returned successfully for language language");

        // Store as system language
        {
            let mut sys = self.system_addresses.lock().await;
            sys.language_language = Some(hash.clone());
            sys.system_language_set.insert(hash.clone());
        }

        info!("Language language loaded: {}", hash);

        if !language_language_only {
            // Step 2: Load other system languages
            let agent_language = RuntimeService::with_global_instance(|rs| rs.get_agent_language());
            let neighbourhood_language =
                RuntimeService::with_global_instance(|rs| rs.get_neighbourhood_language());
            let perspective_language =
                RuntimeService::with_global_instance(|rs| rs.get_perspective_language());

            // Install agent language
            if let Err(e) = self.install_language_from_address(&agent_language).await {
                error!(
                    "Failed to install agent language {}: {}",
                    &agent_language, e
                );
            }

            // Install neighbourhood language
            if let Err(e) = self
                .install_language_from_address(&neighbourhood_language)
                .await
            {
                error!(
                    "Failed to install neighbourhood language {}: {}",
                    &neighbourhood_language, e
                );
            }

            // Install perspective language
            if let Err(e) = self
                .install_language_from_address(&perspective_language)
                .await
            {
                error!(
                    "Failed to install perspective language {}: {}",
                    &perspective_language, e
                );
            }

            // Store system addresses
            {
                let mut sys = self.system_addresses.lock().await;
                sys.agent_language = Some(agent_language.clone());
                sys.neighbourhood_language = Some(neighbourhood_language.clone());
                sys.perspective_language = Some(perspective_language.clone());
                sys.system_language_set.insert(agent_language);
                sys.system_language_set.insert(neighbourhood_language);
                sys.system_language_set.insert(perspective_language);
            }

            // Step 3: Preload known link languages
            let known_link_languages =
                RuntimeService::with_global_instance(|rs| rs.get_know_link_languages());
            for lang_address in known_link_languages {
                if let Err(e) = self.install_language_from_address(&lang_address).await {
                    warn!(
                        "Failed to preload known link language {}: {}",
                        lang_address, e
                    );
                }
            }

            // Step 4: Load any other installed languages from disk
            self.load_installed_languages().await?;
        }

        // Signal that languages are ready
        let _ = self.languages_ready_tx.send(true);
        info!("All languages loaded and ready");

        Ok(())
    }

    /// Scan previously installed languages from the languages directory.
    /// During the transition period, this only logs what's found.
    /// The JS-side LanguageController handles loading these into runtimes.
    async fn load_installed_languages(&self) -> Result<(), LanguageError> {
        let langs_dir = languages_directory();
        let system_set = {
            let sys = self.system_addresses.lock().await;
            sys.system_language_set.clone()
        };

        let entries = match fs::read_dir(&langs_dir) {
            Ok(entries) => entries,
            Err(e) => {
                warn!("Could not read languages directory: {}", e);
                return Ok(());
            }
        };

        for entry in entries {
            let entry = match entry {
                Ok(e) => e,
                Err(_) => continue,
            };

            let path = entry.path();
            if !path.is_dir() {
                continue;
            }

            let dir_name = match path.file_name().and_then(|n| n.to_str()) {
                Some(name) => name.to_string(),
                None => continue,
            };

            // Skip system languages (already loaded)
            if system_set.contains(&dir_name) {
                continue;
            }

            let bundle_path = path.join("bundle.js");
            if bundle_path.exists() {
                info!("Found installed language on disk: {}", dir_name);
            }
        }

        Ok(())
    }

    pub async fn install_language(language: Address) -> Result<(), AnyError> {
        let mut controller = Self::global_instance();

        // Wait for system languages to be ready before installing.
        // This ensures the language-language is loaded and can fetch bundles.
        let mut rx = controller.languages_ready_rx.clone();
        while !*rx.borrow() {
            rx.changed()
                .await
                .map_err(|e| AnyError::msg(format!("languages_ready channel closed: {}", e)))?;
        }

        controller
            .install_language_from_address(&language)
            .await
            .map_err(|e| AnyError::msg(format!("{}", e)))?;

        // After saving the bundle to disk, tell the JS-side LanguageController
        // to install/load the language so it's available via language_by_address().
        let script = format!(
            r#"
            (async () => {{
                try {{
                    await core.languageController.installLanguage("{}", null);
                }} catch(e) {{
                    console.error("JS-side installLanguage failed for {}: " + e);
                }}
            }})()
            "#,
            language, language
        );
        if let Err(e) = controller.js_core.execute(script).await {
            log::warn!("Failed to trigger JS-side language install for {}: {}", language, e);
        }

        Ok(())
    }

    pub async fn create_neighbourhood(neighbourhood: Neighbourhood) -> Result<Address, AnyError> {
        Self::create_neighbourhood_with_context(
            neighbourhood,
            &crate::agent::AgentContext::main_agent(),
        )
        .await
    }

    pub async fn create_neighbourhood_with_context(
        neighbourhood: Neighbourhood,
        context: &crate::agent::AgentContext,
    ) -> Result<Address, AnyError> {
        Self::global_instance()
            .js_core
            .execute("await core.waitForLanguages()".into())
            .await?;

        let neighbourhood_json = serde_json::to_string(&neighbourhood)?;

        // Set user context for neighbourhood creation if it's a managed user
        let script = if let Some(user_email) = &context.user_email {
            format!(
                r#"
                (async () => {{
                    const originalContext = core.agentService.getUserContext();
                    core.agentService.setUserContext("{}");
                    try {{
                        return await core
                                .languageController
                                .getNeighbourhoodLanguage()
                                .expressionAdapter
                                .putAdapter
                                .createPublic({});
                    }} finally {{
                        core.agentService.setUserContext(originalContext);
                    }}
                }})()
                "#,
                user_email, neighbourhood_json,
            )
        } else {
            format!(
                r#"
                await core
                        .languageController
                        .getNeighbourhoodLanguage()
                        .expressionAdapter
                        .putAdapter
                        .createPublic({})
                "#,
                neighbourhood_json,
            )
        };

        let result: String = Self::global_instance().js_core.execute(script).await?;
        Ok(result)
    }

    pub async fn get_neighbourhood(
        address: Address,
    ) -> Result<Option<DecoratedNeighbourhoodExpression>, AnyError> {
        Self::global_instance()
            .js_core
            .execute("await core.waitForLanguages()".into())
            .await?;

        let script = format!(
            r#"
            JSON.stringify(
                await core
                    .languageController
                    .getPerspective("{}")
            )
            "#,
            address,
        );
        let result: String = Self::global_instance().js_core.execute(script).await?;
        let neighbourhood: Option<DecoratedNeighbourhoodExpression> =
            serde_json::from_str(&result)?;
        Ok(neighbourhood)
    }

    pub async fn language_by_address(address: Address) -> Result<Option<Language>, AnyError> {
        let controller = Self::global_instance();

        // First check Rust-side runtimes (system languages loaded in per-language Deno runtimes)
        if controller.is_language_loaded(&address).await {
            let language = Language::new(address, controller.js_core.clone());
            return Ok(Some(language));
        }

        // Fall back: check if the language bundle exists on disk.
        // If it does, the language is either already loaded on the JS side
        // (via applyTemplateAndPublish or loadInstalledLanguages) or will be loaded
        // on-demand when accessed. The Language struct delegates all calls to the
        // JS-side LanguageController which handles loading.
        let bundle_path = languages_directory().join(&address).join("bundle.js");
        if bundle_path.exists() {
            log::info!("language_by_address: found bundle on disk for {}", address);
            let language = Language::new(address, controller.js_core.clone());
            return Ok(Some(language));
        }

        Ok(None)
    }
}
