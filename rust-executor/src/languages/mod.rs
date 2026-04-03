mod byte_array;
pub mod error;
pub mod language;
pub mod language_context;
pub mod language_runtime;
pub mod language_runtime_handle;
pub mod literal;

pub use literal::{literal_decode, literal_encode};

use deno_core::error::AnyError;
use std::collections::HashSet;
use std::sync::{Arc, Mutex};

use crate::graphql::graphql_types::{
    DecoratedNeighbourhoodExpression, ExceptionInfo, ExceptionType, InteractionCall,
    InteractionMeta, LanguageLanguageInput, LanguageMeta, LanguageRef, Neighbourhood,
};
use crate::holochain_service::maybe_get_holochain_service;
use crate::pubsub::{get_global_pubsub, EXCEPTION_OCCURRED_TOPIC};
use crate::runtime_service::RuntimeService;
use crate::types::Address;
use crate::{
    agent::{did, did_for_context, signing_key_id_for_context, AgentContext},
    utils::{language_storage_directory, languages_directory},
};
use base64::prelude::*;
use error::LanguageError;
use language::Language;
use language_context::LanguageContext;
use language_runtime_handle::LanguageRuntimeHandle;
use log::{error, info, warn};
use regex::Regex;
use serde_json::Value as JsonValue;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use tokio::sync::Mutex as TokioMutex;

/// Produce a JSON string with keys sorted alphabetically (matches json-stable-stringify)
fn sorted_json_string(val: &JsonValue) -> String {
    match val {
        JsonValue::Object(map) => {
            let sorted: BTreeMap<&String, &JsonValue> = map.iter().collect();
            let sorted_val: serde_json::Map<String, JsonValue> = sorted
                .into_iter()
                .map(|(k, v)| {
                    let v_sorted: JsonValue =
                        serde_json::from_str(&sorted_json_string(v)).unwrap_or_else(|_| v.clone());
                    (k.clone(), v_sorted)
                })
                .collect();
            serde_json::to_string(&JsonValue::Object(sorted_val)).unwrap_or_default()
        }
        JsonValue::Array(arr) => {
            let sorted_items: Vec<JsonValue> = arr
                .iter()
                .map(|v| serde_json::from_str(&sorted_json_string(v)).unwrap_or_else(|_| v.clone()))
                .collect();
            serde_json::to_string(&sorted_items).unwrap_or_default()
        }
        other => serde_json::to_string(other).unwrap_or_default(),
    }
}

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
    // Per-language runtime handles (isolated Deno instances per language)
    runtimes: Arc<TokioMutex<HashMap<String, LanguageRuntimeHandle>>>,

    // System language address tracking
    pub system_addresses: Arc<TokioMutex<SystemLanguageAddresses>>,

    // Language address aliases (e.g. "lang" -> actual address)
    language_aliases: Arc<TokioMutex<HashMap<String, String>>>,

    // Cached language names (address -> name)
    language_names: Arc<TokioMutex<HashMap<String, String>>>,
}

impl LanguageController {
    pub fn init_global_instance() {
        let mut instance = LANGUAGE_CONTROLLER_INSTANCE.lock().unwrap();
        *instance = Some(LanguageController::new());
    }

    pub fn global_instance() -> LanguageController {
        LANGUAGE_CONTROLLER_INSTANCE
            .lock()
            .unwrap()
            .as_ref()
            .expect("LanguageController not initialized")
            .clone()
    }

    fn new() -> Self {
        Self {
            runtimes: Arc::new(TokioMutex::new(HashMap::new())),
            system_addresses: Arc::new(TokioMutex::new(SystemLanguageAddresses::default())),
            language_aliases: Arc::new(TokioMutex::new(HashMap::new())),
            language_names: Arc::new(TokioMutex::new(HashMap::new())),
        }
    }

    /// Return a human-readable label for a language: "name (address)" or just "address".
    async fn language_label(&self, address: &str) -> String {
        let names = self.language_names.lock().await;
        match names.get(address) {
            Some(name) => format!("{} ({})", name, address),
            None => address.to_string(),
        }
    }

    /// Load a language from a bundle path
    ///
    /// Creates a dedicated per-language runtime with isolated Deno worker.
    /// System/bootstrap languages get wider filesystem access (CWD + storage dir).
    pub async fn load_language(
        &self,
        bundle_path: PathBuf,
        is_system_language: bool,
    ) -> Result<String, LanguageError> {
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

        // Spawn dedicated runtime in its own thread (sandboxed to storage_directory)
        let runtime_handle = LanguageRuntimeHandle::spawn(
            language_address.clone(),
            storage_directory.clone(),
            is_system_language,
        )
        .map_err(|e| LanguageError::LoadError {
            address: language_address.clone(),
            message: e,
        })?;

        // Read the language bundle in Rust and pass source code to JS
        // (the JS sandbox should NOT do file operations)
        let bundle_source =
            std::fs::read_to_string(&bundle_path).map_err(|e| LanguageError::LoadError {
                address: language_address.clone(),
                message: format!("Failed to read language bundle {:?}: {}", bundle_path, e),
            })?;
        info!("Loading module for language {}", language_address);
        runtime_handle
            .load_module(bundle_source)
            .await
            .map_err(|e| LanguageError::LoadError {
                address: language_address.clone(),
                message: format!("Failed to load language module: {}", e),
            })?;
        info!("Module loaded for language {}", language_address);

        // Initialize the language with context
        info!("Initializing language {}", language_address);
        runtime_handle
            .load_language(language_context.to_json())
            .await
            .map_err(|e| LanguageError::LoadError {
                address: language_address.clone(),
                message: format!("Failed to initialize language: {}", e),
            })?;

        // Query the language name now that the constructor has run
        let mut runtime_handle = runtime_handle;
        runtime_handle.query_language_name().await;
        let label = runtime_handle.label();

        info!("Language initialized: {}", label);

        // Register callbacks for adapters
        info!("Registering callbacks for {}", label);
        runtime_handle
            .register_callbacks()
            .await
            .map_err(|e| LanguageError::LoadError {
                address: language_address.clone(),
                message: format!("Failed to register callbacks: {}", e),
            })?;
        info!("Callbacks registered for {}", label);

        // Cache the language name for use by other log sites
        if let Some(name) = &runtime_handle.language_name {
            let mut names = self.language_names.lock().await;
            names.insert(language_address.clone(), name.clone());
        }

        // Store the runtime handle
        let mut runtimes = self.runtimes.lock().await;
        runtimes.insert(language_address.clone(), runtime_handle);
        drop(runtimes);

        info!("Successfully loaded language: {}", label);
        Ok(language_address)
    }

    /// Unload a language and clean up
    pub async fn unload_language(&self, language_address: &str) -> Result<(), LanguageError> {
        let label = self.language_label(language_address).await;
        info!("Unloading language: {}", label);

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
        drop(runtimes);

        // Remove cached name
        let mut names = self.language_names.lock().await;
        names.remove(language_address);

        info!("Successfully unloaded language: {}", label);
        Ok(())
    }

    /// Check if a language is loaded
    pub async fn is_language_loaded(&self, language_address: &str) -> bool {
        // Resolve alias forward (e.g. "did" → actual hash)
        let resolved = {
            let aliases = self.language_aliases.lock().await;
            aliases
                .get(language_address)
                .cloned()
                .unwrap_or_else(|| language_address.to_string())
        };
        let runtimes = self.runtimes.lock().await;
        runtimes.contains_key(&resolved)
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
            return handle.execute(script.to_string()).await.map_err(|e| {
                LanguageError::RuntimeError {
                    address: language_address.to_string(),
                    message: e,
                }
            });
        }

        Err(LanguageError::NotFound {
            address: language_address.to_string(),
        })
    }

    /// Execute a script in a language runtime with a specific agent context.
    /// The agent context is passed through the channel so the runtime thread
    /// sets it as the thread-local before executing the script.
    pub async fn execute_on_language_with_context(
        &self,
        language_address: &str,
        script: &str,
        agent_context: &AgentContext,
    ) -> Result<String, LanguageError> {
        let handle = {
            let runtimes = self.runtimes.lock().await;
            runtimes.get(language_address).cloned()
        };

        if let Some(handle) = handle {
            return handle
                .execute_with_context(script.to_string(), agent_context.clone())
                .await
                .map_err(|e| LanguageError::RuntimeError {
                    address: language_address.to_string(),
                    message: e,
                });
        }

        Err(LanguageError::NotFound {
            address: language_address.to_string(),
        })
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

    /// Ensure a language bundle is saved on disk (fetching from the language language if needed)
    /// and then load it into a per-language runtime.
    async fn install_language_from_address(
        &self,
        address: &str,
        is_system_language: bool,
    ) -> Result<(), LanguageError> {
        let bundle_path = languages_directory().join(address).join("bundle.js");

        if bundle_path.exists() {
            log::debug!("Language bundle already on disk: {}", address);
            // Still need to load into runtime if not already loaded
            if !self.is_language_loaded(address).await {
                self.load_language(bundle_path, is_system_language).await?;
            }
            return Ok(());
        }

        log::debug!(
            "install_language_from_address: fetching {} from language-language",
            address
        );

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

        let (hash, saved_bundle_path) = self.save_language_bundle(&bundle_source, meta)?;

        if hash != address {
            error!(
                "install_language_from_address: hash mismatch for {}! Computed: {}",
                address, hash
            );
            return Err(LanguageError::LoadError {
                address: address.to_string(),
                message: format!(
                    "Bundle hash mismatch: expected {} but got {}",
                    address, hash
                ),
            });
        }

        info!("Saved language bundle for: {}", address);

        // Load into a per-language runtime
        self.load_language(saved_bundle_path, is_system_language)
            .await?;
        info!(
            "Loaded language runtime for: {}",
            self.language_label(address).await
        );

        Ok(())
    }

    /// Load system languages (language language first, then agent/neighbourhood/perspective)
    pub async fn load_system_languages(
        &self,
        language_language_only: bool,
    ) -> Result<(), LanguageError> {
        let result = self
            .load_system_languages_inner(language_language_only)
            .await;

        if result.is_ok() {
            info!("All languages loaded and ready");
        } else {
            warn!("System language loading had errors");
        }

        result
    }

    async fn load_system_languages_inner(
        &self,
        language_language_only: bool,
    ) -> Result<(), LanguageError> {
        // Step 1: Load the language language from the bootstrap seed bundle
        let language_language_bundle =
            RuntimeService::with_global_instance(|rs| rs.get_language_language_bundle());

        let (hash, bundle_path) = self.save_language_bundle(&language_language_bundle, None)?;
        info!("Saved language-language bundle, hash={}, loading...", hash);
        self.load_language(bundle_path, true).await?;

        // Store as system language
        {
            let mut sys = self.system_addresses.lock().await;
            sys.language_language = Some(hash.clone());
            sys.system_language_set.insert(hash.clone());
        }

        info!(
            "Language-language loaded: {}",
            self.language_label(&hash).await
        );

        if !language_language_only {
            // Step 2: Load other system languages
            let agent_language = RuntimeService::with_global_instance(|rs| rs.get_agent_language());
            let neighbourhood_language =
                RuntimeService::with_global_instance(|rs| rs.get_neighbourhood_language());
            let perspective_language =
                RuntimeService::with_global_instance(|rs| rs.get_perspective_language());

            // Install agent, neighbourhood, and perspective languages in parallel
            info!(
                "Installing system languages in parallel: agent={}, neighbourhood={}, perspective={}",
                agent_language, neighbourhood_language, perspective_language
            );
            let (agent_result, neighbourhood_result, perspective_result) = tokio::join!(
                self.install_language_from_address(&agent_language, true),
                self.install_language_from_address(&neighbourhood_language, true),
                self.install_language_from_address(&perspective_language, true),
            );
            if let Err(e) = agent_result {
                error!(
                    "Failed to install agent language {}: {}",
                    &agent_language, e
                );
            }
            if let Err(e) = neighbourhood_result {
                error!(
                    "Failed to install neighbourhood language {}: {}",
                    &neighbourhood_language, e
                );
            }
            if let Err(e) = perspective_result {
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
                sys.system_language_set.insert(agent_language.clone());
                sys.system_language_set
                    .insert(neighbourhood_language.clone());
                sys.system_language_set.insert(perspective_language.clone());
            }

            // Register language aliases matching the old JS executor convention
            {
                let mut aliases = self.language_aliases.lock().await;
                aliases.insert("did".to_string(), agent_language);
                aliases.insert("lang".to_string(), hash.clone());
                aliases.insert("neighbourhood".to_string(), neighbourhood_language);
                aliases.insert("perspective".to_string(), perspective_language);
                info!("Registered language aliases: {:?}", *aliases);
            }

            // Step 3: Preload known link languages in parallel
            let known_link_languages =
                RuntimeService::with_global_instance(|rs| rs.get_know_link_languages());
            if !known_link_languages.is_empty() {
                info!(
                    "Installing {} known link languages in parallel",
                    known_link_languages.len()
                );
                let results = futures::future::join_all(
                    known_link_languages
                        .iter()
                        .map(|addr| self.install_language_from_address(addr, true)),
                )
                .await;
                for (addr, result) in known_link_languages.iter().zip(results) {
                    if let Err(e) = result {
                        warn!("Failed to preload known link language {}: {}", addr, e);
                    }
                }
            }

            // Step 4: Load any other installed languages from disk
            if let Err(e) = self.load_installed_languages().await {
                warn!("Failed to load installed languages: {}", e);
            }
        }

        Ok(())
    }

    /// Scan previously installed languages from the languages directory and load them into runtimes.
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

            // Skip temp directory
            if dir_name == "temp" {
                continue;
            }

            let bundle_path = path.join("bundle.js");
            if bundle_path.exists() {
                if self.is_language_loaded(&dir_name).await {
                    continue;
                }
                info!("Loading installed language from disk: {}", dir_name);
                match self.load_language(bundle_path, false).await {
                    Ok(_) => {
                        info!(
                            "Successfully loaded installed language: {}",
                            self.language_label(&dir_name).await
                        );
                    }
                    Err(e) => {
                        warn!("Failed to load language {}: {}", dir_name, e);
                    }
                }
            }
        }

        Ok(())
    }

    pub async fn install_language(language: Address) -> Result<(), AnyError> {
        log::debug!("install_language called for: {}", language);
        let controller = Self::global_instance();

        // Check if the language is already loaded in Rust runtimes
        if controller.is_language_loaded(&language).await {
            log::debug!("install_language: {} already loaded", language);
            return Ok(());
        }

        let language_dir = languages_directory().join(&language);
        let meta_path = language_dir.join("meta.json");
        let bundle_path = language_dir.join("bundle.js");

        // Check if the language language is loaded on the Rust side
        let language_language_address = {
            let sys = controller.system_addresses.lock().await;
            sys.language_language.clone()
        };

        // Get meta: from disk or from language language with retry
        let meta: Option<JsonValue> = if meta_path.exists() {
            let content = fs::read_to_string(&meta_path)?;
            Some(serde_json::from_str(&content)?)
        } else if let Some(ref ll_addr) = language_language_address {
            // Fetch from language language with retry logic (up to 10 retries)
            let meta_script = format!(
                r#"JSON.stringify(await globalThis.__ad4m_language_instance__.expressionAdapter.get("{}"))"#,
                language
            );

            let mut meta_result = None;
            for retry in 0..10 {
                match controller.execute_on_language(ll_addr, &meta_script).await {
                    Ok(result) => {
                        if let Ok(val) = serde_json::from_str::<JsonValue>(&result) {
                            if !val.is_null() {
                                meta_result = Some(val);
                                break;
                            }
                        }
                    }
                    Err(e) => {
                        error!(
                            "Error getting language meta from language language: {}\nRetrying...",
                            e
                        );
                    }
                }
                tokio::time::sleep(std::time::Duration::from_millis(5000 * (retry + 1))).await;
            }

            meta_result.and_then(|expr| expr.get("data").cloned())
        } else {
            None
        };

        // Get bundle source: from disk or from language language
        let source = if bundle_path.exists() {
            Some(fs::read_to_string(&bundle_path)?)
        } else if let Some(ref ll_addr) = language_language_address {
            let source_script = format!(
                r#"await globalThis.__ad4m_language_instance__.languageAdapter.getLanguageSource("{}")"#,
                language
            );

            match controller
                .execute_on_language(ll_addr, &source_script)
                .await
            {
                Ok(s) if !s.is_empty() => Some(s),
                Ok(_) => None,
                Err(e) => {
                    warn!(
                        "Error getting language source from language language: {}",
                        e
                    );
                    None
                }
            }
        } else {
            None
        };

        // If we have source, save and load into a per-language runtime
        if let Some(source) = source {
            // Compute hash and verify it matches the expected address
            let hash = controller.calculate_language_hash(&source);
            if hash != language {
                error!("install_language: COULDN'T VERIFY HASH OF LANGUAGE!");
                error!("install_language: Address: {}", language);
                error!("install_language: Computed hash: {}", hash);
                error!("install_language: LANGUAGE WILL BE IGNORED");
                return Ok(());
            }

            // Save language bundle to disk
            let (_saved_hash, saved_bundle_path) =
                controller.save_language_bundle(&source, meta.as_ref())?;
            info!("install_language: saved language bundle for {}", language);

            // Load into a per-language Deno runtime
            controller
                .load_language(saved_bundle_path, false)
                .await
                .map_err(|e| {
                    anyhow::anyhow!(
                        "install_language: failed to load language runtime for {}: {}",
                        language,
                        e
                    )
                })?;
            info!("install_language: loaded language runtime for {}", language);
        } else {
            return Err(anyhow::anyhow!(
                "install_language: no source available for {}",
                language
            ));
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
        let controller = Self::global_instance();
        let neighbourhood_address = {
            let sys = controller.system_addresses.lock().await;
            sys.neighbourhood_language
                .clone()
                .ok_or_else(|| deno_core::anyhow::anyhow!("Neighbourhood language not loaded"))?
        };

        let neighbourhood_json = serde_json::to_string(&neighbourhood)?;
        let content: JsonValue = serde_json::from_str(&neighbourhood_json)?;

        let result = controller
            .expression_create(&neighbourhood_address, content, context)
            .await
            .map_err(|e| deno_core::anyhow::anyhow!("Failed to create neighbourhood: {}", e))?;

        // expression_create returns a full URL like "langAddr://exprAddr"
        // We need to return just the expression address part
        let expr_addr = if let Some(idx) = result.find("://") {
            result[idx + 3..].to_string()
        } else {
            result
        };

        Ok(expr_addr)
    }

    pub async fn get_neighbourhood(
        address: Address,
    ) -> Result<Option<DecoratedNeighbourhoodExpression>, AnyError> {
        let controller = Self::global_instance();
        let neighbourhood_address = {
            let sys = controller.system_addresses.lock().await;
            sys.neighbourhood_language
                .clone()
                .ok_or_else(|| deno_core::anyhow::anyhow!("Neighbourhood language not loaded"))?
        };

        match controller
            .get_expression(&neighbourhood_address, &address)
            .await
        {
            Ok(Some(expr_json)) => {
                let neighbourhood: Option<DecoratedNeighbourhoodExpression> =
                    serde_json::from_value(expr_json)?;
                Ok(neighbourhood)
            }
            Ok(None) => Ok(None),
            Err(e) => Err(deno_core::anyhow::anyhow!(
                "Failed to get neighbourhood: {}",
                e
            )),
        }
    }

    pub async fn language_by_address(address: Address) -> Result<Option<Language>, AnyError> {
        let controller = Self::global_instance();

        // Only return a Language if the runtime is actually loaded and can execute.
        // On the old JS-executor branch, a single JsCore worker could load languages on-demand,
        // so checking for the bundle on disk was sufficient. With per-language isolated runtimes,
        // the runtime must be explicitly started before the language can be used.
        if controller.is_language_loaded(&address).await {
            return Ok(Some(Language::new(address)));
        }

        Ok(None)
    }

    /// Resolve a language for an expression reference, resolving aliases first
    pub async fn language_for_expression(&self, address: &str) -> Result<Language, LanguageError> {
        // Resolve alias if present
        let resolved_address = {
            let aliases = self.language_aliases.lock().await;
            aliases
                .get(address)
                .cloned()
                .unwrap_or_else(|| address.to_string())
        };

        // Look up in runtimes
        let runtimes = self.runtimes.lock().await;
        if runtimes.contains_key(&resolved_address) {
            return Ok(Language::new(resolved_address));
        }

        Err(LanguageError::NotFound {
            address: resolved_address,
        })
    }

    /// Apply template data to source language lines.
    /// Port of JS applyTemplateData method.
    fn apply_template_data(
        source_lines: &mut Vec<String>,
        template_data: &serde_json::Map<String, JsonValue>,
    ) {
        let ad4m_template_pattern = "//!@ad4m-template-variable";

        // Find all indexes where the template marker appears
        let indexes: Vec<usize> = source_lines
            .iter()
            .enumerate()
            .filter(|(_, line)| line.contains(ad4m_template_pattern))
            .map(|(i, _)| i)
            .collect();

        // Variable declaration patterns
        let patterns = [
            Regex::new(r"var ([a-zA-Z0-9_-]+)").unwrap(),
            Regex::new(r"const ([a-zA-Z0-9_-]+)").unwrap(),
            Regex::new(r"let ([a-zA-Z0-9_-]+)").unwrap(),
        ];

        // Process each template marker
        for &marker_index in &indexes {
            let variable_index = marker_index + 1;
            if variable_index >= source_lines.len() {
                continue;
            }

            let variable_line = source_lines[variable_index].clone();

            for pattern in &patterns {
                if let Some(captures) = pattern.captures(&variable_line) {
                    let full_match = captures.get(0).unwrap().as_str();
                    let parts: Vec<&str> = full_match.splitn(2, ' ').collect();
                    if parts.len() != 2 {
                        continue;
                    }
                    let variable_type = parts[0];
                    let variable_name = parts[1];

                    if let Some(value) = template_data.get(variable_name) {
                        let replacement = match value {
                            JsonValue::String(s) => {
                                format!("{} {} = \"{}\"", variable_type, variable_name, s)
                            }
                            other => {
                                format!("{} {} = {}", variable_type, variable_name, other)
                            }
                        };
                        source_lines[variable_index] = replacement;
                    }
                }
            }
        }

        // Handle special case for `var happ = "..."`
        if let Some(happ_value) = template_data.get("happ") {
            info!("applying happ template data...");
            let happ_pattern = Regex::new(r"var (happ+)").unwrap();
            let mut happ_index: Option<usize> = None;
            for (i, line) in source_lines.iter().enumerate() {
                if happ_pattern.is_match(line) {
                    happ_index = Some(i);
                }
            }
            if let (Some(idx), JsonValue::String(happ_str)) = (happ_index, happ_value) {
                info!("happIndex: {}", idx);
                source_lines[idx] = format!("var happ = \"{}\"", happ_str);
            }
        }
    }

    /// Read and template a Holochain DNA from language source lines.
    /// Port of JS readAndTemplateHolochainDNA method.
    async fn read_and_template_holochain_dna(
        &self,
        source_lines: &[String],
        template_data: &serde_json::Map<String, JsonValue>,
        source_language_hash: &str,
    ) -> Result<Option<String>, LanguageError> {
        // Look for `var happ = ` in source lines
        let happ_index = source_lines
            .iter()
            .position(|line| line.contains("var happ = "));

        let happ_index = match happ_index {
            Some(idx) => idx,
            None => return Ok(None),
        };

        // Create temp directory for DNA templating operations
        let temp_templating_path = std::env::temp_dir().join(source_language_hash);
        if temp_templating_path.exists() {
            let _ = fs::remove_dir_all(&temp_templating_path);
        }
        fs::create_dir_all(&temp_templating_path)?;

        let temp_happ_path = temp_templating_path.join("happ.happ");

        // Extract base64-encoded happ from the line
        let happ_line = &source_lines[happ_index];
        let happ_code = happ_line.split("var happ = ").nth(1).unwrap_or("");

        // Strip leading `"` and trailing `";`
        let happ_code = happ_code.trim();
        let happ_code = if happ_code.starts_with('"') && happ_code.ends_with("\";") {
            &happ_code[1..happ_code.len() - 2]
        } else if happ_code.starts_with('"') && happ_code.ends_with('"') {
            &happ_code[1..happ_code.len() - 1]
        } else {
            happ_code
        };

        // Decode base64 and write to temp file
        let happ_bytes =
            BASE64_STANDARD
                .decode(happ_code)
                .map_err(|e| LanguageError::InvalidBundle {
                    message: format!("Failed to decode happ base64: {}", e),
                })?;
        fs::write(&temp_happ_path, &happ_bytes)?;

        // Unpack hApp bundle
        info!("readAndTemplateHolochainDna: unpacking hApp bundle");
        let holochain_service =
            maybe_get_holochain_service()
                .await
                .ok_or_else(|| LanguageError::RuntimeError {
                    address: source_language_hash.to_string(),
                    message: "Holochain service not available".to_string(),
                })?;

        let unpack_happ_path = holochain_service
            .unpack_happ(temp_happ_path.to_string_lossy().to_string())
            .await
            .map_err(|e| LanguageError::RuntimeError {
                address: source_language_hash.to_string(),
                message: format!("Failed to unpack hApp: {}", e),
            })?;
        let unpack_happ_path = unpack_happ_path.trim().to_string();

        // Delete the .happ file after unpacking
        let _ = fs::remove_file(&temp_happ_path);

        // Read happ.yaml
        let happ_yaml_path = PathBuf::from(&unpack_happ_path).join("happ.yaml");
        if !happ_yaml_path.exists() {
            return Err(LanguageError::InvalidBundle {
                message: format!(
                    "Expected to find happ.yaml at {} after unpacking but could not find it",
                    happ_yaml_path.display()
                ),
            });
        }

        let happ_yaml_content = fs::read_to_string(&happ_yaml_path)?;
        let happ_yaml: JsonValue = serde_yaml::from_str(&happ_yaml_content).map_err(|e| {
            LanguageError::SerializationError {
                message: format!("Failed to parse happ.yaml: {}", e),
            }
        })?;

        // Extract roles[0].dna.path
        let dna_rel_path = happ_yaml
            .get("roles")
            .and_then(|r| r.get(0))
            .and_then(|r| r.get("dna"))
            .and_then(|d| d.get("path"))
            .and_then(|p| p.as_str())
            .ok_or_else(|| LanguageError::InvalidBundle {
                message: "Could not find roles[0].dna.path in happ.yaml".to_string(),
            })?;

        let dna_bundle_path = PathBuf::from(&unpack_happ_path).join(dna_rel_path);

        // Unpack DNA
        info!("readAndTemplateHolochainDna: unpacking DNA");
        let unpack_dna_path = holochain_service
            .unpack_dna(dna_bundle_path.to_string_lossy().to_string())
            .await
            .map_err(|e| LanguageError::RuntimeError {
                address: source_language_hash.to_string(),
                message: format!("Failed to unpack DNA: {}", e),
            })?;
        let unpack_dna_path = unpack_dna_path.trim().to_string();

        // Read dna.yaml
        let dna_yaml_path = PathBuf::from(&unpack_dna_path).join("dna.yaml");
        if !dna_yaml_path.exists() {
            return Err(LanguageError::InvalidBundle {
                message: format!(
                    "Expected to find dna.yaml at {} after unpacking but could not find it",
                    dna_yaml_path.display()
                ),
            });
        }

        let dna_yaml_content = fs::read_to_string(&dna_yaml_path)?;
        let mut dna_yaml: JsonValue = serde_yaml::from_str(&dna_yaml_content).map_err(|e| {
            LanguageError::SerializationError {
                message: format!("Failed to parse dna.yaml: {}", e),
            }
        })?;

        // Apply template data to DNA yaml
        if let Some(uid) = template_data.get("uid") {
            if let Some(integrity) = dna_yaml.get_mut("integrity") {
                integrity["network_seed"] = uid.clone();
            }
        }

        // Set properties for all template keys
        for (key, value) in template_data {
            if let Some(integrity) = dna_yaml.get_mut("integrity") {
                if integrity.get("properties").is_none() {
                    integrity["properties"] = JsonValue::Object(serde_json::Map::new());
                }
                if let Some(props) = integrity.get_mut("properties") {
                    props[key] = value.clone();
                }
            }
        }

        // Write modified dna.yaml back
        let dna_yaml_dump =
            serde_yaml::to_string(&dna_yaml).map_err(|e| LanguageError::SerializationError {
                message: format!("Failed to serialize dna.yaml: {}", e),
            })?;
        fs::write(&dna_yaml_path, &dna_yaml_dump)?;

        // Pack DNA
        info!("readAndTemplateHolochainDna: packing DNA");
        let pack_dna_path = holochain_service
            .pack_dna(unpack_dna_path.clone())
            .await
            .map_err(|e| LanguageError::RuntimeError {
                address: source_language_hash.to_string(),
                message: format!("Failed to pack DNA: {}", e),
            })?;
        let pack_dna_path = pack_dna_path.trim().to_string();

        // Copy packed DNA back into happ directory
        let pack_dna_filename = PathBuf::from(&pack_dna_path)
            .file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string();
        let target = PathBuf::from(&unpack_happ_path).join(&pack_dna_filename);
        info!(
            "readAndTemplateHolochainDna: copying packed dna back to happ directory: {}",
            target.display()
        );
        fs::copy(&pack_dna_path, &target)?;

        // Pack hApp bundle
        info!("readAndTemplateHolochainDna: packing hApp bundle");
        let pack_happ_path = holochain_service
            .pack_happ(unpack_happ_path.clone())
            .await
            .map_err(|e| LanguageError::RuntimeError {
                address: source_language_hash.to_string(),
                message: format!("Failed to pack hApp: {}", e),
            })?;
        let pack_happ_path = pack_happ_path.trim().to_string();

        // Read packed happ as base64
        let happ_bytes = fs::read(&pack_happ_path)?;
        let base64_string = BASE64_STANDARD.encode(&happ_bytes);

        // Cleanup temp directory
        let _ = fs::remove_dir_all(&temp_templating_path);

        Ok(Some(base64_string))
    }

    /// Apply template on source language and return the resulting LanguageLanguageInput.
    /// Port of JS languageApplyTemplateOnSource method.
    pub async fn language_apply_template_on_source(
        &self,
        source_language_hash: &str,
        mut template_data: serde_json::Map<String, JsonValue>,
    ) -> Result<LanguageLanguageInput, LanguageError> {
        let language_language_address = {
            let sys = self.system_addresses.lock().await;
            sys.language_language
                .clone()
                .ok_or_else(|| LanguageError::LoadError {
                    address: source_language_hash.to_string(),
                    message: "Language language not loaded".to_string(),
                })?
        };

        // Get the language expression (meta)
        let meta_script = format!(
            r#"JSON.stringify(await globalThis.__ad4m_language_instance__.expressionAdapter.get("{}"))"#,
            source_language_hash
        );
        let meta_result = self
            .execute_on_language(&language_language_address, &meta_script)
            .await?;

        let meta_expression: JsonValue =
            serde_json::from_str(&meta_result).map_err(|e| LanguageError::SerializationError {
                message: format!("Failed to parse language expression: {}", e),
            })?;

        if meta_expression.is_null() {
            return Err(LanguageError::NotFound {
                address: source_language_hash.to_string(),
            });
        }

        // Get the language source
        let source_script = format!(
            r#"await globalThis.__ad4m_language_instance__.languageAdapter.getLanguageSource("{}")"#,
            source_language_hash
        );
        let source_language = self
            .execute_on_language(&language_language_address, &source_script)
            .await?;

        if source_language.is_empty() {
            return Err(LanguageError::LoadError {
                address: source_language_hash.to_string(),
                message: "Could not get source language".to_string(),
            });
        }

        let mut source_lines: Vec<String> = source_language.split('\n').map(String::from).collect();

        // Sort template_data keys (equivalent to JS orderObject)
        let sorted_data: serde_json::Map<String, JsonValue> = {
            let mut keys: Vec<String> = template_data.keys().cloned().collect();
            keys.sort();
            let mut sorted = serde_json::Map::new();
            for key in keys {
                if let Some(val) = template_data.remove(&key) {
                    sorted.insert(key, val);
                }
            }
            sorted
        };
        template_data = sorted_data;

        // Read and template Holochain DNA
        let happ_code = self
            .read_and_template_holochain_dna(&source_lines, &template_data, source_language_hash)
            .await?;

        if let Some(happ_code) = happ_code {
            info!("setting happCode in templateData");
            template_data.insert("happ".to_string(), JsonValue::String(happ_code));
        }

        // Re-sort after potentially adding "happ"
        let sorted_data: serde_json::Map<String, JsonValue> = {
            let mut keys: Vec<String> = template_data.keys().cloned().collect();
            keys.sort();
            let mut sorted = serde_json::Map::new();
            for key in keys {
                if let Some(val) = template_data.remove(&key) {
                    sorted.insert(key, val);
                }
            }
            sorted
        };
        template_data = sorted_data;

        // Apply template data to source lines
        Self::apply_template_data(&mut source_lines, &template_data);

        // Remove happ from template_data before storing in meta.
        // Use shift_remove (not remove/swap_remove) to preserve sorted key order.
        template_data.shift_remove("happ");

        // Build the updated LanguageMeta
        let meta_data = meta_expression
            .get("data")
            .cloned()
            .unwrap_or(JsonValue::Object(serde_json::Map::new()));

        // If data is a string, parse it as JSON first
        let meta_data = if let JsonValue::String(s) = &meta_data {
            serde_json::from_str::<JsonValue>(s).unwrap_or(meta_data)
        } else {
            meta_data
        };

        let mut meta: LanguageMeta = match serde_json::from_value(meta_data.clone()) {
            Ok(m) => m,
            Err(e) => {
                warn!(
                    "Failed to deserialize LanguageMeta from expression data: {}. Data: {:?}",
                    e,
                    &meta_data.to_string()[..meta_data.to_string().len().min(500)]
                );
                LanguageMeta::default()
            }
        };

        // Override name and description if present in template_data
        if let Some(JsonValue::String(name)) = template_data.get("name") {
            meta.name = name.clone();
        }
        if let Some(JsonValue::String(desc)) = template_data.get("description") {
            meta.description = Some(desc.clone());
        }

        meta.template_applied_params =
            Some(serde_json::to_string(&JsonValue::Object(template_data)).unwrap_or_default());
        meta.template_source_language_address = Some(source_language_hash.to_string());

        // Compute hash of the joined lines
        let language_data = source_lines.join("\n");
        let language_hash = self.calculate_language_hash(&language_data);
        meta.address = language_hash;

        Ok(LanguageLanguageInput {
            bundle: language_data,
            meta,
        })
    }

    /// Remove a language: unload runtime, remove Holochain app, delete files.
    /// Port of JS languageRemove method.
    pub async fn language_remove(&mut self, address: &str) -> Result<(), LanguageError> {
        // Teardown the per-language Rust runtime (if loaded there).
        // Errors here (e.g. runtime not loaded, teardown failure) must not abort
        // the rest of the removal – the JS version always continued to remove the
        // Holochain app and delete the language directory regardless.
        if let Err(e) = self.unload_language(address).await {
            warn!(
                "language_remove: unload_language failed for {}: {} – continuing with cleanup",
                address, e
            );
        }

        // Remove Holochain DNA for this language
        if let Some(holochain_service) = maybe_get_holochain_service().await {
            match holochain_service.remove_app(address.to_string()).await {
                Ok(()) => {
                    info!("Removed Holochain app for language {}", address);
                }
                Err(e) => {
                    warn!("No DNA found for language {}: {}", address, e);
                }
            }
        }

        // Remove language files from disk
        let language_path = languages_directory().join(address);
        if let Err(e) = fs::remove_dir_all(&language_path) {
            warn!(
                "Failed to remove language directory {}: {}",
                language_path.display(),
                e
            );
        }

        Ok(())
    }

    /// Get a language by reference, installing it if necessary.
    /// Implements trust verification for untrusted authors.
    /// Port of JS languageByRef method.
    pub async fn language_by_ref(&self, address: &str) -> Result<Language, LanguageError> {
        // Resolve alias if present
        let resolved_address = {
            let aliases = self.language_aliases.lock().await;
            aliases
                .get(address)
                .cloned()
                .unwrap_or_else(|| address.to_string())
        };

        // Check if already loaded
        if self.is_language_loaded(&resolved_address).await {
            return Ok(Language::new(resolved_address));
        }

        // Get the language language address
        let language_language_address = {
            let sys = self.system_addresses.lock().await;
            sys.language_language
                .clone()
                .ok_or_else(|| LanguageError::LoadError {
                    address: resolved_address.clone(),
                    message: "Language language not loaded".to_string(),
                })?
        };

        // Fetch language expression (meta) from language language
        let meta_script = format!(
            r#"JSON.stringify(await globalThis.__ad4m_language_instance__.expressionAdapter.get("{}"))"#,
            resolved_address
        );
        let meta_result = self
            .execute_on_language(&language_language_address, &meta_script)
            .await?;

        let mut language_meta: JsonValue =
            serde_json::from_str(&meta_result).map_err(|e| LanguageError::SerializationError {
                message: format!("Failed to parse language meta: {}", e),
            })?;

        if language_meta.is_null() {
            return Err(LanguageError::NotFound {
                address: resolved_address,
            });
        }

        // Verify the expression proof (sets proof.valid / proof.invalid)
        Self::verify_expression_proof(&mut language_meta);

        // Validate proof
        let proof_valid = language_meta
            .get("proof")
            .and_then(|p| p.get("valid"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false);

        if !proof_valid {
            return Err(LanguageError::LoadError {
                address: resolved_address,
                message: "Language to be installed does not have valid proof".to_string(),
            });
        }

        let language_author = language_meta
            .get("author")
            .and_then(|a| a.as_str())
            .unwrap_or("")
            .to_string();

        let language_meta_data = language_meta
            .get("data")
            .cloned()
            .unwrap_or(JsonValue::Object(serde_json::Map::new()));

        // If data is a string, parse it as JSON
        let language_meta_data = if let JsonValue::String(s) = &language_meta_data {
            serde_json::from_str::<JsonValue>(s).unwrap_or(language_meta_data)
        } else {
            language_meta_data
        };

        // Get trusted agents
        let trusted_agents = RuntimeService::with_global_instance(|rs| rs.get_trusted_agents());
        let agent_did = did();

        // Check if author is trusted
        if trusted_agents.contains(&language_author) || agent_did == language_author {
            // Trusted author path: fetch source, verify hash, install
            let source_script = format!(
                r#"await globalThis.__ad4m_language_instance__.languageAdapter.getLanguageSource("{}")"#,
                resolved_address
            );
            let language_source = self
                .execute_on_language(&language_language_address, &source_script)
                .await?;

            if language_source.is_empty() {
                return Err(LanguageError::LoadError {
                    address: resolved_address,
                    message: "Could not get language source".to_string(),
                });
            }

            let language_hash = self.calculate_language_hash(&language_source);

            let meta_address = language_meta_data
                .get("address")
                .and_then(|a| a.as_str())
                .unwrap_or("");

            if meta_address.is_empty() {
                return Err(LanguageError::LoadError {
                    address: resolved_address.clone(),
                    message: format!(
                        "Could not find 'address' value inside languageMetaData: {:?}",
                        language_meta_data
                    ),
                });
            }

            if language_hash != meta_address {
                return Err(LanguageError::LoadError {
                    address: resolved_address,
                    message: "Calculated language hash did not match address in meta".to_string(),
                });
            }

            Self::install_language(resolved_address.clone())
                .await
                .map_err(|e| LanguageError::LoadError {
                    address: resolved_address.clone(),
                    message: format!("Failed to install language: {}", e),
                })?;

            Ok(Language::new(resolved_address))
        } else {
            // Untrusted author path: verify template params
            let template_applied_params = language_meta_data
                .get("templateAppliedParams")
                .or_else(|| language_meta_data.get("template_applied_params"))
                .and_then(|v| v.as_str())
                .unwrap_or("");

            let template_source_language_address = language_meta_data
                .get("templateSourceLanguageAddress")
                .or_else(|| language_meta_data.get("template_source_language_address"))
                .and_then(|v| v.as_str())
                .unwrap_or("");

            if template_applied_params.is_empty() || template_source_language_address.is_empty() {
                // Ensure sourceCodeLink is present (as null) to match json-stable-stringify output
                let mut meta_for_error = language_meta_data.clone();
                if let Some(obj) = meta_for_error.as_object_mut() {
                    obj.entry("sourceCodeLink").or_insert(JsonValue::Null);
                }
                let meta_json = sorted_json_string(&meta_for_error);
                let err_msg = format!(
                    "Language not created by trusted agent: {} and is not templated... aborting language install. Language metadata: {}",
                    language_author, meta_json
                );
                error!("{}", err_msg);

                let exception = ExceptionInfo {
                    title: "Failed to install language".to_string(),
                    message: err_msg.clone(),
                    r#type: ExceptionType::AgentIsUntrusted,
                    addon: Some(language_author),
                };
                get_global_pubsub()
                    .await
                    .publish(
                        &EXCEPTION_OCCURRED_TOPIC,
                        &serde_json::to_string(&exception).unwrap_or_default(),
                    )
                    .await;

                return Err(LanguageError::LoadError {
                    address: resolved_address,
                    message: err_msg,
                });
            }

            // Get source language meta and verify its author is trusted
            let source_meta_script = format!(
                r#"JSON.stringify(await globalThis.__ad4m_language_instance__.expressionAdapter.get("{}"))"#,
                template_source_language_address
            );
            let source_meta_result = self
                .execute_on_language(&language_language_address, &source_meta_script)
                .await?;

            let source_language_meta: JsonValue = serde_json::from_str(&source_meta_result)
                .map_err(|e| LanguageError::SerializationError {
                    message: format!("Failed to parse source language meta: {}", e),
                })?;

            let source_author = source_language_meta
                .get("author")
                .and_then(|a| a.as_str())
                .unwrap_or("")
                .to_string();

            if !trusted_agents.contains(&source_author) {
                let err_msg = "Agent which created source language for language trying to be installed is not a trustedAgent... aborting language install".to_string();

                let exception = ExceptionInfo {
                    title: "Failed to install language".to_string(),
                    message: err_msg.clone(),
                    r#type: ExceptionType::AgentIsUntrusted,
                    addon: Some(source_author),
                };
                get_global_pubsub()
                    .await
                    .publish(
                        &EXCEPTION_OCCURRED_TOPIC,
                        &serde_json::to_string(&exception).unwrap_or_default(),
                    )
                    .await;

                return Err(LanguageError::LoadError {
                    address: resolved_address,
                    message: err_msg,
                });
            }

            // Apply template on source and verify hash
            let template_params: serde_json::Map<String, JsonValue> =
                serde_json::from_str(template_applied_params).map_err(|e| {
                    LanguageError::SerializationError {
                        message: format!("Failed to parse template params: {}", e),
                    }
                })?;

            let templated_input = self
                .language_apply_template_on_source(
                    template_source_language_address,
                    template_params,
                )
                .await?;

            // Fetch actual source of the language to install
            let source_script = format!(
                r#"await globalThis.__ad4m_language_instance__.languageAdapter.getLanguageSource("{}")"#,
                resolved_address
            );
            let language_source = self
                .execute_on_language(&language_language_address, &source_script)
                .await?;

            if language_source.is_empty() {
                return Err(LanguageError::LoadError {
                    address: resolved_address,
                    message: "Could not get language source".to_string(),
                });
            }

            let language_hash = self.calculate_language_hash(&language_source);

            if templated_input.meta.address != language_hash {
                return Err(LanguageError::LoadError {
                    address: resolved_address,
                    message: format!(
                        "Templating of original source language did not result in the same language hash. Expected: {}. Got: {}",
                        language_hash, templated_input.meta.address
                    ),
                });
            }

            Self::install_language(resolved_address.clone())
                .await
                .map_err(|e| LanguageError::LoadError {
                    address: resolved_address.clone(),
                    message: format!("Failed to install language: {}", e),
                })?;

            Ok(Language::new(resolved_address))
        }
    }

    // ─── Parse expression URL utility ───────────────────────────────────

    /// Parse an expression URL of the form `<scheme>://<path>` into (scheme, path).
    /// The scheme is the language address; the path is the expression address.
    /// Handles the `literal://` case where the path may contain `://` itself.
    pub fn parse_expr_url(url: &str) -> Result<(String, String), LanguageError> {
        if let Some(rest) = url.strip_prefix("literal://") {
            return Ok(("literal".to_string(), rest.to_string()));
        }

        // DID URLs: "did:key:z6Mk..." -> language alias "did", expression address is the full DID
        if url.starts_with("did:") {
            return Ok(("did".to_string(), url.to_string()));
        }

        match url.find("://") {
            Some(idx) => {
                let scheme = &url[..idx];
                let path = &url[idx + 3..];
                Ok((scheme.to_string(), path.to_string()))
            }
            None => Err(LanguageError::InvalidBundle {
                message: format!("Invalid expression URL (missing ://): {}", url),
            }),
        }
    }

    // ─── Query/getter methods ───────────────────────────────────────────

    /// Get installed languages, optionally filtered by a property name.
    pub async fn get_installed_languages(&self, filter: Option<&str>) -> Vec<LanguageRef> {
        let runtimes = self.runtimes.lock().await;
        let names = self.language_names.lock().await;
        let mut result = Vec::new();

        for (address, handle) in runtimes.iter() {
            // Check filter if provided
            if let Some(prop) = filter {
                let check_script = format!(
                    r#"JSON.stringify(Object.keys(language).includes("{}"))"#,
                    prop
                );
                match handle.execute(check_script).await {
                    Ok(res) => {
                        let trimmed = res.trim().trim_matches('"');
                        if trimmed != "true" {
                            continue;
                        }
                    }
                    Err(e) => {
                        warn!(
                            "Failed to check filter '{}' on language {}: {}",
                            prop, address, e
                        );
                        continue;
                    }
                }
            }

            let name = names.get(address).cloned().unwrap_or_default();

            result.push(LanguageRef {
                address: address.clone(),
                name,
            });
        }

        result
    }

    /// Get language expression (meta) from the language language.
    pub async fn get_language_expression(
        &self,
        address: &str,
    ) -> Result<LanguageMeta, LanguageError> {
        let language_language_address = {
            let sys = self.system_addresses.lock().await;
            sys.language_language
                .clone()
                .ok_or_else(|| LanguageError::LoadError {
                    address: address.to_string(),
                    message: "Language language not loaded".to_string(),
                })?
        };

        let meta_script = format!(
            r#"JSON.stringify(await globalThis.__ad4m_language_instance__.expressionAdapter.get("{}"))"#,
            address
        );

        let meta_result = self
            .execute_on_language(&language_language_address, &meta_script)
            .await?;

        let meta_expression: JsonValue =
            serde_json::from_str(&meta_result).map_err(|e| LanguageError::SerializationError {
                message: format!("Failed to parse language expression: {}", e),
            })?;

        if meta_expression.is_null() {
            return Err(LanguageError::NotFound {
                address: address.to_string(),
            });
        }

        // Extract author from the expression envelope (not from data)
        let author = meta_expression
            .get("author")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        let data = meta_expression
            .get("data")
            .cloned()
            .unwrap_or(JsonValue::Object(serde_json::Map::new()));

        // If data is a string, parse it as JSON first
        let data = if let JsonValue::String(s) = &data {
            serde_json::from_str::<JsonValue>(s).unwrap_or(data)
        } else {
            data
        };

        let mut meta: LanguageMeta = serde_json::from_value(data).unwrap_or_default();
        // Override author from the expression envelope
        meta.author = author;
        // Set the address from the query parameter
        meta.address = address.to_string();
        // Default templated to false if not present in the data
        if meta.templated.is_none() {
            meta.templated = Some(false);
        }
        Ok(meta)
    }

    /// Get language source from the language language.
    pub async fn get_language_source(&self, address: &str) -> Result<String, LanguageError> {
        let language_language_address = {
            let sys = self.system_addresses.lock().await;
            sys.language_language
                .clone()
                .ok_or_else(|| LanguageError::LoadError {
                    address: address.to_string(),
                    message: "Language language not loaded".to_string(),
                })?
        };

        let source_script = format!(
            r#"await globalThis.__ad4m_language_instance__.languageAdapter.getLanguageSource("{}")"#,
            address
        );

        self.execute_on_language(&language_language_address, &source_script)
            .await
    }

    /// Get the agent language
    pub async fn get_agent_language(&self) -> Result<Language, LanguageError> {
        let sys = self.system_addresses.lock().await;
        let address = sys
            .agent_language
            .clone()
            .ok_or_else(|| LanguageError::NotFound {
                address: "agent_language".to_string(),
            })?;
        Ok(Language::new(address))
    }

    /// Get the language language
    pub async fn get_language_language(&self) -> Result<Language, LanguageError> {
        let sys = self.system_addresses.lock().await;
        let address = sys
            .language_language
            .clone()
            .ok_or_else(|| LanguageError::NotFound {
                address: "language_language".to_string(),
            })?;
        Ok(Language::new(address))
    }

    /// Get the neighbourhood language
    pub async fn get_neighbourhood_language(&self) -> Result<Language, LanguageError> {
        let sys = self.system_addresses.lock().await;
        let address =
            sys.neighbourhood_language
                .clone()
                .ok_or_else(|| LanguageError::NotFound {
                    address: "neighbourhood_language".to_string(),
                })?;
        Ok(Language::new(address))
    }

    /// Get the perspective language
    pub async fn get_perspective_language(&self) -> Result<Language, LanguageError> {
        let sys = self.system_addresses.lock().await;
        let address = sys
            .perspective_language
            .clone()
            .ok_or_else(|| LanguageError::NotFound {
                address: "perspective_language".to_string(),
            })?;
        Ok(Language::new(address))
    }

    /// Get settings for a language (public accessor)
    pub fn get_settings_public(&self, language_address: &str) -> JsonValue {
        self.get_settings(language_address)
            .unwrap_or(JsonValue::Null)
    }

    /// Get cached language name for an address
    pub async fn get_language_name(&self, address: &str) -> String {
        let names = self.language_names.lock().await;
        names.get(address).cloned().unwrap_or_default()
    }

    /// Get icon code from a language's expressionUI/settingsUI interfaces.
    /// Returns (constructorIcon, icon, settingsIcon) as Option<String>.
    pub async fn get_language_icons(
        &self,
        address: &str,
    ) -> (Option<String>, Option<String>, Option<String>) {
        let constructor_icon = match self
            .execute_on_language(
                address,
                r#"(function() {
                    const lang = globalThis.__ad4m_language_instance__;
                    if (lang && lang.expressionUI && lang.expressionUI.constructorIcon) {
                        const code = lang.expressionUI.constructorIcon();
                        return code ? JSON.stringify({code: code}) : JSON.stringify({code: ""});
                    }
                    return "null";
                })()"#,
            )
            .await
        {
            Ok(result) => {
                let trimmed = result.trim().trim_matches('"');
                if trimmed == "null" {
                    None
                } else {
                    Some(result.trim().to_string())
                }
            }
            Err(_) => None,
        };

        let icon = match self
            .execute_on_language(
                address,
                r#"(function() {
                    const lang = globalThis.__ad4m_language_instance__;
                    if (lang && lang.expressionUI && lang.expressionUI.icon) {
                        const code = lang.expressionUI.icon();
                        return code ? JSON.stringify({code: code}) : JSON.stringify({code: ""});
                    }
                    return "null";
                })()"#,
            )
            .await
        {
            Ok(result) => {
                let trimmed = result.trim().trim_matches('"');
                if trimmed == "null" {
                    None
                } else {
                    Some(result.trim().to_string())
                }
            }
            Err(_) => None,
        };

        let settings_icon = match self
            .execute_on_language(
                address,
                r#"(function() {
                    const lang = globalThis.__ad4m_language_instance__;
                    if (lang && lang.settingsUI && lang.settingsUI.settingsIcon) {
                        const code = lang.settingsUI.settingsIcon();
                        return code ? JSON.stringify({code: code}) : JSON.stringify({code: ""});
                    }
                    return "null";
                })()"#,
            )
            .await
        {
            Ok(result) => {
                let trimmed = result.trim().trim_matches('"');
                if trimmed == "null" {
                    None
                } else {
                    Some(result.trim().to_string())
                }
            }
            Err(_) => None,
        };

        (constructor_icon, icon, settings_icon)
    }

    // ─── Expression handling methods ────────────────────────────────────

    /// Check if an expression is immutable (cacheable).
    pub async fn is_immutable_expression(
        &self,
        lang_address: &str,
        expression_address: &str,
    ) -> Result<bool, LanguageError> {
        if lang_address == "literal" {
            return Ok(true);
        }

        let escaped_addr =
            serde_json::to_string(expression_address).unwrap_or_else(|_| "\"\"".to_string());
        let script = format!(
            r#"language.isImmutableExpression ? await language.isImmutableExpression({}) : false"#,
            escaped_addr
        );

        let result = self.execute_on_language(lang_address, &script).await?;
        let trimmed = result.trim();
        Ok(trimmed == "true")
    }

    /// Get an expression from a language.
    pub async fn get_expression(
        &self,
        lang_address: &str,
        expression_address: &str,
    ) -> Result<Option<JsonValue>, LanguageError> {
        // Resolve alias forward (e.g. "did" → actual hash)
        let lang_address = {
            let aliases = self.language_aliases.lock().await;
            aliases
                .get(lang_address)
                .cloned()
                .unwrap_or_else(|| lang_address.to_string())
        };
        let lang_address = lang_address.as_str();

        // Handle literal language
        if lang_address == "literal" {
            let mut decoded = literal_decode(expression_address)?;
            // Verify signature on literal expressions too
            Self::verify_expression_proof(&mut decoded);
            return Ok(Some(decoded));
        }

        // Check immutability for caching
        let immutable = self
            .is_immutable_expression(lang_address, expression_address)
            .await
            .unwrap_or(false);

        // Check cache for immutable expressions
        if immutable {
            let cached = crate::db::Ad4mDb::with_global_instance(|db| {
                db._get_expression(expression_address)
            });
            if let Ok(Some(expr)) = cached {
                let mut expr_json = serde_json::to_value(&expr).unwrap_or(JsonValue::Null);
                // Verify and set proof.valid
                Self::verify_expression_proof(&mut expr_json);
                return Ok(Some(expr_json));
            }
        }

        // Fetch from the language runtime
        let escaped_addr =
            serde_json::to_string(expression_address).unwrap_or_else(|_| "\"\"".to_string());
        let script = format!(
            r#"JSON.stringify(await language.expressionAdapter.get({}))"#,
            escaped_addr
        );

        let result = self.execute_on_language(lang_address, &script).await?;

        if result.trim() == "null" || result.trim() == "undefined" || result.is_empty() {
            return Ok(None);
        }

        let mut expr_json: JsonValue =
            serde_json::from_str(&result).map_err(|e| LanguageError::SerializationError {
                message: format!("Failed to parse expression: {}", e),
            })?;

        if expr_json.is_null() {
            return Ok(None);
        }

        // Cache immutable expressions
        if immutable {
            if let Ok(expr) =
                serde_json::from_value::<crate::types::Expression<JsonValue>>(expr_json.clone())
            {
                let _ = crate::db::Ad4mDb::with_global_instance(|db| {
                    db._add_expression(expression_address, &expr)
                });
            }
        }

        // Verify signature
        Self::verify_expression_proof(&mut expr_json);

        Ok(Some(expr_json))
    }

    /// Verify an expression's proof and set the `valid` field.
    fn verify_expression_proof(expr_json: &mut JsonValue) {
        match serde_json::from_value::<crate::types::Expression<JsonValue>>(expr_json.clone()) {
            Ok(mut expr) => {
                // Sort the data keys to match the canonical order used during signing.
                // This is necessary because `preserve_order` is enabled for serde_json,
                // so deserialized key order depends on source (e.g. Holochain struct field order)
                // while signing always uses alphabetically sorted keys.
                expr.data = crate::js_core::utils::sort_json_value(&expr.data);
                match crate::agent::signatures::verify(&expr) {
                    Ok(valid) => {
                        log::warn!(
                            "verify_expression_proof: author={}, valid={}, sig={}",
                            expr.author,
                            valid,
                            &expr.proof.signature[..20.min(expr.proof.signature.len())]
                        );
                        if let Some(proof) = expr_json.get_mut("proof") {
                            proof["valid"] = JsonValue::Bool(valid);
                            proof["invalid"] = JsonValue::Bool(!valid);
                        }
                    }
                    Err(e) => {
                        log::warn!(
                            "verify_expression_proof: verification error for author={}: {}",
                            expr.author,
                            e
                        );
                        if let Some(proof) = expr_json.get_mut("proof") {
                            proof["valid"] = JsonValue::Bool(false);
                            proof["invalid"] = JsonValue::Bool(true);
                        }
                    }
                }
            }
            Err(e) => {
                log::warn!(
                    "verify_expression_proof: failed to parse expression: {}. Keys: {:?}",
                    e,
                    expr_json.as_object().map(|o| o.keys().collect::<Vec<_>>())
                );
                // If we can't parse the expression, still try to set valid based on existing proof
                if let Some(proof) = expr_json.get_mut("proof") {
                    proof["valid"] = JsonValue::Bool(false);
                    proof["invalid"] = JsonValue::Bool(true);
                }
            }
        }
    }

    /// Create an expression in a language.
    pub async fn expression_create(
        &self,
        lang_address: &str,
        content: JsonValue,
        agent_context: &AgentContext,
    ) -> Result<String, LanguageError> {
        // Handle literal language
        if lang_address == "literal" {
            let signed_expr = crate::agent::create_signed_expression(content, agent_context)
                .map_err(|e| LanguageError::RuntimeError {
                    address: "literal".to_string(),
                    message: format!("Failed to create signed expression: {}", e),
                })?;

            let signed_expr_json = serde_json::to_value(&signed_expr).map_err(|e| {
                LanguageError::SerializationError {
                    message: format!("Failed to serialize signed expression: {}", e),
                }
            })?;

            let expression_part = literal_encode(&signed_expr_json);
            return Ok(format!("literal://{}", expression_part));
        }

        // Resolve aliases in both directions:
        // 1. Forward: if lang_address is an alias name (e.g. "did"), resolve to hash
        // 2. Reverse: if lang_address is a hash, find the alias name for URL scheme
        let (resolved_address, alias_name) = {
            let aliases = self.language_aliases.lock().await;
            // Forward: alias → hash
            if let Some(target) = aliases.get(lang_address) {
                (target.clone(), Some(lang_address.to_string()))
            } else {
                // Reverse: hash → alias
                let mut alias = None;
                for (a, t) in aliases.iter() {
                    if t == lang_address {
                        alias = Some(a.clone());
                        break;
                    }
                }
                (lang_address.to_string(), alias)
            }
        };
        let effective_lang_address = alias_name.unwrap_or_else(|| resolved_address.clone());

        let content_json =
            serde_json::to_string(&content).map_err(|e| LanguageError::SerializationError {
                message: format!("Failed to serialize content: {}", e),
            })?;

        let script = format!(
            r#"JSON.stringify(
                language.expressionAdapter.putAdapter.createPublic
                    ? await language.expressionAdapter.putAdapter.createPublic({})
                    : await language.expressionAdapter.putAdapter.addressOf({})
            )"#,
            content_json, content_json
        );

        let result = self
            .execute_on_language_with_context(&resolved_address, &script, agent_context)
            .await?;

        // Strip surrounding quotes from the result (it's a JSON-encoded string)
        let expression_address = result.trim().trim_matches('"').to_string();

        // Special case: for the "did" scheme, the expression address IS the full URL
        // (e.g. "did:key:z6Mk..."), so don't prefix with "did://"
        if effective_lang_address == "did" {
            Ok(expression_address)
        } else {
            Ok(format!(
                "{}://{}",
                effective_lang_address, expression_address
            ))
        }
    }

    /// Get expression interactions for a URL.
    pub async fn expression_interactions(
        &self,
        url: &str,
    ) -> Result<Vec<InteractionMeta>, LanguageError> {
        let (lang_address, expression_address) = Self::parse_expr_url(url)?;

        // Resolve alias forward (e.g. "did" → actual hash)
        let lang_address = {
            let aliases = self.language_aliases.lock().await;
            aliases.get(&lang_address).cloned().unwrap_or(lang_address)
        };

        let escaped_addr =
            serde_json::to_string(&expression_address).unwrap_or_else(|_| "\"\"".to_string());
        let script = format!(
            r#"JSON.stringify(
                language.interactions({}).map(ic => ({{
                    label: ic.label, name: ic.name, parameters: ic.parameters
                }}))
            )"#,
            escaped_addr
        );

        let result = self.execute_on_language(&lang_address, &script).await?;

        serde_json::from_str(&result).map_err(|e| LanguageError::SerializationError {
            message: format!("Failed to parse interactions: {}", e),
        })
    }

    /// Execute an interaction on an expression.
    pub async fn expression_interact(
        &self,
        url: &str,
        call: &InteractionCall,
    ) -> Result<Option<String>, LanguageError> {
        let (lang_address, expression_address) = Self::parse_expr_url(url)?;

        // Resolve alias forward (e.g. "did" → actual hash)
        let lang_address = {
            let aliases = self.language_aliases.lock().await;
            aliases.get(&lang_address).cloned().unwrap_or(lang_address)
        };

        let escaped_addr =
            serde_json::to_string(&expression_address).unwrap_or_else(|_| "\"\"".to_string());
        let escaped_name = serde_json::to_string(&call.name).unwrap_or_else(|_| "\"\"".to_string());
        let script = format!(
            r#"JSON.stringify(
                await (async () => {{
                    const interaction = language.interactions({})
                        .find(i => i.name === {});
                    if (!interaction) throw new Error("No interaction named " + {});
                    return await interaction.execute({});
                }})()
            )"#,
            escaped_addr, escaped_name, escaped_name, call.parameters_stringified
        );

        let result = self.execute_on_language(&lang_address, &script).await?;

        let trimmed = result.trim();
        if trimmed == "null" || trimmed == "undefined" {
            Ok(None)
        } else {
            // The result is JSON-encoded (via JSON.stringify). If it's a JSON string,
            // unwrap the outer quotes so the GraphQL layer returns the raw value.
            if let Ok(serde_json::Value::String(s)) =
                serde_json::from_str::<serde_json::Value>(trimmed)
            {
                Ok(Some(s))
            } else {
                Ok(Some(trimmed.to_string()))
            }
        }
    }

    // ─── Reload language ────────────────────────────────────────────────

    /// Reload a language: unload and re-load from disk.
    pub async fn reload_language(&self, address: &str) -> Result<(), LanguageError> {
        let is_system = {
            let sys = self.system_addresses.lock().await;
            sys.system_language_set.contains(address)
        };
        self.unload_language(address).await?;

        let bundle_path = languages_directory().join(address).join("bundle.js");
        if bundle_path.exists() {
            self.load_language(bundle_path, is_system).await?;
        }

        Ok(())
    }
}
