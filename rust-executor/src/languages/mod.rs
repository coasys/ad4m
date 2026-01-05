mod byte_array;
pub mod error;
pub mod language;
pub mod language_context;
pub mod language_runtime;

use deno_core::error::AnyError;
use std::sync::{Arc, Mutex};

use crate::types::Address;
use crate::{
    graphql::graphql_types::{DecoratedNeighbourhoodExpression, Neighbourhood},
    js_core::JsCoreHandle,
    utils::{language_storage_directory, languages_directory},
};
use error::LanguageError;
use language::Language;
use log::{error, info};
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use tokio::sync::Mutex as TokioMutex;

lazy_static! {
    static ref LANGUAGE_CONTROLLER_INSTANCE: Arc<Mutex<Option<LanguageController>>> =
        Arc::new(Mutex::new(None));
}

#[derive(Clone)]
pub struct LanguageController {
    // Legacy field for backward compatibility during migration
    js_core: JsCoreHandle,

    // Phase 1: Track loaded languages by address for validation
    // Phase 2 TODO: Replace with actual per-language runtime handles
    loaded_languages: Arc<TokioMutex<HashMap<String, LanguageMetadata>>>,
}

/// Metadata about a loaded language (Phase 1 implementation)
#[derive(Clone, Debug)]
pub struct LanguageMetadata {
    #[allow(dead_code)]
    pub address: String,
    #[allow(dead_code)]
    pub bundle_path: PathBuf,
    #[allow(dead_code)]
    pub storage_directory: PathBuf,
    #[allow(dead_code)]
    pub custom_settings: Option<JsonValue>,
    #[allow(dead_code)]
    pub has_links_adapter: bool,
    pub has_telepresence_adapter: bool,
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
        Self {
            js_core,
            loaded_languages: Arc::new(TokioMutex::new(HashMap::new())),
        }
    }

    /// Load a language from a bundle path
    ///
    /// Phase 1: Delegates to JS LanguageController for actual loading,
    /// but tracks metadata in Rust for management
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

        // Phase 1: Use JS LanguageController to actually load the language
        let mut js_core = self.js_core.clone();
        let bundle_path_str = bundle_path.to_string_lossy().to_string();
        let load_script = format!(
            r#"
            await core.languageController.loadLanguageFromBundle("{}")
            "#,
            bundle_path_str
        );

        js_core.execute(load_script).await.map_err(|e| LanguageError::LoadError {
            address: language_address.clone(),
            message: e.to_string(),
        })?;

        // Check for adapters via JS
        let check_adapters_script = format!(
            r#"
            JSON.stringify({{
                hasLinksAdapter: !!core.languageController.languageByRef({{address: "{}"}})?.linksAdapter,
                hasTelepresenceAdapter: !!core.languageController.languageByRef({{address: "{}"}})?.telepresenceAdapter
            }})
            "#,
            language_address, language_address
        );

        let adapters_result = js_core.execute(check_adapters_script).await.map_err(|e| LanguageError::LoadError {
            address: language_address.clone(),
            message: e.to_string(),
        })?;

        let adapters: serde_json::Value = serde_json::from_str(&adapters_result).unwrap_or_default();

        // Store metadata
        let metadata = LanguageMetadata {
            address: language_address.clone(),
            bundle_path: bundle_path.clone(),
            storage_directory,
            custom_settings,
            has_links_adapter: adapters["hasLinksAdapter"].as_bool().unwrap_or(false),
            has_telepresence_adapter: adapters["hasTelepresenceAdapter"].as_bool().unwrap_or(false),
        };

        let mut loaded = self.loaded_languages.lock().await;
        loaded.insert(language_address.clone(), metadata);

        info!("Successfully loaded language: {}", language_address);
        Ok(language_address)
    }

    /// Unload a language and clean up
    pub async fn unload_language(&self, language_address: &str) -> Result<(), LanguageError> {
        info!("Unloading language: {}", language_address);

        let mut loaded = self.loaded_languages.lock().await;
        loaded.remove(language_address);

        // TODO Phase 2: Call teardown on per-language runtime

        info!("Successfully unloaded language: {}", language_address);
        Ok(())
    }

    /// Check if a language is loaded
    pub async fn is_language_loaded(&self, language_address: &str) -> bool {
        let loaded = self.loaded_languages.lock().await;
        loaded.contains_key(language_address)
    }

    /// Get language metadata
    pub async fn get_language_metadata(&self, language_address: &str) -> Option<LanguageMetadata> {
        let loaded = self.loaded_languages.lock().await;
        loaded.get(language_address).cloned()
    }

    /// Execute a script on a specific language runtime
    ///
    /// Phase 1 Implementation: Delegates to JS LanguageController
    /// Phase 2 TODO: Implement per-language execution handles with proper thread isolation
    pub async fn execute_on_language(
        &self,
        language_address: &str,
        script: &str,
    ) -> Result<String, LanguageError> {
        // Phase 1: Delegate directly to JS without checking Rust registry
        // Languages are loaded by JS LanguageController in this phase
        let wrapped_script = format!(
            r#"
            (async function() {{
                const language = await core.languageController.languageByRef({{address:"{}"}});
                if (!language) throw new Error("Language not found: {}");

                // Set as global for backward compatibility with scripts that reference it
                globalThis.__ad4m_language_instance__ = language;

                // Execute the script (which is already an async IIFE)
                const result = await {};

                // Clean up global
                delete globalThis.__ad4m_language_instance__;

                return result;
            }})()
            "#,
            language_address, language_address, script
        );

        let mut js_core_handle = self.js_core.clone();
        js_core_handle
            .execute(wrapped_script)
            .await
            .map_err(|e| LanguageError::RuntimeError {
                address: language_address.to_string(),
                message: e.to_string(),
            })
    }

    /// Calculate IPFS hash for a language bundle (simplified version)
    fn calculate_language_hash(&self, bundle_content: &str) -> String {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(bundle_content.as_bytes());
        let result = hasher.finalize();
        format!("Qm{}", hex::encode(&result[..20]))
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

        let mut loaded = self.loaded_languages.lock().await;
        loaded.clear();

        // TODO Phase 2: Teardown per-language runtimes

        info!("Language controller shut down");
        Ok(())
    }

    pub async fn install_language(language: Address) -> Result<(), AnyError> {
        Self::global_instance()
            .js_core
            .execute("await core.waitForLanguages()".into())
            .await?;

        let script = format!(
            r#"JSON.stringify(
                await core.languageController.installLanguage("{}")
            )"#,
            language,
        );
        let _result = Self::global_instance().js_core.execute(script).await?;
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
        Self::global_instance()
            .js_core
            .execute("await core.waitForLanguages()".into())
            .await?;

        let script = format!(
            r#"
            await core.languageController.languageByRef({{ address: "{}" }}) ? true : false
            "#,
            address,
        );
        let result: String = Self::global_instance().js_core.execute(script).await?;
        let language_installed = serde_json::from_str::<bool>(&result)?;
        if language_installed {
            let language = Language::new(address, Self::global_instance().js_core.clone());
            Ok(Some(language))
        } else {
            Ok(None)
        }
    }
}
