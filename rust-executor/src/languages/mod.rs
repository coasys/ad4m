mod byte_array;
pub mod error;
pub mod language;
pub mod language_context;
pub mod language_runtime;
pub mod language_runtime_handle;

use deno_core::error::AnyError;
use std::sync::{Arc, Mutex};

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
    // Legacy field for backward compatibility (still used for neighbourhood operations)
    js_core: JsCoreHandle,

    // Per-language runtime handles (isolated Deno instances per language)
    runtimes: Arc<TokioMutex<HashMap<String, LanguageRuntimeHandle>>>,
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
            runtimes: Arc::new(TokioMutex::new(HashMap::new())),
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
        let agent_did = did_for_context(&agent_context)
            .map_err(|e| LanguageError::LoadError {
                address: language_address.clone(),
                message: format!("Failed to get agent DID: {}", e),
            })?;
        let agent_signing_key_id = signing_key_id_for_context(&agent_context)
            .map_err(|e| LanguageError::LoadError {
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

        // Create dedicated runtime handle for this language
        let mut runtime_handle = LanguageRuntimeHandle::new(
            language_address.clone(),
            bundle_path.clone(),
            storage_directory,
            custom_settings,
        )
        .await
        .map_err(|e| LanguageError::LoadError {
            address: language_address.clone(),
            message: e,
        })?;

        // Load the language bundle module
        let bundle_path_str = bundle_path.to_string_lossy().to_string();
        runtime_handle.load_module(bundle_path_str).await
            .map_err(|e| LanguageError::LoadError {
                address: language_address.clone(),
                message: format!("Failed to load language module: {}", e),
            })?;

        // Initialize the language with context
        runtime_handle.load_language(language_context.to_json()).await
            .map_err(|e| LanguageError::LoadError {
                address: language_address.clone(),
                message: format!("Failed to initialize language: {}", e),
            })?;

        // Register callbacks for adapters
        runtime_handle.register_callbacks().await
            .map_err(|e| LanguageError::LoadError {
                address: language_address.clone(),
                message: format!("Failed to register callbacks: {}", e),
            })?;

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
        if let Some(mut runtime) = runtimes.remove(language_address) {
            // Teardown the runtime (cleanup language instance, drop thread)
            runtime.teardown().await
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
    /// Uses dedicated per-language runtime with proper thread isolation
    pub async fn execute_on_language(
        &self,
        language_address: &str,
        script: &str,
    ) -> Result<String, LanguageError> {
        let mut runtimes = self.runtimes.lock().await;

        let runtime = runtimes.get_mut(language_address)
            .ok_or_else(|| LanguageError::RuntimeError {
                address: language_address.to_string(),
                message: "Language not loaded".to_string(),
            })?;

        // Scripts already reference 'language' which is set as globalThis.__ad4m_language_instance__
        runtime.execute(script.to_string())
            .await
            .map_err(|e| LanguageError::RuntimeError {
                address: language_address.to_string(),
                message: e,
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

        let mut runtimes = self.runtimes.lock().await;

        // Teardown all language runtimes
        for (address, mut runtime) in runtimes.drain() {
            info!("Shutting down language runtime: {}", address);
            if let Err(e) = runtime.teardown().await {
                error!("Error shutting down language {}: {}", address, e);
            }
        }

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
