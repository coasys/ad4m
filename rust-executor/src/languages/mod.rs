mod byte_array;
pub mod language;

use deno_core::error::AnyError;
use std::sync::{Arc, Mutex};

use crate::types::Address;
use crate::{
    graphql::graphql_types::{DecoratedNeighbourhoodExpression, Neighbourhood},
    js_core::JsCoreHandle,
};
use language::{Language, LanguageBackend};

lazy_static! {
    static ref LANGUAGE_CONTROLLER_INSTANCE: Arc<Mutex<Option<LanguageController>>> =
        Arc::new(Mutex::new(None));
}

#[derive(Clone)]
pub struct LanguageController {
    js_core: JsCoreHandle,
    app_data_path: String,
}

impl LanguageController {
    pub fn init_global_instance(js_core: JsCoreHandle, app_data_path: String) {
        let mut instance = LANGUAGE_CONTROLLER_INSTANCE.lock().unwrap();
        *instance = Some(LanguageController::new(js_core, app_data_path));
    }

    pub fn global_instance() -> LanguageController {
        LANGUAGE_CONTROLLER_INSTANCE
            .lock()
            .unwrap()
            .as_ref()
            .expect("LanguageController not initialized")
            .clone()
    }

    fn new(js_core: JsCoreHandle, app_data_path: String) -> Self {
        Self { js_core, app_data_path }
    }

    pub async fn install_language(language: Address) -> Result<(), AnyError> {
        // Check if already registered as WASM
        #[cfg(feature = "wasm-languages")]
        if crate::wasm_core::is_wasm_language(&language) {
            log::info!("WASM language {} already registered", language);
            return Ok(());
        }

        // Check for local WASM bundle
        #[cfg(feature = "wasm-languages")]
        {
            let languages_path = Self::languages_path();
            let bundle_path = format!("{}/{}/bundle.wasm", languages_path, language);
            let path = std::path::Path::new(&bundle_path);
            if path.exists() && Self::is_wasm_bundle(path) {
                log::info!("Installing WASM language from local bundle: {}", bundle_path);
                Self::install_wasm_language(path, &language)?;
                return Ok(());
            }
        }

        // Try fetching source from language language and check if it's WASM
        #[cfg(feature = "wasm-languages")]
        {
            match Self::fetch_language_source(&language).await {
                Ok(source) => {
                    if Self::is_base64_wasm(&source) {
                        log::info!("Detected base64-encoded WASM language: {}", language);
                        return Self::install_wasm_from_base64(&source, &language).await;
                    }
                    // Also check meta for bundleType
                    if let Ok(meta) = Self::fetch_language_meta(&language).await {
                        if meta.contains("\"bundleType\":\"wasm\"") || meta.contains("\"bundleType\": \"wasm\"") {
                            log::info!("Language meta indicates WASM bundle: {}", language);
                            return Self::install_wasm_from_base64(&source, &language).await;
                        }
                    }
                }
                Err(e) => {
                    log::debug!("Could not fetch language source for WASM check: {}", e);
                }
            }
        }

        // Fall back to JS language install
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

    /// Get the languages directory path from JS core
    pub fn languages_path() -> String {
        let instance = Self::global_instance();
        format!("{}/ad4m/languages", instance.app_data_path)
    }

    /// Fetch language source from the language language via JS
    async fn fetch_language_source(address: &str) -> Result<String, AnyError> {
        Self::global_instance()
            .js_core
            .execute("await core.waitForLanguages()".into())
            .await?;

        let script = format!(
            r#"await core.languageController.getLanguageSource("{}")"#,
            address,
        );
        let result = Self::global_instance().js_core.execute(script).await?;
        if result == "null" || result.is_empty() {
            return Err(deno_core::anyhow::anyhow!("Language source not found: {}", address));
        }
        Ok(result.trim_matches('"').to_string())
    }

    /// Fetch language meta JSON from the language language via JS
    async fn fetch_language_meta(address: &str) -> Result<String, AnyError> {
        let script = format!(
            r#"JSON.stringify(await core.languageController.getLanguageExpression("{}"))"#,
            address,
        );
        Self::global_instance().js_core.execute(script).await
    }

    /// Check if a string looks like base64-encoded WASM (starts with AGFzbQ == \0asm)
    #[cfg(feature = "wasm-languages")]
    fn is_base64_wasm(data: &str) -> bool {
        data.starts_with("AGFzbQ")
    }

    /// Decode base64 WASM, save to languages dir, and register
    #[cfg(feature = "wasm-languages")]
    async fn install_wasm_from_base64(base64_data: &str, address: &str) -> Result<(), AnyError> {
        use base64::Engine;

        let wasm_bytes = base64::engine::general_purpose::STANDARD
            .decode(base64_data)
            .map_err(|e| deno_core::anyhow::anyhow!("Base64 decode error: {}", e))?;

        // Verify WASM magic
        if wasm_bytes.len() < 4 || &wasm_bytes[0..4] != b"\0asm" {
            return Err(deno_core::anyhow::anyhow!("Decoded data is not valid WASM"));
        }

        // Save to languages directory
        let languages_path = Self::languages_path();
        let lang_dir = format!("{}/{}", languages_path, address);
        std::fs::create_dir_all(&lang_dir)?;
        let bundle_path = format!("{}/bundle.wasm", lang_dir);
        std::fs::write(&bundle_path, &wasm_bytes)?;
        log::info!("Saved WASM bundle ({} bytes) to {}", wasm_bytes.len(), bundle_path);

        // Register in WASM runtime
        Self::install_wasm_language(std::path::Path::new(&bundle_path), address)?;
        Ok(())
    }

    /// Publish a WASM language: base64-encode the binary and publish via language language
    #[cfg(feature = "wasm-languages")]
    pub async fn publish_wasm_language(
        wasm_path: &std::path::Path,
        meta: &str,
    ) -> Result<String, AnyError> {
        use base64::Engine;

        let wasm_bytes = std::fs::read(wasm_path)?;

        // Verify it's actually WASM
        if wasm_bytes.len() < 4 || &wasm_bytes[0..4] != b"\0asm" {
            return Err(deno_core::anyhow::anyhow!("File is not valid WASM: {}", wasm_path.display()));
        }

        let base64_data = base64::engine::general_purpose::STANDARD.encode(&wasm_bytes);

        // Parse meta and add bundleType
        let mut meta_obj: serde_json::Value = serde_json::from_str(meta)
            .unwrap_or(serde_json::json!({}));
        meta_obj["bundleType"] = serde_json::json!("wasm");

        // Compute hash for the address
        let hash_script = format!(
            r#"UTILS.hash("{}")"#,
            base64_data,
        );
        let hash = Self::global_instance().js_core.execute(hash_script).await?;
        let hash = hash.trim_matches('"').to_string();
        meta_obj["address"] = serde_json::json!(&hash);
        let meta_json = serde_json::to_string(&meta_obj)?;

        Self::global_instance()
            .js_core
            .execute("await core.waitForLanguages()".into())
            .await?;

        let script = format!(
            r#"JSON.stringify(
                await (core.languageController.getLanguageLanguage().expressionAdapter.putAdapter).createPublic({{
                    bundle: `{}`,
                    meta: {}
                }})
            )"#,
            base64_data, meta_json,
        );

        let result = Self::global_instance().js_core.execute(script).await?;
        log::info!("Published WASM language: {} (hash: {})", wasm_path.display(), hash);
        Ok(result.trim_matches('"').to_string())
    }

    pub async fn create_neighbourhood(neighbourhood: Neighbourhood) -> Result<Address, AnyError> {
        Self::global_instance()
            .js_core
            .execute("await core.waitForLanguages()".into())
            .await?;

        let neighbourhood_json = serde_json::to_string(&neighbourhood)?;
        let script = format!(
            r#"
            await core
                    .languageController
                    .getNeighbourhoodLanguage()
                    .expressionAdapter
                    .putAdapter
                    .createPublic({})
            "#,
            neighbourhood_json,
        );
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

    /// Look up a language by address, returning a boxed `LanguageBackend`.
    ///
    /// When the `wasm-languages` feature is enabled, this checks the WASM
    /// registry first and falls back to the JS runtime.
    pub async fn language_by_address(
        address: Address,
    ) -> Result<Option<Box<dyn LanguageBackend>>, AnyError> {
        // Check WASM registry first (feature-gated)
        #[cfg(feature = "wasm-languages")]
        {
            if crate::wasm_core::is_wasm_language(&address) {
                let instance = crate::wasm_core::get_wasm_language(&address)
                    .map_err(|e| deno_core::anyhow::anyhow!("{}", e))?;
                let wasm_lang = language::wasm_backend::WasmLanguage::new(instance);
                return Ok(Some(Box::new(wasm_lang)));
            }
        }

        // Fall back to JS
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
            Ok(Some(Box::new(language)))
        } else {
            Ok(None)
        }
    }

    /// Install a WASM language from a file path and return a boxed backend.
    #[cfg(feature = "wasm-languages")]
    pub fn install_wasm_language(
        wasm_path: &std::path::Path,
        address: &str,
    ) -> Result<Box<dyn LanguageBackend>, AnyError> {
        crate::wasm_core::register_wasm_language(wasm_path, address)
            .map_err(|e| deno_core::anyhow::anyhow!("{}", e))?;
        let instance = crate::wasm_core::get_wasm_language(address)
            .map_err(|e| deno_core::anyhow::anyhow!("{}", e))?;
        Ok(Box::new(language::wasm_backend::WasmLanguage::new(instance)))
    }

    /// Detect whether a language bundle file is WASM (magic bytes `\0asm`)
    /// or JS, and return `true` if it is WASM.
    #[cfg(feature = "wasm-languages")]
    pub fn is_wasm_bundle(path: &std::path::Path) -> bool {
        use std::io::Read;
        let mut file = match std::fs::File::open(path) {
            Ok(f) => f,
            Err(_) => return false,
        };
        let mut magic = [0u8; 4];
        if file.read_exact(&mut magic).is_err() {
            return false;
        }
        // WebAssembly magic number: \0asm
        magic == [0x00, 0x61, 0x73, 0x6D]
    }

    /// Remove a language from the JS LanguageController.
    pub async fn language_remove(address: Address) -> Result<(), AnyError> {
        // If it's a WASM language, unregister from WASM registry
        #[cfg(feature = "wasm-languages")]
        {
            if crate::wasm_core::is_wasm_language(&address) {
                crate::wasm_core::unregister_wasm_language(&address)
                    .map_err(|e| deno_core::anyhow::anyhow!("{}", e))?;
                log::info!("🗑️ Successfully removed WASM language: {}", address);
                return Ok(());
            }
        }

        Self::global_instance()
            .js_core
            .execute("await core.waitForLanguages()".into())
            .await?;

        let script = format!(
            r#"await core.languageController.languageRemove("{}")"#,
            address,
        );
        let mut js = Self::global_instance().js_core;
        match js.execute(script).await {
            Ok(_) => {
                log::info!("🗑️ Successfully removed language: {}", address);
                Ok(())
            }
            Err(e) => {
                log::warn!("⚠️ Error removing language {}: {:?}", address, e);
                Ok(())
            }
        }
    }
}
