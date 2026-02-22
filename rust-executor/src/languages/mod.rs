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
        Self { js_core }
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
