pub mod language;

use std::sync::{Arc, Mutex};
use deno_core::error::AnyError;

use crate::{js_core::JsCoreHandle, types::{Neighbourhood, NeighbourhoodExpression}};
use crate::types::Address;

lazy_static! {
    static ref LANGUAGE_CONTROLLER_INSTANCE: Arc<Mutex<Option<LanguageController>>> = Arc::new(Mutex::new(None));
}

pub struct LanguageController {
    js_core: JsCoreHandle,
}

impl LanguageController {
    pub fn init_global_instance(js_core: JsCoreHandle) {
        let mut instance = LANGUAGE_CONTROLLER_INSTANCE.lock().unwrap();
        *instance = Some(LanguageController::new(js_core));
    }

    pub fn global_instance() -> Arc<Mutex<Option<LanguageController>>> {
        LANGUAGE_CONTROLLER_INSTANCE.clone()
    }

    pub fn new(js_core: JsCoreHandle) -> Self {
        Self {
            js_core,
        }
    }

    pub fn with_global_instance<F, R>(func: F) -> R
    where
        F: FnOnce(&mut LanguageController) -> R,
    {
        let global_instance_arc = LanguageController::global_instance();
        let lock_result = global_instance_arc.lock();
        let mut language_controller_lock = lock_result.expect("Couldn't get lock on Ad4mDb");
        let language_controller_ref = language_controller_lock.as_mut().expect("LanguageController not initialized");
        func(language_controller_ref)
    }

    pub async fn install_language(&mut self, language: Address) -> Result<(), AnyError> {
        let script = format!(
            r#"JSON.stringify(
                await core.languageController.installLanguage("{}")
            )"#,
            language,
        );
        let _result = self.js_core.execute(script).await?;
        Ok(())
    }

    pub async fn create_neighbourhood(&mut self, neighbourhood: Neighbourhood) -> Result<Address, AnyError> {
        let neighbourhood_json = serde_json::to_string(&neighbourhood)?;
        let script = format!(
            r#"
            await core
                    .languageController
                    .getNeigbourhoodLanguage()
                    .expressionAdapter
                    .putAdapter
                    .createPublic({}) 
            "#,
            neighbourhood_json,
        );
        let result: String = self.js_core.execute(script).await?;
        Ok(result)
    }

    pub async fn get_neighbourhood(&mut self, address: Address) -> Result<Option<NeighbourhoodExpression>, AnyError> {
        let script = format!(
            r#"
            JSON.stringify(
                await core
                    .languageController
                    .getPerspective({})
            )
            "#,
            address,
        );
        let result: String = self.js_core.execute(script).await?;
        let neighbourhood: Option<NeighbourhoodExpression> = serde_json::from_str(&result)?;
        Ok(neighbourhood)
    }

    pub async fn language_by_address(&mut self, address: Address) -> Result<bool, AnyError> {
        let script = format!(
            r#"if(await core.languageController.languageByRef("{{ address: {} }}")) {
                return true
            } else {
                return false
            }
            )"#,
            address,
        );
        let result: String = self.js_core.execute(script).await?;
        let result: bool = serde_json::from_str(&result)?;
        Ok(result)
    }
}
