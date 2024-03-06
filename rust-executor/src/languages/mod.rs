//pub mod language;

use std::sync::{Arc, Mutex};
use deno_core::error::AnyError;

use crate::{js_core::JsCoreHandle, types::{Neighbourhood, NeighbourhoodExpression}};
use crate::types::Address;

lazy_static! {
    static ref LANGUAGE_CONTROLLER_INSTANCE: Arc<Mutex<Option<LanguageController>>> = Arc::new(Mutex::new(None));
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
        LANGUAGE_CONTROLLER_INSTANCE.lock().unwrap().as_ref().expect("LanguageController not initialized").clone()
    }

    fn new(js_core: JsCoreHandle) -> Self {
        Self {
            js_core,
        }
    }

    pub async fn install_language(language: Address) -> Result<(), AnyError> {
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
        let result: String = Self::global_instance().js_core.execute(script).await?;
        Ok(result)
    }

    pub async fn get_neighbourhood(address: Address) -> Result<Option<NeighbourhoodExpression>, AnyError> {
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
        let result: String = Self::global_instance().js_core.execute(script).await?;
        let neighbourhood: Option<NeighbourhoodExpression> = serde_json::from_str(&result)?;
        Ok(neighbourhood)
    }

    pub async fn language_by_address(address: Address) -> Result<bool, AnyError> {
        let script = format!(
            r#"if(await core.languageController.languageByRef("{{ address: {} }}")) {{
                return true
            }} else {{
                return false
            }}
            )"#,
            address,
        );
        let result: String = Self::global_instance().js_core.execute(script).await?;
        let result: bool = serde_json::from_str(&result)?;
        Ok(result)
    }
}
