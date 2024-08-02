mod byte_array;
pub mod language;

use deno_core::error::AnyError;
use std::sync::{Arc, Mutex};

use crate::types::Address;
use crate::{
    graphql::graphql_types::{DecoratedNeighbourhoodExpression, Neighbourhood},
    js_core::JsCoreHandle,
};
use language::Language;

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
