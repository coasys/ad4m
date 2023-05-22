use juniper::{graphql_value, FieldError, FieldResult};

use crate::js_core::JsCoreHandle;

pub async fn get_capabilies(js: JsCoreHandle, capability: String) -> FieldResult<String> {
    let mut js = js.clone();
    let script = format!(
        r#"JSON.stringify(
        await core.callResolver("Query", "getCapabilities", {})
    )"#,
        capability
    );
    let result = js.execute(script).await;
    match result {
        Ok(result) => Ok(result),
        Err(e) => {
            log::error!("UNAUTHORIZED: {:?}", e);
            Err(FieldError::new(
                "UNAUTHORIZED",
                graphql_value!({ "type": "UNAUTHORIZED" }),
            ))
        }
    }
}
