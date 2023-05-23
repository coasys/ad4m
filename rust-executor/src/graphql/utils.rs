use juniper::{graphql_value, FieldError, FieldResult};

use crate::js_core::JsCoreHandle;

pub async fn get_capabilies(
    js: JsCoreHandle,
    capability: String,
) -> FieldResult<serde_json::Value> {
    let mut js = js.clone();
    let script = format!(
        r#"JSON.stringify(
        await core.callResolver("Query", "getCapabilities", "{}")
    )"#,
        capability
    );
    let result = js.execute(script).await;
    match result {
        Ok(result) => {
            let result = serde_json::from_str::<serde_json::Value>(&result)?;
            match result.get("Ok") {
                Some(ok) => Ok(ok
                    .get("capabilities")
                    .expect("missing capabilities")
                    .to_owned()),
                None => Err(FieldError::new(
                    result.get("Error").unwrap(),
                    graphql_value!({ "type": "UNAUTHORIZED" }),
                )),
            }
        }
        Err(e) => Err(FieldError::new(
            e.to_string(),
            graphql_value!({ "type": "UNAUTHORIZED" }),
        )),
    }
}
