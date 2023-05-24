use juniper::{graphql_value, FieldError, FieldResult};
use serde::{Deserialize, Serialize};

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

pub async fn check_capabilities(
    js: JsCoreHandle,
    capability: serde_json::Value,
    expected_capabilities: serde_json::Value,
) -> FieldResult<()> {
    let mut js = js.clone();

    let script = format!(
        r#"JSON.stringify(
        await core.callResolver("Query", "checkCapability", {}, {})
    )"#,
        capability, expected_capabilities
    );
    let result = js.execute(script).await;
    match result {
        Ok(_result) => Ok(()),
        Err(err) => Err(FieldError::new(
            err.to_string(),
            graphql_value!({ "type": "UNAUTHORIZED" }),
        )),
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Capability {
    pub with: WithCapability,
    pub can: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WithCapability {
    pub domain: String,
    pub pointers: Vec<String>,
}

lazy_static::lazy_static! {
    pub static ref AGENT: String = String::from("agent");
    pub static ref PERSPECTIVE: String = String::from("perspective");
    pub static ref RUNTIME_MESSAGES: String = String::from("runtime.messages");
    pub static ref RUNTIME_EXCEPTION: String = String::from("runtime.exception");
    pub static ref WILD_CARD: String = String::from("*");
    pub static ref SUBSCRIBE: String = String::from("SUBSCRIBE");

    pub static ref AGENT_SUBSCRIBE_CAPABILITY: Capability = Capability {
        with: WithCapability {
            domain: AGENT.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![SUBSCRIBE.to_string()],
    };

    pub static ref PERSPECTIVE_SUBSCRIBE_CAPABILITY: Capability = Capability {
        with: WithCapability {
            domain: PERSPECTIVE.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![SUBSCRIBE.to_string()],
    };

    pub static ref RUNTIME_MESSAGES_SUBSCRIBE_CAPABILITY: Capability = Capability {
          with: WithCapability{
            domain: RUNTIME_MESSAGES.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![SUBSCRIBE.to_string()],
    };

    pub static ref RUNTIME_EXCEPTION_SUBSCRIBE_CAPABILITY: Capability = Capability {
          with: WithCapability{
            domain: RUNTIME_EXCEPTION.to_string(),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![SUBSCRIBE.to_string()],
    };
}
