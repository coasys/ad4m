use crate::graphql::graphql_types::{PerspectiveExpression, SentMessage};
use deno_core::op2;
use crate::js_core::error::AnyhowWrapperError;
use super::RuntimeService;

#[op2]
#[serde]
pub fn friends() -> Result<Vec<String>, AnyhowWrapperError> {
    RuntimeService::with_global_instance(|runtime| Ok(runtime.get_friends()))
}

#[op2]
#[serde]
pub fn get_trusted_agents() -> Result<Vec<String>, AnyhowWrapperError> {
    RuntimeService::with_global_instance(|runtime| Ok(runtime.get_trusted_agents()))
}

#[op2]
pub fn add_message_outbox(
    #[string] did: String,
    #[serde] message: PerspectiveExpression,
    was_sent: bool,
) -> Result<bool, AnyhowWrapperError> {
    RuntimeService::with_global_instance(|runtime| {
        runtime.add_message_to_outbox(SentMessage {
            recipient: did,
            message,
        })
    });
    Ok(was_sent)
}

deno_core::extension!(
    runtime_service,
    ops = [friends, add_message_outbox, get_trusted_agents],
    esm_entry_point = "ext:runtime_service/runtime_service_extension.js",
    esm = [dir "src/runtime_service", "runtime_service_extension.js"]
);
