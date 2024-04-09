use std::borrow::Cow;

use deno_core::{error::AnyError, include_js_files, op2, Extension, Op};
use crate::graphql::graphql_types::{PerspectiveExpression, SentMessage};

use super::RuntimeService;


#[op2]
#[serde]
pub fn friends() -> Result<Vec<String>, AnyError> {
    RuntimeService::with_global_instance(|runtime| {
        Ok(runtime.get_friends())
    })
}

#[op2]
#[serde]
pub fn get_trusted_agents() -> Result<Vec<String>, AnyError> {
    RuntimeService::with_global_instance(|runtime| {
        Ok(runtime.get_trusted_agents())
    })
}

#[op2]
pub fn add_message_outbox(#[string] did: String, #[serde] message: PerspectiveExpression, was_sent: bool) -> Result<bool, AnyError> {
    RuntimeService::with_global_instance(|runtime| {
        runtime.add_message_to_outbox(SentMessage {
            recipient: did,
            message,

        })
    });
    Ok(was_sent)
}


pub fn build() -> Extension {
    Extension {
        name: "runtime_service",
        js_files: Cow::Borrowed(&include_js_files!(runtime_service "src/runtime_service/runtime_service_extension.js",)),
        ops: Cow::Borrowed(&[
            friends::DECL,
            add_message_outbox::DECL,
            get_trusted_agents::DECL,
        ]),
        ..Default::default()
    }
}