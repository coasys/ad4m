use deno_core::{error::AnyError, include_js_files, op2, Extension, Op};
use std::borrow::Cow;
use crate::agent::{create_signed_expression, did, did_document, sign, sign_string_hex, signing_key_id};

use super::utils::sort_json_value;

#[op2]
#[serde]
fn agent_did_document() -> Result<did_key::Document, AnyError> {
    Ok(did_document())
}

#[op2]
#[string]
fn agent_signing_key_id() -> Result<String, AnyError> {
    Ok(signing_key_id())
}

#[op2]
#[string]
fn agent_did() -> Result<String, AnyError> {
    Ok(did())
}

#[op2]
#[serde]
fn agent_create_signed_expression(#[serde] data: serde_json::Value) -> Result<serde_json::Value, AnyError> {
    let sorted_json = sort_json_value(&data);
    let signed_expression = create_signed_expression(sorted_json)?;
    Ok(serde_json::to_value(signed_expression)?)
}

#[op2]
#[string]
fn agent_create_signed_expression_stringified(#[string] data: String) -> Result<String, AnyError> {
    let data: serde_json::Value = serde_json::from_str(&data)?;
    let sorted_json = sort_json_value(&data);
    let signed_expression = create_signed_expression(sorted_json)?;
    let stringified = serde_json::to_string(&signed_expression)?;
    Ok(stringified)
}

#[op2]
#[serde]
fn agent_sign(#[buffer] payload: &[u8]) -> Result<Vec<u8>, AnyError> {
    sign(payload)
}


#[op2]
#[string]
fn agent_sign_string_hex(#[string] payload: String) -> Result<String, AnyError> {
    sign_string_hex(payload)
}


pub fn build() -> Extension {
    Extension {
        name: "agent",
        js_files: Cow::Owned(include_js_files!(rust_executor "src/js_core/agent_extension.js").to_vec()),
        ops: Cow::Borrowed(&[
            agent_did_document::DECL,
            agent_signing_key_id::DECL,
            agent_did::DECL,
            agent_create_signed_expression::DECL,
            agent_create_signed_expression_stringified::DECL,
            agent_sign::DECL,
            agent_sign_string_hex::DECL,
        ]),
        ..Default::default()
    }
}