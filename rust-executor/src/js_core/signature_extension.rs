use deno_core::{error::AnyError, include_js_files, op2, Extension, Op};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use crate::types::Expression;
use crate::agent::signatures::{verify_string_signed_by_did, verify};

use super::utils::sort_json_value;

#[derive(Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct SignatureVerificationResult {
    pub is_valid: bool,
}

#[op2]
#[serde]
fn signature_verify_string_signed_by_did(
    #[string] did: String,
    #[string] data: String,
    #[string] signed_data: String,
) -> Result<SignatureVerificationResult, AnyError> {
    let is_valid = verify_string_signed_by_did(&did, &data, &signed_data)?;
    Ok(SignatureVerificationResult { is_valid })
}

#[op2]
#[serde]
fn signature_verify(
    #[serde] expr: Expression<serde_json::Value>,
) -> Result<SignatureVerificationResult, AnyError> {
    let sorted_expression = Expression {
        author: expr.author,
        timestamp: expr.timestamp,
        data: sort_json_value(&expr.data),
        proof: expr.proof,
    };
    let is_valid =  verify(&sorted_expression)?;
    Ok(SignatureVerificationResult { is_valid })
}

pub fn build() -> Extension {
    Extension {
        name: "signature",
        js_files: Cow::Owned(include_js_files!(rust_executor "src/js_core/signature_extension.js").to_vec()),
        ops: Cow::Borrowed(&[
            signature_verify_string_signed_by_did::DECL,
            signature_verify::DECL,
        ]),
        ..Default::default()
    }
}