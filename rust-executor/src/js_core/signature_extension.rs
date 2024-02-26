use deno_core::{error::AnyError, include_js_files, op2, Extension, Op};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use crate::types::Expression;
use crate::agent::signatures::{verify_string_signed_by_did, verify};

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
    #[serde] expr: Expression,
) -> Result<SignatureVerificationResult, AnyError> {
    let is_valid = verify(&expr)?;
    Ok(SignatureVerificationResult { is_valid })
}

pub fn build() -> Extension {
    Extension {
        name: "signature",
        js_files: Cow::Borrowed(&include_js_files!(rust_executor "src/js_core/signature_extension.js",)),
        ops: Cow::Borrowed(&[
            signature_verify_string_signed_by_did::DECL,
            signature_verify::DECL,
        ]),
        ..Default::default()
    }
}