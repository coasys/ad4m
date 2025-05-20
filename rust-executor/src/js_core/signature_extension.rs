use crate::agent::signatures::{verify, verify_string_signed_by_did};
use crate::types::Expression;
use deno_core::op2;
use serde::{Deserialize, Serialize};
use crate::js_core::error::AnyhowWrapperError;
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
) -> Result<SignatureVerificationResult, AnyhowWrapperError> {
    let is_valid = verify_string_signed_by_did(&did, &data, &signed_data)?;
    Ok(SignatureVerificationResult { is_valid })
}

#[op2]
#[serde]
fn signature_verify(
    #[serde] expr: Expression<serde_json::Value>,
) -> Result<SignatureVerificationResult, AnyhowWrapperError> {
    let sorted_expression = Expression {
        author: expr.author,
        timestamp: expr.timestamp,
        data: sort_json_value(&expr.data),
        proof: expr.proof,
    };
    let is_valid = verify(&sorted_expression)?;
    Ok(SignatureVerificationResult { is_valid })
}

deno_core::extension!(
    signature_service,
    ops = [signature_verify_string_signed_by_did, signature_verify],
    esm_entry_point = "ext:signature_service/signature_extension.js",
    esm = [dir "src/js_core", "signature_extension.js"]
);
