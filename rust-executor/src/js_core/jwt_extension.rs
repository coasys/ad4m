use std::borrow::Cow;
use deno_core::{error::AnyError, include_js_files, op2, Extension, Op};
use crate::agent::capabilities::{AuthInfo, Claims};

#[op2(async)]
#[string]
async fn generate_jwt(
    #[string] issuer: String,
    #[string] audience: String,
    #[smi] expiration_time: u64,
    #[serde] capabilities: AuthInfo,
) -> Result<String, AnyError> {
    crate::agent::capabilities::generate_jwt(issuer, audience, expiration_time, capabilities)
}

#[op2(async)]
#[serde]
pub async fn verify_jwt(#[string] token: String) -> Result<Claims, AnyError> {
    crate::agent::capabilities::decode_jwt(token)
}

pub fn build() -> Extension {
    Extension {
        name: "jwt",
        js_files: Cow::Borrowed(&include_js_files!(holochain_service "src/js_core/jwt_extension.js",)),
        ops: Cow::Borrowed(&[
            generate_jwt::DECL,
            verify_jwt::DECL
        ]),
        ..Default::default()
    }
}
