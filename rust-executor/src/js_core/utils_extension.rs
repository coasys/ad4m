use deno_core::{error::AnyError, include_js_files, op, Extension};

use crate::globals::SIGNING_DNA;

#[op]
fn get_signing_dna() -> Result<Vec<u8>, AnyError> {
    Ok(SIGNING_DNA.to_vec())
}

pub fn build() -> Extension {
    Extension::builder("wallet")
        .js(include_js_files!(wallet "utils_extension.js",))
        .ops(vec![get_signing_dna::decl()])
        .force_op_registration()
        .build()
}
