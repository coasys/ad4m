use std::borrow::Cow;

use deno_core::{error::AnyError, include_js_files, op2, Extension, Op};

use crate::graphql::graphql_types::EntanglementProof;

use super::{add_entanglement_proofs, delete_entanglement_proof, generate_holochain_proof, get_entanglement_proofs, sign_device_key};

#[op2]
#[serde]
pub fn sign_device_key(#[string] device_key: String, #[string] device_key_type: String) -> Result<EntanglementProof, AnyError> {
  Ok(sign_device_key(device_key, device_key_type))
}

#[op2]
#[serde]
pub fn generate_entanglement_proof(#[string] holochain_key: String, #[string] signed_did: String) -> Result<EntanglementProof, AnyError> {
  Ok(generate_holochain_proof(holochain_key, signed_did))
}

#[op2]
pub fn add_entanglement_proofs(#[serde] proofs: Vec<EntanglementProof>) -> Result<(), AnyError> {
  Ok(add_entanglement_proofs(proofs))
}

#[op2]
pub fn delete_entanglement_proof(#[serde] proof: EntanglementProof) -> Result<(), AnyError> {
  Ok(delete_entanglement_proof(proof))
}

#[op2]
#[serde]
pub fn get_entanglement_proofs() -> Result<Vec<EntanglementProof>, AnyError> {
  Ok(get_entanglement_proofs())
}

pub fn build() -> Extension {
  Extension {
    name: "entanglement_service",
    js_files: Cow::Borrowed(&include_js_files!(entanglement_service "src/entanglement_service/entanglement_service_extension.js",)),
    ops: Cow::Borrowed(&[
      sign_device_key::DECL,
      generate_entanglement_proof::DECL,
      add_entanglement_proofs::DECL,
      delete_entanglement_proof::DECL,
      get_entanglement_proofs::DECL
    ]),
    ..Default::default()
  }
}

