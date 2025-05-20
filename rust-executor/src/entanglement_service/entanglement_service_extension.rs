use deno_core::{error::AnyError, op2};

use crate::graphql::graphql_types::EntanglementProof;
use crate::js_core::error::AnyhowWrapperError;

use super::{
    add_entanglement_proofs as add_entanglement_proofs_service,
    delete_entanglement_proof as delete_entanglement_proof_service,
    generate_holochain_proof as generate_holochain_proof_service,
    get_entanglement_proofs as get_entanglement_proofs_service,
    sign_device_key as sign_device_key_service,
};

#[op2]
#[serde]
pub fn sign_device_key(
    #[string] device_key: String,
    #[string] device_key_type: String,
) -> Result<EntanglementProof, AnyhowWrapperError> {
    Ok(sign_device_key_service(device_key, device_key_type))
}

#[op2]
#[serde]
pub fn generate_entanglement_proof(
    #[string] holochain_key: String,
    #[string] signed_did: String,
) -> Result<EntanglementProof, AnyhowWrapperError> {
    Ok(generate_holochain_proof_service(holochain_key, signed_did))
}

#[op2]
#[serde]
pub fn add_entanglement_proofs(#[serde] proofs: Vec<EntanglementProof>) -> Result<(), AnyhowWrapperError> {
    add_entanglement_proofs_service(proofs);
    Ok(())
}

#[op2]
#[serde]
pub fn delete_entanglement_proofs(#[serde] proofs: Vec<EntanglementProof>) -> Result<(), AnyhowWrapperError> {
    delete_entanglement_proof_service(proofs);
    Ok(())
}

#[op2]
#[serde]
pub fn get_entanglement_proofs() -> Result<Vec<EntanglementProof>, AnyhowWrapperError> {
    Ok(get_entanglement_proofs_service())
}

deno_core::extension!(
  entanglement_service,
  ops = [sign_device_key, generate_entanglement_proof, add_entanglement_proofs, delete_entanglement_proofs, get_entanglement_proofs],
  esm_entry_point = "ext:entanglement_service/entanglement_service_extension.js",
  esm = [dir "src/entanglement_service", "entanglement_service_extension.js"]
);
