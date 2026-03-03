use crate::{
    agent::{sign_string_hex, AgentService},
    db::Ad4mDb,
    graphql::graphql_types::EntanglementProof,
};

pub mod entanglement_service_extension;

pub fn sign_device_key(device_key: String, device_key_type: String) -> EntanglementProof {
    let signed_device_key = sign_string_hex(device_key.clone()).unwrap();

    AgentService::with_global_instance(|agent_service| EntanglementProof {
        did: agent_service.did.clone().unwrap(),
        did_signing_key_id: agent_service.signing_key_id.clone().unwrap(),
        device_key_type,
        device_key,
        device_key_signed_by_did: signed_device_key,
        did_signed_by_device_key: None,
    })
}

pub fn generate_holochain_proof(holochain_key: String, signed_did: String) -> EntanglementProof {
    let device_key_type = "holochain".to_string();
    let signed_holo_key = sign_string_hex(holochain_key.clone()).unwrap();

    AgentService::with_global_instance(|agent_service| EntanglementProof {
        did: agent_service.did.clone().unwrap(),
        did_signing_key_id: agent_service.signing_key_id.clone().unwrap(),
        device_key_type,
        device_key: holochain_key,
        device_key_signed_by_did: signed_holo_key,
        did_signed_by_device_key: Some(signed_did),
    })
}

pub fn add_entanglement_proofs(proofs: Vec<EntanglementProof>) {
    Ad4mDb::with_global_instance(|db| db.add_entanglement_proofs(proofs))
        .map_err(|e| e.to_string())
        .unwrap_or(())
}

pub fn delete_entanglement_proof(proofs: Vec<EntanglementProof>) {
    Ad4mDb::with_global_instance(|db| db.remove_entanglement_proofs(proofs))
        .map_err(|e| e.to_string())
        .unwrap_or(())
}

pub fn get_entanglement_proofs() -> Vec<EntanglementProof> {
    Ad4mDb::with_global_instance(|db| db.get_all_entanglement_proofs())
        .map_err(|e| e.to_string())
        .unwrap_or_default()
}
