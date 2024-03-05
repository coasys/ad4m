use std::{collections::HashSet, env, fs::File, io::{BufReader, Write}, path::{self, Path}, sync::Mutex};

use crate::{agent::{sign, sign_string_hex, AgentService}, graphql::graphql_types::EntanglementProof};

pub(crate) mod entanglement_service_extension;

lazy_static! {
  static ref ENTANGLEMENT_PROOF_FILE: Mutex<String> = Mutex::new(format!("{}/entanglement_proof.json", env::var("APPS_DATA_PATH").unwrap_or_else(|_| ".".to_string())));
}

pub fn sign_device_key(device_key: String, device_key_type: String) -> EntanglementProof {
  let agent_instance = AgentService::instance();
  let mut agent_service = agent_instance.lock().expect("agent lock");
  let agent_ref: &AgentService = agent_service.as_ref().expect("agent instance");

  let signed_device_key = sign_string_hex(device_key.clone()).unwrap();

  EntanglementProof {
    did: agent_ref.did.clone(),
    did_signing_key_id: agent_ref.signing_key_id.clone().unwrap(),
    device_key_type,
    device_key,
    device_key_signed_by_did: signed_device_key,
    did_signed_by_device_key: None
  }
}

pub fn generate_holochain_proof(holochain_key: String, signed_did: String) -> EntanglementProof {
  let agent_instance = AgentService::instance();
  let mut agent_service = agent_instance.lock().expect("agent lock");
  let agent_ref: &AgentService = agent_service.as_ref().expect("agent instance");

  let device_key_type = "holochain".to_string();
  let signed_holo_key = sign_string_hex(holochain_key.clone()).unwrap();

  EntanglementProof {
    did: agent_ref.did.clone(),
    did_signing_key_id: agent_ref.signing_key_id.clone().unwrap(),
    device_key_type,
    device_key: holochain_key,
    device_key_signed_by_did: signed_holo_key,
    did_signed_by_device_key: Some(signed_did)
  }
}

pub fn add_entanglement_proofs(proofs: Vec<EntanglementProof>) {
  let file_path = ENTANGLEMENT_PROOF_FILE.lock().unwrap().clone();
  let mut entanglement_proofs: HashSet<EntanglementProof> = HashSet::new();

  if path::Path::new(&file_path).exists() {
    let file = File::open(&file_path).unwrap();
    let reader = BufReader::new(file);
    entanglement_proofs = serde_json::from_reader(reader).unwrap();
  }

  for proof in proofs {
    entanglement_proofs.insert(proof);
  }

  let mut file = File::create(&file_path).unwrap();
  file.write_all(serde_json::to_string(&entanglement_proofs).unwrap().as_bytes()).unwrap();
}

pub fn delete_entanglement_proof(proof: EntanglementProof) {
  let file_path = ENTANGLEMENT_PROOF_FILE.lock().unwrap().clone();
  let mut entanglement_proofs: HashSet<EntanglementProof> = HashSet::new();

  if path::Path::new(&file_path.clone()).exists() {
    let file = File::open(file_path.clone()).unwrap();
    let reader = BufReader::new(file);
    entanglement_proofs = serde_json::from_reader(reader).unwrap();
  }

  entanglement_proofs.remove(&proof);

  let mut file = File::create(&file_path).unwrap();
  file.write_all(serde_json::to_string(&entanglement_proofs).unwrap().as_bytes()).unwrap();
}

pub fn get_entanglement_proofs() -> Vec<EntanglementProof> {
  let file_path = ENTANGLEMENT_PROOF_FILE.lock().unwrap().clone();

  if Path::new(&file_path).exists() {
      let file = File::open(&file_path).unwrap();
      let reader = BufReader::new(file);
      let entanglement_proofs: HashSet<EntanglementProof> = serde_json::from_reader(reader).unwrap();
      entanglement_proofs.into_iter().collect()
  } else {
      Vec::new()
  }
}