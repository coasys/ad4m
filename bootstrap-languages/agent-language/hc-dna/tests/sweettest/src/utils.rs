//! Test utilities for agent_store sweettest tests

use std::path::PathBuf;

use chrono::Utc;
use futures::future;
use holochain::sweettest::{SweetAgents, SweetCell, SweetConductor, SweetConductorBatch, SweetDnaFile};
use holochain_types::prelude::*;
use agent_store_integrity::{AgentExpression, AgentExpressionData, ExpressionProof};
use serde::{de::DeserializeOwned, Serialize};
use uuid::Uuid;

/// Path to the compiled DNA file
pub fn dna_path() -> PathBuf {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    PathBuf::from(manifest_dir)
        .join("../../workdir/agent-store.dna")
}

/// Load the agent-store DNA
pub async fn load_dna() -> DnaFile {
    SweetDnaFile::from_bundle(&dna_path())
        .await
        .expect("Failed to load DNA bundle")
}

/// Setup a single conductor with one agent
pub async fn setup_1_conductor() -> (SweetConductor, SweetCell) {
    let dna = load_dna().await;
    let mut conductor = SweetConductor::standard().await;
    let agent = SweetAgents::one(conductor.keystore()).await;
    let app = conductor
        .setup_app_for_agent("agent-store-test", agent, &[dna])
        .await
        .expect("Failed to setup app");
    let cell = app.cells()[0].clone();
    (conductor, cell)
}

/// Setup multiple conductors with agents, optionally networked together
pub async fn setup_conductors(n: usize, network: bool) -> (SweetConductorBatch, Vec<SweetCell>) {
    let dna = load_dna().await;
    let mut conductors = SweetConductorBatch::standard(n).await;

    let agents: Vec<AgentPubKey> = future::join_all(
        conductors.iter().map(|c| async {
            SweetAgents::one(c.keystore()).await
        })
    ).await;

    let apps = conductors
        .setup_app_for_zipped_agents("agent-store-test", &agents, &[dna.clone()])
        .await
        .expect("Failed to setup apps");

    if network {
        conductors.exchange_peer_info().await;
    }

    let cells: Vec<SweetCell> = apps.iter().map(|app| app.cells()[0].clone()).collect();
    (conductors, cells)
}

/// Call a zome function with type-safe serialization
pub async fn call_zome<I, O>(
    conductor: &SweetConductor,
    cell: &SweetCell,
    fn_name: &str,
    payload: I,
) -> O
where
    I: Serialize + std::fmt::Debug,
    O: DeserializeOwned + std::fmt::Debug,
{
    let zome = cell.zome("agent_store");
    conductor
        .call(&zome, fn_name, payload)
        .await
}

/// Create a test agent expression
pub fn create_test_agent_expression(did: &str, direct_message_language: Option<String>) -> AgentExpression {
    AgentExpression {
        author: did.to_string(),
        timestamp: Utc::now(),
        data: AgentExpressionData {
            did: did.to_string(),
            perspective: None,
            direct_message_language,
            authorised_keys: vec![],
            revoked_keys: vec![],
        },
        proof: ExpressionProof {
            signature: format!("sig_{}", Uuid::new_v4()),
            key: format!("key_{}", did),
        },
    }
}

/// Helper to wait for DHT consistency between conductors
pub async fn await_consistency(delay_secs: u64) {
    tokio::time::sleep(tokio::time::Duration::from_secs(delay_secs)).await;
}
