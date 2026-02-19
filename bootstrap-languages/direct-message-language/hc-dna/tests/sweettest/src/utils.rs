//! Test utilities for direct-message-language sweettest tests

use std::path::PathBuf;

use chrono::Utc;
use futures::future;
use holochain::conductor::api::error::ConductorApiError;
use holochain::sweettest::{SweetAgents, SweetCell, SweetConductor, SweetConductorBatch, SweetDnaFile};
use holochain_types::prelude::*;
use direct_message_integrity::ad4m::{ExpressionProof, Perspective, PerspectiveExpression};
use serde::{de::DeserializeOwned, Serialize};
use uuid::Uuid;

/// Path to the compiled DNA file
pub fn dna_path() -> PathBuf {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    PathBuf::from(manifest_dir)
        .join("../../workdir/direct-message-language.dna")
}

/// Load the direct-message-language DNA
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
        .setup_app_for_agent("direct-message-test", agent, &[dna])
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
        .setup_app_for_zipped_agents("direct-message-test", &agents, &[dna.clone()])
        .await
        .expect("Failed to setup apps");

    if network {
        conductors.exchange_peer_info().await;
    }

    let cells: Vec<SweetCell> = apps.iter().map(|app| app.cells()[0].clone()).collect();
    (conductors, cells)
}

/// Helper to call a zome function on a cell
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
    conductor
        .call(&cell.zome("direct-message"), fn_name, payload)
        .await
}

/// Helper to call a zome function that might fail
pub async fn call_zome_fallible<I, O>(
    conductor: &SweetConductor,
    cell: &SweetCell,
    fn_name: &str,
    payload: I,
) -> Result<O, ConductorApiError>
where
    I: Serialize + std::fmt::Debug,
    O: DeserializeOwned + std::fmt::Debug,
{
    conductor
        .call_fallible(&cell.zome("direct-message"), fn_name, payload)
        .await
}

/// Create a test PerspectiveExpression with empty links
pub fn create_test_perspective_expression(author: &str) -> PerspectiveExpression {
    PerspectiveExpression {
        author: author.to_string(),
        timestamp: Utc::now(),
        data: Perspective { links: vec![] },
        proof: ExpressionProof {
            signature: format!("sig_{}", Uuid::new_v4()),
            key: format!("{}#primary", author),
        },
    }
}

/// Wait for DHT/signal propagation between conductors
pub async fn await_consistency(delay_ms: u64) {
    tokio::time::sleep(tokio::time::Duration::from_millis(delay_ms)).await;
}
