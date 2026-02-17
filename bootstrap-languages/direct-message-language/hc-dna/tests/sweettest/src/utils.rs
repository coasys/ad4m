//! Test utilities for direct_message sweettest tests

use std::path::PathBuf;

use chrono::Utc;
use futures::future;
use holochain::conductor::api::error::ConductorApiError;
use holochain::sweettest::{SweetAgents, SweetCell, SweetConductor, SweetConductorBatch, SweetDnaFile};
use holochain_types::prelude::*;
use direct_message_integrity::ad4m::{ExpressionProof, Link, LinkExpression, Perspective, PerspectiveExpression};
use serde::{de::DeserializeOwned, Serialize};

/// Path to the compiled DNA file
pub fn dna_path() -> PathBuf {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    PathBuf::from(manifest_dir)
        .join("../../workdir/direct-message-language.dna")
}

/// Load the direct_message DNA
pub async fn load_dna() -> DnaFile {
    SweetDnaFile::from_bundle(&dna_path())
        .await
        .expect("Failed to load DNA bundle")
}

/// Setup a single conductor with one agent
pub async fn setup_1_conductor() -> (SweetConductor, SweetCell) {
    let dna = load_dna().await;
    let mut conductor = SweetConductor::from_standard_config().await;
    let agent = SweetAgents::one(conductor.keystore()).await;
    let app = conductor
        .setup_app_for_agent("test-app", agent, &[dna])
        .await
        .expect("Failed to setup app");
    let cell = app.cells()[0].clone();
    (conductor, cell)
}

/// Setup multiple conductors with agents, optionally networked together
pub async fn setup_conductors(n: usize, network: bool) -> (SweetConductorBatch, Vec<SweetCell>) {
    let dna = load_dna().await;
    let mut conductors = SweetConductorBatch::from_standard_config(n).await;

    let agents: Vec<AgentPubKey> = future::join_all(
        conductors.iter().map(|c| async {
            SweetAgents::one(c.keystore()).await
        })
    ).await;

    let apps = conductors
        .setup_app_for_zipped_agents("test-app", &agents, &[dna.clone()])
        .await
        .expect("Failed to setup apps");

    if network {
        conductors.exchange_peer_info().await;
    }

    let cells: Vec<SweetCell> = apps.iter().map(|app| app.cells()[0].clone()).collect();
    (conductors, cells)
}

/// Generate a test PerspectiveExpression (message)
pub fn generate_message(author: &str) -> PerspectiveExpression {
    PerspectiveExpression {
        author: format!("did:test:{}", author),
        timestamp: Utc::now(),
        data: Perspective {
            links: vec![],
        },
        proof: ExpressionProof {
            signature: format!("sig_{}", author),
            key: format!("did:test:{}#primary", author),
        },
    }
}

/// Generate a test PerspectiveExpression with links
pub fn generate_message_with_link(author: &str) -> PerspectiveExpression {
    // Note: LinkExpression fields are private, so we need to use serde
    let link_json = serde_json::json!({
        "author": format!("did:test:{}", author),
        "timestamp": Utc::now(),
        "data": {
            "source": format!("did:test:{}", author),
            "target": "literal://string:online",
            "predicate": null
        },
        "proof": {
            "signature": format!("sig_{}", author),
            "key": format!("did:test:{}#primary", author)
        }
    });
    let link: LinkExpression = serde_json::from_value(link_json).expect("Failed to create LinkExpression");

    PerspectiveExpression {
        author: format!("did:test:{}", author),
        timestamp: Utc::now(),
        data: Perspective {
            links: vec![link],
        },
        proof: ExpressionProof {
            signature: format!("sig_{}", author),
            key: format!("did:test:{}#primary", author),
        },
    }
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
        .call::<I, O>(&cell.zome("direct-message"), fn_name, payload)
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
        .call_fallible::<I, O>(&cell.zome("direct-message"), fn_name, payload)
        .await
}

/// Wait for network consistency
pub async fn await_consistency(ms: u64) {
    tokio::time::sleep(std::time::Duration::from_millis(ms)).await;
}
