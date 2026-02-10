//! Test utilities for perspective_diff_sync sweettest tests

use std::path::PathBuf;

use chrono::Utc;
use futures::future;
use holochain::conductor::api::error::ConductorApiError;
use holochain::sweettest::{SweetAgents, SweetCell, SweetConductor, SweetConductorBatch, SweetDnaFile};
use holochain_types::prelude::*;
use perspective_diff_sync_integrity::{
    CommitInput, ExpressionProof, LinkExpression, PerspectiveDiff, PullResult, Triple,
};
use rand::Rng;
use serde::{de::DeserializeOwned, Serialize};
use uuid::Uuid;

/// Path to the compiled DNA file
pub fn dna_path() -> PathBuf {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    PathBuf::from(manifest_dir)
        .join("../../workdir/perspective-diff-sync.dna")
}

/// Load the perspective_diff_sync DNA
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

/// Generate a random link expression for testing
pub fn generate_link_expression(author: &str) -> LinkExpression {
    let mut rng = rand::thread_rng();
    let random_id: u64 = rng.gen();

    LinkExpression {
        author: format!("did:test:{}", author),
        data: Triple {
            source: Some(format!("test://source/{}", random_id)),
            target: Some(format!("test://target/{}", random_id)),
            predicate: Some(format!("test://predicate/{}", Uuid::new_v4())),
        },
        timestamp: Utc::now(),
        proof: ExpressionProof {
            signature: format!("sig_{}", Uuid::new_v4()),
            key: format!("key_{}", author),
        },
    }
}

/// Create a commit input with a single link addition
pub fn create_commit_input(author: &str) -> CommitInput {
    CommitInput {
        diff: PerspectiveDiff {
            additions: vec![generate_link_expression(author)],
            removals: vec![],
        },
        my_did: format!("did:test:{}", author),
    }
}

/// Create a commit input with multiple link additions
pub fn create_commit_input_multi(author: &str, count: usize) -> CommitInput {
    let additions: Vec<LinkExpression> = (0..count)
        .map(|_| generate_link_expression(author))
        .collect();

    CommitInput {
        diff: PerspectiveDiff {
            additions,
            removals: vec![],
        },
        my_did: format!("did:test:{}", author),
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
        .call(&cell.zome("perspective_diff_sync"), fn_name, payload)
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
        .call_fallible(&cell.zome("perspective_diff_sync"), fn_name, payload)
        .await
}

/// Retry a zome call until it succeeds or matches a predicate
pub async fn retry_until_success<I, O, F>(
    conductor: &SweetConductor,
    cell: &SweetCell,
    fn_name: &str,
    payload: I,
    max_retries: usize,
    delay_ms: u64,
    predicate: F,
) -> Result<O, String>
where
    I: Serialize + std::fmt::Debug + Clone,
    O: DeserializeOwned + std::fmt::Debug,
    F: Fn(&O) -> bool,
{
    for attempt in 1..=max_retries {
        match call_zome_fallible::<I, O>(conductor, cell, fn_name, payload.clone()).await {
            Ok(result) => {
                if predicate(&result) {
                    return Ok(result);
                }
                println!("Attempt {}/{}: predicate not satisfied, retrying...", attempt, max_retries);
            }
            Err(e) => {
                println!("Attempt {}/{}: call failed with {:?}, retrying...", attempt, max_retries, e);
            }
        }
        tokio::time::sleep(tokio::time::Duration::from_millis(delay_ms)).await;
    }
    Err(format!("Failed after {} retries", max_retries))
}

/// Wait for DHT synchronization between conductors
pub async fn await_consistency(delay_ms: u64) {
    tokio::time::sleep(tokio::time::Duration::from_millis(delay_ms)).await;
}

/// Commit a link expression and return the commit hash
pub async fn commit_link(
    conductor: &SweetConductor,
    cell: &SweetCell,
    author: &str,
) -> ActionHash {
    let input = create_commit_input(author);
    call_zome(conductor, cell, "commit", input).await
}

/// Commit multiple link expressions and return the commit hash
pub async fn commit_links(
    conductor: &SweetConductor,
    cell: &SweetCell,
    author: &str,
    count: usize,
) -> ActionHash {
    let input = create_commit_input_multi(author, count);
    call_zome(conductor, cell, "commit", input).await
}

/// Pull changes from a remote revision
pub async fn pull(
    conductor: &SweetConductor,
    cell: &SweetCell,
    hash: ActionHash,
    is_scribe: bool,
) -> PullResult {
    #[derive(Serialize, Debug)]
    struct PullArgs {
        hash: ActionHash,
        is_scribe: bool,
    }

    call_zome(conductor, cell, "pull", PullArgs { hash, is_scribe }).await
}

/// Get current revision hash
pub async fn current_revision(
    conductor: &SweetConductor,
    cell: &SweetCell,
) -> Option<ActionHash> {
    call_zome(conductor, cell, "current_revision", ()).await
}

/// Create DID to public key link
pub async fn create_did_link(
    conductor: &SweetConductor,
    cell: &SweetCell,
    did: &str,
) {
    call_zome::<_, ()>(conductor, cell, "create_did_pub_key_link", did.to_string()).await
}
