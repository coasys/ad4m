//! Test utilities for file-storage sweettest tests

use std::path::PathBuf;

use chrono::Utc;
use futures::future;
use holochain::sweettest::{SweetAgents, SweetCell, SweetConductor, SweetConductorBatch, SweetDnaFile};
use holochain_serialized_bytes::prelude::*;
use holochain_types::prelude::*;
use integrity::{ExpressionProof, FileChunk, FileExpression, FileMetadata};
use serde::{de::DeserializeOwned, Serialize};
use uuid::Uuid;

/// Path to the compiled DNA file
pub fn dna_path() -> PathBuf {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    PathBuf::from(manifest_dir)
        .join("../../workdir/file-storage.dna")
}

/// Load the file-storage DNA
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
        .setup_app_for_agent("file-storage-test", agent, &[dna])
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
        .setup_app_for_zipped_agents("file-storage-test", &agents, &[dna.clone()])
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
        .call(&cell.zome("file_storage"), fn_name, payload)
        .await
}

/// Create a FileChunk from raw bytes
pub fn create_file_chunk(data: &[u8]) -> FileChunk {
    FileChunk(SerializedBytes::from(UnsafeBytes::from(data.to_vec())))
}

/// Create a test FileExpression with the given chunk hashes
pub fn create_test_file_expression(name: &str, size: usize, chunk_hashes: Vec<EntryHash>) -> FileExpression {
    FileExpression {
        author: "did:test:alice".to_string(),
        proof: ExpressionProof {
            signature: format!("sig_{}", Uuid::new_v4()),
            key: "did:test:alice#primary".to_string(),
        },
        timestamp: Utc::now(),
        data: FileMetadata {
            name: name.to_string(),
            size,
            file_type: "application/octet-stream".to_string(),
            checksum: format!("checksum_{}", Uuid::new_v4()),
            chunks_hashes: chunk_hashes,
        },
    }
}

/// Split data into chunks of the given size and store them, returning hashes
pub async fn upload_chunks(
    conductor: &SweetConductor,
    cell: &SweetCell,
    data: &[u8],
    chunk_size: usize,
) -> Vec<EntryHash> {
    let mut hashes = Vec::new();
    for chunk_data in data.chunks(chunk_size) {
        let chunk = create_file_chunk(chunk_data);
        let hash: EntryHash = call_zome(conductor, cell, "store_chunk", chunk).await;
        hashes.push(hash);
    }
    hashes
}

/// Wait for DHT consistency between conductors
pub async fn await_consistency(delay_ms: u64) {
    tokio::time::sleep(tokio::time::Duration::from_millis(delay_ms)).await;
}
