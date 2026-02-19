//! Tests for file-storage zome functions

use crate::utils::{
    await_consistency, call_zome, create_file_chunk, create_test_file_expression,
    setup_1_conductor, setup_conductors, upload_chunks,
};
use holochain_types::prelude::*;
use integrity::{FileChunk, FileExpression};

/// Test storing and retrieving a single file chunk
#[tokio::test(flavor = "multi_thread")]
async fn test_store_and_retrieve_chunk() {
    let (conductor, cell) = setup_1_conductor().await;

    let test_data = b"Hello, World! This is a test chunk.";
    let chunk = create_file_chunk(test_data);

    // Store the chunk
    let hash: EntryHash = call_zome(&conductor, &cell, "store_chunk", chunk.clone()).await;

    // Retrieve the chunk
    let retrieved: Option<FileChunk> = call_zome(&conductor, &cell, "get_file_chunk", hash).await;

    assert!(retrieved.is_some(), "Chunk should be retrievable after storage");

    // Verify the content matches
    let retrieved_bytes: Vec<u8> = retrieved.unwrap().0.bytes().to_vec();
    assert_eq!(
        retrieved_bytes,
        test_data.to_vec(),
        "Retrieved chunk bytes should match original data"
    );
}

/// Test that storing the same chunk twice returns the same hash (deduplication)
#[tokio::test(flavor = "multi_thread")]
async fn test_chunk_deduplication() {
    let (conductor, cell) = setup_1_conductor().await;

    let test_data = b"Duplicate chunk data";
    let chunk1 = create_file_chunk(test_data);
    let chunk2 = create_file_chunk(test_data);

    // Store the same chunk twice
    let hash1: EntryHash = call_zome(&conductor, &cell, "store_chunk", chunk1).await;
    let hash2: EntryHash = call_zome(&conductor, &cell, "store_chunk", chunk2).await;

    // Both should return the same entry hash (content-addressed)
    assert_eq!(hash1, hash2, "Same content should produce the same entry hash");
}

/// Test storing and retrieving a FileExpression
#[tokio::test(flavor = "multi_thread")]
async fn test_store_and_retrieve_file_expression() {
    let (conductor, cell) = setup_1_conductor().await;

    // First store some chunks to get their hashes
    let chunk_data = b"File content data";
    let chunk_hashes = upload_chunks(&conductor, &cell, chunk_data, chunk_data.len()).await;

    // Create a FileExpression with the chunk hashes
    let file_expression = create_test_file_expression("test.txt", chunk_data.len(), chunk_hashes);

    // Store the FileExpression
    let expr_hash: EntryHash =
        call_zome(&conductor, &cell, "store_file_expression", file_expression.clone()).await;

    // Retrieve it
    let retrieved: Option<FileExpression> =
        call_zome(&conductor, &cell, "get_file_expression", expr_hash).await;

    assert!(retrieved.is_some(), "FileExpression should be retrievable after storage");

    let retrieved = retrieved.unwrap();
    assert_eq!(
        retrieved.author, file_expression.author,
        "Author should match"
    );
    assert_eq!(
        retrieved.data.name, file_expression.data.name,
        "File name should match"
    );
    assert_eq!(
        retrieved.data.size, file_expression.data.size,
        "File size should match"
    );
    assert_eq!(
        retrieved.data.file_type, file_expression.data.file_type,
        "File type should match"
    );
    assert_eq!(
        retrieved.data.checksum, file_expression.data.checksum,
        "Checksum should match"
    );
    assert_eq!(
        retrieved.data.chunks_hashes, file_expression.data.chunks_hashes,
        "Chunk hashes should match"
    );
}

/// Test multi-chunk upload: split data into multiple chunks, store all,
/// then retrieve and verify each chunk
#[tokio::test(flavor = "multi_thread")]
async fn test_multi_chunk_upload_and_download() {
    let (conductor, cell) = setup_1_conductor().await;

    // Create 10KB of test data
    let test_data: Vec<u8> = (0..10240).map(|i| (i % 256) as u8).collect();
    let chunk_size = 1024; // 1KB chunks

    // Upload all chunks
    let chunk_hashes = upload_chunks(&conductor, &cell, &test_data, chunk_size).await;

    assert_eq!(
        chunk_hashes.len(),
        10,
        "Should have 10 chunks for 10KB data with 1KB chunk size"
    );

    // Retrieve and verify each chunk
    let mut reconstructed: Vec<u8> = Vec::new();
    for (i, hash) in chunk_hashes.iter().enumerate() {
        let retrieved: Option<FileChunk> =
            call_zome(&conductor, &cell, "get_file_chunk", hash.clone()).await;

        assert!(retrieved.is_some(), "Chunk {} should be retrievable", i);
        let chunk_bytes = retrieved.unwrap().0.bytes().to_vec();
        reconstructed.extend_from_slice(&chunk_bytes);
    }

    assert_eq!(
        reconstructed, test_data,
        "Reconstructed data should match original"
    );
}

/// Test multi-agent file sharing: Alice uploads, Bob downloads after DHT sync
#[tokio::test(flavor = "multi_thread")]
async fn test_multi_agent_file_sharing() {
    let (mut conductors, cells) = setup_conductors(2, true).await;
    let alice_cell = &cells[0];
    let bob_cell = &cells[1];

    // Alice creates test data
    let test_data: Vec<u8> = (0..2048).map(|i| (i % 256) as u8).collect();
    let chunk_size = 1024; // 1KB chunks

    // Alice uploads chunks
    let chunk_hashes = upload_chunks(&conductors[0], alice_cell, &test_data, chunk_size).await;

    // Alice creates and stores a FileExpression
    let file_expression =
        create_test_file_expression("shared_file.bin", test_data.len(), chunk_hashes.clone());
    let expr_hash: EntryHash =
        call_zome(&conductors[0], alice_cell, "store_file_expression", file_expression.clone())
            .await;

    // Wait for DHT propagation
    await_consistency(3000).await;

    // Bob retrieves the FileExpression
    let retrieved_expr: Option<FileExpression> =
        call_zome(&conductors[1], bob_cell, "get_file_expression", expr_hash).await;

    assert!(
        retrieved_expr.is_some(),
        "Bob should be able to retrieve Alice's FileExpression"
    );

    let retrieved_expr = retrieved_expr.unwrap();
    assert_eq!(
        retrieved_expr.data.name, "shared_file.bin",
        "File name should match"
    );
    assert_eq!(
        retrieved_expr.data.size,
        test_data.len(),
        "File size should match"
    );

    // Bob downloads the chunks and reconstructs the file
    let mut reconstructed: Vec<u8> = Vec::new();
    for hash in &retrieved_expr.data.chunks_hashes {
        let chunk: Option<FileChunk> =
            call_zome(&conductors[1], bob_cell, "get_file_chunk", hash.clone()).await;

        assert!(chunk.is_some(), "Bob should be able to retrieve each chunk");
        let chunk_bytes = chunk.unwrap().0.bytes().to_vec();
        reconstructed.extend_from_slice(&chunk_bytes);
    }

    assert_eq!(
        reconstructed, test_data,
        "Bob's reconstructed data should match Alice's original"
    );
}

/// Test retrieving a non-existent file expression returns None
#[tokio::test(flavor = "multi_thread")]
async fn test_get_nonexistent_file_expression() {
    let (conductor, cell) = setup_1_conductor().await;

    // EntryHash prefix: 0x84, 0x21, 0x24 followed by 36 bytes of data
    let mut raw = vec![0x84u8, 0x21, 0x24];
    raw.extend_from_slice(&[0xffu8; 36]);
    let fake_hash = EntryHash::from_raw_39(raw);

    let result: Option<FileExpression> =
        call_zome(&conductor, &cell, "get_file_expression", fake_hash).await;

    assert!(result.is_none(), "Getting a non-existent FileExpression should return None");
}

/// Test retrieving a non-existent chunk returns None
#[tokio::test(flavor = "multi_thread")]
async fn test_get_nonexistent_chunk() {
    let (conductor, cell) = setup_1_conductor().await;

    // EntryHash prefix: 0x84, 0x21, 0x24 followed by 36 bytes of data
    let mut raw = vec![0x84u8, 0x21, 0x24];
    raw.extend_from_slice(&[0xffu8; 36]);
    let fake_hash = EntryHash::from_raw_39(raw);

    let result: Option<FileChunk> =
        call_zome(&conductor, &cell, "get_file_chunk", fake_hash).await;

    assert!(result.is_none(), "Getting a non-existent chunk should return None");
}
