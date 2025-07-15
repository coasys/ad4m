use deno_runtime::ops::bootstrap::SnapshotOptions;
use deno_runtime::snapshot::create_runtime_snapshot;
use rust_executor::entanglement_service::entanglement_service_extension::entanglement_service;
use rust_executor::holochain_service::holochain_service_extension::holochain_service;
use rust_executor::js_core::agent_extension::agent_service;
use rust_executor::js_core::languages_extension::language_service;
use rust_executor::js_core::pubsub_extension::pubsub_service;
use rust_executor::js_core::signature_extension::signature_service;
use rust_executor::js_core::utils_extension::utils_service;
use rust_executor::js_core::wallet_extension::wallet_service;
use rust_executor::runtime_service::runtime_service_extension::runtime_service;
use std::path::Path;

fn main() {
    // Define output path for the snapshot
    let snapshot_path = Path::new("CUSTOM_DENO_SNAPSHOT.bin").to_path_buf();

    // Define extensions to include in the snapshot
    let extensions = vec![
        wallet_service::init(),
        utils_service::init(),
        pubsub_service::init(),
        holochain_service::init(),
        signature_service::init(),
        agent_service::init(),
        entanglement_service::init(),
        runtime_service::init(),
        language_service::init(),
    ];

    create_runtime_snapshot(snapshot_path, SnapshotOptions::default(), extensions);
    println!("Snapshot generated successfully!");
}
