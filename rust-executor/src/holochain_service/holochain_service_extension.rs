use deno_core::{error::AnyError, include_js_files, op, Extension};
use log::info;

use crate::holochain_service::{HolochainService, LocalConductorConfig};

use super::get_global_conductor;

#[op]
async fn start_holochain_conductor(config: LocalConductorConfig) -> Result<(), AnyError> {
    HolochainService::new(config).await?;
    Ok(())
}

#[op]
async fn log_dht_status() -> Result<(), AnyError> {
    let conductor = get_global_conductor().await;
    let dht_status = conductor.conductor.dump_network_metrics(None).await?;
    info!("DHT Status: {:?}", dht_status);
    Ok(())
}

pub fn build() -> Extension {
    Extension::builder("holochain_service")
        .js(include_js_files!(holochain_service "holochain_service_extension.js",))
        .ops(vec![
            start_holochain_conductor::decl(),
            log_dht_status::decl(),
        ])
        .force_op_registration()
        .build()
}
