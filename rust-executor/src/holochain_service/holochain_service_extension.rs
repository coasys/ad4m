use deno_core::{error::AnyError, include_js_files, op, Extension};
use holochain::{
    conductor::api::AppInfo,
    prelude::{
        agent_store::AgentInfoSigned, hash_type::Agent, ExternIO, HoloHash, InstallAppPayload,
        Signature, ZomeCallResponse,
    },
};
use log::info;

use crate::holochain_service::{HolochainService, LocalConductorConfig};

use super::get_holochain_service;

#[op]
async fn start_holochain_conductor(config: LocalConductorConfig) -> Result<(), AnyError> {
    HolochainService::init(config).await?;
    Ok(())
}

#[op]
async fn log_dht_status() -> Result<(), AnyError> {
    let interface = get_holochain_service().await;
    let metrics = interface.get_network_metrics().await?;
    info!("DHT metrics: {:?}", serde_json::Value::try_from(metrics)?);
    Ok(())
}

#[op]
async fn install_app(install_app_payload: InstallAppPayload) -> Result<AppInfo, AnyError> {
    let interface = get_holochain_service().await;
    interface.install_app(install_app_payload).await
}

#[op]
async fn get_app_info(app_id: String) -> Result<Option<AppInfo>, AnyError> {
    let interface = get_holochain_service().await;
    interface.get_app_info(app_id).await
}

//TODO
//Have install app use lair to generate the membrane proof
#[op]
async fn call_zome_function(
    app_id: String,
    cell_name: String,
    zome_name: String,
    fn_name: String,
    payload: Option<ExternIO>,
) -> Result<ZomeCallResponse, AnyError> {
    let interface = get_holochain_service().await;
    interface
        .call_zome_function(app_id, cell_name, zome_name, fn_name, payload)
        .await
}

#[op]
async fn agent_infos() -> Result<Vec<AgentInfoSigned>, AnyError> {
    let interface = get_holochain_service().await;
    interface.agent_infos().await
}

#[op]
async fn add_agent_infos(agent_infos_payload: Vec<AgentInfoSigned>) -> Result<(), AnyError> {
    let interface = get_holochain_service().await;
    interface.add_agent_infos(agent_infos_payload).await
}

#[op]
async fn remove_app(app_id: String) -> Result<(), AnyError> {
    let interface = get_holochain_service().await;
    interface.remove_app(app_id).await
}

#[op]
async fn sign_string(data: String) -> Result<Signature, AnyError> {
    let interface = get_holochain_service().await;
    interface.sign(data).await
}

#[op]
async fn shutdown() -> Result<(), AnyError> {
    let interface = get_holochain_service().await;
    interface.shutdown().await
}

#[op]
async fn get_agent_key() -> Result<HoloHash<Agent>, AnyError> {
    let interface = get_holochain_service().await;
    interface.get_agent_key().await
}

//TODO: implement dna packing and unpacking (not currently possible with holochain_cli_bundle unpack / pack functions since it does not exposed the functions in lib)

//Implement signal callbacks from dna/holochain to js

pub fn build() -> Extension {
    Extension::builder("holochain_service")
        .js(include_js_files!(holochain_service "holochain_service_extension.js",))
        .ops(vec![
            start_holochain_conductor::decl(),
            log_dht_status::decl(),
            install_app::decl(),
            get_app_info::decl(),
            call_zome_function::decl(),
            agent_infos::decl(),
            add_agent_infos::decl(),
            remove_app::decl(),
            sign_string::decl(),
            shutdown::decl(),
            get_agent_key::decl(),
        ])
        .force_op_registration()
        .build()
}