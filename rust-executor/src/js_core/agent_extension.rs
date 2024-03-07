use deno_core::{error::AnyError, include_js_files, op2, Extension, Op};
use std::borrow::Cow;
use crate::{agent::{create_signed_expression, did, did_document, sign, sign_string_hex, signing_key_id, AgentService}, graphql::graphql_types::{Agent, AgentStatus}};

use super::utils::sort_json_value;

#[op2]
#[serde]
fn agent_did_document() -> Result<did_key::Document, AnyError> {
    Ok(did_document())
}

#[op2]
#[string]
fn agent_signing_key_id() -> Result<String, AnyError> {
    Ok(signing_key_id())
}

#[op2]
#[string]
fn agent_did() -> Result<String, AnyError> {
    Ok(did())
}

#[op2]
#[serde]
fn agent_create_signed_expression(#[serde] data: serde_json::Value) -> Result<serde_json::Value, AnyError> {
    let sorted_json = sort_json_value(&data);
    let signed_expression = create_signed_expression(sorted_json)?;
    Ok(serde_json::to_value(signed_expression)?)
}

#[op2]
#[serde]
fn agent_sign(#[buffer] payload: &[u8]) -> Result<Vec<u8>, AnyError> {
    sign(payload)
}


#[op2]
#[string]
fn agent_sign_string_hex(#[string] payload: String) -> Result<String, AnyError> {
    sign_string_hex(payload)
}

#[op2(fast)]
fn agent_is_initialized() -> Result<bool, AnyError> {
    let agent_instance = AgentService::instance();
    let agent_service = agent_instance.lock().expect("agent lock");
    let agent_ref: &AgentService = agent_service.as_ref().expect("agent instance");

    let is_initialized = agent_ref.is_initialized();

    Ok(is_initialized)
}

#[op2(fast)]
fn agent_is_unlocked() -> Result<bool, AnyError> {
    let agent_instance = AgentService::instance();
    let agent_service = agent_instance.lock().expect("agent lock");
    let agent_ref: &AgentService = agent_service.as_ref().expect("agent instance");

    let is_unlocked = agent_ref.is_unlocked();

    Ok(is_unlocked)
}

#[op2]
#[serde]
fn agent() -> Result<Agent, AnyError> {
    let agent_instance = AgentService::instance();
    let mut agent_service = agent_instance.lock().expect("agent lock");
    let agent_ref: &mut AgentService = agent_service.as_mut().expect("agent instance");

    let agent = agent_ref.agent.clone().unwrap();

    Ok(agent)
}

#[op2]
#[serde]
fn agent_load() -> Result<AgentStatus, AnyError> {
    let agent_instance = AgentService::instance();
    let mut agent_service = agent_instance.lock().expect("agent lock");
    let agent_ref: &mut AgentService = agent_service.as_mut().expect("agent instance");

    agent_ref.load();

    let agent_status = agent_ref.dump();

    Ok(agent_status)
}

#[op2(async)]
#[serde]
async fn agent_unlock(#[string] passphrase: String) -> Result<(), AnyError> {
    let agent_instance = AgentService::instance();
    let mut agent_service = agent_instance.lock().expect("agent lock");
    let agent_ref: &AgentService = agent_service.as_ref().expect("agent instance");

    agent_ref.unlock(passphrase).unwrap();

    Ok(())
}

#[op2(async)]
#[serde]
async fn agent_lock(#[string] passphrase: String) -> Result<(), AnyError> {
    let agent_instance = AgentService::instance();
    let mut agent_service = agent_instance.lock().expect("agent lock");
    let agent_ref: &AgentService = agent_service.as_ref().expect("agent instance");

    agent_ref.lock(passphrase);

    Ok(())
}

#[op2]
fn save_agent_profile(#[serde] agent: Agent) -> Result<(), AnyError> {
    let agent_instance = AgentService::instance();
    let mut agent_service = agent_instance.lock().expect("agent lock");
    let agent_ref: &mut AgentService = agent_service.as_mut().expect("agent instance");

    agent_ref.save_agent_profile(agent);

    Ok(())
}

pub fn build() -> Extension {
    Extension {
        name: "agent",
        js_files: Cow::Borrowed(&include_js_files!(rust_executor "src/js_core/agent_extension.js",)),
        ops: Cow::Borrowed(&[
            agent_did_document::DECL,
            agent_signing_key_id::DECL,
            agent_did::DECL,
            agent_create_signed_expression::DECL,
            agent_sign::DECL,
            agent_sign_string_hex::DECL,
            agent_is_initialized::DECL,
            agent_is_unlocked::DECL,
            agent::DECL,
            agent_load::DECL,
            agent_unlock::DECL,
            agent_lock::DECL,
            save_agent_profile::DECL,
        ]),
        ..Default::default()
    }
}