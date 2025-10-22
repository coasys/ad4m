use super::utils::sort_json_value;
use crate::js_core::error::AnyhowWrapperError;
use crate::{
    agent::{
        create_signed_expression, did, did_document, did_for_context, sign_for_context,
        sign_string_hex, signing_key_id, AgentContext, AgentService,
    },
    graphql::graphql_types::{Agent, AgentStatus},
};
// use coasys_juniper::{FieldError, Value};
use deno_core::anyhow;
use deno_core::op2;

#[op2]
#[serde]
fn agent_did_document() -> Result<did_key::Document, AnyhowWrapperError> {
    Ok(did_document())
}

#[op2]
#[string]
fn agent_signing_key_id() -> Result<String, AnyhowWrapperError> {
    Ok(signing_key_id())
}

#[op2]
#[string]
fn agent_did() -> Result<String, AnyhowWrapperError> {
    Ok(did())
}

#[op2]
#[serde]
fn agent_create_signed_expression(
    #[serde] data: serde_json::Value,
) -> Result<serde_json::Value, AnyhowWrapperError> {
    let sorted_json = sort_json_value(&data);
    let signed_expression = create_signed_expression(sorted_json, &AgentContext::main_agent())
        .map_err(AnyhowWrapperError::from)?;
    serde_json::to_value(signed_expression).map_err(AnyhowWrapperError::from)
}

#[op2]
#[string]
fn agent_create_signed_expression_stringified(
    #[string] data: String,
) -> Result<String, AnyhowWrapperError> {
    let data: serde_json::Value = serde_json::from_str(&data)?;
    let sorted_json = sort_json_value(&data);
    let signed_expression = create_signed_expression(sorted_json, &AgentContext::main_agent())?;
    let stringified =
        serde_json::to_string(&signed_expression).map_err(AnyhowWrapperError::from)?;
    Ok(stringified)
}

#[op2]
#[serde]
fn agent_create_signed_expression_for_user(
    #[string] user_email: String,
    #[serde] data: serde_json::Value,
) -> Result<serde_json::Value, AnyhowWrapperError> {
    let sorted_json = sort_json_value(&data);
    let agent_context = AgentContext::for_user_email(user_email);
    let signed_expression =
        create_signed_expression(sorted_json, &agent_context).map_err(AnyhowWrapperError::from)?;
    serde_json::to_value(signed_expression).map_err(AnyhowWrapperError::from)
}

#[op2]
#[string]
fn agent_did_for_user(#[string] user_email: String) -> Result<String, AnyhowWrapperError> {
    let context = AgentContext::for_user_email(user_email);
    did_for_context(&context).map_err(AnyhowWrapperError::from)
}

#[op2]
#[serde]
fn agent_list_user_emails() -> Result<Vec<String>, AnyhowWrapperError> {
    AgentService::list_user_emails().map_err(AnyhowWrapperError::from)
}

#[op2]
#[string]
fn agent_get_user_did_by_email(#[string] user_email: String) -> Result<String, AnyhowWrapperError> {
    AgentService::get_user_did_by_email(&user_email).map_err(AnyhowWrapperError::from)
}

#[op2]
#[serde]
fn agent_get_all_local_user_dids() -> Result<Vec<String>, AnyhowWrapperError> {
    let mut dids = Vec::new();

    // Add main agent DID
    dids.push(did());

    // Add all user DIDs
    let user_emails = AgentService::list_user_emails().map_err(AnyhowWrapperError::from)?;
    for email in user_emails {
        let user_did =
            AgentService::get_user_did_by_email(&email).map_err(AnyhowWrapperError::from)?;
        dids.push(user_did);
    }

    Ok(dids)
}

#[op2]
#[serde]
fn agent_agent_for_user(
    #[string] user_email: String,
) -> Result<serde_json::Value, AnyhowWrapperError> {
    let agent = AgentService::load_user_agent_profile(&user_email)
        .map_err(|_| anyhow::anyhow!("User agent profile not found"))
        .map_err(AnyhowWrapperError::from)?;

    match agent {
        Some(agent) => serde_json::to_value(agent).map_err(AnyhowWrapperError::from),
        None => Err(AnyhowWrapperError::from(anyhow::anyhow!(
            "User agent profile not found"
        ))),
    }
}

#[op2]
#[serde]
fn agent_sign(#[buffer] payload: &[u8]) -> Result<Vec<u8>, AnyhowWrapperError> {
    sign_for_context(payload, &AgentContext::main_agent()).map_err(AnyhowWrapperError::from)
}

#[op2]
#[string]
fn agent_sign_string_hex(#[string] payload: String) -> Result<String, AnyhowWrapperError> {
    sign_string_hex(payload).map_err(AnyhowWrapperError::from)
}

#[op2(fast)]
fn agent_is_initialized() -> Result<bool, AnyhowWrapperError> {
    AgentService::with_global_instance(|agent_service| Ok(agent_service.is_initialized()))
}

#[op2(fast)]
fn agent_is_unlocked() -> Result<bool, AnyhowWrapperError> {
    AgentService::with_global_instance(|agent_service| Ok(agent_service.is_unlocked()))
}

#[op2]
#[serde]
fn agent() -> Result<Agent, AnyhowWrapperError> {
    AgentService::with_global_instance(|agent_service| {
        let mut agent = agent_service
            .agent
            .clone()
            .ok_or_else(|| AnyhowWrapperError::from(anyhow::anyhow!("Agent not found")))?;

        if agent.perspective.is_some() {
            agent.perspective.as_mut().unwrap().verify_link_signatures();
        }

        Ok(agent)
    })
}

#[op2]
#[serde]
fn agent_load() -> Result<AgentStatus, AnyhowWrapperError> {
    AgentService::with_mutable_global_instance(|agent_service| {
        // Only load if the agent is initialized (agent file exists)
        if agent_service.is_initialized() {
            agent_service.load();
        }
        Ok(agent_service.dump())
    })
}

#[op2(async)]
#[serde]
async fn agent_unlock(#[string] passphrase: String) -> Result<(), AnyhowWrapperError> {
    AgentService::with_global_instance(|agent_service| agent_service.unlock(passphrase))
        .map_err(AnyhowWrapperError::from)
}

#[op2(async)]
#[serde]
async fn agent_lock(#[string] passphrase: String) -> Result<(), AnyhowWrapperError> {
    AgentService::with_global_instance(|agent_service| {
        agent_service.lock(passphrase);
        Ok(())
    })
}

#[op2]
fn save_agent_profile(#[serde] agent: Agent) -> Result<(), AnyhowWrapperError> {
    AgentService::with_mutable_global_instance(|agent_service| {
        agent_service.save_agent_profile(agent);

        Ok(())
    })
}

deno_core::extension!(
    agent_service,
    ops = [agent_did_document, agent_signing_key_id, agent_did, agent_create_signed_expression, agent_create_signed_expression_stringified, agent_create_signed_expression_for_user, agent_did_for_user, agent_list_user_emails, agent_get_user_did_by_email, agent_get_all_local_user_dids, agent_agent_for_user, agent_sign, agent_sign_string_hex, agent_is_initialized, agent_is_unlocked, agent, agent_load, agent_unlock, agent_lock, save_agent_profile],
    esm_entry_point = "ext:agent_service/agent_extension.js",
    esm = [dir "src/js_core", "agent_extension.js"]
);
