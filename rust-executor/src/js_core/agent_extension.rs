use super::utils::sort_json_value;
use crate::js_core::error::AnyhowWrapperError;
use crate::languages::language_runtime::get_runtime_agent_context;
use crate::{
    agent::{
        create_signed_expression, did, did_document, did_for_context, sign_for_context,
        sign_string_hex_for_context, signing_key_id_for_context, AgentContext, AgentService,
    },
    types::{Agent, AgentStatus},
};
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
    signing_key_id_for_context(&get_runtime_agent_context()).map_err(AnyhowWrapperError::from)
}

#[op2]
#[string]
fn agent_did() -> Result<String, AnyhowWrapperError> {
    did_for_context(&get_runtime_agent_context()).map_err(AnyhowWrapperError::from)
}

#[op2]
#[serde]
fn agent_create_signed_expression(
    #[serde] data: serde_json::Value,
) -> Result<serde_json::Value, AnyhowWrapperError> {
    let ctx = get_runtime_agent_context();
    let sorted_json = sort_json_value(&data);
    let signed_expression =
        create_signed_expression(sorted_json, &ctx).map_err(AnyhowWrapperError::from)?;
    serde_json::to_value(signed_expression).map_err(AnyhowWrapperError::from)
}

#[op2]
#[string]
fn agent_create_signed_expression_stringified(
    #[string] data: String,
) -> Result<String, AnyhowWrapperError> {
    let ctx = get_runtime_agent_context();
    let data: serde_json::Value = serde_json::from_str(&data)?;
    let sorted_json = sort_json_value(&data);
    let signed_expression = create_signed_expression(sorted_json, &ctx)?;
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
    create_signed_expression_for_user(&user_email, &data).map_err(AnyhowWrapperError::from)
}

/// Signs `data` as `user_email`, but only while the language runs for that user. Language
/// code comes from third parties: without this check, any language on a multi-user node
/// signs anything as any managed user.
fn create_signed_expression_for_user(
    user_email: &str,
    data: &serde_json::Value,
) -> Result<serde_json::Value, anyhow::Error> {
    let ctx = get_runtime_agent_context();
    if ctx.user_email.as_deref() != Some(user_email) {
        return Err(anyhow::anyhow!(
            "A language signs only as the user it runs for"
        ));
    }
    let signed_expression = create_signed_expression(sort_json_value(data), &ctx)?;
    Ok(serde_json::to_value(signed_expression)?)
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
#[serde]
fn agent_get_all_local_user_dids() -> Result<Vec<String>, AnyhowWrapperError> {
    let mut dids = Vec::new();

    // Add main agent DID
    dids.push(did());

    // Add all managed user DIDs
    let user_emails = AgentService::list_user_emails().map_err(AnyhowWrapperError::from)?;
    for email in user_emails {
        let context = AgentContext::for_user_email(email);
        let user_did = did_for_context(&context).map_err(AnyhowWrapperError::from)?;
        dids.push(user_did);
    }

    Ok(dids)
}

#[op2]
#[serde]
fn agent_agent_for_user(
    #[string] user_email: String,
) -> Result<serde_json::Value, AnyhowWrapperError> {
    let agent = AgentService::with_global_instance(|agent_service| {
        agent_service.load_user_agent_profile(&user_email)
    })
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
    sign_for_context(payload, &get_runtime_agent_context()).map_err(AnyhowWrapperError::from)
}

#[op2]
#[string]
fn agent_sign_string_hex(#[string] payload: String) -> Result<String, AnyhowWrapperError> {
    sign_string_hex_for_context(payload, &get_runtime_agent_context())
        .map_err(AnyhowWrapperError::from)
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

#[op2(async(lazy), fast)]
#[serde]
async fn agent_unlock(#[string] passphrase: String) -> Result<(), AnyhowWrapperError> {
    AgentService::with_mutable_global_instance(|agent_service| agent_service.unlock(passphrase))
        .map_err(AnyhowWrapperError::from)
}

#[op2(async(lazy), fast)]
#[serde]
async fn agent_lock(#[string] passphrase: String) -> Result<(), AnyhowWrapperError> {
    AgentService::with_mutable_global_instance(|agent_service| {
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
    ops = [agent_did_document, agent_signing_key_id, agent_did, agent_create_signed_expression, agent_create_signed_expression_stringified, agent_create_signed_expression_for_user, agent_did_for_user, agent_list_user_emails, agent_get_all_local_user_dids, agent_agent_for_user, agent_sign, agent_sign_string_hex, agent_is_initialized, agent_is_unlocked, agent, agent_load, agent_unlock, agent_lock, save_agent_profile],
    esm_entry_point = "ext:agent_service/agent_extension.js",
    esm = [dir "src/js_core", "agent_extension.js"]
);

#[cfg(test)]
mod tests {
    use super::create_signed_expression_for_user;
    use crate::agent::{AgentContext, AgentService};
    use crate::languages::language_runtime::set_runtime_agent_context;
    use serde_json::json;

    #[test]
    fn a_language_signs_only_as_the_user_it_runs_for() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let alice = format!("alice.{}@example.org", uuid::Uuid::new_v4());
        let bob = format!("bob.{}@example.org", uuid::Uuid::new_v4());
        AgentService::ensure_user_key_exists(&alice).unwrap();
        AgentService::ensure_user_key_exists(&bob).unwrap();
        let data = json!({ "claim": "x" });

        set_runtime_agent_context(&AgentContext::for_user_email(alice.clone()));
        let own = create_signed_expression_for_user(&alice, &data);
        let other = create_signed_expression_for_user(&bob, &data);
        set_runtime_agent_context(&AgentContext::main_agent());
        let from_the_node = create_signed_expression_for_user(&alice, &data);

        let own = own.expect("a language signs as the user it runs for");
        assert_eq!(
            own["author"],
            json!(AgentService::get_user_did_by_email(&alice).unwrap())
        );
        assert!(other.is_err(), "a language signed as another user");
        assert!(
            from_the_node.is_err(),
            "a language that runs for the node signed as a user"
        );
    }
}
