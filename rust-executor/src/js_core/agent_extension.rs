use super::utils::sort_json_value;
use crate::js_core::error::AnyhowWrapperError;
use crate::languages::language_runtime::get_runtime_agent_context;
use crate::{
    agent::{
        create_signed_expression, did, did_document, did_for_context,
        sealed_box::{open_sealed_for_context, SealedBox},
        sign_for_context, sign_string_hex_for_context, signing_key_id_for_context, AgentService,
    },
    types::Agent,
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
/// signs anything as any managed user. The host contract names users by email; the email
/// resolves to the user's agent here.
fn create_signed_expression_for_user(
    user_email: &str,
    data: &serde_json::Value,
) -> Result<serde_json::Value, anyhow::Error> {
    let ctx = get_runtime_agent_context();
    let named = AgentService::did_for_email(user_email).ok();
    if named.is_none() || ctx.managed_did() != named.as_deref() {
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
    let context =
        AgentService::managed_agent_for_email(&user_email).map_err(AnyhowWrapperError::from)?;
    did_for_context(&context).map_err(AnyhowWrapperError::from)
}

#[op2]
#[serde]
fn agent_list_user_emails() -> Result<Vec<String>, AnyhowWrapperError> {
    let users = crate::db::Ad4mDb::with_global_instance(|db| db.list_users())
        .map_err(AnyhowWrapperError::from)?;
    Ok(users
        .into_iter()
        .filter(|user| !user.did.is_empty())
        .map(|user| user.username)
        .collect())
}

#[op2]
#[serde]
fn agent_get_all_local_user_dids() -> Result<Vec<String>, AnyhowWrapperError> {
    let mut dids = Vec::new();

    // Add main agent DID
    dids.push(did());

    // Add all managed agents' DIDs
    dids.extend(AgentService::managed_agent_dids().map_err(AnyhowWrapperError::from)?);

    Ok(dids)
}

#[op2]
#[serde]
fn agent_agent_for_user(
    #[string] user_email: String,
) -> Result<serde_json::Value, AnyhowWrapperError> {
    let agent = AgentService::did_for_email(&user_email)
        .and_then(|did| AgentService::load_managed_agent_profile(&did))
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

/// Opens a box sealed to the agent the language runs for, and returns the plaintext as
/// hex. Fails for a box sealed to any other DID.
#[op2]
#[string]
fn agent_open_sealed(#[serde] sealed: SealedBox) -> Result<String, AnyhowWrapperError> {
    open_sealed_for_context(&sealed, &get_runtime_agent_context())
        .map(hex::encode)
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

// Languages sign and read through these ops. Unlocking, locking, reloading the keystore
// and overwriting the node's profile stay out: any language could otherwise probe the
// passphrase, lock the node under a passphrase of its own, or rewrite the node's profile.
deno_core::extension!(
    agent_service,
    ops = [agent_did_document, agent_signing_key_id, agent_did, agent_create_signed_expression, agent_create_signed_expression_stringified, agent_create_signed_expression_for_user, agent_did_for_user, agent_list_user_emails, agent_get_all_local_user_dids, agent_agent_for_user, agent_sign, agent_sign_string_hex, agent_open_sealed, agent_is_initialized, agent_is_unlocked, agent],
    esm_entry_point = "ext:agent_service/agent_extension.js",
    esm = [dir "src/js_core", "agent_extension.js"]
);

#[cfg(test)]
mod tests {
    use super::create_signed_expression_for_user;
    use crate::agent::AgentContext;
    use crate::languages::language_runtime::set_runtime_agent_context;
    use serde_json::json;

    #[test]
    fn a_language_signs_only_as_the_user_it_runs_for() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let alice = format!("alice.{}@example.org", uuid::Uuid::new_v4());
        let bob = format!("bob.{}@example.org", uuid::Uuid::new_v4());
        let alice_did = crate::test_utils::create_test_user(&alice);
        crate::test_utils::create_test_user(&bob);
        let data = json!({ "claim": "x" });

        set_runtime_agent_context(&AgentContext::managed(alice_did.clone()));
        let own = create_signed_expression_for_user(&alice, &data);
        let other = create_signed_expression_for_user(&bob, &data);
        let nobody = create_signed_expression_for_user("nobody@example.org", &data);
        set_runtime_agent_context(&AgentContext::main_agent());
        let from_the_node = create_signed_expression_for_user(&alice, &data);

        let own = own.expect("a language signs as the user it runs for");
        assert_eq!(own["author"], json!(alice_did));
        assert!(other.is_err(), "a language signed as another user");
        assert!(nobody.is_err(), "a language signed as an email no user has");
        assert!(
            from_the_node.is_err(),
            "a language that runs for the node signed as a user"
        );
    }
}
