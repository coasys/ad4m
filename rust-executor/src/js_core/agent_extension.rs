use super::utils::sort_json_value;
use crate::js_core::error::AnyhowWrapperError;
use crate::{
    agent::{
        create_signed_expression, did, did_document, did_for_context,
        sealed_box::{open_sealed_for_context, SealedBox},
        sign_for_context, sign_string_hex_for_context, signing_key_id_for_context, AgentContext,
        AgentService,
    },
    types::Agent,
};
use deno_core::anyhow;
use deno_core::op2;
use std::cell::RefCell;
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// The agent a call acts as
// ---------------------------------------------------------------------------
//
// Calls into one language overlap (reads run beside other calls), so the agent
// a call acts as travels with the call, not with the thread. The runtime
// registers it under the call's id (`register_call_agent`) and runs the script
// inside `__ad4mRunCall(id, ...)` (`agent_extension.js`), which takes it out
// once (`agent_call_token`) as a `CallAgent`, a cppgc object JS cannot forge,
// and keeps it in a V8 async-context variable. V8 carries that variable with
// every continuation the call creates (awaits, promise reactions, timers). The
// ops below act as the `CallAgent` JS hands them, and as no agent when there is
// none: they refuse. Authority comes only from a token. The host's own entries
// into a language (module evaluation, `init`, callback registration, teardown:
// `LanguageRuntime::execute_as_host`) and the calls it makes as the node (a
// Holochain signal, a sync) run with a main-agent token like any call, so code
// that clears its async context, or calls an op with no token, during a call
// for a managed user cannot sign or open sealed data as the main agent.
//
// Not enforced: a token is a JS value, so language code can keep one. Code that
// saves the async context (`getAsyncContext`) during Bob's call and restores it
// in a later call for Alice signs as Bob. Separating agents needs per-agent
// isolation (SEC-4); see `languages/AGENTS.md`.

/// The agent of one call into a language. Only `agent_call_token` makes one.
pub struct CallAgent(AgentContext);

// SAFETY: holds no garbage-collected references, so there is nothing to trace.
unsafe impl deno_core::GarbageCollected for CallAgent {
    fn trace(&self, _visitor: &mut deno_core::v8::cppgc::Visitor) {}

    fn get_name(&self) -> &'static std::ffi::CStr {
        c"CallAgent"
    }
}

thread_local! {
    /// Calls this language thread is starting, by request id: the agent each acts as.
    static CALL_AGENTS: RefCell<HashMap<u64, AgentContext>> = RefCell::new(HashMap::new());
}

/// Registers the agent call `id` acts as, for its script to take out with `agent_call_token`.
pub(crate) fn register_call_agent(id: u64, ctx: AgentContext) {
    CALL_AGENTS.with(|calls| calls.borrow_mut().insert(id, ctx));
}

/// Drops the agent of call `id` if its script never took it out (a script that did not
/// compile).
pub(crate) fn forget_call_agent(id: u64) {
    CALL_AGENTS.with(|calls| calls.borrow_mut().remove(&id));
}

/// The agent the ops act as: the call's. With no token the code acts as no agent, and the
/// ops refuse: falling back to the main agent would let code in a managed user's call sign as
/// the node by clearing its async context.
fn agent_of(call: Option<&CallAgent>) -> Result<AgentContext, anyhow::Error> {
    call.map(|call| call.0.clone()).ok_or_else(|| {
        anyhow::anyhow!(
            "No agent: this code runs outside any call into the language, so it acts as no agent"
        )
    })
}

/// Takes out the agent of call `id`, once. The call's script runs this before any other JS.
#[op2]
#[cppgc]
fn agent_call_token(#[number] id: u64) -> Result<CallAgent, AnyhowWrapperError> {
    CALL_AGENTS
        .with(|calls| calls.borrow_mut().remove(&id))
        .map(CallAgent)
        .ok_or_else(|| anyhow::anyhow!("No call {id} waits for its agent").into())
}

#[op2]
#[serde]
fn agent_did_document() -> Result<did_key::Document, AnyhowWrapperError> {
    Ok(did_document())
}

#[op2]
#[string]
fn agent_signing_key_id(#[cppgc] call: Option<&CallAgent>) -> Result<String, AnyhowWrapperError> {
    signing_key_id_for_context(&agent_of(call)?).map_err(AnyhowWrapperError::from)
}

#[op2]
#[string]
fn agent_did(#[cppgc] call: Option<&CallAgent>) -> Result<String, AnyhowWrapperError> {
    did_for_context(&agent_of(call)?).map_err(AnyhowWrapperError::from)
}

#[op2]
#[serde]
fn agent_create_signed_expression(
    #[serde] data: serde_json::Value,
    #[cppgc] call: Option<&CallAgent>,
) -> Result<serde_json::Value, AnyhowWrapperError> {
    let ctx = agent_of(call)?;
    let sorted_json = sort_json_value(&data);
    let signed_expression =
        create_signed_expression(sorted_json, &ctx).map_err(AnyhowWrapperError::from)?;
    serde_json::to_value(signed_expression).map_err(AnyhowWrapperError::from)
}

#[op2]
#[string]
fn agent_create_signed_expression_stringified(
    #[string] data: String,
    #[cppgc] call: Option<&CallAgent>,
) -> Result<String, AnyhowWrapperError> {
    let ctx = agent_of(call)?;
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
    #[cppgc] call: Option<&CallAgent>,
) -> Result<serde_json::Value, AnyhowWrapperError> {
    create_signed_expression_for_user(&user_email, &data, &agent_of(call)?)
        .map_err(AnyhowWrapperError::from)
}

/// Signs `data` as `user_email`, but only while the language runs for that user (`ctx`, the
/// call's agent). Language code comes from third parties: without this check, any language on
/// a multi-user executor signs anything as any managed user. The host contract names users by
/// email; the email resolves to the user's agent (its DID) here, once, and the check compares
/// DIDs.
fn create_signed_expression_for_user(
    user_email: &str,
    data: &serde_json::Value,
    ctx: &AgentContext,
) -> Result<serde_json::Value, anyhow::Error> {
    let named = AgentService::did_for_email(user_email).ok();
    if named.is_none() || ctx.managed_did() != named.as_deref() {
        return Err(anyhow::anyhow!(
            "A language signs only as the user it runs for"
        ));
    }
    let signed_expression = create_signed_expression(sort_json_value(data), ctx)?;
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
    let users = crate::db_backend::db_backend()
        .list_users()
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
fn agent_sign(
    #[buffer] payload: &[u8],
    #[cppgc] call: Option<&CallAgent>,
) -> Result<Vec<u8>, AnyhowWrapperError> {
    sign_for_context(payload, &agent_of(call)?).map_err(AnyhowWrapperError::from)
}

#[op2]
#[string]
fn agent_sign_string_hex(
    #[string] payload: String,
    #[cppgc] call: Option<&CallAgent>,
) -> Result<String, AnyhowWrapperError> {
    sign_string_hex_for_context(payload, &agent_of(call)?).map_err(AnyhowWrapperError::from)
}

/// Opens a box sealed to the agent the call runs for, and returns the plaintext as hex.
/// Fails for a box sealed to any other DID.
#[op2]
#[string]
fn agent_open_sealed(
    #[serde] sealed: SealedBox,
    #[cppgc] call: Option<&CallAgent>,
) -> Result<String, AnyhowWrapperError> {
    open_sealed_for_context(&sealed, &agent_of(call)?)
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
// and overwriting the main agent's profile stay out: any language could otherwise probe the
// passphrase, lock the executor under a passphrase of its own, or rewrite the main agent's
// profile.
deno_core::extension!(
    agent_service,
    ops = [agent_call_token, agent_did_document, agent_signing_key_id, agent_did, agent_create_signed_expression, agent_create_signed_expression_stringified, agent_create_signed_expression_for_user, agent_did_for_user, agent_list_user_emails, agent_get_all_local_user_dids, agent_agent_for_user, agent_sign, agent_sign_string_hex, agent_open_sealed, agent_is_initialized, agent_is_unlocked, agent],
    esm_entry_point = "ext:agent_service/agent_extension.js",
    esm = [dir "src/js_core", "agent_extension.js"]
);

#[cfg(test)]
mod tests {
    use super::create_signed_expression_for_user;
    use crate::agent::AgentContext;
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

        let as_alice = AgentContext::managed(alice_did.clone());
        let own = create_signed_expression_for_user(&alice, &data, &as_alice);
        let other = create_signed_expression_for_user(&bob, &data, &as_alice);
        let nobody = create_signed_expression_for_user("nobody@example.org", &data, &as_alice);
        let from_the_main_agent =
            create_signed_expression_for_user(&alice, &data, &AgentContext::main_agent());

        let own = own.expect("a language signs as the user it runs for");
        assert_eq!(own["author"], json!(alice_did));
        assert!(other.is_err(), "a language signed as another user");
        assert!(nobody.is_err(), "a language signed as an email no user has");
        assert!(
            from_the_main_agent.is_err(),
            "a language that runs for the main agent signed as a user"
        );
    }
}
