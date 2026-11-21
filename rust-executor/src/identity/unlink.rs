//! `identity.unlinkSelf`: this executor's device key leaves the person's identity, without the
//! master key, and is dead here from then on. An `unlink` signed by another device (the master
//! key) ends the key the same way on this executor: [`end_sessions_of`] signs the agent out.
//!
//! A dead key signs nothing new, and a copy of an unlinked key could still sign a link with an
//! old anchor, so the key does not stay in use. [`IdentityService::unlink_self`] publishes the
//! `unlink` (signed with the agent's wallet key, built by the identity core) and then
//! takes the key out of use:
//!
//! | Agent | What happens after the `unlink` is accepted |
//! |---|---|
//! | the main agent | `identity.rekey`'s move (`rekey::rekey_main`): the key is retired, a new `did:key` takes its place, the agent's apps are signed out |
//! | a managed agent | its key is retired in the wallet (`AgentService::retire_managed_key`), its sessions and apps are signed out; the user needs a new agent to be used again |
//!
//! When that last step fails the `unlink` stands (the log is the truth): the call answers the
//! error, `identity.status` shows `unlinked`, and the operator finishes with `identity.rekey`.

use ad4m_identity_core::build::unlink_self;

use super::device::{device_key, WalletSigner};
use super::service::{db, error, IdentityService};
use super::types::{PublishParams, SignedHead};
use super::verdict::{KeyVerdict, VerdictChange};
use crate::agent::grants::Grants;
use crate::agent::{AgentContext, AgentService};
use crate::api::ws_handler::WsRpcError;
use crate::db_backend::db_backend;

/// One unlink at a time: a second call waits and then finds the key already dead.
static UNLINK: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

impl IdentityService {
    /// `identity.unlinkSelf`: publishes an `unlink` of `agent`'s device key signed by that key,
    /// then kills the key on this executor (the table above). Answers the head after the
    /// `unlink`. Refused (409) for an agent without an identity (`no-identity`), a key the log
    /// does not list as a device key (`not-listed`), and a key that is already unlinked or
    /// revoked (`key-dead`).
    pub async fn unlink_self(&self, agent: &AgentContext) -> Result<SignedHead, WsRpcError> {
        let _one_at_a_time = UNLINK.lock().await;
        let (agent_did, key) = device_key(agent).map_err(WsRpcError::internal)?;
        let scid = db(|db| db.agent_identity(&agent_did))?.ok_or_else(|| {
            error(
                409,
                "no-identity: this agent speaks for no identity, so there is nothing to unlink",
            )
        })?;
        let chain = self
            .refreshed_or_cached(&scid)
            .await?
            .ok_or_else(|| WsRpcError::not_found("not-found: no log of this identity"))?;
        let record = chain
            .state()
            .device_key_id_of(&key)
            .and_then(|id| chain.state().keys.get(id))
            .ok_or_else(|| error(409, "not-listed: the log lists no device key of this agent"))?;
        if record.revoked.is_some() || record.unlinked.is_some() {
            return Err(error(
                409,
                "key-dead: this agent's device key is unlinked or revoked already",
            ));
        }
        if !agent.is_main_agent() && !crate::wallet::wallet_backend().can_rename_keys() {
            return Err(error(
                409,
                "cannot-retire-key: this wallet cannot take the key out of use",
            ));
        }
        let signer = WalletSigner::new(agent).map_err(WsRpcError::internal)?;
        let event = unlink_self(&chain, &record.id, &signer)
            .map_err(|e| WsRpcError::bad_request(format!("invalid: {e}")))?;
        signer.check().map_err(WsRpcError::internal)?;
        let head = self
            .publish(
                agent,
                &PublishParams {
                    events: vec![serde_json::to_value(&event)?],
                },
            )
            .await?;
        kill_key(agent, &agent_did).await.map_err(|e| {
            WsRpcError::internal(format!(
                "the key is unlinked (seq {}), but it is still in this executor's wallet: {e}",
                head.seq
            ))
        })?;
        Ok(head)
    }
}

/// Takes the dead device key of `agent` out of use and signs the agent out.
async fn kill_key(agent: &AgentContext, agent_did: &str) -> Result<(), String> {
    if agent.is_main_agent() {
        return super::rekey::rekey_main()
            .await
            .map(|_| ())
            .map_err(|e| e.message);
    }
    let did = agent_did.to_string();
    AgentService::with_global_instance(|svc| svc.retire_managed_key(&did))
        .map_err(|e| e.to_string())?;
    Grants::global()
        .revoke_agent(agent)
        .map(|_| ())
        .map_err(|e| format!("{e:?}"))
}

/// A key a log unlinked or revoked that is the device key of an agent here ends that agent's
/// sessions and apps: the device is signed out. Its key stays in the wallet until
/// `identity.rekey`. Any other change (a key that is not an agent of this executor, one the
/// agent is not bound to the log of, a verdict that does not end the key) does nothing.
pub(crate) fn end_sessions_of(change: &VerdictChange) {
    if !matches!(change.verdict, KeyVerdict::Unlinked | KeyVerdict::Revoked) {
        return;
    }
    let did = format!("did:key:{}", change.key);
    if db_backend().agent_identity(&did).ok().flatten().as_deref() != Some(change.scid.as_str()) {
        return;
    }
    let agent = match crate::agent::did_for_context(&AgentContext::main_agent()) {
        Ok(main) if main == did => AgentContext::main_agent(),
        _ => AgentContext::managed(did.clone()),
    };
    match Grants::global().revoke_agent(&agent) {
        Ok(0) => {}
        Ok(n) => log::warn!(
            "identity: the key of {did} is {:?} in its log: {n} grant(s) ended",
            change.verdict
        ),
        Err(e) => log::error!("identity: could not end the grants of {did}: {e:?}"),
    }
}
