//! `identity.unlinkSelf`: this executor's device key leaves the person's identity, without the
//! master key, and is destroyed here from then on. An `unlink` or a revocation signed by another
//! device (the master key) ends the key the same way on this executor ([`end_sessions_of`]).
//!
//! A key's past links stay valid after an `unlink` (they are anchored below it), so any
//! surviving copy of its secret could still sign a link with an old anchor that validates. The
//! key therefore does not stay in the wallet: its secret is deleted, and only the public key
//! stays, under a retired name, so what the key signed still verifies and the key is still
//! listed. The device seed also derives the key's X25519 key (its encryption key), so the
//! deletion destroys that too: anything sealed only to the old `did:key` cannot be opened on
//! this executor afterwards. What happens, in order:
//!
//! 1. an **intent** is stored (`identity_unlinks`) and the key is **fenced**
//!    ([`crate::agent::fence`]): nothing signs with it any more, except the `unlink` itself;
//! 2. the `unlink` (signed with the agent's wallet key, built by the identity core) is
//!    published;
//! 3. the key is taken out of use:
//!
//! | Agent | What happens after the `unlink` is accepted |
//! |---|---|
//! | the main agent | `identity.rekey`'s move (`rekey::rekey_main`): the key is retired, a new `did:key` takes its place, the agent's apps are signed out; then the retired key's secret is deleted |
//! | a managed agent | its key is retired in the wallet (`AgentService::retire_managed_key`), its sessions and apps are signed out, and the retired key's secret is deleted; the user needs a new agent to be used again |
//!
//! 4. the intent is cleared and the fence lifted (the key holds no secret by then, in the
//!    wallet, in the keystore file or in the legacy backup).
//!
//! What a failure leaves:
//!
//! | The publish | The intent and the fence |
//! |---|---|
//! | is accepted, or the log already holds an `unlink` of the key | the cleanup runs, then both go |
//! | is refused for sure (the store answers 400, 403 or key-taken, or a check fails before anything is sent), the log read afresh shows no `unlink`, and this call made the intent | both go: the key works as before |
//! | is refused for sure, but an earlier call left the intent (its answer may have been lost, so it may have committed) | both stay |
//! | is refused for sure, but the log cannot be read afresh | both stay |
//! | fails in a way that does not say whether the store committed (transport, timeout, 429, 5xx, a failure after the append such as the cache) | both stay |
//!
//! A remote end (`end_dead_key`) while the wallet is **locked** cannot tell a live key from an
//! absent one, so it records the intent and fences the key; the unlock's `resume_pending` then
//! destroys the key (re-key or retire, sign-out, secret). A destroyed secret also leaves
//! nothing in the legacy keystore backup (`agent.json.legacy`): the deletion removes it, and
//! the intent stays when that fails.
//!
//! A retry of `identity.unlinkSelf` finishes the job: a key the log shows unlinked or revoked
//! but whose secret is still here is cleaned up (not refused as `key-dead`), and an intent
//! whose log does not show the `unlink` yet publishes again. The executor does the same at
//! startup and when the wallet unlocks ([`IdentityService::resume_pending`]). The cleanup runs
//! on its own task, so a request that goes away does not stop it, and [`end_sessions_of`]
//! leaves an agent with an intent alone: the cleanup signs it out itself.

use ad4m_identity_core::build::unlink_self;

use super::cache::KelCache;
use super::device::{device_key, WalletSigner};
use super::service::{db, error, mirror_head, now, IdentityService};
use super::types::{PublishParams, SignedHead};
use super::verdict::{key_verdict, KeyVerdict, VerdictChange};
use crate::agent::grants::Grants;
use crate::agent::{AgentContext, AgentService};
use crate::api::ws_handler::WsRpcError;
use crate::db_backend::db_backend;

/// One unlink at a time: a second call waits and then finds the key already dead.
static UNLINK: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

/// One cleanup of a dead key at a time, whoever asks (an `unlinkSelf`, a verdict, a sweep).
static CLEANUP: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

/// The agent of this executor whose device key is `did`: the main agent while its key is
/// `did`, else a managed agent.
fn agent_for(did: &str) -> AgentContext {
    match crate::agent::did_for_context(&AgentContext::main_agent()) {
        Ok(main) if main == did => AgentContext::main_agent(),
        _ => AgentContext::managed(did.to_string()),
    }
}

/// Whether this executor's wallet still holds the secret of `agent`'s device key.
fn key_is_live(agent: &AgentContext) -> bool {
    agent.wallet_key_name().is_ok_and(|name| {
        crate::wallet::wallet_backend()
            .get_secret_key(&name)
            .is_some()
    })
}

/// Stores the intent to unlink `did` from `scid` and fences the key. Idempotent.
fn begin(did: &str, scid: &str) -> Result<(), WsRpcError> {
    db(|db| db.set_unlink_intent(did, scid, now()))?;
    crate::agent::fence::raise(did);
    Ok(())
}

/// Forgets the intent of `did` and lifts its fence. A database failure keeps the fence: the
/// intent is still there, and the next resume finishes it (a cleanup with nothing left to do).
fn end(did: &str) {
    match db_backend().clear_unlink_intent(did) {
        Ok(()) => crate::agent::fence::lift(did),
        Err(e) => log::error!("identity: could not clear the unlink intent of {did}: {e}"),
    }
}

impl IdentityService {
    /// `identity.unlinkSelf`: publishes an `unlink` of `agent`'s device key signed by that key,
    /// then destroys the key on this executor (the table above). Answers the head after the
    /// `unlink`. Refused (409) for an agent without an identity (`no-identity`), a key the log
    /// does not list as a device key (`not-listed`), and a key that is dead (unlinked or
    /// revoked) and already gone from the wallet (`key-dead`). A key the log shows dead whose
    /// secret is still in the wallet is cleaned up, so the call can be repeated after any
    /// failure.
    pub async fn unlink_self(&self, agent: &AgentContext) -> Result<SignedHead, WsRpcError> {
        let _one_at_a_time = UNLINK.lock().await;
        let (agent_did, key) = device_key(agent).map_err(WsRpcError::internal)?;
        let intent = db(|db| db.unlink_intent(&agent_did))?;
        let scid =
            match db(|db| db.agent_identity(&agent_did))?.or_else(|| intent.clone()) {
                Some(scid) => scid,
                None => return Err(error(
                    409,
                    "no-identity: this agent speaks for no identity, so there is nothing to unlink",
                )),
            };
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
            // The log shows the key dead: what is left is the cleanup here.
            if intent.is_none() && !key_is_live(agent) {
                return Err(error(
                    409,
                    "key-dead: this agent's device key is unlinked or revoked already",
                ));
            }
            return self.clean_up_dead_key(agent, &agent_did, &scid).await;
        }
        if !agent.is_main_agent() && !crate::wallet::wallet_backend().can_rename_keys() {
            return Err(error(
                409,
                "cannot-retire-key: this wallet cannot take the key out of use",
            ));
        }
        self.refuse_without_store_keys()?;
        // Signed before the fence goes up; the signer is the one signer the fence lets through.
        let signer = WalletSigner::for_unlink(agent).map_err(WsRpcError::internal)?;
        let event = unlink_self(&chain, &record.id, &signer)
            .map_err(|e| WsRpcError::bad_request(format!("invalid: {e}")))?;
        signer.check().map_err(WsRpcError::internal)?;

        // The intent and the fence come before the append: from here on a copy of the event may
        // be in the store, and nothing may sign with the key beyond it.
        // Only an intent this attempt makes is this attempt's to clear: an earlier attempt that
        // left one may have committed.
        let made_here = intent.is_none();
        begin(&agent_did, &scid)?;
        let published = self
            .publish_classified(
                agent,
                &PublishParams {
                    events: vec![serde_json::to_value(&event)?],
                    pow: None,
                },
            )
            .await;
        match published {
            Ok(head) => {
                finish_local(agent.clone(), agent_did.clone())
                    .await
                    .map_err(|e| still_in_wallet(head.seq, &e))?;
                Ok(head)
            }
            Err(failure) if failure.refused => {
                let e = failure.error;
                // A refusal can also be the store saying the log moved on, past an `unlink` that
                // is already in it, so the log is read afresh (not from the cache: a stale
                // cache cannot show an `unlink` the store holds).
                let Ok(chain) = self.cache.refresh(self.backend.as_ref(), &scid).await else {
                    // The read failed: whether an `unlink` is in the log is not known.
                    return Err(e);
                };
                let listed_dead = chain
                    .state()
                    .device_key_id_of(&key)
                    .and_then(|id| chain.state().keys.get(id))
                    .is_some_and(|r| r.unlinked.is_some() || r.revoked.is_some());
                if listed_dead {
                    return self.clean_up_dead_key(agent, &agent_did, &scid).await;
                }
                // The log shows no `unlink`, and the store said no. An intent an earlier attempt
                // left stays (that attempt's answer may be lost: it may have committed).
                if made_here {
                    end(&agent_did);
                }
                Err(e)
            }
            // Maybe committed: the intent and the fence stay, and a retry finishes.
            Err(failure) => Err(failure.error),
        }
    }

    /// The key of `agent` is unlinked or revoked in the log of `scid`: takes it out of use here,
    /// and answers the head the cache accepted.
    async fn clean_up_dead_key(
        &self,
        agent: &AgentContext,
        did: &str,
        scid: &str,
    ) -> Result<SignedHead, WsRpcError> {
        begin(did, scid)?;
        let head = self
            .cache
            .accepted_head(scid)
            .map_err(super::service::cache_error)?;
        let seq = head.as_ref().map_or(0, |h| h.seq);
        finish_local(agent.clone(), did.to_string())
            .await
            .map_err(|e| still_in_wallet(seq, &e))?;
        let head = head.ok_or_else(|| {
            WsRpcError::internal("the key is gone from the wallet, but the cache holds no head")
        })?;
        mirror_head(&head)
    }

    /// Finishes the unlinks the database holds an intent for: each one as a repeated
    /// `identity.unlinkSelf` would (the log is read again; the cleanup runs when it shows the
    /// key dead, the `unlink` is published again when it does not). Called at startup and
    /// when the wallet unlocks; a failure (a locked wallet, a store outage) is logged and the
    /// intent stays for the next call.
    pub async fn resume_pending(&self) {
        let intents = match db_backend().unlink_intents() {
            Ok(intents) => intents,
            Err(e) => {
                log::warn!("identity: could not read the pending unlinks: {e}");
                return;
            }
        };
        for (did, _scid) in intents {
            let agent = agent_for(&did);
            // An end recorded while the wallet was locked, or one whose key is gone already (a
            // re-keyed or retired key): nothing is left to publish, the cleanup is what remains.
            if crate::wallet::wallet_backend().is_unlocked() && !key_is_live(&agent) {
                match finish_local(agent, did.clone()).await {
                    Ok(()) => log::info!("identity: finished the pending end of {did}"),
                    Err(e) => log::warn!("identity: the pending end of {did} is still open: {e}"),
                }
                continue;
            }
            match self.unlink_self(&agent).await {
                Ok(head) => log::info!(
                    "identity: finished the pending unlink of {did} (seq {})",
                    head.seq
                ),
                Err(e) => log::warn!(
                    "identity: the pending unlink of {did} is still open: {} ({})",
                    e.message,
                    e.code
                ),
            }
        }
    }
}

fn still_in_wallet(seq: u64, cause: &str) -> WsRpcError {
    WsRpcError::internal(format!(
        "the key is unlinked (seq {seq}), but it is still in this executor's wallet: {cause}"
    ))
}

/// Takes the dead device key `did` of `agent` out of use, signs the agent out, deletes the
/// secret and clears the intent. Runs on its own task, so a caller that goes away does not
/// stop it half done, and only one runs at a time. Every step is repeatable: a key that is
/// not in the wallet any more is not retired again.
pub(crate) async fn finish_local(agent: AgentContext, did: String) -> Result<(), String> {
    tokio::spawn(async move {
        let _one_at_a_time = CLEANUP.lock().await;
        if key_is_live(&agent) {
            if agent.is_main_agent() {
                super::rekey::rekey_main().await.map_err(|e| e.message)?;
            } else {
                let retiring = did.clone();
                AgentService::with_global_instance(|svc| svc.retire_managed_key(&retiring))
                    .map_err(|e| e.to_string())?;
            }
        }
        // The managed agent's grants (the main agent's ended with the re-key, but a grant
        // minted since is ended too).
        Grants::global()
            .revoke_agent(&agent)
            .map_err(|e| format!("{e:?}"))?;
        let destroying = did.clone();
        AgentService::with_global_instance(|svc| svc.destroy_retired_key(&destroying))
            .map_err(|e| e.to_string())?;
        end(&did);
        Ok(())
    })
    .await
    .map_err(|e| format!("the cleanup stopped: {e}"))?
}

/// A key a log unlinked or revoked that is the device key of an agent here ends that agent's
/// sessions and apps and is destroyed here ([`end_dead_key`]), on its own task. Any other change
/// (a key that is not an agent of this executor, one the agent is not bound to the log of, a
/// verdict that does not end the key) does nothing.
pub(crate) fn end_sessions_of(change: &VerdictChange) {
    if !matches!(change.verdict, KeyVerdict::Unlinked | KeyVerdict::Revoked) {
        return;
    }
    let change = change.clone();
    tokio::spawn(async move { end_dead_key(&change).await });
}

/// The work of [`end_sessions_of`] for one change.
pub(crate) async fn end_dead_key(change: &VerdictChange) {
    if !matches!(change.verdict, KeyVerdict::Unlinked | KeyVerdict::Revoked) {
        return;
    }
    let did = format!("did:key:{}", change.key);
    if db_backend().agent_identity(&did).ok().flatten().as_deref() != Some(change.scid.as_str()) {
        return;
    }
    // An unlink of this agent's own: its cleanup signs the agent out, and runs on its own task.
    if db_backend().unlink_intent(&did).ok().flatten().is_some() {
        return;
    }
    let agent = agent_for(&did);
    match Grants::global().revoke_agent(&agent) {
        Ok(0) => {}
        Ok(n) => log::warn!(
            "identity: the key of {did} is {:?} in its log: {n} grant(s) ended",
            change.verdict
        ),
        Err(e) => log::error!("identity: could not end the grants of {did}: {e:?}"),
    }
    if !crate::wallet::wallet_backend().is_unlocked() {
        // A locked wallet shows no keys, so "not live" says nothing now. The end is recorded
        // (the intent) and the key fenced; the unlock's `resume_pending` destroys it.
        log::warn!(
            "identity: the key of {did} is {:?} in its log, and the wallet is locked: it is \
             fenced, and removed at the next unlock",
            change.verdict
        );
        if let Err(e) = begin(&did, &change.scid) {
            log::error!(
                "identity: could not record the end of the key of {did}: {}",
                e.message
            );
        }
        return;
    }
    if !key_is_live(&agent) {
        return;
    }
    log::warn!(
        "identity: the key of {did} is {:?} in its log: it is removed from this executor's wallet",
        change.verdict
    );
    if let Err(e) = begin(&did, &change.scid) {
        log::error!(
            "identity: could not record the end of the key of {did}: {}",
            e.message
        );
        return;
    }
    if let Err(e) = finish_local(agent, did.clone()).await {
        // The intent stays: the next unlock or start finishes it.
        log::error!("identity: the key of {did} is dead but still in the wallet: {e}");
    }
}

/// Checks every local agent against its own trusted log and ends (and destroys) the keys that
/// are unlinked or revoked there. Used where a change may have been missed: when the verdict
/// channel lagged, and at start.
pub(crate) async fn sweep_dead_keys(cache: &KelCache) {
    let backend = db_backend();
    let scids = match backend.local_identities() {
        Ok(scids) => scids,
        Err(e) => {
            log::warn!("identity: could not read the local identities for the sweep: {e}");
            return;
        }
    };
    for scid in scids {
        let agents = match backend.agents_of_identity(&scid) {
            Ok(agents) => agents,
            Err(e) => {
                log::warn!("identity: could not read the agents of {scid}: {e}");
                continue;
            }
        };
        let (chain, trusted) = match cache.trusted(&scid).await {
            Ok(Some(trusted)) => trusted,
            Ok(None) => continue,
            Err(e) => {
                log::warn!("identity: could not read the log of {scid} for the sweep: {e}");
                continue;
            }
        };
        for agent_did in agents {
            let Some(key) = agent_did.strip_prefix("did:key:") else {
                continue;
            };
            let verdict = key_verdict(chain.state(), trusted, key, now());
            if matches!(verdict, KeyVerdict::Unlinked | KeyVerdict::Revoked) {
                end_dead_key(&VerdictChange {
                    key: key.to_string(),
                    scid: scid.clone(),
                    verdict,
                })
                .await;
            }
        }
    }
}
