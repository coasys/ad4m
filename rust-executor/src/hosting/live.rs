//! [`LiveEnv`]: the host's side effects on the real executor (wallet, users table, person
//! store, identity service, grants).

use async_trait::async_trait;

use super::{ClaimFacts, HostEnv, HostEvent, HostRefusal, KeyStanding, LeaveResult, LogFacts};
use crate::agent::{AgentContext, AgentService};
use crate::at_rest::data_keys::DestroyReason;
use crate::identity::cache::CacheError;
use crate::identity::types::OfferKind;

/// The executor's real [`HostEnv`].
pub struct LiveEnv;

fn save_wallet() {
    AgentService::with_global_instance(|s| {
        if let Some(p) = &s.passphrase {
            s.save(p.clone());
        }
    });
}

/// A password hash nobody can match: a tenant logs in with its key, never a password.
fn unusable_password_hash() -> String {
    let mut bytes = [0u8; 32];
    rand::RngCore::fill_bytes(&mut rand::thread_rng(), &mut bytes);
    format!("!{}", bs58::encode(bytes).into_string())
}

fn db_error(e: impl std::fmt::Display) -> String {
    e.to_string()
}

#[async_trait]
impl HostEnv for LiveEnv {
    fn now(&self) -> u64 {
        chrono::Utc::now().timestamp().max(0) as u64
    }

    fn multi_user(&self) -> bool {
        crate::user_management::is_multi_user_enabled()
    }

    async fn log_facts(&self, did_scid: &str) -> Result<LogFacts, HostRefusal> {
        let service = crate::identity::service().map_err(|_| HostRefusal::StoreUnreachable)?;
        match service
            .cache
            .refresh(service.backend.as_ref(), did_scid)
            .await
        {
            Ok(_) => {}
            Err(CacheError::NotFound) => return Err(HostRefusal::UnknownIdentity),
            Err(e) => {
                // A cached log answers a store that is down; none means no verdict.
                let cached = service
                    .cache
                    .chain(did_scid)
                    .await
                    .map_err(|_| HostRefusal::StoreUnreachable)?;
                if cached.is_none() {
                    log::info!("hosting: the log of {did_scid} cannot be read: {e}");
                    return Err(HostRefusal::StoreUnreachable);
                }
            }
        }
        Ok(LogFacts {
            vouched: service.cache.vouched(did_scid),
        })
    }

    async fn make_device(
        &self,
        tenant_id: &str,
        first_join: bool,
    ) -> Result<(String, String), String> {
        crate::user_management::check_executor_unlocked()?;
        if first_join {
            crate::at_rest::persons::create_for_user(tenant_id)?;
        }
        let did = AgentService::create_managed_agent_key(tenant_id)
            .map_err(|e| format!("Failed to create the tenant's key: {e}"))?;
        save_wallet();
        let name = AgentService::wallet_key_for_did(&did).map_err(db_error)?;
        let db = crate::db_backend::db_backend();
        if first_join {
            db.add_user_prehashed(tenant_id, &did, &unusable_password_hash())
                .map_err(db_error)?;
        } else {
            db.set_user_did(tenant_id, &did).map_err(db_error)?;
        }
        Ok((name, did))
    }

    async fn claim(&self, device_did: &str, offer: &str) -> Result<ClaimFacts, HostRefusal> {
        let service = crate::identity::service().map_err(|_| HostRefusal::StoreUnreachable)?;
        let offer =
            crate::identity::service::parse_offer(offer).map_err(|_| HostRefusal::OfferInvalid)?;
        let claimed = service
            .claims()
            .claim_for_tenant(device_did, &offer)
            .await
            .map_err(|e| match e.code {
                404 | 400 | 410 => HostRefusal::OfferInvalid,
                503 => HostRefusal::StoreUnreachable,
                _ => HostRefusal::Internal(e.message),
            })?;
        Ok(ClaimFacts {
            is_node: claimed.kind == OfferKind::Node,
            expires: claimed.expires,
        })
    }

    async fn key_standing(&self, did_scid: &str, device_did: &str) -> KeyStanding {
        let Ok(service) = crate::identity::service() else {
            return KeyStanding::Unknown;
        };
        let _ = service
            .cache
            .refresh(service.backend.as_ref(), did_scid)
            .await;
        let key = match crate::identity::device::multikey_of(device_did) {
            Ok(key) => key,
            Err(_) => return KeyStanding::Gone,
        };
        match service.cache.trusted(did_scid).await {
            Ok(Some((chain, head))) => {
                let state = chain.state();
                let Some(id) = state.device_key_id_of(&key).cloned() else {
                    return KeyStanding::Gone;
                };
                if state.bot_scope_at(&id, head).is_none()
                    && state.key_valid_at_time(&id, head, self.now())
                {
                    KeyStanding::Valid
                } else {
                    KeyStanding::Gone
                }
            }
            _ => KeyStanding::Unknown,
        }
    }

    fn bind_device(&self, device_did: &str, did_scid: &str, now: u64) -> Result<(), String> {
        crate::db_backend::db_backend()
            .set_agent_identity(device_did, did_scid, now)
            .map_err(db_error)
    }

    fn end_device(&self, device_did: &str, key_name: &str) -> Result<(), String> {
        crate::agent::grants::Grants::global()
            .revoke_agent(&AgentContext::managed(device_did.to_string()))
            .map_err(|e| e.to_string())?;
        crate::db_backend::db_backend()
            .unbind_agent_identity(device_did)
            .map_err(db_error)?;
        crate::wallet::wallet_backend()
            .destroy_key(key_name)
            .map_err(db_error)?;
        save_wallet();
        Ok(())
    }

    async fn undo_first_join(
        &self,
        tenant_id: &str,
        device: Option<(&str, &str)>,
    ) -> Result<(), String> {
        if let Some((did, name)) = device {
            self.end_device(did, name)?;
        }
        crate::at_rest::persons::leave(tenant_id, false, DestroyReason::PendingExpired)
            .await
            .map(drop)?;
        crate::db_backend::db_backend()
            .delete_user(tenant_id)
            .map_err(db_error)?;
        Ok(())
    }

    fn guest_device(&self, username: &str) -> Result<(String, String), String> {
        let user = crate::db_backend::db_backend()
            .get_user(username)
            .map_err(db_error)?;
        let name = AgentService::wallet_key_for_did(&user.did).map_err(db_error)?;
        Ok((name, user.did))
    }

    async fn leave_person(&self, tenant_id: &str, keep_data: bool) -> Result<LeaveResult, String> {
        let left =
            crate::at_rest::persons::leave(tenant_id, keep_data, DestroyReason::TenantDeleted)
                .await?;
        if !keep_data {
            crate::db_backend::db_backend()
                .delete_user(tenant_id)
                .map_err(db_error)?;
        }
        Ok(left.into())
    }

    fn emit(&self, event: HostEvent) {
        event.publish();
    }
}
