//! The private details record and the request box, through the agent language (spec section
//! 17). The executor never decrypts a record: an app does. An app on this machine whose device
//! is this executor (the desktop's custody service) has no `enc` key of its own, so the operator
//! may have the executor unwrap the record key of its entry (`identity.unwrapRecordKey`), in
//! the wallet, and decrypts the record with it. A read, a
//! record write and a deletion pass the request the caller signed with the identity's master
//! key (the custody service, a restoring app), or, when the caller gives none, sign one with
//! this agent's device key at its [`DevicePosition`]. A post is the device key's only: the
//! store refuses a master key there.
//!
//! [`DevicePosition`] is also what the inbox pull and the person's profile sign with: the
//! agent's device key in its identity's log at the trusted seq, and the store-signed head there.

use ad4m_identity_core::did_of;
use ad4m_identity_core::Multikey;
use ad4m_ldk::identity::{PostedRequest, SignedHead};
use serde::Serialize;

use super::device::{body_bytes, device_key, store_request, WalletSigner, REQUEST_TTL_S};
use super::service::{backend_error, cache_error, db, now, IdentityService};
use super::types::{
    convert, DeleteRequestParams, DeviceRequest, PostRequestParams, RecordKey, RecordParams,
    RecordPutParams, RecordStored, RequestPosted, RequestsParams, SealedRecord, SignedRequest,
    StoredRecord, UnwrapRecordKeyParams,
};
use crate::agent::AgentContext;
use crate::api::ws_handler::WsRpcError;

/// The longest a request of the box lives (`ad4m_identity_core::requests::REQUEST_TTL_S`).
const REQUEST_BOX_TTL_S: u64 = ad4m_identity_core::requests::REQUEST_TTL_S;

/// Where an agent's device key stands in its identity's log: what a store request it signs
/// names.
pub struct DevicePosition {
    pub agent: AgentContext,
    pub scid: String,
    pub did: String,
    /// The device key, a multikey.
    pub key: Multikey,
    /// Its id in the log (`d3`).
    pub key_id: String,
    /// The accepted head at the log's trusted seq.
    pub head: SignedHead,
}

impl DevicePosition {
    /// A store request for `method path` with `body` (empty for a read), signed by the device
    /// key at this position.
    pub fn request(
        &self,
        method: &str,
        path: &str,
        body: &[u8],
    ) -> Result<ad4m_ldk::identity::SignedRequest, WsRpcError> {
        let signer = WalletSigner::new(&self.agent).map_err(WsRpcError::internal)?;
        let request = store_request(
            &signer,
            &self.did,
            &self.key_id,
            &self.head,
            method,
            path,
            body,
            now() + REQUEST_TTL_S,
        )
        .map_err(WsRpcError::internal)?;
        signer.check().map_err(WsRpcError::internal)?;
        Ok(request)
    }

    /// The device key's signature over the envelope of `sealed` (`coasys:private-record`), for
    /// an envelope that names this device key as its `signer`; 400 `signer` otherwise.
    pub fn sign_envelope(&self, sealed: &SealedRecord) -> Result<String, WsRpcError> {
        use ad4m_identity_core::build::Signer;
        if sealed.signer != self.key_id {
            return Err(WsRpcError::bad_request(format!(
                "signer: an unsigned record must name this device's key {}, not {}",
                self.key_id, sealed.signer
            )));
        }
        let envelope: ad4m_identity_core::record::SealedRecord =
            convert(sealed).map_err(|e| WsRpcError::bad_request(e.to_string()))?;
        let bytes = ad4m_identity_core::record::envelope_signing_bytes(&self.scid, &envelope)
            .map_err(|e| WsRpcError::bad_request(e.to_string()))?;
        let signer = WalletSigner::new(&self.agent).map_err(WsRpcError::internal)?;
        let sig = signer.sign(&bytes);
        signer.check().map_err(WsRpcError::internal)?;
        Ok(ad4m_hash_chain::encode_proof_value(&sig))
    }
}

impl IdentityService {
    /// `agent`'s [`DevicePosition`]: bound to an identity (`identity_agents`), its key a device
    /// key of that log valid at the trusted seq by the clock now, without the bot mark. `None`
    /// when the agent speaks for no one.
    pub async fn device_position(
        &self,
        agent: &AgentContext,
    ) -> Result<Option<DevicePosition>, WsRpcError> {
        let (agent_did, key) = device_key(agent).map_err(WsRpcError::internal)?;
        let Some(scid) = db(|db| db.agent_identity(&agent_did))? else {
            return Ok(None);
        };
        let Some((chain, trusted)) = self.cache.trusted(&scid).await.map_err(cache_error)? else {
            return Ok(None);
        };
        let state = chain.state();
        let Some(key_id) = super::resolver::device_key_id(state, &key).filter(|id| {
            state.bot_scope_at(id, trusted).is_none() && state.key_valid_at_time(id, trusted, now())
        }) else {
            return Ok(None);
        };
        let head = self
            .cache
            .accepted_head(&scid)
            .map_err(cache_error)?
            .filter(|head| head.seq == trusted)
            .ok_or_else(|| WsRpcError::internal("the accepted head moved while signing"))?;
        Ok(Some(DevicePosition {
            agent: agent.clone(),
            did: did_of(&scid),
            scid,
            key,
            key_id,
            head,
        }))
    }

    /// [`Self::device_position`], or 409 `no-identity`.
    pub async fn bound_position(&self, agent: &AgentContext) -> Result<DevicePosition, WsRpcError> {
        self.device_position(agent).await?.ok_or_else(|| {
            super::service::error(
                409,
                "no-identity: this agent's device key speaks for no identity",
            )
        })
    }

    /// The request a call passes: the caller's, or one this agent's device key signs.
    fn request_for(
        &self,
        position: &DevicePosition,
        given: Option<&SignedRequest>,
        method: &str,
        path: &str,
        body: &[u8],
    ) -> Result<ad4m_ldk::identity::SignedRequest, WsRpcError> {
        match given {
            Some(request) => convert(request).map_err(|e| WsRpcError::bad_request(e.to_string())),
            None => position.request(method, path, body),
        }
    }

    /// `identity.record`: the sealed private record of `agent`'s identity with the store's
    /// receipt for it, both as the store answered; `null` before the first write.
    pub async fn record(
        &self,
        agent: &AgentContext,
        params: &RecordParams,
    ) -> Result<Option<StoredRecord>, WsRpcError> {
        let position = self.bound_position(agent).await?;
        let path = format!("/v1/private/{}", position.scid);
        let request = self.request_for(&position, params.request.as_ref(), "GET", &path, b"")?;
        let record = self
            .backend
            .get_record(&position.did, &request)
            .await
            .map_err(backend_error)?;
        wire(&record)
    }

    /// `identity.recordPut`: writes the next version of the sealed record; answers the version
    /// and the store's receipt for the write. An envelope with an empty `sig` is this device's
    /// write: its `signer` must be the agent's device key id, and the device key signs the
    /// envelope in the wallet (an app's device key never leaves it). A signed envelope (a
    /// master-signed write) passes unchanged.
    pub async fn record_put(
        &self,
        agent: &AgentContext,
        params: &RecordPutParams,
    ) -> Result<RecordStored, WsRpcError> {
        let position = self.bound_position(agent).await?;
        let mut sealed = params.record.clone();
        if sealed.sig.is_empty() {
            sealed.sig = position.sign_envelope(&sealed)?;
        }
        let record: ad4m_ldk::identity::SealedRecord =
            convert(&sealed).map_err(|e| WsRpcError::bad_request(e.to_string()))?;
        let path = format!("/v1/private/{}", position.scid);
        let body = body_bytes(&record).map_err(WsRpcError::internal)?;
        let request = self.request_for(&position, params.request.as_ref(), "PUT", &path, &body)?;
        let stored = self
            .backend
            .put_record(&position.did, &record, &request)
            .await
            .map_err(backend_error)?;
        wire(&stored)
    }

    /// `identity.unwrapRecordKey`: the record key that `wrapped`, this device's entry of a
    /// record of `agent`'s identity, seals to the agent's device key, opened in the wallet. A
    /// locked wallet is 503 `unavailable`; an entry that does not open is 400 `open`.
    pub async fn unwrap_record_key(
        &self,
        agent: &AgentContext,
        params: &UnwrapRecordKeyParams,
    ) -> Result<RecordKey, WsRpcError> {
        let position = self.bound_position(agent).await?;
        let wallet = crate::wallet::wallet_backend();
        let name = agent
            .wallet_key_name()
            .map_err(|e| WsRpcError::internal(e.to_string()))?;
        if !wallet.is_unlocked() {
            return Err(super::service::error(
                503,
                "unavailable: the wallet is locked",
            ));
        }
        let key = wallet
            .unwrap_record_key(&name, &position.scid, &params.wrapped)
            .map_err(|e| WsRpcError::bad_request(format!("open: {e}")))?;
        Ok(RecordKey {
            key: bs58::encode(key.as_ref()).into_string(),
        })
    }

    /// `identity.requests`: the request box since `since` (unix seconds, default all).
    pub async fn requests(
        &self,
        agent: &AgentContext,
        params: &RequestsParams,
    ) -> Result<Vec<DeviceRequest>, WsRpcError> {
        let position = self.bound_position(agent).await?;
        let path = format!("/v1/requests/{}", position.scid);
        let request = self.request_for(&position, params.request.as_ref(), "GET", &path, b"")?;
        let requests = self
            .backend
            .list_requests(&position.did, params.since.unwrap_or(0), &request)
            .await
            .map_err(backend_error)?;
        wire(&requests)
    }

    /// `identity.postRequest`: posts a request to the box; it lives until `expires` (at most,
    /// and by default, 7 days).
    pub async fn post_request(
        &self,
        agent: &AgentContext,
        params: &PostRequestParams,
    ) -> Result<RequestPosted, WsRpcError> {
        let position = self.bound_position(agent).await?;
        let post = PostedRequest {
            body: convert(&params.body).map_err(|e| WsRpcError::bad_request(e.to_string()))?,
            expires: params.expires.unwrap_or_else(|| now() + REQUEST_BOX_TTL_S),
        };
        let path = format!("/v1/requests/{}", position.scid);
        let body = body_bytes(&post).map_err(WsRpcError::internal)?;
        let request = position.request("POST", &path, &body)?;
        let posted = self
            .backend
            .post_request(&position.did, &post, &request)
            .await
            .map_err(backend_error)?;
        wire(&posted)
    }

    /// `identity.deleteRequest`: deletes request `id` (the poster's, or by the master key).
    pub async fn delete_request(
        &self,
        agent: &AgentContext,
        params: &DeleteRequestParams,
    ) -> Result<bool, WsRpcError> {
        let position = self.bound_position(agent).await?;
        let path = format!("/v1/requests/{}/{}", position.scid, params.id);
        let request = self.request_for(&position, params.request.as_ref(), "DELETE", &path, b"")?;
        self.backend
            .delete_request(&position.did, &params.id, &request)
            .await
            .map_err(backend_error)?;
        Ok(true)
    }
}

/// The agent language's answer as this module's RPC type.
fn wire<A: Serialize, B: serde::de::DeserializeOwned>(value: &A) -> Result<B, WsRpcError> {
    convert(value).map_err(|e| WsRpcError::internal(format!("the agent language answered {e}")))
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::identity::cache::tests::{accept_signed, trusting, FakeStore, TestLog};
    use crate::identity::device::enc_key_of;
    use crate::identity::types::DeviceRequestBody;
    use ad4m_hash_chain::{decode_proof_value, hash_bytes};
    use ad4m_identity_core::build::{add_unsigned, cosign};
    use ad4m_identity_core::{verify_signed, REQUEST_DOMAIN};
    use std::sync::Arc;

    /// A managed agent whose key is `d2` of a person's log the store signed, on a service over
    /// that store.
    pub(crate) struct Bound {
        pub service: Arc<IdentityService>,
        pub store: Arc<FakeStore>,
        pub agent: AgentContext,
        pub key: String,
        pub log: TestLog,
    }

    impl Bound {
        pub async fn new(seed: u8) -> Bound {
            crate::test_utils::setup_wallet();
            crate::test_utils::setup_agent();
            let user = crate::test_utils::create_test_user(&format!(
                "bound-{}@example.org",
                uuid::Uuid::new_v4().simple()
            ));
            let agent = AgentContext::managed(user);
            let (agent_did, key) = device_key(&agent).unwrap();
            let mut log = TestLog::new(seed);
            let chain = log.chain();
            let device = ad4m_identity_core::DeviceKey {
                id: chain.state().next_device_id(),
                enc: enc_key_of(&key).unwrap(),
                sign: key.clone(),
                life: ad4m_identity_core::KeyLife::Standing,
            };
            let mut add = add_unsigned(&chain, device, "offer", "co-sig").unwrap();
            cosign(&mut add, "m0", &log.master).unwrap();
            log.events.push(add);
            let store = Arc::new(FakeStore::new(log.events.clone()));
            let cache = Arc::new(trusting());
            accept_signed(&cache, &log.events).await;
            let backend: Arc<dyn crate::identity::backend::IdentityBackend> = store.clone();
            let service = Arc::new(IdentityService {
                resolver: Arc::new(crate::identity::resolver::Resolver::new(
                    cache.clone(),
                    backend.clone(),
                    None,
                    false,
                )),
                backend,
                cache,
                store_url: Some("http://store.test".into()),
                challenges: Default::default(),
                inbox: Default::default(),
                audiences: Default::default(),
                store_keys_source: crate::identity::StoreSource::Config,
            });
            db(|db| db.set_agent_identity(&agent_did, &log.scid(), now())).unwrap();
            Bound {
                service,
                store,
                agent,
                key,
                log,
            }
        }
    }

    /// The executor relays the signed envelope whole: the writer's key id and signature and the
    /// recovery key's master attestation reach the store and come back from a read unchanged,
    /// and so does the store's receipt, from the write and from the read. A field it dropped
    /// would break the envelope signature, or the receipt's, that every reader checks.
    #[tokio::test]
    async fn the_record_passes_the_signed_envelope_and_the_recovery_attestation_unchanged() {
        let world = Bound::new(72).await;
        let service = &world.service;
        let wire_record = serde_json::json!({
            "version": 1,
            "wrapped": {"d2": "AAAA", "recovery": "RRRR"},
            "recoveryKey": "z6LSRec",
            "recoveryAttestation": {"keyId": "m0", "sig": "zAtt"},
            "nonce": "zN",
            "ciphertext": "CCCC",
            "signer": "d2",
            "sig": "zSig",
        });
        let params: RecordPutParams =
            serde_json::from_value(serde_json::json!({ "record": wire_record })).unwrap();
        let stored = service.record_put(&world.agent, &params).await.unwrap();
        let held = world.store.record.lock().unwrap().clone().unwrap();
        let receipt = serde_json::to_value(&held.receipt).unwrap();
        assert_eq!(
            serde_json::to_value(&stored).unwrap(),
            serde_json::json!({ "version": 1, "receipt": receipt })
        );
        let read = service
            .record(&world.agent, &RecordParams::default())
            .await
            .unwrap()
            .expect("the stored record");
        assert_eq!(
            serde_json::to_value(&read).unwrap(),
            serde_json::json!({ "record": wire_record, "receipt": receipt })
        );
        // The store's signature over the envelope as it reached it: still a receipt a reader
        // takes for this log.
        let sealed: ad4m_identity_core::record::SealedRecord =
            serde_json::from_value(wire_record.clone()).unwrap();
        let core_receipt: ad4m_identity_core::record::RecordReceipt =
            serde_json::from_value(receipt).unwrap();
        assert_eq!(
            core_receipt.envelope,
            ad4m_identity_core::record::envelope_hash(&world.log.scid(), &sealed).unwrap()
        );
        assert_eq!(
            core_receipt.store,
            format!("did:key:{}", world.store.key.multikey())
        );
        let seen = world.store.requests.lock().unwrap().clone();
        let (call, put) = &seen[0];
        assert_eq!(*call, "put_record");
        let ldk: ad4m_ldk::identity::SealedRecord = serde_json::from_value(wire_record).unwrap();
        assert_eq!(put.req.body_hash, hash_bytes(&body_bytes(&ldk).unwrap()));
    }

    /// Without a request from the caller, each call signs one with the agent's device key at
    /// the trusted head, for the store that signed it, over the route and body it sends.
    #[tokio::test]
    async fn the_record_and_the_request_box_sign_with_the_device_key() {
        let world = Bound::new(71).await;
        let service = &world.service;
        let scid = world.log.scid();
        assert!(service
            .record(&world.agent, &RecordParams::default())
            .await
            .unwrap()
            .is_none());
        let sealed = crate::identity::types::SealedRecord {
            version: 1,
            wrapped: [("d1".to_string(), "AAAA".to_string())].into(),
            recovery_key: None,
            recovery_attestation: None,
            nonce: "zN".into(),
            ciphertext: "CCCC".into(),
            signer: "d1".into(),
            sig: "zSig".into(),
        };
        let stored = service
            .record_put(
                &world.agent,
                &RecordPutParams {
                    record: sealed.clone(),
                    request: None,
                },
            )
            .await
            .unwrap();
        assert_eq!(stored.version, 1);
        assert_eq!(
            service
                .record(&world.agent, &RecordParams::default())
                .await
                .unwrap(),
            Some(StoredRecord {
                record: sealed.clone(),
                receipt: stored.receipt.clone(),
            })
        );
        service
            .post_request(
                &world.agent,
                &PostRequestParams {
                    body: DeviceRequestBody::SendMaster {},
                    expires: None,
                },
            )
            .await
            .unwrap();
        service
            .requests(&world.agent, &RequestsParams::default())
            .await
            .unwrap();
        assert!(service
            .delete_request(
                &world.agent,
                &DeleteRequestParams {
                    id: "r1".into(),
                    request: None
                }
            )
            .await
            .unwrap());

        let ldk_record: ad4m_ldk::identity::SealedRecord = convert(&sealed).unwrap();
        let expected = [
            ("get_record", "GET", format!("/v1/private/{scid}"), vec![]),
            (
                "put_record",
                "PUT",
                format!("/v1/private/{scid}"),
                body_bytes(&ldk_record).unwrap(),
            ),
            ("get_record", "GET", format!("/v1/private/{scid}"), vec![]),
            (
                "post_request",
                "POST",
                format!("/v1/requests/{scid}"),
                vec![],
            ),
            (
                "list_requests",
                "GET",
                format!("/v1/requests/{scid}"),
                vec![],
            ),
            (
                "delete_request",
                "DELETE",
                format!("/v1/requests/{scid}/r1"),
                vec![],
            ),
        ];
        let seen = world.store.requests.lock().unwrap().clone();
        assert_eq!(seen.len(), expected.len());
        let head = world.log.chain().head().clone();
        for ((call, request), (want, method, path, body)) in seen.iter().zip(expected) {
            assert_eq!(*call, want);
            let req = &request.req;
            assert_eq!(
                (req.method.as_str(), req.path.as_str()),
                (method, path.as_str()),
                "{call}"
            );
            assert_eq!(req.key_id, "d2");
            assert_eq!(req.did, did_of(&scid));
            assert_eq!(
                (req.kel_seq, req.kel_hash.as_str()),
                (head.seq, head.hash.as_str())
            );
            assert_eq!(req.store, format!("did:key:{}", world.store.key.multikey()));
            if call != &"post_request" {
                assert_eq!(req.body_hash, hash_bytes(&body), "{call}");
            }
            let sig = decode_proof_value(&request.sig).unwrap();
            assert!(
                verify_signed(REQUEST_DOMAIN, req, &world.key, &sig),
                "{call}"
            );
        }
    }

    /// An agent bound to no identity gets 409 `no-identity`, and a caller's own request is
    /// passed as it is.
    #[tokio::test]
    async fn an_unbound_agent_has_no_record_and_a_given_request_passes_unchanged() {
        let world = Bound::new(72).await;
        let user = crate::test_utils::create_test_user(&format!(
            "unbound-{}@example.org",
            uuid::Uuid::new_v4().simple()
        ));
        let refused = world
            .service
            .record(&AgentContext::managed(user), &RecordParams::default())
            .await
            .unwrap_err();
        assert_eq!(refused.code, 409);
        assert!(
            refused.message.starts_with("no-identity"),
            "{}",
            refused.message
        );

        let given = world
            .service
            .bound_position(&world.agent)
            .await
            .unwrap()
            .request("GET", "/v1/private/elsewhere", b"")
            .unwrap();
        let mirror: SignedRequest = convert(&given).unwrap();
        world
            .service
            .record(
                &world.agent,
                &RecordParams {
                    request: Some(mirror),
                },
            )
            .await
            .unwrap();
        let seen = world.store.requests.lock().unwrap().clone();
        assert_eq!(seen.last().unwrap().1, given);
    }

    /// The record key of the agent's entry opens in the wallet and decrypts that version; an
    /// entry sealed to another device's key does not open.
    #[tokio::test]
    async fn the_record_key_of_the_agents_entry_opens_in_the_wallet() {
        use ad4m_identity_core::record::{decrypt_record, seal_record, PrivateRecord};
        let world = Bound::new(73).await;
        let chain = world.log.chain();
        let record = PrivateRecord {
            words_checked_at: Some(7),
            ..Default::default()
        };
        let sealed = seal_record(
            &chain,
            1,
            &record,
            None,
            &world.log.master,
            &mut rand::rngs::OsRng,
        )
        .unwrap();
        let key = world
            .service
            .unwrap_record_key(
                &world.agent,
                &UnwrapRecordKeyParams {
                    wrapped: sealed.wrapped["d2"].clone(),
                },
            )
            .await
            .unwrap();
        let bytes: [u8; 32] = bs58::decode(&key.key)
            .into_vec()
            .unwrap()
            .try_into()
            .unwrap();
        let receipt = ad4m_identity_core::record::sign_receipt(
            &world.log.scid(),
            &sealed,
            chain.head().seq,
            &world.store.key.0,
        )
        .unwrap();
        let stores = [world.store.key.multikey()];
        assert_eq!(
            decrypt_record(&sealed, &receipt, &chain, &stores, &bytes),
            Ok(record)
        );
        assert!(!format!("{key:?}").contains(&key.key));

        let refused = world
            .service
            .unwrap_record_key(
                &world.agent,
                &UnwrapRecordKeyParams {
                    wrapped: sealed.wrapped["d1"].clone(),
                },
            )
            .await
            .unwrap_err();
        assert_eq!(refused.code, 400);
        assert!(refused.message.starts_with("open"), "{}", refused.message);
    }

    /// An envelope with an empty `sig` is this device's write: the device key signs it in the
    /// wallet, and the store gets an envelope that verifies as the device's. One that names
    /// another key is refused before anything is signed.
    #[tokio::test]
    async fn an_unsigned_envelope_is_signed_by_the_device_key() {
        use ad4m_identity_core::record::{check_sealed, seal_unsigned, PrivateRecord, Writer};
        let world = Bound::new(74).await;
        let chain = world.log.chain();
        let record = PrivateRecord {
            words_checked_at: Some(9),
            ..Default::default()
        };
        let unsigned = seal_unsigned(
            &chain,
            1,
            &record,
            None,
            "d2".into(),
            &mut rand::rngs::OsRng,
        )
        .unwrap();
        assert!(unsigned.sig.is_empty());
        let params = RecordPutParams {
            record: convert(&unsigned).unwrap(),
            request: None,
        };
        world
            .service
            .record_put(&world.agent, &params)
            .await
            .unwrap();
        let stored: ad4m_identity_core::record::SealedRecord = convert(
            &world
                .service
                .record(&world.agent, &RecordParams::default())
                .await
                .unwrap()
                .unwrap()
                .record,
        )
        .unwrap();
        assert_eq!(
            check_sealed(&stored, &chain),
            Ok(Writer::Device("d2".into()))
        );

        let other = seal_unsigned(
            &chain,
            2,
            &record,
            None,
            "m0".into(),
            &mut rand::rngs::OsRng,
        )
        .unwrap();
        let refused = world
            .service
            .record_put(
                &world.agent,
                &RecordPutParams {
                    record: convert(&other).unwrap(),
                    request: None,
                },
            )
            .await
            .unwrap_err();
        assert_eq!(refused.code, 400);
        assert!(refused.message.starts_with("signer"), "{}", refused.message);
    }
}
