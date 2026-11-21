//! This device's enc key, held by the app ([`EncKey`]), and the adapter that answers the
//! executor calls that need it ([`WithEncKey`]).
//!
//! The log lists the app's enc key as the enc key of the executor's device key: the holder seals
//! the master key and wraps the record key to it. The executor never holds it. It passes the
//! joined inbox through as sealed text (`identity.inboxSealed`); this crate opens each message
//! with the enc key and checks it against its own fold of the log ([`take_from`]). The adapter
//! keeps the wire shapes of `identity.takeMaster`, `identity.inbox` and
//! `identity.unwrapRecordKey` for its caller, so every flow runs unchanged over
//! `ViaExecutor<WithEncKey<R>>`.
//!
//! A phone has no executor: its `ViaStore` (feature `store`) holds its own enc key, reads the
//! joined inbox from the store and runs the same `take_from`, so the desktop's custody
//! service and the phone open, check and choose the master key in one place.

use ad4m_identity_core::inbox::{
    check_payload, Accept, InboxAddress, InboxBody, InboxError, InboxPayload,
};
use ad4m_identity_core::record::{RecordError, RECORD_KEY_LEN};
use ad4m_identity_core::X25519Secret;
use ad4m_identity_core::{x25519_multikey_of, x25519_secret_of_ed25519, KelChain, Multikey};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use zeroize::Zeroize;

use crate::executor::{ExecutorRpc, IdentityLog, IdentityStatus, RpcError, TakenMaster};
use crate::secret::Secret;
use crate::Clock;

/// Why [`EncKey::open`] or [`EncKey::unwrap_record_key`] gave no answer. `Unavailable` is
/// transient — the keystore is locked or the open failed in between — so the message is kept
/// and the call answers `unavailable` (503); `Sealed` is final — the bytes do not open with
/// this device's key at this address — so the message is for no one here and is dropped.
#[derive(Debug, thiserror::Error)]
pub enum EncOpenError<E> {
    /// The key cannot open right now: a locked keystore, an I/O failure. Try again later.
    #[error("{0}")]
    Unavailable(String),
    /// It does not open with this device's key: it is for no one here.
    #[error("{0}")]
    Sealed(E),
}

/// This device's X25519 enc key, wherever it is kept (a desktop's custody service, a phone's
/// keystore): it opens inbox messages and unwraps the record's key for this device's entry.
/// [`SoftEncKey`] is the software version for tests and callers without a keystore.
pub trait EncKey {
    /// The `enc` multikey a claim, an incept or a device string carries.
    fn enc(&self) -> Multikey;
    /// Opens a message sealed for `address` (`coasys:inbox` + address is the seal's `info`, so
    /// a seal for one inbox never opens at another); [`check_payload`] checks it after.
    /// `Unavailable` keeps the message for the next try; `Sealed` drops it.
    fn open(
        &self,
        address: &InboxAddress,
        sealed: &str,
    ) -> Result<InboxPayload, EncOpenError<InboxError>>;
    /// The record key that `wrapped` (this device's entry of a record's `wrapped`) seals for
    /// identity `scid` to this key — the same two error cases as `open`.
    fn unwrap_record_key(
        &self,
        scid: &str,
        wrapped: &str,
    ) -> Result<Secret<[u8; RECORD_KEY_LEN]>, EncOpenError<RecordError>>;
}

/// An enc key held in memory.
pub struct SoftEncKey {
    secret: X25519Secret,
}

impl SoftEncKey {
    pub fn new(secret: X25519Secret) -> Self {
        SoftEncKey { secret }
    }

    /// The X25519 form of an Ed25519 key.
    pub fn from_ed25519(key: &ed25519_dalek::SigningKey) -> Self {
        Self::new(x25519_secret_of_ed25519(key))
    }
}

impl EncKey for SoftEncKey {
    fn enc(&self) -> Multikey {
        x25519_multikey_of(&self.secret)
    }

    fn open(
        &self,
        address: &InboxAddress,
        sealed: &str,
    ) -> Result<InboxPayload, EncOpenError<InboxError>> {
        ad4m_identity_core::inbox::open(&self.secret, address, sealed).map_err(EncOpenError::Sealed)
    }

    fn unwrap_record_key(
        &self,
        scid: &str,
        wrapped: &str,
    ) -> Result<Secret<[u8; RECORD_KEY_LEN]>, EncOpenError<RecordError>> {
        ad4m_identity_core::record::unwrap_key(wrapped, scid, &self.secret)
            .map(|key| Secret::new(*key))
            .map_err(EncOpenError::Sealed)
    }
}

/// `identity.inboxSealed`: one joined message of the executor's device key, as the store keeps
/// it. The executor does not open it.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SealedInboxMessage {
    pub id: String,
    /// base64url HPKE text, sealed to the enc key the log lists for this device key.
    pub sealed: String,
    /// The address the seal's `info` binds: `{scid, keyId}`.
    pub scid: String,
    pub key_id: String,
    pub stored_at: u64,
    pub expires: u64,
}

impl SealedInboxMessage {
    fn address(&self) -> InboxAddress {
        InboxAddress::Joined {
            scid: self.scid.clone(),
            key_id: self.key_id.clone(),
        }
    }
}

/// What [`take_from`] found.
pub(crate) struct Taken {
    /// The message, its identity, its index and the secret (wiped on drop), or `None`.
    pub master: Option<TakenMaster>,
    /// The ids to acknowledge and forget.
    pub drop: Vec<String>,
}

/// The oldest current master key among `messages` (the joined inbox of device `me`, a signing
/// multikey, of `chain`'s identity), opened with `enc` and checked by `check_payload` at `now`.
/// Each message that does not open (`Sealed`), is stale, or fails the check goes into `drop`.
/// `Unavailable` stops the search: the messages stay. An offer ticket stays too: it is not a
/// master key, and the device claims it elsewhere.
pub(crate) fn take_from(
    enc: &dyn EncKey,
    messages: &[SealedInboxMessage],
    me: &str,
    chain: &KelChain,
    now: u64,
) -> Result<Taken, EncOpenError<InboxError>> {
    let mut drop = Vec::new();
    for message in messages {
        let mut payload = match enc.open(&message.address(), &message.sealed) {
            Ok(payload) => payload,
            Err(EncOpenError::Sealed(_)) => {
                drop.push(message.id.clone());
                continue;
            }
            Err(unavailable) => return Err(unavailable),
        };
        if !matches!(payload.body, InboxBody::Master { .. }) {
            continue;
        }
        match check_payload(&payload, me, chain, now) {
            Ok(Accept::KeepMaster { index }) => {
                let master = match &payload.body {
                    InboxBody::Master { did, secret, .. } => {
                        decode_secret(secret).map(|secret| TakenMaster {
                            message_id: message.id.clone(),
                            did: did.clone(),
                            index,
                            secret,
                        })
                    }
                    InboxBody::OfferTicket(_) => None,
                };
                payload.body.wipe();
                match master {
                    Some(master) => {
                        return Ok(Taken {
                            master: Some(master),
                            drop,
                        })
                    }
                    None => drop.push(message.id.clone()),
                }
            }
            Ok(Accept::ClaimOffer(_)) => payload.body.wipe(),
            Ok(Accept::Stale) | Err(_) => {
                payload.body.wipe();
                drop.push(message.id.clone());
            }
        }
    }
    Ok(Taken { master: None, drop })
}

/// The 32 bytes of a base58btc secret, the decoded copy wiped.
fn decode_secret(text: &str) -> Option<Secret<[u8; 32]>> {
    let mut bytes = bs58::decode(text).into_vec().ok()?;
    let secret = <[u8; 32]>::try_from(bytes.as_slice()).ok().map(Secret::new);
    bytes.zeroize();
    secret
}

/// An executor RPC whose calls that need this device's enc key run here, with `enc`.
pub struct WithEncKey<R: ExecutorRpc> {
    rpc: R,
    enc: Box<dyn EncKey>,
    clock: Box<dyn Clock>,
}

fn answered<T: serde::de::DeserializeOwned>(method: &str, value: Value) -> Result<T, RpcError> {
    serde_json::from_value(value).map_err(|e| RpcError {
        code: 502,
        message: format!("{method} answered {e}"),
    })
}

fn unavailable(message: String) -> RpcError {
    RpcError {
        code: 503,
        message: format!("unavailable: {message}"),
    }
}

impl<R: ExecutorRpc> WithEncKey<R> {
    pub fn new(rpc: R, enc: Box<dyn EncKey>, clock: Box<dyn Clock>) -> Self {
        WithEncKey { rpc, enc, clock }
    }

    /// The executor RPC under the adapter.
    pub fn rpc(&self) -> &R {
        &self.rpc
    }

    /// The enc multikey this adapter answers and adds.
    pub fn enc(&self) -> Multikey {
        self.enc.enc()
    }

    async fn status(&self) -> Result<IdentityStatus, RpcError> {
        const METHOD: &str = "identity.status";
        answered(METHOD, self.rpc.call(METHOD, json!({})).await?)
    }

    async fn sealed(&self) -> Result<Vec<SealedInboxMessage>, RpcError> {
        const METHOD: &str = "identity.inboxSealed";
        answered(METHOD, self.rpc.call(METHOD, json!({})).await?)
    }

    async fn chain(&self, did: &str) -> Result<KelChain, RpcError> {
        const METHOD: &str = "identity.log";
        let log: IdentityLog = answered(
            METHOD,
            self.rpc
                .call(METHOD, json!({ "did": did, "from": 0, "fresh": true }))
                .await?,
        )?;
        let chain = ad4m_identity_core::fold_json(&log.events).map_err(|e| RpcError {
            code: 502,
            message: format!(
                "{METHOD} served a log that does not fold: {}",
                ad4m_identity_core::error_code(&e)
            ),
        })?;
        // A valid log of another identity would check that identity's messages.
        if chain.state().did != did {
            return Err(RpcError {
                code: 502,
                message: format!("{METHOD} served the log of {} for {did}", chain.state().did),
            });
        }
        Ok(chain)
    }

    /// The oldest current master key sealed to this device, opened here: the `TakenMaster` JSON
    /// (`messageId`, `did`, `index`, `secret`) or `null`. Each message that does not open, is
    /// stale or fails the check is acknowledged; a failed acknowledgement is tried again at the
    /// next take.
    async fn take_master(&self) -> Result<Value, RpcError> {
        let status = self.status().await?;
        let (Some(did), Some(_)) = (status.did, status.key_id) else {
            return Ok(Value::Null);
        };
        let messages = self.sealed().await?;
        if messages.is_empty() {
            return Ok(Value::Null);
        }
        let chain = self.chain(&did).await?;
        let taken = take_from(
            self.enc.as_ref(),
            &messages,
            &status.device_key,
            &chain,
            self.clock.now(),
        )
        .map_err(|e| match e {
            EncOpenError::Unavailable(m) => unavailable(format!(
                "the enc key is locked, so the inbox does not open: {m}"
            )),
            EncOpenError::Sealed(e) => RpcError {
                code: 400,
                message: format!("open: {e}"),
            },
        })?;
        for id in &taken.drop {
            let _ = self
                .rpc
                .call("identity.inboxAck", json!({ "id": id }))
                .await;
        }
        Ok(match taken.master {
            Some(master) => json!({
                "messageId": master.message_id,
                "did": master.did,
                "index": master.index,
                "secret": bs58::encode(master.secret.expose()).into_string(),
            }),
            None => Value::Null,
        })
    }

    /// The joined inbox with each message's kind: `[{id, kind, storedAt, expires}]`, `kind`
    /// `"master"` or `"offer-ticket"`. Each body is wiped as soon as its kind is read; a
    /// message that does not open is left out (a read acknowledges nothing).
    async fn inbox(&self) -> Result<Value, RpcError> {
        let mut entries = Vec::new();
        for message in self.sealed().await? {
            let mut payload = match self.enc.open(&message.address(), &message.sealed) {
                Ok(payload) => payload,
                Err(EncOpenError::Sealed(_)) => continue,
                Err(EncOpenError::Unavailable(m)) => {
                    return Err(unavailable(format!(
                        "the enc key is locked, so the inbox does not open: {m}"
                    )))
                }
            };
            let kind = match payload.body {
                InboxBody::Master { .. } => "master",
                InboxBody::OfferTicket(_) => "offer-ticket",
            };
            payload.body.wipe();
            entries.push(json!({
                "id": message.id,
                "kind": kind,
                "storedAt": message.stored_at,
                "expires": message.expires,
            }));
        }
        Ok(Value::Array(entries))
    }

    /// `{key}`: the record key `params.wrapped` wraps to this device's enc key, base58btc.
    /// An entry this key does not open goes to the executor: a device linked without keeping
    /// the master key claimed with its wallet enc key (the adapter names its own only with
    /// `canKeepMaster`), so the log lists that one and the executor's wallet opens the entry.
    async fn unwrap_record_key(&self, params: Value) -> Result<Value, RpcError> {
        let wrapped = params["wrapped"].as_str().ok_or(RpcError {
            code: 400,
            message: "identity.unwrapRecordKey takes {wrapped}".into(),
        })?;
        let did = self.status().await?.did.ok_or(RpcError {
            code: 404,
            message: "the executor's device key is in no log".into(),
        })?;
        let scid = ad4m_identity_core::scid_of(&did).ok_or(RpcError {
            code: 502,
            message: format!("identity.status answered {did}, not a did:scid:coasys:1"),
        })?;
        match self.enc.unwrap_record_key(scid, wrapped) {
            Ok(key) => Ok(json!({ "key": bs58::encode(key.expose()).into_string() })),
            Err(EncOpenError::Sealed(_)) => self.rpc.call("identity.unwrapRecordKey", params).await,
            Err(EncOpenError::Unavailable(m)) => Err(unavailable(format!(
                "the enc key is locked, so the record does not open: {m}"
            ))),
        }
    }

    /// The executor's answer to `method`, with `field` set to this device's enc key.
    async fn with_enc_answered(
        &self,
        method: &str,
        params: Value,
        field: &str,
    ) -> Result<Value, RpcError> {
        let mut value = self.rpc.call(method, params).await?;
        if let Some(object) = value.as_object_mut() {
            object.insert(field.into(), Value::String(self.enc.enc()));
        }
        Ok(value)
    }

    /// The executor's call to `method` with `"enc"` added to its params.
    async fn with_enc_sent(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        let mut params = match params {
            Value::Object(object) => object,
            _ => serde_json::Map::new(),
        };
        params.insert("enc".into(), Value::String(self.enc.enc()));
        self.rpc.call(method, Value::Object(params)).await
    }
}

#[async_trait(?Send)]
impl<R: ExecutorRpc> ExecutorRpc for WithEncKey<R> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        match method {
            "identity.takeMaster" => self.take_master().await,
            "identity.inbox" => self.inbox().await,
            "identity.unwrapRecordKey" => self.unwrap_record_key(params).await,
            "identity.status" => self.with_enc_answered(method, params, "deviceEnc").await,
            "identity.custodyStatus" => self.with_enc_answered(method, params, "enc").await,
            "identity.claimOffer" if params["canKeepMaster"] == true => {
                self.with_enc_sent(method, params).await
            }
            "identity.prepareDevice" => self.with_enc_sent(method, params).await,
            _ => self.rpc.call(method, params).await,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ad4m_identity_core::build::{cosign, incept_human, rotate, Signer};
    use ad4m_identity_core::inbox::{seal, sign_payload};
    use ad4m_identity_core::link::SignedBy;
    use ad4m_identity_core::{commitment, DeviceKey, KeyLife};
    use ed25519_dalek::SigningKey;

    const NOW: u64 = 1_800_000_000;

    fn key(seed: u8) -> SigningKey {
        SigningKey::from_bytes(&[seed; 32])
    }

    fn salt(seed: u8) -> String {
        bs58::encode([seed; 16]).into_string()
    }

    /// A log with device `d1` (signing key `key(3)`, enc `custody`), rotated once: `m0` is
    /// stale, `m1` is current.
    fn rotated_log(custody: &dyn EncKey) -> KelChain {
        let (m0, m1, m2, d1) = (key(1), key(2), key(9), key(3));
        let mut incept = incept_human(
            &m0,
            commitment(&m1.key(), &salt(1)),
            vec![DeviceKey {
                id: "d1".into(),
                sign: d1.key(),
                enc: custody.enc(),
                life: KeyLife::Standing,
            }],
        )
        .unwrap();
        cosign(&mut incept, "d1", &d1).unwrap();
        let chain = crate::fold(std::slice::from_ref(&incept)).unwrap();
        let rotation = rotate(
            &chain,
            &m1,
            &salt(1),
            Some(commitment(&m2.key(), &salt(2))),
            &[],
        )
        .unwrap();
        crate::fold(&[incept, rotation]).unwrap()
    }

    /// A message to `to` with master key `index` (secret `secret`), signed by `signer` as
    /// master `by` at seq `seq`, sealed to `enc`.
    #[allow(clippy::too_many_arguments)]
    fn message(
        id: &str,
        chain: &KelChain,
        to: &SigningKey,
        index: u32,
        secret: &SigningKey,
        by: &str,
        seq: u64,
        signer: &SigningKey,
        enc: &str,
    ) -> SealedInboxMessage {
        let address = InboxAddress::Joined {
            scid: chain.id().to_string(),
            key_id: "d1".into(),
        };
        let body = InboxBody::Master {
            did: chain.state().did.clone(),
            index,
            secret: bs58::encode(secret.to_bytes()).into_string(),
        };
        let by = SignedBy {
            key_id: by.into(),
            kel_seq: seq,
            kel_hash: chain.hash_at(seq).unwrap().clone(),
        };
        let payload = sign_payload(to.key(), body, NOW, by, signer).unwrap();
        let sealed = seal(enc, &address, &payload, &mut rand_core::OsRng).unwrap();
        SealedInboxMessage {
            id: id.into(),
            sealed,
            scid: chain.id().to_string(),
            key_id: "d1".into(),
            stored_at: NOW,
            expires: NOW + 3600,
        }
    }

    fn custody() -> SoftEncKey {
        SoftEncKey::new(X25519Secret::from([4u8; 32]))
    }

    #[test]
    fn take_from_opens_the_current_master_and_drops_what_does_not_hold() {
        let custody = custody();
        let enc = custody.enc();
        let chain = rotated_log(&custody);
        let (m0, m1, d1) = (key(1), key(2), key(3));
        let elsewhere = SoftEncKey::new(X25519Secret::from([5u8; 32])).enc();
        let messages = vec![
            // Master #0, signed by m0 while it was current: stale now.
            message("stale", &chain, &d1, 0, &m0, "m0", 0, &m0, &enc),
            // Says it is from m1, signed by another key.
            message("signer", &chain, &d1, 1, &m1, "m1", 1, &key(7), &enc),
            // For another device.
            message("to", &chain, &key(8), 1, &m1, "m1", 1, &m1, &enc),
            // Sealed to another enc key: it does not open here.
            message("sealed", &chain, &d1, 1, &m1, "m1", 1, &m1, &elsewhere),
            message("current", &chain, &d1, 1, &m1, "m1", 1, &m1, &enc),
            message("later", &chain, &d1, 0, &m0, "m0", 0, &m0, &enc),
        ];
        let taken = take_from(&custody, &messages, &d1.key(), &chain, NOW).unwrap();
        let master = taken.master.expect("the current master key");
        assert_eq!(master.message_id, "current");
        assert_eq!(master.index, 1);
        assert_eq!(master.did, chain.state().did);
        assert_eq!(master.secret.expose(), &m1.to_bytes());
        // The messages after the one taken are left for the next take.
        assert_eq!(taken.drop, ["stale", "signer", "to", "sealed"]);
    }

    #[test]
    fn take_from_with_no_current_master_drops_the_rest() {
        let custody = custody();
        let chain = rotated_log(&custody);
        let (m0, d1) = (key(1), key(3));
        let messages = vec![message(
            "stale",
            &chain,
            &d1,
            0,
            &m0,
            "m0",
            0,
            &m0,
            &custody.enc(),
        )];
        let taken = take_from(&custody, &messages, &d1.key(), &chain, NOW).unwrap();
        assert!(taken.master.is_none());
        assert_eq!(taken.drop, ["stale"]);
    }

    /// A keystore that is locked.
    struct Locked(Multikey);

    impl EncKey for Locked {
        fn enc(&self) -> Multikey {
            self.0.clone()
        }
        fn open(
            &self,
            _: &InboxAddress,
            _: &str,
        ) -> Result<InboxPayload, EncOpenError<InboxError>> {
            Err(EncOpenError::Unavailable("locked".into()))
        }
        fn unwrap_record_key(
            &self,
            _: &str,
            _: &str,
        ) -> Result<Secret<[u8; RECORD_KEY_LEN]>, EncOpenError<RecordError>> {
            Err(EncOpenError::Unavailable("locked".into()))
        }
    }

    #[test]
    fn take_from_with_the_key_unavailable_keeps_every_message() {
        let custody = custody();
        let chain = rotated_log(&custody);
        let (m0, m1, d1) = (key(1), key(2), key(3));
        let enc = custody.enc();
        let messages = vec![
            message("stale", &chain, &d1, 0, &m0, "m0", 0, &m0, &enc),
            message("current", &chain, &d1, 1, &m1, "m1", 1, &m1, &enc),
        ];
        let locked = Locked(enc);
        match take_from(&locked, &messages, &d1.key(), &chain, NOW) {
            Err(EncOpenError::Unavailable(_)) => {}
            Err(e) => panic!("expected unavailable, got {e}"),
            Ok(_) => panic!("expected unavailable, got an answer"),
        }
    }

    #[test]
    fn a_soft_enc_key_from_an_ed25519_key_names_its_x25519_form() {
        let device = key(3);
        assert_eq!(
            SoftEncKey::from_ed25519(&device).enc(),
            x25519_multikey_of(&x25519_secret_of_ed25519(&device))
        );
    }
}
