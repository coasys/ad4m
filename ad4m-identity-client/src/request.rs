//! What every flow that writes for the master key needs: the log folded here, the store the
//! executor accepted its head from, the master key's position at the head, and a store request
//! signed by the master key there.

use ad4m_hash_chain::{canonical_json, encode_proof_value, hash_bytes};
use ad4m_identity_core::link::SignedBy;
use ad4m_identity_core::{signing_bytes, KelChain, REQUEST_DOMAIN};
use ad4m_ldk::identity::{RequestBody, SignedRequest};
use serde::Serialize;

use crate::executor::{ExecutorRpc, ViaExecutor};
use crate::vault::MasterSigner;
use crate::{fold, ClientError, Random};

/// How long a signed store request stays valid (the store takes at most 300 s).
pub(crate) const REQUEST_TTL_S: u64 = 60;

/// `len` random bytes, base58btc.
pub(crate) fn b58_random(random: &dyn Random, len: usize) -> String {
    let mut bytes = vec![0u8; len];
    random.fill(&mut bytes);
    bs58::encode(bytes).into_string()
}

/// The identity's log through the executor, refreshed and folded here.
pub(crate) async fn chain_of<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
) -> Result<KelChain, ClientError> {
    fold(&executor.events(did).await?)
}

/// The `did:key` of the store whose head the executor accepted for `did`: a signed request is
/// for that store only.
pub(crate) async fn store_of<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
) -> Result<String, ClientError> {
    executor
        .log(did, 0, false)
        .await?
        .head
        .map(|head| head.store)
        .ok_or_else(|| {
            ClientError::Protocol(format!(
                "the executor accepted no head from the identity store for {did}"
            ))
        })
}

/// The scid of `did`, a `did:scid:coasys:1`.
pub(crate) fn scid_of(did: &str) -> Result<String, ClientError> {
    ad4m_identity_core::scid_of(did)
        .map(str::to_string)
        .ok_or_else(|| ClientError::Protocol(format!("{did} is not a did:scid:coasys:1")))
}

/// `signer`'s position at `chain`'s head; refused unless it is the current master key.
pub(crate) fn master_at_head(
    chain: &KelChain,
    signer: &dyn MasterSigner,
) -> Result<SignedBy, ClientError> {
    let master = chain
        .state()
        .current_master()
        .ok_or_else(|| ClientError::Protocol("the identity has ended".into()))?;
    if master.sign != signer.key() {
        return Err(ClientError::Protocol(
            "the vault's master key is not the identity's current master".into(),
        ));
    }
    let head = chain.head();
    Ok(SignedBy {
        key_id: master.id.clone(),
        kel_seq: head.seq,
        kel_hash: head.hash.clone(),
    })
}

/// The bytes a store request's `bodyHash` covers: the canonical JSON of the body.
pub(crate) fn body_bytes<T: Serialize>(body: &T) -> Result<Vec<u8>, ClientError> {
    let value = serde_json::to_value(body).map_err(|e| ClientError::Protocol(e.to_string()))?;
    canonical_json(&value)
        .map(String::into_bytes)
        .map_err(|e| ClientError::Protocol(e.to_string()))
}

/// Where a master key signs from: the identity, the store its requests name, and the master
/// key's position at the head.
pub(crate) struct MasterPosition<'a> {
    pub did: &'a str,
    pub store: &'a str,
    pub by: &'a SignedBy,
}

/// A store request for `method path` with `body` (empty for a read or a deletion), signed by
/// `signer` at `at`, valid from `now` for [`REQUEST_TTL_S`].
pub(crate) fn master_request(
    at: &MasterPosition<'_>,
    signer: &dyn MasterSigner,
    method: &str,
    path: &str,
    body: &[u8],
    random: &dyn Random,
    now: u64,
) -> Result<SignedRequest, ClientError> {
    let req = RequestBody {
        did: at.did.to_string(),
        store: at.store.to_string(),
        key_id: at.by.key_id.clone(),
        kel_seq: at.by.kel_seq,
        kel_hash: at.by.kel_hash.clone(),
        method: method.into(),
        path: path.into(),
        body_hash: hash_bytes(body),
        nonce: b58_random(random, 16),
        expires: now + REQUEST_TTL_S,
    };
    let bytes =
        signing_bytes(REQUEST_DOMAIN, &req).map_err(|e| ClientError::Protocol(e.to_string()))?;
    let sig = encode_proof_value(&signer.sign(&bytes));
    Ok(SignedRequest { req, sig })
}
