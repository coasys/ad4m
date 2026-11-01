use crate::types::Expression;
use chrono::SecondsFormat;
use chrono::{DateTime, Utc};
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use did_key::{CoreSign, PatchedKeyPair};
use log::error;
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::str::FromStr;

pub fn verify_string_signed_by_did(
    did: &str,
    data: &str,
    signed_data: &str,
) -> Result<bool, AnyError> {
    let sig_bytes = hex::decode(signed_data)?;
    let message = hash_message(&data.to_string());
    Ok(inner_verify(did, &message, &sig_bytes))
}

/// The one check of a signed expression (a link above all): whether it counts.
///
/// - No `proof.anchor`: the signature under the author's `did:key`, over the old signed bytes
///   ([`hash_data_and_timestamp`]); then, when a log lists the key (a key a log adopted
///   after it signed), the log's rules at the seq that added the key.
/// - With `proof.anchor`: the signature under the author's `did:key`, over
///   `coasys:link` `{data, timestamp, anchor}` ([`anchored_signing_bytes`]); the log that lists
///   the key as a device key, at the anchor.
///
/// The log is read from the key map ([`crate::identity::check_signed`]), so the answer needs no
/// `await`. `Ok(true)` only for [`LinkVerdict::Valid`](crate::identity::verdict::LinkVerdict).
/// `Err` only for a malformed proof envelope (a signature that is not hex, a timestamp that
/// does not parse).
pub fn verify<T: Serialize>(expr: &Expression<T>) -> Result<bool, AnyError> {
    // The envelope first: a malformed one is an error, not a verdict.
    hex::decode(&expr.proof.signature)?;
    if expr.proof.anchor.is_none() {
        parse_timestamp(&expr.timestamp)?;
    }
    Ok(crate::identity::check_signed(expr).verdict.counts())
}

/// Whether `expr`'s signature holds under its author's `did:key`, over the bytes its proof
/// names: the old signed bytes without an anchor, `coasys:link` `{data, timestamp, anchor}`
/// with one. Reads no log.
pub fn signature_holds<T: Serialize>(expr: &Expression<T>) -> bool {
    let Ok(sig_bytes) = hex::decode(&expr.proof.signature) else {
        return false;
    };
    match expr.proof.anchor {
        None => verify_by(expr, &expr.author).unwrap_or(false),
        Some(anchor) => {
            let Some(key) = expr.author.strip_prefix("did:key:") else {
                return false;
            };
            let value = AnchoredLink {
                data: &expr.data,
                timestamp: &expr.timestamp,
                anchor,
            };
            ad4m_identity_core::verify_signed(
                ad4m_identity_core::LINK_DOMAIN,
                &value,
                key,
                &sig_bytes,
            )
        }
    }
}

/// What an anchored link signs under `coasys:link`: its data, its timestamp as it carries it,
/// and the anchor.
#[derive(Serialize)]
struct AnchoredLink<'a, T: Serialize> {
    data: &'a T,
    timestamp: &'a str,
    anchor: u64,
}

/// The bytes an anchored expression signs: `coasys:link` over `{data, timestamp, anchor}`.
pub(crate) fn anchored_signing_bytes<T: Serialize>(
    data: &T,
    timestamp: &str,
    anchor: u64,
) -> Result<Vec<u8>, AnyError> {
    ad4m_identity_core::signing_bytes(
        ad4m_identity_core::LINK_DOMAIN,
        &AnchoredLink {
            data,
            timestamp,
            anchor,
        },
    )
    .map_err(|e| anyhow!("cannot build the signed bytes of an anchored expression: {e}"))
}

fn parse_timestamp(timestamp: &str) -> Result<DateTime<Utc>, AnyError> {
    DateTime::<Utc>::from_str(timestamp).map_err(|e| {
        anyhow!(
            "Failed to parse timestamp when trying to verify signature: {}",
            e
        )
    })
}

/// `expr`'s signature under the `did:key` `did`, over the old signed bytes (no anchor): for an
/// author that is not a `did:key` (a `did:scid` person), once the caller has established that
/// `did` signs for it.
pub fn verify_by<T: Serialize>(expr: &Expression<T>, did: &str) -> Result<bool, AnyError> {
    let sig_bytes = hex::decode(&expr.proof.signature)?;
    let timestamp = parse_timestamp(&expr.timestamp)?;
    let message = hash_data_and_timestamp(&expr.data, &timestamp);
    let result = inner_verify(did, &message, &sig_bytes);
    Ok(result)
}

/// Verify `expr`, collapsing a verification *error* to "not verified".
///
/// `proof.valid` answers exactly one question — does this signature verify? —
/// so every way of failing to establish a signature has to answer it `false`.
/// [`verify`] only returns `Err` for a malformed proof envelope: a signature
/// that is not hex, or a timestamp that does not parse. Neither of those is a
/// signature that verifies. A wrong signature or an unparseable DID does not
/// even reach here — [`inner_verify`] already answers those `Ok(false)`.
///
/// This exists because the three call sites used to spell it
/// `verify(&expr).unwrap_or(false)`, which reaches the same verdict but throws
/// the error away, leaving a malformed envelope indistinguishable from a forged
/// one in the logs. The verdict is unchanged; it is now observable. Making the
/// error a *third* verdict instead would push the "unevaluated" ambiguity that
/// #1046 removed from storage back up into the producer.
///
/// `context` names the caller in the log line.
pub fn verify_or_false<T: Serialize>(expr: &Expression<T>, context: &str) -> bool {
    match verify(expr) {
        Ok(valid) => valid,
        Err(e) => {
            log::warn!(
                "{}: malformed proof on expression by {} ({}) — treating as not verified",
                context,
                expr.author,
                e
            );
            false
        }
    }
}

pub(crate) fn hash_data_and_timestamp<T: Serialize>(
    data: &T,
    timestamp: &DateTime<Utc>,
) -> Vec<u8> {
    let mut hasher = Sha256::new();

    // Serialize and hash the data directly.
    let serialized_data = serde_json::to_vec(data).expect("Failed to serialize data");
    hasher.update(&serialized_data);

    // Serialize and hash the timestamp.
    let timestamp_str = timestamp.to_rfc3339_opts(SecondsFormat::Millis, true);
    hasher.update(timestamp_str.as_bytes());

    // Finalize the hash and return the result.
    hasher.finalize().as_slice().into()
}

pub(super) fn hash_message(message: &String) -> Vec<u8> {
    let mut hasher = Sha256::new();
    hasher.update(message.as_bytes());
    hasher.finalize().as_slice().into()
}

fn inner_verify(did: &str, message: &[u8], signature: &[u8]) -> bool {
    if let Ok(key_pair) = PatchedKeyPair::try_from(did) {
        match key_pair.verify(message, signature) {
            Ok(_) => true,
            Err(_) => {
                //debug!("Signature verification failed: {:?}", e);
                false
            }
        }
    } else {
        error!("Failed to parse DID as key method: {}", did);
        false
    }
}

/// A second signing identity for tests: a real `did:key` keypair that signs
/// the way [`crate::agent::create_signed_expression`] signs for the local
/// agent, so links it authors verify on the receiving side
/// (`proof.valid == Some(true)`).
///
/// It lives next to [`verify`] deliberately. The flow engine counts a vote
/// only when the stored signature verdict is valid, so a test can no longer
/// stand in for a second agent by writing a link with a made-up signature —
/// it has to hold a key.
#[cfg(test)]
pub struct TestSigner {
    keypair: PatchedKeyPair,
    /// `did:key:…` — the author of everything this signer signs.
    pub did: String,
    /// The verification-method id that goes into `proof.key`.
    pub key_id: String,
}

#[cfg(test)]
impl TestSigner {
    pub fn generate() -> Self {
        use did_key::{DIDCore, Ed25519KeyPair};
        let keypair = did_key::generate::<Ed25519KeyPair>(None);
        // Only the ids are read; the wallet's public config keeps this from
        // being the one place that still builds documents with secrets.
        let document = keypair.get_did_document(crate::wallet::DID_DOCUMENT_CONFIG);
        TestSigner {
            did: document.id.clone(),
            key_id: document.verification_method[0].id.clone(),
            keypair,
        }
    }

    /// Sign `data` now, producing the same `Expression` shape the wallet path
    /// produces.
    pub fn sign<T: Serialize>(&self, data: T) -> Expression<T> {
        self.sign_at(data, Utc::now())
    }

    /// Sign `data` as of `timestamp` — for fixtures that need a *stated*
    /// instant (role grants and tombstones are dated material, and a reader
    /// that re-verifies signatures needs the signed timestamp to be the one
    /// the fixture claims).
    pub fn sign_at<T: Serialize>(&self, data: T, timestamp: DateTime<Utc>) -> Expression<T> {
        let signature = hex::encode(
            self.keypair
                .sign(&hash_data_and_timestamp(&data, &timestamp)),
        );
        Expression {
            author: self.did.clone(),
            timestamp: timestamp.to_rfc3339_opts(SecondsFormat::Millis, true),
            data,
            proof: crate::types::ExpressionProof {
                key: self.key_id.clone(),
                signature,
                anchor: None,
            },
        }
    }

    /// Sign `data` now, anchored at seq `anchor` of the log that lists this signer's key, as
    /// the executor signs a link of an agent bound to an identity.
    pub fn sign_anchored<T: Serialize>(&self, data: T, anchor: u64) -> Expression<T> {
        let timestamp = Utc::now().to_rfc3339_opts(SecondsFormat::Millis, true);
        let bytes = anchored_signing_bytes(&data, &timestamp, anchor).expect("signed bytes");
        Expression {
            author: self.did.clone(),
            timestamp,
            data,
            proof: crate::types::ExpressionProof {
                key: self.key_id.clone(),
                signature: hex::encode(self.keypair.sign(&bytes)),
                anchor: Some(anchor),
            },
        }
    }

    /// This signer's signing multikey (the `did:key` without its prefix).
    pub fn multikey(&self) -> String {
        self.did.trim_start_matches("did:key:").to_string()
    }
}
