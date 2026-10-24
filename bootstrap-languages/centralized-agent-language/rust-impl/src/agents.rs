//! Agent profiles on the shared store's agent records, in place of
//! the Node server at mock.ad4m.dev.
//!
//! - `create` publishes the running agent's profile under its `did:key`: an `AgentRecord`
//!   whose `sig` is the agent key's signature (through the host's `agentSign`) of the
//!   `coasys:link` bytes of `{profile, version, anchor: null}`, sent as
//!   `PUT /v1/agents/{did}` with no signed request.
//! - `get` reads `GET /v1/agents/{did}` and keeps the record only when its signature holds:
//!   for a `did:key`, by that key; for a `did:scid`, by a key that the log (read from the store
//!   and replayed here with the identity core, every event's signatures checked) lists at the
//!   anchor, valid and without the bot mark at the head. Any other record is ignored (`None`).

use ad4m_hash_chain::{decode_ed25519_multikey, decode_proof_value, encode_proof_value};
use ad4m_identity_core::{fold_json, verify_signed, KelChain, LINK_DOMAIN};
use ad4m_ldk::identity::{AgentRecord, IdentityErrorCode, LogPage};
use ad4m_ldk::prelude::{Expression, ExpressionProof, LanguageError, LanguageResult};
use serde_json::Value;

use crate::store::{segment, Http, Store};

/// The running agent, as the host gives it.
pub trait AgentKey {
    fn did(&self) -> LanguageResult<String>;
    /// Ed25519 signature of `bytes` by the agent's key.
    fn sign(&self, bytes: &[u8]) -> LanguageResult<Vec<u8>>;
    /// Now, in unix milliseconds.
    fn now_ms(&self) -> u64;
}

fn signature_holds(record: &AgentRecord, key: &str) -> bool {
    decode_proof_value(&record.sig)
        .is_some_and(|sig| verify_signed(LINK_DOMAIN, &record.signed_value(), key, &sig))
}

fn invalid(reason: impl Into<String>) -> LanguageError {
    IdentityErrorCode::Invalid {
        reason: reason.into(),
    }
    .into()
}

/// The agent profile from an Agent object, as `expressionCreate` receives it.
pub fn profile_of(mut content: Value, did: &str) -> LanguageResult<Value> {
    let has_links = content
        .get("perspective")
        .and_then(|p| p.get("links"))
        .is_some_and(Value::is_array);
    let named = content
        .get("did")
        .and_then(Value::as_str)
        .map(str::to_string);
    let Some(named) = named.filter(|_| has_links) else {
        return Err(LanguageError::invalid_input(
            "Content must be an Agent object",
        ));
    };
    if named != did {
        return Err(LanguageError::permission_denied(
            "Can't set Agent Expression for foreign DID - only for self",
        ));
    }
    let object = content
        .as_object_mut()
        .ok_or_else(|| LanguageError::invalid_input("Content must be an Agent object"))?;
    if object.get("directMessageLanguage").is_none() {
        object.insert("directMessageLanguage".into(), Value::Null);
    }
    if let Some(links) = object
        .get_mut("perspective")
        .and_then(|p| p.get_mut("links"))
        .and_then(Value::as_array_mut)
    {
        for link in links {
            if let Some(proof) = link.get_mut("proof").and_then(Value::as_object_mut) {
                proof.remove("valid");
                proof.remove("invalid");
            }
        }
    }
    Ok(content)
}

/// A `did:key` record of `agent` at `version`.
pub fn did_key_record(
    agent: &impl AgentKey,
    profile: Value,
    version: u64,
) -> LanguageResult<AgentRecord> {
    let did = agent.did()?;
    let mut record = AgentRecord {
        did: did.clone(),
        profile,
        version,
        signed_by: did,
        anchor: None,
        sig: String::new(),
    };
    let bytes = ad4m_identity_core::signing_bytes(LINK_DOMAIN, &record.signed_value())
        .map_err(|_| invalid("profile"))?;
    let sig: [u8; 64] = agent
        .sign(&bytes)?
        .try_into()
        .map_err(|_| LanguageError::internal("the agent's signature is not 64 bytes"))?;
    record.sig = encode_proof_value(&sig);
    Ok(record)
}

/// `expressionCreate`: publishes the agent's profile; answers its did.
pub async fn create<H: Http>(
    store: &Store<H>,
    agent: &impl AgentKey,
    content: Value,
) -> LanguageResult<String> {
    let did = agent.did()?;
    if !did.starts_with("did:key:") {
        return Err(invalid(
            "an agent with a log publishes through identityPutAgent",
        ));
    }
    let profile = profile_of(content, &did)?;
    let mut version = agent.now_ms();
    for attempt in 0..2 {
        let record = did_key_record(agent, profile.clone(), version)?;
        let body = serde_json::to_value(&record).map_err(|_| invalid("malformed"))?;
        let reply = store
            .send(
                "PUT",
                &format!("/v1/agents/{}", segment(&did)),
                crate::store::Send {
                    body: Some(body),
                    ..Default::default()
                },
            )
            .await?;
        if (200..300).contains(&reply.status) {
            return Ok(did);
        }
        // The store holds a newer version (another clock): publish just above it, once.
        let stored = (reply.status == 409)
            .then(|| serde_json::from_str::<Value>(&reply.body).ok())
            .flatten()
            .and_then(|b| b.get("version").and_then(Value::as_u64));
        match stored {
            Some(stored) if attempt == 0 => version = stored + 1,
            _ => return Err(crate::store::refusal_of(reply.status, &reply.body).into()),
        }
    }
    Err(IdentityErrorCode::Unavailable.into())
}

/// `expressionGet`: the agent's profile as an expression, or `None`.
pub async fn get<H: Http>(
    store: &Store<H>,
    did: &str,
    now_s: u64,
) -> LanguageResult<Option<Expression>> {
    let record: Option<AgentRecord> = store
        .read(&format!("/v1/agents/{}", segment(did)), Vec::new())
        .await?;
    let Some(record) = record.filter(|r| r.did == did) else {
        return Ok(None);
    };
    if !verified(store, &record, now_s).await? {
        return Ok(None);
    }
    Ok(Some(Expression {
        author: record.did.clone(),
        timestamp: iso_time_ms(record.version),
        data: record.profile.clone(),
        proof: ExpressionProof {
            signature: record.sig.clone(),
            key: record.signed_by.clone(),
            valid: None,
            invalid: None,
        },
    }))
}

async fn verified<H: Http>(
    store: &Store<H>,
    record: &AgentRecord,
    now_s: u64,
) -> LanguageResult<bool> {
    if let Some(key) = record.did.strip_prefix("did:key:") {
        return Ok(decode_ed25519_multikey(key).is_some()
            && record.signed_by == record.did
            && record.anchor.is_none()
            && signature_holds(record, key));
    }
    if !record.did.starts_with(ad4m_identity_core::DID_PREFIX) {
        return Ok(false);
    }
    let (Some(anchor), Some(key)) = (&record.anchor, record.signed_by.strip_prefix("did:key:"))
    else {
        return Ok(false);
    };
    if anchor.kel_did != record.did || !signature_holds(record, key) {
        return Ok(false);
    }
    let Some(chain) = read_log(store, &record.did).await? else {
        return Ok(false);
    };
    let state = chain.state();
    let head = chain.head().seq;
    Ok(state.key_id_of(&key.to_string()) == Some(&anchor.key_id)
        && chain.hash_at(anchor.kel_seq) == Some(&anchor.kel_hash)
        && state.bot_scope_at(&anchor.key_id, head).is_none()
        && state.key_valid_at_time(&anchor.key_id, head, now_s))
}

/// The whole log of `did` from the store, replayed (every event's signatures checked).
/// `None` when the store holds none or the events do not replay.
pub async fn read_log<H: Http>(store: &Store<H>, did: &str) -> LanguageResult<Option<KelChain>> {
    let mut events: Vec<Value> = Vec::new();
    loop {
        let Some(page): Option<LogPage> = store.log(did, events.len() as u64).await? else {
            return Ok(None);
        };
        let done = page.events.is_empty()
            || events.len() as u64 + page.events.len() as u64 > page.head.seq;
        events.extend(page.events);
        if done {
            break;
        }
    }
    Ok(fold_json(&events).ok())
}

/// `version` (unix milliseconds) as an ISO 8601 timestamp, `YYYY-MM-DDTHH:MM:SS.mmmZ`.
pub fn iso_time_ms(ms: u64) -> String {
    let secs = ms / 1000;
    let days = (secs / 86_400) as i64;
    let rem = secs % 86_400;
    // Civil date from days since 1970-01-01 (Howard Hinnant's algorithm).
    let z = days + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let day = doy - (153 * mp + 2) / 5 + 1;
    let month = if mp < 10 { mp + 3 } else { mp - 9 };
    let year = yoe + era * 400 + i64::from(month <= 2);
    format!(
        "{year:04}-{month:02}-{day:02}T{:02}:{:02}:{:02}.{:03}Z",
        rem / 3600,
        rem % 3600 / 60,
        rem % 60,
        ms % 1000
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn iso_time_matches_known_dates() {
        assert_eq!(iso_time_ms(0), "1970-01-01T00:00:00.000Z");
        assert_eq!(iso_time_ms(1_800_000_000_123), "2027-01-15T08:00:00.123Z");
        assert_eq!(iso_time_ms(951_782_400_000), "2000-02-29T00:00:00.000Z");
    }

    #[test]
    fn a_profile_drops_link_verdicts_and_refuses_a_foreign_did() {
        let content = json!({
            "did": "did:key:z6MkA",
            "perspective": { "links": [{ "proof": { "signature": "s", "valid": true, "invalid": false } }] }
        });
        let profile = profile_of(content.clone(), "did:key:z6MkA").unwrap();
        assert_eq!(
            profile["perspective"]["links"][0]["proof"],
            json!({ "signature": "s" })
        );
        assert_eq!(profile["directMessageLanguage"], Value::Null);
        assert!(profile_of(content, "did:key:z6MkB").is_err());
        assert!(profile_of(json!({ "did": "did:key:z6MkA" }), "did:key:z6MkA").is_err());
    }
}
