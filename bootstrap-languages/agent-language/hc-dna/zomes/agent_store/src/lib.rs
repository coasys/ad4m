use agent_store_integrity::{
    AddAuthorisedKeyInput, AgentExpression, AgentExpressionData, AuthorisedKey, Did, EntryTypes,
    IsKeyValidInput, KeyAuthorisation, KeyRevocation, LinkTypes, RevokeKeyInput,
};
use hdk::prelude::*;

mod utils;

use utils::{err, get_latest_link};

#[hdk_extern]
fn init(_: ()) -> ExternResult<InitCallbackResult> {
    Ok(InitCallbackResult::Pass)
}

/// Extract the key portion from a did:key: URI
fn extract_key_from_did(did: &str) -> Result<String, WasmError> {
    if did.starts_with("did:key:") {
        Ok(did.trim_start_matches("did:key:").to_string())
    } else {
        Err(err(&format!("Cannot extract key from non did:key DID: {}", did)))
    }
}

/// Decode a multibase/multicodec Ed25519 public key string to raw 32 bytes.
///
/// Expected format: base58btc-encoded (prefix 'z') with Ed25519 multicodec
/// prefix bytes `0xed 0x01` followed by 32 bytes of public key.
fn decode_ed25519_pubkey(key_str: &str) -> ExternResult<[u8; 32]> {
    // Strip 'z' multibase prefix (base58btc)
    let without_prefix = key_str.strip_prefix('z').unwrap_or(key_str);
    let decoded = bs58::decode(without_prefix)
        .into_vec()
        .map_err(|e| err(&format!("Failed to base58-decode key: {}", e)))?;

    // Ed25519 multicodec: 0xed 0x01 + 32 bytes = 34 bytes
    if decoded.len() == 34 && decoded[0] == 0xed && decoded[1] == 0x01 {
        let mut key = [0u8; 32];
        key.copy_from_slice(&decoded[2..]);
        Ok(key)
    } else if decoded.len() == 32 {
        // Raw 32-byte key without multicodec prefix
        let mut key = [0u8; 32];
        key.copy_from_slice(&decoded);
        Ok(key)
    } else {
        Err(err(&format!(
            "Invalid Ed25519 key: expected 34 bytes (multicodec) or 32 bytes (raw), got {}",
            decoded.len()
        )))
    }
}

/// Decode a hex-encoded Ed25519 signature to a Signature (64 bytes).
fn decode_signature(sig_hex: &str) -> ExternResult<Signature> {
    let bytes = hex_decode(sig_hex)
        .map_err(|e| err(&format!("Failed to decode signature hex: {}", e)))?;
    if bytes.len() != 64 {
        return Err(err(&format!(
            "Invalid signature length: expected 64 bytes, got {}",
            bytes.len()
        )));
    }
    let mut sig = [0u8; 64];
    sig.copy_from_slice(&bytes);
    Ok(Signature(sig))
}

/// Simple hex decoding (no external crate needed)
fn hex_decode(hex: &str) -> Result<Vec<u8>, String> {
    if hex.len() % 2 != 0 {
        return Err("Hex string has odd length".to_string());
    }
    (0..hex.len())
        .step_by(2)
        .map(|i| {
            u8::from_str_radix(&hex[i..i + 2], 16)
                .map_err(|e| format!("Invalid hex at position {}: {}", i, e))
        })
        .collect()
}

/// Verify an Ed25519 signature using Holochain's built-in verify_signature_raw.
///
/// ## Signature message format
///
/// The signed message is the UTF-8 bytes of the concatenation:
///   `<key> + <did> + <timestamp>`
///
/// Where:
/// - `key` is the key being added or revoked (the multibase-encoded string)
/// - `did` is the DID string (e.g., `did:key:z...`)
/// - `timestamp` is the ISO 8601 timestamp string (e.g., `2024-01-01T00:00:00Z`)
///
/// Example: `"zABC123did:key:zXYZ7892024-01-01T00:00:00Z"`
fn verify_key_signature(
    signing_key_str: &str,
    signature_hex: &str,
    subject_key: &str,
    did: &str,
    timestamp: &str,
) -> ExternResult<bool> {
    // Skip verification for self-signed root keys
    if signature_hex == "self" {
        return Ok(true);
    }

    let pubkey_bytes = decode_ed25519_pubkey(signing_key_str)?;
    let agent_pubkey = AgentPubKey::from_raw_32(pubkey_bytes.to_vec());
    let signature = decode_signature(signature_hex)?;

    // Message = subject_key + did + timestamp (UTF-8 bytes)
    let message = format!("{}{}{}", subject_key, did, timestamp);

    verify_signature_raw(agent_pubkey, signature, message.into_bytes())
}

#[hdk_extern]
pub fn create_agent_expression(mut agent_expression: AgentExpression) -> ExternResult<()> {
    // Auto-populate authorised_keys with the DID root key if empty (migration path)
    if agent_expression.data.authorised_keys.is_empty() {
        if let Ok(root_key) = extract_key_from_did(&agent_expression.author) {
            let now = chrono::Utc::now();
            agent_expression.data.authorised_keys.push(AuthorisedKey {
                key: root_key.clone(),
                name: "Root Key".to_string(),
                added_at: now,
                added_by: agent_expression.author.clone(),
                proof: KeyAuthorisation {
                    authorising_key: root_key,
                    signature: "self".to_string(),
                    timestamp: now.to_rfc3339(),
                },
            });
        }
    }

    let did = EntryTypes::Did(Did(agent_expression.author.clone()));
    let did_hash = hash_entry(&did)?;

    create_entry(&did)?;

    let agent_expression = EntryTypes::AgentExpression(agent_expression);
    let agent_expression_hash = hash_entry(&agent_expression)?;
    create_entry(&agent_expression)?;

    //Link profile entry to did
    create_link(
        did_hash,
        agent_expression_hash,
        LinkTypes::ProfileLink,
        LinkTag::new("profile"),
    )?;

    Ok(())
}

/// Helper to get current agent expression for a DID
fn get_current_expression(did: &str) -> ExternResult<Option<AgentExpression>> {
    let did_entry = Did(did.to_string());
    get_agent_expression(did_entry)
}

#[hdk_extern]
pub fn add_authorised_key(input: AddAuthorisedKeyInput) -> ExternResult<AgentExpressionData> {
    let current = get_current_expression(&input.did)?
        .ok_or_else(|| err("Agent expression not found"))?;

    // Check that the authorising key is in the current authorised_keys
    let authorising_key_valid = current
        .data
        .authorised_keys
        .iter()
        .any(|k| k.key == input.proof.authorising_key);

    if !authorising_key_valid {
        return Err(err("Authorising key is not in the current authorised keys"));
    }

    // Check key is not already revoked
    let is_revoked = current
        .data
        .revoked_keys
        .iter()
        .any(|r| r.revoked_key == input.proof.authorising_key);

    if is_revoked {
        return Err(err("Authorising key has been revoked"));
    }

    // Verify the signature over (new_key + did + timestamp)
    let sig_valid = verify_key_signature(
        &input.proof.authorising_key,
        &input.proof.signature,
        &input.key,
        &input.did,
        &input.proof.timestamp,
    )?;
    if !sig_valid {
        return Err(err("Invalid signature: Ed25519 verification failed for add_authorised_key proof"));
    }

    // Check the new key isn't already authorised
    let already_exists = current.data.authorised_keys.iter().any(|k| k.key == input.key);
    if already_exists {
        return Err(err("Key is already authorised"));
    }

    let now = chrono::Utc::now();
    let new_key = AuthorisedKey {
        key: input.key,
        name: input.name,
        added_at: now,
        added_by: input.did.clone(),
        proof: input.proof,
    };

    let mut new_data = current.data.clone();
    new_data.authorised_keys.push(new_key);

    // Return updated data — the adapter is responsible for signing and storing
    Ok(new_data)
}

#[hdk_extern]
pub fn revoke_key(input: RevokeKeyInput) -> ExternResult<AgentExpressionData> {
    let current = get_current_expression(&input.did)?
        .ok_or_else(|| err("Agent expression not found"))?;

    // Check the key exists in authorised_keys
    let key_exists = current.data.authorised_keys.iter().any(|k| k.key == input.key);
    if !key_exists {
        return Err(err("Key not found in authorised keys"));
    }

    // Check not already revoked
    let already_revoked = current.data.revoked_keys.iter().any(|r| r.revoked_key == input.key);
    if already_revoked {
        return Err(err("Key is already revoked"));
    }

    // Check that the revoking key is currently authorised
    let revoker_valid = current
        .data
        .authorised_keys
        .iter()
        .any(|k| k.key == input.revoked_by_key);
    if !revoker_valid {
        return Err(err("Revoking key is not in the current authorised keys"));
    }

    // Check revoking key is not itself revoked
    let revoker_revoked = current
        .data
        .revoked_keys
        .iter()
        .any(|r| r.revoked_key == input.revoked_by_key);
    if revoker_revoked {
        return Err(err("Revoking key has been revoked"));
    }

    // Verify the signature over (revoked_key + did + timestamp)
    let sig_valid = verify_key_signature(
        &input.revoked_by_key,
        &input.signature,
        &input.key,
        &input.did,
        &input.timestamp,
    )?;
    if !sig_valid {
        return Err(err("Invalid signature: Ed25519 verification failed for revoke_key"));
    }

    let now = chrono::Utc::now();
    let revocation = KeyRevocation {
        revoked_key: input.key.clone(),
        revoked_at: now,
        revoked_by: input.did.clone(),
        revoked_by_key: input.revoked_by_key,
        signature: input.signature,
        reason: input.reason,
    };

    let mut new_data = current.data.clone();
    new_data.authorised_keys.retain(|k| k.key != input.key);
    new_data.revoked_keys.push(revocation);

    // Return updated data — the adapter is responsible for signing and storing
    Ok(new_data)
}

#[hdk_extern]
pub fn is_key_valid(input: IsKeyValidInput) -> ExternResult<bool> {
    let current = match get_current_expression(&input.did)? {
        Some(expr) => expr,
        None => return Ok(false),
    };

    let in_authorised = current.data.authorised_keys.iter().any(|k| k.key == input.key);
    let in_revoked = current.data.revoked_keys.iter().any(|r| r.revoked_key == input.key);

    Ok(in_authorised && !in_revoked)
}

#[hdk_extern]
pub fn get_agent_expression(did: Did) -> ExternResult<Option<AgentExpression>> {
    let expression_links = get_latest_link(
        hash_entry(did)?,
        Some(LinkTag::new("profile")),
    )
    .map_err(|error| err(format!("{}", error).as_ref()))?;

    match expression_links {
        Some(link) => {
            match get(
                link.target
                    .into_entry_hash()
                    .expect("could not get action hash"),
                GetOptions::default(),
            )
            .map_err(|error| err(format!("{}", error).as_ref()))?
            {
                Some(elem) => {
                    let exp_data: AgentExpression = elem
                        .entry()
                        .to_app_option()
                        .map_err(|sb_err| err(&format!("{}", sb_err)))?
                        .ok_or(err(
                            "Could not deserialize link expression data into Profile type",
                        ))?;
                    Ok(Some(exp_data))
                }
                None => Ok(None),
            }
        }
        None => Ok(None),
    }
}

//Validation logic

//Validate did entry
//Validate did syntax
//Validate integrity of DID

//Validate did document entry
//TODO: resolve did subject and validate that did documents are the same.
//Validate that signed_agent inside did document is the same agent who is trying to post this did document. This is the validation stage that allows for the "claiming/pairing" of a did on this DHT.
//Note that this signed_agent validation doesnt give us anything that isnt already handled by holochain validation logic. It is however useful if we can do did resolving. So we can keep it here ready for the future.
//In the case that we can resolve did's since we can trust a given did subject document pair we can deduce the the agent making the post is the same agent who authored the first claim of this DID on some other system.

//Validate create profile entry
//Validate length/size of entry?
//Perhaps validate that agent does not have more than N profiles already post'd as to reduce possibility of someone spamming network?

//Validate update profile entry
//Validate length/size of entry
//Validate that agent creating update is the same agent who made the first profile entry
//Actually possible here that we could allow multiple agents to update profile entry if the did document had multiple signed_agent fields where each signed_agent was allowed editable agent
//Editing from multiple agents would require that profile has links to did document so that we can check this signed agents field

//Validate links

//did subject -> did document:
//Validate that author of subject and document are the same. Since creating a did document entry requires the validation of signed_agent field we can be sure that author of did document is the rightful owner of this did.
//Validate that subject inside did document is the same as the did subject as source for this link.

//did subject -> profile
//Validate that there is a link between did subject -> did document. This gives us the verification that creator of did subject is same agent as creator of did document.
//Validate that agent posting profile is the same agent who created the did subject.
