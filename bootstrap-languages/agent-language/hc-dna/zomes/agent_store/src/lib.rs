use agent_store_integrity::{
    AddAuthorisedKeyInput, AgentExpression, AuthorisedKey, Did, EntryTypes,
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
fn extract_key_from_did(did: &str) -> Option<String> {
    if did.starts_with("did:key:") {
        Some(did.trim_start_matches("did:key:").to_string())
    } else {
        None
    }
}

#[hdk_extern]
pub fn create_agent_expression(mut agent_expression: AgentExpression) -> ExternResult<()> {
    // Auto-populate authorised_keys with the DID root key if empty (migration path)
    if agent_expression.data.authorised_keys.is_empty() {
        if let Some(root_key) = extract_key_from_did(&agent_expression.author) {
            let now = chrono::Utc::now();
            agent_expression.data.authorised_keys.push(AuthorisedKey {
                key: root_key.clone(),
                name: "Root Key".to_string(),
                added_at: now,
                added_by: agent_expression.author.clone(),
                proof: KeyAuthorisation {
                    authorising_key: root_key,
                    signature: "self".to_string(),
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
pub fn add_authorised_key(input: AddAuthorisedKeyInput) -> ExternResult<AgentExpression> {
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

    let new_expression = AgentExpression {
        author: current.author.clone(),
        timestamp: now,
        data: new_data,
        proof: current.proof.clone(),
    };

    // Store updated expression
    let did = EntryTypes::Did(Did(current.author.clone()));
    let did_hash = hash_entry(&did)?;
    let entry = EntryTypes::AgentExpression(new_expression.clone());
    let entry_hash = hash_entry(&entry)?;
    create_entry(&entry)?;
    create_link(
        did_hash,
        entry_hash,
        LinkTypes::ProfileLink,
        LinkTag::new("profile"),
    )?;

    Ok(new_expression)
}

#[hdk_extern]
pub fn revoke_key(input: RevokeKeyInput) -> ExternResult<AgentExpression> {
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

    let now = chrono::Utc::now();
    let revocation = KeyRevocation {
        revoked_key: input.key.clone(),
        revoked_at: now,
        revoked_by: input.did.clone(),
        signature: input.signature,
        reason: input.reason,
    };

    let mut new_data = current.data.clone();
    new_data.authorised_keys.retain(|k| k.key != input.key);
    new_data.revoked_keys.push(revocation);

    let new_expression = AgentExpression {
        author: current.author.clone(),
        timestamp: now,
        data: new_data,
        proof: current.proof.clone(),
    };

    let did = EntryTypes::Did(Did(current.author.clone()));
    let did_hash = hash_entry(&did)?;
    let entry = EntryTypes::AgentExpression(new_expression.clone());
    let entry_hash = hash_entry(&entry)?;
    create_entry(&entry)?;
    create_link(
        did_hash,
        entry_hash,
        LinkTypes::ProfileLink,
        LinkTag::new("profile"),
    )?;

    Ok(new_expression)
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
