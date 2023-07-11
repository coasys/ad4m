mod test_inspect;

use direct_message_integrity::ad4m::*;
use direct_message_integrity::{
    EntryTypes, LinkTypes, Properties, PublicMessage, Recipient, Signal, StatusUpdate,
    StoredMessage,
};
use hdk::prelude::*;
use test_inspect::get_test_recipient;

#[hdk_extern]
fn init(_: ()) -> ExternResult<InitCallbackResult> {
    let mut functions = BTreeSet::new();
    functions.insert((
        ZomeName::from("direct-message"),
        "recv_remote_signal".into(),
    ));
    functions.insert((ZomeName::from("direct-message"), "get_status".into()));
    let granted_functions: GrantedFunctions = GrantedFunctions::Listed(functions);

    //Create open cap grant to allow agents to send signals of links to each other
    create_cap_grant(CapGrantEntry {
        tag: "".into(),
        // empty access converts to unrestricted
        access: ().into(),
        functions: granted_functions,
    })?;
    Ok(InitCallbackResult::Pass)
}

fn recipient() -> ExternResult<Recipient> {
    //debug!("RECIPIENT");
    if let Some(recipient) = get_test_recipient(())? {
        //debug!("TEST RECIPIENT");
        Ok(Recipient(recipient.get()))
    } else {
        //debug!("RECIPIENT from properties");
        let properties = Properties::try_from(dna_info()?.properties)
            .map_err(|err| wasm_error!(WasmErrorInner::Host(err.to_string())))?;
        let bytes = hex::decode(properties.recipient_hc_agent_pubkey).or_else(|_| {
            Err(wasm_error!(WasmErrorInner::Host(
                "Could not hex-decode property".to_string()
            )))
        })?;
        //debug!("RECIPIENT hex decoded");
        Ok(Recipient(AgentPubKey::from_raw_39(bytes).or_else(
            |_| {
                Err(wasm_error!(WasmErrorInner::Host(
                    "Could not decode property as AgentPubKey".to_string()
                )))
            },
        )?))
    }
}

//---------------------------------------------------------
//----Status-----------------------------------------------
//---------------------------------------------------------

#[hdk_extern]
pub fn set_status(new_status: PerspectiveExpression) -> ExternResult<()> {
    if Recipient(agent_info()?.agent_latest_pubkey) == recipient()? {
        create_entry(EntryTypes::StatusUpdate(StatusUpdate(new_status)))?;
        Ok(())
    } else {
        Err(wasm_error!(WasmErrorInner::Host(
            "Only recipient can set their status".to_string()
        )))
    }
}

#[hdk_extern]
pub fn get_status(_: ()) -> ExternResult<Option<PerspectiveExpression>> {
    //debug!("GET STATUS");
    if Recipient(agent_info()?.agent_latest_pubkey) == recipient()? {
        // If called on the recipient node
        // (either from local ad4m-executor or via remote_call)
        // we retrieve the latest status entry from source chain
        let mut filter = QueryFilter::new();
        filter.entry_type = Some(EntryType::App(AppEntryDef::new(
            0.into(),
            0.into(),
            EntryVisibility::Private,
        )));
        filter.include_entries = true;
        if let Some(element) = query(filter)?.pop() {
            let status = StatusUpdate::try_from(element)?;
            Ok(Some(status.into()))
        } else {
            Ok(None)
        }
    } else {
        // Otherwise proxy to recipient
        match call_remote(
            recipient()?.0,
            "direct-message",
            "get_status".into(),
            None,
            (),
        )? {
            ZomeCallResponse::Ok(extern_io) => Ok(extern_io
                .decode()
                .map_err(|err| wasm_error!(WasmErrorInner::Host(err.to_string())))?),
            ZomeCallResponse::Unauthorized(_, _, _, _, _) => Err(wasm_error!(
                WasmErrorInner::Host("Unauthorized error".to_string())
            )),
            ZomeCallResponse::NetworkError(error) => Err(wasm_error!(WasmErrorInner::Host(error))),
            ZomeCallResponse::CountersigningSession(session) => {
                Err(wasm_error!(WasmErrorInner::Host(session)))
            }
        }
    }
}

//---------------------------------------------------------
//----Messages---------------------------------------------
//---------------------------------------------------------

#[hdk_extern]
fn recv_remote_signal(signal: SerializedBytes) -> ExternResult<()> {
    debug!("RECEIVEING MESSAGE...");
    match PerspectiveExpression::try_from(signal) {
        Ok(message) => {
            let json = serde_json::to_string(&message).unwrap();
            emit_signal(
                &SerializedBytes::try_from(Signal { json })
                    .map_err(|err| wasm_error!(WasmErrorInner::Host(err.to_string())))?,
            )?;
            create_entry(EntryTypes::StoredMessage(StoredMessage(message)))?;
            Ok(())
        }
        Err(error) => {
            let error_message = format!(
                "Received signal that does not parse to PerspectiveExpression: {}",
                error
            );
            debug!("Error in recv_remote_sigal: {}", error_message);
            Err(wasm_error!(WasmErrorInner::Host(error_message)))
        }
    }
}

#[hdk_extern]
fn inbox(did_filter: Option<String>) -> ExternResult<Vec<PerspectiveExpression>> {
    //debug!("INBOX({:?})", did_filter);
    let mut filter = QueryFilter::new();
    filter.entry_type = Some(EntryType::App(AppEntryDef::new(
        1.into(),
        0.into(),
        EntryVisibility::Private,
    )));
    filter.include_entries = true;
    Ok(query(filter)?
        .into_iter()
        .map(|val| {
            val.entry()
                .to_app_option::<PerspectiveExpression>()
                .map_err(|err| wasm_error!(WasmErrorInner::Host(err.to_string())))?
                .ok_or(wasm_error!(WasmErrorInner::Host(
                    "Expected entry to contain data".to_string()
                )))
        })
        .filter_map(|m| match &did_filter {
            None => Some(m),
            Some(did) => match m.clone() {
                Ok(pers) => {
                    if &pers.author == did {
                        Some(m)
                    } else {
                        None
                    }
                }
                Err(_err) => None,
            },
        })
        .collect::<Result<Vec<_>, _>>()?)
}

#[hdk_extern]
pub fn send_p2p(message: PerspectiveExpression) -> ExternResult<()> {
    //debug!("SENDING MESSAGE...");
    remote_signal(
        SerializedBytes::try_from(message)
            .map_err(|err| wasm_error!(WasmErrorInner::Host(err.to_string())))?,
        vec![recipient()?.0],
    )
}

#[hdk_extern]
pub fn send_inbox(message: PerspectiveExpression) -> ExternResult<()> {
    //debug!("SEND INBOX");
    let entry = PublicMessage(message);
    let entry_hash = hash_entry(&entry)?;
    create_entry(EntryTypes::PublicMessage(entry))?;
    create_link(
        hash_entry(recipient()?)?,
        entry_hash,
        LinkTypes::Message,
        LinkTag::new(String::from("message")),
    )?;
    //debug!("Link created");
    Ok(())
}

#[hdk_extern]
pub fn fetch_inbox(_: ()) -> ExternResult<()> {
    let agent_address: EntryHash = hash_entry(recipient()?)?;
    //debug!("fetch_inbox");
    if Recipient(agent_info()?.agent_latest_pubkey) == recipient()? {
        //debug!("fetch_inbox agent");
        //debug!("agent_address: {}", agent_address);
        for link in get_links(
            agent_address,
            LinkTypes::Message,
            Some(LinkTag::new(String::from("message"))),
        )? {
            //debug!("fetch_inbox link");
            if let Some(message_entry) = get(
                link.target
                    .into_entry_hash()
                    .expect("Could not get entry hash"),
                GetOptions::latest(),
            )? {
                //debug!("fetch_inbox link got");
                let header_address = message_entry.action_address().clone();
                let public_message = PublicMessage::try_from(message_entry)?;
                let message: PerspectiveExpression = public_message.into();
                create_entry(EntryTypes::StoredMessage(StoredMessage(message)))?;
                delete_link(link.create_link_hash)?;
                delete_entry(header_address)?;
            } else {
                error!("Message linked in inbox not retrievable")
            }
        }
        Ok(())
    } else {
        Err(wasm_error!(WasmErrorInner::Guest(
            "Only recipient can fetch the inbox".to_string()
        )))
    }
}

/*
#[hdk_extern]
fn validate_delete_link(
    validate_delete_link: ValidateDeleteLinkData,
) -> ExternResult<ValidateLinkCallbackResult> {
    let delete_link = validate_delete_link.delete_link;
    let recipient = recipient()?;
    if delete_link.author == recipient {
        Ok(ValidateLinkCallbackResult::Valid)
    } else {
        Ok(ValidateLinkCallbackResult::Invalid("Only recipient is allowed to delete inbox links".to_string()),)
    }
}
 */
