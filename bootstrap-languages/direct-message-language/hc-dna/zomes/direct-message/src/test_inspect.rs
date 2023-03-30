use direct_message_integrity::{EntryTypes, Recipient};
use hdk::prelude::*;

#[hdk_extern]
pub fn set_test_recipient(recipient: AgentPubKey) -> ExternResult<()> {
    create_entry(EntryTypes::Recipient(Recipient(recipient)))?;
    Ok(())
}

#[hdk_extern]
pub fn get_test_recipient(_: ()) -> ExternResult<Option<Recipient>> {
    let app_entry = AppEntryDef::new(3.into(), 0.into(), EntryVisibility::Private);
    let filter = QueryFilter::new()
        .entry_type(EntryType::App(app_entry))
        .include_entries(true);
    if let Some(element) = query(filter)?.pop() {
        let recipient = Recipient::try_from(element)?;
        return Ok(Some(recipient));
    }
    return Ok(None);
}
