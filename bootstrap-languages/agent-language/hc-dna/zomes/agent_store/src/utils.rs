use agent_store_integrity::LinkTypes;
use hdk::prelude::*;

pub(crate) fn err(reason: &str) -> WasmError {
    wasm_error!(WasmErrorInner::Host(String::from(reason)))
}

pub(crate) fn get_latest_link(base: EntryHash, tag: Option<LinkTag>) -> ExternResult<Option<Link>> {
    let mut query = LinkQuery::try_new(
        base,
        LinkTypes::ProfileLink,
    )?;
    
    if let Some(t) = tag {
        query = query.tag_prefix(t);
    }

    let profile_info = get_links(query, GetStrategy::Local)?;

    // Find the latest
    let latest_info =
        profile_info
            .into_iter()
            .fold(None, |latest: Option<Link>, link| match latest {
                Some(latest) => {
                    if link.timestamp > latest.timestamp {
                        Some(link)
                    } else {
                        Some(latest)
                    }
                }
                None => Some(link),
            });
    return Ok(latest_info);
}
