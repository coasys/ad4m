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

    // Holochain 0.7 removed the network fallback from GetStrategy::Local
    // (see holochain_integrity_types::GetStrategy docs). Agent profile
    // lookups target links authored by ANOTHER agent — they only exist
    // on our local shard once DHT gossip has reached us. Use Network so
    // multi-agent scenarios (multi-user tests, fresh nodes joining a
    // running neighbourhood, cross-node profile fetches) resolve reliably
    // instead of returning None until gossip catches up.
    let profile_info = get_links(query, GetStrategy::Network)?;

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
