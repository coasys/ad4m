use integrity::LinkTypes;
use hdk::prelude::*;

pub(crate) fn err(reason: &str) -> WasmError {
    wasm_error!(WasmErrorInner::Host(String::from(reason)))
}

pub(crate) fn get_oldest_link(base: EntryHash, tag: Option<LinkTag>) -> ExternResult<Option<Link>> {
    let language_info = get_links(base, LinkTypes::LanguageLink, tag)?;

    // Find the oldest
    let oldest_info =
        language_info
            .into_iter()
            .fold(None, |oldest: Option<Link>, link| match oldest {
                Some(oldest) => {
                    if link.timestamp < oldest.timestamp {
                        Some(link)
                    } else {
                        Some(oldest)
                    }
                }
                None => Some(link),
            });
    return Ok(oldest_info);
}
