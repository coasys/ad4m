use chrono::{DateTime, NaiveDateTime, Utc};
use hdk::prelude::*;
use integrity::{EntryTypes, LanguageChunk, LanguageExpression, LinkTypes, LanguageAddress};
mod utils;
use utils::{err, get_oldest_link};

#[hdk_extern]
fn init(_: ()) -> ExternResult<InitCallbackResult> {
    Ok(InitCallbackResult::Pass)
}

pub fn get_now() -> DateTime<Utc> {
    match sys_time() {
        Ok(time) => {
            let now = time.as_seconds_and_nanos();
            let out = DateTime::<Utc>::from_utc(
                NaiveDateTime::from_timestamp_opt(now.0, now.1).unwrap(),
                Utc,
            );
            out
        }
        Err(_err) => Utc::now(),
    }
}

#[hdk_extern]
pub fn store_language_expression(expression: LanguageExpression) -> ExternResult<()> {
    let address = EntryTypes::LanguageAddress(LanguageAddress(expression.data.address.clone()));
    let address_hash = hash_entry(&address)?;

    let found_expression = get_language_expression(LanguageAddress(expression.data.address.clone()));

    match found_expression {
        Ok(Some(expression)) => {
            Err(err(format!("An language with same address was found {:?}", expression).as_ref()))
        }
        Ok(None) => {
            let language_expression = EntryTypes::LanguageExpression(expression);
            let language_expression_hash = hash_entry(&language_expression)?;
            create_entry(&language_expression)?;
        
            //Link profile entry to did
            create_link(
                address_hash,
                language_expression_hash,
                LinkTypes::LanguageLink,
                LinkTag::from("".as_bytes().to_owned()),
            )?;

            Ok(())
        }
        Err(error) => {
            Err(err(format!("Error: {}", error).as_ref()))
        }
    }
}

#[hdk_extern]
pub fn store_chunk(file_chunk: LanguageChunk) -> ExternResult<EntryHash> {
    let file_chunk_hash = hash_entry(&file_chunk)?;

    if let None = get(file_chunk_hash.clone(), GetOptions::default())? {
        create_entry(&EntryTypes::LanguageChunk(file_chunk))?;
    }

    Ok(file_chunk_hash)
}

#[hdk_extern]
pub fn get_language_expression(
    file_expression_hash: LanguageAddress,
) -> ExternResult<Option<LanguageExpression>> {
    let expression_links = get_oldest_link(
        hash_entry(file_expression_hash)?,
        Some(LinkTag::from("".as_bytes().to_owned())),
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
                    let exp_data: LanguageExpression = elem
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

#[hdk_extern]
pub fn get_language_chunk(file_chunk_hash: EntryHash) -> ExternResult<Option<LanguageChunk>> {
    match get(file_chunk_hash, GetOptions::default())? {
        Some(record) => {
            let file_chunk: LanguageChunk = record
                .entry()
                .to_app_option()
                .map_err(|e| wasm_error!(e))?
                .ok_or(wasm_error!(WasmErrorInner::Guest(
                    "Malformed file chunk".into()
                )))?;

            Ok(Some(file_chunk))
        }
        None => Ok(None),
    }
}
