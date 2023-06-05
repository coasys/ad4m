use chrono::{DateTime, NaiveDateTime, Utc};
use hdk::prelude::*;
use integrity::{EntryTypes, LanguageChunk, LanguageExpression};

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
pub fn store_language_expression(expression: LanguageExpression) -> ExternResult<EntryHash> {
    let hash = hash_entry(&expression)?;
    create_entry(&EntryTypes::LanguageExpression(expression))?;

    Ok(hash)
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
    file_expression_hash: EntryHash,
) -> ExternResult<Option<LanguageExpression>> {
    match get(file_expression_hash.clone(), GetOptions::default())? {
        Some(record) => {
            let file_expression: LanguageExpression = record
                .entry()
                .to_app_option()
                .map_err(|e| wasm_error!(e))?
                .ok_or(wasm_error!(WasmErrorInner::Guest(
                    "Malformed file chunk".into()
                )))?;

            Ok(Some(file_expression))
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
