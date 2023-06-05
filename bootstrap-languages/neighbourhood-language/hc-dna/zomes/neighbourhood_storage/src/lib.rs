use chrono::{DateTime, NaiveDateTime, Utc};
use hdk::prelude::*;
use integrity::{EntryTypes, NeighbourhoodExpression};

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
pub fn store_neighbourhood_expression(expression: NeighbourhoodExpression) -> ExternResult<EntryHash> {
    let hash = hash_entry(&expression)?;
    create_entry(&EntryTypes::NeighbourhoodExpression(expression))?;

    Ok(hash)
}

#[hdk_extern]
pub fn get_neighbourhood_expression(
    file_expression_hash: EntryHash,
) -> ExternResult<Option<NeighbourhoodExpression>> {
    match get(file_expression_hash.clone(), GetOptions::default())? {
        Some(record) => {
            let file_expression: NeighbourhoodExpression = record
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