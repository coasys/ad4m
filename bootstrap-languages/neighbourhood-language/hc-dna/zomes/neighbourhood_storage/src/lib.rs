use chrono::{DateTime, NaiveDateTime, Utc};
use hdk::prelude::*;
use integrity::{EntryTypes, NeighbourhoodExpression, NeighbourhoodParam, LinkTypes, NeighbourhoodAddress};
mod utils;
use utils::{err, get_latest_link};

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
pub fn store_neighbourhood_expression(neighbourhood: NeighbourhoodParam) -> ExternResult<()> {
    let address = EntryTypes::NeighbourhoodAddress(NeighbourhoodAddress(neighbourhood.address.clone()));
    let address_hash = hash_entry(&address)?;

    let neighbourhood_expression = EntryTypes::NeighbourhoodExpression(neighbourhood.neighbourhood);
    let neighbourhood_expression_hash = hash_entry(&neighbourhood_expression)?;
    create_entry(&neighbourhood_expression)?;

    //Link profile entry to did
    create_link(
        address_hash,
        neighbourhood_expression_hash,
        LinkTypes::NeighbourhoodLink,
        LinkTag::from("".as_bytes().to_owned()),
    )?;

    Ok(())
}

#[hdk_extern]
pub fn get_neighbourhood_expression(
    file_expression_hash: NeighbourhoodAddress,
) -> ExternResult<Option<NeighbourhoodExpression>> {
    let expression_links = get_latest_link(
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
                    let exp_data: NeighbourhoodExpression = elem
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