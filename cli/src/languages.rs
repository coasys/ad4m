use graphql_client::{GraphQLQuery};
use anyhow::{Context, Result};
use crate::util::query;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug",
)]
pub struct ByFilter;

pub async fn run_by_filter(cap_token: String, filter: String) -> Result<Vec<by_filter::ByFilterLanguages>> {
    let response_data: by_filter::ResponseData = query(cap_token, ByFilter::build_query(by_filter::Variables {filter}))
        .await
        .with_context(|| "Failed to run languages->all query")?;
    Ok(response_data.languages)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug",
)]
pub struct ByAddress;

pub async fn run_by_address(cap_token: String, address: String) -> Result<Option<by_address::ByAddressLanguage>> {
    let response_data: by_address::ResponseData = query(cap_token, ByAddress::build_query(by_address::Variables {address}))
        .await
        .with_context(|| "Failed to run languages->all query")?;
    Ok(response_data.language)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug",
)]
pub struct WriteSettings;

pub async fn run_write_settings(cap_token: String, language_address: String, settings: String) -> Result<write_settings::ResponseData> {
    Ok(query(cap_token, WriteSettings::build_query(write_settings::Variables {
        language_address,
        settings
    }))
        .await
        .with_context(|| "Failed to run languages->all query")?)
}

