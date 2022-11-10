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