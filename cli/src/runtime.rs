use crate::util::query;
use anyhow::{Context, Result};
use graphql_client::GraphQLQuery;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct Info;

pub async fn run_info(cap_token: String) -> Result<info::InfoRuntimeInfo> {
    let response_data: info::ResponseData = query(cap_token, Info::build_query(info::Variables {}))
        .await
        .with_context(|| "Failed to run runtime->info query")?;
    Ok(response_data.runtime_info)
}
