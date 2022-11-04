use graphql_client::{GraphQLQuery, Response};
use crate::startup::get_executor_url;
use anyhow::{Result, anyhow};

use self::all::AllPerspectives;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug",
)]
pub struct All;

pub async fn run_all(cap_token: String) -> Result<Vec<AllPerspectives>> {
    let query = All::build_query(all::Variables {});
    let response_body: Response<all::ResponseData> = reqwest::Client::new()
        .post(get_executor_url()?)
        .header("Authorization", cap_token) 
        .json(&query)
        .send()
        .await?
        .json()
        .await?;

    let response_data = response_body.data.ok_or(anyhow!("No data in response"))?;
    Ok(response_data.perspectives)
}