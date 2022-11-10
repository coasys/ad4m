use graphql_client::{GraphQLQuery, Response};
use crate::{startup::get_executor_url, util::query};
use anyhow::{Result, anyhow, Context};
#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug",
)]
pub struct AgentStatus;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug",
)]
pub struct RequestCapability;

pub async fn run_request_capability() -> Result<String> {
    let query = RequestCapability::build_query(request_capability::Variables {
        app_name: "AD4M cli".to_string(),
        app_desc: "Command line administration tool for AD4M".to_string(),
        app_url: "org.perspect3vism.ad4m.cli".to_string(),
        capabilities: "[{\"with\":{\"domain\":\"*\",\"pointers\":[\"*\"]},\"can\":[\"*\"]}]".to_string(),
    });
    let response_body: Response<request_capability::ResponseData> = reqwest::Client::new()
        .post(get_executor_url()?)
        .json(&query)
        .send()
        .await?
        .json()
        .await?;

    let response_data = response_body.data.ok_or_else(|| anyhow!("No data in response"))?;
    Ok(response_data.agent_request_capability)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug",
)]
pub struct RetrieveCapability;

pub async fn run_retrieve_capability(request_id: String, rand: String) -> Result<String> {
    let query = RetrieveCapability::build_query(retrieve_capability::Variables {
        request_id,
        rand,
    });
    let response_body: Response<retrieve_capability::ResponseData> = reqwest::Client::new()
        .post(get_executor_url()?)
        .json(&query)
        .send()
        .await?
        .json()
        .await?;

    let response_data = response_body.data.ok_or_else(|| anyhow!("No data in response"))?;
    Ok(response_data.agent_generate_jwt)
}


#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug",
)]
pub struct Me;

pub async fn run_me(cap_token: String) -> Result<me::MeAgent> {
    let response_data: me::ResponseData = query(cap_token, Me::build_query(me::Variables {}))
        .await
        .with_context(|| "Failed to run agent->me query")?;
    Ok(response_data.agent)
}