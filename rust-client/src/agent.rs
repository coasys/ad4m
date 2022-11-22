use crate::{get_executor_url, util::query};
use anyhow::{anyhow, Context, Result};
use graphql_client::{GraphQLQuery, Response};

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug"
)]
pub struct RequestCapability;

pub async fn request_capability(
    app_name: String,
    app_desc: String,
    app_url: String,
    capabilities: String,
) -> Result<String> {
    let query = RequestCapability::build_query(request_capability::Variables {
        app_name,
        app_desc,
        app_url,
        capabilities,
    });
    let response_body: Response<request_capability::ResponseData> = reqwest::Client::new()
        .post(get_executor_url())
        .json(&query)
        .send()
        .await?
        .json()
        .await?;

    let response_data = response_body
        .data
        .ok_or_else(|| anyhow!("No data in response! Errors: {:?}", response_body.errors))?;
    Ok(response_data.agent_request_capability)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug"
)]
pub struct RetrieveCapability;

pub async fn retrieve_capability(request_id: String, rand: String) -> Result<String> {
    let query =
        RetrieveCapability::build_query(retrieve_capability::Variables { request_id, rand });
    let response_body: Response<retrieve_capability::ResponseData> = reqwest::Client::new()
        .post(get_executor_url())
        .json(&query)
        .send()
        .await?
        .json()
        .await?;

    let response_data = response_body
        .data
        .ok_or_else(|| anyhow!("No data in response! Errors: {:?}", response_body.errors))?;
    Ok(response_data.agent_generate_jwt)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug"
)]
pub struct Me;

pub async fn me(cap_token: String) -> Result<me::MeAgent> {
    let response_data: me::ResponseData = query(cap_token, Me::build_query(me::Variables {}))
        .await
        .with_context(|| "Failed to run agent->me query")?;
    Ok(response_data.agent)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug"
)]
pub struct AgentStatus;

pub async fn status(cap_token: String) -> Result<agent_status::AgentStatusAgentStatus> {
    let response_data: agent_status::ResponseData = query(
        cap_token,
        AgentStatus::build_query(agent_status::Variables {}),
    )
    .await
    .with_context(|| "Failed to run agent->me query")?;
    Ok(response_data.agent_status)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug"
)]
pub struct Lock;

pub async fn lock(cap_token: String, passphrase: String) -> Result<lock::LockAgentLock> {
    let response_data: lock::ResponseData =
        query(cap_token, Lock::build_query(lock::Variables { passphrase }))
            .await
            .with_context(|| "Failed to run agent->lock")?;
    Ok(response_data.agent_lock)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug"
)]
pub struct Unlock;

pub async fn unlock(cap_token: String, passphrase: String) -> Result<unlock::UnlockAgentUnlock> {
    let response_data: unlock::ResponseData = query(
        cap_token,
        Unlock::build_query(unlock::Variables { passphrase }),
    )
    .await
    .with_context(|| "Failed to run agent->unlock")?;
    Ok(response_data.agent_unlock)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug"
)]
pub struct ByDID;

pub async fn by_did(cap_token: String, did: String) -> Result<Option<by_did::ByDidAgentByDid>> {
    let response_data: by_did::ResponseData =
        query(cap_token, ByDID::build_query(by_did::Variables { did }))
            .await
            .with_context(|| "Failed to run agent->byDID query")?;
    Ok(response_data.agent_by_did)
}
