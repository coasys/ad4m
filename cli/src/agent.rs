use graphql_client::{GraphQLQuery, Response};

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

pub async fn run_request_capability() -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
    let query = RequestCapability::build_query(request_capability::Variables {
        app_name: "AD4M cli".to_string(),
        app_desc: "Command line administration tool for AD4M".to_string(),
        app_url: "org.perspect3vism.ad4m.cli".to_string(),
        capabilities: "[{\"with\":{\"domain\":\"*\",\"pointers\":[\"*\"]},\"can\":[\"*\"]}]".to_string(),
    });
    let response_body: Response<request_capability::ResponseData> = reqwest::Client::new()
        .post("http://localhost:12000/graphql")
        .json(&query)
        .send()
        .await?
        .json()
        .await?;

    let response_data = response_body.data.ok_or("No data in response")?;
    Ok(response_data.agent_request_capability)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug",
)]
pub struct RetrieveCapability;

pub async fn run_retrieve_capability(request_id: String, rand: String) -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
    let query = RetrieveCapability::build_query(retrieve_capability::Variables {
        request_id: request_id,
        rand: rand,
    });
    let response_body: Response<retrieve_capability::ResponseData> = reqwest::Client::new()
        .post("http://localhost:12000/graphql")
        .json(&query)
        .send()
        .await?
        .json()
        .await?;

    let response_data = response_body.data.ok_or("No data in response")?;
    Ok(response_data.agent_generate_jwt)
}
