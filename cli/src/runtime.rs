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

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct Quit;

pub async fn run_quit(cap_token: String) -> Result<quit::ResponseData> {
    query(cap_token, Quit::build_query(quit::Variables {}))
        .await
        .with_context(|| "Failed to run runtime->quit query")
}


#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct AddTrustedAgents;

pub async fn run_add_trusted_agents(
    cap_token: String,
    agents: Vec<String>,
) -> Result<add_trusted_agents::ResponseData> {
    query(
        cap_token,
        AddTrustedAgents::build_query(add_trusted_agents::Variables { agents }),
    )
    .await
    .with_context(|| "Failed to run runtime->add-trusted-agents query")
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct DeleteTrustedAgents;

pub async fn run_delete_trusted_agents(
    cap_token: String,
    agents: Vec<String>,
) -> Result<delete_trusted_agents::ResponseData> {
    query(
        cap_token,
        DeleteTrustedAgents::build_query(delete_trusted_agents::Variables { agents }),
    )
    .await
    .with_context(|| "Failed to run runtime -> delete-trusted-agents query")
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct TrustedAgents;

pub async fn run_trusted_agents(cap_token: String) -> Result<Vec<String>> {
    let response_data: trusted_agents::ResponseData = query(
        cap_token,
        TrustedAgents::build_query(trusted_agents::Variables {}),
    )
    .await
    .with_context(|| "Failed to run runtime->trusted-agents query")?;
    Ok(response_data.get_trusted_agents)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct LinkLanguageTemplates;

pub async fn run_link_language_templates(cap_token: String) -> Result<Vec<String>> {
    let response_data: link_language_templates::ResponseData = query(
        cap_token,
        LinkLanguageTemplates::build_query(link_language_templates::Variables { }),
    )
    .await
    .with_context(|| "Failed to run runtime->link-language-templates query")?;

    Ok(response_data.runtime_known_link_language_templates)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct AddLinkLanguageTemplates;

pub async fn run_add_link_language_templates(
    cap_token: String,
    addresses: Vec<String>,
) -> Result<add_link_language_templates::ResponseData> {
    query(
        cap_token,
        AddLinkLanguageTemplates::build_query(add_link_language_templates::Variables { addresses }),
    )
    .await
    .with_context(|| "Failed to run runtime->add-link-language-templates query")
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct RemoveLinkLanguageTemplates;

pub async fn run_remove_link_language_templates(
    cap_token: String,
    addresses: Vec<String>,
) -> Result<remove_link_language_templates::ResponseData> {
    query(
        cap_token,
        RemoveLinkLanguageTemplates::build_query(remove_link_language_templates::Variables { addresses }),
    )
    .await
    .with_context(|| "Failed to run runtime->remove-link-language-templates query")
}