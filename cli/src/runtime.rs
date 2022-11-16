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

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct Friends;

pub async fn run_friends(cap_token: String) -> Result<Vec<String>> {
    let response_data: friends::ResponseData = query(
        cap_token,
        Friends::build_query(friends::Variables {}),
    )
    .await
    .with_context(|| "Failed to run runtime->friends query")?;
    Ok(response_data.runtime_friends)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct AddFriends;

pub async fn run_add_friends(
    cap_token: String,
    dids: Vec<String>,
) -> Result<add_friends::ResponseData> {
    query(
        cap_token,
        AddFriends::build_query(add_friends::Variables { dids }),
    )
    .await
    .with_context(|| "Failed to run runtime->add-friends query")
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct RemoveFriends;

pub async fn run_remove_friends(
    cap_token: String,
    dids: Vec<String>,
) -> Result<remove_friends::ResponseData> {
    query(
        cap_token,
        RemoveFriends::build_query(remove_friends::Variables { dids }),
    )
    .await
    .with_context(|| "Failed to run runtime->remove-friends query")
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct HcAgentInfos;

pub async fn run_hc_agent_infos(cap_token: String) -> Result<String> {
    let response_data: hc_agent_infos::ResponseData = query(
        cap_token,
        HcAgentInfos::build_query(hc_agent_infos::Variables {}),
    )
    .await
    .with_context(|| "Failed to run runtime->hc-agent-infos query")?;
    Ok(response_data.runtime_hc_agent_infos)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct HcAddAgentInfos;

pub async fn run_hc_add_agent_infos(
    cap_token: String,
    agent_infos: String,
) -> Result<hc_add_agent_infos::ResponseData> {
    query(
        cap_token,
        HcAddAgentInfos::build_query(hc_add_agent_infos::Variables { agent_infos }),
    )
    .await
    .with_context(|| "Failed to run runtime->hc-add-agent-infos query")
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct VerifyStringSignedByDid;

pub async fn run_verify_string_signed_by_did(
    cap_token: String,
    did: String,
    did_signing_key_id: String,
    data: String,
    signed_data: String,
) -> Result<verify_string_signed_by_did::ResponseData> {
    query(
        cap_token,
        VerifyStringSignedByDid::build_query(verify_string_signed_by_did::Variables {
            did,
            did_signing_key_id,
            data,
            signed_data
        }),
    )
    .await
    .with_context(|| "Failed to run runtime->verify-string-signed-by-did query")
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct SetStatus;

pub async fn run_set_status(
    cap_token: String,
    status: set_status::PerspectiveInput,
) -> Result<set_status::ResponseData> {
    query(
        cap_token,
        SetStatus::build_query(set_status::Variables { status }),
    )
    .await
    .with_context(|| "Failed to run runtime->set-status query")
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct FriendStatus;

pub async fn run_friend_status(
    cap_token: String,
    did: String,
) -> Result<friend_status::ResponseData> {
    query(
        cap_token,
        FriendStatus::build_query(friend_status::Variables { did }),
    )
    .await
    .with_context(|| "Failed to run runtime->friend-status query")
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct FriendSendMessage;

pub async fn run_friend_send_message(
    cap_token: String,
    did: String,
    message: friend_send_message::PerspectiveInput,
) -> Result<friend_send_message::ResponseData> {
    query(
        cap_token,
        FriendSendMessage::build_query(friend_send_message::Variables { did, message }),
    )
    .await
    .with_context(|| "Failed to run runtime->friend-send-message query")
}