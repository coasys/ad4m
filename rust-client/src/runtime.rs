use std::sync::Arc;

use crate::{
    types::{PerspectiveExpression, SentPerspectiveMessage},
    util::query,
    ClientInfo,
};
use anyhow::{Context, Result};
use graphql_client::GraphQLQuery;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct Info;

pub async fn info(executor_url: String, cap_token: String) -> Result<info::InfoRuntimeInfo> {
    let response_data: info::ResponseData = query(
        executor_url,
        cap_token,
        Info::build_query(info::Variables {}),
    )
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

pub async fn quit(executor_url: String, cap_token: String) -> Result<quit::ResponseData> {
    query(
        executor_url,
        cap_token,
        Quit::build_query(quit::Variables {}),
    )
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

pub async fn add_trusted_agents(
    executor_url: String,
    cap_token: String,
    agents: Vec<String>,
) -> Result<add_trusted_agents::ResponseData> {
    query(
        executor_url,
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

pub async fn delete_trusted_agents(
    executor_url: String,
    cap_token: String,
    agents: Vec<String>,
) -> Result<delete_trusted_agents::ResponseData> {
    query(
        executor_url,
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

pub async fn trusted_agents(executor_url: String, cap_token: String) -> Result<Vec<String>> {
    let response_data: trusted_agents::ResponseData = query(
        executor_url,
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

pub async fn link_language_templates(
    executor_url: String,
    cap_token: String,
) -> Result<Vec<String>> {
    let response_data: link_language_templates::ResponseData = query(
        executor_url,
        cap_token,
        LinkLanguageTemplates::build_query(link_language_templates::Variables {}),
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

pub async fn add_link_language_templates(
    executor_url: String,
    cap_token: String,
    addresses: Vec<String>,
) -> Result<add_link_language_templates::ResponseData> {
    query(
        executor_url,
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

pub async fn remove_link_language_templates(
    executor_url: String,
    cap_token: String,
    addresses: Vec<String>,
) -> Result<remove_link_language_templates::ResponseData> {
    query(
        executor_url,
        cap_token,
        RemoveLinkLanguageTemplates::build_query(remove_link_language_templates::Variables {
            addresses,
        }),
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

pub async fn friends(executor_url: String, cap_token: String) -> Result<Vec<String>> {
    let response_data: friends::ResponseData = query(
        executor_url,
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

pub async fn add_friends(
    executor_url: String,
    cap_token: String,
    dids: Vec<String>,
) -> Result<add_friends::ResponseData> {
    query(
        executor_url,
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

pub async fn remove_friends(
    executor_url: String,
    cap_token: String,
    dids: Vec<String>,
) -> Result<remove_friends::ResponseData> {
    query(
        executor_url,
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

pub async fn hc_agent_infos(executor_url: String, cap_token: String) -> Result<String> {
    let response_data: hc_agent_infos::ResponseData = query(
        executor_url,
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

pub async fn hc_add_agent_infos(
    executor_url: String,
    cap_token: String,
    agent_infos: String,
) -> Result<hc_add_agent_infos::ResponseData> {
    query(
        executor_url,
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

pub async fn verify_string_signed_by_did(
    executor_url: String,
    cap_token: String,
    did: String,
    did_signing_key_id: String,
    data: String,
    signed_data: String,
) -> Result<verify_string_signed_by_did::ResponseData> {
    query(
        executor_url,
        cap_token,
        VerifyStringSignedByDid::build_query(verify_string_signed_by_did::Variables {
            did,
            did_signing_key_id,
            data,
            signed_data,
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

pub async fn set_status(
    executor_url: String,
    cap_token: String,
    status: set_status::PerspectiveInput,
) -> Result<set_status::ResponseData> {
    query(
        executor_url,
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

pub async fn friend_status(
    executor_url: String,
    cap_token: String,
    did: String,
) -> Result<friend_status::ResponseData> {
    query(
        executor_url,
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

pub async fn friend_send_message(
    executor_url: String,
    cap_token: String,
    did: String,
    message: friend_send_message::PerspectiveInput,
) -> Result<friend_send_message::ResponseData> {
    query(
        executor_url,
        cap_token,
        FriendSendMessage::build_query(friend_send_message::Variables { did, message }),
    )
    .await
    .with_context(|| "Failed to run runtime->friend-send-message query")
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct MessageInbox;

pub async fn message_inbox(
    executor_url: String,
    cap_token: String,
    filter: Option<String>,
) -> Result<Vec<PerspectiveExpression>> {
    let response: message_inbox::ResponseData = query(
        executor_url,
        cap_token,
        MessageInbox::build_query(message_inbox::Variables { filter }),
    )
    .await
    .with_context(|| "Failed to run runtime->message-inbox query")?;

    Ok(response
        .runtime_message_inbox
        .into_iter()
        .map(|d| d.into())
        .collect())
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/runtime.gql",
    response_derives = "Debug"
)]
pub struct MessageOutbox;

pub async fn message_outbox(
    executor_url: String,
    cap_token: String,
    filter: Option<String>,
) -> Result<Vec<SentPerspectiveMessage>> {
    let response: message_outbox::ResponseData = query(
        executor_url,
        cap_token,
        MessageOutbox::build_query(message_outbox::Variables { filter }),
    )
    .await
    .with_context(|| "Failed to run runtime->message-outbox query")?;

    Ok(response
        .runtime_message_outbox
        .into_iter()
        .map(|d| d.into())
        .collect())
}

pub struct RuntimeClient {
    info: Arc<ClientInfo>,
}

impl RuntimeClient {
    pub fn new(info: Arc<ClientInfo>) -> Self {
        Self { info }
    }

    pub async fn info(&self) -> Result<info::InfoRuntimeInfo> {
        info(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn quit(&self) -> Result<quit::ResponseData> {
        quit(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn add_trusted_agents(
        &self,
        agents: Vec<String>,
    ) -> Result<add_trusted_agents::ResponseData> {
        add_trusted_agents(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            agents,
        )
        .await
    }

    pub async fn delete_trusted_agents(
        &self,
        agents: Vec<String>,
    ) -> Result<delete_trusted_agents::ResponseData> {
        delete_trusted_agents(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            agents,
        )
        .await
    }

    pub async fn trusted_agents(&self) -> Result<Vec<String>> {
        trusted_agents(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn link_language_templates(&self) -> Result<Vec<String>> {
        link_language_templates(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn add_link_language_templates(
        &self,
        addresses: Vec<String>,
    ) -> Result<add_link_language_templates::ResponseData> {
        add_link_language_templates(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            addresses,
        )
        .await
    }

    pub async fn remove_link_language_templates(
        &self,
        addresses: Vec<String>,
    ) -> Result<remove_link_language_templates::ResponseData> {
        remove_link_language_templates(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            addresses,
        )
        .await
    }

    pub async fn friends(&self) -> Result<Vec<String>> {
        friends(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn add_friends(&self, friends: Vec<String>) -> Result<add_friends::ResponseData> {
        add_friends(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            friends,
        )
        .await
    }

    pub async fn remove_friends(
        &self,
        friends: Vec<String>,
    ) -> Result<remove_friends::ResponseData> {
        remove_friends(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            friends,
        )
        .await
    }

    pub async fn hc_agent_infos(&self) -> Result<String> {
        hc_agent_infos(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn hc_add_agent_infos(
        &self,
        agent_infos: String,
    ) -> Result<hc_add_agent_infos::ResponseData> {
        hc_add_agent_infos(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            agent_infos,
        )
        .await
    }

    pub async fn verify_string_signed_by_did(
        &self,
        did: String,
        did_signing_key_id: String,
        data: String,
        signed_data: String,
    ) -> Result<bool> {
        let response = verify_string_signed_by_did(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            did,
            did_signing_key_id,
            data,
            signed_data,
        )
        .await?;

        Ok(response.runtime_verify_string_signed_by_did)
    }

    pub async fn set_status(&self, status: set_status::PerspectiveInput) -> Result<()> {
        set_status(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            status.into(),
        )
        .await?;

        Ok(())
    }

    pub async fn friend_status(&self, did: String) -> Result<friend_status::ResponseData> {
        friend_status(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            did,
        )
        .await
    }

    pub async fn friend_send_message(
        &self,
        did: String,
        message: friend_send_message::PerspectiveInput,
    ) -> Result<friend_send_message::ResponseData> {
        friend_send_message(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            did,
            message.into(),
        )
        .await
    }

    pub async fn message_inbox(
        &self,
        filter: Option<String>,
    ) -> Result<Vec<PerspectiveExpression>> {
        message_inbox(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            filter,
        )
        .await
    }

    pub async fn message_outbox(
        &self,
        filter: Option<String>,
    ) -> Result<Vec<SentPerspectiveMessage>> {
        message_outbox(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            filter,
        )
        .await
    }
}
