use std::sync::Arc;

use crate::{util::query, ClientInfo};
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
    executor_url: String,
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
        .post(executor_url)
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

pub async fn retrieve_capability(
    executor_url: String,
    request_id: String,
    rand: String,
) -> Result<String> {
    let query =
        RetrieveCapability::build_query(retrieve_capability::Variables { request_id, rand });
    let response_body: Response<retrieve_capability::ResponseData> = reqwest::Client::new()
        .post(executor_url)
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

pub async fn me(executor_url: String, cap_token: String) -> Result<me::MeAgent> {
    let response_data: me::ResponseData =
        query(executor_url, cap_token, Me::build_query(me::Variables {}))
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
pub struct GetApps;

pub async fn get_apps(executor_url: String, cap_token: String) -> Result<Vec<get_apps::GetAppsAgentGetApps>> {
    let response_data: get_apps::ResponseData =
        query(executor_url, cap_token, GetApps::build_query(get_apps::Variables {}))
            .await
            .with_context(|| "Failed to run agent->get apps query")?;
    Ok(response_data.agent_get_apps)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug"
)]
pub struct RevokeToken;

pub async fn revoke_token(executor_url: String, cap_token: String, request_id: String) -> Result<Vec<revoke_token::RevokeTokenAgentRevokeToken>> {
    let response_data: revoke_token::ResponseData =
        query(executor_url, cap_token, RevokeToken::build_query(revoke_token::Variables { request_id }))
            .await
            .with_context(|| "Failed to run agent->revoke_token query")?;
    Ok(response_data.agent_revoke_token)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug"
)]
pub struct RemoveApp;

pub async fn remove_app(executor_url: String, cap_token: String, request_id: String) -> Result<Vec<remove_app::RemoveAppAgentRemoveApp>> {
    let response_data: remove_app::ResponseData =
        query(executor_url, cap_token, RemoveApp::build_query(remove_app::Variables { request_id }))
            .await
            .with_context(|| "Failed to run agent->remove_app query")?;
    Ok(response_data.agent_remove_app)
}


#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug"
)]
pub struct AgentStatus;

pub async fn status(
    executor_url: String,
    cap_token: String,
) -> Result<agent_status::AgentStatusAgentStatus> {
    let response_data: agent_status::ResponseData = query(
        executor_url,
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

pub async fn lock(
    executor_url: String,
    cap_token: String,
    passphrase: String,
) -> Result<lock::LockAgentLock> {
    let response_data: lock::ResponseData = query(
        executor_url,
        cap_token,
        Lock::build_query(lock::Variables { passphrase }),
    )
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

pub async fn unlock(
    executor_url: String,
    cap_token: String,
    passphrase: String,
) -> Result<unlock::UnlockAgentUnlock> {
    let response_data: unlock::ResponseData = query(
        executor_url,
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

pub async fn by_did(
    executor_url: String,
    cap_token: String,
    did: String,
) -> Result<Option<by_did::ByDidAgentByDid>> {
    let response_data: by_did::ResponseData = query(
        executor_url,
        cap_token,
        ByDID::build_query(by_did::Variables { did }),
    )
    .await
    .with_context(|| "Failed to run agent->byDID query")?;
    Ok(response_data.agent_by_did)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug"
)]
pub struct Generate;

pub async fn generate(
    executor_url: String,
    cap_token: String,
    passphrase: String,
) -> Result<generate::GenerateAgentGenerate> {
    let response_data: generate::ResponseData = query(
        executor_url,
        cap_token,
        Generate::build_query(generate::Variables { passphrase }),
    )
    .await
    .with_context(|| "Failed to run agent->generate")?;
    Ok(response_data.agent_generate)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/agent.gql",
    response_derives = "Debug"
)]
pub struct SignMessage;

pub async fn sign_message(
    executor_url: String,
    cap_token: String,
    message: String,
) -> Result<sign_message::SignMessageAgentSignMessage> {
    let response: sign_message::ResponseData = query(
        executor_url,
        cap_token,
        SignMessage::build_query(sign_message::Variables { message }),
    )
    .await
    .with_context(|| "Failed to run agent->sign_message")?;

    Ok(response.agent_sign_message)
}

pub struct AgentClient {
    info: Arc<ClientInfo>,
}

impl AgentClient {
    pub fn new(info: Arc<ClientInfo>) -> Self {
        Self { info }
    }

    pub async fn request_capability(
        &self,
        app_name: String,
        app_desc: String,
        app_url: String,
        capabilities: String,
    ) -> Result<String> {
        request_capability(
            self.info.executor_url.clone(),
            app_name,
            app_desc,
            app_url,
            capabilities,
        )
        .await
    }

    pub async fn retrieve_capability(&self, request_id: String, rand: String) -> Result<String> {
        retrieve_capability(self.info.executor_url.clone(), request_id, rand).await
    }

    pub async fn me(&self) -> Result<me::MeAgent> {
        me(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn status(&self) -> Result<agent_status::AgentStatusAgentStatus> {
        status(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn get_apps(&self) -> Result<Vec<get_apps::GetAppsAgentGetApps>> {
        get_apps(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn lock(&self, passphrase: String) -> Result<lock::LockAgentLock> {
        lock(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            passphrase,
        )
        .await
    }

    pub async fn unlock(&self, passphrase: String) -> Result<unlock::UnlockAgentUnlock> {
        unlock(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            passphrase,
        )
        .await
    }

    pub async fn by_did(&self, did: String) -> Result<Option<by_did::ByDidAgentByDid>> {
        by_did(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            did,
        )
        .await
    }

    pub async fn generate(&self, passphrase: String) -> Result<generate::GenerateAgentGenerate> {
        generate(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            passphrase,
        )
        .await
    }

    pub async fn sign_message(
        &self,
        message: String,
    ) -> Result<sign_message::SignMessageAgentSignMessage> {
        sign_message(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            message,
        )
        .await
    }
}
