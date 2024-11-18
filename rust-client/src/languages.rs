use std::sync::Arc;

use crate::{util::query, ClientInfo};
use anyhow::{Context, Result};
use graphql_client::GraphQLQuery;
use serde::{Deserialize, Serialize};

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct ByFilter;

pub async fn by_filter(
    executor_url: String,
    cap_token: String,
    filter: String,
) -> Result<Vec<by_filter::ByFilterLanguages>> {
    let response_data: by_filter::ResponseData = query(
        executor_url,
        cap_token,
        ByFilter::build_query(by_filter::Variables { filter }),
    )
    .await
    .with_context(|| "Failed to run languages->all query")?;
    Ok(response_data.languages)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct ByAddress;

pub async fn by_address(
    executor_url: String,
    cap_token: String,
    address: String,
) -> Result<Option<by_address::ByAddressLanguage>> {
    let response_data: by_address::ResponseData = query(
        executor_url,
        cap_token,
        ByAddress::build_query(by_address::Variables { address }),
    )
    .await
    .with_context(|| "Failed to run languages -> by-address query")?;
    Ok(response_data.language)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct WriteSettings;

pub async fn write_settings(
    executor_url: String,
    cap_token: String,
    language_address: String,
    settings: String,
) -> Result<write_settings::ResponseData> {
    query(
        executor_url,
        cap_token,
        WriteSettings::build_query(write_settings::Variables {
            language_address,
            settings,
        }),
    )
    .await
    .with_context(|| "Failed to run languages -> write-settings query")
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct ApplyTemplateAndPublish;

pub async fn apply_template_and_publish(
    executor_url: String,
    cap_token: String,
    source: String,
    template_data: String,
) -> Result<apply_template_and_publish::ApplyTemplateAndPublishLanguageApplyTemplateAndPublish> {
    let response_data: apply_template_and_publish::ResponseData = query(
        executor_url,
        cap_token,
        ApplyTemplateAndPublish::build_query(apply_template_and_publish::Variables {
            source_language_hash: source,
            template_data,
        }),
    )
    .await
    .with_context(|| "Failed to run languages -> apply-template-and-publish")?;
    Ok(response_data.language_apply_template_and_publish)
}

#[derive(GraphQLQuery, Debug, Serialize, Deserialize)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct Meta;

pub async fn meta(
    executor_url: String,
    cap_token: String,
    address: String,
) -> Result<meta::MetaLanguageMeta> {
    let response_data: meta::ResponseData = query(
        executor_url,
        cap_token,
        Meta::build_query(meta::Variables { address }),
    )
    .await
    .with_context(|| "Failed to run languages -> meta")?;
    Ok(response_data.language_meta)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct Publish;

pub async fn publish(
    executor_url: String,
    cap_token: String,
    language_path: String,
    name: String,
    description: Option<String>,
    possible_template_params: Option<Vec<String>>,
    source_code_link: Option<String>,
) -> Result<publish::PublishLanguagePublish> {
    let response_data: publish::ResponseData = query(
        executor_url,
        cap_token,
        Publish::build_query(publish::Variables {
            language_path,
            language_meta: publish::LanguageMetaInput {
                name,
                description,
                possible_template_params,
                source_code_link,
            },
        }),
    )
    .await
    .with_context(|| "Failed to run languages -> publish")?;
    Ok(response_data.language_publish)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct Source;

pub async fn source(executor_url: String, cap_token: String, address: String) -> Result<String> {
    let response_data: source::ResponseData = query(
        executor_url,
        cap_token,
        Source::build_query(source::Variables { address }),
    )
    .await
    .with_context(|| "Failed to run languages -> source")?;
    Ok(response_data.language_source)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct Remove;

pub async fn remove(executor_url: String, cap_token: String, address: String) -> Result<()> {
    query::<_, ()>(
        executor_url,
        cap_token,
        Remove::build_query(remove::Variables { address }),
    )
    .await
    .with_context(|| "Failed to run languages -> remove")?;
    Ok(())
}

pub struct LanguagesClient {
    info: Arc<ClientInfo>,
}

impl LanguagesClient {
    pub fn new(info: Arc<ClientInfo>) -> Self {
        Self { info }
    }

    pub async fn by_filter(
        &self,
        filter: Option<String>,
    ) -> Result<Vec<by_filter::ByFilterLanguages>> {
        by_filter(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            filter.unwrap_or_default(),
        )
        .await
    }

    pub async fn by_address(
        &self,
        address: String,
    ) -> Result<Option<by_address::ByAddressLanguage>> {
        by_address(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            address,
        )
        .await
    }

    pub async fn write_settings(
        &self,
        language_address: String,
        settings: String,
    ) -> Result<write_settings::ResponseData> {
        write_settings(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            language_address,
            settings,
        )
        .await
    }

    pub async fn apply_template_and_publish(
        &self,
        source: String,
        template_data: String,
    ) -> Result<apply_template_and_publish::ApplyTemplateAndPublishLanguageApplyTemplateAndPublish>
    {
        apply_template_and_publish(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            source,
            template_data,
        )
        .await
    }

    pub async fn meta(&self, address: String) -> Result<meta::MetaLanguageMeta> {
        meta(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            address,
        )
        .await
    }

    pub async fn publish(
        &self,
        language_path: String,
        name: String,
        description: Option<String>,
        possible_template_params: Option<Vec<String>>,
        source_code_link: Option<String>,
    ) -> Result<publish::PublishLanguagePublish> {
        publish(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            language_path,
            name,
            description,
            possible_template_params,
            source_code_link,
        )
        .await
    }

    pub async fn source(&self, address: String) -> Result<String> {
        source(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            address,
        )
        .await
    }

    pub async fn remove(&self, address: String) -> Result<()> {
        remove(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            address,
        )
        .await
    }
}
