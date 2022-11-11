use crate::util::query;
use anyhow::{Context, Result};
use graphql_client::GraphQLQuery;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct ByFilter;

pub async fn run_by_filter(
    cap_token: String,
    filter: String,
) -> Result<Vec<by_filter::ByFilterLanguages>> {
    let response_data: by_filter::ResponseData = query(
        cap_token,
        ByFilter::build_query(by_filter::Variables { filter }),
    )
    .await
    .with_context(|| "Failed to run languages->all query")?;
    Ok(response_data.languages)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct ByAddress;

pub async fn run_by_address(
    cap_token: String,
    address: String,
) -> Result<Option<by_address::ByAddressLanguage>> {
    let response_data: by_address::ResponseData = query(
        cap_token,
        ByAddress::build_query(by_address::Variables { address }),
    )
    .await
    .with_context(|| "Failed to run languages -> by-address query")?;
    Ok(response_data.language)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct WriteSettings;

pub async fn run_write_settings(
    cap_token: String,
    language_address: String,
    settings: String,
) -> Result<write_settings::ResponseData> {
    query(
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
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct ApplyTemplateAndPublish;

pub async fn run_apply_template_and_publish(
    cap_token: String,
    source: String,
    template_data: String,
) -> Result<apply_template_and_publish::ApplyTemplateAndPublishLanguageApplyTemplateAndPublish> {
    let response_data: apply_template_and_publish::ResponseData = query(
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

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct Meta;

pub async fn run_meta(cap_token: String, address: String) -> Result<meta::MetaLanguageMeta> {
    let response_data: meta::ResponseData =
        query(cap_token, Meta::build_query(meta::Variables { address }))
            .await
            .with_context(|| "Failed to run languages -> meta")?;
    Ok(response_data.language_meta)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct Publish;

pub async fn run_publish(
    cap_token: String,
    language_path: String,
    name: String,
    description: Option<String>,
    possible_template_params: Option<Vec<String>>,
    source_code_link: Option<String>,
) -> Result<publish::PublishLanguagePublish> {
    let response_data: publish::ResponseData = query(
        cap_token,
        Publish::build_query(publish::Variables {
            language_path,
            language_meta: publish::LanguageMetaInput {
                name,
                description,
                possibleTemplateParams: possible_template_params,
                sourceCodeLink: source_code_link,
            },
        }),
    )
    .await
    .with_context(|| "Failed to run languages -> publish")?;
    Ok(response_data.language_publish)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct Source;

pub async fn run_source(cap_token: String, address: String) -> Result<String> {
    let response_data: source::ResponseData = query(
        cap_token,
        Source::build_query(source::Variables { address }),
    )
    .await
    .with_context(|| "Failed to run languages -> source")?;
    Ok(response_data.language_source)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/languages.gql",
    response_derives = "Debug"
)]
pub struct Remove;

pub async fn run_remove(cap_token: String, address: String) -> Result<()> {
    let _ = query(
        cap_token,
        Remove::build_query(remove::Variables { address }),
    )
    .await
    .with_context(|| "Failed to run languages -> remove")?;
    Ok(())
}
