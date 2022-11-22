use crate::util::query;
use anyhow::{Context, Result};
use graphql_client::GraphQLQuery;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/neighbourhoods.gql",
    response_derives = "Debug"
)]
pub struct PublishFromPerspective;

pub async fn publish(
    cap_token: String,
    link_language: String,
    meta: Option<publish_from_perspective::PerspectiveInput>,
    perspective_uuid: String,
) -> Result<String> {
    let meta = meta.unwrap_or(publish_from_perspective::PerspectiveInput { links: vec![] });
    let response_data: publish_from_perspective::ResponseData = query(
        cap_token,
        PublishFromPerspective::build_query(publish_from_perspective::Variables {
            link_language,
            meta,
            perspective_uuid,
        }),
    )
    .await
    .with_context(|| "Failed to run neighbourhoods->publish query")?;
    Ok(response_data.neighbourhood_publish_from_perspective)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/neighbourhoods.gql",
    response_derives = "Debug"
)]
pub struct JoinFromUrl;

pub async fn join(
    cap_token: String,
    url: String,
) -> Result<join_from_url::JoinFromUrlNeighbourhoodJoinFromUrl> {
    let response_data: join_from_url::ResponseData = query(
        cap_token,
        JoinFromUrl::build_query(join_from_url::Variables { url }),
    )
    .await
    .with_context(|| "Failed to run neighbourhoods->join query")?;
    Ok(response_data.neighbourhood_join_from_url)
}
