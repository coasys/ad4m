use crate::util::query;
use anyhow::{Context, Result};
use graphql_client::GraphQLQuery;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/neighbourhoods.gql",
    response_derives = "Debug"
)]
pub struct PublishFromPerspective;

pub async fn run_publish(
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