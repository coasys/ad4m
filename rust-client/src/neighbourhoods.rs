use std::sync::Arc;

use crate::{util::query, ClientInfo};
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
    executor_url: String,
    cap_token: String,
    link_language: String,
    meta: Option<publish_from_perspective::PerspectiveInput>,
    perspective_uuid: String,
) -> Result<String> {
    let meta = meta.unwrap_or(publish_from_perspective::PerspectiveInput { links: vec![] });
    let response_data: publish_from_perspective::ResponseData = query(
        executor_url,
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
    executor_url: String,
    cap_token: String,
    url: String,
) -> Result<join_from_url::JoinFromUrlNeighbourhoodJoinFromUrl> {
    let response_data: join_from_url::ResponseData = query(
        executor_url,
        cap_token,
        JoinFromUrl::build_query(join_from_url::Variables { url }),
    )
    .await
    .with_context(|| "Failed to run neighbourhoods->join query")?;
    Ok(response_data.neighbourhood_join_from_url)
}

pub struct NeighbourhoodsClient {
    info: Arc<ClientInfo>,
}

impl NeighbourhoodsClient {
    pub fn new(info: Arc<ClientInfo>) -> Self {
        Self { info }
    }

    pub async fn publish(
        &self,
        link_language: String,
        meta: Option<publish_from_perspective::PerspectiveInput>,
        perspective_uuid: String,
    ) -> Result<String> {
        publish(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            link_language,
            meta,
            perspective_uuid,
        )
        .await
    }

    pub async fn join(
        &self,
        url: String,
    ) -> Result<join_from_url::JoinFromUrlNeighbourhoodJoinFromUrl> {
        join(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            url,
        )
        .await
    }
}
