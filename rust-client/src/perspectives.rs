use std::sync::Arc;

use crate::types::{LinkExpression, Perspective};
use crate::util::{create_websocket_client, query, query_raw};
use crate::ClientInfo;
use anyhow::{anyhow, Context, Result};
use chrono::naive::NaiveDateTime;
use graphql_client::{GraphQLQuery, Response};
use graphql_ws_client::graphql::StreamingOperation;
use serde_json::Value;

type DateTime = NaiveDateTime;

use self::add_link::AddLinkPerspectiveAddLink;
use self::add_local_link::AddLocalLinkPerspectiveAddLocalLink;
use self::all::AllPerspectives;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug"
)]
pub struct All;

pub async fn all(executor_url: String, cap_token: String) -> Result<Vec<AllPerspectives>> {
    let response_data: all::ResponseData =
        query(executor_url, cap_token, All::build_query(all::Variables {}))
            .await
            .with_context(|| "Failed to run perspectives->all query")?;
    Ok(response_data.perspectives)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug"
)]
pub struct Add;

pub async fn add(executor_url: String, cap_token: String, name: String) -> Result<String> {
    let response_data: add::ResponseData = query(
        executor_url,
        cap_token,
        Add::build_query(add::Variables { name }),
    )
    .await
    .with_context(|| "Failed to run perspectives->add query")?;
    Ok(response_data.perspective_add.uuid)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug"
)]
pub struct Remove;

pub async fn remove(executor_url: String, cap_token: String, uuid: String) -> Result<()> {
    let response: remove::ResponseData = query(
        executor_url,
        cap_token,
        Remove::build_query(remove::Variables { uuid }),
    )
    .await
    .with_context(|| "Failed to run perspectives->remove query")?;
    if response.perspective_remove {
        Ok(())
    } else {
        Err(anyhow!("Failed to remove perspective"))
    }
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug"
)]
pub struct AddLink;

pub async fn add_link(
    executor_url: String,
    cap_token: String,
    uuid: String,
    source: String,
    target: String,
    predicate: Option<String>,
) -> Result<AddLinkPerspectiveAddLink> {
    let response_data: add_link::ResponseData = query(
        executor_url,
        cap_token,
        AddLink::build_query(add_link::Variables {
            uuid,
            link: add_link::LinkInput {
                source,
                target,
                predicate,
            },
        }),
    )
    .await
    .with_context(|| "Failed to run perspectives->addLink query")?;

    Ok(response_data.perspective_add_link)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug"
)]
pub struct AddLocalLink;

pub async fn add_local_link(
    executor_url: String,
    cap_token: String,
    uuid: String,
    source: String,
    target: String,
    predicate: Option<String>,
) -> Result<AddLocalLinkPerspectiveAddLocalLink> {
    let response_data: add_local_link::ResponseData = query(
        executor_url,
        cap_token,
        AddLocalLink::build_query(add_local_link::Variables {
            uuid,
            link: add_local_link::LinkInput {
                source,
                target,
                predicate,
            },
        }),
    )
    .await
    .with_context(|| "Failed to run perspectives->addLocalLink query")?;

    Ok(response_data.perspective_add_local_link)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug"
)]
pub struct QueryLinks;

pub async fn query_links(
    executor_url: String,
    cap_token: String,
    uuid: String,
    source: Option<String>,
    target: Option<String>,
    predicate: Option<String>,
    from_date: Option<DateTime>,
    until_date: Option<DateTime>,
    limit: Option<f64>,
) -> Result<Vec<query_links::QueryLinksPerspectiveQueryLinks>> {
    let response_data: query_links::ResponseData = query(
        executor_url,
        cap_token,
        QueryLinks::build_query(query_links::Variables {
            uuid,
            query: query_links::LinkQuery {
                source,
                target,
                predicate,
                fromDate: from_date,
                untilDate: until_date,
                limit,
            },
        }),
    )
    .await
    .with_context(|| "Failed to run perspectives->queryLinks query")?;

    Ok(response_data.perspective_query_links.unwrap_or_default())
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug"
)]
pub struct Infer;

pub async fn infer(
    executor_url: String,
    cap_token: String,
    uuid: String,
    prolog_query: String,
) -> Result<Value> {
    let response: Response<infer::ResponseData> = query_raw(
        executor_url,
        cap_token,
        Infer::build_query(infer::Variables {
            uuid,
            query: prolog_query,
        }),
    )
    .await?;

    if let Some(data) = response.data {
        let v: Value = serde_json::from_str(&data.perspective_query_prolog)?;
        Ok(match v {
            Value::String(string) => {
                if string == "true" {
                    Value::Bool(true)
                } else if string == "false" {
                    Value::Bool(false)
                } else {
                    Value::String(string)
                }
            }
            _ => v,
        })
    } else {
        if let Some(errors) = response.errors.clone() {
            if let Some(error) = errors.first() {
                if error.message.starts_with("error(") {
                    return Err(anyhow!(error.message.clone()));
                }
            }
        }
        Err(anyhow!(
            "Failed to run perspective->infer query: {:?}",
            response.errors
        ))
    }
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug"
)]
pub struct SubscriptionLinkAdded;

pub async fn watch(
    executor_url: String,
    cap_token: String,
    id: String,
    link_callback: Box<dyn Fn(LinkExpression)>,
) -> Result<()> {
    use futures::StreamExt;

    let mut client = create_websocket_client(executor_url, cap_token)
        .await
        .with_context(|| "Failed to create websocket client")?;

    let mut stream = client
        .streaming_operation(StreamingOperation::<SubscriptionLinkAdded>::new(
            subscription_link_added::Variables { uuid: id.clone() },
        ))
        .await
        .with_context(|| "Failed to subscribe to perspectiveLinkAdded")?;

    println!(
        "Successfully subscribed to perspectiveLinkAdded for perspective {}",
        id
    );
    println!("Waiting for events...");

    while let Some(item) = stream.next().await {
        match item {
            Ok(response) => {
                if let Some(link) = response.data.and_then(|data| data.perspective_link_added) {
                    link_callback(link.into())
                }
            }
            Err(e) => {
                println!("Received Error: {:?}", e);
            }
        }
    }

    println!("Stream ended. Exiting...");

    Ok(())
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug"
)]
pub struct Snapshot;

pub async fn snapshot(
    executor_url: String,
    cap_token: String,
    uuid: String,
) -> Result<Perspective> {
    let response: snapshot::ResponseData = query(
        executor_url,
        cap_token,
        Snapshot::build_query(snapshot::Variables { uuid }),
    )
    .await
    .with_context(|| "Failed to run perspectives->snapshot query")?;
    Ok(response
        .perspective_snapshot
        .ok_or_else(|| anyhow!("No perspective found"))?
        .into())
}

pub struct PerspectivesClient {
    info: Arc<ClientInfo>,
}

impl PerspectivesClient {
    pub fn new(info: Arc<ClientInfo>) -> Self {
        Self { info }
    }

    pub async fn all(&self) -> Result<Vec<AllPerspectives>> {
        all(self.info.executor_url.clone(), self.info.cap_token.clone()).await
    }

    pub async fn add(&self, name: String) -> Result<String> {
        add(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            name,
        )
        .await
    }

    pub async fn remove(&self, uuid: String) -> Result<()> {
        remove(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uuid,
        )
        .await
    }

    pub async fn add_link(
        &self,
        uid: String,
        source: String,
        target: String,
        predicate: Option<String>,
    ) -> Result<AddLinkPerspectiveAddLink> {
        add_link(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uid,
            source,
            target,
            predicate,
        )
        .await
    }

    pub async fn add_local_link(
        &self,
        uid: String,
        source: String,
        target: String,
        predicate: Option<String>,
    ) -> Result<AddLocalLinkPerspectiveAddLocalLink> {
        add_local_link(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uid,
            source,
            target,
            predicate,
        )
        .await
    }

    pub async fn query_links(
        &self,
        uuid: String,
        source: Option<String>,
        target: Option<String>,
        predicate: Option<String>,
        from_date: Option<DateTime>,
        until_date: Option<DateTime>,
        limit: Option<f64>,
    ) -> Result<Vec<query_links::QueryLinksPerspectiveQueryLinks>> {
        query_links(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uuid,
            source,
            target,
            predicate,
            from_date,
            until_date,
            limit,
        )
        .await
    }

    pub async fn infer(&self, uuid: String, prolog_query: String) -> Result<Value> {
        infer(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uuid,
            prolog_query,
        )
        .await
    }

    pub async fn watch(
        &self,
        id: String,
        link_callback: Box<dyn Fn(LinkExpression)>,
    ) -> Result<()> {
        watch(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            id,
            link_callback,
        )
        .await
    }

    pub async fn snapshot(&self, uuid: String) -> Result<Perspective> {
        snapshot(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uuid,
        )
        .await
    }
}
