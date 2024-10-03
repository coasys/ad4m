use std::sync::Arc;

use crate::perspective_proxy::PerspectiveProxy;
use crate::types::{LinkExpression, Perspective};
use crate::util::{create_websocket_client, query, query_raw};
use crate::ClientInfo;
use anyhow::{anyhow, Context, Result};
use chrono::naive::NaiveDateTime;
use futures::StreamExt;
use graphql_client::{GraphQLQuery, Response};
use graphql_ws_client::graphql::StreamingOperation;
use serde_json::Value;

type DateTime = NaiveDateTime;

use self::add_link::AddLinkPerspectiveAddLink;
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
    status: Option<String>,
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
            status,
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
pub struct RemoveLink;

pub async fn remove_link(
    executor_url: String,
    cap_token: String,
    uuid: String,
    link: LinkExpression,
) -> Result<()> {
    let response_data: remove_link::ResponseData = query(
        executor_url,
        cap_token,
        RemoveLink::build_query(remove_link::Variables {
            uuid,
            link: remove_link::LinkExpressionInput {
                author: link.author,
                timestamp: link.timestamp,
                data: remove_link::LinkInput {
                    source: link.data.source,
                    target: link.data.target,
                    predicate: link.data.predicate,
                },
                proof: remove_link::ExpressionProofInput {
                    signature: link.proof.signature,
                    key: link.proof.key,
                    invalid: link.proof.invalid,
                    valid: link.proof.valid,
                },
                status: link.status,
            },
        }),
    )
    .await
    .with_context(|| "Failed to run perspectives->removeLink query")?;

    if response_data.perspective_remove_link {
        Ok(())
    } else {
        Err(anyhow!("Failed to remove link"))
    }
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug"
)]
pub struct QueryLinks;

#[allow(clippy::too_many_arguments)]
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
                from_date,
                until_date,
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

#[derive(Clone)]
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
        status: Option<String>,
    ) -> Result<AddLinkPerspectiveAddLink> {
        add_link(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uid,
            source,
            target,
            predicate,
            status,
        )
        .await
    }

    pub async fn remove_link(&self, uid: String, link: LinkExpression) -> Result<()> {
        remove_link(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            uid,
            link,
        )
        .await
    }

    #[allow(clippy::too_many_arguments)]
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

    pub async fn get(&self, uuid: String) -> Result<PerspectiveProxy> {
        self.all()
            .await?
            .iter()
            .find(|p| p.uuid == uuid)
            .ok_or_else(|| anyhow!("Perspective with ID {} not found!", uuid))?;

        Ok(PerspectiveProxy::new(self.clone(), uuid.clone()))
    }
}
