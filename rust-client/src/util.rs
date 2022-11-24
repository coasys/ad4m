use anyhow::{anyhow, Result};
use async_tungstenite::tungstenite::{client::IntoClientRequest, http::HeaderValue, Message};
use futures::StreamExt;
use graphql_client::{QueryBody, Response};
use graphql_ws_client::{graphql::GraphQLClient, AsyncWebsocketClient, GraphQLClientClientBuilder};
use serde::{de::DeserializeOwned, Serialize};

pub async fn query<Q, R>(executor_url: String, cap_token: String, query: QueryBody<Q>) -> Result<R>
where
    Q: Serialize,
    R: DeserializeOwned,
{
    let response_body: Response<R> = reqwest::Client::new()
        .post(executor_url)
        .header("Authorization", cap_token)
        .json(&query)
        .send()
        .await?
        .json()
        .await?;
    let response_data = response_body
        .data
        .ok_or_else(|| anyhow!("No data in response! Errors: {:?}", response_body.errors))?;
    Ok(response_data)
}

pub async fn query_raw<Q, R>(executor_url: String, cap_token: String, query: QueryBody<Q>) -> Result<Response<R>>
where
    Q: Serialize,
    R: DeserializeOwned,
{
    Ok(reqwest::Client::new()
        .post(executor_url)
        .header("Authorization", cap_token)
        .json(&query)
        .send()
        .await?
        .json()
        .await?)
}

struct TokioSpawner(tokio::runtime::Handle);

impl TokioSpawner {
    pub fn new(handle: tokio::runtime::Handle) -> Self {
        TokioSpawner(handle)
    }

    pub fn current() -> Self {
        TokioSpawner::new(tokio::runtime::Handle::current())
    }
}

impl futures::task::Spawn for TokioSpawner {
    fn spawn_obj(
        &self,
        obj: futures::task::FutureObj<'static, ()>,
    ) -> Result<(), futures::task::SpawnError> {
        self.0.spawn(obj);
        Ok(())
    }
}

pub async fn create_websocket_client(
    executor_url: String,
    cap_token: String,
) -> Result<AsyncWebsocketClient<GraphQLClient, Message>> {
    let url = executor_url.replace("http", "ws");
    let mut request = url.into_client_request().unwrap();
    request.headers_mut().insert(
        "Sec-WebSocket-Protocol",
        HeaderValue::from_str("graphql-transport-ws").unwrap(),
    );
    request
        .headers_mut()
        .insert("Authorization", HeaderValue::from_str(&cap_token).unwrap());
    let (connection, _) = async_tungstenite::tokio::connect_async(request).await?;

    let (sink, stream) = connection.split();
    Ok(GraphQLClientClientBuilder::new()
        .build(stream, sink, TokioSpawner::current())
        .await
        .unwrap())
}
