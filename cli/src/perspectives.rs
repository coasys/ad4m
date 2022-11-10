use graphql_client::{GraphQLQuery};
use graphql_ws_client::graphql::StreamingOperation;
use serde_json::{json, Value};
use crate::{util::query, startup::get_executor_url};
use anyhow::{Result, Context};
use chrono::naive::NaiveDateTime;

type DateTime = NaiveDateTime;

use self::all::AllPerspectives;
use self::add_link::AddLinkPerspectiveAddLink;
//use websocket::{ClientBuilder, Message};
//use websocket::header::{Headers, Authorization};

//use graphql_ws_client::{GraphQLClientClientBuilder};
//use async_tungstenite::tungstenite::{client::IntoClientRequest, http::HeaderValue};

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug",
)]
pub struct All;

pub async fn run_all(cap_token: String) -> Result<Vec<AllPerspectives>> {
    let response_data: all::ResponseData = query(cap_token, All::build_query(all::Variables {}))
        .await
        .with_context(|| "Failed to run perspectives->all query")?;
    Ok(response_data.perspectives)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug",
)]
pub struct Add;

pub async fn run_add(cap_token: String, name: String) -> Result<String> {
    let response_data: add::ResponseData = query(cap_token, Add::build_query(add::Variables { name }))
        .await
        .with_context(|| "Failed to run perspectives->add query")?;
    Ok(response_data.perspective_add.uuid)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug",
)]
pub struct Remove;

pub async fn run_remove(cap_token: String, uuid: String) -> Result<()> {
    query(cap_token, Remove::build_query(remove::Variables { uuid }))
        .await
        .with_context(|| "Failed to run perspectives->remove query")?;
    Ok(())
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug",
)]
pub struct AddLink;

pub async fn run_add_link(cap_token: String, uuid: String, source: String, target: String, predicate: Option<String>) -> Result<AddLinkPerspectiveAddLink> {
    let response_data: add_link::ResponseData = query(
        cap_token, 
        AddLink::build_query(add_link::Variables { 
            uuid, 
            link: add_link::LinkInput {
                source,
                target,
                predicate,
            }
        })
    )
        .await
        .with_context(|| "Failed to run perspectives->addLink query")?;
    
    Ok(response_data.perspective_add_link)
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug",
)]
pub struct QueryLinks;

pub async fn run_query_links(
    cap_token: String, 
    uuid: String, 
    source: Option<String>, 
    target: Option<String>, 
    predicate: Option<String>,
    from_date: Option<DateTime>,
    until_date: Option<DateTime>,
    limit: Option<f64>
) -> Result<Vec<query_links::QueryLinksPerspectiveQueryLinks>> {

    let response_data: query_links::ResponseData = query(
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
            }
        })
    )
        .await
        .with_context(|| "Failed to run perspectives->queryLinks query")?;

    Ok(response_data.perspective_query_links.unwrap_or_default())
}
 

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug",
)]
pub struct Infer;

pub async fn run_infer(cap_token: String, uuid: String, prolog_query: String) -> Result<Value> {
    let response_data: infer::ResponseData = query(cap_token, Infer::build_query(infer::Variables { uuid, query: prolog_query }))
        .await
        .with_context(|| "Failed to run perspectives->infer query")?;
    let v: Value = serde_json::from_str(&response_data.perspective_query_prolog)?;
    Ok(match v {
        Value::String(string) => {
            if string == "true" {
                Value::Bool(true)
            } else if string == "false" {
                Value::Bool(false)
            } else {
                Value::String(string)
            }
        },
        _ => v,
    })
}
/*
mod schema {
    cynic::use_schema!("../core/lib/src/schema.gql");
}

#[derive(cynic::QueryFragment, Debug)]
#[cynic(schema_path = "../core/lib/src/schema.gql", graphql_type = "Link")]
struct Link {
    predicate: Option<String>,
    source: String,
    target: String,
}

#[derive(cynic::QueryFragment, Debug)]
#[cynic(schema_path = "../core/lib/src/schema.gql", graphql_type = "ExpressionProof")]
struct ExpressionProof {
    invalid: Option<bool>,
    key: String,
    signature: String,
    valid: Option<bool>,
}

#[derive(cynic::QueryFragment, Debug)]
#[cynic(schema_path = "../core/lib/src/schema.gql", graphql_type = "LinkExpression")]
struct LinkExpression {
    author: String,
    data: Link,
    proof: ExpressionProof,
    timestamp: String,
}

#[derive(cynic::QueryFragment, Debug)]
#[cynic(schema_path = "../core/lib/src/schema.gql", graphql_type = "AgentStatus")]
struct AgentStatus {
    did: Option<String>,
    did_document: Option<String>,
    error: Option<String>,
    is_initialized: bool,
    is_unlocked: bool,
}

#[derive(cynic::QueryFragment, Debug)]
#[cynic(
    schema_path = "../core/lib/src/schema.gql",
    graphql_type = "Subscription"
)]
struct AgentStatusChangedSubscription {
    agent_status_changed: AgentStatus,
}

#[derive(cynic::QueryFragment, Debug)]
#[cynic(
    schema_path = "../core/lib/src/schema.gql",
    graphql_type = "SubscriptionRoot"
)]
struct LinkAddedSubscription {
    #[arguments(uuid = "uuid")]
    perspective_link_added: Option<LinkExpression>,
}
 */
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

//fn build_query() -> cynic::StreamingOperation<'static, LinkAddedSubscription> {
//    use cynic::SubscriptionBuilder;
//    LinkAddedSubscription::build(())
//}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug",
)]
pub struct SubscriptionLinkAdded;

pub async fn run_watch(cap_token: String, id: String) -> Result<()> {
    
    use async_tungstenite::tungstenite::{client::IntoClientRequest, http::HeaderValue};
    use futures::StreamExt;
    use graphql_ws_client::GraphQLClientClientBuilder;

    let url = get_executor_url()?.replace("http", "ws");
    let mut request = url.into_client_request().unwrap();
    request.headers_mut().insert(
        "Sec-WebSocket-Protocol",
        HeaderValue::from_str("graphql-transport-ws").unwrap(),
    );
    request.headers_mut().insert(
        "Authorization",
        HeaderValue::from_str(&cap_token).unwrap(),
    );
    let (connection, _) = async_tungstenite::tokio::connect_async(request)
        .await?;

    println!("Connected");

    let (sink, stream) = connection.split();


    let mut client = GraphQLClientClientBuilder::new()
        .build(stream, sink, TokioSpawner::current())
        .await
        .unwrap();

    let op = StreamingOperation::<SubscriptionLinkAdded>::new(subscription_link_added::Variables {
        uuid: id,
    });

    //let query_op = StreamingOperation::<QueryLinks>::new(query_links::Variables { 
    //    uuid: id,
    //    query: query_links::LinkQuery {
    //        source: None, 
    //        target: None, 
    //        predicate: None,
    //        fromDate: None,
    //        untilDate: None,
    //        limit: None,
    //    }
    //});

    let mut stream = client.streaming_operation(op).await.unwrap();
    println!("Running subscription apparently?");
    while let Some(item) = stream.next().await {
        println!("{:?}", item);
    }

    println!("after loop");
  

 /*
    let (connection, _) = async_tungstenite::tokio::connect_async(request)
        .await
        .unwrap();

    println!("Connected");

    let (sink, stream) = connection.split();

    let mut client = GraphQLClientClientBuilder::new()
        .build(stream, sink, TokioSpawner::current())
        .await
        .unwrap();

    let mut stream = client.streaming_operation(build_query()).await.unwrap();
    println!("Running subscription apparently?");
    while let Some(item) = stream.next().await {
        println!("{:?}", item);
    }
  */
    /*
    let mut headers = Headers::new();
    headers.set(Authorization(cap_token));
    let executor_url = get_executor_url()?;
    let mut client = ClientBuilder::new("ws://localhost:12000/graphql")
        .unwrap()
        .custom_headers(&headers) 
        .add_protocol("graphql-transport-ws") 
        .connect(None)
        .unwrap();

    client.send_message(&Message::text(serde_json::to_string(&json!({
        "type": "connection_init"
    }))?
    )).unwrap();
    let init_response = client.recv_message();
    println!("Init response: {:?}", init_response);

    //while let Ok(message) = client.recv_message() {
    //    println!("After init: Received message: {:?}", message);
    //}

    /* 
    let message = r#"
    {
        "id":"dc04f846-fb97-42b0-bfb8-7ddf0540aedf",
        "type":"subscribe",
        "payload":{
            "variables":{
                "uuid":"da0333e9-275d-4b57-8851-0d1678d75a1c",
                "query":{
                    "source":"ad4m://self",
                    "predicate":"flux://has_channel"
                }
            },
            "extensions":{},
            "operationName": "perspectiveQueryLinks",
            "query":"query perspectiveQueryLinks($uuid: String!, $query: LinkQuery!) {\n  perspectiveQueryLinks(query: $query, uuid: $uuid) {\n    author\n    timestamp\n    data {\n      source\n      predicate\n      target\n    }\n    proof {\n      valid\n      invalid\n      signature\n      key\n    }\n  }\n}\n"
        }
    }"#;
    client.send_message(&Message::text(message)).unwrap();
     
    */
    
    //let gql_query = format!("perspectiveLinkAdded() {{ author timestamp data {{ source, predicate, target }} proof {{ valid, invalid, signature, key }} }}", id);
    client.send_message(&Message::text(serde_json::to_string(&json!({
        "type": "subscribe",
        "id": "1",
        "payload": {
            "variables":{
                "uuid": id,
                //"query":{
                //    "source":"ad4m://self"
                //}
            },
            "extensions":{},
            "operationName": "perspectiveLinkAdded",
            "query": "query perspectiveLinkAdded($uuid: String!) { author timestamp data { source, predicate, target } proof { valid, invalid, signature, key } }"
        },
    }))?
    )).unwrap(); 

    while let Ok(message) = client.recv_message() {
        println!("Received message: {:?}", message);
    }
        
 */
    Ok(())
}