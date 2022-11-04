use graphql_client::{GraphQLQuery, Response};
use crate::startup::get_executor_url;
use anyhow::{Result, anyhow};
//use chrono::{DateTime as DT, Utc};

//type DateTime = DT<Utc>;

use self::all::AllPerspectives;
use self::add_link::AddLinkPerspectiveAddLink;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "src/perspectives.gql",
    response_derives = "Debug",
)]
pub struct All;

pub async fn run_all(cap_token: String) -> Result<Vec<AllPerspectives>> {
    let query = All::build_query(all::Variables {});
    let response_body: Response<all::ResponseData> = reqwest::Client::new()
        .post(get_executor_url()?)
        .header("Authorization", cap_token) 
        .json(&query)
        .send()
        .await?
        .json()
        .await?;

    let response_data = response_body.data.ok_or(anyhow!("No data in response"))?;
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
    let query = Add::build_query(add::Variables { name });
    let response_body: Response<add::ResponseData> = reqwest::Client::new()
        .post(get_executor_url()?)
        .header("Authorization", cap_token) 
        .json(&query)
        .send()
        .await?
        .json()
        .await?;

    let response_data = response_body.data.ok_or(anyhow!("No data in response"))?;
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
    let query = Remove::build_query(remove::Variables { uuid });
    let response_body: Response<remove::ResponseData> = reqwest::Client::new()
        .post(get_executor_url()?)
        .header("Authorization", cap_token) 
        .json(&query)
        .send()
        .await?
        .json()
        .await?;
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
    let query = AddLink::build_query(add_link::Variables { 
        uuid, 
        link: add_link::LinkInput {
            source,
            target,
            predicate,
        }
    });
       
    let response_body: Response<add_link::ResponseData> = reqwest::Client::new()
        .post(get_executor_url()?)
        .header("Authorization", cap_token) 
        .json(&query)
        .send()
        .await?
        .json()
        .await?;
    
    let response_data = response_body.data.ok_or(anyhow!("No data in response"))?;
    Ok(response_data.perspective_add_link)
}
/*
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
    predicate: Option<String>
) -> Result<Vec<query_links::QueryLinksPerspectiveQueryLinks>> {
    let query = QueryLinks::build_query(query_links::Variables { 
        uuid, 
        query: query_links::LinkQuery {
            source,
            target,
            predicate,
            from_date: None,
            until_date: None,
            limit: None,
        }
    });
       
    let response_body: Response<query_links::ResponseData> = reqwest::Client::new()
        .post(get_executor_url()?)
        .header("Authorization", cap_token) 
        .json(&query)
        .send()
        .await?
        .json()
        .await?;
    
    let response_data = response_body.data.ok_or(anyhow!("No data in response"))?;
    Ok(response_data.perspective_query_links.unwrap_or(vec![]))
}
 */