use anyhow::{Result, anyhow};
use graphql_client::{Response, QueryBody};
use serde::{Serialize, de::DeserializeOwned};
use crate::startup::get_executor_url;

pub async fn query<Q,R>(cap_token: String, query: QueryBody<Q>) -> Result<R>
where Q: Serialize,  R: DeserializeOwned {
    let response_body: Response::<R> = reqwest::Client::new()
        .post(get_executor_url()?)
        .header("Authorization", cap_token) 
        .json(&query)
        .send()
        .await?
        .json()
        .await?;
    let response_data = response_body.data.ok_or_else(|| anyhow!("No data in response"))?;
    Ok(response_data)
}

pub fn maybe_parse_datetime(maybe_date: Option<String>) -> Result<Option<chrono::naive::NaiveDateTime>> {
    let datetime_format = "%Y-%m-%dT%H:%M:%S%.fZ";
    Ok( match maybe_date.clone().map(|s| chrono::naive::NaiveDateTime::parse_from_str(&s, datetime_format)) {
        Some(Err(e)) => {
            return Err(anyhow!(e).context(format!("Couldn't parse datetime '{}' with expected format: {}", maybe_date.unwrap(), datetime_format)));
        },
        Some(Ok(x)) => Some(x),
        None => None,
    })
}