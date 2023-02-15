use std::sync::Arc;

use crate::{util::query, ClientInfo};
use anyhow::{Context, Result};
use graphql_client::GraphQLQuery;
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(GraphQLQuery, Debug)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/expressions.gql",
    response_derives = "Debug"
)]
pub struct ExpressionCreate;

pub async fn expression_create(
    executor_url: String,
    cap_token: String,
    language_address: String,
    content: Value,
) -> Result<String> {
    let content = serde_json::to_string(&content)?;
    let response_data: expression_create::ResponseData = query(
        executor_url,
        cap_token,
        ExpressionCreate::build_query(expression_create::Variables {
            language_address,
            content,
        }),
    )
    .await
    .with_context(|| "Failed to run expressions->create mutation")?;
    Ok(response_data.expression_create)
}

#[derive(GraphQLQuery, Debug, Serialize, Deserialize)]
#[graphql(
    schema_path = "schema.gql",
    query_path = "src/expressions.gql",
    response_derives = "Debug"
)]
pub struct Expression;

pub async fn expression(
    executor_url: String,
    cap_token: String,
    url: String,
) -> Result<Option<expression::ExpressionExpression>> {
    let response_data: expression::ResponseData = query(
        executor_url,
        cap_token,
        Expression::build_query(expression::Variables { url }),
    )
    .await
    .with_context(|| "Failed to run expressions->get query")?;
    Ok(response_data.expression)
}

pub struct ExpressionsClient {
    info: Arc<ClientInfo>,
}

impl ExpressionsClient {
    pub fn new(info: Arc<ClientInfo>) -> Self {
        Self { info }
    }

    pub async fn expression_create(
        &self,
        language_address: String,
        content: Value,
    ) -> Result<String> {
        expression_create(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            language_address,
            content,
        )
        .await
    }

    pub async fn expression(
        &self,
        url: String,
    ) -> Result<Option<expression::ExpressionExpression>> {
        expression(
            self.info.executor_url.clone(),
            self.info.cap_token.clone(),
            url,
        )
        .await
    }
}
