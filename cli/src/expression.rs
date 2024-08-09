use ad4m_client::{expressions::expression, Ad4mClient};
use anyhow::{bail, Result};
use clap::Subcommand;
use serde_json::Value;

#[derive(Debug, Subcommand)]
pub enum ExpressionFunctions {
    Create {
        language_address: String,
        content: String,
    },
    Get {
        url: String,
    },
    GetRaw {
        url: String,
    },
}

pub async fn run(ad4m_client: Ad4mClient, command: ExpressionFunctions) -> Result<()> {
    match command {
        ExpressionFunctions::Create {
            language_address,
            content,
        } => {
            let content = serde_json::from_str::<Value>(&content).unwrap_or_else(|_| {
                serde_json::from_str::<Value>(&format!("\"{}\"", content)).unwrap()
            });
            let expression_url = ad4m_client
                .expressions
                .expression_create(language_address, content)
                .await?;
            println!("Expression created with url: {}", expression_url);
        }
        ExpressionFunctions::Get { url } => {
            let maybe_content: Option<expression::ExpressionExpression> =
                ad4m_client.expressions.expression(url.clone()).await?;
            match maybe_content {
                Some(content) => {
                    println!("author: {}", content.author);
                    println!("timestamp: {}", content.timestamp);
                    if content.proof.valid.unwrap_or(false) {
                        println!("signature: ✅");
                    } else {
                        println!("signature: ❌");
                    }
                    println!("data: {}", content.data)
                }
                None => println!("No expression found at url: {}", url),
            }
        }

        ExpressionFunctions::GetRaw { url } => {
            let maybe_content: Option<expression::ExpressionExpression> =
                ad4m_client.expressions.expression(url.clone()).await?;
            match maybe_content {
                Some(content) => {
                    if let Ok(Value::String(content)) = serde_json::from_str::<Value>(&content.data)
                    {
                        println!("{}", &content);
                    } else {
                        println!("{}", content.data);
                    }
                }
                None => bail!("No expression found at url: {}", url),
            }
        }
    };
    Ok(())
}
