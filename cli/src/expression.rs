use ad4m_client::Ad4mClient;
use anyhow::Result;
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
            let content_str = serde_json::from_str::<Value>(&content)
                .map(|v| v.to_string())
                .unwrap_or_else(|_| format!("\"{}\"", content));
            let expression_url = ad4m_client
                .expressions
                .expression_create(content_str, language_address)
                .await?;
            println!("Expression created with url: {}", expression_url);
        }
        ExpressionFunctions::Get { url } => {
            let content: Value = ad4m_client.expressions.expression(url.clone()).await?;
            if content.is_null() {
                println!("No expression found at url: {}", url);
            } else {
                if let Some(author) = content.get("author") {
                    println!("author: {}", author);
                }
                if let Some(timestamp) = content.get("timestamp") {
                    println!("timestamp: {}", timestamp);
                }
                if let Some(data) = content.get("data") {
                    println!("data: {}", data);
                }
            }
        }
        ExpressionFunctions::GetRaw { url } => {
            let content: Value = ad4m_client.expressions.expression(url.clone()).await?;
            if content.is_null() {
                println!("No expression found at url: {}", url);
            } else if let Some(data) = content.get("data") {
                println!("{}", data);
            }
        }
    };
    Ok(())
}
