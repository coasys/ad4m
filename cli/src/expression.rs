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
            let content = ad4m_client.expressions.expression(url).await?;
            println!("Expression data fetched:\n{:#?}", content);
        }
    };
    Ok(())
}
