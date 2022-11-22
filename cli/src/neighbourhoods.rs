use ad4m_client::neighbourhoods;
use anyhow::{Result};
use clap::{Subcommand};


#[derive(Debug, Subcommand)]
pub enum NeighbourhoodFunctions {
    Create {
        perspective_id: String,
        link_language: String,
    },
    Join {
        url: String,
    },
}

pub async fn run(
    cap_token: String,
    command: NeighbourhoodFunctions,
) -> Result<()> {
    match command {
        NeighbourhoodFunctions::Create {
            perspective_id,
            link_language,
        } => {
            let neighbourhood =
                neighbourhoods::run_publish(cap_token, link_language, None, perspective_id)
                    .await?;
            println!("Neighbourhood shared as: {}", neighbourhood);
        }
        NeighbourhoodFunctions::Join { url } => {
            let neighbourhood = neighbourhoods::run_join(cap_token, url).await?;
            println!("Neighbourhod joined!\n{:#?}", neighbourhood);
        }
    };
    Ok(())
}