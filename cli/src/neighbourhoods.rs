use ad4m_client::Ad4mClient;
use anyhow::Result;
use clap::Subcommand;

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

pub async fn run(ad4m_client: Ad4mClient, command: NeighbourhoodFunctions) -> Result<()> {
    match command {
        NeighbourhoodFunctions::Create {
            perspective_id,
            link_language,
        } => {
            let neighbourhood =
            ad4m_client.neighbourhoods.publish(link_language, None, perspective_id).await?;
            println!("Neighbourhood shared as: {}", neighbourhood);
        }
        NeighbourhoodFunctions::Join { url } => {
            let neighbourhood = ad4m_client.neighbourhoods.join(url).await?;
            println!("Neighbourhod joined!\n{:#?}", neighbourhood);
        }
    };
    Ok(())
}
