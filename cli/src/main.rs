extern crate clap;
extern crate anyhow;
extern crate graphql_client;
extern crate reqwest;
extern crate tokio;

use clap::Parser;
use anyhow::{Result};
use graphql_client::{GraphQLQuery, Response};

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "queries/agent.gql",
    response_derives = "Debug",
)]
pub struct AgentStatus;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "../core/lib/src/schema.gql",
    query_path = "queries/agent.gql",
    response_derives = "Debug",
)]
pub struct RequestCapability;

#[tokio::main]
async fn main() -> Result<()> {
    //let path = "test.txt";
    //let content = std::fs::read_to_string(path)
    //    .with_context(|| format!("could not read file `{}`", path))?;
    //println!("file content: {}", content);

    let query = RequestCapability::build_query(request_capability::Variables {
        app_name: "AD4M cli".to_string(),
        app_desc: "Command line administration tool for AD4M".to_string(),
        app_url: "org.perspect3vism.ad4m.cli".to_string(),
        capabilities: "[{\"with\":{\"domain\":\"*\",\"pointers\":[\"*\"]},\"can\":[\"*\"]}]".to_string(),
    });
    let response_body: Response<request_capability::ResponseData> = reqwest::Client::new()
        .post("http://localhost:12000/graphql")
        .json(&query)
        .send()
        .await?
        .json()
        .await?;
    println!("{:#?}", response_body);

    Ok(())
}
/// AD4M command line client
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
   /// Name of the person to greet
   #[arg(short, long)]
   name: String,

   /// Number of times to greet
   #[arg(short, long, default_value_t = 1)]
   count: u8,
}

/*
fn main() {
   let args = Args::parse();

   for _ in 0..args.count {
       println!("Hello {}!", args.name)
   }
}
 */