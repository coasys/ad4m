extern crate clap;
extern crate anyhow;
extern crate graphql_client;
extern crate reqwest;
extern crate tokio;
extern crate rustyline;
extern crate dirs;

mod agent;
mod startup;

use clap::Parser;
use anyhow::{Result};

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

#[tokio::main]
async fn main() -> Result<()> {
    //let args = Args::parse();

    let cap_token = startup::get_cap_token().await?;

    println!("cap token: {}", cap_token);

    Ok(())
}
