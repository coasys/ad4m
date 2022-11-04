extern crate clap;
extern crate anyhow;
extern crate graphql_client;
extern crate reqwest;
extern crate tokio;
extern crate rustyline;

mod agent;

use clap::Parser;
use anyhow::{Result};
use rustyline::error::ReadlineError;
use rustyline::{Editor};


#[tokio::main]
async fn main() -> Result<()> {
    //let path = "test.txt";
    //let content = std::fs::read_to_string(path)
    //    .with_context(|| format!("could not read file `{}`", path))?;
    //println!("file content: {}", content);

    if let Ok(request_id) = agent::run_request_capability().await {
        println!("Got request id: {:#?}", request_id);

        let mut rl = Editor::<()>::new()?;
        if let Ok(rand) = rl.readline("Enter random string: ") {
            if let Ok(jwt) = agent::run_retrieve_capability(request_id, rand).await {
                println!("Got jwt: {:#?}", jwt);
            }
        }
        
    } else {
        println!("Error requesting capability");
    }
    

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