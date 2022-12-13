use ad4m_client::{Ad4mClient};
use anyhow::{Result};
use regex::Regex;
use rustyline::Editor;

use crate::formatting::print_prolog_results;

pub async fn repl_loop(ad4m_client: Ad4mClient, id: String) -> Result<()> {
    let mut rl = Editor::<()>::new()?;
    loop {
        let line = rl.readline("\x1b[97m> ")?;
        rl.add_history_entry(line.as_str());
        let line = line.trim().to_string();
        if line == "exit" {
            break;
        }

        let add_link = Regex::new(
            r"add-link\s+(?P<source>\S+)\s+(?P<predicate>\S+)\s+(?P<target>\S+)",
        )?;
        let caps = add_link.captures(&line);
        if let Some(caps) = caps {
            let source = caps.name("source").unwrap().as_str().to_string();
            let predicate = caps.name("predicate").unwrap().as_str().to_string();
            let target = caps.name("target").unwrap().as_str().to_string();

            let predicate = if predicate == "_" {
                None
            } else {
                Some(predicate)
            };

            ad4m_client
                .perspectives
                .add_link(id.clone(), source, target, predicate)
                .await?;
            continue;
        }

        match ad4m_client.perspectives.infer(id.clone(), line).await {
            Ok(results) => {
                print_prolog_results(results)?;
            }
            Err(e) => {
                println!("\x1b[91m{}", e.root_cause());
            }
        }
    }
    Ok(())
}
