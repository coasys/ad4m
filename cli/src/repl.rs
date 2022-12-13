use ad4m_client::perspective_proxy::PerspectiveProxy;
use anyhow::{Result};
use regex::Regex;
use rustyline::Editor;

use crate::formatting::print_prolog_results;

pub async fn repl_loop(perspective: PerspectiveProxy) -> Result<()> {
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

            perspective
                .add_link(source, target, predicate)
                .await?;
            continue;
        }

        if line == "subject.classes" {
            for class in perspective.subject_classes().await? {
                println!("\x1b[36mSubject Class: \x1b[97m{}", class);
            }
            continue;
        }

        match perspective.infer(line).await {
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
