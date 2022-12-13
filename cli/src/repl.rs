use ad4m_client::perspective_proxy::PerspectiveProxy;
use anyhow::{Result};
use regex::Regex;
use rustyline::Editor;
use syntect::easy::HighlightLines;
use syntect::parsing::SyntaxSet;
use syntect::highlighting::{ThemeSet, Style};
use syntect::util::{as_24_bit_terminal_escaped, LinesWithEndings};

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

        if line == "sdna" {
            let dna_zomes = perspective.get_dna().await?;

            // Load these once at the start of your program
            let ps = SyntaxSet::load_defaults_newlines();
            let ts = ThemeSet::load_defaults();

            let syntax = ps.find_syntax_by_extension("pl").unwrap();
            let mut h = HighlightLines::new(syntax, &ts.themes["Solarized (light)"]);

            for zome in dna_zomes {
                println!("\x1b[97m================");
                for line in LinesWithEndings::from(&zome) {
                    let ranges: Vec<(Style, &str)> = h.highlight_line(line, &ps).unwrap();
                    let escaped = as_24_bit_terminal_escaped(&ranges[..], false);
                    print!("{}", escaped);
                }
                println!("\x1b[97m================");
            }
            continue;
        }

        let subject_create = Regex::new(
            r"subject.construct\s+(?P<class>\S+)\s+(?P<base>\S+)",
        )?;
        let caps = subject_create.captures(&line);
        if let Some(caps) = caps {
            let class = caps.name("class").unwrap().as_str().to_string();
            let base = caps.name("base").unwrap().as_str().to_string();

            match perspective.create_subject(&class, &base).await {
                Ok(()) => {
                    println!("\x1b[36mSubject of class {} Created at: \x1b[97m{}", class, base);
                }
                Err(e) => {
                    println!("\x1b[91m{}", e.root_cause());
                }
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
