use ad4m_client::perspective_proxy::PerspectiveProxy;
use anyhow::{Context, Result};
use regex::Regex;
use rustyline::Editor;
use syntect::easy::HighlightLines;
use syntect::highlighting::{Style, ThemeSet};
use syntect::parsing::SyntaxSet;
use syntect::util::{as_24_bit_terminal_escaped, LinesWithEndings};

use crate::formatting::{print_link, print_prolog_results};

async fn add_link(perspective: &PerspectiveProxy, line: &str) -> bool {
    // add_link(source, predicate, target)
    let add_link = Regex::new(
        r"add_link\((?P<source>\S+),\s*(?P<predicate>\S+),\s*(?P<status>\S+),\s*(?P<target>\S+)\)",
    )
    .expect("Error parsing add_link regex");
    let caps = add_link.captures(line);
    if let Some(caps) = caps {
        let source = caps.name("source").unwrap().as_str().to_string();
        let predicate = caps.name("predicate").unwrap().as_str().to_string();
        let target = caps.name("target").unwrap().as_str().to_string();
        let status = caps.name("status").unwrap().as_str().to_string();

        let predicate = if predicate == "_" {
            None
        } else {
            Some(predicate)
        };

        let status = if status == "_" { None } else { Some(status) };

        if let Err(e) = perspective
            .add_link(source, target, predicate, status)
            .await
        {
            println!("Error adding link: {}", e);
        }
        true
    } else {
        false
    }
}

async fn link_query(perspective: &PerspectiveProxy, line: &str) -> bool {
    // query(source, predicate, target)
    let link_query =
        Regex::new(r"query\(\s*(?P<source>\S+)?(,\s*(?P<predicate>\S+))?(,\s*(?P<target>\S+))?\)")
            .expect("Error parsing link_query regex");

    let caps = link_query.captures(line);
    if let Some(caps) = caps {
        let source = caps
            .name("source")
            .map(|x| x.as_str().to_string())
            .filter(|x| x != "_");
        let predicate = caps
            .name("predicate")
            .map(|x| x.as_str().to_string())
            .filter(|x| x != "_");
        let target = caps
            .name("target")
            .map(|x| x.as_str().to_string())
            .filter(|x| x != "_");

        match perspective
            .get(source, target, predicate, None, None, None)
            .await
        {
            Ok(links) => {
                for link in links {
                    print_link(link.into());
                }
            }
            Err(e) => println!("Error querying links: {}", e),
        }
        true
    } else {
        false
    }
}

async fn subject_classes(perspective: &PerspectiveProxy, line: &String) -> bool {
    if line == "subject.classes" {
        if let Ok(classes) = perspective.subject_classes().await {
            for class in classes {
                println!("\x1b[36mSubject Class: \x1b[97m{}", class);
            }
        }
        true
    } else {
        false
    }
}

async fn sdna(perspective: &PerspectiveProxy, line: &String) -> bool {
    if line == "sdna" {
        match perspective.get_dna().await {
            Ok(dna_zomes) => {
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
            }
            Err(e) => println!("Error getting dna: {}", e),
        }
        true
    } else if line.starts_with("sdna ") {
        // sdna <class> - show SDNA for a specific class
        let class_name = line[5..].trim();
        match perspective.get_dna_for_class(class_name).await {
            Ok(Some(sdna_code)) => {
                // Load these once at the start of your program
                let ps = SyntaxSet::load_defaults_newlines();
                let ts = ThemeSet::load_defaults();

                let syntax = ps.find_syntax_by_extension("pl").unwrap();
                let mut h = HighlightLines::new(syntax, &ts.themes["Solarized (light)"]);

                println!("\x1b[97m================");
                println!("\x1b[36mSDNA for class: \x1b[97m{}", class_name);
                println!("\x1b[97m================");
                for line in LinesWithEndings::from(&sdna_code) {
                    let ranges: Vec<(Style, &str)> = h.highlight_line(line, &ps).unwrap();
                    let escaped = as_24_bit_terminal_escaped(&ranges[..], false);
                    print!("{}", escaped);
                }
                println!("\x1b[97m================");
            }
            Ok(None) => {
                println!("\x1b[91mNo SDNA found for class: \x1b[97m{}", class_name);
            }
            Err(e) => println!("Error getting dna for class {}: {}", class_name, e),
        }
        true
    } else {
        false
    }
}

async fn subject_new(perspective: &PerspectiveProxy, line: &str) -> bool {
    // new <class>(<base>)
    let subject_new = Regex::new(r"new\s+(?P<class>\S+)\(\s*(?P<base>\S+)\s*\)")
        .expect("Error parsing subject_new regex");
    let caps = subject_new.captures(line);
    if let Some(caps) = caps {
        let class = caps.name("class").unwrap().as_str().to_string();
        let base = caps.name("base").unwrap().as_str().to_string();

        match perspective.create_subject(&class, &base).await {
            Ok(()) => {
                println!(
                    "\x1b[36mSubject of class {} Created at: \x1b[97m{}",
                    class, base
                );
            }
            Err(e) => {
                println!("\x1b[91m{}", e.root_cause());
            }
        }
        println!();
        true
    } else {
        false
    }
}

async fn subject_set_prop(perspective: &PerspectiveProxy, line: &str) -> Result<bool> {
    // subject(<base>)[<name>] = <value>
    let subject_set_prop =
        Regex::new(r"subject\(\s*(?P<base>\S+)\s*\)\[(?P<name>\S+)\][\s--<]*=\s*(?P<value>\S+)")
            .expect("Error parsing subject_set_prop regex");
    let caps = subject_set_prop.captures(line);

    if let Some(caps) = caps {
        let base = caps.name("base").unwrap().as_str().to_string();
        let name = caps.name("name").unwrap().as_str().to_string();
        let value = caps.name("value").unwrap().as_str().to_string();

        let classes = perspective
            .get_subject_classes(&base)
            .await
            .with_context(|| format!("Error getting subject classes at: {}", base))?;
        if classes.is_empty() {
            println!("\x1b[91mNo subject found at: \x1b[97m{}", base);
            return Ok(true);
        }
        let mut done = false;
        for class in &classes {
            if let Ok(subject) = perspective.get_subject(class, &base).await {
                if let Ok(()) = subject.set_property(&name, &value).await {
                    done = true;
                }
            }
        }
        if !done {
            println!("\x1b[91mNo subject class found at: '\x1b[97m{}\x1b[91m', that has a property named '{}'", base, name);
            println!("Found classes:");
            for class in &classes {
                println!(
                    "\t{} [{}]",
                    class,
                    perspective
                        .subject_class_properties(class)
                        .await
                        .with_context(|| format!(
                            "Getting subject class properties for class {}",
                            class
                        ))?
                        .join(", ")
                );
            }
        }
        println!();
        Ok(true)
    } else {
        Ok(false)
    }
}

async fn subject_add_collection(perspective: &PerspectiveProxy, line: &str) -> Result<bool> {
    // subject(<base>)[<name>] <= <value>
    let subject_add_collection =
        Regex::new(r"subject\(\s*(?P<base>\S+)\s*\)\[(?P<name>\S+)\]\s*<=\s*(?P<value>\S+)")
            .expect("Error parsing subject_add_collection regex");
    let caps = subject_add_collection.captures(line);

    if let Some(caps) = caps {
        let base = caps.name("base").unwrap().as_str().to_string();
        let name = caps.name("name").unwrap().as_str().to_string();
        let value = caps.name("value").unwrap().as_str().to_string();

        let classes = perspective
            .get_subject_classes(&base)
            .await
            .with_context(|| format!("Getting subject classs for {}", base))?;
        if classes.is_empty() {
            println!("\x1b[91mNo subject found at: \x1b[97m{}", base);
            return Ok(true);
        }
        let mut done = false;
        for class in &classes {
            if let Ok(subject) = perspective.get_subject(class, &base).await {
                if let Ok(()) = subject.add_collection(&name, &value).await {
                    done = true;
                }
            }
        }
        if !done {
            println!("\x1b[91mNo subject class found at: '\x1b[97m{}\x1b[91m', that has a collection named '{}'", base, name);
            println!("Found classes:");
            for class in &classes {
                println!(
                    "\t{} [{}]",
                    class,
                    perspective
                        .subject_class_properties(class)
                        .await
                        .with_context(|| format!(
                            "Getting subject class properties for class {}",
                            class
                        ))?
                        .join(", ")
                );
            }
        }
        println!();
        Ok(true)
    } else {
        Ok(false)
    }
}

async fn subject_print(perspective: &PerspectiveProxy, line: &str) -> Result<bool> {
    // subject(<base>)
    let subject_print = Regex::new(r"^\s*subject\(\s*(?P<base>\S+)\s*\)\s*$")
        .expect("Error parsing subject_print regex");
    let caps = subject_print.captures(line);

    if let Some(caps) = caps {
        let base = caps.name("base").unwrap().as_str().to_string();

        let classes = perspective
            .get_subject_classes(&base)
            .await
            .with_context(|| format!("Getting subject classes for {}", base))?;

        if classes.is_empty() {
            println!("\x1b[91mNo subject found at: \x1b[97m{}", base);
            return Ok(true);
        }
        for class in classes {
            match perspective.get_subject(&class, &base).await {
                Ok(subject) => {
                    let properties = subject
                        .get_property_values()
                        .await
                        .with_context(|| format!("Getting property values of {}", base))?;
                    let collections = subject
                        .get_collection_values()
                        .await
                        .with_context(|| format!("Getting collection values of {}", base))?;
                    println!("\x1b[90mSubject at:\n\x1b[95m{}\x1b[37m", base);
                    println!("================");
                    println!("\x1b[90mClass \x1b[36m{}:", class);
                    for (key, value) in properties {
                        println!("🧩\t \x1b[36m{}: \x1b[97m{}", key, value);
                    }
                    println!("================");
                    for (key, value) in collections {
                        println!("📦\t \x1b[36m{}: \x1b[97m{}", key, value.join(", "));
                    }
                }
                Err(e) => {
                    println!("\x1b[91m{}", e.root_cause());
                }
            }
        }
        println!();
        Ok(true)
    } else {
        Ok(false)
    }
}

pub async fn repl_loop(perspective: PerspectiveProxy) -> Result<()> {
    let mut rl = Editor::<()>::new()?;
    loop {
        let line = rl.readline("\x1b[97m> ")?;
        rl.add_history_entry(line.as_str());
        let line = line.trim().to_string();
        if line == "exit" {
            break;
        }

        if add_link(&perspective, &line).await {
            continue;
        }

        if link_query(&perspective, &line).await {
            continue;
        }

        if subject_classes(&perspective, &line).await {
            continue;
        }

        if sdna(&perspective, &line).await {
            continue;
        }

        if subject_new(&perspective, &line).await {
            continue;
        }

        match subject_set_prop(&perspective, &line).await {
            Ok(true) => continue,
            Ok(false) => (),
            Err(e) => {
                println!("\x1b[91m{}", e.root_cause());
                continue;
            }
        }

        match subject_add_collection(&perspective, &line).await {
            Ok(true) => continue,
            Ok(false) => (),
            Err(e) => {
                println!("\x1b[91m{}", e.root_cause());
                continue;
            }
        }

        match subject_print(&perspective, &line).await {
            Ok(true) => continue,
            Ok(false) => (),
            Err(e) => {
                println!("\x1b[91m{}", e.root_cause());
                continue;
            }
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
