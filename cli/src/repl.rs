use ad4m_client::perspective_proxy::PerspectiveProxy;
use anyhow::{Context, Result};
use regex::Regex;
use rustyline::Editor;
use syntect::easy::HighlightLines;
use syntect::highlighting::ThemeSet;
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

async fn get_links(perspective: &PerspectiveProxy, line: &str) -> bool {
    // get_links(source, predicate, target)
    let get_links_regex = Regex::new(
        r"get_links\(\s*(?P<source>\S+)?(,\s*(?P<predicate>\S+))?(,\s*(?P<target>\S+))?\)",
    )
    .expect("Error parsing get_links regex");

    let caps = get_links_regex.captures(line);
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
            Err(e) => println!("Error getting links: {}", e),
        }
        true
    } else {
        false
    }
}

async fn all_links(perspective: &PerspectiveProxy, line: &String) -> bool {
    if line == "all-links" {
        match perspective.get(None, None, None, None, None, None).await {
            Ok(links) => {
                println!("\x1b[36mAll links in perspective:\x1b[0m");
                for link in links {
                    print_link(link.into());
                }
            }
            Err(e) => println!("Error getting all links: {}", e),
        }
        true
    } else {
        false
    }
}

async fn subject_classes(perspective: &PerspectiveProxy, line: &String) -> bool {
    if line == "classes" {
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

async fn help_command(_perspective: &PerspectiveProxy, line: &String) -> bool {
    if line == "help" || line == "?" {
        println!("\x1b[97m================");
        println!("\x1b[36mAD4M Perspective REPL Commands");
        println!("\x1b[97m================");
        println!();

        println!("\x1b[36m📚 Link Management:");
        println!("\x1b[97m  add_link(<source>, <predicate>, <status>, <target>)");
        println!("\x1b[90m    Add a new link to the perspective");
        println!("\x1b[90m    Use _ to omit predicate or status (source and target are required)");
        println!();
        println!("\x1b[97m  get_links(<source>, <predicate>, <target>)");
        println!("\x1b[90m    Get links with optional variables (use _ for any value)");
        println!("\x1b[90m    Call get_links() with no parameters to get all links");
        println!();
        println!("\x1b[97m  all-links");
        println!("\x1b[90m    Show all links in the perspective (same as get_links())");
        println!();

        println!("\x1b[36m🧬 Subject Classes:");
        println!("\x1b[97m  classes");
        println!("\x1b[90m    List all available subject classes");
        println!();
        println!("\x1b[97m  shapes [<class>]");
        println!("\x1b[90m    Show the registered classes: target class, shape hash, shape JSON");
        println!("\x1b[90m    Without <class>: every class; with <class>: only that one");
        println!();

        println!("\x1b[36m🏗️  Subject Management:");
        println!("\x1b[97m  new <class>(<base>)");
        println!("\x1b[90m    Create a new subject instance of the given class");
        println!();
        println!("\x1b[97m  subject(<base>)[<property>] = <value>");
        println!("\x1b[90m    Set a property value on a subject");
        println!();
        println!("\x1b[97m  subject(<base>)[<collection>] <= <value>");
        println!("\x1b[90m    Add a value to a collection property");
        println!();
        println!("\x1b[97m  subject(<base>)");
        println!("\x1b[90m    Display all properties and collections of a subject");
        println!();

        println!("\x1b[36m🔍 Prolog Queries:");
        println!("\x1b[97m  <prolog_query>");
        println!("\x1b[90m    Run any valid Prolog query against the perspective");
        println!("\x1b[90m    Examples:");
        println!("\x1b[90m      subject(X, 'Person')");
        println!("\x1b[90m      hasProperty(Subject, 'name', Name)");
        println!("\x1b[90m      friend(X, Y), hobby(Y, 'coding')");
        println!();

        println!("\x1b[36m🚪 System:");
        println!("\x1b[97m  help, ?");
        println!("\x1b[90m    Show this help message");
        println!();
        println!("\x1b[97m  clear");
        println!("\x1b[90m    Clear the screen");
        println!();
        println!("\x1b[97m  version");
        println!("\x1b[90m    Show detailed version and build information");
        println!();
        println!("\x1b[97m  exit");
        println!("\x1b[90m    Exit the REPL");
        println!();

        println!("\x1b[97m================");
        println!("\x1b[90mTip: Use uppercase letters for variables in Prolog queries");
        println!("\x1b[90mExample: ?Person knows Bob");
        println!("\x1b[97m================");
        true
    } else {
        false
    }
}

async fn system_commands(_perspective: &PerspectiveProxy, line: &String) -> bool {
    match line.as_str() {
        "clear" => {
            // Clear the screen by printing multiple newlines
            for _ in 0..50 {
                println!();
            }
            true
        }
        "version" => {
            println!(
                "\x1b[36mAD4M CLI Version: \x1b[97m{}",
                env!("CARGO_PKG_VERSION")
            );
            println!("\x1b[36mPackage: \x1b[97m{}", env!("CARGO_PKG_NAME"));

            let git_commit = option_env!("GIT_COMMIT_HASH").unwrap_or("unknown");
            let git_dirty = option_env!("GIT_DIRTY").unwrap_or("N/A");
            println!("\x1b[36mGit Commit: \x1b[97m{} ({})", git_commit, git_dirty);

            println!("\x1b[36mHomepage: \x1b[97m{}", env!("CARGO_PKG_HOMEPAGE"));
            println!(
                "\x1b[36mRepository: \x1b[97m{}",
                env!("CARGO_PKG_REPOSITORY")
            );
            println!("\x1b[36mLicense: \x1b[97m{}", env!("CARGO_PKG_LICENSE"));
            true
        }
        _ => false,
    }
}

/// `shapes [<class>]`: the registered classes (`perspective.shapes`), each
/// with its target class, shape hash and shape JSON.
async fn shapes(perspective: &PerspectiveProxy, line: &String) -> bool {
    if line != "shapes" && !line.starts_with("shapes ") {
        return false;
    }
    let names = line
        .strip_prefix("shapes ")
        .map(str::trim)
        .filter(|name| !name.is_empty())
        .map(|name| vec![name.to_string()]);
    match perspective.shapes(names.clone()).await {
        Ok(classes) if classes.is_empty() => match names {
            Some(names) => println!("\x1b[93mNo class named '{}'", names[0]),
            None => println!("\x1b[93mNo classes registered"),
        },
        Ok(classes) => {
            let ps = SyntaxSet::load_defaults_newlines();
            let ts = ThemeSet::load_defaults();
            let syntax = ps.find_syntax_by_extension("json").unwrap();
            for class in classes {
                let field = |key: &str| class[key].as_str().unwrap_or("").to_string();
                println!(
                    "\x1b[36m{} \x1b[90m{} \x1b[90msha256:{}",
                    field("name"),
                    class["shape"]["target_class"].as_str().unwrap_or(""),
                    field("hash")
                );
                let json = serde_json::to_string_pretty(&class["shape"]).unwrap_or_default();
                let mut h = HighlightLines::new(syntax, &ts.themes["base16-ocean.dark"]);
                for line in LinesWithEndings::from(&json) {
                    let ranges = h.highlight_line(line, &ps).unwrap();
                    print!("{}", as_24_bit_terminal_escaped(&ranges[..], false));
                }
                println!("\x1b[0m");
            }
        }
        Err(e) => println!("Error getting shapes: {}", e),
    }
    true
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

    // Welcome message
    println!("\x1b[97m================");
    println!("\x1b[36mWelcome to AD4M Perspective REPL!");
    println!("\x1b[90mType 'help' or '?' to see available commands");
    println!("\x1b[90mUse ↑/↓ arrows to navigate command history");
    println!("\x1b[90mType 'exit' to quit");
    println!("\x1b[97m================");
    println!();

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

        if get_links(&perspective, &line).await {
            continue;
        }

        if all_links(&perspective, &line).await {
            continue;
        }

        if subject_classes(&perspective, &line).await {
            continue;
        }

        if help_command(&perspective, &line).await {
            continue;
        }

        if system_commands(&perspective, &line).await {
            continue;
        }

        if shapes(&perspective, &line).await {
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
