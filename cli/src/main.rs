extern crate anyhow;
extern crate async_tungstenite;
extern crate chrono;
extern crate clap;
extern crate dirs;
extern crate graphql_client;
extern crate reqwest;
extern crate rustyline;
extern crate tokio;

mod agent;
mod formatting;
mod languages;
mod neighbourhoods;
mod perspectives;
mod runtime;
mod startup;
mod types;
mod util;

use anyhow::{bail, Context, Result};
use clap::{Args, Parser, Subcommand};
use formatting::{print_agent, print_link, print_logo, print_prolog_result};
use rustyline::Editor;
use serde_json::Value;
use startup::executor_data_path;
use util::{maybe_parse_datetime, readline_masked};

/// AD4M command line interface.
/// Provides all means of interacting with the AD4M executor / agent.
/// See help of commands for more information.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct ClapApp {
    #[command(subcommand)]
    domain: Domain,
}

#[derive(Debug, Subcommand)]
enum Domain {
    /// Functions related to local agent / user
    Agent {
        #[command(subcommand)]
        command: AgentFunctions,
    },

    /// See, create, and manage Languages
    Languages {
        #[command(subcommand)]
        command: Option<LanguageFunctions>,
    },

    /// Add, remove and access Perspectives / add and remove links
    Perspectives {
        #[command(subcommand)]
        command: Option<PerspectiveFunctions>,
    },
    /// Publish perspectives as Neighbourhoods and join Neighbourhoods
    Neighbourhoods {
        #[command(subcommand)]
        command: NeighbourhoodFunctions,
    },
    /// Access various states of the local AD4M executor
    Runtime {
        #[command(subcommand)]
        command: RuntimeFunctions,
    },
    /// Print the executor log
    Log,
}

#[derive(Debug, Subcommand)]
enum AgentFunctions {
    /// Print the local agent's information (public perspective, direct message language, DID)
    Me,
    /// Show status of agent keys (locked/unlocked, etc.)
    Status,
    /// Lock the agent keys
    Lock,
    /// Unlock the agent keys
    Unlock,
    /// Lookup agent by DID
    ByDID { did: String },
}

#[derive(Debug, Subcommand)]
enum LanguageFunctions {
    /// List all languages
    All,
    /// Show information about a language
    ByAddress { address: String },
    /// List all languages that implement a given interface
    ByFilter { filter: String },
    /// Set language settings (JSON) string
    WriteSettings { address: String, settings: String },
    /// Clone a source language, set template parameters and publish the result
    ApplyTemplateAndPublish {
        /// Address of the source language to clone
        source: String,
        /// JSON string of template parameters
        template_data: String,
    },
    /// Publish AD4M Language from local file path
    Publish { path: String },
    /// Show meta information about a language
    Meta { address: String },
    /// Show source code of a language
    Source { address: String },
    /// Uninstall given language
    Remove { address: String },
}

#[derive(Debug, Subcommand)]
enum PerspectiveFunctions {
    /// Add a perspective with given name
    Add {
        name: String,
    },

    /// Remove perspective with given uuid
    Remove {
        id: String,
    },

    /// Add link to perspective with given uuid
    AddLink {
        id: String,
        source: String,
        target: String,
        predicate: Option<String>,
    },

    /// Query links from perspective with given uuid
    QueryLinks(QueryLinksArgs),

    /// Run Prolog / SDNA query on perspective with given uuid
    Infer {
        id: String,
        query: String,
    },

    Watch {
        id: String,
    },
}

#[derive(Args, Debug)]
struct QueryLinksArgs {
    /// Perspective ID
    id: String,

    /// Filter by source
    source: Option<String>,

    /// Filter by target
    target: Option<String>,

    /// Filter by predicate
    predicate: Option<String>,

    /// Get only links after this date (fromat: %Y-%m-%dT%H:%M:%S%.fZ)
    #[arg(short, long)]
    from_date: Option<String>,

    /// Get only links before this date (fromat: %Y-%m-%dT%H:%M:%S%.fZ)
    #[arg(short, long)]
    until_date: Option<String>,

    /// Get only the first n links
    #[arg(short, long)]
    limit: Option<f64>,
}

#[derive(Debug, Subcommand)]
enum NeighbourhoodFunctions {
    Create {
        perspective_id: String,
        link_language: String,
    },
    Join {
        url: String,
    },
}

#[derive(Debug, Subcommand)]
enum RuntimeFunctions {
    Info,
    Quit,
    AddTrustedAgents {
        agents: Vec<String>,
    },
    DeleteTrustedAgents {
        agents: Vec<String>,
    },
    TrustedAgents,
    AddLinkLanguageTemplates {
        addresses: Vec<String>,
    },
    RemoveLinkLanguageTemplates {
        addresses: Vec<String>,
    },
    LinkLanguageTemplates,
    AddFriends {
        agents: Vec<String>,
    },
    RemoveFriends {
        agents: Vec<String>,
    },
    Friends,
    HcAgentInfos,
    HcAddAgentInfos {
        infos: Vec<String>,
    },
    VerifySignature {
        did: String,
        did_signing_key: String,
        data: String,
        signed_data: String,
    },
    SetStatus {
        status: String,
    },
    FriendStatus {
        agent: String,
    },
    FriendSendMessage {
        agent: String,
        message: String,
    },
    MessageInbox {
        filter: Option<String>,
    },
    MessageOutbox {
        filter: Option<String>,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    print_logo();
    let args = ClapApp::parse();
    let cap_token = match &args.domain {
        Domain::Log => "".to_string(),
        Domain::Agent { command } => match command {
            AgentFunctions::Lock | AgentFunctions::Unlock => "".to_string(),
            _ => startup::get_cap_token().await?,
        },
        _ => startup::get_cap_token().await?,
    };

    match args.domain {
        Domain::Agent { command } => match command {
            AgentFunctions::Me => {
                let agent = agent::run_me(cap_token).await?;
                print_agent(agent.into());
            }
            AgentFunctions::Status => {
                let status = agent::run_status(cap_token).await?;
                println!(
                    "\x1b[36mDID: \x1b[97m{}",
                    status.did.unwrap_or_else(|| "<undefined>".to_string())
                );
                println!("\x1b[36mis_initiliazed: \x1b[97m{}", status.is_initialized);
                println!("\x1b[36mis_unlocked: \x1b[97m{}", status.is_unlocked);
                println!(
                    "\x1b[36mDID Document:\n\x1b[97m{}",
                    status.did_document.unwrap_or_else(|| "<undefined>".to_string())
                );
            }
            AgentFunctions::Lock => {
                let result = agent::run_lock(cap_token, readline_masked("Passphrase: ")?).await?;
                if let Some(error) = result.error {
                    bail!(error);
                } else {
                    println!("Agent locked");
                }
            }
            AgentFunctions::Unlock => {
                let result = agent::run_unlock(cap_token, readline_masked("Passphrase: ")?).await?;
                if let Some(error) = result.error {
                    bail!(error);
                } else {
                    println!("Agent unlocked");
                }
            }
            AgentFunctions::ByDID { did } => {
                if let Some(agent) = agent::run_by_did(cap_token, did).await? {
                    print_agent(agent.into());
                } else {
                    println!("Agent not found");
                }
            }
        },
        Domain::Languages { command } => {
            if command.is_none() {
                let all_perspectives = languages::run_by_filter(cap_token, "".to_string()).await?;
                println!("{:#?}", all_perspectives);
                return Ok(());
            }

            match command.unwrap() {
                LanguageFunctions::All => {
                    let all_perspectives =
                        languages::run_by_filter(cap_token, "".to_string()).await?;
                    println!("{:#?}", all_perspectives);
                }
                LanguageFunctions::ByFilter { filter } => {
                    let languages = languages::run_by_filter(cap_token, filter).await?;
                    println!("{:#?}", languages);
                }
                LanguageFunctions::ByAddress { address } => {
                    let maybe_language = languages::run_by_address(cap_token, address).await?;
                    if let Some(language) = maybe_language {
                        println!("{:#?}", language);
                    } else {
                        println!("Language not found");
                    }
                }
                LanguageFunctions::WriteSettings { address, settings } => {
                    languages::run_write_settings(cap_token, address, settings).await?;
                    println!("Language settings written");
                }
                LanguageFunctions::ApplyTemplateAndPublish {
                    source,
                    template_data,
                } => {
                    let new_language =
                        languages::run_apply_template_and_publish(cap_token, source, template_data)
                            .await?;
                    println!("Language template applied and published!");
                    println!("Name: {}", new_language.name);
                    println!("Address: {}", new_language.address);
                }
                LanguageFunctions::Meta { address } => {
                    let meta = languages::run_meta(cap_token, address).await?;
                    println!("{:#?}", meta);
                }
                LanguageFunctions::Publish { path } => {
                    let _ = std::fs::read_to_string(path.clone())
                        .with_context(|| format!("Could not read language file `{}`!", path))?;

                    println!("Publishing language found in file `{}`...", path);
                    println!("Please enter meta-information for this language: ");
                    let mut rl = Editor::<()>::new()?;
                    let name = rl.readline("Name (should match name in code): ")?;
                    let description = rl.readline("Description: ")?;
                    let possible_template_params_string =
                        rl.readline("Template parameters (comma spearated list): ")?;
                    let possible_template_params: Vec<String> = possible_template_params_string
                        .split(',')
                        .map(|s| s.trim().to_string())
                        .collect();
                    let source_code_link = rl.readline("Source code link: ")?;

                    let description = if description.is_empty() {
                        None
                    } else {
                        Some(description)
                    };
                    let possible_template_params = if possible_template_params_string.is_empty() {
                        None
                    } else {
                        Some(possible_template_params)
                    };
                    let source_code_link = if source_code_link.is_empty() {
                        None
                    } else {
                        Some(source_code_link)
                    };

                    let publish_result = languages::run_publish(
                        cap_token,
                        path,
                        name,
                        description,
                        possible_template_params,
                        source_code_link,
                    )
                    .await?;
                    println!(
                        "Language published with address: {}",
                        publish_result.address
                    );
                }
                LanguageFunctions::Source { address } => {
                    let source = languages::run_source(cap_token, address).await?;
                    println!("{}", source);
                }
                LanguageFunctions::Remove { address } => {
                    languages::run_remove(cap_token, address).await?;
                    println!("Language removed");
                }
            }
        }
        Domain::Perspectives { command } => {
            if command.is_none() {
                let all_perspectives = perspectives::run_all(cap_token).await?;
                println!("{:#?}", all_perspectives);
                return Ok(());
            }

            match command.unwrap() {
                PerspectiveFunctions::Add { name } => {
                    let new_perspective = perspectives::run_add(cap_token, name).await?;
                    println!("{:#?}", new_perspective);
                }
                PerspectiveFunctions::Remove { id } => {
                    perspectives::run_remove(cap_token, id).await?;
                }
                PerspectiveFunctions::AddLink {
                    id,
                    source,
                    target,
                    predicate,
                } => {
                    perspectives::run_add_link(cap_token, id, source, target, predicate).await?;
                }
                PerspectiveFunctions::QueryLinks(args) => {
                    let from_date = maybe_parse_datetime(args.from_date)?;
                    let until_date = maybe_parse_datetime(args.until_date)?;
                    let result = perspectives::run_query_links(
                        cap_token,
                        args.id,
                        args.source,
                        args.target,
                        args.predicate,
                        from_date,
                        until_date,
                        args.limit,
                    )
                    .await?;
                    for link in result {
                        print_link(link.into());
                    }
                }
                PerspectiveFunctions::Infer { id, query } => {
                    match perspectives::run_infer(cap_token, id, query).await? {
                        Value::Bool(true) => println!("true ✅"),
                        Value::Bool(false) => println!("false ❌"),
                        Value::String(string) => println!("{}", string),
                        Value::Array(array) => {
                            println!("\x1b[90m{} results:", array.len());
                            let mut i = 1;
                            for item in array {
                                println!("\x1b[90m{}:", i);
                                print_prolog_result(item)?;
                                println!("====================");
                                i += 1;
                            }
                        }
                        _ => bail!("Unexpected result value in response of run_infer()"),
                    }
                }
                PerspectiveFunctions::Watch { id } => {
                    perspectives::run_watch(cap_token, id).await?;
                }
            }
        }
        Domain::Neighbourhoods { command } => match command {
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
        },
        Domain::Runtime { command } => {
            match command {
                RuntimeFunctions::Info => {
                    let info = runtime::run_info(cap_token).await?;
                    println!("{:#?}", info);
                },
                RuntimeFunctions::Quit => {
                    runtime::run_quit(cap_token).await?;
                    println!("Executor shut down!");
                },
                RuntimeFunctions::AddTrustedAgents { agents } => {
                    runtime::run_add_trusted_agents(cap_token, agents).await?;
                    println!("Trusted agents added!");
                },
                RuntimeFunctions::DeleteTrustedAgents { agents } => {
                    runtime::run_delete_trusted_agents(cap_token, agents).await?;
                    println!("Trusted agents removed!");
                },
                RuntimeFunctions::TrustedAgents => {
                    let agents = runtime::run_trusted_agents(cap_token).await?;
                    for agent in agents {
                        println!("{}", agent);
                    }
                },
                RuntimeFunctions::LinkLanguageTemplates => {
                    let templates = runtime::run_link_language_templates(cap_token).await?;
                    for template in templates {
                        println!("{}", template);
                    }
                },
                RuntimeFunctions::AddLinkLanguageTemplates { addresses } => {
                    runtime::run_add_link_language_templates(cap_token, addresses).await?;
                    println!("Link language templates added!");
                },
                RuntimeFunctions::RemoveLinkLanguageTemplates { addresses } => {
                    runtime::run_remove_link_language_templates(cap_token, addresses).await?;
                    println!("Link language templates removed!");
                },
                RuntimeFunctions::Friends => {
                    let friends = runtime::run_friends(cap_token).await?;
                    for friend in friends {
                        println!("{}", friend);
                    }
                },
                RuntimeFunctions::AddFriends { agents } => {
                    runtime::run_add_friends(cap_token, agents).await?;
                    println!("Friends added!");
                },
                _ => unimplemented!("Runtime command not implemented yet"),
            }
        }
        Domain::Log => {
            let file = executor_data_path().join("ad4min.log");
            let log = std::fs::read_to_string(file.clone()).with_context(|| {
                format!(
                    "Could not read log file `{}`!\nIs AD4M executor running?",
                    file.display()
                )
            })?;
            println!("{}", log);
        }
    }

    Ok(())
}
