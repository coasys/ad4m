use ad4m_client::Ad4mClient;
use anyhow::{Context, Result};
use clap::Subcommand;
use colour::{self, blue_ln, green_ln};
use rustyline::Editor;
use std::fs;
use std::sync::mpsc::channel;

use crate::bootstrap_publish::*;

#[derive(Debug, Subcommand)]
pub enum LanguageFunctions {
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
    Publish { 
        /// Path to the language file
        path: String,
        /// Name of the language
        #[arg(short, long)]
        name: Option<String>,
        /// Description of the language
        #[arg(short, long)]
        description: Option<String>,
        /// Template parameters (if any) of the language, comma separated list of strings
        #[arg(short, long)]
        possible_template_params:  Option<String>,
        /// URL of public repository of the language source code
        #[arg(short, long)]
        source_code_link: Option<String>,
    },
    /// Show meta information about a language
    Meta { address: String },
    /// Show source code of a language
    Source { address: String },
    /// Uninstall given language
    Remove { address: String },
    /// Generate bootstrap seed from a local prototype JSON file declaring languages to be published
    GenerateBootstrap {
        agent_path: String,
        passphrase: String,
        ad4m_host_path: String,
        seed_proto: String,
    },
}

pub async fn run(ad4m_client: Ad4mClient, command: Option<LanguageFunctions>) -> Result<()> {
    if command.is_none() {
        let all_languages = ad4m_client.languages.by_filter(None).await?;
        for language in all_languages {
            println!("\x1b[36mName: \x1b[97m{}", language.name);
            println!("\x1b[36mAddress: \x1b[97m{}", language.address);

            if let Some(settings) = language.settings {
                if settings != "{}" {
                    println!("\x1b[36mSettings: \x1b[97m{}", settings);
                } else {
                    println!("\x1b[36mSettings: \x1b[90m<empty>");
                }
            } else {
                println!("\x1b[36mSettings: \x1b[90m<undefined>");
            }
            println!()
        }
        return Ok(());
    }

    match command.unwrap() {
        LanguageFunctions::All => {
            let all_perspectives = ad4m_client.languages.by_filter(None).await?;
            println!("{:#?}", all_perspectives);
        }
        LanguageFunctions::ByFilter { filter } => {
            let languages = ad4m_client.languages.by_filter(Some(filter)).await?;
            println!("{:#?}", languages);
        }
        LanguageFunctions::ByAddress { address } => {
            let maybe_language = ad4m_client.languages.by_address(address).await?;
            if let Some(language) = maybe_language {
                println!("{:#?}", language);
            } else {
                println!("Language not found");
            }
        }
        LanguageFunctions::WriteSettings { address, settings } => {
            ad4m_client
                .languages
                .write_settings(address, settings)
                .await?;
            println!("Language settings written");
        }
        LanguageFunctions::ApplyTemplateAndPublish {
            source,
            template_data,
        } => {
            let new_language = ad4m_client
                .languages
                .apply_template_and_publish(source, template_data)
                .await?;
            println!("Language template applied and published!");
            println!("Name: {}", new_language.name);
            println!("Address: {}", new_language.address);
        }
        LanguageFunctions::Meta { address } => {
            let meta = ad4m_client.languages.meta(address).await?;
            println!("{:#?}", meta);
        }
        LanguageFunctions::Publish { 
            path, 
            name, 
            description, 
            possible_template_params, 
            source_code_link 
        } => {
            let _ = std::fs::read_to_string(path.clone())
                .with_context(|| format!("Could not read language file `{}`!", path))?;

            println!("Publishing language found in file `{}`...", path);

            if name.is_none() || description.is_none() || possible_template_params.is_none() || source_code_link.is_none() {
                println!("Please enter meta-information for this language: ");
            };

            let mut rl = Editor::<()>::new()?;
            let name = name.unwrap_or_else(|| {
                rl.readline("Name (should match name in code): ").expect("Could not read name from stdin!")
            });
            let description = description.unwrap_or_else(|| {
                rl.readline("Description: ").expect("Could not read description from stdin!")
            });
            let possible_template_params_string = possible_template_params.unwrap_or_else(|| {
                rl.readline("Template parameters (comma spearated list): ").expect("Could not read template parameters from stdin!")
            });
            let possible_template_params: Vec<String> = possible_template_params_string
                .split(',')
                .map(|s| s.trim().to_string())
                .collect();
            let source_code_link = source_code_link.unwrap_or_else(|| {
                rl.readline("Source code link: ").expect("Could not read source code link from stdin!")
            });

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

            let publish_result = ad4m_client
                .languages
                .publish(
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
            let source = ad4m_client.languages.source(address).await?;
            println!("{}", source);
        }
        LanguageFunctions::Remove { address } => {
            ad4m_client.languages.remove(address).await?;
            println!("Language removed");
        }
        LanguageFunctions::GenerateBootstrap {
            agent_path,
            passphrase,
            ad4m_host_path,
            seed_proto,
        } => {
            green_ln!(
                "Attempting to generate a new bootstrap seed using ad4m-host path: {:?} and agent path: {:?}\n",
                ad4m_host_path,
                agent_path
            );

            //Load the seed proto first so we know that works before making new agent path
            let seed_proto = fs::read_to_string(seed_proto)?;
            let seed_proto: SeedProto = serde_json::from_str(&seed_proto)?;
            green_ln!("Loaded seed prototype file!\n");

            //Create a new ~/.ad4m-publish path with agent.json file supplied
            let data_path = dirs::home_dir()
                .expect("Could not get home directory")
                .join(".ad4m-publish");
            let data_path_files = std::fs::read_dir(&data_path);
            if data_path_files.is_ok() {
                fs::remove_dir_all(&data_path)?;
            }
            //Create the ad4m directory
            fs::create_dir(&data_path)?;
            let ad4m_data_path = data_path.join("ad4m");
            fs::create_dir(&ad4m_data_path)?;
            let data_data_path = data_path.join("data");
            fs::create_dir(&data_data_path)?;

            //Read the agent file
            let agent_file = fs::read_to_string(agent_path)?;
            //Copy the agent file to correct directory
            fs::write(ad4m_data_path.join("agent.json"), agent_file)?;
            fs::write(data_data_path.join("DIDCache.json"), String::from("{}"))?;
            green_ln!("Publishing agent directory setup\n");

            green_ln!("Creating temporary bootstrap seed for publishing purposes...\n");
            let lang_lang_source = fs::read_to_string(&seed_proto.language_language_ref)?;
            let temp_bootstrap_seed = BootstrapSeed {
                trusted_agents: vec![],
                known_link_languages: vec![],
                language_language_bundle: lang_lang_source.clone(),
                direct_message_language: String::from(""),
                agent_language: String::from(""),
                perspective_language: String::from(""),
                neighbourhood_language: String::from(""),
            };
            let temp_publish_bootstrap_path = data_path.join("publishing_bootstrap.json");
            fs::write(
                &temp_publish_bootstrap_path,
                serde_json::to_string(&temp_bootstrap_seed)?,
            )?;

            //start ad4m-host with publishing bootstrap
            let ad4m_host_init = std::process::Command::new(&ad4m_host_path)
                .arg("init")
                .arg("--networkBootstrapSeed")
                .arg(&temp_publish_bootstrap_path)
                .arg("--dataPath")
                .arg(&data_path)
                .arg("--overrideConfig")
                .output()?;

            blue_ln!(
                "ad4m-host init output: {}\n",
                String::from_utf8_lossy(&ad4m_host_init.stdout)
            );

            green_ln!(
                "Starting publishing with bootstrap path: {}\n",
                temp_publish_bootstrap_path.to_str().unwrap()
            );

            let (tx, rx) = channel();
            serve_ad4m_host(ad4m_host_path, data_path, tx)?;

            for line in &rx {
                println!("{}", line);
                if line.contains("GraphQL server started, Unlock the agent to start holohchain") {
                    green_ln!("AD4M Host ready for publishing\n");
                    //Spawn in a new thread so we can continue reading logs in loop below, whilst publishing is happening
                    tokio::spawn(async move {
                        start_publishing(
                            passphrase.clone(),
                            seed_proto.clone(),
                            lang_lang_source.clone(),
                        )
                        .await;
                    });
                    break;
                }
            }

            for line in rx {
                println!("{}", line);
            }
        }
    };
    Ok(())
}
