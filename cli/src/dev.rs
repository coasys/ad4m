
use anyhow::{Result};
use clap::Subcommand;
use colour::{self, blue_ln, green_ln};
use std::fs;
use std::sync::mpsc::channel;

use crate::bootstrap_publish::*;

#[derive(Debug, Subcommand)]
pub enum DevFunctions {
    /// Generate bootstrap seed from a local prototype JSON file declaring languages to be published
    GenerateBootstrap {
        agent_path: String,
        passphrase: String,
        ad4m_host_path: String,
        seed_proto: String,
    },
}

pub async fn run(command: DevFunctions) -> Result<()> {
    match command {
        DevFunctions::GenerateBootstrap {
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
