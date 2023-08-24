use anyhow::Result;
use clap::Subcommand;
use colour::{self, green_ln};
use std::fs;

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
            // //Create the ad4m directory
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
            rust_executor::init::init(
                Some(data_path.to_str().unwrap().to_string()),
                Some(temp_publish_bootstrap_path.to_str().unwrap().to_string()),
            )
            .map_err(|err| {
                colour::red_ln!("Error in init: {:?}", err);
                err
            })
            .unwrap();

            green_ln!(
                "Starting publishing with bootstrap path: {}\n",
                temp_publish_bootstrap_path.to_str().unwrap()
            );

            let run_fut = async move {
                rust_executor::run(rust_executor::Ad4mConfig {
                    app_data_path: Some(data_path.to_str().unwrap().to_string()),
                    network_bootstrap_seed: Some(
                        temp_publish_bootstrap_path.to_str().unwrap().to_string(),
                    ),
                    language_language_only: Some(true),
                    run_dapp_server: None,
                    gql_port: None,
                    hc_admin_port: None,
                    hc_app_port: None,
                    hc_use_bootstrap: None,
                    hc_use_local_proxy: None,
                    hc_use_mdns: None,
                    hc_use_proxy: None,
                    connect_holochain: None,
                    admin_credential: None,
                    hc_proxy_url: None,
                    hc_bootstrap_url: None,
                })
                .await;
            };

            //Spawn in a new thread so we can continue reading logs in loop below, whilst publishing is happening
            let publish_fut = async move {
                tokio::time::sleep(std::time::Duration::from_millis(5000)).await;
                green_ln!("AD4M ready for publishing\n");
                start_publishing(
                    passphrase.clone(),
                    seed_proto.clone(),
                    lang_lang_source.clone(),
                )
                .await;
            };

            tokio::select! {
                _ = run_fut => {
                    green_ln!("AD4M finished running\n");
                }
                _ = publish_fut => {
                    green_ln!("AD4M finished publishing\n");
                }
            }
        }
    };
    Ok(())
}
