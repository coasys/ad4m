use anyhow::Result;
use clap::Subcommand;
use colour::{self, green_ln};
use std::{fs, str::FromStr};

use crate::bootstrap_publish::*;

#[derive(Debug, Subcommand)]
pub enum DevFunctions {
    /// Generate bootstrap seed from a local prototype JSON file declaring languages to be published
    GenerateBootstrap {
        agent_path: String,
        passphrase: String,
        seed_proto: String,
    },
    PublishAndTestExpressionLanguage {
        language_path: String,
        data: String,
    },
}

pub async fn run(command: DevFunctions) -> Result<()> {
    match command {
        DevFunctions::PublishAndTestExpressionLanguage {
            language_path,
            data,
        } => {
            let ad4m_test_dir = dirs::home_dir()
                .expect("Could not get home directory")
                .join(".ad4m-test");
            let ad4m_test_dir: String = ad4m_test_dir.to_string_lossy().to_string();
            let ad4m_test_dir_clone = ad4m_test_dir.clone();

            rust_executor::init::init(Some(ad4m_test_dir.clone()), None)
                .map_err(|err| anyhow::anyhow!("Error in init: {:?}", err))?;

            let run_handle = tokio::task::spawn(async move {
                rust_executor::run(rust_executor::Ad4mConfig {
                    app_data_path: Some(ad4m_test_dir_clone),
                    network_bootstrap_seed: None,
                    language_language_only: Some(false),
                    run_dapp_server: Some(false),
                    gql_port: None,
                    hc_admin_port: None,
                    hc_app_port: None,
                    hc_use_bootstrap: None,
                    hc_use_local_proxy: None,
                    hc_use_mdns: None,
                    hc_use_proxy: None,
                    connect_holochain: None,
                    admin_credential: Some(String::from("*")),
                    hc_proxy_url: None,
                    hc_bootstrap_url: None,
                    localhost: None,
                    auto_permit_cap_requests: Some(true),
                    tls: None,
                    log_holochain_metrics: None,
                    enable_multi_user: None,
                    smtp_config: None,
                })
                .await
                .join()
                .expect("Error awaiting executor main thread");
            });

            let test_res = tokio::task::spawn(async move {
                tokio::time::sleep(std::time::Duration::from_millis(5000)).await;
                let client = ad4m_client::Ad4mClient::new(
                    String::from("http://127.0.0.1:4000/graphql"),
                    String::from("*"),
                );
                let me = client.agent.me().await;
                println!("Me: {:?}", me);
                let agent_generate = client.agent.generate(String::from("test")).await;
                println!("Agent generate: {:?}", agent_generate);
                let publish_language = client
                    .languages
                    .publish(
                        language_path,
                        String::from("some-test-lang"),
                        Some(String::from("some-desc")),
                        None,
                        None,
                    )
                    .await;
                println!("Publish language: {:?}", publish_language);
                let language_info = publish_language.unwrap();
                let language = client
                    .languages
                    .by_address(language_info.address.clone())
                    .await;
                println!("Language: {:?}", language);
                let expression = client
                    .expressions
                    .expression_create(
                        language_info.address,
                        serde_json::Value::from_str(&data)
                            .expect("could not cast input data to serde_json::Value"),
                    )
                    .await;
                println!("Expression create: {:?}", expression);
                let expression = client.expressions.expression(expression.unwrap()).await;
                println!("Expression get: {:?}", expression);
            })
            .await;
            green_ln!("Test future finished with: {:?}", test_res);

            run_handle.abort();
            //Cleanup test agent
            let _ = fs::remove_dir_all(std::path::Path::new(&ad4m_test_dir));
            green_ln!("Test agent cleaned up\n");
            std::process::exit(0);
        }
        DevFunctions::GenerateBootstrap {
            agent_path,
            passphrase,
            seed_proto,
        } => {
            green_ln!(
                "Attempting to generate a new bootstrap seed using agent path: {:?}\n",
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
            green_ln!(
                "Writting temp publish boostrap at path: {:?}\n",
                temp_publish_bootstrap_path.to_str()
            );
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

            tokio::task::spawn(async move {
                rust_executor::run(rust_executor::Ad4mConfig {
                    app_data_path: Some(data_path.to_str().unwrap().to_string()),
                    network_bootstrap_seed: Some(
                        temp_publish_bootstrap_path.to_str().unwrap().to_string(),
                    ),
                    language_language_only: Some(true),
                    run_dapp_server: Some(false),
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
                    localhost: None,
                    auto_permit_cap_requests: Some(true),
                    tls: None,
                    log_holochain_metrics: None,
                    enable_multi_user: None,
                    smtp_config: None,
                })
                .await
                .join()
                .expect("Error awaiting executor main thread");
            });

            //Spawn in a new thread so we can continue reading logs in loop below, whilst publishing is happening
            let publish_fut = tokio::task::spawn(async move {
                green_ln!("Runing publish fut");
                tokio::time::sleep(std::time::Duration::from_millis(5000)).await;
                green_ln!("AD4M ready for publishing\n");
                start_publishing(
                    passphrase.clone(),
                    seed_proto.clone(),
                    lang_lang_source.clone(),
                )
                .await;
            })
            .await;
            green_ln!("Publish future finished with: {:?}", publish_fut);

            // tokio::select! {
            //     biased;

            //     _ = run_fut => {
            //         green_ln!("AD4M finished running\n");
            //     }
            //     _ = publish_fut => {
            //         green_ln!("AD4M finished publishing\n");
            //     }
            // }
        }
    };
    Ok(())
}
