use anyhow::{anyhow, Result};
use clap::Subcommand;
use colour::{self, green_ln};
use std::fs;
use tokio::task::JoinHandle;

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

            let run_handle = tokio::task::spawn(run_executor(rust_executor::Ad4mConfig {
                app_data_path: Some(ad4m_test_dir_clone),
                network_bootstrap_seed: None,
                language_language_only: Some(false),
                run_dapp_server: Some(false),
                port: None,
                hc_admin_port: None,
                hc_app_port: None,
                hc_use_bootstrap: None,
                hc_use_local_proxy: None,
                hc_use_mdns: None,
                hc_use_proxy: None,
                connect_holochain: None,
                run_holochain: None,
                admin_credential: Some(String::from("*")),
                hc_proxy_url: None,
                hc_bootstrap_url: None,
                hc_relay_url: None,
                localhost: None,
                auto_permit_cap_requests: Some(true),
                tls: None,
                log_holochain_metrics: None,
                enable_multi_user: None,
                enable_mcp: None,
                mcp_port: None,
                smtp_config: None,
                pid_file: None,
                ..Default::default()
            }));

            let test = tokio::task::spawn(async move {
                tokio::time::sleep(std::time::Duration::from_millis(5000)).await;
                let client = ad4m_client::Ad4mClient::connect(
                    String::from("http://127.0.0.1:4000"),
                    String::from("*"),
                )
                .await
                .expect("could not connect to executor");
                let me = client.agent.me().await;
                println!("Me: {:?}", me);
                let agent_generate = client.agent.generate(String::from("test")).await;
                println!("Agent generate: {:?}", agent_generate);
                let publish_language = client
                    .languages
                    .publish(
                        language_path,
                        Some(String::from("some-test-lang")),
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
                    .expression_create(data.clone(), language_info.address)
                    .await;
                println!("Expression create: {:?}", expression);
                let expression = client.expressions.expression(expression.unwrap()).await;
                println!("Expression get: {:?}", expression);
            });
            let outcome = run_against_executor(run_handle, test).await;

            //Cleanup test agent
            let _ = fs::remove_dir_all(std::path::Path::new(&ad4m_test_dir));
            green_ln!("Test agent cleaned up\n");
            exit_with(outcome, "Language test")
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
                "Writting temp publish bootstrap at path: {:?}\n",
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

            // The publishing executor holds the agent that signs the seed; a
            // throwaway credential shared only with start_publishing() keeps
            // its API closed to everyone else on the host.
            let admin_credential = random_admin_credential();
            let publish_credential = admin_credential.clone();

            let run_handle = tokio::task::spawn(run_executor(rust_executor::Ad4mConfig {
                app_data_path: Some(data_path.to_str().unwrap().to_string()),
                network_bootstrap_seed: Some(
                    temp_publish_bootstrap_path.to_str().unwrap().to_string(),
                ),
                language_language_only: Some(true),
                run_dapp_server: Some(false),
                port: None,
                hc_admin_port: None,
                hc_app_port: None,
                hc_use_bootstrap: None,
                hc_use_local_proxy: None,
                hc_use_mdns: None,
                hc_use_proxy: None,
                connect_holochain: None,
                run_holochain: None,
                admin_credential: Some(admin_credential),
                hc_proxy_url: None,
                hc_bootstrap_url: None,
                hc_relay_url: None,
                localhost: None,
                auto_permit_cap_requests: Some(true),
                tls: None,
                log_holochain_metrics: None,
                enable_multi_user: None,
                enable_mcp: None,
                mcp_port: None,
                smtp_config: None,
                pid_file: None,
                ..Default::default()
            }));

            //Spawn in a new thread so we can continue reading logs in loop below, whilst publishing is happening
            let publish = tokio::task::spawn(async move {
                green_ln!("Runing publish fut");
                tokio::time::sleep(std::time::Duration::from_millis(5000)).await;
                green_ln!("AD4M ready for publishing\n");
                start_publishing(
                    publish_credential,
                    passphrase.clone(),
                    seed_proto.clone(),
                    lang_lang_source.clone(),
                )
                .await;
            });
            let outcome = run_against_executor(run_handle, publish).await;
            exit_with(outcome, "Publish")
        }
    }
}

/// Starts an executor and resolves when it stops. That is `Ok` only when the
/// REST API thread returned cleanly, `Err` when the API failed to start (e.g.
/// the port is taken) or the executor panicked.
///
/// [`rust_executor::run`] hands back the API thread. Joining it on a runtime
/// worker would block that worker for the executor's whole lifetime, so the
/// join runs on the blocking pool.
async fn run_executor(config: rust_executor::Ad4mConfig) -> Result<()> {
    let api_thread = rust_executor::run(config).await;
    match tokio::task::spawn_blocking(move || api_thread.join()).await {
        Ok(Ok(Ok(()))) => Ok(()),
        Ok(Ok(Err(e))) => Err(anyhow!("REST API server failed: {e:?}")),
        Ok(Err(_)) => Err(anyhow!("executor main thread panicked")),
        Err(e) => Err(anyhow!("executor join task failed: {e}")),
    }
}

/// Runs `workflow` against the executor kept up by `executor`.
///
/// `executor` only completes when the executor stops, so if it completes
/// first the workflow has been running against nothing: the workflow is
/// aborted and the executor's error is returned. An executor that stops
/// cleanly before the workflow is done is an error for the same reason. A
/// workflow that panics is an error too. Otherwise the workflow's value is
/// returned and the executor task is dropped; the executor's own threads are
/// stopped by process exit.
async fn run_against_executor<T>(
    mut executor: JoinHandle<Result<()>>,
    mut workflow: JoinHandle<T>,
) -> Result<T> {
    tokio::select! {
        executor_stopped = &mut executor => {
            workflow.abort();
            Err(match executor_stopped {
                Ok(Ok(())) => anyhow!("executor stopped before the workflow finished"),
                Ok(Err(e)) => e,
                Err(e) => anyhow!("executor task panicked: {e}"),
            })
        }
        finished = &mut workflow => {
            executor.abort();
            finished.map_err(|e| anyhow!("workflow panicked: {e}"))
        }
    }
}

/// Ends the process with the workflow's outcome. Both workflows leave
/// executor threads running that would otherwise keep the process alive, and
/// a failed executor must not end in exit code 0.
fn exit_with(outcome: Result<()>, workflow: &str) -> ! {
    match outcome {
        Ok(()) => {
            green_ln!("{workflow} finished\n");
            std::process::exit(0)
        }
        Err(e) => {
            colour::red_ln!("{workflow} failed: {e:?}\n");
            std::process::exit(1)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::future::pending;
    use std::time::Duration;
    use tokio::task::spawn;
    use tokio::time::timeout;

    /// Bounds every test: with the executor handle ignored, a workflow that
    /// never finishes would hang instead of failing.
    const BOUND: Duration = Duration::from_secs(2);

    #[tokio::test]
    async fn executor_error_fails_the_workflow() {
        let executor = spawn(async { Err(anyhow!("REST API server failed: port taken")) });
        let workflow = spawn(pending::<()>());

        let outcome = timeout(BOUND, run_against_executor(executor, workflow))
            .await
            .expect("must fail as soon as the executor stops, not wait for the workflow");

        let err = outcome.expect_err("executor failure must fail the workflow");
        assert!(err.to_string().contains("port taken"), "{err:?}");
    }

    #[tokio::test]
    async fn executor_panic_fails_the_workflow() {
        let executor = spawn(async { panic!("Error awaiting executor main thread") });
        let workflow = spawn(pending::<()>());

        let outcome = timeout(BOUND, run_against_executor(executor, workflow))
            .await
            .expect("must fail as soon as the executor panics");

        let err = outcome.expect_err("executor panic must fail the workflow");
        assert!(
            err.to_string().contains("executor task panicked"),
            "{err:?}"
        );
    }

    #[tokio::test]
    async fn executor_stopping_cleanly_fails_an_unfinished_workflow() {
        let executor = spawn(async { Ok(()) });
        let workflow = spawn(pending::<()>());

        let outcome = timeout(BOUND, run_against_executor(executor, workflow))
            .await
            .expect("must fail as soon as the executor stops");

        let err = outcome.expect_err("an executor that stops early must fail the workflow");
        assert!(err.to_string().contains("stopped before"), "{err:?}");
    }

    #[tokio::test]
    async fn workflow_result_is_returned_while_executor_runs() {
        let executor = spawn(pending::<Result<()>>());
        let workflow = spawn(async { 42 });

        let outcome = timeout(BOUND, run_against_executor(executor, workflow))
            .await
            .expect("a finished workflow must not wait for the executor");

        assert_eq!(outcome.expect("workflow result is passed through"), 42);
    }

    #[tokio::test]
    async fn workflow_panic_is_an_error() {
        let executor = spawn(pending::<Result<()>>());
        let workflow = spawn(async { panic!("could not connect to executor") });

        let outcome: Result<()> = timeout(BOUND, run_against_executor(executor, workflow))
            .await
            .expect("a panicked workflow must not wait for the executor");

        let err = outcome.expect_err("workflow panic must be an error");
        assert!(err.to_string().contains("workflow panicked"), "{err:?}");
    }
}

/// 32 random bytes, hex-encoded.
fn random_admin_credential() -> String {
    use rand::RngCore;
    let mut bytes = [0u8; 32];
    rand::thread_rng().fill_bytes(&mut bytes);
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}
