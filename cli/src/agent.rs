use crate::{formatting::*, util::readline_masked};
use ad4m_client::{types::EntanglementProof, Ad4mClient};
use anyhow::{bail, Result};
use clap::Subcommand;

#[derive(Debug, Subcommand)]
pub enum AgentFunctions {
    /// Print the local agent's information (public perspective, direct message language, DID)
    Me,
    /// Show status of agent keys (locked/unlocked, etc.)
    Status,
    /// Lock the agent keys
    Lock,
    /// Unlock the agent keys
    Unlock {
        /// Agent passphrase
        #[arg(short, long)]
        passphrase: Option<String>,

        /// Agent passphrase
        #[arg(short, long)]
        holochain: Option<bool>,
    },
    /// Lookup agent by DID
    ByDID {
        did: String,
    },
    /// Initialize a new agent
    Generate {
        /// Agent passphrase
        #[arg(short, long)]
        passphrase: Option<String>,
    },
    AddEntanglementProof {
        device_key: String,
        device_key_signed_by_did: String,
        device_key_type: String,
        did: Option<String>,
        did_signed_by_device_key: String,
        did_signing_key_id: Option<String>,
    },
    DeleteEntanglementProof {
        device_key: String,
        device_key_signed_by_did: String,
        device_key_type: String,
        did: Option<String>,
        did_signed_by_device_key: String,
        did_signing_key_id: Option<String>,
    },
    EntanglementProofPreFlight {
        device_key: String,
        device_key_type: String,
    },
    GenerateJwt {
        request_id: String,
        rand: String,
    },
    /// Stay connected and print any agent status changed events
    Watch {},
}

pub async fn run(ad4m_client: Ad4mClient, command: AgentFunctions) -> Result<()> {
    match command {
        AgentFunctions::Me => {
            let agent = ad4m_client.agent.me().await?;
            print_agent(agent.into());
        }
        AgentFunctions::Status => {
            let status = ad4m_client.agent.status().await?;
            println!(
                "\x1b[36mDID: \x1b[97m{}",
                status.did.unwrap_or_else(|| "<undefined>".to_string())
            );
            println!("\x1b[36mis_initiliazed: \x1b[97m{}", status.is_initialized);
            println!("\x1b[36mis_unlocked: \x1b[97m{}", status.is_unlocked);
            println!(
                "\x1b[36mDID Document:\n\x1b[97m{}",
                status
                    .did_document
                    .unwrap_or_else(|| "<undefined>".to_string())
            );
        }
        AgentFunctions::Lock => {
            let result = ad4m_client
                .agent
                .lock(readline_masked("Passphrase: ")?)
                .await?;
            if let Some(error) = result.error {
                bail!(error);
            } else {
                println!("Agent locked");
            }
        }
        AgentFunctions::Unlock {
            passphrase,
            holochain,
        } => {
            let pp = if let Some(pp) = passphrase {
                pp
            } else {
                readline_masked("Passphrase: ")?
            };

            let holo = holochain.unwrap_or(true);

            let result = ad4m_client.agent.unlock(pp, holo).await?;
            if let Some(error) = result.error {
                bail!(error);
            } else {
                println!("Agent unlocked");
            }
        }
        AgentFunctions::ByDID { did } => {
            if let Some(agent) = ad4m_client.agent.by_did(did).await? {
                print_agent(agent.into());
            } else {
                println!("Agent not found");
            }
        }
        AgentFunctions::Generate { passphrase } => {
            let pp = if let Some(pp) = passphrase {
                pp
            } else {
                let passphrase1 = readline_masked("Passphrase: ")?;
                let passphrase2 = readline_masked("Repeat passphrase: ")?;
                if passphrase1 != passphrase2 {
                    bail!("Passphrases do not match");
                }
                passphrase1
            };

            let result = ad4m_client.agent.generate(pp).await?;
            if let Some(error) = result.error {
                bail!(error);
            } else {
                println!("Agent generated");
            }
        }
        AgentFunctions::AddEntanglementProof {
            device_key,
            device_key_signed_by_did,
            device_key_type,
            did,
            did_signed_by_device_key,
            did_signing_key_id,
        } => {
            let input = EntanglementProof {
                device_key,
                device_key_signed_by_did: Some(device_key_signed_by_did),
                device_key_type,
                did,
                did_signed_by_device_key: Some(did_signed_by_device_key),
                did_signing_key_id,
            };
            ad4m_client
                .agent
                .add_entanglement_proofs(vec![input])
                .await?;
            println!("Entanglement proofs added!");
        }
        AgentFunctions::DeleteEntanglementProof {
            device_key,
            device_key_signed_by_did,
            device_key_type,
            did,
            did_signed_by_device_key,
            did_signing_key_id,
        } => {
            let input = EntanglementProof {
                device_key,
                device_key_signed_by_did: Some(device_key_signed_by_did),
                device_key_type,
                did,
                did_signed_by_device_key: Some(did_signed_by_device_key),
                did_signing_key_id,
            };
            ad4m_client
                .agent
                .delete_entanglement_proofs(vec![input])
                .await?;
            println!("Entanglement proofs removed!");
        }
        AgentFunctions::EntanglementProofPreFlight {
            device_key,
            device_key_type,
        } => {
            let result = ad4m_client
                .agent
                .entanglement_proof_pre_flight(device_key, device_key_type)
                .await?;
            println!("Add preflight!: {:#?}", result);
        }
        AgentFunctions::GenerateJwt { request_id, rand } => {
            let result = ad4m_client
                .agent
                .retrieve_capability(request_id, rand)
                .await?;
            println!("JWT: {:#?}", result);
        }
        AgentFunctions::Watch {} => {
            println!("Watching for agent status changes...");
            println!("(Press Ctrl+C to stop)\n");
            let mut rx = ad4m_client.subscribe_events();
            loop {
                match rx.recv().await {
                    Ok(event) => {
                        let event_type = event.get("type").and_then(|v| v.as_str()).unwrap_or("");
                        match event_type {
                            "agent-status-changed" => {
                                if let Some(agent) = event.get("agent") {
                                    println!("Agent status changed: {}", agent);
                                } else {
                                    println!("Agent status changed: {}", event);
                                }
                            }
                            "agent-updated" => {
                                if let Some(agent) = event.get("agent") {
                                    println!("Agent updated: {}", agent);
                                } else {
                                    println!("Agent updated: {}", event);
                                }
                            }
                            _ => {}
                        }
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Lagged(n)) => {
                        eprintln!("Warning: dropped {} events (too slow)", n);
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Closed) => {
                        eprintln!("Event stream closed");
                        break;
                    }
                }
            }
        }
    };
    Ok(())
}
