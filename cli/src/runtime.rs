use crate::util::string_2_perspective_snapshot;
use ad4m_client::Ad4mClient;
use anyhow::Result;
use clap::Subcommand;

#[derive(Debug, Subcommand)]
pub enum RuntimeFunctions {
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
    NetworkMetrics,
    HcAddAgentInfos {
        infos_file: Option<String>,
    },
    VerifySignature {
        did: String,
        did_signing_key_id: Option<String>,
        data: String,
        signed_data: String,
    },
    SetStatus {
        status: String,
    },
    MessageInbox {
        filter: Option<String>,
    },
    MessageOutbox {
        filter: Option<String>,
    },
}

pub async fn run(ad4m_client: Ad4mClient, command: RuntimeFunctions) -> Result<()> {
    match command {
        RuntimeFunctions::Info => {
            let info = ad4m_client.runtime.info().await?;
            println!("{:#?}", info);
        }
        RuntimeFunctions::Quit => {
            ad4m_client.runtime.quit().await?;
            println!("Executor shut down!");
        }
        RuntimeFunctions::AddTrustedAgents { agents } => {
            ad4m_client.runtime.add_trusted_agents(agents).await?;
            println!("Trusted agents added!");
        }
        RuntimeFunctions::DeleteTrustedAgents { agents } => {
            ad4m_client.runtime.delete_trusted_agents(agents).await?;
            println!("Trusted agents removed!");
        }
        RuntimeFunctions::TrustedAgents => {
            let agents = ad4m_client.runtime.trusted_agents().await?;
            for agent in agents {
                println!("{}", agent);
            }
        }
        RuntimeFunctions::LinkLanguageTemplates => {
            let templates = ad4m_client.runtime.link_language_templates().await?;
            for template in templates {
                println!("{}", template);
            }
        }
        RuntimeFunctions::AddLinkLanguageTemplates { addresses } => {
            ad4m_client
                .runtime
                .add_link_language_templates(addresses)
                .await?;
            println!("Link language templates added!");
        }
        RuntimeFunctions::RemoveLinkLanguageTemplates { addresses } => {
            ad4m_client
                .runtime
                .remove_link_language_templates(addresses)
                .await?;
            println!("Link language templates removed!");
        }
        RuntimeFunctions::Friends => {
            let friends = ad4m_client.runtime.friends().await?;
            for friend in friends {
                println!("{}", friend);
            }
        }
        RuntimeFunctions::AddFriends { agents } => {
            ad4m_client.runtime.add_friends(agents).await?;
            println!("Friends added!");
        }
        RuntimeFunctions::RemoveFriends { agents } => {
            ad4m_client.runtime.remove_friends(agents).await?;
            println!("Friends removed!");
        }
        RuntimeFunctions::NetworkMetrics => {
            let metrics = ad4m_client.runtime.network_metrics().await?;
            let parsed: serde_json::Value = serde_json::from_str(&metrics)?;
            println!("{}", serde_json::to_string_pretty(&parsed)?);
        }
        RuntimeFunctions::HcAgentInfos => {
            let infos = ad4m_client.runtime.hc_agent_infos().await?;
            println!("\x1b[36mAll AgentInfos encoded:\n \x1b[32m{}\n\n", infos);

            let separate_agent_infos: Vec<String> = serde_json::from_str(&infos)?;

            println!("\x1b[36mSeparate AgentInfos:\n");
            for agent_info in &separate_agent_infos {
                let val: serde_json::Value = serde_json::from_str(agent_info)?;
                println!("\x1b[36mAgent: \x1b[37m{:?}", val["agent"]);
                println!("\x1b[36mURL List: \x1b[94m{:?}", val["url_list"]);
            }
        }
        RuntimeFunctions::HcAddAgentInfos { infos_file } => {
            // NOTE: K2 spaces must already exist (via join) before adding agent infos,
            // otherwise the call will fail with K2SpaceNotFound.
            if let Some(infos_file) = infos_file {
                let infos = std::fs::read_to_string(infos_file)?;
                let parsed: Vec<String> = serde_json::from_str(&infos)
                    .map_err(|e| anyhow::anyhow!("Failed to parse agent infos JSON array: {e}"))?;
                ad4m_client.runtime.hc_add_agent_infos(parsed).await?;
                println!("Holochain agent infos added!");
            } else {
                let mut rl = rustyline::Editor::<()>::new()?;
                let readline = rl.readline("Please enter the encoded agent infos JSON array: ");
                match readline {
                    Ok(line) => {
                        let parsed: Vec<String> = serde_json::from_str(&line).map_err(|e| {
                            anyhow::anyhow!("Failed to parse agent infos JSON array: {e}")
                        })?;
                        ad4m_client.runtime.hc_add_agent_infos(parsed).await?;
                        println!("Holochain agent infos added!");
                    }
                    Err(_) => println!("Failed to read line"),
                }
            }
        }
        RuntimeFunctions::VerifySignature {
            did,
            did_signing_key_id,
            data,
            signed_data,
        } => {
            let result = ad4m_client
                .runtime
                .verify_string_signed_by_did(
                    did,
                    did_signing_key_id.unwrap_or_default(),
                    data,
                    signed_data,
                )
                .await?;
            println!("{:?}", result);
        }
        RuntimeFunctions::SetStatus { status } => {
            let perspective = string_2_perspective_snapshot(&ad4m_client, status).await?;
            ad4m_client
                .runtime
                .set_status(serde_json::to_value(perspective)?)
                .await?;
            println!("Status set!");
        }
        RuntimeFunctions::MessageInbox { filter } => {
            let messages = ad4m_client.runtime.message_inbox(filter).await?;
            for message in messages {
                println!("{:#?}", message);
            }
        }
        RuntimeFunctions::MessageOutbox { filter } => {
            let messages = ad4m_client.runtime.message_outbox(filter).await?;
            for message in messages {
                println!("{:#?}", message);
            }
        }
    };
    Ok(())
}
