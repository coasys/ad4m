use crate::formatting::{print_message_perspective, print_sent_message_perspective};
use ad4m_client::Ad4mClient;
use anyhow::Result;
use clap::Subcommand;

use crate::util::string_2_perspective_snapshot;

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
    HcAddAgentInfos {
        infos: String,
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
            ad4m_client.runtime.add_link_language_templates(addresses).await?;
            println!("Link language templates added!");
        }
        RuntimeFunctions::RemoveLinkLanguageTemplates { addresses } => {
            ad4m_client.runtime.remove_link_language_templates(addresses).await?;
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
        RuntimeFunctions::HcAgentInfos => {
            let infos = ad4m_client.runtime.hc_agent_infos().await?;
            println!("{}", infos);
        }
        RuntimeFunctions::HcAddAgentInfos { infos } => {
            ad4m_client.runtime.hc_add_agent_infos(infos).await?;
            println!("Holochain agent infos added!");
        }
        RuntimeFunctions::VerifySignature {
            did,
            did_signing_key,
            data,
            signed_data,
        } => {
            let result = ad4m_client.runtime.verify_string_signed_by_did(
                did,
                did_signing_key,
                data,
                signed_data,
            )
            .await?;
            println!("{:?}", result);
        }
        RuntimeFunctions::SetStatus { status } => {
            let perspective = string_2_perspective_snapshot(&ad4m_client, status).await?;
            ad4m_client.runtime.set_status(perspective.into()).await?;
            println!("Status set!");
        }
        RuntimeFunctions::FriendStatus { agent } => {
            let status = ad4m_client.runtime.friend_status(agent).await?;
            println!("{:?}", status.runtime_friend_status);
        }
        RuntimeFunctions::FriendSendMessage { agent, message } => {
            let message = string_2_perspective_snapshot(&ad4m_client, message).await?;
            ad4m_client.runtime.friend_send_message(agent, message.into()).await?;
            println!("Message sent!");
        }
        RuntimeFunctions::MessageInbox { filter } => {
            let messages = ad4m_client.runtime.message_inbox(filter).await?;
            for message in messages {
                print_message_perspective(message);
                println!();
            }
        }
        RuntimeFunctions::MessageOutbox { filter } => {
            let messages = ad4m_client.runtime.message_outbox(filter).await?;
            for message in messages {
                print_sent_message_perspective(message);
                println!();
            }
        }
    };
    Ok(())
}
