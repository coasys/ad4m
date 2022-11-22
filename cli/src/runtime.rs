use crate::formatting::{print_message_perspective, print_sent_message_perspective};
use ad4m_client::runtime;
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

pub async fn run(cap_token: String, command: RuntimeFunctions) -> Result<()> {
    match command {
        RuntimeFunctions::Info => {
            let info = runtime::run_info(cap_token).await?;
            println!("{:#?}", info);
        }
        RuntimeFunctions::Quit => {
            runtime::run_quit(cap_token).await?;
            println!("Executor shut down!");
        }
        RuntimeFunctions::AddTrustedAgents { agents } => {
            runtime::run_add_trusted_agents(cap_token, agents).await?;
            println!("Trusted agents added!");
        }
        RuntimeFunctions::DeleteTrustedAgents { agents } => {
            runtime::run_delete_trusted_agents(cap_token, agents).await?;
            println!("Trusted agents removed!");
        }
        RuntimeFunctions::TrustedAgents => {
            let agents = runtime::run_trusted_agents(cap_token).await?;
            for agent in agents {
                println!("{}", agent);
            }
        }
        RuntimeFunctions::LinkLanguageTemplates => {
            let templates = runtime::run_link_language_templates(cap_token).await?;
            for template in templates {
                println!("{}", template);
            }
        }
        RuntimeFunctions::AddLinkLanguageTemplates { addresses } => {
            runtime::run_add_link_language_templates(cap_token, addresses).await?;
            println!("Link language templates added!");
        }
        RuntimeFunctions::RemoveLinkLanguageTemplates { addresses } => {
            runtime::run_remove_link_language_templates(cap_token, addresses).await?;
            println!("Link language templates removed!");
        }
        RuntimeFunctions::Friends => {
            let friends = runtime::run_friends(cap_token).await?;
            for friend in friends {
                println!("{}", friend);
            }
        }
        RuntimeFunctions::AddFriends { agents } => {
            runtime::run_add_friends(cap_token, agents).await?;
            println!("Friends added!");
        }
        RuntimeFunctions::RemoveFriends { agents } => {
            runtime::run_remove_friends(cap_token, agents).await?;
            println!("Friends removed!");
        }
        RuntimeFunctions::HcAgentInfos => {
            let infos = runtime::run_hc_agent_infos(cap_token).await?;
            println!("{}", infos);
        }
        RuntimeFunctions::HcAddAgentInfos { infos } => {
            runtime::run_hc_add_agent_infos(cap_token, infos).await?;
            println!("Holochain agent infos added!");
        }
        RuntimeFunctions::VerifySignature {
            did,
            did_signing_key,
            data,
            signed_data,
        } => {
            let result = runtime::run_verify_string_signed_by_did(
                cap_token,
                did,
                did_signing_key,
                data,
                signed_data,
            )
            .await?;
            println!("{:?}", result);
        }
        RuntimeFunctions::SetStatus { status } => {
            let perspective = string_2_perspective_snapshot(cap_token.clone(), status).await?;
            runtime::run_set_status(cap_token, perspective.into()).await?;
            println!("Status set!");
        }
        RuntimeFunctions::FriendStatus { agent } => {
            let status = runtime::run_friend_status(cap_token, agent).await?;
            println!("{:?}", status.runtime_friend_status);
        }
        RuntimeFunctions::FriendSendMessage { agent, message } => {
            let message = string_2_perspective_snapshot(cap_token.clone(), message).await?;
            runtime::run_friend_send_message(cap_token, agent, message.into()).await?;
            println!("Message sent!");
        }
        RuntimeFunctions::MessageInbox { filter } => {
            let messages = runtime::run_message_inbox(cap_token, filter).await?;
            for message in messages {
                print_message_perspective(message);
                println!();
            }
        }
        RuntimeFunctions::MessageOutbox { filter } => {
            let messages = runtime::run_message_outbox(cap_token, filter).await?;
            for message in messages {
                print_sent_message_perspective(message);
                println!();
            }
        }
    };
    Ok(())
}
