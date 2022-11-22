extern crate ad4m_client;
extern crate anyhow;
extern crate chrono;
extern crate clap;
extern crate dirs;
extern crate rand;
extern crate regex;
extern crate rustyline;
extern crate tokio;

mod formatting;
mod startup;
mod util;

mod agent;
mod languages;
mod perspectives;
mod neighbourhoods;

use ad4m_client::*;
use crate::{agent::*, languages::*, perspectives::*, neighbourhoods::*};
use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use formatting::{
    print_message_perspective,
    print_sent_message_perspective,
};
use startup::executor_data_path;

use crate::util::string_2_perspective_snapshot;

/// AD4M command line interface.
/// https://ad4m.dev
///                                                                                                                               .xXKkd:'                         
///                                                                                                                              .oNOccx00x;.                      
///                                                                                                                              ;KK;  .ck0Oxdolc;..               
///                                                                                                                              lWk:oOK0OxdoxOOO0KOd;.            
///                                                                                                                              dWkdkl,.    .o0x;'cxK0l.          
///       .,ldxxxxxxoc.         .cdxxxxxxxxxxxdoc,.             'oxo'     ;dx;     .;dxxxo:.               .;odxxx:             .dWk'         .dNx.  'o0O:         
///      .xNWNKKKKKXWWK:       'OWWX0000000000KNWWKo.          ,0WNd.     dWWd.    .oWMNXNWKc.            :0WWXNMMk.           :dxX0,      .,cllOXo:oooxkd,        
///     .xWWk,......lXMK;      ;XMK:. . .  ....':kNWK;        ,0MNo.      oWWd.    .oWMk':0MNo.          cXMXc'dWMx.          ,0KlxNd. .;dO00kd;cK0c:loxkOko:.     
///     cNM0'        oNMk.     ;XM0'             .lXM0'      ;0MNo.       oWWd.    .oWMx. ,0MNl         :KMX:  lWMx.         .dWx.'kkcd0Kxc'.   .kK:    .;dkO0d,   
///    ,KMX:         .kWWo.    ;XM0'              .kMNc     ;KMNo.        oWWd.    .oWWd.  ;KMXl       :KMXc   lWMx.         .ONc  ;kX0o.       .xXc     'kk::kKx,
///   .kWWo.          ,KMX:    ;XM0'              .OMNc    ;KMNo.         oWWd.    .oWMd.   ;KMXc     ;KMXc    lWMx.         .ONc.cKKddx:.      .OK;     ,00, .c00c
///   lNMO.            lNMO'   ;XM0'             .dNMO.   :KMMKocccccccccl0WM0l;.  .oWMx.    :KMXc   ;0MNl     lWMx.         .dNxlKO,.,x0Ol'    :Kk.     cXk'.:d0x;
///  ;KMK;             .xWWd.  ;XMXc..........';o0WWO,   ;0WWWWWWWWWWWWWWWMMMMW0,  .oWWd.     :XMXl,c0MNl.     lWMx.          ,0Xxc'    'lk0ko:;lx:.    ,OKookko,.
/// .kMNo               ,0MXc  .kWMWNNNNNXNNNNNWWXk:.    .,;;;;;;;;;;;;;,:OMWO:'   .oWMd.      :0WWWWWKc.      lWMx.           ;0Kc.      .':okOOOkkxdcck0l,:,.    
/// .co:.                ,ll,   .;lllloolllllllc;.                        'll'      'll,        .;cll;.        'll,            ,dOKx,        ;xd:,,;cox0k;         
///                                                                                                                           .d0lck0kc,.  ,xKk;..':dOkc.          
///                                                                                                                           .dNo..,lddllk0Oxddxkkxl,.            
///                                                                                                                            lXk;';cdO0ko,':lc:,.                
///                                                                                                                            'x0Okkdl:'.                         
///                                                                                                                             .,'..                               
/// This is a full featured AD4M client.
/// Provides all means of interacting with the AD4M executor / agent.
/// See help of commands for more information.
#[derive(Parser, Debug)]
#[command(author, version, verbatim_doc_comment)]
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

#[tokio::main]
async fn main() -> Result<()> {
    set_executor_url(crate::startup::get_executor_url()?);
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
        Domain::Agent { command } => agent::run(cap_token, command).await?,
        Domain::Languages { command } => languages::run(cap_token, command).await?,
        Domain::Perspectives { command } => perspectives::run(cap_token, command).await?,
        Domain::Neighbourhoods { command } => neighbourhoods::run(cap_token, command).await?,
        Domain::Runtime { command } => match command {
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
        },
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
