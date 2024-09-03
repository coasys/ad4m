#![allow(dead_code)]
extern crate ad4m_client;
extern crate anyhow;
extern crate chrono;
extern crate clap;
extern crate dirs;
extern crate kitsune_p2p_types;
extern crate rand;
extern crate regex;
extern crate rustyline;
extern crate tokio;

mod formatting;
mod startup;
mod util;

mod agent;
mod bootstrap_publish;
mod dev;
mod eve;
mod expression;
mod languages;
mod neighbourhoods;
mod perspectives;
mod repl;
mod runtime;

use crate::{
    agent::*, eve::*, expression::*, languages::*, neighbourhoods::*, perspectives::*, runtime::*,
};
use ad4m_client::*;
use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use startup::executor_data_path;

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

    /// Don't request/use capability token - provide empty string
    #[arg(short, long, action)]
    no_capability: bool,

    /// Override default executor URL look-up and provide custom URL
    #[arg(short, long)]
    executor_url: Option<String>,

    /// Provide admin credential to gain all capabilities
    #[arg(short, long)]
    admin_credential: Option<String>,
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
    /// Create and get language expressions
    Expression {
        #[command(subcommand)]
        command: ExpressionFunctions,
    },
    /// Print the executor log
    Log,
    Eve {
        #[command(subcommand)]
        command: EveCommands,
    },
}

async fn get_ad4m_client(args: &ClapApp) -> Result<Ad4mClient> {
    let executor_url = if let Some(custom_url) = args.executor_url.clone() {
        custom_url
    } else {
        crate::startup::get_executor_url()?
    };

    let cap_token = if let Some(admin_credential) = &args.admin_credential {
        admin_credential.clone()
    } else if args.no_capability {
        "".to_string()
    } else {
        match &args.domain {
            Domain::Log => "".to_string(),
            Domain::Agent {
                command:
                    AgentFunctions::Lock
                    | AgentFunctions::Unlock { .. }
                    | AgentFunctions::Generate { .. },
            } => "".to_string(),
            _ => startup::get_cap_token(executor_url.clone()).await?,
        }
    };

    let ad4m_client = Ad4mClient::new(executor_url, cap_token);

    Ok(ad4m_client)
}

#[tokio::main(flavor = "multi_thread")]
async fn main() -> Result<()> {
    let args = ClapApp::parse();

    let ad4m_client = get_ad4m_client(&args).await?;

    match args.domain {
        Domain::Agent { command } => agent::run(ad4m_client, command).await?,
        Domain::Languages { command } => languages::run(ad4m_client, command).await?,
        Domain::Perspectives { command } => perspectives::run(ad4m_client, command).await?,
        Domain::Neighbourhoods { command } => neighbourhoods::run(ad4m_client, command).await?,
        Domain::Runtime { command } => runtime::run(ad4m_client, command).await?,
        Domain::Expression { command } => expression::run(ad4m_client, command).await?,
        Domain::Log => {
            let file = executor_data_path().join("ad4m.log");
            let log = std::fs::read_to_string(file.clone()).with_context(|| {
                format!(
                    "Could not read log file `{}`!\nIs AD4M executor running?",
                    file.display()
                )
            })?;
            println!("{}", log);
        }
        Domain::Eve { command } => eve::run(command).await?,
    }

    Ok(())
}
