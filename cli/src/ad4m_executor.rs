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
mod expression;
mod languages;
mod neighbourhoods;
mod perspectives;
mod repl;
mod runtime;

use ad4m_client::*;
use anyhow::Result;
use clap::{Parser, Subcommand};
use dev::DevFunctions;
use rust_executor::{config::TlsConfig, Ad4mConfig};

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
    /// Print the executor log
    Log,
    Dev {
        #[command(subcommand)]
        command: DevFunctions,
    },
    Init {
        #[arg(short, long, action)]
        data_path: Option<String>,
        #[arg(short, long, action)]
        network_bootstrap_seed: Option<String>,
    },
    Run {
        #[arg(short, long, action)]
        app_data_path: Option<String>,
        #[arg(short, long, action)]
        network_bootstrap_seed: Option<String>,
        #[arg(short, long, action)]
        language_language_only: Option<bool>,
        #[arg(long, action)]
        run_dapp_server: Option<bool>,
        #[arg(short, long, action)]
        gql_port: Option<u16>,
        #[arg(long, action)]
        hc_admin_port: Option<u16>,
        #[arg(long, action)]
        hc_app_port: Option<u16>,
        #[arg(long, action)]
        hc_use_bootstrap: Option<bool>,
        #[arg(long, action)]
        hc_use_local_proxy: Option<bool>,
        #[arg(long, action)]
        hc_use_mdns: Option<bool>,
        #[arg(long, action)]
        hc_use_proxy: Option<bool>,
        #[arg(long, action)]
        hc_proxy_url: Option<String>,
        #[arg(long, action)]
        hc_bootstrap_url: Option<String>,
        #[arg(short, long, action)]
        connect_holochain: Option<bool>,
        #[arg(long, action)]
        admin_credential: Option<String>,
        #[arg(long, action)]
        localhost: Option<bool>,
        #[arg(short, long, action)]
        tls_cert_file: Option<String>,
        #[arg(short, long, action)]
        tls_key_file: Option<String>,
        #[arg(short, long, action)]
        log_holochain_metrics: Option<bool>,
    },
    RunLocalHcServices {},
}

#[tokio::main(flavor = "multi_thread")]
async fn main() -> Result<()> {
    let args = ClapApp::parse();

    if let Domain::Dev { command } = args.domain {
        dev::run(command).await?;
        return Ok(());
    };

    if let Domain::Init {
        data_path,
        network_bootstrap_seed,
    } = args.domain
    {
        match rust_executor::init::init(data_path, network_bootstrap_seed) {
            Ok(()) => println!("Successfully initialized AD4M executor!"),
            Err(e) => {
                println!("Failed to initialize AD4M executor: {}", e);
                std::process::exit(1);
            }
        };
        return Ok(());
    };

    if let Domain::Run {
        app_data_path,
        network_bootstrap_seed,
        language_language_only,
        run_dapp_server,
        gql_port,
        hc_admin_port,
        hc_app_port,
        hc_use_bootstrap,
        hc_use_local_proxy,
        hc_use_mdns,
        hc_use_proxy,
        hc_proxy_url,
        hc_bootstrap_url,
        connect_holochain,
        admin_credential,
        localhost,
        tls_cert_file,
        tls_key_file,
        log_holochain_metrics,
    } = args.domain
    {
        let tls = if tls_cert_file.is_some() && tls_cert_file.is_some() {
            Some(TlsConfig {
                cert_file_path: tls_cert_file.unwrap(),
                key_file_path: tls_key_file.unwrap(),
            })
        } else {
            if tls_cert_file.is_some() || tls_key_file.is_some() {
                println!("To active TLS encryption, please provide arguments: tls_cert_file and tls_key_file!");
            }
            None
        };
        let _ = tokio::spawn(async move {
            rust_executor::run(Ad4mConfig {
                app_data_path,
                network_bootstrap_seed,
                language_language_only,
                run_dapp_server,
                gql_port,
                hc_admin_port,
                hc_app_port,
                hc_use_bootstrap,
                hc_use_local_proxy,
                hc_use_mdns,
                hc_use_proxy,
                hc_proxy_url,
                hc_bootstrap_url,
                connect_holochain,
                admin_credential,
                localhost,
                auto_permit_cap_requests: Some(true),
                tls,
                log_holochain_metrics,
            })
            .await;
        })
        .await;

        let _ = ctrlc::set_handler(move || {
            println!("Received CTRL-C! Exiting...");
            exit(0);
        });

        use ctrlc;
        use std::process::exit;
        use std::time::Duration;
        use tokio::time::sleep;

        loop {
            sleep(Duration::from_secs(2)).await;
        }
    };

    if let Domain::RunLocalHcServices {} = args.domain {
        rust_executor::run_local_hc_services().await?;
        return Ok(());
    }

    Ok(())
}
