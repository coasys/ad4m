use clap::{Parser, Subcommand};
use log::error;
use rust_executor::{config::TlsConfig, Ad4mConfig};

#[derive(Parser, Debug)]
#[command(name = "ad4m-executor")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Run {
        #[arg(short, long)]
        app_data_path: Option<String>,
        #[arg(short, long)]
        network_bootstrap_seed: Option<String>,
        #[arg(short, long)]
        language_language_only: Option<bool>,
        #[arg(long)]
        run_dapp_server: Option<bool>,
        #[arg(short, long)]
        gql_port: Option<u16>,
        #[arg(long)]
        hc_admin_port: Option<u16>,
        #[arg(long)]
        hc_app_port: Option<u16>,
        #[arg(long)]
        hc_use_bootstrap: Option<bool>,
        #[arg(long)]
        hc_use_local_proxy: Option<bool>,
        #[arg(long)]
        hc_use_mdns: Option<bool>,
        #[arg(long)]
        hc_use_proxy: Option<bool>,
        #[arg(long)]
        hc_proxy_url: Option<String>,
        #[arg(long)]
        hc_bootstrap_url: Option<String>,
        #[arg(long)]
        hc_relay_url: Option<String>,
        #[arg(short, long)]
        connect_holochain: Option<bool>,
        #[arg(long)]
        admin_credential: Option<String>,
        #[arg(long)]
        localhost: Option<bool>,
        #[arg(long)]
        tls_cert_file: Option<String>,
        #[arg(long)]
        tls_key_file: Option<String>,
        #[arg(long)]
        tls_port: Option<u16>,
        #[arg(long)]
        log_holochain_metrics: Option<bool>,
        #[arg(long)]
        enable_multi_user: Option<bool>,
        #[arg(long)]
        enable_mcp: Option<bool>,
        #[arg(long)]
        mcp_port: Option<u16>,
        #[arg(long)]
        pid_file: Option<String>,
    },
}

#[tokio::main(flavor = "multi_thread")]
async fn main() {
    let cli = Cli::parse();

    match cli.command {
        Command::Run {
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
            hc_relay_url,
            connect_holochain,
            admin_credential,
            localhost,
            tls_cert_file,
            tls_key_file,
            tls_port,
            log_holochain_metrics,
            enable_multi_user,
            enable_mcp,
            mcp_port,
            pid_file,
        } => {
            let tls = if tls_cert_file.is_some() && tls_key_file.is_some() {
                Some(TlsConfig {
                    cert_file_path: tls_cert_file.unwrap(),
                    key_file_path: tls_key_file.unwrap(),
                    tls_port: tls_port.unwrap_or(12001),
                })
            } else {
                None
            };

            let config = Ad4mConfig {
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
                hc_relay_url,
                connect_holochain,
                admin_credential,
                localhost,
                auto_permit_cap_requests: Some(true),
                tls,
                log_holochain_metrics,
                enable_multi_user,
                smtp_config: None,
                enable_mcp,
                mcp_port,
                pid_file,
            };

            let handle = rust_executor::run(config).await;

            if let Err(e) = handle.join() {
                error!("Executor thread panicked: {:?}", e);
                std::process::exit(1);
            }
        }
    }
}
