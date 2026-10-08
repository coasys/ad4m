#![allow(dead_code)]

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

// Tune jemalloc to return freed memory to the OS more aggressively. jemalloc
// reads `MALLOC_CONF` (or, with tikv-jemallocator's prefixed symbols on
// macOS/Linux, `_RJEM_MALLOC_CONF`) at process start. With the defaults,
// peak allocations from bursty work — most notably SPARQL queries that
// materialise the full link set into a Vec<DecoratedLinkExpression> — sit
// in jemalloc arenas indefinitely, inflating RSS even after the Rust-side
// memory has been dropped. The wind tunnel was attributing this to a leak:
// RSS would step up ~5–7 MB per query and never come back down within the
// monitor window.
//
// jemalloc reads its config on first allocation, which happens before
// main(), so the only reliable way to apply this from the binary itself
// is to override the static `malloc_conf` symbol that jemalloc looks up
// at init time. With tikv-jemallocator the symbol is exposed as
// `_rjem_malloc_conf` on prefixed builds (default on macOS/Linux).
//
// Operators / test harnesses can override these defaults by exporting
// `_RJEM_MALLOC_CONF=...` (or `MALLOC_CONF=...` on unprefixed builds)
// before launching ad4m-executor.
//
// - background_thread:true   purge runs on a background thread
// - dirty_decay_ms:1000      release dirty pages 10× faster than default
// - muzzy_decay_ms:1000      same for muzzy pages
#[cfg(not(target_env = "msvc"))]
#[allow(non_upper_case_globals)]
#[export_name = "_rjem_malloc_conf"]
pub static _rjem_malloc_conf: Option<&'static [u8]> =
    Some(b"background_thread:true,dirty_decay_ms:1000,muzzy_decay_ms:1000\0");

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
mod bootstrap_publish;
mod dev;
mod expression;
mod languages;
mod neighbourhoods;
mod perspectives;
mod repl;
mod run_config;
mod runtime;

use anyhow::Result;
use clap::{Parser, Subcommand};
use dev::DevFunctions;
use run_config::{process_env, ResolvedRun, RunArgs};

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
    Run(RunArgs),
    /// Inspect the settings `run` would start with
    Config {
        #[command(subcommand)]
        command: ConfigCommand,
    },
}

#[derive(Debug, Subcommand)]
enum ConfigCommand {
    /// Print the merged settings (config file < AD4M_* env < flags) as JSON,
    /// with every secret replaced by "<redacted>". Takes the flags of `run`.
    Print(RunArgs),
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

    if let Domain::Config {
        command: ConfigCommand::Print(run_args),
    } = args.domain
    {
        let resolved = run_args.resolve(process_env)?;
        println!(
            "{}",
            serde_json::to_string_pretty(&resolved.redacted_json())?
        );
        return Ok(());
    }

    if let Domain::Run(run_args) = args.domain {
        let ResolvedRun {
            config,
            unlock_passphrase,
        } = run_args.resolve(process_env)?;
        let startup = tokio::spawn(async move { rust_executor::run(config).await }).await;
        // Exit 1 when the REST API fails (e.g. the port is taken), instead of
        // running on with no API.
        match startup {
            Ok(api_thread) => rust_executor::exit_when_api_fails(api_thread),
            Err(e) => {
                eprintln!("rust_executor::run panicked during startup: {e}");
                exit(1);
            }
        }
        if let Some(passphrase) = unlock_passphrase {
            tokio::spawn(rust_executor::unlock_agent_at_startup(passphrase));
        }

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

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::run_config::tests::lock_env;

    fn run_admin_credential(argv: &[&str]) -> Option<String> {
        let app = ClapApp::try_parse_from(argv).expect("argv parses");
        match app.domain {
            Domain::Run(run) => run.admin_credential,
            other => panic!("expected the run subcommand, got {other:?}"),
        }
    }

    /// `--admin-credential` on the command line is visible in `ps` and shell
    /// history, so the documented way to pass it is the environment. Both
    /// must land in the same field, the explicit flag winning when both are
    /// set.
    #[test]
    fn run_reads_the_admin_credential_from_the_environment() {
        let _env = lock_env();
        std::env::set_var("AD4M_ADMIN_CREDENTIAL", "from-env");
        assert_eq!(
            run_admin_credential(&["ad4m-executor", "run"]).as_deref(),
            Some("from-env")
        );
        assert_eq!(
            run_admin_credential(&["ad4m-executor", "run", "--admin-credential", "from-flag"])
                .as_deref(),
            Some("from-flag"),
            "an explicit flag overrides the environment"
        );
        std::env::remove_var("AD4M_ADMIN_CREDENTIAL");
        assert_eq!(
            run_admin_credential(&["ad4m-executor", "run"]),
            None,
            "no flag and no variable means no credential"
        );
    }

    fn run_insecure_no_admin_credential(argv: &[&str]) -> bool {
        let app = ClapApp::try_parse_from(argv).expect("argv parses");
        match app.domain {
            Domain::Run(run) => run.insecure_no_admin_credential,
            other => panic!("expected the run subcommand, got {other:?}"),
        }
    }

    /// The testing flag is off unless set on the command line or through
    /// AD4M_INSECURE_NO_ADMIN_CREDENTIAL with a truthy value.
    #[test]
    fn run_reads_the_testing_flag_from_argv_and_environment() {
        let _env = lock_env();
        std::env::remove_var("AD4M_INSECURE_NO_ADMIN_CREDENTIAL");
        assert!(!run_insecure_no_admin_credential(&["ad4m-executor", "run"]));
        assert!(run_insecure_no_admin_credential(&[
            "ad4m-executor",
            "run",
            "--insecure-no-admin-credential"
        ]));
        std::env::set_var("AD4M_INSECURE_NO_ADMIN_CREDENTIAL", "true");
        assert!(run_insecure_no_admin_credential(&["ad4m-executor", "run"]));
        std::env::set_var("AD4M_INSECURE_NO_ADMIN_CREDENTIAL", "false");
        assert!(!run_insecure_no_admin_credential(&["ad4m-executor", "run"]));
        std::env::remove_var("AD4M_INSECURE_NO_ADMIN_CREDENTIAL");
    }

    /// An "off" variable leaves the mode off. Anything but `true` never
    /// enables it; unknown values are rejected. An empty variable is an
    /// error that names it, as for every other `AD4M_<FLAG>`.
    #[test]
    fn only_true_enables_the_testing_flag() {
        let _env = lock_env();
        for off in ["0", "false", "no", "off"] {
            std::env::set_var("AD4M_INSECURE_NO_ADMIN_CREDENTIAL", off);
            assert!(
                !run_insecure_no_admin_credential(&[
                    "ad4m-executor",
                    "run",
                    "--admin-credential",
                    "secret"
                ]),
                "{off:?}"
            );
        }
        for invalid in ["1", "TRUE", "yes"] {
            std::env::set_var("AD4M_INSECURE_NO_ADMIN_CREDENTIAL", invalid);
            assert!(
                ClapApp::try_parse_from(["ad4m-executor", "run"]).is_err(),
                "{invalid:?}"
            );
        }
        std::env::set_var("AD4M_INSECURE_NO_ADMIN_CREDENTIAL", "");
        let err = ClapApp::try_parse_from(["ad4m-executor", "run", "--admin-credential", "secret"])
            .expect_err("an empty variable is an error");
        assert!(
            err.to_string()
                .contains("AD4M_INSECURE_NO_ADMIN_CREDENTIAL is set but empty"),
            "{err}"
        );
        std::env::remove_var("AD4M_INSECURE_NO_ADMIN_CREDENTIAL");
        assert!(!run_insecure_no_admin_credential(&[
            "ad4m-executor",
            "run",
            "--insecure-no-admin-credential=false"
        ]));
        assert!(run_insecure_no_admin_credential(&[
            "ad4m-executor",
            "run",
            "--insecure-no-admin-credential",
            "--localhost",
            "true"
        ]));
    }

    /// The help text must not echo the variable's value.
    #[test]
    fn run_help_hides_the_environment_value() {
        let _env = lock_env();
        std::env::set_var("AD4M_ADMIN_CREDENTIAL", "s3cret-value");
        let err = ClapApp::try_parse_from(["ad4m-executor", "run", "--help"])
            .expect_err("--help exits through an error");
        let help = err.to_string();
        std::env::remove_var("AD4M_ADMIN_CREDENTIAL");
        assert!(help.contains("AD4M_ADMIN_CREDENTIAL"), "{help}");
        assert!(!help.contains("s3cret-value"), "{help}");
    }
    fn parse_run(argv: &[&str]) -> RunArgs {
        match ClapApp::try_parse_from(argv).expect("argv parses").domain {
            Domain::Run(run) => run,
            other => panic!("expected the run subcommand, got {other:?}"),
        }
    }

    /// A config file's SMTP block, with the password from the environment,
    /// reaches the executor's `Ad4mConfig.smtp_config`, so a headless node
    /// can send verification emails like the launcher does.
    #[test]
    fn run_config_takes_smtp_from_the_config_file() {
        let _env = lock_env();
        let dir = std::env::temp_dir().join(format!("ad4m-cli-smtp-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("executor-config.json");
        std::fs::write(
            &path,
            r#"{
                "multi_user_config": {
                    "enabled": true,
                    "tls_config": null,
                    "smtp_config": {
                        "enabled": true,
                        "host": "smtp.example",
                        "port": 465,
                        "username": "ad4m@example",
                        "from_address": "ad4m@example"
                    }
                }
            }"#,
        )
        .unwrap();
        std::env::set_var("AD4M_CONFIG", &path);
        std::env::set_var("AD4M_SMTP_PASSWORD", "smtp-secret");
        let config = parse_run(&["ad4m-executor", "run"])
            .resolve(crate::run_config::process_env)
            .map(|resolved| resolved.config);
        std::env::remove_var("AD4M_CONFIG");
        std::env::remove_var("AD4M_SMTP_PASSWORD");
        let _ = std::fs::remove_dir_all(&dir);

        let config = config.expect("the config resolves");
        assert_eq!(config.enable_multi_user, Some(true));
        let smtp = config
            .smtp_config
            .expect("the config file's SMTP settings reach Ad4mConfig");
        assert!(smtp.enabled);
        assert_eq!(smtp.host, "smtp.example");
        assert_eq!(smtp.port, 465);
        assert_eq!(smtp.username, "ad4m@example");
        assert_eq!(smtp.from_address, "ad4m@example");
        assert_eq!(smtp.password, "smtp-secret");
    }
}
