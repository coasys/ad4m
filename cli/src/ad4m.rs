#![allow(dead_code)]
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
mod login;
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

use crate::{
    agent::*, expression::*, languages::*, neighbourhoods::*, perspectives::*, runtime::*,
};
use ad4m_client::connect::normalize_base_url;
use ad4m_client::Ad4mClient;
use anyhow::{bail, Context, Result};
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

    /// Permit a plaintext executor that is not on loopback.
    /// Applies to every command, including `login`.
    #[arg(long, global = true)]
    allow_insecure_remote: bool,
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

    /// Log in to an executor and store a token for that URL
    Login {
        /// Executor URL (`http://` or `ws://`)
        url: String,

        /// Email for a multi-user executor
        #[arg(long)]
        email: Option<String>,
    },
}

async fn get_ad4m_client(args: &ClapApp, domain: &Domain) -> Result<Ad4mClient> {
    let executor_url = normalize_base_url(&if let Some(custom_url) = args.executor_url.clone() {
        custom_url
    } else {
        crate::startup::get_executor_url()?
    });

    // Before the token is copied into the socket URL. `login` calls this
    // same function. Every command, including `tui`, comes through here.
    login::credential_transport(&executor_url, args.allow_insecure_remote)?;

    let cap_token = if let Some(admin_credential) = &args.admin_credential {
        admin_credential.clone()
    } else if args.no_capability {
        String::new()
    } else {
        match domain {
            Domain::Log => String::new(),
            Domain::Agent {
                command:
                    AgentFunctions::Lock
                    | AgentFunctions::Unlock { .. }
                    | AgentFunctions::Generate { .. },
            } => String::new(),
            Domain::Login { .. } => String::new(),
            _ => match login::stored_token(&executor_url)? {
                Some(token) => token,
                None if login::is_loopback(&executor_url) => String::new(),
                None => {
                    bail!("No token stored for {executor_url}. Run `ad4m login {executor_url}`.")
                }
            },
        }
    };

    let ad4m_client = Ad4mClient::connect(executor_url.clone(), cap_token)
        .await
        .with_context(|| format!("cannot connect to {executor_url}"))?;

    Ok(ad4m_client)
}

#[tokio::main(flavor = "multi_thread")]
async fn main() -> Result<()> {
    let args = ClapApp::parse();

    if let Domain::Login { url, email } = &args.domain {
        return login::login(
            url,
            email.clone(),
            args.admin_credential.as_deref(),
            args.allow_insecure_remote,
        )
        .await;
    }

    let ad4m_client = get_ad4m_client(&args, &args.domain).await?;

    match args.domain {
        Domain::Login { .. } => unreachable!("login returns above"),
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
    }

    Ok(())
}

#[cfg(test)]
mod client_transport_tests {
    use super::*;
    use crate::login::transport_probe;
    use crate::runtime::RuntimeFunctions;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Arc;
    use std::time::Duration;
    use tokio::io::AsyncReadExt;

    const SECRET: &str = "admin-secret-should-not-travel";

    struct TokenDirGuard {
        previous: Option<std::ffi::OsString>,
    }

    impl TokenDirGuard {
        fn set(path: &std::path::Path) -> Self {
            let previous = std::env::var_os("AD4M_TOKEN_DIR");
            std::env::set_var("AD4M_TOKEN_DIR", path);
            Self { previous }
        }
    }

    impl Drop for TokenDirGuard {
        fn drop(&mut self) {
            match self.previous.take() {
                Some(value) => std::env::set_var("AD4M_TOKEN_DIR", value),
                None => std::env::remove_var("AD4M_TOKEN_DIR"),
            }
        }
    }

    async fn hold_token_dir() -> (
        tokio::sync::MutexGuard<'static, ()>,
        tempfile::TempDir,
        TokenDirGuard,
    ) {
        let lock = transport_probe::token_dir_lock().await;
        let dir = tempfile::tempdir().unwrap();
        let guard = TokenDirGuard::set(dir.path());
        transport_probe::reset();
        (lock, dir, guard)
    }

    fn runtime_app(url: &str, admin: Option<&str>, allow: bool) -> ClapApp {
        ClapApp {
            domain: Domain::Runtime {
                command: RuntimeFunctions::Info,
            },
            no_capability: false,
            executor_url: Some(url.to_string()),
            admin_credential: admin.map(str::to_string),
            allow_insecure_remote: allow,
        }
    }

    async fn connect_error(app: &ClapApp) -> String {
        match tokio::time::timeout(Duration::from_secs(3), get_ad4m_client(app, &app.domain)).await
        {
            Ok(Err(err)) => err.to_string(),
            Ok(Ok(_)) => panic!("connected to {app:?}"),
            Err(_) => panic!("blocked on a socket instead of returning"),
        }
    }

    fn assert_shared_check(allow: bool) {
        assert_eq!(
            transport_probe::calls(),
            1,
            "get_ad4m_client did not call the shared transport check"
        );
        assert_eq!(transport_probe::last_allow(), allow);
    }

    /// Explicit `--admin-credential` to a plaintext remote must not open a socket.
    #[tokio::test]
    async fn explicit_admin_remote_plaintext_is_refused() {
        let (_lock, _dir, _guard) = hold_token_dir().await;
        let app = runtime_app("http://203.0.113.55:9", Some(SECRET), false);
        let msg = connect_error(&app).await;
        assert!(
            msg.contains("plaintext"),
            "expected a plaintext refusal, got {msg}"
        );
        assert!(
            !msg.contains(SECRET),
            "the admin credential leaked into the error: {msg}"
        );
        assert_shared_check(false);
    }

    /// A token stored for that URL must not travel over plaintext either.
    #[tokio::test]
    async fn stored_token_remote_plaintext_is_refused() {
        let (_lock, dir, _guard) = hold_token_dir().await;
        let url = "http://203.0.113.55:9";
        ad4m_client::connect::TokenStore::new(dir.path())
            .set_token(url, crate::login::APP_NAME, SECRET)
            .unwrap();
        let app = runtime_app(url, None, false);
        let msg = connect_error(&app).await;
        assert!(
            msg.contains("plaintext"),
            "expected a plaintext refusal, got {msg}"
        );
        assert!(
            !msg.contains(SECRET),
            "the stored token leaked into the error: {msg}"
        );
        assert_shared_check(false);
    }

    /// Loopback plaintext is still allowed, and the credential may be dialed.
    #[tokio::test]
    async fn loopback_plaintext_is_allowed() {
        let (_lock, _dir, _guard) = hold_token_dir().await;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let port = listener.local_addr().unwrap().port();
        let seen = Arc::new(AtomicBool::new(false));
        let seen_task = Arc::clone(&seen);
        let secret = SECRET.to_string();
        tokio::spawn(async move {
            let accepted = tokio::time::timeout(Duration::from_secs(2), listener.accept()).await;
            let Ok(Ok((mut sock, _))) = accepted else {
                return;
            };
            let mut got = String::new();
            let deadline = tokio::time::Instant::now() + Duration::from_secs(1);
            while tokio::time::Instant::now() < deadline {
                let mut buf = [0u8; 1024];
                match tokio::time::timeout(Duration::from_millis(200), sock.read(&mut buf)).await {
                    Ok(Ok(0)) | Ok(Err(_)) | Err(_) => break,
                    Ok(Ok(n)) => {
                        got.push_str(&String::from_utf8_lossy(&buf[..n]));
                        if got.contains(&secret) {
                            seen_task.store(true, Ordering::SeqCst);
                            break;
                        }
                    }
                }
            }
        });

        let app = runtime_app(&format!("http://127.0.0.1:{port}"), Some(SECRET), false);
        let msg = connect_error(&app).await;
        assert!(!msg.contains("plaintext"), "loopback was refused: {msg}");
        assert!(
            !msg.contains(SECRET),
            "credential leaked into the error: {msg}"
        );
        assert!(
            seen.load(Ordering::SeqCst),
            "loopback did not dial the admin credential: {msg}"
        );
        assert_shared_check(false);
    }

    /// TLS to a non-loopback host is allowed. `0.0.0.0` is not loopback; the dial is refused at once.
    #[tokio::test]
    async fn tls_remote_is_allowed() {
        let (_lock, _dir, _guard) = hold_token_dir().await;
        let app = runtime_app("https://0.0.0.0:1", Some(SECRET), false);
        let msg = connect_error(&app).await;
        assert!(
            !msg.contains("plaintext"),
            "TLS was refused as plaintext: {msg}"
        );
        assert!(
            !msg.contains(SECRET),
            "credential leaked into the error: {msg}"
        );
        assert_shared_check(false);
    }

    /// The global flag lets an explicit admin credential dial plaintext.
    #[tokio::test]
    async fn allow_insecure_remote_lets_explicit_admin_dial_plaintext() {
        let (_lock, _dir, _guard) = hold_token_dir().await;
        let app = runtime_app("http://0.0.0.0:1", Some(SECRET), true);
        let msg = connect_error(&app).await;
        assert!(
            !msg.contains("plaintext"),
            "the flag did not permit plaintext: {msg}"
        );
        assert!(
            !msg.contains(SECRET),
            "credential leaked into the error: {msg}"
        );
        assert_shared_check(true);
    }

    #[test]
    fn allow_insecure_remote_is_a_global_flag() {
        for argv in [
            vec!["ad4m", "--allow-insecure-remote", "runtime", "info"],
            vec!["ad4m", "runtime", "--allow-insecure-remote", "info"],
            vec!["ad4m", "runtime", "info", "--allow-insecure-remote"],
            vec![
                "ad4m",
                "--allow-insecure-remote",
                "login",
                "http://203.0.113.55:9",
            ],
            vec![
                "ad4m",
                "login",
                "http://203.0.113.55:9",
                "--allow-insecure-remote",
            ],
        ] {
            let app = ClapApp::try_parse_from(&argv).unwrap_or_else(|err| {
                panic!("failed to parse {argv:?}: {err}");
            });
            assert!(app.allow_insecure_remote, "flag was not set for {argv:?}");
        }
        let app = ClapApp::try_parse_from(["ad4m", "runtime", "info"]).unwrap();
        assert!(!app.allow_insecure_remote);
    }
}
