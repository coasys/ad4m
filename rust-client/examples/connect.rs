//! The `ad4m-connect` flow from a terminal: find the executor, run the
//! capability handshake — or the email sign-in on a multi-user executor —
//! and keep the token. A second run connects without asking again.
//!
//! ```sh
//! cargo run -p ad4m-client --example connect -- \
//!     [--url http://127.0.0.1:12000] [--port 12000] [--email you@example.com]
//! ```

use ad4m_client::connect::{
    default_token_dir, Ad4mConnect, AppInfo, AuthState, ConnectOptions, ConnectPrompts,
    ConnectStatus,
};
use anyhow::{anyhow, Result};
use std::future::Future;
use std::io::Write;

/// Terminal `ConnectPrompts`: prints the request/wait states and reads the
/// typed answers from stdin. An empty line cancels.
struct TermPrompts;

fn ask(prompt: &str) -> Result<String> {
    print!("{prompt}");
    std::io::stdout().flush()?;
    let mut line = String::new();
    std::io::stdin().read_line(&mut line)?;
    match line.trim() {
        "" => Err(anyhow!("cancelled")),
        answer => Ok(answer.to_string()),
    }
}

impl ConnectPrompts for TermPrompts {
    fn capability_code<'a>(
        &'a mut self,
        request_id: &'a str,
        app: &'a AppInfo,
    ) -> impl Future<Output = Result<String>> + Send + 'a {
        let request_id = request_id.to_string();
        let app_name = app.name.clone();
        async move {
            println!("\n\"{app_name}\" is asking for access (request {request_id}).");
            println!("Approve it in your AD4M UI, then type the 6-digit code.");
            ask("code> ")
        }
    }

    async fn email(&mut self) -> Result<String> {
        ask("email> ")
    }

    fn email_code<'a>(
        &'a mut self,
        email: &'a str,
    ) -> impl Future<Output = Result<String>> + Send + 'a {
        let email = email.to_string();
        async move {
            println!("A sign-in code was emailed to {email}.");
            ask("code> ")
        }
    }

    fn password<'a>(
        &'a mut self,
        email: &'a str,
        new_account: bool,
    ) -> impl Future<Output = Result<String>> + Send + 'a {
        let email = email.to_string();
        async move {
            if new_account {
                println!("No account for {email} yet — pick a password to create one.");
            } else {
                println!("This executor has no email configured; log in to {email} by password.");
            }
            ask("password> ")
        }
    }

    fn status(&mut self, status: ConnectStatus) {
        match status {
            ConnectStatus::Connecting => println!("Looking for an executor…"),
            ConnectStatus::WaitingForApproval { request_id } => {
                println!("Request {request_id} waits for approval…")
            }
            ConnectStatus::WaitingForEmailCode { email } => {
                println!("Waiting for the code emailed to {email}…")
            }
            ConnectStatus::CodeRejected => println!("That code did not take — try again."),
            ConnectStatus::RequestExpired => {
                println!("The request expired; asking again…")
            }
        }
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let mut opts = ConnectOptions::new(AppInfo::new(
        "ad4m-connect-example",
        "The Rust connect example",
        "http://localhost",
    ));
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--url" => opts.url = Some(args.next().ok_or_else(|| anyhow!("--url needs a value"))?),
            "--port" => {
                opts.port = Some(
                    args.next()
                        .ok_or_else(|| anyhow!("--port needs a value"))?
                        .parse()?,
                )
            }
            "--email" => {
                opts.email = Some(
                    args.next()
                        .ok_or_else(|| anyhow!("--email needs a value"))?,
                )
            }
            _ => {
                return Err(anyhow!(
                "unknown argument {arg}\nusage: connect [--url URL] [--port PORT] [--email EMAIL]"
            ))
            }
        }
    }

    let token_file = opts
        .token_dir
        .clone()
        .unwrap_or_else(default_token_dir)
        .join("tokens.json");
    println!("Tokens live in {}", token_file.display());

    let mut session = Ad4mConnect::new(opts)?;
    match session.connect(&mut TermPrompts).await? {
        AuthState::Locked => {
            println!("The executor's wallet is locked — unlock it and run again.");
            return Ok(());
        }
        AuthState::Unauthenticated => {
            println!("Could not authenticate.");
            return Ok(());
        }
        AuthState::Authenticated => {}
    }

    println!("Connected to {}", session.url().unwrap_or("?"));
    let status = session.client().unwrap().agent.status().await?;
    println!(
        "Agent: did={:?} initialized={} unlocked={}",
        status.did, status.is_initialized, status.is_unlocked
    );
    println!("Run again — the stored token connects without asking.");
    Ok(())
}
