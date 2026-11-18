//! The `ad4m-connect` flow from a terminal: find the executor, run the
//! capability handshake — or the email sign-in on a multi-user executor —
//! and keep the token. A second run connects without asking again.
//!
//! ```sh
//! cargo run -p ad4m-client --example connect -- \
//!     [--url http://127.0.0.1:12000] [--port 12000] [--email you@example.com] \
//!     [--allow-insecure-remote]
//! ```
//!
//! A plaintext (`http://`/`ws://`) executor off loopback is refused unless
//! `--allow-insecure-remote` is given.

use ad4m_client::connect::{
    default_token_dir, Ad4mConnect, AppInfo, AuthState, ConnectOptions, ConnectPrompts,
    ConnectStatus,
};
use anyhow::{anyhow, Result};
use std::future::Future;
use std::io::Write;

/// Terminal `ConnectPrompts`: prints the request/wait states and reads the
/// typed answers: the email from stdin, passwords and codes from the
/// terminal with echo off. An empty line cancels.
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

/// A secret (password, approval or email code): read from the terminal with
/// echo off. `rpassword` switches echo back on when the read ends, on an
/// error too; on ^C it raises SIGINT first, which `restore_terminal_on_interrupt`
/// handles.
fn ask_secret(prompt: &str) -> Result<String> {
    let answer = rpassword::prompt_password(prompt)?;
    match answer.trim() {
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
            ask_secret("code> ")
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
            println!("Type the code. Type resend to email a new code.");
            ask_secret("code> ")
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
            ask_secret("password> ")
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
            ConnectStatus::CodeRejected => {
                println!(
                    "That code did not take. Type another code, or type resend to email a new one."
                )
            }
            ConnectStatus::RequestExpired => {
                println!("The request expired; asking again…")
            }
            ConnectStatus::EmailCodeExpired => {
                println!("That code is no longer valid; sending a new one…")
            }
        }
    }
}

/// ^C during a hidden read: `rpassword` raises SIGINT before it puts the
/// terminal back, so with the default disposition the shell would inherit a
/// terminal without echo. Instead: save the terminal's settings now, and on
/// SIGINT put them back and exit like an interrupted program (130). Not for
/// uncatchable ends (SIGKILL).
#[cfg(unix)]
fn restore_terminal_on_interrupt() {
    use std::os::fd::AsRawFd;
    let saved = std::fs::File::open("/dev/tty").ok().and_then(|tty| {
        let mut term = std::mem::MaybeUninit::<libc::termios>::uninit();
        // SAFETY: a valid fd and a pointer to storage for one termios.
        (unsafe { libc::tcgetattr(tty.as_raw_fd(), term.as_mut_ptr()) } == 0)
            // SAFETY: tcgetattr succeeded, so it filled `term`.
            .then(|| (tty, unsafe { term.assume_init() }))
    });
    tokio::spawn(async move {
        if tokio::signal::ctrl_c().await.is_err() {
            return;
        }
        if let Some((tty, term)) = &saved {
            // SAFETY: the fd is still open (`tty` lives here); `term` came
            // from tcgetattr.
            unsafe { libc::tcsetattr(tty.as_raw_fd(), libc::TCSANOW, term) };
        }
        println!();
        std::process::exit(130);
    });
}

#[tokio::main]
async fn main() -> Result<()> {
    #[cfg(unix)]
    restore_terminal_on_interrupt();
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
            "--allow-insecure-remote" => opts = opts.allow_insecure_remote(true),
            _ => {
                return Err(anyhow!(
                    "unknown argument {arg}\nusage: connect [--url URL] [--port PORT] \
                     [--email EMAIL] [--allow-insecure-remote]"
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
