//! `ad4m login <url>`: the rust-client `ad4m-connect` flow.
//!
//! A local executor takes the admin credential, or a capability request when
//! none is given. A multi-user executor takes an email login. The token is
//! stored per executor URL under the package version (`#1610`), not in the
//! single `~/.ad4m-cli/cap_token` file.

use std::io::{IsTerminal, Write};
use std::path::PathBuf;

use ad4m_client::connect::{
    normalize_base_url, Ad4mConnect, AppInfo, AuthState, ConnectOptions, ConnectPrompts,
    ConnectStatus, TokenStore,
};
use ad4m_client::Ad4mClient;
use anyhow::{anyhow, bail, Context, Result};

/// The app name the capability request shows and the token store keys on.
pub const APP_NAME: &str = "AD4M cli";
const APP_DESC: &str = "Command line administration tool for AD4M";
const APP_URL: &str = "org.perspect3vism.ad4m.cli";

/// `AD4M_TOKEN_DIR` when set, otherwise the rust client's connect directory.
pub fn token_dir() -> PathBuf {
    match std::env::var_os("AD4M_TOKEN_DIR") {
        Some(dir) if !dir.is_empty() => PathBuf::from(dir),
        _ => ad4m_client::connect::default_token_dir(),
    }
}

/// The token stored for `executor_url`, after the version rule runs.
/// `None` when this version has none and no older version of the same minor
/// line stored one for this executor.
pub fn stored_token(executor_url: &str) -> Result<Option<String>> {
    let url = normalize_base_url(executor_url);
    TokenStore::new(token_dir()).token(&url, APP_NAME)
}

/// Whether `url` is a loopback executor (`localhost`, 127.0.0.0/8, `::1`).
pub fn is_loopback(url: &str) -> bool {
    let url = normalize_base_url(url);
    let Ok(parsed) = url::Url::parse(&url) else {
        return false;
    };
    match parsed.host() {
        Some(url::Host::Domain(host)) => host.eq_ignore_ascii_case("localhost"),
        Some(url::Host::Ipv4(ip)) => ip.is_loopback(),
        Some(url::Host::Ipv6(ip)) => ip.is_loopback(),
        None => false,
    }
}

/// Log in to `url` and store the token for that executor.
///
/// `admin_credential` is checked and stored. It does not start the capability
/// handshake. Without it, [`Ad4mConnect`] runs the capability handshake on a
/// single-user executor and the email login on a multi-user one.
pub async fn login(
    url: &str,
    email: Option<String>,
    admin_credential: Option<&str>,
    allow_insecure_remote: bool,
) -> Result<()> {
    let url = normalize_base_url(url);
    if url.is_empty() {
        bail!("login needs an executor URL");
    }
    if let Some(credential) = admin_credential {
        if credential.trim().is_empty() {
            bail!("the admin credential is empty");
        }
        return login_with_admin(&url, credential).await;
    }

    let mut opts = ConnectOptions::new(AppInfo::new(APP_NAME, APP_DESC, APP_URL))
        .allow_insecure_remote(allow_insecure_remote);
    opts.url = Some(url.clone());
    opts.email = email;
    opts.token_dir = Some(token_dir());
    // The URL was given on the command line. Do not replace it with a
    // remembered one.
    opts.use_stored_url = false;

    let mut session = Ad4mConnect::new(opts)?;
    let state = session.connect(&mut TermPrompts).await?;
    match state {
        AuthState::Authenticated => {
            println!("Logged in to {url}");
            println!("Token file: {}", session.store().path().display());
            Ok(())
        }
        AuthState::Locked => {
            bail!("the executor wallet is locked. Unlock it and run `ad4m login {url}` again")
        }
        AuthState::Unauthenticated => {
            bail!("could not log in to {url}")
        }
    }
}

async fn login_with_admin(url: &str, credential: &str) -> Result<()> {
    let client = Ad4mClient::connect(url.to_string(), credential.to_string())
        .await
        .with_context(|| format!("cannot reach {url}"))?;
    client
        .agent
        .status()
        .await
        .with_context(|| format!("the admin credential was refused by {url}"))?;
    TokenStore::new(token_dir()).set_token(url, APP_NAME, credential)?;
    println!("Logged in to {url}");
    println!(
        "Token file: {}",
        TokenStore::new(token_dir()).path().display()
    );
    Ok(())
}

fn say(line: &str) {
    println!("{line}");
    let _ = std::io::stdout().flush();
}

/// A line from the terminal, or from stdin when stdin is not a terminal
/// (a test pipes the answer). An empty line cancels.
fn ask(prompt: &str, secret: bool) -> Result<String> {
    let answer = if std::io::stdin().is_terminal() {
        if secret {
            crate::util::readline_masked(prompt)?
        } else {
            let mut editor = rustyline::Editor::<()>::new()?;
            editor.readline(prompt)?
        }
    } else {
        print!("{prompt}");
        std::io::stdout().flush()?;
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;
        line
    };
    match answer.trim() {
        "" => Err(anyhow!("cancelled")),
        answer => Ok(answer.to_string()),
    }
}

struct TermPrompts;

impl ConnectPrompts for TermPrompts {
    fn capability_code<'a>(
        &'a mut self,
        request_id: &'a str,
        app: &'a AppInfo,
    ) -> impl std::future::Future<Output = Result<String>> + Send + 'a {
        let request_id = request_id.to_string();
        let app_name = app.name.clone();
        async move {
            say(&format!("\"{app_name}\" is asking for access."));
            // The test reads this line. Flush so a pipe sees it before the read.
            say(&format!("request_id: {request_id}"));
            say("Approve the request in the AD4M UI, then type the 6-digit code.");
            ask("code> ", true)
        }
    }

    async fn email(&mut self) -> Result<String> {
        ask("email> ", false)
    }

    fn email_code<'a>(
        &'a mut self,
        email: &'a str,
    ) -> impl std::future::Future<Output = Result<String>> + Send + 'a {
        let email = email.to_string();
        async move {
            say(&format!("A sign-in code was emailed to {email}."));
            ask("code> ", true)
        }
    }

    fn password<'a>(
        &'a mut self,
        email: &'a str,
        new_account: bool,
    ) -> impl std::future::Future<Output = Result<String>> + Send + 'a {
        let email = email.to_string();
        async move {
            if new_account {
                say(&format!(
                    "No account for {email} yet. Type a password to create one."
                ));
            } else {
                say(&format!(
                    "This executor has no email configured. Log in to {email} with a password."
                ));
            }
            ask("password> ", true)
        }
    }

    fn status(&mut self, status: ConnectStatus) {
        match status {
            ConnectStatus::Connecting => say("Looking for an executor..."),
            ConnectStatus::WaitingForApproval { request_id } => {
                say(&format!("Request {request_id} waits for approval..."));
            }
            ConnectStatus::WaitingForEmailCode { email } => {
                say(&format!("Waiting for the code emailed to {email}..."));
            }
            ConnectStatus::CodeRejected => say("That code did not take. Try again."),
            ConnectStatus::RequestExpired => say("The request expired. Asking again..."),
            ConnectStatus::EmailCodeExpired => {
                say("That code is no longer valid. Sending a new one...");
            }
        }
    }
}
