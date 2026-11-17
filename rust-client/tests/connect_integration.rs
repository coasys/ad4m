//! `connect` against a real `ad4m-executor`: the capability handshake end to
//! end (approval through the operator socket, as the launcher would), token
//! reuse, a mid-session refusal, and multi-user email sign-in with the
//! executor's email test mode.
//!
//! Same harness as `integration.rs`: skips without a binary unless
//! `AD4M_REQUIRE_EXECUTOR=1`.

#![cfg(not(target_arch = "wasm32"))]

mod common;

use ad4m_client::connect::{
    Ad4mConnect, AppInfo, AuthState, ConnectOptions, ConnectPrompts, ConnectStatus,
};
use ad4m_client::ws_rpc::WsRpcClient;
use anyhow::{anyhow, Result};
use serde_json::{json, Value};
use std::collections::VecDeque;
use std::future::Future;
use std::sync::Arc;
use std::time::Duration;

/// The launcher's half of the flow: approves capability requests and reads
/// emailed codes through the operator's socket — the "user approves and
/// types the code" step, scripted.
struct ApprovingPrompts {
    admin: Arc<WsRpcClient>,
    /// The request ids this prompt approved.
    requests: Vec<String>,
    /// Email codes fetched from `user.emailTest`.
    email_codes: usize,
    /// Passwords to answer, in order.
    passwords: VecDeque<String>,
    statuses: Vec<ConnectStatus>,
}

impl ApprovingPrompts {
    fn new(admin: Arc<WsRpcClient>) -> Self {
        Self {
            admin,
            requests: vec![],
            email_codes: 0,
            passwords: VecDeque::new(),
            statuses: vec![],
        }
    }
}

impl ConnectPrompts for ApprovingPrompts {
    /// Approving in the UI mints the code — `agent.permitCapability`, as the
    /// launcher's consent screen calls it.
    fn capability_code<'a>(
        &'a mut self,
        request_id: &'a str,
        app: &'a AppInfo,
    ) -> impl Future<Output = Result<String>> + Send + 'a {
        self.requests.push(request_id.to_string());
        let app_name = app.name.clone();
        let app_desc = app.description.clone();
        let capabilities = app.capabilities.clone();
        async move {
            let auth = serde_json::to_string(&json!({
                "requestId": request_id,
                "auth": {
                    "appName": app_name,
                    "appDesc": app_desc,
                    "capabilities": capabilities,
                },
            }))?;
            self.admin
                .call::<String>("agent.permitCapability", json!({ "auth": auth }))
                .await
        }
    }

    /// The code the executor's test-mode email "sent" — read back through
    /// `user.emailTest` (an operator call).
    fn email_code<'a>(
        &'a mut self,
        email: &'a str,
    ) -> impl Future<Output = Result<String>> + Send + 'a {
        self.email_codes += 1;
        let email = email.to_string();
        async move {
            // The code lands when `user.requestVerification` returns; a small
            // poll covers the async send.
            let deadline = std::time::Instant::now() + Duration::from_secs(10);
            loop {
                let code: Value = self
                    .admin
                    .call(
                        "user.emailTest",
                        json!({ "action": "get-code", "email": email }),
                    )
                    .await?;
                if let Some(code) = code.as_str() {
                    return Ok(code.to_string());
                }
                assert!(
                    std::time::Instant::now() < deadline,
                    "no test email code arrived for {email}"
                );
                tokio::time::sleep(Duration::from_millis(100)).await;
            }
        }
    }

    fn password<'a>(
        &'a mut self,
        email: &'a str,
        new_account: bool,
    ) -> impl Future<Output = Result<String>> + Send + 'a {
        let _ = (email, new_account);
        let password = self
            .passwords
            .pop_front()
            .ok_or_else(|| anyhow!("no scripted password"));
        async move { password }
    }

    fn status(&mut self, status: ConnectStatus) {
        eprintln!("[connect-it] {status:?}");
        self.statuses.push(status);
    }
}

fn options(url: &str, token_dir: &std::path::Path, app: &str) -> ConnectOptions {
    let mut opts = ConnectOptions::new(AppInfo::new(app, "integration test", "http://localhost"));
    opts.url = Some(url.to_string());
    opts.token_dir = Some(token_dir.to_path_buf());
    opts.use_stored_url = false;
    opts
}

/// Single-user: `agent.requestCapability` → approval → `agent.generateJwt`
/// → a working session — then the stored token connects again with no
/// approval, and a revoked grant throws the session back to the handshake.
#[tokio::test]
async fn single_user_handshake_token_reuse_and_reauth() {
    let Some(executor) = common::start_executor_with_admin("a5-it-admin") else {
        return;
    };
    let credential = executor.admin_credential.clone().unwrap();
    let admin = common::connect(&executor.url, &credential).await;
    admin
        .agent
        .generate("passphrase".to_string())
        .await
        .unwrap();

    let token_dir = tempfile::tempdir().unwrap();
    let mut session = Ad4mConnect::new(options(&executor.url, token_dir.path(), "a5-it")).unwrap();
    let mut prompts = ApprovingPrompts::new(admin.ws().clone());

    assert_eq!(
        session.connect(&mut prompts).await.unwrap(),
        AuthState::Authenticated
    );
    assert_eq!(prompts.requests.len(), 1, "exactly one capability request");
    let request_id = prompts.requests[0].clone();

    // A working client: the wildcard grant answers `agent.status`.
    let status = session.client().unwrap().agent.status().await.unwrap();
    assert!(status.is_unlocked, "{status:?}");

    // Second run, same token dir: the stored token connects with no approval.
    let mut prompts2 = ApprovingPrompts::new(admin.ws().clone());
    let mut session2 = Ad4mConnect::new(options(&executor.url, token_dir.path(), "a5-it")).unwrap();
    assert_eq!(
        session2.connect(&mut prompts2).await.unwrap(),
        AuthState::Authenticated
    );
    assert!(
        prompts2.requests.is_empty(),
        "a stored token must not ask again"
    );
    // Close session2's socket only: `disconnect` would delete the shared
    // url|app token and pre-satisfy the deletion check below (F7).
    session2.rpc().unwrap().close().await;
    drop(session2);

    // Revoking the grant closes the session socket with 1008: the session
    // flips to Unauthenticated and drops the stored token, and a fresh
    // connect re-runs the handshake.
    let mut watcher = session.watch_state();
    let stored = session.store().token(&executor.url, "a5-it").unwrap();
    assert!(
        stored.is_some(),
        "the token must still be stored right before the revocation"
    );
    admin
        .ws()
        .call::<Value>("agent.removeApp", json!({ "id": request_id }))
        .await
        .unwrap();
    tokio::time::timeout(Duration::from_secs(15), watcher.changed())
        .await
        .expect("the session never noticed the revoked grant")
        .unwrap();
    assert_eq!(session.state(), AuthState::Unauthenticated);
    assert_eq!(
        session.store().token(&executor.url, "a5-it").unwrap(),
        None,
        "the refused token must be dropped"
    );

    let mut prompts3 = ApprovingPrompts::new(admin.ws().clone());
    assert_eq!(
        session.connect(&mut prompts3).await.unwrap(),
        AuthState::Authenticated
    );
    assert_eq!(prompts3.requests.len(), 1, "re-auth asks for a new grant");

    session.disconnect().await.unwrap();
    admin.ws().close().await;
    drop(executor);
}

/// Multi-user: `user.requestVerification` routes a new account through
/// `user.create` + `user.login` (password), and an existing one through the
/// emailed login code (`user.verifyEmail`) fetched by `user.emailTest`.
#[tokio::test]
async fn multi_user_signup_and_email_code_login() {
    let Some(executor) = common::start_executor_with_admin("a5-mu-admin") else {
        return;
    };
    let credential = executor.admin_credential.clone().unwrap();
    let admin = common::connect(&executor.url, &credential).await;
    admin
        .agent
        .generate("passphrase".to_string())
        .await
        .unwrap();
    admin
        .ws()
        .call::<bool>("user.setMultiUserEnabled", json!({ "enabled": true }))
        .await
        .unwrap();
    admin
        .ws()
        .call::<bool>("user.emailTest", json!({ "action": "enable" }))
        .await
        .unwrap();

    let email = "ada@example.com";

    // Run 1 — a new account: requestVerification answers
    // requiresPassword+!isExistingUser → password prompt → user.create +
    // user.login.
    let dir1 = tempfile::tempdir().unwrap();
    let mut opts = options(&executor.url, dir1.path(), "a5-mu");
    opts.email = Some(email.to_string());
    let mut session = Ad4mConnect::new(opts).unwrap();
    let mut prompts = ApprovingPrompts::new(admin.ws().clone());
    prompts.passwords.push_back("correct horse".to_string());
    assert_eq!(
        session.connect(&mut prompts).await.unwrap(),
        AuthState::Authenticated
    );

    // The session is the user's managed agent: a DID, unlocked.
    let status = session.client().unwrap().agent.status().await.unwrap();
    assert!(status.is_unlocked, "{status:?}");
    assert!(status.did.is_some(), "{status:?}");
    session.disconnect().await.unwrap();

    // Run 2 — the existing account: the emailed login code path.
    let dir2 = tempfile::tempdir().unwrap();
    let mut opts2 = options(&executor.url, dir2.path(), "a5-mu");
    opts2.email = Some(email.to_string());
    let mut session2 = Ad4mConnect::new(opts2).unwrap();
    let mut prompts2 = ApprovingPrompts::new(admin.ws().clone());
    assert_eq!(
        session2.connect(&mut prompts2).await.unwrap(),
        AuthState::Authenticated
    );
    assert_eq!(prompts2.email_codes, 1, "the email code prompt ran once");

    // Run 3 — same token dir: the stored session token, no prompts.
    let mut session3 = Ad4mConnect::new(options(&executor.url, dir2.path(), "a5-mu")).unwrap();
    let mut prompts3 = ApprovingPrompts::new(admin.ws().clone());
    assert_eq!(
        session3.connect(&mut prompts3).await.unwrap(),
        AuthState::Authenticated
    );
    assert_eq!(prompts3.email_codes, 0, "a stored token must not ask again");

    session2.disconnect().await.unwrap();
    session3.disconnect().await.unwrap();
    admin.ws().close().await;
    drop(executor);
}
