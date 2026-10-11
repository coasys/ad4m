//! Who calls an MCP session: one [`Session`] per MCP session, as RPC holds one per connection.
//!
//! A token reaches an MCP session two ways. A login tool (`login_email`, `verify_email_code`,
//! `generate_jwt`) sets one, and a client that authenticates by `Authorization` header sends
//! one with every request. Either token opens a [`Session`] once, through [`Session::open`],
//! so the admin credential is the operator and a JWT is an app or a user, exactly as on RPC.
//! Every tool call checks the session again ([`Session::check`]): a token that is revoked,
//! expires, logs out or falls behind its user's session cutoff stops working on the next call.

use crate::agent::capabilities::ListenerReach;
use crate::agent::session::{AuthError, Session};
use crate::secret::Secret;
use std::sync::Arc;
use tokio::sync::RwLock;

/// The outcome of opening a session from a token.
pub(crate) type Opened = Result<Arc<Session>, AuthError>;

/// The session one MCP session's token opened, if a token arrived yet.
///
/// Only a token is remembered. A caller without one resolves again on every call
/// ([`Session::open`] with an empty token: the operator on a loopback listener of a node
/// without an admin credential, anonymous otherwise), so a header that arrives later still
/// takes effect.
#[derive(Clone, Default)]
pub struct Caller(Arc<RwLock<Option<Opened>>>);

impl Caller {
    /// A caller whose session starts from `token`. An empty token remembers nothing.
    pub(crate) fn from_token(token: &str, admin: &Option<Secret>, reach: ListenerReach) -> Self {
        let opened = (!token.is_empty()).then(|| Session::open(token, admin, reach).map(Arc::new));
        Caller(Arc::new(RwLock::new(opened)))
    }

    /// A caller that holds `opened` already, for tests that need a session no token makes
    /// (one that expires within the test, say).
    #[cfg(test)]
    pub(crate) fn holding(opened: Opened) -> Self {
        Caller(Arc::new(RwLock::new(Some(opened))))
    }

    /// Replace the session with the one `token` opens: what a login tool does. A token that
    /// opens no session is remembered as refused, so the next call says why.
    pub(crate) async fn sign_in(&self, token: &str, admin: &Option<Secret>, reach: ListenerReach) {
        *self.0.write().await = Some(Session::open(token, admin, reach).map(Arc::new));
    }

    /// The caller of one tool call.
    ///
    /// 1. A remembered session that still passes [`Session::check`] answers.
    /// 2. Otherwise a non-empty `header` token opens a session. One that opens is
    ///    remembered; one that does not is refused and not remembered.
    /// 3. Otherwise a remembered session that stopped working is refused.
    /// 4. Otherwise the caller has no token: [`Session::open`] with an empty one.
    pub(crate) async fn resolve(
        &self,
        header: Option<&str>,
        admin: &Option<Secret>,
        reach: ListenerReach,
    ) -> Opened {
        let refused = match self.0.read().await.clone() {
            Some(Ok(session)) => match session.check() {
                Ok(()) => return Ok(session),
                Err(e) => Some(e),
            },
            Some(Err(e)) => Some(e),
            None => None,
        };
        if let Some(token) = header.filter(|token| !token.is_empty()) {
            let session = Arc::new(Session::open(token, admin, reach)?);
            *self.0.write().await = Some(Ok(session.clone()));
            return Ok(session);
        }
        match refused {
            Some(e) => Err(e),
            None => Session::open("", admin, reach).map(Arc::new),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::capabilities::{AuthInfo, ALL_CAPABILITY, PERSPECTIVE_CREATE_CAPABILITY};
    use crate::agent::revoked::RevokedSet;
    use crate::agent::session::{Principal, TokenId};
    use crate::test_utils::{ensure_db, setup_agent, setup_wallet};

    const ADMIN: &str = "mcp-caller-admin";

    fn admin() -> Option<Secret> {
        Some(Secret::from(ADMIN))
    }

    fn app_token() -> String {
        setup_wallet();
        setup_agent();
        crate::agent::capabilities::token::generate_jwt(
            "an mcp app".to_string(),
            3600,
            AuthInfo {
                capabilities: Some(vec![PERSPECTIVE_CREATE_CAPABILITY.clone()]),
                ..AuthInfo::default()
            },
        )
        .unwrap()
    }

    fn token_id(session: &Session) -> TokenId {
        match &session.principal {
            Principal::App { token_id, .. } | Principal::User { token_id, .. } => token_id.clone(),
            other => panic!("expected a token session, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn the_admin_credential_by_header_is_the_operator_and_is_remembered() {
        let caller = Caller::default();
        let session = caller
            .resolve(Some(ADMIN), &admin(), ListenerReach::Network)
            .await
            .unwrap();
        assert!(session.is_operator());
        // The next call sends no header and is still the operator.
        let again = caller
            .resolve(None, &admin(), ListenerReach::Network)
            .await
            .unwrap();
        assert!(again.is_operator());
    }

    #[tokio::test]
    async fn a_remembered_session_outranks_a_later_header() {
        let caller = Caller::from_token(&app_token(), &admin(), ListenerReach::Network);
        let session = caller
            .resolve(Some(ADMIN), &admin(), ListenerReach::Network)
            .await
            .unwrap();
        assert!(matches!(session.principal, Principal::App { .. }));
    }

    #[tokio::test]
    async fn a_token_revoked_mid_session_is_refused_on_the_next_call() {
        let caller = Caller::from_token(&app_token(), &admin(), ListenerReach::Network);
        let session = caller
            .resolve(None, &admin(), ListenerReach::Network)
            .await
            .unwrap();
        RevokedSet::global().revoke(token_id(&session), u64::MAX);
        let err = caller
            .resolve(None, &admin(), ListenerReach::Network)
            .await
            .unwrap_err();
        assert_eq!(err, AuthError::Revoked);
        // It stays refused: no fallback to an anonymous or operator session.
        assert_eq!(
            caller
                .resolve(None, &None, ListenerReach::Loopback)
                .await
                .unwrap_err(),
            AuthError::Revoked
        );
    }

    #[tokio::test]
    async fn a_token_that_expires_mid_session_is_refused_on_the_next_call() {
        let principal = Principal::App {
            token_id: TokenId("mcp-expiring".into()),
            caps: vec![ALL_CAPABILITY.clone()],
        };
        let caller = Caller::holding(Ok(Arc::new(Session::for_test_expiring(principal, 1))));
        let err = caller
            .resolve(None, &admin(), ListenerReach::Network)
            .await
            .unwrap_err();
        assert_eq!(err, AuthError::Expired);
    }

    #[tokio::test]
    async fn a_fresh_header_token_replaces_a_dead_session() {
        let principal = Principal::App {
            token_id: TokenId("mcp-dead".into()),
            caps: vec![],
        };
        let caller = Caller::holding(Ok(Arc::new(Session::for_test_expiring(principal, 1))));
        let session = caller
            .resolve(Some(&app_token()), &admin(), ListenerReach::Network)
            .await
            .unwrap();
        assert!(session.may(&PERSPECTIVE_CREATE_CAPABILITY));
    }

    #[tokio::test]
    async fn a_bad_header_is_refused_and_not_remembered() {
        setup_wallet();
        ensure_db();
        let caller = Caller::default();
        let err = caller
            .resolve(Some("wrong-credential"), &admin(), ListenerReach::Network)
            .await
            .unwrap_err();
        assert!(matches!(err, AuthError::Invalid(_)), "{err:?}");
        let next = caller
            .resolve(None, &admin(), ListenerReach::Network)
            .await
            .unwrap();
        assert!(!next.is_authenticated());
    }

    #[tokio::test]
    async fn no_token_follows_the_listener_reach() {
        ensure_db();
        let caller = Caller::default();
        let local = caller.resolve(None, &None, ListenerReach::Loopback).await;
        assert!(local.unwrap().is_operator());
        let remote = caller.resolve(None, &None, ListenerReach::Network).await;
        assert!(!remote.unwrap().is_authenticated(), "#1059");
        let credentialed = caller
            .resolve(None, &admin(), ListenerReach::Loopback)
            .await;
        assert!(!credentialed.unwrap().is_authenticated());
    }

    #[tokio::test]
    async fn a_login_token_that_opens_no_session_is_remembered_as_refused() {
        setup_wallet();
        let caller = Caller::default();
        caller
            .sign_in("not-a-jwt", &admin(), ListenerReach::Network)
            .await;
        let err = caller
            .resolve(None, &admin(), ListenerReach::Network)
            .await
            .unwrap_err();
        assert!(matches!(err, AuthError::Invalid(_)), "{err:?}");
    }
}
