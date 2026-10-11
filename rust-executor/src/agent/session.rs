//! Who is calling: one [`Session`] per connection.
//!
//! [`Session::open`] is the one place that turns a token into an identity. It reproduces
//! the rules of `capabilities_on`, `is_admin_credential_token_on`, `user_email_from_token`
//! and `resolve_user_session`, whose callers move onto it later. Nothing calls it yet.

use super::capabilities::{
    anonymous_capabilities, capability_matches, check_token_revoked, decode_jwt,
    is_admin_credential_token_on, Capability, ListenerReach, AGENT_AUTH_CAPABILITY, ALL_CAPABILITY,
    TOKEN_EXPIRY_LEEWAY_SECONDS,
};
use super::AgentService;

/// The id of a JWT: its `nonce` claim, a fresh v4 UUID per token. Revocation by id needs no
/// change to the token format.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TokenId(pub String);

/// The kind of caller behind a connection.
#[derive(Clone, Debug)]
pub enum Principal {
    /// The admin credential, or no token on a loopback listener of a node without one (#1059).
    Operator,
    /// A token without a user: an approved app. It acts as the node's agent, limited to `caps`.
    App {
        token_id: TokenId,
        caps: Vec<Capability>,
    },
    /// A token that names a managed user. It acts as `did`, limited to `caps`, whatever the
    /// multi-user setting says: a user token never acts as the node.
    User {
        token_id: TokenId,
        email: String,
        did: String,
        caps: Vec<Capability>,
    },
    /// No token. `caps` hold what an anonymous caller may do: request a capability, and sign
    /// up or log in when multi-user mode is on.
    Anonymous { caps: Vec<Capability> },
}

/// Why a token opens no session.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AuthError {
    /// The operator revoked the token.
    Revoked,
    /// The token's expiry has passed.
    Expired,
    /// The token does not decode: a bad signature, garbage, or a locked wallet.
    Invalid(String),
    /// The token names a user whose DID does not load.
    NoIdentity(String),
}

impl std::fmt::Display for AuthError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AuthError::Revoked => write!(f, "Unauthorized access"),
            AuthError::Expired => write!(f, "Unauthorized access: the token has expired"),
            AuthError::Invalid(e) => write!(f, "{e}"),
            AuthError::NoIdentity(e) => write!(
                f,
                "Could not load the identity of this user session; reconnect: {e}"
            ),
        }
    }
}

/// A connection's identity, checked again on every request and every delivered event.
#[derive(Clone, Debug)]
pub struct Session {
    pub principal: Principal,
    pub reach: ListenerReach,
    /// `None` for the operator and anonymous callers: they have no token that expires.
    expires_at: Option<u64>,
    /// `apps_map` revokes by the raw token, so `check` needs it.
    token: String,
}

impl Session {
    /// The identity of a caller that presents `token` on a listener with this reach.
    pub fn open(
        token: &str,
        admin_credential: &Option<String>,
        reach: ListenerReach,
    ) -> Result<Session, AuthError> {
        let session = |principal, expires_at| Session {
            principal,
            reach,
            expires_at,
            token: token.to_string(),
        };

        if is_admin_credential_token_on(token, admin_credential, reach) {
            return Ok(session(Principal::Operator, None));
        }
        if token.is_empty() {
            let caps = anonymous_capabilities();
            return Ok(session(Principal::Anonymous { caps }, None));
        }

        check_token_revoked(token).map_err(|_| AuthError::Revoked)?;
        let claims = decode_jwt(token.to_string()).map_err(|e| {
            if e.downcast_ref::<jsonwebtoken::errors::Error>()
                .is_some_and(|e| *e.kind() == jsonwebtoken::errors::ErrorKind::ExpiredSignature)
            {
                AuthError::Expired
            } else {
                AuthError::Invalid(e.to_string())
            }
        })?;

        let token_id = TokenId(claims.nonce().to_string());
        let expires_at = Some(claims.expires_at());
        let caps = claims
            .capabilities
            .capabilities
            .unwrap_or_else(|| vec![AGENT_AUTH_CAPABILITY.clone()]);
        let principal = match claims.sub {
            Some(email) => {
                let did = AgentService::get_user_did_by_email(&email)
                    .map_err(|e| AuthError::NoIdentity(e.to_string()))?;
                Principal::User {
                    token_id,
                    email,
                    did,
                    caps,
                }
            }
            None => Principal::App { token_id, caps },
        };
        Ok(session(principal, expires_at))
    }

    /// Fails once the token is revoked or past its expiry. Needs no wallet: the signature was
    /// checked in [`Session::open`].
    pub fn check(&self) -> Result<(), AuthError> {
        if matches!(
            self.principal,
            Principal::Operator | Principal::Anonymous { .. }
        ) {
            return Ok(());
        }
        check_token_revoked(&self.token).map_err(|_| AuthError::Revoked)?;
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(u64::MAX);
        match self.expires_at {
            Some(expires_at) if now > expires_at + TOKEN_EXPIRY_LEEWAY_SECONDS => {
                Err(AuthError::Expired)
            }
            _ => Ok(()),
        }
    }

    /// Whether the session's capabilities cover `expected`. The operator may do anything.
    pub fn may(&self, expected: &Capability) -> bool {
        match &self.principal {
            Principal::Operator => true,
            Principal::App { caps, .. }
            | Principal::User { caps, .. }
            | Principal::Anonymous { caps } => capability_matches(caps, expected),
        }
    }

    /// The one test for "is this the node's owner?".
    pub fn is_operator(&self) -> bool {
        matches!(self.principal, Principal::Operator)
    }

    /// The capabilities that checks run against: every capability for the operator.
    pub fn capabilities(&self) -> Vec<Capability> {
        match &self.principal {
            Principal::Operator => vec![ALL_CAPABILITY.clone()],
            Principal::App { caps, .. }
            | Principal::User { caps, .. }
            | Principal::Anonymous { caps } => caps.clone(),
        }
    }

    /// A user session's email and DID.
    pub fn user(&self) -> Option<(&str, &str)> {
        match &self.principal {
            Principal::User { email, did, .. } => Some((email, did)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod resolution_tests {
    //! One test per way a token resolves.
    use super::*;
    use crate::agent::capabilities::{
        apps_map, AuthInfo, AuthInfoExtended, PERSPECTIVE_CREATE_CAPABILITY,
        RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY,
    };
    use crate::test_utils::{setup_agent, setup_wallet, MultiUserMode};

    const ADMIN: &str = "the-admin-credential";

    fn admin() -> Option<String> {
        Some(ADMIN.to_string())
    }

    fn app_token(capabilities: Option<Vec<Capability>>) -> String {
        setup_wallet();
        setup_agent();
        crate::agent::capabilities::token::generate_jwt(
            "an app".to_string(),
            3600,
            AuthInfo {
                capabilities,
                ..AuthInfo::default()
            },
        )
        .unwrap()
    }

    /// The anonymous set reads the multi-user setting. Initialises the database only when no
    /// test has, so it does not wipe one another test is using.
    fn ensure_db() {
        use crate::db::Ad4mDb;
        if Ad4mDb::global_instance().lock().unwrap().is_none() {
            Ad4mDb::init_global_instance(":memory:").unwrap();
        }
    }

    #[test]
    fn the_admin_credential_is_the_operator_on_any_listener() {
        for reach in [ListenerReach::Loopback, ListenerReach::Network] {
            let session = Session::open(ADMIN, &admin(), reach).unwrap();
            assert!(session.is_operator());
            assert!(session.may(&ALL_CAPABILITY));
            assert!(session.check().is_ok());
        }
    }

    #[test]
    fn no_token_and_no_credential_is_the_operator_only_on_loopback() {
        let local = Session::open("", &None, ListenerReach::Loopback).unwrap();
        assert!(local.is_operator());

        ensure_db();
        let remote = Session::open("", &None, ListenerReach::Network).unwrap();
        assert!(matches!(remote.principal, Principal::Anonymous { .. }));
        assert!(
            !remote.may(&ALL_CAPABILITY),
            "#1059: no operator over the network"
        );
    }

    #[test]
    fn no_token_on_a_node_with_a_credential_is_anonymous() {
        ensure_db();
        for reach in [ListenerReach::Loopback, ListenerReach::Network] {
            let session = Session::open("", &admin(), reach).unwrap();
            assert!(matches!(session.principal, Principal::Anonymous { .. }));
            assert!(
                session.may(&AGENT_AUTH_CAPABILITY),
                "may request a capability"
            );
            assert!(!session.may(&PERSPECTIVE_CREATE_CAPABILITY));
        }
    }

    #[test]
    fn anonymous_callers_may_sign_up_only_in_multi_user_mode() {
        ensure_db();
        let single = Session::open("", &admin(), ListenerReach::Network).unwrap();
        assert!(!single.may(&RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY));

        let _multi_user = MultiUserMode::on();
        let multi = Session::open("", &admin(), ListenerReach::Network).unwrap();
        assert!(multi.may(&RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY));
    }

    #[test]
    fn a_token_without_a_user_is_an_app_limited_to_its_capabilities() {
        let token = app_token(Some(vec![PERSPECTIVE_CREATE_CAPABILITY.clone()]));
        let session = Session::open(&token, &admin(), ListenerReach::Network).unwrap();
        assert!(matches!(session.principal, Principal::App { .. }));
        assert!(session.may(&PERSPECTIVE_CREATE_CAPABILITY));
        assert!(!session.may(&ALL_CAPABILITY));
        assert!(!session.is_operator());
        assert!(session.check().is_ok());
    }

    #[test]
    fn a_token_without_capabilities_may_only_request_one() {
        let session = Session::open(&app_token(None), &admin(), ListenerReach::Network).unwrap();
        assert!(session.may(&AGENT_AUTH_CAPABILITY));
        assert!(!session.may(&PERSPECTIVE_CREATE_CAPABILITY));
    }

    #[test]
    fn a_user_token_is_the_user_with_their_did_whatever_the_multi_user_setting() {
        setup_wallet();
        setup_agent();
        let email = "session.user@example.org";
        AgentService::ensure_user_key_exists(email).unwrap();
        let token = crate::user_management::generate_user_jwt(email, "test").unwrap();
        let expected_did = AgentService::get_user_did_by_email(email).unwrap();

        let open = || Session::open(&token, &admin(), ListenerReach::Network).unwrap();
        let single = open();
        let multi_user = MultiUserMode::on();
        let multi = open();
        drop(multi_user);

        for session in [single, multi] {
            match &session.principal {
                Principal::User { email: e, did, .. } => {
                    assert_eq!(e, email);
                    assert_eq!(did, &expected_did);
                }
                other => panic!("expected a user, got {other:?}"),
            }
            assert!(!session.is_operator());
        }
    }

    #[test]
    fn a_user_whose_did_does_not_load_opens_no_session() {
        setup_wallet();
        setup_agent();
        let token =
            crate::user_management::generate_user_jwt("session.nokey@example.org", "test").unwrap();
        let err = Session::open(&token, &admin(), ListenerReach::Network).unwrap_err();
        assert!(matches!(err, AuthError::NoIdentity(_)), "{err:?}");
    }

    #[test]
    fn an_expired_token_opens_no_session() {
        setup_wallet();
        setup_agent();
        let token = crate::test_utils::expired_user_token("session.expired@example.org");
        let err = Session::open(&token, &admin(), ListenerReach::Network).unwrap_err();
        assert_eq!(err, AuthError::Expired);
    }

    #[test]
    fn garbage_opens_no_session() {
        setup_wallet();
        setup_agent();
        let err = Session::open("not-a-jwt", &admin(), ListenerReach::Network).unwrap_err();
        assert!(matches!(err, AuthError::Invalid(_)), "{err:?}");
    }

    #[test]
    fn a_locked_wallet_opens_no_session_and_says_so() {
        let token = app_token(None);
        let backend = crate::wallet::wallet_backend();
        backend.lock("session-locked-test");
        let result = Session::open(&token, &admin(), ListenerReach::Network);
        // Unlock before asserting, so a failure cannot leave the shared wallet locked.
        backend
            .unlock("session-locked-test")
            .expect("restore the shared wallet");

        match result {
            Err(AuthError::Invalid(e)) => assert!(e.contains("Executor is locked"), "{e}"),
            other => panic!("expected Invalid, got {other:?}"),
        }
    }

    #[test]
    fn a_revoked_token_opens_no_session_and_ends_an_open_one() {
        crate::test_utils::use_test_apps_file();
        let token = app_token(None);
        let request_id = format!("session-{}", uuid::Uuid::new_v4());
        let app = AuthInfoExtended {
            request_id: request_id.clone(),
            auth: AuthInfo::default(),
        };
        apps_map::insert_app(request_id.clone(), app, token.clone()).unwrap();
        let open = Session::open(&token, &admin(), ListenerReach::Network).unwrap();

        apps_map::revoke_app(&request_id).unwrap();
        assert_eq!(open.check().unwrap_err(), AuthError::Revoked);
        assert_eq!(
            Session::open(&token, &admin(), ListenerReach::Network).unwrap_err(),
            AuthError::Revoked
        );
    }

    #[test]
    fn a_token_past_its_expiry_fails_the_check_after_the_leeway() {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        let session = |expires_at| Session {
            principal: Principal::App {
                token_id: TokenId("t".into()),
                caps: vec![],
            },
            reach: ListenerReach::Network,
            expires_at: Some(expires_at),
            token: "an-unrevoked-token".into(),
        };
        assert!(session(now - 10).check().is_ok(), "inside the leeway");
        assert_eq!(session(now - 3600).check().unwrap_err(), AuthError::Expired);
    }
}
