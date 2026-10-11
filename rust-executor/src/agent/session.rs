//! Who is calling: one [`Session`] per connection.
//!
//! [`Session::open`] is the one place that turns a token into an identity:
//!
//! - The admin credential is the operator. A node without one treats no token on a loopback
//!   listener as the operator (#1059).
//! - Any other caller without a token is anonymous.
//! - A token that is revoked, past its expiry or does not decode opens no session.
//! - A token that names a user (`sub`) is that user, and the user's DID must load. Any other
//!   token is an app. Both are limited to the token's capabilities.
//!
//! A token that opens no session is refused where it arrives. [`Session::check`] runs again
//! on every request and every delivered event. MCP still resolves its tokens itself
//! (`capabilities_from_token`, `AgentContext::from_auth_token`).

use super::capabilities::{
    anonymous_capabilities, capability_matches, decode_jwt, is_admin_credential_token_on,
    Capability, ListenerReach, AGENT_AUTH_CAPABILITY, AGENT_PERMIT_CAPABILITY, ALL_CAPABILITY,
    TOKEN_EXPIRY_LEEWAY_SECONDS,
};
use super::revoked::{RevokedSet, SessionCutoffs};
use super::AgentService;
use crate::events::{Audience, Event};
use crate::secret::Secret;
use std::sync::OnceLock;

/// The id of a JWT: its `nonce` claim, a fresh v4 UUID per token. Revocation by id needs no
/// change to the token format.
#[derive(Clone, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
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
    /// The token does not decode: a bad signature or garbage.
    Invalid(String),
    /// The token names a user whose DID does not load.
    NoIdentity(String),
    /// The node's wallet is locked, so no token can be checked yet. A state of the node, not
    /// of the token: the token may be good, and the caller should try again after the unlock.
    Locked,
}

impl AuthError {
    /// The close code a socket gets when its token opens no session: 1008 ("policy
    /// violation", clients log in again) for a token that does not hold, 1013 ("try again
    /// later", clients keep the token and reconnect) for a locked wallet.
    pub fn close_code(&self) -> u16 {
        match self {
            AuthError::Locked => 1013,
            _ => 1008,
        }
    }
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
            AuthError::Locked => write!(
                f,
                "Executor is locked: ask the operator to unlock it, then reconnect"
            ),
        }
    }
}

/// A connection's identity, checked again on every request and every delivered event. It
/// holds no token: revocation goes by the token's id.
#[derive(Clone, Debug)]
pub struct Session {
    pub principal: Principal,
    pub reach: ListenerReach,
    /// `None` for the operator and anonymous callers: they have no token that expires.
    expires_at: Option<u64>,
    /// When the token was issued (Unix seconds): a user's session cutoff revokes the tokens
    /// issued at or before it. 0 for the operator and anonymous callers.
    issued_at: u64,
    /// The main agent's DID, for the operator and apps. Read once it exists (a socket can
    /// open before `agent.generate()`), then fixed for the session's lifetime.
    main_did: OnceLock<String>,
}

impl Session {
    /// The identity of a caller that presents `token` on a listener with this reach.
    pub fn open(
        token: &str,
        admin_credential: &Option<Secret>,
        reach: ListenerReach,
    ) -> Result<Session, AuthError> {
        let session = |principal, expires_at, issued_at| Session {
            principal,
            reach,
            expires_at,
            issued_at,
            main_did: OnceLock::new(),
        };

        if is_admin_credential_token_on(token, admin_credential, reach) {
            return Ok(session(Principal::Operator, None, 0));
        }
        if token.is_empty() {
            let caps = anonymous_capabilities();
            return Ok(session(Principal::Anonymous { caps }, None, 0));
        }

        // Checking a token needs the wallet's signing key. A locked wallet says nothing about
        // the token, so it gets its own outcome: the client keeps the token and retries.
        if !crate::wallet::wallet_backend().is_unlocked() {
            return Err(AuthError::Locked);
        }
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
        let issued_at = claims.issued_at();
        let cut_off = |email: &str| SessionCutoffs::global().refuses(email, issued_at);
        if RevokedSet::global().contains(&token_id) || claims.sub.as_deref().is_some_and(cut_off) {
            return Err(AuthError::Revoked);
        }
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
        Ok(session(principal, expires_at, issued_at))
    }

    /// Fails once the token is revoked (by id, or for a user by the user's session cutoff)
    /// or past its expiry. Needs no wallet: the signature was checked in [`Session::open`].
    pub fn check(&self) -> Result<(), AuthError> {
        let (token_id, user) = match &self.principal {
            Principal::Operator | Principal::Anonymous { .. } => return Ok(()),
            Principal::App { token_id, .. } => (token_id, None),
            Principal::User {
                token_id, email, ..
            } => (token_id, Some(email)),
        };
        if RevokedSet::global().contains(token_id)
            || user.is_some_and(|email| SessionCutoffs::global().refuses(email, self.issued_at))
        {
            return Err(AuthError::Revoked);
        }
        let now = super::revoked::now();
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

    /// Whether the caller is the operator or presented a token: every session but an
    /// anonymous one. Only these sessions get an event stream.
    pub fn is_authenticated(&self) -> bool {
        !matches!(self.principal, Principal::Anonymous { .. })
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

    /// A user session's token id and the token's expiry (Unix seconds): what `user.logout`
    /// revokes.
    pub fn user_token(&self) -> Option<(&TokenId, u64)> {
        match (&self.principal, self.expires_at) {
            (Principal::User { token_id, .. }, Some(expires_at)) => Some((token_id, expires_at)),
            _ => None,
        }
    }

    /// A user session's email and DID.
    pub fn user(&self) -> Option<(&str, &str)> {
        match &self.principal {
            Principal::User { email, did, .. } => Some((email, did)),
            _ => None,
        }
    }

    /// The agent the session acts as: a user acts as that user, every other caller as the
    /// node's main agent.
    pub fn acts_as(&self) -> super::AgentContext {
        match &self.principal {
            Principal::User { email, .. } => super::AgentContext::for_user_email(email.clone()),
            _ => super::AgentContext::main_agent(),
        }
    }

    /// The DID of [`Session::acts_as`]: a user's own DID, the main agent's for the operator
    /// and apps. `None` for an anonymous caller, and for the operator and apps while the
    /// main agent has no DID yet. Reads the agent service only until the DID exists.
    pub fn acts_as_did(&self) -> Option<&str> {
        match &self.principal {
            Principal::User { did, .. } => Some(did),
            Principal::Anonymous { .. } => None,
            Principal::Operator | Principal::App { .. } => {
                if self.main_did.get().is_none() {
                    let did = AgentService::global_instance()
                        .lock()
                        .ok()
                        .and_then(|agent| agent.as_ref().and_then(|a| a.did.clone()));
                    if let Some(did) = did {
                        let _ = self.main_did.set(did);
                    }
                }
                self.main_did.get().map(String::as_str)
            }
        }
    }

    /// Whether this session's event stream delivers `event`: the session is in the event's
    /// audience and holds the capability that reads the topic's data (#1284). An app acts as
    /// the main agent, so the audience alone let it read every perspective the main agent
    /// owns. The stream itself ends once [`Session::check`] fails.
    pub(crate) fn may_receive(&self, event: &Event) -> bool {
        self.in_audience(&event.audience)
            && self.may(&event.topic.capability(event.perspective.as_deref()))
    }

    /// Whether this session is one of `audience`, whatever its capabilities.
    pub(crate) fn in_audience(&self, audience: &Audience) -> bool {
        match audience {
            Audience::Everyone => true,
            Audience::Operator => self.is_operator(),
            Audience::Approvers => self.may(&AGENT_PERMIT_CAPABILITY),
            Audience::Dids(dids) => self.acts_as_one_of(dids),
            Audience::OperatorAndDids(dids) => self.is_operator() || self.acts_as_one_of(dids),
        }
    }

    fn acts_as_one_of(&self, dids: &[String]) -> bool {
        self.acts_as_did()
            .is_some_and(|did| dids.iter().any(|d| d == did))
    }

    /// A session for `principal` that never expires.
    #[cfg(test)]
    pub(crate) fn for_test(principal: Principal, reach: ListenerReach) -> Session {
        Session {
            principal,
            reach,
            expires_at: None,
            issued_at: 0,
            main_did: OnceLock::new(),
        }
    }

    /// A session for `principal` on a network listener whose token expires at `expires_at`
    /// (Unix seconds). Only an app or a user session expires.
    #[cfg(test)]
    pub(crate) fn for_test_expiring(principal: Principal, expires_at: u64) -> Session {
        Session {
            expires_at: Some(expires_at),
            ..Session::for_test(principal, ListenerReach::Network)
        }
    }
}

#[cfg(test)]
mod resolution_tests {
    //! One test per way a token resolves.
    use super::*;
    use crate::agent::capabilities::{
        apps_map, get_user_default_capabilities, AuthInfo, AuthInfoExtended, ALL_CAPABILITY,
        PERSPECTIVE_CREATE_CAPABILITY, RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY,
        RUNTIME_USER_MANAGEMENT_READ_CAPABILITY,
    };
    use crate::test_utils::{setup_agent, setup_wallet, MultiUserMode};

    const ADMIN: &str = "the-admin-credential";

    fn admin() -> Option<Secret> {
        Some(Secret::from(ADMIN))
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

    /// `Capability` has no `PartialEq`: tests compare capability lists by their debug form.
    fn caps_of(caps: Vec<Capability>) -> String {
        format!("{caps:?}")
    }

    fn caps(session: &Session) -> String {
        caps_of(session.capabilities())
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
        assert!(
            !multi.may(&RUNTIME_USER_MANAGEMENT_READ_CAPABILITY),
            "anonymous callers cannot list users"
        );
    }

    /// Every combination of credential, listener and multi-user setting for an empty token.
    /// Neither the operator nor an anonymous caller has a token that expires.
    #[test]
    fn an_empty_token_is_the_operator_or_anonymous_in_every_mode_and_never_expires() {
        ensure_db();
        for multi_user in [false, true] {
            let mode = multi_user.then(MultiUserMode::on);
            for credential in [None, admin()] {
                for reach in [ListenerReach::Loopback, ListenerReach::Network] {
                    let case = format!("{credential:?}, {reach:?}, multi-user {multi_user}");
                    let session = Session::open("", &credential, reach).unwrap();
                    let operator = credential.is_none() && reach == ListenerReach::Loopback;
                    assert_eq!(session.is_operator(), operator, "{case}");
                    if operator {
                        assert_eq!(caps(&session), caps_of(vec![ALL_CAPABILITY.clone()]));
                    } else {
                        assert_eq!(caps(&session), caps_of(anonymous_capabilities()), "{case}");
                        assert_eq!(
                            session.may(&RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY),
                            multi_user,
                            "{case}"
                        );
                    }
                    assert_eq!(session.user(), None, "{case}");
                    assert_eq!(session.expires_at, None, "{case}");
                    assert!(session.check().is_ok(), "{case}");
                }
            }
            drop(mode);
        }
    }

    #[test]
    fn a_token_without_a_user_is_an_app_limited_to_its_capabilities_on_any_node() {
        let token = app_token(Some(vec![PERSPECTIVE_CREATE_CAPABILITY.clone()]));
        for credential in [None, admin()] {
            for reach in [ListenerReach::Loopback, ListenerReach::Network] {
                let session = Session::open(&token, &credential, reach).unwrap();
                assert!(matches!(session.principal, Principal::App { .. }));
                assert_eq!(
                    caps(&session),
                    caps_of(vec![PERSPECTIVE_CREATE_CAPABILITY.clone()])
                );
                assert!(!session.may(&ALL_CAPABILITY));
                assert!(!session.is_operator());
                assert_eq!(session.user(), None);
                assert!(session.check().is_ok());
            }
        }
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

        let open = || {
            let mut sessions = vec![];
            for credential in [None, admin()] {
                for reach in [ListenerReach::Loopback, ListenerReach::Network] {
                    sessions.push(Session::open(&token, &credential, reach).unwrap());
                }
            }
            sessions
        };
        let mut sessions = open();
        let multi_user = MultiUserMode::on();
        sessions.extend(open());
        drop(multi_user);

        for session in sessions {
            match &session.principal {
                Principal::User { email: e, did, .. } => {
                    assert_eq!(e, email);
                    assert_eq!(did, &expected_did);
                }
                other => panic!("expected a user, got {other:?}"),
            }
            assert_eq!(caps(&session), caps_of(get_user_default_capabilities()));
            assert!(!session.is_operator());
            assert!(
                !session.may(&ALL_CAPABILITY),
                "a user never holds every capability"
            );
            assert!(session.expires_at.is_some(), "a user token expires");
            assert!(session.check().is_ok());
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

    // On a node without an admin credential, a string that is not a JWT is garbage, even
    // the one another node uses as its credential.
    #[test]
    fn a_credential_on_a_node_without_one_opens_no_session() {
        setup_wallet();
        setup_agent();
        for reach in [ListenerReach::Loopback, ListenerReach::Network] {
            let err = Session::open(ADMIN, &None, reach).unwrap_err();
            assert!(matches!(err, AuthError::Invalid(_)), "{reach:?}: {err:?}");
        }
    }

    #[test]
    fn a_locked_wallet_opens_no_session_and_says_so() {
        let token = app_token(None);
        let backend = crate::wallet::wallet_backend();
        backend.lock("session-locked-test");
        let locked = Session::open(&token, &admin(), ListenerReach::Network);
        let admin_while_locked = Session::open(ADMIN, &admin(), ListenerReach::Network);
        // Unlock before asserting, so a failure cannot leave the shared wallet locked.
        backend
            .unlock("session-locked-test")
            .expect("restore the shared wallet");

        // A locked wallet is a state of the node, not of the token: its own outcome, with a
        // close code that clients do not read as "log in again".
        let err = locked.unwrap_err();
        assert_eq!(err, AuthError::Locked);
        assert_eq!(err.close_code(), 1013);
        assert!(err.to_string().contains("Executor is locked"), "{err}");
        assert_eq!(AuthError::Expired.close_code(), 1008);
        // The operator needs no wallet to be recognised, so it can unlock it.
        assert!(admin_while_locked.unwrap().is_operator());
        // The same token works once the wallet is unlocked again.
        assert!(Session::open(&token, &admin(), ListenerReach::Network).is_ok());
    }

    /// An approved app, as `agent.generateJwt` leaves it: its request id and its token.
    fn approved_app() -> (String, String) {
        crate::test_utils::use_test_apps_file();
        let token = app_token(None);
        let request_id = format!("session-{}", uuid::Uuid::new_v4());
        let app = AuthInfoExtended {
            request_id: request_id.clone(),
            auth: AuthInfo::default(),
        };
        crate::test_utils::insert_app_with_token(request_id.clone(), app, &token);
        (request_id, token)
    }

    #[test]
    fn a_revoked_token_opens_no_session_and_ends_an_open_one() {
        let (request_id, token) = approved_app();
        let open = Session::open(&token, &admin(), ListenerReach::Network).unwrap();

        apps_map::revoke_app(&request_id).unwrap();
        assert_eq!(open.check().unwrap_err(), AuthError::Revoked);
        assert_eq!(
            Session::open(&token, &admin(), ListenerReach::Network).unwrap_err(),
            AuthError::Revoked
        );
        apps_map::remove_app(&request_id).unwrap();
    }

    // #396: removing a revoked app deleted the only record of the revocation, so its token
    // worked again until it expired.
    #[test]
    fn a_revoked_token_stays_revoked_after_its_app_is_removed() {
        let (request_id, token) = approved_app();
        let open = Session::open(&token, &admin(), ListenerReach::Network).unwrap();

        apps_map::revoke_app(&request_id).unwrap();
        apps_map::remove_app(&request_id).unwrap();
        assert_eq!(open.check().unwrap_err(), AuthError::Revoked);
        assert_eq!(
            Session::open(&token, &admin(), ListenerReach::Network).unwrap_err(),
            AuthError::Revoked
        );
    }

    #[test]
    fn revoking_one_app_leaves_another_apps_token_working() {
        let (revoked_id, _) = approved_app();
        let (other_id, other_token) = approved_app();
        let open = Session::open(&other_token, &admin(), ListenerReach::Network).unwrap();

        apps_map::revoke_app(&revoked_id).unwrap();
        assert!(open.check().is_ok());
        assert!(Session::open(&other_token, &admin(), ListenerReach::Network).is_ok());
        apps_map::remove_app(&revoked_id).unwrap();
        apps_map::remove_app(&other_id).unwrap();
    }

    #[test]
    fn a_token_past_its_expiry_fails_the_check_after_the_leeway() {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        let app = || Principal::App {
            token_id: TokenId("t".into()),
            caps: vec![],
        };
        let user = || Principal::User {
            token_id: TokenId("t".into()),
            email: "expiring@example.org".into(),
            did: "did:key:expiring".into(),
            caps: vec![],
        };
        // Both principals that carry a token stop working at its expiry, apps and users alike.
        for principal in [app, user] {
            let session = |expires_at| Session::for_test_expiring(principal(), expires_at);
            assert!(session(now + 3600).check().is_ok());
            assert!(session(now - 10).check().is_ok(), "inside the leeway");
            let err = session(now - 3600).check().unwrap_err();
            assert_eq!(err, AuthError::Expired);
            assert_eq!(
                err.to_string(),
                "Unauthorized access: the token has expired"
            );
        }
    }

    #[test]
    fn a_formatted_session_never_carries_its_token() {
        let token = app_token(None);
        let session = Session::open(&token, &admin(), ListenerReach::Network).unwrap();
        let formatted = format!("{session:?}");
        assert!(!formatted.contains(&token), "{formatted}");
        assert!(formatted.contains("App"), "{formatted}");
    }

    #[test]
    fn a_user_acts_as_the_user_and_every_other_caller_as_the_main_agent() {
        let session = |principal| Session::for_test(principal, ListenerReach::Network);
        let user = session(Principal::User {
            token_id: TokenId("t".into()),
            email: "acts.as@example.org".into(),
            did: "did:key:user".into(),
            caps: vec![],
        });
        assert_eq!(
            user.acts_as(),
            crate::agent::AgentContext::for_user_email("acts.as@example.org".into())
        );

        for principal in [
            Principal::Operator,
            Principal::App {
                token_id: TokenId("t".into()),
                caps: vec![],
            },
            Principal::Anonymous { caps: vec![] },
        ] {
            assert_eq!(
                session(principal).acts_as(),
                crate::agent::AgentContext::main_agent()
            );
        }
    }
}
