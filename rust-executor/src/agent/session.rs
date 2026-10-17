//! Who is calling: one [`Session`] per connection.
//!
//! An executor hosts N agents: the main agent (agent #0, owned by the operator) and one
//! managed agent per user. Every session acts as one agent within a grant (a token and its
//! capabilities), or is the operator, or is anonymous.
//!
//! [`Session::open`] is the one place that turns a token into an identity:
//!
//! - The admin credential is the operator. An executor without one treats no token on a
//!   loopback listener as the operator (#1059).
//! - Any other caller without a token is anonymous.
//! - A token that does not decode or is past its expiry opens no session, and neither does
//!   one without an active grant (`agent::grants`): grants are the allowlist, so a revoked,
//!   removed or never-recorded token is refused.
//! - A grant on a managed agent acts as that agent, by its DID, whose key must derive that
//!   DID. The token's `sub` must name the grant's agent by its DID. Any other grant acts as the
//!   main agent. Both are limited to the grant's capabilities.
//!
//! A token that opens no session is refused where it arrives. [`Session::check`] runs again
//! on every request and every delivered event. MCP opens one per MCP session the same way
//! (`mcp::caller`) and checks it on every tool call.
//!
//! [`Session::in_process`] is the exception: a session the executor opens for its own work
//! as one of its agents, with no token and no grant.

use std::sync::{Arc, OnceLock};

use tokio_util::sync::CancellationToken;

use super::capabilities::{
    anonymous_capabilities, capability_matches, decode_jwt, is_admin_credential_token_on,
    require_capability, Capability, ListenerReach, ALL_CAPABILITY, TOKEN_EXPIRY_LEEWAY_SECONDS,
};
use super::grants::{Grants, LiveGrant};
use super::{did_for_context, AgentContext, AgentService};
use crate::events::{Audience, Event};
use crate::secret::Secret;

/// The id of a JWT: its `nonce` claim, a fresh v4 UUID per token. It names the token's grant.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TokenId(pub String);

/// The kind of caller behind a connection.
#[derive(Clone, Debug, PartialEq)]
pub enum Principal {
    /// The executor's administrator role: the admin credential, or no token on a loopback
    /// listener of an executor without one (#1059). Not an agent: it acts as the main agent,
    /// which it owns.
    Operator,
    /// A grant on one agent: a token without `sub` acts as the main agent, a token with
    /// `sub` as that user's managed agent, whatever the multi-user setting says.
    Agent(AgentContext),
    /// No token: may request a capability, and sign up or log in when multi-user mode is on.
    Anonymous,
}

/// Why a token opens no session.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AuthError {
    /// The token has no active grant: revoked, removed, or never recorded (a user token
    /// issued before grants).
    Revoked,
    /// The token's expiry has passed.
    Expired,
    /// The token does not decode: a bad signature or garbage.
    Invalid(String),
    /// The token names a user whose DID does not load.
    NoIdentity(String),
    /// The executor's wallet is locked, so no token can be checked yet. A state of the
    /// executor, not of the token: the token may be good, and the caller should try again
    /// after the unlock.
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
/// holds no token: a grant's session holds the grant.
#[derive(Clone, Debug)]
pub struct Session {
    pub principal: Principal,
    /// What the session may do: every capability for the operator, the anonymous set, or the
    /// grant's capabilities.
    caps: Vec<Capability>,
    /// The grant the token acts within. `None` for the operator and anonymous callers.
    grant: Option<Arc<LiveGrant>>,
    /// The DID of the agent the session acts as. A grant's is set at open; the main agent's
    /// is read once it exists for the operator (a socket can open before `agent.generate()`).
    did: OnceLock<String>,
}

impl Session {
    /// The identity of a caller that presents `token` on a listener with this reach.
    pub fn open(
        token: &str,
        admin_credential: &Option<Secret>,
        reach: ListenerReach,
    ) -> Result<Session, AuthError> {
        if is_admin_credential_token_on(token, admin_credential, reach) {
            return Ok(Session::without_token(
                Principal::Operator,
                vec![ALL_CAPABILITY.clone()],
            ));
        }
        if token.is_empty() {
            return Ok(Session::without_token(
                Principal::Anonymous,
                anonymous_capabilities(),
            ));
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

        let grant = grant_for(&claims)?;
        let did = OnceLock::new();
        match grant.agent.managed_did() {
            Some(agent_did) => {
                let _ = did.set(agent_did.to_string());
            }
            None if !grant.did.is_empty() => {
                let _ = did.set(grant.did.clone());
            }
            None => {}
        }
        Ok(Session {
            principal: Principal::Agent(grant.agent.clone()),
            caps: grant.caps.clone(),
            grant: Some(grant),
            did,
        })
    }

    /// A session without a token: the operator, an anonymous caller, or an in-process agent.
    fn without_token(principal: Principal, caps: Vec<Capability>) -> Session {
        Session {
            principal,
            caps,
            grant: None,
            did: OnceLock::new(),
        }
    }

    /// A session for work the executor runs as `agent` without a token from it, limited to
    /// `caps`. The interpretation pass of the auto-processor acts as the owner of its
    /// perspective this way: never as the operator, so the guards treat it as that agent.
    ///
    /// No token and no grant stand behind it: nothing to revoke or expire, so
    /// [`Session::check`] always passes and [`Session::ended`] never fires. It lives only as
    /// long as the work that holds it, and `user.logout` finds no grant in it to revoke.
    pub fn in_process(agent: AgentContext, caps: Vec<Capability>) -> Session {
        Session::without_token(Principal::Agent(agent), caps)
    }

    /// This session with `did` as the DID of the agent it acts as: for an in-process session
    /// whose caller knows the DID already (a perspective's owner list names it).
    pub(crate) fn with_did(self, did: &str) -> Session {
        Session {
            did: OnceLock::from(did.to_string()),
            ..self
        }
    }

    /// Fails once the grant is revoked or removed, or the token is past its expiry. Needs no
    /// wallet and no lock: the signature was checked in [`Session::open`], and the grant
    /// carries its own state.
    pub fn check(&self) -> Result<(), AuthError> {
        match &self.grant {
            Some(grant) => grant.holds(),
            None => Ok(()),
        }
    }

    /// Fires when the session's grant ends (revoked or removed): each socket of the session
    /// watches one and closes. Cancelling it ends only that socket. Never fires for the
    /// operator and anonymous callers.
    pub fn ended(&self) -> CancellationToken {
        match &self.grant {
            Some(grant) => grant.ended.child_token(),
            None => CancellationToken::new(),
        }
    }

    /// Completes when the session stops holding: its grant ends or its token expires, so an
    /// idle socket closes at expiry as it does at a revocation. Never for the operator and
    /// anonymous callers.
    pub fn over(&self) -> impl std::future::Future<Output = ()> + Send + 'static {
        let grant = self.grant.clone();
        async move {
            let Some(grant) = grant else {
                return std::future::pending().await;
            };
            let expired = grant
                .expires_at
                .saturating_add(TOKEN_EXPIRY_LEEWAY_SECONDS + 1);
            let wait = std::time::Duration::from_secs(expired.saturating_sub(super::grants::now()));
            tokio::select! {
                _ = grant.ended.cancelled() => {}
                _ = tokio::time::sleep(wait) => {}
            }
        }
    }

    /// Whether the session's capabilities cover `expected`. The operator may do anything.
    pub fn may(&self, expected: &Capability) -> bool {
        capability_matches(&self.caps, expected)
    }

    /// `Ok` if the session may do `expected`; otherwise the error a refused call reports.
    pub fn require(&self, expected: &Capability) -> Result<(), String> {
        require_capability(&self.caps, expected)
    }

    /// The one test for "is this the executor's administrator?".
    pub fn is_operator(&self) -> bool {
        self.principal == Principal::Operator
    }

    /// Whether the caller presented a credential: the operator or a grant on an agent.
    pub fn is_authenticated(&self) -> bool {
        self.principal != Principal::Anonymous
    }

    /// The capabilities that checks run against: every capability for the operator.
    pub fn capabilities(&self) -> Vec<Capability> {
        self.caps.clone()
    }

    /// The grant the session acts within. `None` for the operator and anonymous callers.
    pub fn grant(&self) -> Option<&Arc<LiveGrant>> {
        self.grant.as_ref()
    }

    /// A user session's grant: what `user.logout` revokes.
    pub fn user_grant(&self) -> Option<&Arc<LiveGrant>> {
        match &self.principal {
            Principal::Agent(AgentContext::Managed { .. }) => self.grant.as_ref(),
            _ => None,
        }
    }

    /// The DID of the managed agent the session acts as; `None` for the main agent, the
    /// operator and anonymous callers.
    pub fn managed_did(&self) -> Option<&str> {
        match &self.principal {
            Principal::Agent(agent) => agent.managed_did(),
            _ => None,
        }
    }

    /// The agent the session acts as: a grant's agent; the main agent for the operator,
    /// which owns it, and for everyone else.
    pub fn acts_as(&self) -> AgentContext {
        match &self.principal {
            Principal::Agent(agent) => agent.clone(),
            Principal::Operator | Principal::Anonymous => AgentContext::main_agent(),
        }
    }

    /// The DID of the agent the session acts as: a managed agent's own DID, the main
    /// agent's for the operator and for grants on the main agent. `None` for an anonymous
    /// caller, and for the main agent until it has a DID.
    pub fn acts_as_did(&self) -> Option<&str> {
        if self.principal == Principal::Anonymous {
            return None;
        }
        if self.did.get().is_none() {
            if let Ok(did) = did_for_context(&self.acts_as()) {
                let _ = self.did.set(did);
            }
        }
        self.did.get().map(String::as_str)
    }

    /// Whether this session's event stream delivers `event`: the session is in the event's
    /// audience (everyone, or it acts as one of the DIDs) and holds the event's capability
    /// (#1284): the one that reads the topic's data, or AGENT_PERMIT for an approval request.
    /// A grant on the main agent acts as the main agent, so the audience alone let it read
    /// every perspective the main agent owns. The operator acts as the main agent too: it gets
    /// the main agent's events, not every agent's. The stream itself ends once
    /// [`Session::check`] fails.
    pub(crate) fn may_receive(&self, event: &Event) -> bool {
        let in_audience = match &event.audience {
            Audience::Everyone => true,
            Audience::Dids(dids) => self
                .acts_as_did()
                .is_some_and(|did| dids.iter().any(|d| d == did)),
        };
        in_audience && self.may(&event.capability)
    }
}

/// The grant that the decoded token `claims` acts within. The allowlist: a token works only
/// while its grant does, so a token without one (revoked and removed, or a user token from
/// before grants) is refused. Its `sub` must name the grant's agent, and a managed agent's
/// key must derive that agent's DID.
pub(crate) fn grant_for(claims: &super::capabilities::Claims) -> Result<Arc<LiveGrant>, AuthError> {
    let grant = Grants::global()
        .get(&TokenId(claims.nonce().to_string()))
        .ok_or(AuthError::Revoked)?;
    if !sub_names_agent(claims.sub.as_deref(), &grant.agent) {
        return Err(AuthError::Revoked);
    }
    if let Some(agent_did) = grant.agent.managed_did() {
        AgentService::wallet_key_for_did(agent_did)
            .map_err(|e| AuthError::NoIdentity(e.to_string()))?;
    }
    Ok(grant)
}

/// Whether a token's `sub` names the agent of its grant: none for the main agent, the DID for
/// a managed agent.
fn sub_names_agent(sub: Option<&str>, agent: &AgentContext) -> bool {
    sub == agent.managed_did()
}

#[cfg(test)]
impl Session {
    /// A session for `principal` that never expires and carries no raw token: every
    /// capability for the operator, none otherwise. An agent gets a grant that no store
    /// holds.
    pub(crate) fn for_test(principal: Principal) -> Session {
        let caps = match principal {
            Principal::Operator => vec![ALL_CAPABILITY.clone()],
            _ => vec![],
        };
        let grant = match &principal {
            Principal::Agent(agent) => Some(Arc::new(LiveGrant::for_test(
                "test",
                agent.clone(),
                u64::MAX,
            ))),
            _ => None,
        };
        Session {
            grant,
            ..Session::without_token(principal, caps)
        }
    }

    /// This session with `caps` as its capabilities.
    pub(crate) fn with_caps(self, caps: Vec<Capability>) -> Session {
        Session { caps, ..self }
    }

    /// This session within `grant`.
    pub(crate) fn with_grant(self, grant: Arc<LiveGrant>) -> Session {
        Session {
            grant: Some(grant),
            ..self
        }
    }

    /// This grant's session with a token that expires at `expires_at` (Unix seconds).
    pub(crate) fn expiring_at(self, expires_at: u64) -> Session {
        let grant = self
            .grant
            .as_ref()
            .expect("only a grant's session has a token that expires")
            .expiring_at(expires_at);
        self.with_grant(Arc::new(grant))
    }
}

#[cfg(test)]
mod resolution_tests {
    //! One test per way a token resolves.
    use super::*;
    use crate::agent::capabilities::{
        get_user_default_capabilities, AuthInfo, AGENT_AUTH_CAPABILITY, ALL_CAPABILITY,
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
        crate::test_utils::granted_token(AuthInfo {
            app_name: "an app".to_string(),
            capabilities,
            ..AuthInfo::default()
        })
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
        assert_eq!(remote.principal, Principal::Anonymous);
        assert!(
            !remote.may(&ALL_CAPABILITY),
            "#1059: no operator over the network"
        );
    }

    #[test]
    fn no_token_on_an_executor_with_a_credential_is_anonymous() {
        ensure_db();
        for reach in [ListenerReach::Loopback, ListenerReach::Network] {
            let session = Session::open("", &admin(), reach).unwrap();
            assert_eq!(session.principal, Principal::Anonymous);
            assert!(!session.is_authenticated());
            assert_eq!(
                session.acts_as_did(),
                None,
                "an anonymous caller acts as no agent"
            );
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
                    assert_eq!(session.managed_did(), None, "{case}");
                    assert!(session.grant().is_none(), "{case}");
                    assert!(session.check().is_ok(), "{case}");
                }
            }
            drop(mode);
        }
    }

    #[test]
    fn a_token_without_a_user_is_a_grant_on_the_main_agent_on_any_executor() {
        let token = app_token(Some(vec![PERSPECTIVE_CREATE_CAPABILITY.clone()]));
        for credential in [None, admin()] {
            for reach in [ListenerReach::Loopback, ListenerReach::Network] {
                let session = Session::open(&token, &credential, reach).unwrap();
                assert_eq!(
                    session.principal,
                    Principal::Agent(AgentContext::main_agent())
                );
                assert_eq!(
                    caps(&session),
                    caps_of(vec![PERSPECTIVE_CREATE_CAPABILITY.clone()])
                );
                assert!(!session.may(&ALL_CAPABILITY));
                assert!(!session.is_operator());
                assert_eq!(session.managed_did(), None);
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
    fn a_user_token_is_a_grant_on_the_users_agent_whatever_the_multi_user_setting() {
        setup_wallet();
        setup_agent();
        let email = "session.user@example.org";
        let expected_did = crate::test_utils::create_test_user(email);
        let token = crate::user_management::generate_user_jwt(email, "test").unwrap();

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
        // Multi-user mode starts on a fresh database: the user signs up there too.
        crate::db::Ad4mDb::with_global_instance(|db| db.add_user(email, &expected_did, "pw"))
            .unwrap();
        sessions.extend(open());
        drop(multi_user);

        for session in sessions {
            assert_eq!(
                session.principal,
                Principal::Agent(AgentContext::managed(expected_did.clone()))
            );
            assert_eq!(session.managed_did(), Some(expected_did.as_str()));
            assert_eq!(session.acts_as_did(), Some(expected_did.as_str()));
            assert_eq!(caps(&session), caps_of(get_user_default_capabilities()));
            assert!(!session.is_operator());
            assert!(
                !session.may(&ALL_CAPABILITY),
                "a user never holds every capability"
            );
            assert!(
                session.grant().is_some_and(|g| g.expires_at > 0),
                "a user token expires"
            );
            assert!(session.check().is_ok());
        }
    }

    // A user whose key went after the login: the grant stays, the identity does not load.
    #[test]
    fn a_user_whose_did_does_not_load_opens_no_session() {
        setup_wallet();
        setup_agent();
        let token = crate::test_utils::keyless_user_token("session.nokey@example.org");
        let err = Session::open(&token, &admin(), ListenerReach::Network).unwrap_err();
        assert!(matches!(err, AuthError::NoIdentity(_)), "{err:?}");
        // Nor does a user token for an email no user has get minted at all.
        assert!(crate::user_management::generate_user_jwt("ghost@example.org", "test").is_err());
    }

    // A token's `sub` must name its grant's agent: one re-signed to name another agent, by
    // email or by DID, is refused.
    #[test]
    fn a_token_whose_sub_names_another_agent_opens_no_session() {
        setup_wallet();
        setup_agent();
        let email = "session.sub@example.org";
        crate::test_utils::create_test_user(email);
        let other = "session.sub.other@example.org";
        let other_did = crate::test_utils::create_test_user(other);
        let token = crate::user_management::generate_user_jwt(email, "test").unwrap();
        for ghost in [
            other,
            other_did.as_str(),
            "session.ghost@example.org",
            "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK",
        ] {
            let token = crate::test_utils::resigned_token(&token, |claims| {
                claims["sub"] = serde_json::json!(ghost)
            });
            let err = Session::open(&token, &admin(), ListenerReach::Network).unwrap_err();
            assert_eq!(err, AuthError::Revoked, "{ghost}");
        }
        // A key named by its DID that no user owns (a signup that failed after the key was
        // made): the key alone is no agent. A token re-signed to name it keeps its grant on
        // another agent, so the grant check refuses it first (Revoked); the subject lookup
        // refuses the orphan on its own too.
        let orphan = crate::wallet::wallet_backend()
            .generate_keypair_named_by_did()
            .unwrap();
        let orphan_token = crate::test_utils::resigned_token(&token, |claims| {
            claims["sub"] = serde_json::json!(orphan)
        });
        let err = Session::open(&orphan_token, &admin(), ListenerReach::Network).unwrap_err();
        assert_eq!(err, AuthError::Revoked, "{orphan}");
        assert!(
            crate::agent::AgentService::managed_agent_for_subject(&orphan, None).is_err(),
            "{orphan}: no user owns this DID"
        );
        // Nor does a user token for an email no user has get minted at all.
        assert!(crate::user_management::generate_user_jwt("ghost@example.org", "test").is_err());
    }

    // A grant on the main agent whose token names a user is refused: acting on it would turn
    // a user's app into a main-agent app. MCP opens the same `Session`.
    #[test]
    fn a_token_whose_sub_names_another_agent_than_its_grant_is_refused() {
        setup_wallet();
        setup_agent();
        let email = crate::test_utils::managed_user("session.mcp.sub");
        let app = crate::test_utils::granted_token(AuthInfo {
            app_name: "an app approved for a user before grants".into(),
            capabilities: Some(vec![ALL_CAPABILITY.clone()]),
            ..AuthInfo::default()
        });
        let legacy = crate::test_utils::resigned_token(&app, |claims| {
            claims["sub"] = serde_json::json!(email)
        });
        let err = Session::open(&legacy, &admin(), ListenerReach::Network).unwrap_err();
        assert_eq!(err, AuthError::Revoked);
    }

    /// D16: a user token names its agent by DID in `sub` and acts as that agent. A token
    /// whose `sub` is the user's email instead opens no session.
    #[test]
    fn a_user_token_acts_as_the_agent_its_sub_names() {
        setup_wallet();
        setup_agent();
        let _multi_user = MultiUserMode::on();
        let email = "session.did.sub@example.org";
        let did = crate::test_utils::create_test_user(email);

        let token = crate::user_management::generate_user_jwt(email, "test").unwrap();
        let claims = crate::agent::capabilities::decode_jwt(token.clone()).unwrap();
        assert_eq!(
            claims.sub.as_deref(),
            Some(did.as_str()),
            "tokens name the DID"
        );

        let session = Session::open(&token, &admin(), ListenerReach::Network).unwrap();
        assert_eq!(
            session.principal,
            Principal::Agent(AgentContext::managed(did.clone()))
        );
        assert_eq!(session.acts_as_did(), Some(did.as_str()));
        let signed = crate::agent::create_signed_expression(
            serde_json::json!({ "x": 1 }),
            &session.acts_as(),
        )
        .unwrap();
        assert_eq!(signed.author, did, "the session signs as the user's agent");

        let email_token = crate::test_utils::resigned_token(&token, |claims| {
            claims["sub"] = serde_json::json!(email)
        });
        let err = Session::open(&email_token, &admin(), ListenerReach::Network).unwrap_err();
        assert_eq!(err, AuthError::Revoked);
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

    // On an executor without an admin credential, a string that is not a JWT is garbage,
    // even the one another executor uses as its credential.
    #[test]
    fn a_credential_on_an_executor_without_one_opens_no_session() {
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

        // A locked wallet is a state of the executor, not of the token: its own outcome, with a
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

    fn revoke(token: &str) {
        let grants = Grants::global();
        grants.revoke(&grants.find(token)).unwrap();
    }

    // The allowlist: a token the executor signed but holds no grant for (a user token from
    // before grants, D4) opens no session.
    #[test]
    fn a_signed_token_without_a_grant_opens_no_session() {
        setup_wallet();
        setup_agent();
        let (token, _) =
            crate::agent::capabilities::token::sign_token("no grant".to_string(), 3600, None)
                .unwrap();
        for reach in [ListenerReach::Loopback, ListenerReach::Network] {
            let err = Session::open(&token, &admin(), reach).unwrap_err();
            assert_eq!(err, AuthError::Revoked);
            assert_eq!(err.close_code(), 1008);
        }
    }

    #[test]
    fn a_revoked_token_opens_no_session_and_ends_an_open_one() {
        let token = app_token(None);
        let open = Session::open(&token, &admin(), ListenerReach::Network).unwrap();
        let socket = open.ended();

        revoke(&token);
        assert_eq!(open.check().unwrap_err(), AuthError::Revoked);
        assert!(socket.is_cancelled(), "the session's sockets end");
        assert_eq!(
            Session::open(&token, &admin(), ListenerReach::Network).unwrap_err(),
            AuthError::Revoked
        );
    }

    // #396: removing a revoked app deleted the only record of the revocation, so its token
    // worked again until it expired. A removed grant refuses as a revoked one does.
    #[test]
    fn a_removed_apps_token_stays_refused() {
        for revoke_first in [true, false] {
            let token = app_token(None);
            let open = Session::open(&token, &admin(), ListenerReach::Network).unwrap();
            let grants = Grants::global();
            let found = grants.find(&token);
            if revoke_first {
                grants.revoke(&found).unwrap();
            }
            grants.remove(&found).unwrap();
            assert_eq!(open.check().unwrap_err(), AuthError::Revoked);
            assert_eq!(
                Session::open(&token, &admin(), ListenerReach::Network).unwrap_err(),
                AuthError::Revoked
            );
        }
    }

    #[test]
    fn revoking_one_app_leaves_another_apps_token_working() {
        let revoked = app_token(None);
        let other = app_token(None);
        let open = Session::open(&other, &admin(), ListenerReach::Network).unwrap();

        revoke(&revoked);
        assert!(open.check().is_ok());
        assert!(!open.ended().is_cancelled());
        assert!(Session::open(&other, &admin(), ListenerReach::Network).is_ok());
    }

    // Capabilities come from the grant, not the token: a token carries none.
    #[test]
    fn a_session_has_its_grants_capabilities() {
        let token = app_token(Some(vec![PERSPECTIVE_CREATE_CAPABILITY.clone()]));
        let claims = crate::agent::capabilities::read_stored_claims(&token).unwrap();
        let payload = token.split('.').nth(1).unwrap();
        let raw =
            base64::Engine::decode(&base64::engine::general_purpose::URL_SAFE_NO_PAD, payload)
                .unwrap();
        let raw: serde_json::Value = serde_json::from_slice(&raw).unwrap();
        let mut fields: Vec<&str> = raw
            .as_object()
            .unwrap()
            .keys()
            .map(|k| k.as_str())
            .collect();
        fields.sort();
        assert_eq!(fields, ["aud", "exp", "iat", "iss", "nonce", "sub"]);
        let session = Session::open(&token, &admin(), ListenerReach::Network).unwrap();
        assert_eq!(
            session.grant().unwrap().id,
            TokenId(claims.nonce().to_string())
        );
        assert_eq!(
            caps(&session),
            caps_of(vec![PERSPECTIVE_CREATE_CAPABILITY.clone()])
        );
    }

    #[test]
    fn a_token_past_its_expiry_fails_the_check_after_the_leeway() {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        // A grant stops working at its token's expiry, on the main agent and on a managed one.
        for agent in [
            AgentContext::main_agent(),
            AgentContext::managed("did:key:expiring".into()),
        ] {
            let session = |expires_at| {
                Session::for_test(Principal::Agent(agent.clone())).expiring_at(expires_at)
            };
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
        assert!(formatted.contains("Agent"), "{formatted}");
    }

    #[test]
    fn a_grant_acts_as_its_agent_and_every_other_caller_as_the_main_agent() {
        let managed = AgentContext::managed("did:key:acts-as".into());
        let user = Session::for_test(Principal::Agent(managed.clone()));
        assert_eq!(user.acts_as(), managed);

        for principal in [
            Principal::Operator,
            Principal::Agent(AgentContext::main_agent()),
            Principal::Anonymous,
        ] {
            assert_eq!(
                Session::for_test(principal).acts_as(),
                AgentContext::main_agent()
            );
        }
    }

    /// One agent model: a token for the main agent and a token for a managed agent open the
    /// same kind of session, a grant on that agent. Only the agent differs. The operator is
    /// no agent, but acts as the main agent it owns.
    #[test]
    fn tokens_for_the_main_agent_and_a_managed_agent_both_open_a_grant_on_their_agent() {
        // Sets up the wallet and the agent, so the main agent's DID is read after it.
        let main_token = app_token(Some(vec![PERSPECTIVE_CREATE_CAPABILITY.clone()]));
        let main_did = AgentService::with_global_instance(|a| a.did.clone()).unwrap();
        let email = "session.grant@example.org";
        let user_did = crate::test_utils::create_test_user(email);

        let main = Session::open(&main_token, &admin(), ListenerReach::Network).unwrap();
        assert!(matches!(&main.principal, Principal::Agent(agent) if agent.is_main_agent()));
        assert!(main.acts_as().is_main_agent());
        assert_eq!(main.acts_as_did(), Some(main_did.as_str()));
        assert_eq!(main.managed_did(), None, "the main agent has no user");
        assert_eq!(
            caps(&main),
            caps_of(vec![PERSPECTIVE_CREATE_CAPABILITY.clone()])
        );

        let user_token = crate::user_management::generate_user_jwt(email, "test").unwrap();
        let user = Session::open(&user_token, &admin(), ListenerReach::Network).unwrap();
        assert!(matches!(&user.principal, Principal::Agent(agent) if !agent.is_main_agent()));
        assert_eq!(user.acts_as(), AgentContext::managed(user_did.clone()));
        assert_eq!(user.acts_as_did(), Some(user_did.as_str()));
        assert_eq!(caps(&user), caps_of(get_user_default_capabilities()));

        for grant in [&main, &user] {
            assert!(grant.is_authenticated());
            assert!(!grant.is_operator());
            assert!(grant.grant().is_some(), "a grant's session holds its grant");
        }

        let operator = Session::open(ADMIN, &admin(), ListenerReach::Network).unwrap();
        assert!(operator.is_authenticated());
        assert_eq!(operator.acts_as(), AgentContext::main_agent());
        assert_eq!(operator.acts_as_did(), Some(main_did.as_str()));
        assert_eq!(operator.managed_did(), None);
        assert!(operator.grant().is_none());
    }

    #[test]
    fn require_refuses_with_the_capability_error() {
        let session = Session::for_test(Principal::Agent(AgentContext::main_agent()))
            .with_caps(vec![AGENT_AUTH_CAPABILITY.clone()]);
        assert!(session.require(&AGENT_AUTH_CAPABILITY).is_ok());
        let err = session.require(&PERSPECTIVE_CREATE_CAPABILITY).unwrap_err();
        assert_eq!(
            err,
            crate::agent::capabilities::check_capability(
                &Ok(vec![AGENT_AUTH_CAPABILITY.clone()]),
                &PERSPECTIVE_CREATE_CAPABILITY
            )
            .unwrap_err()
        );
        assert!(Session::for_test(Principal::Operator)
            .require(&PERSPECTIVE_CREATE_CAPABILITY)
            .is_ok());
    }

    // An in-process session acts as its agent within no grant: it is never the operator,
    // revoking every grant of its agent leaves it working, and logout finds nothing in it.
    #[test]
    fn an_in_process_session_is_its_agent_never_the_operator_and_has_no_grant() {
        let agent = AgentContext::managed("did:key:in-process".to_string());
        let user = Session::in_process(agent.clone(), get_user_default_capabilities());
        assert!(!user.is_operator());
        assert!(user.is_authenticated());
        assert_eq!(user.acts_as(), agent);
        assert_eq!(caps(&user), caps_of(get_user_default_capabilities()));
        assert!(user.grant().is_none());
        assert!(
            user.user_grant().is_none(),
            "nothing for user.logout to revoke"
        );

        let read = crate::agent::capabilities::perspective_query_capability(vec!["p".into()]);
        let main = Session::in_process(AgentContext::main_agent(), vec![read.clone()]);
        assert!(!main.is_operator());
        assert!(main.may(&read));
        assert!(!main.may(&ALL_CAPABILITY));

        let _ = Grants::global().revoke_agent(&agent);
        assert!(user.check().is_ok());
        assert!(main.check().is_ok());
        assert!(!user.ended().is_cancelled());
    }
}
