//! A multi-user executor for handler-level isolation tests.
//!
//! Requests go through the real handler map, guards included. User and app sessions open from
//! tokens the executor signs, the way the WS upgrade opens them (`RequestContext::open`). Users sign
//! up through `user_management`, so they hold exactly the default user capabilities.

use std::sync::Arc;

use serde_json::Value;
use uuid::Uuid;

use crate::agent::capabilities::{AuthInfo, ListenerReach, ALL_CAPABILITY};
use crate::agent::AgentService;
use crate::api::ws_handler::{build_handler_map, HandlerMap, WsRpcError};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::{register_perspective, unregister_perspective};
use crate::test_utils::{setup_agent, setup_wallet, MultiUserMode};
use crate::types::{PerspectiveHandle, PerspectiveState, RequestContext};

pub(crate) type Session = Arc<RequestContext>;

pub(crate) struct User {
    pub email: String,
    pub did: String,
    pub session: Session,
}

/// An executor with multi-user mode on, until dropped.
pub(crate) struct Executor {
    handlers: HandlerMap,
    _multi_user: MultiUserMode,
}

impl Executor {
    pub(crate) async fn start() -> Self {
        let multi_user = MultiUserMode::on();
        setup_wallet();
        setup_agent();
        crate::prolog_service::init_prolog_service().await;
        Executor {
            handlers: build_handler_map(),
            _multi_user: multi_user,
        }
    }

    pub(crate) async fn call(
        &self,
        op: &str,
        params: Value,
        session: &Session,
    ) -> Result<Value, WsRpcError> {
        self.handlers.dispatch(op, params, session.clone()).await
    }

    /// The operator: the admin credential.
    pub(crate) fn admin(&self) -> Session {
        Arc::new(RequestContext::operator_for_test())
    }

    /// An app token of the main agent holding every capability: not the operator.
    pub(crate) fn main_agent_app(&self) -> Session {
        let auth = AuthInfo {
            app_name: "isolation-test-app".to_string(),
            app_desc: "main-agent app".to_string(),
            capabilities: Some(vec![ALL_CAPABILITY.clone()]),
            ..Default::default()
        };
        let token = crate::test_utils::granted_token(auth);
        session_for(&token)
    }

    /// A managed user with a fresh email, signed up and holding a user token.
    pub(crate) fn user(&self, name: &str) -> User {
        let email = format!("{name}-{}@isolation.test", Uuid::new_v4().simple());
        let did = crate::user_management::create_user(&email, "correct horse battery staple")
            .expect("create user");
        let token = crate::user_management::generate_user_jwt(&email, "isolation-test")
            .expect("user token");
        let session = session_for(&token);
        assert_eq!(session.user_email(), Some(email.as_str()));
        User {
            email,
            did,
            session,
        }
    }

    pub(crate) fn main_agent_did(&self) -> String {
        AgentService::with_global_instance(|agent| agent.did.clone()).expect("main agent DID")
    }
}

fn session_for(token: &str) -> Session {
    let ctx = RequestContext::open(token, &None, ListenerReach::Network)
        .unwrap_or_else(|e| panic!("the token opens a session: {e}"));
    Arc::new(ctx)
}

/// A perspective in the global registry, removed again when dropped.
pub(crate) struct RegisteredPerspective {
    pub uuid: String,
}

impl RegisteredPerspective {
    /// A private perspective owned by `owners` (`None`: a legacy unowned one).
    pub(crate) fn private(owners: Option<Vec<String>>) -> Self {
        Self::register(owners, None)
    }

    /// A perspective shared as a neighbourhood, with no link language running: signals to
    /// co-located owners still route through the executor, and anything that needs the
    /// network reports the missing link language.
    pub(crate) fn neighbourhood(owners: Option<Vec<String>>) -> Self {
        let url = format!("neighbourhood://isolation-test-{}", Uuid::new_v4());
        Self::register(owners, Some(url))
    }

    fn register(owners: Option<Vec<String>>, shared_url: Option<String>) -> Self {
        let uuid = Uuid::new_v4().to_string();
        let state = if shared_url.is_some() {
            PerspectiveState::Synced
        } else {
            PerspectiveState::Private
        };
        let handle = PerspectiveHandle {
            uuid: uuid.clone(),
            name: Some("isolation test".to_string()),
            shared_url,
            state,
            owners,
            ..Default::default()
        };
        register_perspective(uuid.clone(), PerspectiveInstance::new(handle, None));
        RegisteredPerspective { uuid }
    }

    pub(crate) async fn owners(&self) -> Option<Vec<String>> {
        crate::perspectives::get_perspective(&self.uuid)
            .expect("registered perspective")
            .persisted
            .lock()
            .await
            .owners
            .clone()
    }
}

impl Drop for RegisteredPerspective {
    fn drop(&mut self) {
        unregister_perspective(&self.uuid);
    }
}

pub(crate) fn expect_code(result: Result<Value, WsRpcError>, code: u16, what: &str) -> WsRpcError {
    match result {
        Ok(value) => panic!("{what}: expected error {code}, got Ok({value})"),
        Err(err) => {
            assert_eq!(err.code, code, "{what}: {}", err.message);
            err
        }
    }
}
