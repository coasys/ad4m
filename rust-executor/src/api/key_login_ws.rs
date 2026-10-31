//! Login with a device key and the calls of key sessions: `user.sessionChallenge`,
//! `user.loginWithKey`, `user.renewSession`, `user.currentSession`, `user.endAllSessions` and
//! `agent.signSessionStatement`. The work is in `identity::key_login`.

use std::sync::Arc;

use serde_json::Value;

use super::guards::Guard;
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};
use crate::agent::capabilities::AGENT_AUTH_CAPABILITY;
use crate::identity::key_login;
use crate::identity::types::{
    EndAllSessionsParams, EndedSessions, KeySessionResult, LoginWithKeyParams, RenewSessionParams,
    RenewedSession, SessionChallenge, SessionInfo, SignSessionStatementParams, SignedKeyStatement,
};
use crate::types::RequestContext;

fn parse<T: serde::de::DeserializeOwned>(params: Value) -> Result<T, WsRpcError> {
    serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))
}

fn answer<T: serde::Serialize>(value: T) -> Result<Value, WsRpcError> {
    Ok(serde_json::to_value(value)?)
}

fn now() -> u64 {
    crate::agent::grants::now()
}

async fn session_challenge(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let service = crate::identity::service()?;
    answer(key_login::challenge(&service, now())?)
}

async fn login_with_key(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let params: LoginWithKeyParams = parse(params)?;
    let service = crate::identity::service()?;
    answer(key_login::login(&service, params, now()).await?)
}

async fn renew_session(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let params: RenewSessionParams = parse(params)?;
    // A session that no key login opened has nothing to renew, whatever the module's state.
    key_login::require_key_session(&ctx.session)?;
    let service = crate::identity::service()?;
    answer(key_login::renew(&service, &ctx.session, params.proof, now()).await?)
}

async fn current_session(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    answer(key_login::session_info(&ctx.session))
}

async fn end_all_sessions(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let params: EndAllSessionsParams = parse(params)?;
    answer(key_login::end_all_sessions(
        &ctx.session,
        params.keep_current,
        params.before,
    )?)
}

async fn sign_session_statement(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let params: SignSessionStatementParams = parse(params)?;
    answer(key_login::sign_statement(&ctx.session, params.statement)?)
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    // A challenge for a key login. AGENT_AUTH, which every caller holds (the anonymous set
    // included, in single-user and multi-user mode alike): every executor answers a key login.
    map.method::<NoParams, SessionChallenge>("user.sessionChallenge", session_challenge)
        .guard(Guard::Cap(AGENT_AUTH_CAPABILITY.clone()));
    // A login signed by a device key: the handler checks the statement, the key and the role
    // (operator record, or the local agent that speaks for the identity) and refuses with one
    // stable reason. AGENT_AUTH, as `user.sessionChallenge`.
    map.method::<LoginWithKeyParams, KeySessionResult>("user.loginWithKey", login_with_key)
        .guard(Guard::Cap(AGENT_AUTH_CAPABILITY.clone()));
    // Open: a key session renews itself whatever its capabilities. The handler requires a
    // key-bound grant and a statement of its own key naming it; 400 for any other session.
    map.method::<RenewSessionParams, RenewedSession>("user.renewSession", renew_session)
        .guard(Guard::Open);
    // Open: answers for the caller's own session, anonymous included.
    map.method::<NoParams, SessionInfo>("user.currentSession", current_session)
        .read()
        .guard(Guard::Open);
    // Open: log out everywhere ends the caller's own sessions whatever its capabilities. The
    // handler requires a key-bound session or a session on a managed agent; 400 otherwise.
    map.method::<EndAllSessionsParams, EndedSessions>("user.endAllSessions", end_all_sessions)
        .guard(Guard::Open);
    // Signs a native key statement as this executor's device. The handler admits only the
    // admin credential and the loopback caller (the shell on this machine): an operator from a
    // key login gets 403 local-only.
    map.method::<SignSessionStatementParams, SignedKeyStatement>(
        "agent.signSessionStatement",
        sign_session_statement,
    )
    .guard(Guard::Operator);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::session::{Principal, Session};
    use crate::agent::AgentContext;

    /// Each call carries its guard: the challenge and the login AGENT_AUTH (the anonymous set),
    /// the calls on the caller's own session `Open`, the statement signature the operator.
    #[test]
    fn every_key_session_call_has_its_guard() {
        let mut map = HandlerMap::new();
        register_ws_handlers(&mut map);
        let auth = format!("{:?}", Guard::Cap(AGENT_AUTH_CAPABILITY.clone()));
        let open = format!("{:?}", Guard::Open);
        let expected = [
            (
                "agent.signSessionStatement",
                format!("{:?}", Guard::Operator),
            ),
            ("user.currentSession", open.clone()),
            ("user.endAllSessions", open.clone()),
            ("user.loginWithKey", auth.clone()),
            ("user.renewSession", open),
            ("user.sessionChallenge", auth),
        ];
        let got: Vec<(String, String)> = map
            .guards()
            .into_iter()
            .map(|(name, guard)| (name.to_string(), format!("{:?}", guard.expect("a guard"))))
            .collect();
        let want: Vec<(String, String)> = expected
            .iter()
            .map(|(name, guard)| (name.to_string(), guard.clone()))
            .collect();
        assert_eq!(got, want);
    }

    /// An anonymous caller passes the guards of the challenge and the login in single-user mode
    /// (multi-user off), and reads its own session as anonymous; the admin credential has no
    /// identity to end every session of.
    #[tokio::test]
    async fn an_anonymous_caller_reaches_the_key_login_calls() {
        crate::test_utils::ensure_test_db();
        let map = crate::api::ws_handler::build_handler_map();
        let anonymous = || {
            RequestContext::for_session(Session::for_test(Principal::Anonymous).with_caps(
                crate::agent::capabilities::anonymous_capabilities_when(false),
            ))
        };
        let statement = serde_json::json!({
            "v": 1, "purpose": "login", "scid": "did:scid:coasys:1:QmS", "key": "z6Mk",
            "executor": "did:key:z6Mk", "challenge": "c", "client": {"kind": "native"},
            "issuedAt": 1
        });
        let login = serde_json::json!({
            "proof": {"statement": statement, "sig": "z"},
            "app": {"appName": "a", "appDesc": "", "appDomain": ""}
        });
        for (method, params) in [
            ("user.sessionChallenge", serde_json::json!({})),
            ("user.loginWithKey", login),
        ] {
            let admitted = map.admit(method, &params, &anonymous()).await;
            assert!(admitted.is_ok(), "{method}: {admitted:?}");
        }
        let reply = map
            .dispatch(
                "user.currentSession",
                serde_json::json!({}),
                Arc::new(anonymous()),
            )
            .await
            .unwrap();
        assert_eq!(reply["role"], "anonymous");

        let refused = map
            .dispatch(
                "user.endAllSessions",
                serde_json::json!({"keepCurrent": true}),
                Arc::new(RequestContext::operator_for_test()),
            )
            .await
            .unwrap_err();
        assert_eq!(refused.code, 400);
        // A user's session that no key login opened renews nothing.
        let user = RequestContext::for_test(Principal::Agent(AgentContext::managed(
            "did:key:z6MkUser".into(),
        )));
        let renew = serde_json::json!({"proof": {"statement": statement_of_renew(), "sig": "z"}});
        let refused = map
            .dispatch("user.renewSession", renew, Arc::new(user))
            .await;
        assert!(
            matches!(refused, Err(WsRpcError { code: 400, .. })),
            "{refused:?}"
        );
    }

    fn statement_of_renew() -> Value {
        serde_json::json!({
            "v": 1, "purpose": "renew", "scid": "did:scid:coasys:1:QmS", "key": "z6Mk",
            "executor": "did:key:z6Mk", "challenge": "c", "client": {"kind": "native"},
            "grant": "g", "issuedAt": 1
        })
    }
}
