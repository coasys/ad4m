//! Who a session acts as, and guards for calls that act on the node itself.
//!
//! On a multi-user node, a user session (a JWT whose `sub` names a user) acts for that
//! user only. Calls that change or use the node's own identity, wallet, files or
//! node-wide settings belong to the node's operator: the admin credential, or an app the
//! operator approved. Those calls refuse user sessions, whatever capabilities the user
//! token carries, because the default user capabilities overlap the ones these calls
//! check.

use crate::agent::AgentContext;
use crate::api::ws_handler::WsRpcError;
use crate::types::RequestContext;

/// Refuses the call for a multi-user user session. `what` names the call in the error.
pub fn refuse_user_session(ctx: &RequestContext, what: &str) -> Result<(), WsRpcError> {
    if ctx.user_email.is_some() {
        return Err(WsRpcError::forbidden(format!(
            "{what} acts on the node itself, so a user session may not call it"
        )));
    }
    Ok(())
}

/// The agent a session signs and publishes as: the user of a user session, otherwise the
/// node's main agent.
pub fn session_agent_context(ctx: &RequestContext) -> AgentContext {
    match ctx.user_email.clone() {
        Some(user_email) => AgentContext::for_user_email(user_email),
        None => AgentContext::main_agent(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ctx(user_email: Option<&str>) -> RequestContext {
        RequestContext {
            capabilities: Ok(vec![]),
            auto_permit_cap_requests: false,
            auth_token: String::new(),
            is_admin_credential: false,
            user_email: user_email.map(String::from),
            user_did: None,
            cancel_token: None,
        }
    }

    #[test]
    fn user_sessions_get_refused() {
        let err = refuse_user_session(&ctx(Some("alice@example.org")), "agent.lock").unwrap_err();
        assert_eq!(err.code, 403);
        assert!(err.message.contains("agent.lock"));
    }

    #[test]
    fn operator_sessions_pass() {
        assert!(refuse_user_session(&ctx(None), "agent.lock").is_ok());
    }

    #[test]
    fn a_user_session_acts_as_its_user() {
        assert_eq!(
            session_agent_context(&ctx(Some("alice@example.org"))),
            AgentContext::for_user_email("alice@example.org".to_string())
        );
    }

    #[test]
    fn an_operator_session_acts_as_the_main_agent() {
        assert_eq!(
            session_agent_context(&ctx(None)),
            AgentContext::main_agent()
        );
    }
}
