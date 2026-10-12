//! Guards for calls that act on the executor itself.
//!
//! On a multi-user executor, a session on a managed agent (a JWT whose `sub` names a user)
//! acts for that user only. Calls that change or use the main agent's identity, wallet,
//! files or executor-wide settings belong to the operator: the admin credential, or a grant
//! on the main agent the operator approved. Those calls refuse sessions on managed agents,
//! whatever capabilities the user token carries, because the default user capabilities
//! overlap the ones these calls check.

use crate::api::ws_handler::WsRpcError;
use crate::types::RequestContext;

/// Refuses the call for a session on a managed agent. `what` names the call in the error.
pub fn refuse_user_session(ctx: &RequestContext, what: &str) -> Result<(), WsRpcError> {
    if ctx.user_email().is_some() {
        return Err(WsRpcError::forbidden(format!(
            "{what} acts on the node itself, so a user session may not call it"
        )));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn user_sessions_get_refused() {
        let user =
            RequestContext::user_for_test("alice@example.org", "did:key:guarded-user", vec![]);
        let err = refuse_user_session(&user, "agent.lock").unwrap_err();
        assert_eq!(err.code, 403);
        assert!(err.message.contains("agent.lock"));
    }

    #[test]
    fn main_agent_sessions_pass() {
        assert!(refuse_user_session(&RequestContext::app_for_test(vec![]), "agent.lock").is_ok());
        assert!(refuse_user_session(&RequestContext::operator_for_test(), "agent.lock").is_ok());
    }
}
