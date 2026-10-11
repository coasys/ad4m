//! Who may call a method, on whose data.
//!
//! On a multi-user node, a user session (a JWT whose `sub` names a user) acts for that
//! user only. Calls that change or use the node's own identity, wallet, files or
//! node-wide settings belong to the node's operator: the admin credential, or an app the
//! operator approved. Those calls refuse user sessions, whatever capabilities the user
//! token carries, because the default user capabilities overlap the ones these calls
//! check.
//!
//! A method declares its [`Guard`] where it registers (`map.method(..).guard(..)`), and the
//! dispatcher runs it before the handler. Every method has one: `build_handler_map` panics on
//! a method without a guard, so the executor does not start. A guard is a capability check
//! first, then an ownership rule: capabilities stay, the guard adds whose data the call may
//! touch. Two guards check no capability: [`Guard::Open`] (the handler decides) and
//! [`Guard::Operator`] (the operator holds every capability).

use crate::agent::capabilities::{check_capability, Capability};
use crate::agent::AgentContext;
use crate::api::ws_handler::WsRpcError;
use crate::types::RequestContext;
use serde_json::Value;

/// Who may call a method. The dispatcher checks the capability (403), then the rule of the
/// variant (403, or `null` for a perspective method that hides existence).
#[derive(Clone, Debug)]
pub enum Guard {
    /// No check: every caller reaches the handler, anonymous ones included, and the handler
    /// decides. For the methods that check no capability: public node facts
    /// (`runtime.info`), calls on the caller's own session (`user.logout`), stubs that answer
    /// every caller alike.
    Open,
    /// The node's operator only ([`RequestContext::is_operator`]: the admin credential, or no
    /// token on a loopback listener of a node without one). Every other caller gets 403
    /// "Admin credential required", whatever its capabilities: no app token is the operator.
    Operator,
    /// Callers without a token too: requesting a capability, signing up, logging in. The
    /// capability must be one that anonymous callers hold (`anonymous_capabilities`).
    Anonymous(Capability),
    /// Acts on the node itself. The operator, an app or an anonymous caller that holds the
    /// capability; never a user session, whatever its capabilities: the default user
    /// capabilities overlap the ones these methods check.
    Node(Capability),
    /// Acts on the caller's own agent or user record, which the handler takes from
    /// `ctx.acts_as()`. An anonymous caller has no agent of its own, so it gets 403.
    SelfAgent(Capability),
    /// Acts on the perspective that `uuid` reads from the params. The operator reaches every
    /// perspective; a user only those it owns; an app or anonymous caller the unowned ones and
    /// those the main agent owns (`helpers::can_access_perspective_with_did`). A missing
    /// perspective is 404.
    Perspective {
        /// The capability for the perspective, built from its uuid
        /// (`perspective_query_capability` and its siblings).
        capability: fn(Vec<String>) -> Capability,
        /// The perspective's uuid in the params, e.g. [`uuid_param`].
        uuid: fn(&Value) -> Option<String>,
        /// The method answers `null` for a perspective that is missing or not the caller's,
        /// so a caller cannot learn that it exists. The handler does not run.
        hides_existence: bool,
    },
    /// Acts on the AI task whose id `task_id` reads from the params. A user session reaches
    /// only the tasks it created (`tasks.user_email`): another user's task, an operator task
    /// and a missing one are all 404, so a user cannot learn which task ids exist. The
    /// operator and apps reach every task, and the handler answers for a missing one. An
    /// anonymous caller gets 403, as for [`Guard::SelfAgent`].
    Task {
        capability: Capability,
        /// The task's id in the params.
        task_id: fn(&Value) -> Option<String>,
    },
}

/// What [`Guard::Operator`] answers every other caller, as the handlers did before it
/// (`anonymous_access_tests` pins it).
pub const OPERATOR_ONLY: &str = "Admin credential required";

/// What a guard lets the dispatcher do.
#[derive(Debug, PartialEq, Eq)]
pub enum Admission {
    /// Run the handler.
    Granted,
    /// Answer `null` without running the handler: the method hides that the resource exists.
    Hidden,
}

/// The `uuid` field of the params: where most perspective methods name their perspective.
pub fn uuid_param(params: &Value) -> Option<String> {
    params.get("uuid")?.as_str().map(str::to_string)
}

impl Guard {
    /// [`Guard::Perspective`] on the perspective the `uuid` param names.
    pub fn perspective(capability: fn(Vec<String>) -> Capability) -> Guard {
        Guard::Perspective {
            capability,
            uuid: uuid_param,
            hides_existence: false,
        }
    }

    /// [`Guard::perspective`] for a method that answers `null` when the perspective is
    /// missing or not the caller's.
    pub fn perspective_or_null(capability: fn(Vec<String>) -> Capability) -> Guard {
        Guard::Perspective {
            capability,
            uuid: uuid_param,
            hides_existence: true,
        }
    }

    /// The capability the guard checks first, `None` for [`Guard::Open`] and
    /// [`Guard::Operator`]. For a perspective guard, it names the perspective `uuid`.
    pub fn capability(&self, uuid: &str) -> Option<Capability> {
        match self {
            Guard::Open | Guard::Operator => None,
            Guard::Anonymous(cap) | Guard::Node(cap) | Guard::SelfAgent(cap) => Some(cap.clone()),
            Guard::Task { capability, .. } => Some(capability.clone()),
            Guard::Perspective { capability, .. } => Some(capability(vec![uuid.to_string()])),
        }
    }

    /// Whether `ctx` may call the method with `params`: the capability, then the rule.
    pub async fn admit(
        &self,
        method: &str,
        params: &Value,
        ctx: &RequestContext,
    ) -> Result<Admission, WsRpcError> {
        let uuid = match self {
            Guard::Perspective { uuid, .. } => Some(uuid(params).ok_or_else(|| {
                WsRpcError::bad_request(format!("{method} needs the perspective's uuid"))
            })?),
            _ => None,
        };
        let task_id =
            match self {
                Guard::Task { task_id, .. } => Some(task_id(params).ok_or_else(|| {
                    WsRpcError::bad_request(format!("{method} needs the task's id"))
                })?),
                _ => None,
            };
        if let Some(capability) = self.capability(uuid.as_deref().unwrap_or_default()) {
            check_capability(&ctx.capabilities, &capability).map_err(WsRpcError::forbidden)?;
        }

        match self {
            Guard::Open | Guard::Anonymous(_) => {}
            Guard::Operator => {
                if !ctx.is_operator() {
                    return Err(WsRpcError::forbidden(OPERATOR_ONLY));
                }
            }
            Guard::Node(_) => {
                if ctx.user_email().is_some() {
                    return Err(WsRpcError::forbidden(format!(
                        "{method} acts on the node itself, so a user session may not call it"
                    )));
                }
            }
            Guard::SelfAgent(_) => {
                if !ctx.session.is_authenticated() {
                    return Err(WsRpcError::forbidden(format!(
                        "{method} acts on the caller's own agent, so it needs a token"
                    )));
                }
            }
            Guard::Task { .. } => {
                if !ctx.session.is_authenticated() {
                    return Err(WsRpcError::forbidden(format!(
                        "{method} acts on the caller's own AI task, so it needs a token"
                    )));
                }
                if let Some(email) = ctx.user_email() {
                    let id = task_id.expect("a task guard reads a task id");
                    let owner =
                        crate::db::Ad4mDb::with_global_instance(|db| db.get_task_owner(&id))
                            .map_err(|e| WsRpcError::internal(e.to_string()))?;
                    if owner.flatten().as_deref() != Some(email) {
                        return Err(WsRpcError::not_found(format!("Task {id} not found")));
                    }
                }
            }
            Guard::Perspective {
                hides_existence, ..
            } => {
                let uuid = uuid.expect("a perspective guard reads a uuid");
                match crate::api::perspectives_ws::get_perspective_with_access(&uuid, ctx).await {
                    Ok(_) => {}
                    Err(e) if *hides_existence && (e.code == 403 || e.code == 404) => {
                        return Ok(Admission::Hidden)
                    }
                    Err(e) => return Err(e),
                }
            }
        }
        Ok(Admission::Granted)
    }
}

/// The agent a session signs and publishes as: the user of a user session, otherwise the
/// node's main agent.
pub fn session_agent_context(ctx: &RequestContext) -> AgentContext {
    ctx.acts_as()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A user session for `user_email`, else an app token.
    fn ctx(user_email: Option<&str>) -> RequestContext {
        match user_email {
            Some(email) => RequestContext::user_for_test(email, "did:key:guarded-user", vec![]),
            None => RequestContext::app_for_test(vec![]),
        }
    }

    #[tokio::test]
    async fn a_node_guard_refuses_user_sessions_and_names_the_method() {
        let update = crate::agent::capabilities::AGENT_UPDATE_CAPABILITY.clone();
        let lock = Guard::Node(update.clone());
        let user = RequestContext::user_for_test(
            "alice@example.org",
            "did:key:alice",
            vec![update.clone()],
        );
        let err = lock
            .admit("agent.lock", &Value::Null, &user)
            .await
            .unwrap_err();
        assert_eq!(err.code, 403);
        assert!(err.message.contains("agent.lock"), "{}", err.message);

        let app = RequestContext::app_for_test(vec![update]);
        assert_eq!(
            lock.admit("agent.lock", &Value::Null, &app).await.unwrap(),
            Admission::Granted
        );
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
