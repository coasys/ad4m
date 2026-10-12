//! Who may call a method, on whose data.
//!
//! An executor hosts many agents. The main agent is the operator's; every other agent is a
//! user's managed agent. A session acts as one agent within a grant (its token and
//! capabilities), or is the operator (the executor's administrator role), or is anonymous.
//! Calls that change or use the executor itself (the main agent's keys, wallet, files,
//! executor-wide settings) belong to the operator and to grants on the main agent the
//! operator approved; they refuse sessions on managed agents whatever capabilities those
//! carry, because the default user capabilities overlap the ones these calls check.
//!
//! A method declares its [`Guard`] where it registers (`map.method(..).guard(..)`), and the
//! dispatcher runs it before the handler. Every method has one: `build_handler_map` panics on
//! a method without a guard, so the executor does not start. A guard is a capability check
//! first, then a rule: capabilities stay, the guard adds whose data the call may touch. Two
//! guards check no capability: [`Guard::Open`] (the handler decides) and [`Guard::Operator`]
//! (the operator holds every capability).
//!
//! Missing and not yours are one answer, 404, on every resource a guard names (perspective,
//! AI task, grant, capability request), so a caller cannot learn which ids exist. A method
//! flagged `.or_null()` (`perspective.get`, `perspective.snapshot`) answers `null` instead.

use crate::agent::capabilities::Capability;
use crate::agent::grants::Grants;
use crate::api::ws_handler::WsRpcError;
use crate::types::RequestContext;
use serde_json::Value;

/// Who may call a method. The dispatcher checks the capability (403), then the rule of the
/// variant (403 for [`Guard::Operator`] and [`Guard::Executor`], 404 for [`Guard::Owns`]).
#[derive(Clone, Debug)]
pub enum Guard {
    /// No check: every caller reaches the handler, anonymous ones included, and the handler
    /// decides. For the methods that check no capability: public executor facts
    /// (`runtime.info`) and calls on the caller's own session (`user.logout`).
    Open,
    /// The executor's operator only ([`RequestContext::is_operator`]: the admin credential, or
    /// no token on a loopback listener of an executor without one). Every other caller gets 403
    /// "Admin credential required", whatever its capabilities: no grant is the operator.
    Operator,
    /// The capability alone. Any caller that holds it: an anonymous caller only for the
    /// capabilities of the anonymous set (requesting a capability, signing up, logging in);
    /// a grant for its agent, which the handler takes from `ctx.acts_as()`.
    Cap(Capability),
    /// Acts on the executor itself: the capability, then the operator or a grant on the main
    /// agent. Never a session on a managed agent, whatever its capabilities, nor an anonymous
    /// caller. A grant on the main agent passes: an app the operator approved administers
    /// the executor (WE web holds `*` on a local executor).
    Executor(Capability),
    /// Acts on one resource that `id` reads from the params: the capability, then the owner
    /// rule. The operator reaches every resource; any other session only those its agent
    /// owns ([`Owner`]). Missing and not yours are both 404.
    Owns {
        /// The capability, built from the resource's id (a perspective's capability names
        /// its uuid; the others ignore it).
        cap: fn(Vec<String>) -> Capability,
        /// Where the params hold the resource's id, e.g. [`UUID_PARAM`]. Missing is 400.
        id: IdParam,
        owner: Owner,
    },
}

/// Where a [`Guard::Owns`] reads its resource's id in the params. A description, not a
/// function, so the generated guard table in `api/AGENTS.md` shows the field the guard
/// reads, and moving the id to another field changes the table.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IdParam {
    /// A string field, reached by this path of keys: `["uuid"]`, `["task", "taskId"]`.
    Field(&'static [&'static str]),
    /// The first of these string fields that is present: one param under several names.
    FirstOf(&'static [&'static str]),
    /// The string field `field` of the JSON object that the string param `param` holds.
    InJson {
        param: &'static str,
        field: &'static str,
    },
}

impl IdParam {
    /// The id in `params`, `None` when the field is missing or not a string.
    pub fn read(self, params: &Value) -> Option<String> {
        let string = |value: &Value| value.as_str().map(str::to_string);
        match self {
            IdParam::Field(path) => {
                string(path.iter().try_fold(params, |value, key| value.get(*key))?)
            }
            IdParam::FirstOf(names) => names
                .iter()
                .find_map(|name| params.get(*name))
                .and_then(string),
            IdParam::InJson { param, field } => {
                let inner: Value = serde_json::from_str(params.get(param)?.as_str()?).ok()?;
                string(inner.get(field)?)
            }
        }
    }
}

/// How the guard table names the field: `uuid`, `task.taskId`, `perspectiveUuid|perspectiveUUID`,
/// `auth(JSON).requestId`.
impl std::fmt::Display for IdParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IdParam::Field(path) => write!(f, "{}", path.join(".")),
            IdParam::FirstOf(names) => write!(f, "{}", names.join("|")),
            IdParam::InJson { param, field } => write!(f, "{param}(JSON).{field}"),
        }
    }
}

/// The kind of resource a [`Guard::Owns`] names, and how its owning agent is found.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Owner {
    /// A perspective: its owners (DIDs); one without owners is the main agent's
    /// (`helpers::can_access_perspective_with_did`).
    Perspective,
    /// An AI task: its `agent_did`; none is the main agent's.
    Task,
    /// A grant, named by its token or its request id (`Grants::find`): the grant's agent.
    Grant,
    /// A pending capability request (`PendingRequests`): the agent it asks to act as (D12).
    Request,
}

impl Owner {
    fn noun(self) -> &'static str {
        match self {
            Owner::Perspective => "perspective",
            Owner::Task => "AI task",
            Owner::Grant => "app",
            Owner::Request => "capability request",
        }
    }

    /// The 404 for `id`, missing or not the caller's. A grant's id may be its token, so the
    /// answer does not repeat it.
    fn not_found(self, id: &str) -> WsRpcError {
        WsRpcError::not_found(match self {
            Owner::Perspective => format!("Perspective {id} not found"),
            Owner::Task => format!("Task {id} not found"),
            Owner::Grant => {
                "App not found: no app of yours matches this token or request id".to_string()
            }
            Owner::Request => "Capability request not found: it may have expired. The app must \
                               request access again"
                .to_string(),
        })
    }
}

/// What [`Guard::Operator`] answers every other caller, as the handlers did before it
/// (`anonymous_access_tests` pins it).
pub const OPERATOR_ONLY: &str = "Admin credential required";

/// The `uuid` field of the params: where most perspective methods name their perspective.
pub const UUID_PARAM: IdParam = IdParam::Field(&["uuid"]);

impl Guard {
    /// [`Guard::Owns`] on the perspective the `uuid` param names.
    pub fn perspective(cap: fn(Vec<String>) -> Capability) -> Guard {
        Guard::Owns {
            cap,
            id: UUID_PARAM,
            owner: Owner::Perspective,
        }
    }

    /// The capability the guard checks first, `None` for [`Guard::Open`] and
    /// [`Guard::Operator`]. For [`Guard::Owns`], built from the resource's `id`.
    pub fn capability(&self, id: &str) -> Option<Capability> {
        match self {
            Guard::Open | Guard::Operator => None,
            Guard::Cap(cap) | Guard::Executor(cap) => Some(cap.clone()),
            Guard::Owns { cap, .. } => Some(cap(vec![id.to_string()])),
        }
    }

    /// Whether `ctx` may call the method with `params`: the capability, then the rule.
    pub async fn admit(
        &self,
        method: &str,
        params: &Value,
        ctx: &RequestContext,
    ) -> Result<(), WsRpcError> {
        let resource = match self {
            Guard::Owns { id, owner, .. } => Some((
                *owner,
                id.read(params).ok_or_else(|| {
                    WsRpcError::bad_request(format!("{method} needs the {}'s id", owner.noun()))
                })?,
            )),
            _ => None,
        };
        let id = resource.as_ref().map(|(_, id)| id.as_str());
        if let Some(capability) = self.capability(id.unwrap_or_default()) {
            ctx.require(&capability).map_err(WsRpcError::forbidden)?;
        }

        match self {
            Guard::Open | Guard::Cap(_) => Ok(()),
            Guard::Operator if ctx.is_operator() => Ok(()),
            Guard::Operator => Err(WsRpcError::forbidden(OPERATOR_ONLY)),
            Guard::Executor(_) if !ctx.session.is_authenticated() => Err(WsRpcError::forbidden(
                format!("{method} acts on the executor itself, so it needs a token"),
            )),
            Guard::Executor(_) if !ctx.acts_as().is_main_agent() => Err(WsRpcError::forbidden(
                format!("{method} acts on the executor itself, so a user session may not call it"),
            )),
            Guard::Executor(_) => Ok(()),
            Guard::Owns { .. } => {
                let (owner, id) = resource.expect("an owner guard reads an id");
                owns(ctx, owner, &id).await
            }
        }
    }
}

/// The owner rule of [`Guard::Owns`]: `Ok` if the resource `id` exists and the operator or
/// the agent `ctx` acts as owns it; 404 otherwise, missing or not, so a caller cannot learn
/// which ids exist. An anonymous caller owns nothing.
pub async fn owns(ctx: &RequestContext, owner: Owner, id: &str) -> Result<(), WsRpcError> {
    let not_found = || owner.not_found(id);
    let agent = ctx.acts_as();
    let owned = match owner {
        // One rule for the guard and for the handlers that name a perspective in a param
        // they read themselves (`runtime.exportData`).
        Owner::Perspective => {
            return crate::api::perspectives_ws::get_perspective_with_access(id, ctx)
                .await
                .map(|_| ())
        }
        Owner::Task => {
            let task_owner = crate::db::Ad4mDb::with_global_instance(|db| db.get_task_owner(id))
                .map_err(|e| WsRpcError::internal(e.to_string()))?;
            // The outer `None`: no such task. The inner one: a task of the main agent.
            let Some(task_owner) = task_owner else {
                return Err(not_found());
            };
            task_owner.as_deref() == agent.managed_did()
        }
        Owner::Grant => {
            let grants = Grants::global().find(id);
            if grants.is_empty() {
                return Err(not_found());
            }
            grants.iter().all(|grant| grant.agent == agent)
        }
        Owner::Request => match crate::agent::capabilities::request_target(id) {
            None => return Err(not_found()),
            // The same DID; a request for an email without an agent (`NoAgent`) is the
            // operator's alone.
            Some(target) => target.agent() == Some(&agent),
        },
    };
    if ctx.is_operator() || (ctx.session.is_authenticated() && owned) {
        Ok(())
    } else {
        Err(not_found())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn an_executor_guard_refuses_user_sessions_and_names_the_method() {
        let update = crate::agent::capabilities::AGENT_UPDATE_CAPABILITY.clone();
        let lock = Guard::Executor(update.clone());
        let user = RequestContext::user_for_test("did:key:alice", vec![update.clone()]);
        let err = lock
            .admit("agent.lock", &Value::Null, &user)
            .await
            .unwrap_err();
        assert_eq!(
            (err.code, err.message.as_str()),
            (
                403,
                "agent.lock acts on the executor itself, so a user session may not call it"
            )
        );

        let app = RequestContext::app_for_test(vec![update]);
        assert!(lock.admit("agent.lock", &Value::Null, &app).await.is_ok());
    }

    #[test]
    fn an_id_param_reads_the_field_it_names() {
        use serde_json::json;
        let nested = IdParam::Field(&["task", "taskId"]);
        assert_eq!(
            nested.read(&json!({ "task": { "taskId": "t1" } })),
            Some("t1".into())
        );
        assert_eq!(nested.read(&json!({ "taskId": "t1" })), None);
        assert_eq!(nested.read(&json!({ "task": { "taskId": 7 } })), None);

        let aliases = IdParam::FirstOf(&["perspectiveUuid", "perspectiveUUID"]);
        assert_eq!(
            aliases.read(&json!({ "perspectiveUUID": "p" })),
            Some("p".into())
        );
        assert_eq!(
            aliases.read(&json!({ "perspectiveUuid": "a", "perspectiveUUID": "b" })),
            Some("a".into())
        );

        let in_json = IdParam::InJson {
            param: "auth",
            field: "requestId",
        };
        let auth = json!({ "requestId": "r1", "auth": {} }).to_string();
        assert_eq!(in_json.read(&json!({ "auth": auth })), Some("r1".into()));
        assert_eq!(in_json.read(&json!({ "auth": "not json" })), None);

        assert_eq!(UUID_PARAM.to_string(), "uuid");
        assert_eq!(nested.to_string(), "task.taskId");
        assert_eq!(aliases.to_string(), "perspectiveUuid|perspectiveUUID");
        assert_eq!(in_json.to_string(), "auth(JSON).requestId");
    }
}
