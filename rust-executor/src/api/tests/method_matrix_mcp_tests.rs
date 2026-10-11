//! The method matrix through MCP: every MCP tool that wraps an RPC method, run as each matrix
//! caller on each matrix perspective, allows and refuses exactly as its methods' guards do
//! (#851).
//!
//! Each row runs the tool through its MCP entry point, with the caller's session in the MCP
//! session, and asks the shared handler map's guard (`HandlerMap::admit`) about the same
//! session and the params the tool sends. A tool that calls several methods is allowed when
//! every one of them is. The tools that obtain a session (the login tools, `generate_jwt`)
//! call as a caller without a token, so their rows expect that caller's answer, whoever holds
//! the MCP session. A tool reads as refused when it answers a status of 401, 403 or 404
//! (`mcp::tools::method_error`); a login tool refused by the guard names the multi-user mode.
//!
//! [`every_tool_is_classified`] fails for a new tool that is in neither list, so a tool that
//! wraps an RPC method cannot skip the matrix.
//!
//! The tools with no RPC method that take a perspective (`NOT_WRAPPED`, and the dynamic
//! per-class tools) run the same rows against the RPC rule for reading or changing a
//! perspective, `Guard::perspective(perspective_query_capability | perspective_update_capability)`
//! ([`every_perspective_tool_without_a_method_follows_the_perspective_rule`]).

use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use rmcp::handler::server::wrapper::Parameters;
use serde_json::{json, Value};

use super::method_matrix_tests::{
    perspective_rows, resource_rows, Caller, Target, World, OTHER_USER_DID, USER_DID, USER_EMAIL,
};
use crate::agent::capabilities::{
    anonymous_capabilities_when, perspective_query_capability, perspective_update_capability,
    Capability, ListenerReach,
};
use crate::agent::session::{Principal, Session, TokenId};
use crate::api::guards::{Admission, Guard};
use crate::api::ws_handler::shared_handler_map;
use crate::mcp::tools::test_support::{handler_as, status, RuntimeService};
use crate::mcp::tools::{auth, flows, instances, languages, neighbourhoods, perspectives};
use crate::mcp::tools::{
    perspective_error, profiles, request_context, subscriptions, Ad4mMcpHandler,
};

type Answer = Pin<Box<dyn Future<Output = String>>>;

/// How a tool calls its methods.
#[derive(Clone, Copy, PartialEq)]
enum Calls {
    /// As the MCP session's caller.
    AsCaller,
    /// As a caller without a token, whoever holds the MCP session.
    WithoutToken,
}

/// An MCP tool that wraps RPC methods.
struct Tool {
    name: &'static str,
    calls: Calls,
    /// The methods it calls, each with the params it sends for a target perspective.
    methods: Vec<Method>,
    /// Runs the tool on a target perspective.
    run: Run,
    /// `false` for a tool whose allowed path needs what a unit test node lacks (a language
    /// download): only its refused rows run it; its allowed rows check its guard step.
    runs_when_allowed: bool,
    /// Whether the answer is a refusal.
    refused: fn(&str) -> bool,
}

/// A refusal of a method that the tool called ([`crate::mcp::tools::method_error`]).
fn by_status(answer: &str) -> bool {
    serde_json::from_str::<Value>(answer)
        .ok()
        .and_then(|v| v.get("status")?.as_u64())
        .is_some_and(|status| matches!(status, 401 | 403 | 404))
}

/// A login tool's guard refusal: callers without a token hold the user-management
/// capabilities only in multi-user mode, and the tool names the mode.
fn by_mode(answer: &str) -> bool {
    serde_json::from_str::<Value>(answer)
        .ok()
        .is_some_and(|v| v["error"] == "Multi-user mode is not enabled")
}

const SHAPE: &str = r#"{"target_class":"matrix://C","properties":[]}"#;
const COMMANDS: &str =
    r#"[{"action":"addLink","source":"this","predicate":"matrix://p","target":"matrix://t"}]"#;

fn link_params(uuid: &str) -> Value {
    json!({
        "uuid": uuid,
        "link": { "source": "matrix://s", "predicate": "matrix://p", "target": "matrix://t" },
        "status": "shared",
    })
}

fn query_params(uuid: &str) -> Value {
    json!({ "uuid": uuid, "source": "matrix://s", "predicate": null, "target": null })
}

fn none(_: &str) -> Value {
    json!({})
}

fn update_profile(_: &str) -> Value {
    json!({ "publicPerspective": { "links": [] } })
}

type Method = (&'static str, fn(&str) -> Value);

/// A method a tool calls, with the params it sends for a target perspective.
fn m(method: &'static str, params: fn(&str) -> Value) -> Method {
    (method, params)
}

type Run = fn(Ad4mMcpHandler, String) -> Answer;

fn tools() -> Vec<Tool> {
    use Calls::*;
    let tool = |name: &'static str, calls: Calls, methods: Vec<Method>, run: Run| Tool {
        name,
        calls,
        methods,
        run,
        runs_when_allowed: true,
        refused: by_status,
    };
    let login =
        |name: &'static str, method: &'static str, params: fn(&str) -> Value, run: Run| Tool {
            refused: by_mode,
            ..tool(name, WithoutToken, vec![m(method, params)], run)
        };
    vec![
        tool(
            "list_perspectives",
            AsCaller,
            vec![m("perspective.all", none)],
            |h, _| {
                Box::pin(async move {
                    h.list_perspectives(Parameters(perspectives::ListPerspectivesParams {}))
                        .await
                })
            },
        ),
        tool(
            "add_perspective",
            AsCaller,
            vec![m("perspective.create", |_| json!({ "name": "matrix" }))],
            |h, _| {
                Box::pin(async move {
                    h.add_perspective(Parameters(perspectives::AddPerspectiveParams {
                        name: "matrix".into(),
                    }))
                    .await
                })
            },
        ),
        tool(
            "add_link",
            AsCaller,
            vec![m("perspective.addLink", link_params)],
            |h, uuid| {
                Box::pin(async move {
                    h.add_link(Parameters(perspectives::AddLinkParams {
                        perspective_id: uuid,
                        source: "matrix://s".into(),
                        predicate: "matrix://p".into(),
                        target: "matrix://t".into(),
                    }))
                    .await
                })
            },
        ),
        tool(
            "query_links",
            AsCaller,
            vec![m("perspective.queryLinks", query_params)],
            |h, uuid| {
                Box::pin(async move {
                    h.query_links(Parameters(perspectives::QueryLinksParams {
                        perspective_id: uuid,
                        source: Some("matrix://s".into()),
                        predicate: None,
                        target: None,
                    }))
                    .await
                })
            },
        ),
        tool(
            "add_model",
            AsCaller,
            vec![m(
                "perspective.addSdna",
                |uuid| json!({ "uuid": uuid, "name": "C", "sdnaType": "subject_class", "shaclJson": SHAPE }),
            )],
            |h, uuid| {
                Box::pin(async move {
                    h.add_model(Parameters(perspectives::AddModelParams {
                        perspective_id: uuid,
                        class_name: "C".into(),
                        shacl_json: SHAPE.into(),
                    }))
                    .await
                })
            },
        ),
        tool(
            "add_flow",
            AsCaller,
            vec![m(
                "perspective.addSdna",
                |uuid| json!({ "uuid": uuid, "name": "F", "sdnaType": "flow", "shaclJson": "{}" }),
            )],
            |h, uuid| {
                Box::pin(async move {
                    h.add_flow(Parameters(flows::AddFlowParams {
                        perspective_id: uuid,
                        flow_name: "F".into(),
                        shacl_json: "{}".into(),
                    }))
                    .await
                })
            },
        ),
        tool(
            "get_flows",
            AsCaller,
            vec![m("perspective.queryLinks", query_params)],
            |h, uuid| {
                Box::pin(async move {
                    h.get_flows(Parameters(flows::GetFlowsParams {
                        perspective_id: uuid,
                    }))
                    .await
                })
            },
        ),
        tool(
            "flow_proposal_accept",
            AsCaller,
            vec![m(
                "perspective.acceptFlowProposal",
                |uuid| json!({ "uuid": uuid, "proposalUri": "matrix://proposal" }),
            )],
            |h, uuid| {
                Box::pin(async move {
                    h.flow_proposal_accept(Parameters(flows::FlowProposalParams {
                        perspective_id: uuid,
                        proposal_uri: "matrix://proposal".into(),
                    }))
                    .await
                })
            },
        ),
        tool(
            "flow_proposal_reject",
            AsCaller,
            vec![m(
                "perspective.rejectFlowProposal",
                |uuid| json!({ "uuid": uuid, "proposalUri": "matrix://proposal" }),
            )],
            |h, uuid| {
                Box::pin(async move {
                    h.flow_proposal_reject(Parameters(flows::FlowProposalParams {
                        perspective_id: uuid,
                        proposal_uri: "matrix://proposal".into(),
                    }))
                    .await
                })
            },
        ),
        tool(
            "flow_valid_outputs",
            AsCaller,
            vec![m(
                "perspective.flowValidOutputs",
                |uuid| json!({ "uuid": uuid, "flow": "F", "state": null }),
            )],
            |h, uuid| {
                Box::pin(async move {
                    h.flow_valid_outputs(Parameters(flows::FlowValidOutputsParams {
                        perspective_id: uuid,
                        flow_name: "F".into(),
                        state: None,
                    }))
                    .await
                })
            },
        ),
        tool(
            "execute_commands",
            AsCaller,
            vec![m(
                "perspective.executeCommands",
                |uuid| json!({ "uuid": uuid, "commands": COMMANDS, "expression": "matrix://e", "parameters": null }),
            )],
            |h, uuid| {
                Box::pin(async move {
                    h.execute_commands(Parameters(instances::ExecuteCommandsParams {
                        perspective_id: uuid,
                        commands: COMMANDS.into(),
                        expression_address: "matrix://e".into(),
                        parameters: None,
                    }))
                    .await
                })
            },
        ),
        tool(
            "add_child",
            AsCaller,
            vec![m("perspective.addLink", link_params)],
            |h, uuid| {
                Box::pin(async move {
                    h.add_child(Parameters(instances::AddChildParams {
                        perspective_id: uuid,
                        parent: "matrix://s".into(),
                        child: "matrix://t".into(),
                    }))
                    .await
                })
            },
        ),
        tool(
            "get_children",
            AsCaller,
            vec![m("perspective.queryLinks", query_params)],
            |h, uuid| {
                Box::pin(async move {
                    h.get_children(Parameters(instances::GetChildrenParams {
                        perspective_id: uuid,
                        parent: "matrix://s".into(),
                        limit: None,
                    }))
                    .await
                })
            },
        ),
        tool(
            "language_meta",
            AsCaller,
            vec![m("language.meta", |_| json!({ "address": "QmMatrix" }))],
            |h, _| {
                Box::pin(async move {
                    h.language_meta(Parameters(languages::LanguageMetaParams {
                        address: "QmMatrix".into(),
                    }))
                    .await
                })
            },
        ),
        tool(
            "list_link_language_templates",
            AsCaller,
            vec![m("runtime.linkLanguageTemplates", none)],
            |h, _| Box::pin(async move { h.list_link_language_templates().await }),
        ),
        tool(
            "neighbourhood_publish_from_perspective",
            AsCaller,
            vec![
                m(
                    "neighbourhood.publish",
                    |uuid| json!({ "perspectiveUuid": uuid, "linkLanguage": "QmMatrix", "meta": { "links": [] } }),
                ),
                m(
                    "language.applyTemplate",
                    |_| json!({ "sourceLanguageHash": "QmMatrix", "templateData": r#"{"name":"matrix"}"# }),
                ),
            ],
            |h, uuid| {
                Box::pin(async move {
                    h.neighbourhood_publish_from_perspective(Parameters(
                        neighbourhoods::NeighbourhoodPublishParams {
                            perspective_uuid: uuid,
                            link_language: "QmMatrix".into(),
                            name: "matrix".into(),
                        },
                    ))
                    .await
                })
            },
        ),
        tool(
            "neighbourhood_join_from_url",
            AsCaller,
            vec![m("neighbourhood.join", |_| json!({ "url": "" }))],
            |h, _| {
                Box::pin(async move {
                    h.neighbourhood_join_from_url(Parameters(
                        neighbourhoods::NeighbourhoodJoinParams { url: "".into() },
                    ))
                    .await
                })
            },
        ),
        tool(
            "get_my_did",
            AsCaller,
            vec![m("agent.get", none)],
            |h, _| {
                Box::pin(async move {
                    h.get_my_did(Parameters(profiles::GetAgentProfileParams {}))
                        .await
                })
            },
        ),
        tool(
            "get_agent_profile",
            AsCaller,
            vec![m("agent.get", none)],
            |h, _| {
                Box::pin(async move {
                    h.get_agent_profile(Parameters(profiles::GetAgentProfileParams {}))
                        .await
                })
            },
        ),
        tool(
            "get_agent_public_perspective",
            AsCaller,
            vec![m(
                "agent.byDid",
                |_| json!({ "did": "did:key:matrix-stranger" }),
            )],
            |h, _| {
                Box::pin(async move {
                    h.get_agent_public_perspective(Parameters(
                        profiles::GetAgentPublicPerspectiveParams {
                            did: Some("did:key:matrix-stranger".into()),
                        },
                    ))
                    .await
                })
            },
        ),
        tool(
            "set_agent_profile",
            AsCaller,
            vec![
                m("agent.get", none),
                m("agent.updateProfile", update_profile),
            ],
            |h, _| {
                Box::pin(async move {
                    h.set_agent_profile(Parameters(profiles::SetAgentProfileParams {
                        username: Some("matrix".into()),
                        given_name: None,
                        family_name: None,
                        email: None,
                        bio: None,
                    }))
                    .await
                })
            },
        ),
        tool(
            "set_agent_public_perspective",
            AsCaller,
            vec![m("agent.updateProfile", update_profile)],
            |h, _| {
                Box::pin(async move {
                    h.set_agent_public_perspective(Parameters(
                        profiles::SetAgentPublicPerspectiveParams {
                            links_json: "[]".into(),
                        },
                    ))
                    .await
                })
            },
        ),
        Tool {
            runs_when_allowed: false,
            ..tool(
                "set_agent_profile_picture",
                AsCaller,
                vec![
                    m(
                        "expression.create",
                        |_| json!({ "languageAddress": "QmMatrix", "content": "{}" }),
                    ),
                    m("agent.get", none),
                    m("agent.updateProfile", update_profile),
                ],
                |h, _| {
                    Box::pin(async move {
                        h.set_agent_profile_picture(Parameters(
                            profiles::SetAgentProfilePictureParams {
                                image_base64: "AA==".into(),
                                mime_type: None,
                            },
                        ))
                        .await
                    })
                },
            )
        },
        // The request is created as a caller without a token; the inline code is
        // `agent.permitCapability` as the caller, so only that method decides the row.
        Tool {
            refused: |answer| {
                serde_json::from_str::<Value>(answer).map_or(true, |v| !v["code"].is_string())
            },
            ..tool(
                "request_capability",
                AsCaller,
                vec![m("agent.permitCapability", |_| json!({ "auth": "{}" }))],
                |h, _| {
                    Box::pin(async move {
                        h.request_capability_as(
                            crate::mcp::tools::RequestAuth::default(),
                            auth::RequestCapabilityParams {
                                app_name: "matrix".into(),
                                app_desc: "d".into(),
                                app_domain: None,
                                app_url: None,
                            },
                        )
                        .await
                    })
                },
            )
        },
        tool(
            "generate_jwt",
            WithoutToken,
            vec![m(
                "agent.generateJwt",
                |_| json!({ "requestId": "matrix-request", "rand": "000000" }),
            )],
            |h, _| {
                Box::pin(async move {
                    h.generate_jwt(Parameters(auth::GenerateJwtParams {
                        request_id: "matrix-request".into(),
                        code: "000000".into(),
                    }))
                    .await
                })
            },
        ),
        login(
            "login_email",
            "user.login",
            |_| json!({ "email": "matrix@example.org", "password": "p", "appName": "mcp-agent" }),
            |h, _| {
                Box::pin(async move {
                    h.login_email(Parameters(auth::LoginEmailParams {
                        email: "matrix@example.org".into(),
                        password: "p".into(),
                    }))
                    .await
                })
            },
        ),
        login(
            "signup",
            "user.create",
            |_| json!({ "email": "matrix@example.org", "password": "p" }),
            |h, _| {
                Box::pin(async move {
                    h.signup(Parameters(auth::SignupParams {
                        email: "matrix@example.org".into(),
                        password: "p".into(),
                    }))
                    .await
                })
            },
        ),
        login(
            "request_login_verification",
            "user.requestVerification",
            |_| json!({ "email": "matrix@example.org", "appInfo": { "appName": "MCP Agent" } }),
            |h, _| {
                Box::pin(async move {
                    h.request_login_verification(Parameters(auth::RequestLoginVerificationParams {
                        email: "matrix@example.org".into(),
                    }))
                    .await
                })
            },
        ),
        login(
            "verify_email_code",
            "user.verifyEmail",
            |_| {
                json!({
                    "email": "matrix@example.org", "code": "000000",
                    "verificationType": "login", "appName": "mcp-agent",
                })
            },
            |h, _| {
                Box::pin(async move {
                    h.verify_email_code(Parameters(auth::VerifyEmailCodeParams {
                        email: "matrix@example.org".into(),
                        code: "000000".into(),
                        verification_type: "login".into(),
                    }))
                    .await
                })
            },
        ),
    ]
}

/// The tools with no RPC method. Those that take a perspective follow the RPC perspective rule
/// ([`perspective_tools`]).
const NOT_WRAPPED: &[&str] = &[
    "auth_status",
    "describe_perspective",
    "flow_actions",
    "flow_state",
    "generate_waker_query",
    "get_documentation",
    "get_mention_waker_config",
    "instance_add_to_collection",
    "instance_create",
    "instance_get",
    "instance_query",
    "instance_remove",
    "instance_remove_from_collection",
    "instance_transcript",
    "instance_update",
];

/// The session of `caller`, holding `caps` where the row gives it capabilities.
fn session(caller: Caller, caps: &[Capability]) -> Arc<Session> {
    let user = |email: &str, did: &str| Principal::User {
        token_id: TokenId(format!("matrix-{email}")),
        email: email.into(),
        did: did.into(),
        caps: caps.to_vec(),
    };
    let principal = match caller {
        Caller::Operator => Principal::Operator,
        Caller::AppWithout => Principal::App {
            token_id: TokenId("matrix-app".into()),
            caps: vec![],
        },
        Caller::AppWith => Principal::App {
            token_id: TokenId("matrix-app".into()),
            caps: caps.to_vec(),
        },
        Caller::User => user(USER_EMAIL, USER_DID),
        Caller::OtherUser => user("other@example.org", OTHER_USER_DID),
        Caller::Anonymous => Principal::Anonymous {
            caps: anonymous_capabilities_when(true),
        },
    };
    Arc::new(Session::for_test(principal, ListenerReach::Network))
}

/// Whether the guards of all of `tool`'s methods admit `session` with the tool's params.
async fn methods_admit(tool: &Tool, session: Arc<Session>, target: &str) -> bool {
    let map = shared_handler_map();
    let ctx = request_context(session);
    for (method, params) in &tool.methods {
        match map.admit(method, &params(target), &ctx).await {
            Ok(Admission::Granted) => {}
            Ok(Admission::Hidden) | Err(_) => return false,
        }
    }
    true
}

/// The rows of `tool`: the perspective rows when its first method names a perspective, the
/// resource rows of that method's guard otherwise.
fn rows(guard: &Guard) -> Vec<(Caller, Option<Target>)> {
    match guard {
        Guard::Perspective { .. } => perspective_rows()
            .iter()
            .map(|&(caller, target, _)| (caller, Some(target)))
            .collect(),
        _ => resource_rows(guard)
            .iter()
            .map(|&(caller, _)| (caller, None))
            .collect(),
    }
}

#[tokio::test]
async fn every_wrapping_tool_allows_and_refuses_as_its_methods_guards_do() {
    let world = World::new().await;
    let _runtime = RuntimeService::start();
    // Multi-user mode off: a caller without a token may not log in or sign up, so the login
    // tools' rows expect the guard's refusal.
    crate::db::Ad4mDb::with_global_instance(|db| db.set_multi_user_enabled(false)).unwrap();
    let map = shared_handler_map();
    let guards: std::collections::HashMap<&str, Guard> = map
        .guards()
        .into_iter()
        .filter_map(|(name, guard)| Some((name, guard?.clone())))
        .collect();
    let without_token =
        Arc::new(Session::open("", &Some("test-admin".into()), ListenerReach::Network).unwrap());

    let mut failures = Vec::new();
    let mut created = Vec::new();
    let mut checked = 0;
    for tool in tools() {
        let first = &guards[tool.methods[0].0];
        // "With the capability" holds every capability the tool's methods check.
        let caps: Vec<Capability> = tool
            .methods
            .iter()
            .filter_map(|(method, _)| guards[method].capability("*"))
            .collect();
        for (caller, target) in rows(first) {
            let uuid = target.map(|t| world.uuid(t)).unwrap_or_default();
            let session = session(caller, &caps);
            let expected = match tool.calls {
                Calls::AsCaller => methods_admit(&tool, session.clone(), &uuid).await,
                Calls::WithoutToken => methods_admit(&tool, without_token.clone(), &uuid).await,
            };
            let handler = handler_as(session.principal.clone());
            let allowed = if expected && !tool.runs_when_allowed {
                // The tool's own first step: the guard of its first method.
                handler
                    .admit_method(tool.methods[0].0, &(tool.methods[0].1)(&uuid))
                    .await
                    .is_ok()
            } else {
                let answer = (tool.run)(handler, uuid.clone()).await;
                if let Ok(v) = serde_json::from_str::<Value>(&answer) {
                    if tool.name == "add_perspective" {
                        created.extend(v["uuid"].as_str().map(str::to_string));
                    }
                }
                let refused = (tool.refused)(&answer);
                if refused == expected {
                    failures.push(format!(
                        "{} as {caller:?} on {target:?}: the methods {}, the tool answered {answer}",
                        tool.name,
                        if expected { "allow" } else { "refuse" },
                    ));
                }
                !refused
            };
            if !tool.runs_when_allowed && expected && !allowed {
                failures.push(format!(
                    "{} as {caller:?}: the methods allow, its guard step refuses",
                    tool.name
                ));
            }
            checked += 1;
        }
    }
    for uuid in created {
        crate::perspectives::remove_perspective(&uuid).await;
    }
    world.remove().await;
    assert!(failures.is_empty(), "{}", failures.join("\n"));
    // 12 perspective tools × 13 rows, 17 others × 5 rows: the walk reached them all.
    assert!(checked >= 12 * 13 + 17 * 5, "only {checked} rows checked");
}

/// What a tool that wraps no RPC method does to its perspective: the RPC rule it must follow.
#[derive(Clone, Copy, Debug)]
enum Access {
    /// `Guard::perspective(perspective_query_capability)`.
    Read,
    /// `Guard::perspective(perspective_update_capability)`.
    Write,
}

impl Access {
    fn guard(self) -> Guard {
        match self {
            Access::Read => Guard::perspective(perspective_query_capability),
            Access::Write => Guard::perspective(perspective_update_capability),
        }
    }
}

/// A tool with no RPC method that works on one perspective.
struct PerspectiveTool {
    name: &'static str,
    access: Access,
    run: Run,
}

/// Runs the dynamic per-class tool `name`, whose class the matrix perspectives do not have.
fn dynamic(handler: Ad4mMcpHandler, name: &'static str, uuid: String) -> Answer {
    Box::pin(async move {
        let arguments =
            json!({ "perspective_id": uuid, "expression_address": "matrix://e", "value": "v" });
        let result = handler
            .handle_dynamic_tool(name, arguments.as_object().cloned())
            .await
            .expect("a dynamic tool answers");
        result
            .content
            .iter()
            .filter_map(|c| c.as_text().map(|t| t.text.clone()))
            .collect()
    })
}

fn perspective_tools() -> Vec<PerspectiveTool> {
    use Access::*;
    let tool = |name: &'static str, access: Access, run: Run| PerspectiveTool { name, access, run };
    const CLASS: &str = "Matrix";
    vec![
        tool("describe_perspective", Read, |h, uuid| {
            Box::pin(async move {
                h.describe_perspective(Parameters(instances::DescribePerspectiveParams {
                    perspective_id: uuid,
                }))
                .await
            })
        }),
        tool("instance_query", Read, |h, uuid| {
            Box::pin(async move {
                h.instance_query(Parameters(instances::InstanceQueryParams {
                    perspective_id: uuid,
                    class_name: CLASS.into(),
                    filter: None,
                    parent: None,
                    limit: None,
                    offset: None,
                }))
                .await
            })
        }),
        tool("instance_get", Read, |h, uuid| {
            Box::pin(async move {
                h.instance_get(Parameters(instances::InstanceGetParams {
                    perspective_id: uuid,
                    class_name: CLASS.into(),
                    base_uri: "matrix://e".into(),
                }))
                .await
            })
        }),
        tool("instance_transcript", Read, |h, uuid| {
            Box::pin(async move {
                h.instance_transcript(Parameters(instances::InstanceTranscriptParams {
                    perspective_id: uuid,
                    class_name: CLASS.into(),
                    parent: "matrix://s".into(),
                    limit: None,
                    text_property: None,
                }))
                .await
            })
        }),
        tool("instance_create", Write, |h, uuid| {
            Box::pin(async move {
                h.instance_create(Parameters(instances::InstanceCreateParams {
                    perspective_id: uuid,
                    class_name: CLASS.into(),
                    properties: None,
                    base_uri: None,
                    parent: None,
                }))
                .await
            })
        }),
        tool("instance_update", Write, |h, uuid| {
            Box::pin(async move {
                h.instance_update(Parameters(instances::InstanceUpdateParams {
                    perspective_id: uuid,
                    class_name: CLASS.into(),
                    base_uri: "matrix://e".into(),
                    properties: Default::default(),
                }))
                .await
            })
        }),
        tool("instance_remove", Write, |h, uuid| {
            Box::pin(async move {
                h.instance_remove(Parameters(instances::InstanceRemoveParams {
                    perspective_id: uuid,
                    class_name: CLASS.into(),
                    base_uri: "matrix://e".into(),
                }))
                .await
            })
        }),
        tool("instance_add_to_collection", Write, |h, uuid| {
            Box::pin(async move {
                h.instance_add_to_collection(Parameters(instances::InstanceAddToCollectionParams {
                    perspective_id: uuid,
                    class_name: CLASS.into(),
                    base_uri: "matrix://e".into(),
                    collection: "items".into(),
                    item_uri: "matrix://t".into(),
                }))
                .await
            })
        }),
        tool("instance_remove_from_collection", Write, |h, uuid| {
            Box::pin(async move {
                h.instance_remove_from_collection(Parameters(
                    instances::InstanceRemoveFromCollectionParams {
                        perspective_id: uuid,
                        class_name: CLASS.into(),
                        base_uri: "matrix://e".into(),
                        collection: "items".into(),
                        item_uri: "matrix://t".into(),
                    },
                ))
                .await
            })
        }),
        tool("flow_state", Read, |h, uuid| {
            Box::pin(async move {
                h.flow_state(Parameters(flows::FlowExprParams {
                    perspective_id: uuid,
                    flow_name: "F".into(),
                    expression_address: "matrix://e".into(),
                }))
                .await
            })
        }),
        tool("flow_actions", Read, |h, uuid| {
            Box::pin(async move {
                h.flow_actions(Parameters(flows::FlowExprParams {
                    perspective_id: uuid,
                    flow_name: "F".into(),
                    expression_address: "matrix://e".into(),
                }))
                .await
            })
        }),
        tool("generate_waker_query", Read, |h, uuid| {
            Box::pin(async move {
                h.generate_waker_query(Parameters(subscriptions::SubscribeToModelParams {
                    perspective_id: uuid,
                    class_name: CLASS.into(),
                    parent: None,
                    predicate: None,
                    target_value: None,
                }))
                .await
            })
        }),
        tool("get_mention_waker_config", Read, |h, uuid| {
            Box::pin(async move {
                h.get_mention_waker_config(Parameters(subscriptions::MentionWakerConfigParams {
                    perspective_id: uuid,
                    name_override: None,
                }))
                .await
            })
        }),
        // The dynamic per-class tools: reads and writes, plain and per property.
        tool("matrix_query", Read, |h, uuid| {
            dynamic(h, "matrix_query", uuid)
        }),
        tool("matrix_get_items", Read, |h, uuid| {
            dynamic(h, "matrix_get_items", uuid)
        }),
        tool("matrix_create", Write, |h, uuid| {
            dynamic(h, "matrix_create", uuid)
        }),
        tool("matrix_set_name", Write, |h, uuid| {
            dynamic(h, "matrix_set_name", uuid)
        }),
    ]
}

/// The refusal status `access`'s RPC rule gives `session` on perspective `uuid` (`None`: it
/// allows), mapped as a tool maps it ([`perspective_error`]). A caller without an
/// authenticated session gets 401: MCP refuses it before any rule.
async fn rule_refusal(access: Access, session: Arc<Session>, uuid: &str) -> Option<u64> {
    if !session.is_authenticated() {
        return Some(401);
    }
    let params = json!({ "uuid": uuid });
    match access
        .guard()
        .admit("matrix", &params, &request_context(session))
        .await
    {
        Ok(Admission::Granted) => None,
        Ok(Admission::Hidden) => unreachable!("Guard::perspective does not hide existence"),
        Err(e) => status(&perspective_error(uuid, e)),
    }
}

/// The tools with no RPC method that take a perspective (the dynamic per-class tools too) allow
/// and refuse every matrix caller on every matrix perspective as the RPC rule for reading
/// or changing a perspective does, with the same status.
#[tokio::test]
async fn every_perspective_tool_without_a_method_follows_the_perspective_rule() {
    let tools = perspective_tools();
    let mut uncovered: Vec<&str> = NOT_WRAPPED
        .iter()
        .copied()
        .filter(|name| !matches!(*name, "auth_status" | "get_documentation"))
        .filter(|name| !tools.iter().any(|t| t.name == *name))
        .collect();
    uncovered.sort();
    assert!(
        uncovered.is_empty(),
        "NOT_WRAPPED tools without rows: {uncovered:?}"
    );

    let world = World::new().await;
    let mut failures = Vec::new();
    let mut checked = 0;
    for tool in &tools {
        let caps = vec![tool
            .access
            .guard()
            .capability("*")
            .expect("a perspective guard checks a capability")];
        for &(caller, target, _) in perspective_rows() {
            let uuid = world.uuid(target);
            let session = session(caller, &caps);
            let expected = rule_refusal(tool.access, session.clone(), &uuid).await;
            let answer = (tool.run)(handler_as(session.principal.clone()), uuid).await;
            let actual = status(&answer).filter(|s| matches!(s, 401 | 403 | 404));
            if actual != expected {
                failures.push(format!(
                    "{} ({:?}) as {caller:?} on {target:?}: the rule answers {expected:?}, \
                     the tool {actual:?}: {answer}",
                    tool.name, tool.access,
                ));
            }
            checked += 1;
        }
    }
    world.remove().await;
    assert!(failures.is_empty(), "{}", failures.join("\n"));
    assert_eq!(checked, tools.len() * perspective_rows().len());
}

/// Every static tool is either in the matrix or has no RPC method.
#[tokio::test]
async fn every_tool_is_classified() {
    let handler = handler_as(Principal::Operator);
    let wrapped: Vec<&str> = tools().iter().map(|t| t.name).collect();
    let mut unclassified: Vec<String> = handler
        .exposed_tools()
        .await
        .into_iter()
        .map(|t| t.name.to_string())
        .filter(|name| !wrapped.contains(&name.as_str()) && !NOT_WRAPPED.contains(&name.as_str()))
        .collect();
    unclassified.sort();
    assert!(
        unclassified.is_empty(),
        "tools in neither the matrix nor NOT_WRAPPED: {unclassified:?}"
    );
    for name in NOT_WRAPPED {
        assert!(!wrapped.contains(name), "{name} is in both lists");
    }
}
