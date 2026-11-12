//! What a turn may reach through the real MCP tool surface.
//!
//! A session turn runs its tool calls through the executor's own MCP handler, inside the
//! intersection of the caller's grant and the session's stored capabilities. These tests
//! drive the real surface (`provider: None`) rather than a stub: every tool that names a
//! perspective is aimed at one the session does not hold, and every tool that could open
//! or widen a session is called by name.

use serde_json::{json, Value};

use super::service::{self, AiSessionCreate};
use super::testkit::*;
use super::types::AiTurnStatus::*;
use super::types::{AiSession, AiSessionMessage};
use crate::agent::capabilities::{perspective_query_capability, perspective_update_capability};
use crate::ai_service::harness::provider::ToolSchema;
use crate::mcp::tools::test_support::{self, class_shape, clean_up, links_of, two_perspectives_of};
use crate::types::RequestContext;

/// What a tool answers a session whose capabilities do not cover the call.
const NOT_HELD: &str = "Capability is not matched";

fn refusal(step: &AiSessionMessage) -> &str {
    step.content["error"].as_str().unwrap_or_default()
}

// ── the capability intersection ──────────────────────────────────────────────

#[tokio::test(flavor = "multi_thread")]
async fn a_turn_reaches_only_what_session_and_caller_share() {
    setup();
    let did = "did:key:alice-d4";
    let (a, b, cleanup) = two_perspectives_of(did).await;
    let read = |uuid: &String| perspective_query_capability(vec![uuid.clone()]);
    let wide = user(did, vec![read(&a), read(&b)]);

    // The caller may read both; the session may read only A. What narrows the turn is the
    // session's set, not the caller's: a read on A answers, a read on B and a write on A
    // are refused — and the refused write lands no link.
    let session = service::create(
        &wide,
        AiSessionCreate {
            model: Some("scripted".into()),
            capabilities: Some(vec![read(&a)]),
            ..Default::default()
        },
    )
    .unwrap();
    let before = links_of(&a).await;
    let steps = steps_of(
        &session,
        &wide,
        Scripted::answering(vec![
            calls("query_links", json!({"perspective_id": a})),
            calls("query_links", json!({"perspective_id": b})),
            calls(
                "add_link",
                json!({
                    "perspective_id": a,
                    "source": "s://a", "predicate": "p://x", "target": "t://1",
                }),
            ),
        ]),
        None,
    )
    .await;
    assert_eq!(steps.len(), 3, "{steps:?}");
    assert_eq!(steps[0].status, Some(Done), "the read answers: {steps:?}");
    assert_eq!(
        steps[1].status,
        Some(Failed),
        "perspective B is outside the session's caps: {steps:?}"
    );
    assert!(refusal(&steps[1]).contains(NOT_HELD), "{steps:?}");
    assert_eq!(
        steps[2].status,
        Some(Failed),
        "the session's write is refused: {steps:?}"
    );
    assert!(steps[2].content.get("result").is_none());
    assert_eq!(links_of(&a).await, before, "the refused write left a link");

    // A session of the wide caller's holding A and B: sent by another session of the
    // same agent that reads A alone, B fails; sent by the wide caller, B answers.
    let session = create(&wide);
    assert_eq!(session.capabilities, vec![read(&a), read(&b)]);
    let narrow = user(did, vec![read(&a)]);
    let steps = steps_of(
        &session,
        &narrow,
        Scripted::answering(vec![
            calls("query_links", json!({"perspective_id": a})),
            calls("query_links", json!({"perspective_id": b})),
        ]),
        None,
    )
    .await;
    assert_eq!(steps.len(), 2, "{steps:?}");
    assert_eq!(steps[0].status, Some(Done), "{steps:?}");
    assert_eq!(
        steps[1].status,
        Some(Failed),
        "the sender does not read B, so neither does its turn: {steps:?}"
    );
    assert!(refusal(&steps[1]).contains(NOT_HELD), "{steps:?}");
    let steps = steps_of(
        &session,
        &wide,
        Scripted::answering(vec![calls("query_links", json!({"perspective_id": b}))]),
        None,
    )
    .await;
    assert_eq!(steps[0].status, Some(Done), "{steps:?}");
    clean_up(cleanup).await;
}

/// The tools that answer before a caller is authenticated, less the two that only read:
/// what is left logs in, signs up, asks for a capability or mints a token.
fn session_opening_tools() -> Vec<&'static str> {
    let opening: Vec<&str> = crate::mcp::tools::AUTH_TOOLS
        .iter()
        .copied()
        .filter(|tool| !["get_documentation", "auth_status"].contains(tool))
        .collect();
    assert_eq!(opening.len(), 6, "{opening:?}");
    opening
}

#[tokio::test(flavor = "multi_thread")]
async fn a_turn_can_never_call_a_tool_that_opens_or_widens_a_session() {
    setup();

    // The first lock: the tools are not on a turn's surface at all — a model naming
    // one anyway gets a refusal, never a call.
    let (ctx, session) = alice("d3");
    let opening = session_opening_tools();
    let model = Scripted::answering(opening.iter().map(|tool| calls(tool, json!({}))).collect());
    let steps = steps_of(&session, &ctx, model.clone(), None).await;

    let offered: Vec<String> = model.offered().into_iter().map(|tool| tool.name).collect();
    assert!(
        offered.iter().any(|tool| tool == "query_links"),
        "the turn has the executor's tools: {offered:?}"
    );
    for tool in &opening {
        assert!(
            !offered.iter().any(|name| name == tool),
            "`{tool}` is on offer"
        );
    }
    // A model that names one anyway gets no further.
    assert_eq!(steps.len(), opening.len(), "{steps:?}");
    for step in &steps {
        assert_eq!(step.status, Some(Failed), "{step:?}");
        assert!(
            refusal(step).contains("is not in the current provider's filtered surface"),
            "{step:?}"
        );
    }

    // The second lock: the bridge every in-process caller dispatches through has no arm
    // for these, whoever holds the handler. The tools beside them that only read answer.
    let operator = test_support::handler_as(test_support::operator());
    for tool in opening {
        let answer = match operator.call_tool_by_name(tool, json!({})).await {
            Ok(answer) => answer,
            Err(e) => e.to_string(),
        };
        assert!(
            answer.contains(&format!("Unknown tool: {tool}")),
            "`{tool}` was dispatched: {answer}"
        );
    }
    let status = operator
        .call_tool_by_name("auth_status", json!({}))
        .await
        .expect("auth_status answers");
    assert!(!status.contains("Unknown tool"), "{status}");
}

// ── every tool, against a perspective the session does not hold ─────────────

/// One call of every tool that names a perspective, aimed at `p`, whose class is `class`
/// and whose instance is `item://{name}`: the reads first, then the writes, then the two
/// kinds of tool a class brings.
fn perspective_calls(p: &str, class: &str, name: &str) -> Vec<(String, Value)> {
    let base = format!("item://{name}");
    let command = json!([{
        "action": "addLink", "source": "this",
        "predicate": "attack://wrote", "target": "attack://by-command",
    }])
    .to_string();
    let mut rows: Vec<(String, Value)> = Vec::new();
    // `perspective_id` alone.
    for tool in [
        "query_links",
        "describe_perspective",
        "get_flows",
        "get_mention_waker_config",
    ] {
        rows.push((tool.to_string(), json!({"perspective_id": p})));
    }
    for (tool, extra) in [
        ("instance_query", json!({"class_name": class})),
        (
            "instance_get",
            json!({"class_name": class, "base_uri": base}),
        ),
        ("instance_transcript", json!({"parent": base})),
        ("get_children", json!({"parent": base})),
        (
            "flow_state",
            json!({"flow_name": "f", "expression_address": base}),
        ),
        (
            "flow_actions",
            json!({"flow_name": "f", "expression_address": base}),
        ),
        ("flow_valid_outputs", json!({"flow_name": "f"})),
        ("generate_waker_query", json!({"class_name": class})),
        ("template_read", json!({"base_uri": base})),
        (
            "add_link",
            json!({
                "source": base,
                "predicate": "attack://wrote", "target": "attack://landed",
            }),
        ),
        (
            "add_model",
            json!({
                "class_name": "Planted", "shacl_json": class_shape("Planted"),
            }),
        ),
        (
            "instance_create",
            json!({"class_name": class, "properties": {"body": "planted"}}),
        ),
        (
            "instance_update",
            json!({
                "class_name": class, "base_uri": base, "properties": {"body": "changed"},
            }),
        ),
        (
            "instance_add_to_collection",
            json!({
                "class_name": class, "base_uri": base,
                "collection": "items", "item_uri": "attack://item",
            }),
        ),
        (
            "instance_remove_from_collection",
            json!({
                "class_name": class, "base_uri": base,
                "collection": "items", "item_uri": "attack://item",
            }),
        ),
        (
            "add_child",
            json!({"parent": base, "child": "attack://child"}),
        ),
        (
            "execute_commands",
            json!({"commands": command, "expression_address": base}),
        ),
        ("template_update", json!({"base_uri": base})),
        (
            "add_flow",
            json!({"flow_name": "planted", "shacl_json": "{}"}),
        ),
        ("flow_proposal_accept", json!({"proposal_uri": base})),
        ("flow_proposal_reject", json!({"proposal_uri": base})),
        (
            "instance_remove",
            json!({"class_name": class, "base_uri": base}),
        ),
    ] {
        let mut arguments = extra;
        arguments["perspective_id"] = json!(p);
        rows.push((tool.to_string(), arguments));
    }
    rows.push((
        "neighbourhood_publish_from_perspective".to_string(),
        json!({"perspective_uuid": p, "link_language": "lang://none"}),
    ));
    let lower = class.to_lowercase();
    rows.push((format!("{lower}_query"), json!({"perspective_id": p})));
    rows.push((
        format!("{lower}_create"),
        json!({"perspective_id": p, "body": "planted"}),
    ));
    rows
}

/// One call of every tool that names no perspective: what it reaches is the agent's.
/// `list_perspectives` is not one: the session holds perspective READ on A, so it answers,
/// narrowed to A — [`a_narrowed_sessions_tools_stay_inside_it`] asserts that separately.
fn agent_wide_calls() -> Vec<(String, Value)> {
    [
        ("add_perspective", json!({"name": "planted"})),
        (
            "neighbourhood_join_from_url",
            json!({"url": "neighbourhood://QmPlanted"}),
        ),
        ("set_agent_profile", json!({"username": "planted"})),
        (
            "set_agent_profile_picture",
            json!({"image_base64": "AAAA", "mime_type": "image/png"}),
        ),
        ("set_agent_public_perspective", json!({"links_json": "[]"})),
        ("get_agent_profile", json!({})),
        ("get_agent_public_perspective", json!({})),
        ("get_my_did", json!({})),
        ("language_meta", json!({"address": "QmNone"})),
        ("list_link_language_templates", json!({})),
    ]
    .into_iter()
    .map(|(tool, arguments)| (tool.to_string(), arguments))
    .collect()
}

/// Calls on `a`, which the session holds, that name `b` or what is in it wherever an
/// argument takes a free value: none of them may read from `b` or write to it.
fn calls_naming_another_perspective(a: &str, b: &str, theirs: &str) -> Vec<(String, Value)> {
    let command = json!([{
        "action": "addLink", "source": "this",
        "predicate": "attack://wrote", "target": "attack://by-command",
    }])
    .to_string();
    [
        (
            "query_links",
            json!({"perspective_id": a, "perspective_uuid": b, "uuid": b, "source": theirs}),
        ),
        (
            "instance_get",
            json!({"perspective_id": a, "class_name": "Note", "base_uri": theirs}),
        ),
        (
            "instance_query",
            json!({"perspective_id": a, "class_name": "Vault"}),
        ),
        (
            "instance_query",
            json!({
                "perspective_id": a, "class_name": "Note", "parent": b,
                "filter": {"perspective_id": b, "id": theirs},
            }),
        ),
        (
            "get_children",
            json!({"perspective_id": a, "parent": theirs}),
        ),
        (
            "instance_transcript",
            json!({"perspective_id": a, "parent": b}),
        ),
        (
            "instance_update",
            json!({
                "perspective_id": a, "class_name": "Note", "base_uri": theirs,
                "properties": {"body": "changed", "perspective_id": b},
            }),
        ),
        (
            "add_child",
            json!({"perspective_id": a, "parent": b, "child": theirs}),
        ),
        (
            "execute_commands",
            json!({"perspective_id": a, "commands": command, "expression_address": theirs}),
        ),
    ]
    .into_iter()
    .map(|(tool, arguments)| (tool.to_string(), arguments))
    .collect()
}

/// Whether a tool's parameters name a perspective at their top level.
fn names_a_perspective(tool: &ToolSchema) -> bool {
    tool.parameters["properties"]
        .as_object()
        .is_some_and(|properties| properties.keys().any(|key| key.contains("perspective")))
}

/// Whether a tool's parameters name a perspective anywhere below their top level, where
/// `perspective_calls` would not aim it.
fn names_a_perspective_below_the_top(parameters: &Value) -> bool {
    fn any_key(value: &Value) -> bool {
        match value {
            Value::Object(object) => object
                .iter()
                .any(|(key, value)| key.contains("perspective") || any_key(value)),
            Value::Array(items) => items.iter().any(any_key),
            _ => false,
        }
    }
    parameters.as_object().is_some_and(|top| {
        top.iter().any(|(key, value)| match value.as_object() {
            Some(properties) if key == "properties" => properties.values().any(any_key),
            _ => any_key(value),
        })
    })
}

/// The tool steps of `table` called in order on `session` by `caller`, and the tools the
/// model was offered. Six calls a turn: a turn stops at twenty.
async fn steps_of_calls(
    session: &AiSession,
    caller: &RequestContext,
    table: &[(String, Value)],
) -> (Vec<AiSessionMessage>, Vec<ToolSchema>) {
    let mut steps = Vec::new();
    let mut offered = Vec::new();
    for chunk in table.chunks(6) {
        let model = Scripted::answering(
            chunk
                .iter()
                .map(|(tool, arguments)| calls(tool, arguments.clone()))
                .collect(),
        );
        steps.extend(steps_of(session, caller, model.clone(), None).await);
        offered = model.offered();
    }
    assert_eq!(steps.len(), table.len(), "one step a call: {steps:?}");
    (steps, offered)
}

/// A step that failed on `expected` and left no result.
fn expect_failed(step: &AiSessionMessage, tool: &str, expected: &str) {
    assert_eq!(step.status, Some(Failed), "`{tool}`: {step:?}");
    assert!(refusal(step).contains(expected), "`{tool}`: {step:?}");
    assert!(step.content.get("result").is_none(), "`{tool}`: {step:?}");
}

/// A session of `caller`'s that holds read and write on perspective A and nothing else,
/// sent by that caller, who holds everything: whatever tool a turn calls, perspective B —
/// the same agent's — is neither read nor changed, and nothing of the agent's outside A is
/// reached.
async fn a_narrowed_sessions_tools_stay_inside_it(
    caller: RequestContext,
    owners: Option<Vec<String>>,
) {
    let _runtime = test_support::RuntimeService::start();
    let a = test_support::perspective(owners.clone()).await;
    let b = test_support::perspective(owners).await;
    test_support::seed(&a, "Note", "inside-a", "words-of-a").await;
    test_support::seed(&b, "Vault", "inside-b", "words-of-b").await;
    let untouched = links_of(&b).await;
    assert!(
        untouched
            .iter()
            .any(|(_, _, target)| target.contains("words-of-b")),
        "B holds its words: {untouched:?}"
    );

    let session = service::create(
        &caller,
        AiSessionCreate {
            model: Some("scripted".into()),
            capabilities: Some(vec![
                perspective_query_capability(vec![a.clone()]),
                perspective_update_capability(vec![a.clone()]),
            ]),
            ..Default::default()
        },
    )
    .unwrap();

    // Every tool that names a perspective, aimed at B.
    let table = perspective_calls(&b, "Vault", "inside-b");
    let (outside, offered) = steps_of_calls(&session, &caller, &table).await;
    for (step, (tool, _)) in outside.iter().zip(&table) {
        // A class tool of B's is not there to call; every other tool refuses the session.
        let expected = if tool.starts_with("vault_") {
            "is not in the current provider's filtered surface"
        } else {
            NOT_HELD
        };
        expect_failed(step, tool, expected);
    }

    // The table is the whole surface: every tool on offer that names a perspective has a
    // row, none names one where the table would not aim it, and no class tool — A's or
    // B's — is on offer while `dynamicClassTools` is off.
    for tool in &offered {
        assert!(
            !names_a_perspective(tool) || table.iter().any(|(row, _)| *row == tool.name),
            "`{}` names a perspective and `perspective_calls` has no row for it",
            tool.name
        );
        assert!(
            !names_a_perspective_below_the_top(&tool.parameters),
            "`{}` names a perspective inside an argument: {}",
            tool.name,
            tool.parameters
        );
        assert!(
            !tool.name.starts_with("vault_") && !tool.name.starts_with("note_"),
            "`{}` is on offer",
            tool.name
        );
    }

    // Every tool that names none: the session holds no capability of the agent's own.
    let table = agent_wide_calls();
    let (agent_wide, _) = steps_of_calls(&session, &caller, &table).await;
    for (step, (tool, _)) in agent_wide.iter().zip(&table) {
        expect_failed(step, tool, NOT_HELD);
    }
    // `list_perspectives` is the one agent-wide call a perspective-scoped session may make:
    // it holds READ on A, so `perspective.all` answers — with A alone, never B.
    let (listed, _) = steps_of_calls(
        &session,
        &caller,
        &[("list_perspectives".to_string(), json!({}))],
    )
    .await;
    assert_eq!(listed[0].status, Some(Done), "the list answers: {listed:?}");
    let answer = listed[0].content["result"].as_str().unwrap_or_default();
    assert!(answer.contains(&a), "A is listed: {answer}");
    assert!(!answer.contains(&b), "B is not: {answer}");
    let (status, _) =
        steps_of_calls(&session, &caller, &[("auth_status".to_string(), json!({}))]).await;
    assert!(
        !status[0].content.to_string().contains(&b),
        "what the session reads of itself names B: {status:?}"
    );
    for tool in &offered {
        assert!(
            names_a_perspective(tool)
                || table.iter().any(|(row, _)| *row == tool.name)
                || ["auth_status", "get_documentation", "list_perspectives"]
                    .contains(&tool.name.as_str()),
            "`{}` names no perspective and `agent_wide_calls` has no row for it",
            tool.name
        );
    }

    // The same session on A, which it holds: the reads answer with A's words and the write
    // lands, so what refused every call above was the session's capabilities.
    let (inside, _) = steps_of_calls(
        &session,
        &caller,
        &[
            ("query_links".to_string(), json!({"perspective_id": a})),
            (
                "instance_query".to_string(),
                json!({"perspective_id": a, "class_name": "Note"}),
            ),
            ("note_query".to_string(), json!({"perspective_id": a})),
            (
                "add_link".to_string(),
                json!({
                    "perspective_id": a, "source": "item://inside-a",
                    "predicate": "attack://wrote", "target": "attack://landed",
                }),
            ),
        ],
    )
    .await;
    for step in [&inside[0], &inside[1], &inside[3]] {
        assert_eq!(step.status, Some(Done), "on A: {step:?}");
    }
    // A per-class tool name is refused outright: not on the surface while
    // `dynamicClassTools` is off, so the filter never dispatches it.
    expect_failed(
        &inside[2],
        "note_query",
        "is not in the current provider's filtered surface",
    );
    for step in &inside[..2] {
        assert!(
            step.content["result"]
                .as_str()
                .unwrap_or_default()
                .contains("words-of-a"),
            "on A: {step:?}"
        );
    }
    assert!(
        links_of(&a).await.contains(&(
            "item://inside-a".to_string(),
            "attack://wrote".to_string(),
            "attack://landed".to_string(),
        )),
        "the write on A landed: {inside:?}"
    );

    // Calls on A that name B wherever an argument takes a free value.
    let table = calls_naming_another_perspective(&a, &b, "item://inside-b");
    let (naming, _) = steps_of_calls(&session, &caller, &table).await;

    // Nothing of B's reached the transcript, and B is as it was.
    for step in outside
        .iter()
        .chain(&agent_wide)
        .chain(&listed)
        .chain(&inside)
        .chain(&naming)
    {
        assert!(
            !step.content.to_string().contains("words-of-b"),
            "B's words are in the transcript: {step:?}"
        );
    }
    assert_eq!(links_of(&b).await, untouched, "B changed");
    let perspectives = test_support::handler_as(test_support::operator())
        .call_tool_by_name("list_perspectives", json!({}))
        .await
        .unwrap();
    assert!(!perspectives.contains("planted"), "{perspectives}");
    service::delete(&caller, &session.id).unwrap();
    clean_up(vec![a, b]).await;
}

#[tokio::test(flavor = "multi_thread")]
async fn no_tool_reaches_a_perspective_outside_a_users_sessions_capabilities() {
    setup();
    let did = crate::test_utils::create_test_user("alice-d5@example.org");
    a_narrowed_sessions_tools_stay_inside_it(user(&did, everything()), Some(vec![did])).await;
}

#[tokio::test(flavor = "multi_thread")]
async fn no_tool_reaches_a_perspective_outside_the_main_agents_sessions_capabilities() {
    setup();
    a_narrowed_sessions_tools_stay_inside_it(operator(), None).await;
}

/// The scenario `capabilities` exists for: a session scoped to perspective A lists
/// perspectives — the first thing a model does — and sees only A, reads and writes A, and
/// is refused on B.
#[tokio::test(flavor = "multi_thread")]
async fn a_session_scoped_to_one_perspective_lists_only_it() {
    setup();
    // A real managed agent: adding a link signs as it.
    let did = crate::test_utils::create_test_user("alice-d6@example.org");
    let (a, b, cleanup) = two_perspectives_of(&did).await;
    let caller = user(&did, everything());
    let session = service::create(
        &caller,
        AiSessionCreate {
            model: Some("scripted".into()),
            default_perspective: Some(a.clone()),
            capabilities: Some(vec![
                perspective_query_capability(vec![a.clone()]),
                perspective_update_capability(vec![a.clone()]),
            ]),
            ..Default::default()
        },
    )
    .unwrap();

    let steps = steps_of(
        &session,
        &caller,
        Scripted::answering(vec![
            calls("list_perspectives", json!({})),
            calls("query_links", json!({"perspective_id": a})),
            calls(
                "add_link",
                json!({
                    "perspective_id": a,
                    "source": "s://a", "predicate": "p://x", "target": "t://1",
                }),
            ),
            calls("query_links", json!({"perspective_id": b})),
        ]),
        None,
    )
    .await;
    assert_eq!(steps.len(), 4, "{steps:?}");

    // The listing answers — with A alone.
    assert_eq!(steps[0].status, Some(Done), "the list answers: {steps:?}");
    let listed = steps[0].content["result"].as_str().unwrap_or_default();
    assert!(listed.contains(&a), "A is listed: {listed}");
    assert!(!listed.contains(&b), "B is not: {listed}");

    // Reading A and writing it work.
    assert_eq!(steps[1].status, Some(Done), "A reads: {steps:?}");
    assert_eq!(steps[2].status, Some(Done), "A takes a link: {steps:?}");
    assert!(
        links_of(&a)
            .await
            .contains(&("s://a".into(), "p://x".into(), "t://1".into())),
        "the link landed"
    );

    // B stays refused.
    assert_eq!(
        steps[3].status,
        Some(Failed),
        "B is outside the session: {steps:?}"
    );
    assert!(refusal(&steps[3]).contains(NOT_HELD), "{steps:?}");
    clean_up(cleanup).await;
}

// ── the tool list follows `dynamicClassTools` ────────────────────────────────

/// A Note with two writable properties and a collection, so a test sees the class's
/// setters and collection tools on offer, not only the CRUD five.
fn note_shape() -> String {
    json!({
        "target_class": "attack://Note",
        "constructor_actions": [{
            "action": "addLink", "source": "this",
            "predicate": "rdf://type", "target": "attack://Note",
        }],
        "properties": [
            {
                "path": "attack://title", "name": "title", "datatype": "xsd:string",
                "min_count": 0, "max_count": 1, "writable": true,
                "setter": [{
                    "action": "setSingleTarget", "source": "this",
                    "predicate": "attack://title", "target": "value",
                }],
            },
            {
                "path": "attack://body", "name": "body", "datatype": "xsd:string",
                "min_count": 1, "max_count": 1, "writable": true,
                "setter": [{
                    "action": "setSingleTarget", "source": "this",
                    "predicate": "attack://body", "target": "value",
                }],
            },
            {
                "path": "ad4m://has_child", "name": "items", "collection": true,
                "writable": true,
                "adder": [{
                    "action": "addLink", "source": "this",
                    "predicate": "ad4m://has_child", "target": "value",
                }],
                "remover": [{
                    "action": "removeLink", "source": "this",
                    "predicate": "ad4m://has_child", "target": "value",
                }],
            },
        ],
    })
    .to_string()
}

/// Restores the executor's `dynamicClassTools` on drop, so a failed assert does not
/// leave the flag on for the tests that follow.
struct DynamicToolsFlag(Option<bool>);

impl DynamicToolsFlag {
    fn held() -> Self {
        Self(crate::config::get_global_config().dynamic_class_tools)
    }
    fn set(&self, value: Option<bool>) {
        let mut config = crate::config::get_global_config();
        config.dynamic_class_tools = value;
        crate::config::set_global_config(config);
    }
}

impl Drop for DynamicToolsFlag {
    fn drop(&mut self) {
        self.set(self.0);
    }
}

/// A session's tool list follows the MCP rule: the static tools, plus the per-class
/// tools of the perspectives it may read only while `dynamicClassTools` is on.
#[tokio::test(flavor = "multi_thread")]
async fn a_session_lists_the_per_class_tools_only_with_dynamic_class_tools() {
    setup();
    let _runtime = test_support::RuntimeService::start();
    let did = "did:key:alice-d7";
    let a = test_support::perspective(Some(vec![did.to_string()])).await;
    test_support::handler_as(test_support::operator())
        .call_tool_by_name(
            "add_model",
            json!({"perspective_id": a, "class_name": "Note", "shacl_json": note_shape()}),
        )
        .await
        .expect("the class registers");
    let caller = user(did, everything());
    let session = service::create(
        &caller,
        AiSessionCreate {
            model: Some("scripted".into()),
            capabilities: Some(vec![
                perspective_query_capability(vec![a.clone()]),
                perspective_update_capability(vec![a.clone()]),
            ]),
            ..Default::default()
        },
    )
    .unwrap();
    let flag = DynamicToolsFlag::held();
    let offered_of = |model: &Scripted| -> Vec<String> {
        model
            .offered()
            .iter()
            .map(|tool| tool.name.clone())
            .collect()
    };

    // Flag unset, as the executor runs by default: the static surface alone — every
    // name on offer is a static tool's, and none is the class's.
    flag.set(None);
    let model = Scripted::answering(vec![]);
    steps_of(&session, &caller, model.clone(), None).await;
    let statics = crate::mcp::tools::Ad4mMcpHandler::static_tool_names();
    let names = offered_of(&model);
    for name in &names {
        assert!(
            statics.contains(name),
            "a tool outside the static surface is on offer: {name}"
        );
        assert!(
            !name.starts_with("note_"),
            "a per-class tool is on offer with the flag off: {name}"
        );
    }
    for name in ["describe_perspective", "instance_create"] {
        assert!(names.iter().any(|n| n == name), "{name} is not on offer");
    }

    // The operator opted in: the class's tools join what the model is offered — its
    // CRUD five, a setter per property and the collection's three.
    flag.set(Some(true));
    let model = Scripted::answering(vec![]);
    steps_of(&session, &caller, model.clone(), None).await;
    let names = offered_of(&model);
    for name in [
        "note_create",
        "note_query",
        "note_list",
        "note_get",
        "note_delete",
        "note_set_title",
        "note_set_body",
        "note_get_items",
        "note_add_items",
        "note_remove_items",
    ] {
        assert!(
            names.iter().any(|n| n == name),
            "{name} is not on offer: {names:?}"
        );
    }
    clean_up(vec![a]).await;
}
