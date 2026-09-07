//! Integration tests for the static `instance_*` surface, driven through
//! the same handler methods the MCP transport dispatches to.

use super::*;
use crate::mcp::server::McpContext;
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
use crate::perspectives::register_perspective;
use rmcp::handler::server::wrapper::Parameters;
use tokio::sync::RwLock;

/// Flux-shaped Channel: string / boolean / integer scalars, one
/// `has_child` collection, class + property interpretation hints.
const CHANNEL_SDNA: &str = r#"{
  "target_class": "flux://Channel",
  "interpretation_hint": "A chat room that groups messages by topic.",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"flux://entry_type","target":"flux://has_channel"},
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"flux://Channel"}
  ],
  "properties": [
    {"path":"flux://channel_name","name":"name","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "interpretation_hint":"Short room name.",
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"flux://channel_name","target":"value"}]},
    {"path":"flux://channel_description","name":"description","datatype":"xsd:string","min_count":0,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"flux://channel_description","target":"value"}]},
    {"path":"flux://channel_is_pinned","name":"isPinned","datatype":"xsd:boolean","min_count":0,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"flux://channel_is_pinned","target":"value"}]},
    {"path":"flux://channel_rank","name":"rank","datatype":"xsd:integer","min_count":0,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"flux://channel_rank","target":"value"}]},
    {"path":"ad4m://has_child","name":"messages","collection":true,"writable":true,
     "interpretation_hint":"The messages posted in this room.",
     "adder":[{"action":"addLink","source":"this","predicate":"ad4m://has_child","target":"value"}],
     "remover":[{"action":"removeLink","source":"this","predicate":"ad4m://has_child","target":"value"}]}
  ]
}"#;

/// Message whose body is stored as a signed literal envelope — the read
/// path has to unwrap it to the plain text.
const MESSAGE_SDNA: &str = r#"{
  "target_class": "flux://Message",
  "interpretation_hint": "One chat message.",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"flux://entry_type","target":"flux://has_message"},
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"flux://Message"}
  ],
  "properties": [
    {"path":"flux://body","name":"body","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "resolve_language":"literal","interpretation_hint":"The message text.",
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"flux://body","target":"value"}]}
  ]
}"#;

/// A perspective with both classes registered in the global registry, and
/// an MCP handler authenticated as admin against it — the same path an
/// external client takes minus the HTTP transport.
/// Relation-typed fixtures: Post --hasOne--> User (forward, writable),
/// Comment --belongsToOne--> Post (reverse, read-only on this side).
const USER_SDNA: &str = r#"{
  "target_class": "ns://User",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://User"}
  ],
  "properties": [
    {"path":"ns://user_name","name":"name","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://user_name","target":"value"}]}
  ]
}"#;

const POST_SDNA: &str = r#"{
  "target_class": "ns://Post",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Post"}
  ],
  "properties": [
    {"path":"ns://post_title","name":"title","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://post_title","target":"value"}]},
    {"path":"ns://post_author","name":"writer","node_kind":"IRI","relation_kind":"hasOne","max_count":1,
     "target_class_name":"User","writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://post_author","target":"value"}]}
  ]
}"#;

const COMMENT_SDNA: &str = r#"{
  "target_class": "ns://Comment",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Comment"}
  ],
  "properties": [
    {"path":"ns://comment_text","name":"text","datatype":"xsd:string","min_count":1,"max_count":1,"writable":true,
     "setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://comment_text","target":"value"}]},
    {"path":"ns://post_comments","name":"post","node_kind":"IRI","relation_kind":"belongsToOne",
     "target_class_name":"Post"}
  ]
}"#;

/// Unregisters the fixture perspective when the test ends (also on
/// panic), so tests that assert on empty global state stay honest.
struct PerspectiveGuard(String);
impl Drop for PerspectiveGuard {
    fn drop(&mut self) {
        crate::perspectives::unregister_perspective(&self.0);
    }
}

async fn setup(dynamic_class_tools: bool) -> (Ad4mMcpHandler, String, PerspectiveGuard) {
    setup_with(
        &[("Channel", CHANNEL_SDNA), ("Message", MESSAGE_SDNA)],
        dynamic_class_tools,
    )
    .await
}

async fn setup_with(
    classes: &[(&str, &str)],
    dynamic_class_tools: bool,
) -> (Ad4mMcpHandler, String, PerspectiveGuard) {
    let (perspective, _shapes, _ctx) = setup_perspective_no_llm(classes).await;
    let uuid = perspective.persisted.lock().await.uuid.clone();
    register_perspective(uuid.clone(), perspective);
    let handler = Ad4mMcpHandler::new(McpContext {
        admin_credential: Some("test-admin".to_string()),
        auth_token: Arc::new(RwLock::new(Some("test-admin".to_string()))),
        dynamic_class_tools,
    });
    let guard = PerspectiveGuard(uuid.clone());
    (handler, uuid, guard)
}

fn parse(s: &str) -> Value {
    serde_json::from_str(s).unwrap_or_else(|e| panic!("tool returned non-JSON ({e}): {s}"))
}

fn find<'a>(list: &'a [Value], name: &str) -> &'a Value {
    list.iter()
        .find(|v| v["name"] == name)
        .unwrap_or_else(|| panic!("no entry named {name} in {list:?}"))
}

fn props(pairs: &[(&str, Value)]) -> Map<String, Value> {
    pairs
        .iter()
        .map(|(k, v)| (k.to_string(), v.clone()))
        .collect()
}

#[test]
fn friendly_type_maps_xsd_datatypes() {
    assert_eq!(friendly_type(None), "string");
    assert_eq!(friendly_type(Some("xsd://string")), "string");
    assert_eq!(friendly_type(Some("xsd:string")), "string");
    assert_eq!(friendly_type(Some("xsd:boolean")), "boolean");
    assert_eq!(friendly_type(Some("xsd://integer")), "integer");
    assert_eq!(friendly_type(Some("xsd:int")), "integer");
    assert_eq!(friendly_type(Some("xsd:decimal")), "number");
    assert_eq!(friendly_type(Some("xsd:double")), "number");
    assert_eq!(friendly_type(Some("xsd:dateTime")), "datetime");
    assert_eq!(
        friendly_type(Some("http://www.w3.org/2001/XMLSchema#date")),
        "datetime"
    );
    assert_eq!(friendly_type(Some("xsd:hexBinary")), "hexbinary");
}

#[tokio::test(flavor = "multi_thread")]
async fn describe_perspective_reports_schema_as_data() {
    let (handler, uuid, _guard) = setup(false).await;
    let out = handler
        .describe_perspective(Parameters(DescribePerspectiveParams {
            perspective_id: uuid.clone(),
        }))
        .await;
    let desc = parse(&out);
    assert_eq!(desc["perspective_id"], uuid);

    let classes = desc["classes"].as_array().expect("classes array");
    let channel = find(classes, "Channel");
    assert_eq!(channel["class_uri"], "flux://Channel");
    assert_eq!(
        channel["interpretation_hint"],
        "A chat room that groups messages by topic."
    );

    let properties = channel["properties"].as_array().expect("properties");
    let name = find(properties, "name");
    assert_eq!(name["type"], "string");
    assert_eq!(name["required"], true);
    assert_eq!(name["cardinality"], json!({"min": 1, "max": 1}));
    assert_eq!(name["predicate"], "flux://channel_name");
    assert_eq!(name["interpretation_hint"], "Short room name.");
    assert_eq!(find(properties, "isPinned")["type"], "boolean");
    assert_eq!(find(properties, "rank")["type"], "integer");
    assert_eq!(find(properties, "description")["required"], false);
    assert!(
        properties.iter().all(|p| p["name"] != "messages"),
        "collections must not be listed under properties"
    );

    let collections = channel["collections"].as_array().expect("collections");
    let messages = find(collections, "messages");
    assert_eq!(messages["predicate"], "ad4m://has_child");
    assert_eq!(messages["cardinality"]["max"], Value::Null);
    assert_eq!(
        messages["interpretation_hint"],
        "The messages posted in this room."
    );

    let message = find(classes, "Message");
    let body = find(message["properties"].as_array().unwrap(), "body");
    assert_eq!(body["required"], true);
    assert_eq!(body["resolve_language"], "literal");

    assert!(desc["flows"].as_array().unwrap().is_empty());
    assert!(desc["usage"].as_str().unwrap().contains("instance_create"));
}

#[tokio::test(flavor = "multi_thread")]
async fn validation_names_property_type_and_cardinality() {
    let (_handler, uuid, _guard) = setup(false).await;
    let perspective = crate::perspectives::get_perspective(&uuid).unwrap();
    let shape = perspective.get_shape("Channel").expect("Channel shape");

    // Missing required property.
    let errs = validate_properties(
        &shape,
        &props(&[("description", json!("no name"))]),
        WriteMode::Create,
    )
    .expect_err("missing name must fail");
    assert_eq!(errs.len(), 1);
    assert_eq!(errs[0].property, "name");
    assert!(errs[0].problem.contains("missing required"));
    assert_eq!(errs[0].expected_type.as_deref(), Some("string"));
    assert!(errs[0]
        .cardinality
        .as_deref()
        .unwrap()
        .contains("minCount 1"));

    // Wrong type + array on a single-valued property: both reported.
    let errs = validate_properties(
        &shape,
        &props(&[("name", json!(["a", "b"])), ("isPinned", json!("maybe"))]),
        WriteMode::Create,
    )
    .expect_err("bad values must fail");
    assert_eq!(errs.len(), 2, "{errs:?}");
    let pinned = errs.iter().find(|e| e.property == "isPinned").unwrap();
    assert_eq!(pinned.expected_type.as_deref(), Some("boolean"));
    assert!(pinned.received.as_deref().unwrap().contains("maybe"));
    let name = errs.iter().find(|e| e.property == "name").unwrap();
    assert!(name.problem.contains("array of 2"));
    assert!(name.cardinality.as_deref().unwrap().contains("maxCount 1"));

    // Unknown property lists the available ones.
    let errs = validate_properties(
        &shape,
        &props(&[("name", json!("x")), ("nmae", json!("typo"))]),
        WriteMode::Create,
    )
    .expect_err("unknown property must fail");
    assert_eq!(errs[0].property, "nmae");
    assert!(errs[0].problem.contains("unknown property"));
    assert!(errs[0].problem.contains("description"));

    // Lenient coercion where intent is unambiguous.
    let ok = validate_properties(
        &shape,
        &props(&[
            ("name", json!("general")),
            ("rank", json!("7")),
            ("isPinned", json!("TRUE")),
            ("messages", json!(["flux://m1", "flux://m2"])),
        ]),
        WriteMode::Create,
    )
    .expect("valid create");
    assert_eq!(ok.scalars["rank"], json!(7));
    assert_eq!(ok.scalars["isPinned"], json!(true));
    assert_eq!(
        ok.collections,
        vec![(
            "messages".to_string(),
            vec![json!("flux://m1"), json!("flux://m2")]
        )]
    );

    // Case-insensitive property names resolve to the canonical spelling.
    let ok = validate_properties(
        &shape,
        &props(&[("Name", json!("x")), ("ispinned", json!(false))]),
        WriteMode::Create,
    )
    .expect("case-insensitive names");
    assert!(ok.scalars.contains_key("name"));
    assert!(ok.scalars.contains_key("isPinned"));

    // Update: no required check, collections refused.
    let ok = validate_properties(
        &shape,
        &props(&[("description", json!("only this"))]),
        WriteMode::Update,
    )
    .expect("partial update");
    assert_eq!(ok.scalars.len(), 1);
    let errs = validate_properties(
        &shape,
        &props(&[("messages", json!(["flux://m1"]))]),
        WriteMode::Update,
    )
    .expect_err("collection on update must fail");
    assert!(errs[0].problem.contains("instance_add_to_collection"));

    // The error payload carries the summary in `error` too.
    let payload = parse(&validation_failure("Channel", &errs));
    assert!(payload["error"].as_str().unwrap().contains("messages"));
    assert_eq!(payload["validation_errors"][0]["property"], "messages");
}

/// Relation-typed properties through the generic tools: schema exposure,
/// URI-gated writes on a forward hasOne, rejection of non-URI values, and
/// read-only enforcement on a belongsTo (reverse) relation.
#[tokio::test(flavor = "multi_thread")]
async fn instance_tools_relation_typed_properties() {
    let (handler, uuid, _guard) = setup_with(
        &[
            ("User", USER_SDNA),
            ("Post", POST_SDNA),
            ("Comment", COMMENT_SDNA),
        ],
        false,
    )
    .await;

    // describe_perspective exposes the relation with kind + target class,
    // and marks the reverse side read-only.
    let desc = parse(
        &handler
            .describe_perspective(Parameters(DescribePerspectiveParams {
                perspective_id: uuid.clone(),
            }))
            .await,
    );
    let classes = desc["classes"].as_array().expect("classes array");
    let post = find(classes, "Post");
    let writer = find(post["properties"].as_array().expect("post props"), "writer");
    assert_eq!(writer["type"], "reference");
    assert_eq!(writer["relation_kind"], "hasOne");
    assert_eq!(writer["target_class"], "User");
    let comment = find(classes, "Comment");
    let post_rel = find(
        comment["properties"].as_array().expect("comment props"),
        "post",
    );
    assert_eq!(post_rel["relation_kind"], "belongsToOne");
    assert_eq!(post_rel["read_only"], true, "{post_rel}");

    // Create a User, then a Post pointing at it through the relation.
    let user = parse(
        &handler
            .instance_create(Parameters(InstanceCreateParams {
                perspective_id: uuid.clone(),
                class_name: "User".into(),
                properties: Some(props(&[("name", json!("Geordi"))])),
                base_uri: None,
                parent: None,
            }))
            .await,
    );
    assert_eq!(user["created"], true, "{user}");
    let user_uri = user["base_uri"].as_str().unwrap().to_string();

    let post = parse(
        &handler
            .instance_create(Parameters(InstanceCreateParams {
                perspective_id: uuid.clone(),
                class_name: "Post".into(),
                properties: Some(props(&[
                    ("title", json!("Warp field notes")),
                    ("writer", json!(user_uri.clone())),
                ])),
                base_uri: None,
                parent: None,
            }))
            .await,
    );
    assert_eq!(post["created"], true, "{post}");
    let post_uri = post["base_uri"].as_str().unwrap().to_string();

    // Read back: the hasOne relation resolves to the linked User's URI.
    let got = parse(
        &handler
            .instance_get(Parameters(InstanceGetParams {
                perspective_id: uuid.clone(),
                class_name: "Post".into(),
                base_uri: post_uri.clone(),
            }))
            .await,
    );
    assert_eq!(got["title"], "Warp field notes", "{got}");
    let writer_val = &got["writer"];
    let writer_str = writer_val
        .as_str()
        .map(|s| s.to_string())
        .or_else(|| {
            writer_val
                .get("id")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string())
        })
        .unwrap_or_else(|| writer_val.to_string());
    assert!(
        writer_str.contains(&user_uri),
        "writer should resolve to the linked user, got: {got}"
    );

    // A non-URI value for a relation property is rejected before any write
    // (the is_safe_iri_target gate).
    let bad = parse(
        &handler
            .instance_create(Parameters(InstanceCreateParams {
                perspective_id: uuid.clone(),
                class_name: "Post".into(),
                properties: Some(props(&[
                    ("title", json!("Bad author")),
                    ("writer", json!("Geordi La Forge")),
                ])),
                base_uri: None,
                parent: None,
            }))
            .await,
    );
    assert!(bad["error"].is_string(), "{bad}");
    assert_eq!(bad["validation_errors"][0]["property"], "writer", "{bad}");
    assert!(
        bad["validation_errors"][0]["problem"]
            .as_str()
            .unwrap()
            .contains("existing User instance"),
        "{bad}"
    );

    // Writing the reverse side of a belongsTo relation is rejected: the
    // link lives on the target instance.
    let comment = parse(
        &handler
            .instance_create(Parameters(InstanceCreateParams {
                perspective_id: uuid.clone(),
                class_name: "Comment".into(),
                properties: Some(props(&[("text", json!("Fascinating."))])),
                base_uri: None,
                parent: None,
            }))
            .await,
    );
    assert_eq!(comment["created"], true, "{comment}");
    let comment_uri = comment["base_uri"].as_str().unwrap().to_string();

    let reverse_write = parse(
        &handler
            .instance_update(Parameters(InstanceUpdateParams {
                perspective_id: uuid.clone(),
                class_name: "Comment".into(),
                base_uri: comment_uri,
                properties: props(&[("post", json!(post_uri))]),
            }))
            .await,
    );
    assert!(reverse_write["error"].is_string(), "{reverse_write}");
    assert_eq!(
        reverse_write["validation_errors"][0]["property"], "post",
        "{reverse_write}"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn instance_tools_round_trip_typed_data() {
    let (handler, uuid, _guard) = setup(false).await;

    // Create a channel with typed values.
    let out = handler
        .instance_create(Parameters(InstanceCreateParams {
            perspective_id: uuid.clone(),
            class_name: "Channel".into(),
            properties: Some(props(&[
                ("name", json!("general")),
                ("isPinned", json!(true)),
                ("rank", json!(3)),
            ])),
            base_uri: None,
            parent: None,
        }))
        .await;
    let created = parse(&out);
    assert_eq!(created["created"], true, "{out}");
    let channel = created["base_uri"].as_str().unwrap().to_string();

    // Read it back: native JSON types, not strings.
    let got = parse(
        &handler
            .instance_get(Parameters(InstanceGetParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: channel.clone(),
            }))
            .await,
    );
    assert_eq!(got["id"], channel);
    assert_eq!(got["name"], "general");
    assert_eq!(got["isPinned"], true);
    assert_eq!(got["rank"], 3);

    // Validation happens before any write: nothing created on failure.
    let bad = parse(
        &handler
            .instance_create(Parameters(InstanceCreateParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                properties: Some(props(&[("isPinned", json!("maybe"))])),
                base_uri: Some("flux://never-created".into()),
                parent: None,
            }))
            .await,
    );
    let err = bad["error"].as_str().unwrap();
    assert!(err.contains("name") && err.contains("isPinned"), "{err}");
    assert!(
        err.contains("boolean") && err.contains("minCount 1"),
        "{err}"
    );
    let missing = parse(
        &handler
            .instance_get(Parameters(InstanceGetParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: "flux://never-created".into(),
            }))
            .await,
    );
    assert!(missing["error"]
        .as_str()
        .unwrap()
        .contains("No Channel instance"));

    // Messages: one linked via `parent`, one via the collection tool.
    let m1 = parse(
        &handler
            .instance_create(Parameters(InstanceCreateParams {
                perspective_id: uuid.clone(),
                class_name: "Message".into(),
                properties: Some(props(&[("body", json!("Hello from a static tool"))])),
                base_uri: None,
                parent: Some(channel.clone()),
            }))
            .await,
    );
    assert_eq!(m1["created"], true, "{m1}");
    assert_eq!(m1["added_to_parent"], true);
    let msg1 = m1["base_uri"].as_str().unwrap().to_string();

    let m2 = parse(
        &handler
            .instance_create(Parameters(InstanceCreateParams {
                perspective_id: uuid.clone(),
                class_name: "message".into(), // case-insensitive class name
                properties: Some(props(&[("body", json!("Second message"))])),
                base_uri: None,
                parent: None,
            }))
            .await,
    );
    assert_eq!(m2["created"], true, "{m2}");
    assert_eq!(m2["class_name"], "Message");
    let msg2 = m2["base_uri"].as_str().unwrap().to_string();

    let add = parse(
        &handler
            .instance_add_to_collection(Parameters(InstanceAddToCollectionParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: channel.clone(),
                collection: "messages".into(),
                item_uri: msg2.clone(),
            }))
            .await,
    );
    assert_eq!(add["success"], true, "{add}");
    assert_eq!(add["links_added"], 1, "{add}");

    // Adding an item that is already a member is a no-op: no second link,
    // no duplicate entry when the collection is read back.
    let again = parse(
        &handler
            .instance_add_to_collection(Parameters(InstanceAddToCollectionParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: channel.clone(),
                collection: "messages".into(),
                item_uri: msg2.clone(),
            }))
            .await,
    );
    assert_eq!(again["success"], true, "{again}");
    assert_eq!(again["links_added"], 0, "{again}");
    assert_eq!(again["already_member"], true, "{again}");

    let got = parse(
        &handler
            .instance_get(Parameters(InstanceGetParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: channel.clone(),
            }))
            .await,
    );
    let members = got["messages"].as_array().expect("messages array");
    assert!(members.contains(&json!(msg1)) && members.contains(&json!(msg2)));
    assert_eq!(members.len(), 2, "no duplicate membership: {got}");
    let raw_membership = crate::perspectives::get_perspective(&uuid)
        .unwrap()
        .get_links(&crate::types::LinkQuery {
            source: Some(channel.clone()),
            predicate: Some("ad4m://has_child".to_string()),
            target: Some(msg2.clone()),
            ..Default::default()
        })
        .await
        .unwrap();
    assert_eq!(raw_membership.len(), 1, "exactly one membership link: {raw_membership:?}");

    // Query: envelope-stored bodies come back as plain text; parent
    // scope and filters narrow; pagination reports the full count.
    let all = parse(
        &handler
            .instance_query(Parameters(InstanceQueryParams {
                perspective_id: uuid.clone(),
                class_name: "Message".into(),
                filter: None,
                parent: None,
                limit: None,
                offset: None,
            }))
            .await,
    );
    assert_eq!(all["count"], 2, "{all}");
    let bodies: Vec<&str> = all["instances"]
        .as_array()
        .unwrap()
        .iter()
        .map(|i| i["body"].as_str().unwrap())
        .collect();
    assert!(bodies.contains(&"Hello from a static tool"));
    assert!(bodies.contains(&"Second message"));

    let in_channel = parse(
        &handler
            .instance_query(Parameters(InstanceQueryParams {
                perspective_id: uuid.clone(),
                class_name: "Message".into(),
                filter: None,
                parent: Some(channel.clone()),
                limit: None,
                offset: None,
            }))
            .await,
    );
    assert_eq!(in_channel["count"], 2, "{in_channel}");

    let filtered = parse(
        &handler
            .instance_query(Parameters(InstanceQueryParams {
                perspective_id: uuid.clone(),
                class_name: "Message".into(),
                filter: Some(props(&[("body", json!({"contains": "Second"}))])),
                parent: None,
                limit: None,
                offset: None,
            }))
            .await,
    );
    assert_eq!(filtered["count"], 1, "{filtered}");
    assert_eq!(filtered["instances"][0]["id"], msg2);

    let paged = parse(
        &handler
            .instance_query(Parameters(InstanceQueryParams {
                perspective_id: uuid.clone(),
                class_name: "Message".into(),
                filter: None,
                parent: None,
                limit: Some(1),
                offset: None,
            }))
            .await,
    );
    assert_eq!(paged["count"], 1);
    assert_eq!(paged["total_count"], 2);

    let bad_filter = parse(
        &handler
            .instance_query(Parameters(InstanceQueryParams {
                perspective_id: uuid.clone(),
                class_name: "Message".into(),
                filter: Some(props(&[("subject", json!("x"))])),
                parent: None,
                limit: None,
                offset: None,
            }))
            .await,
    );
    let err = bad_filter["error"].as_str().unwrap();
    assert!(err.contains("subject") && err.contains("body"), "{err}");

    // Update: partial, validated, atomic.
    let upd = parse(
        &handler
            .instance_update(Parameters(InstanceUpdateParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: channel.clone(),
                properties: props(&[
                    ("name", json!("general-renamed")),
                    ("isPinned", json!(false)),
                ]),
            }))
            .await,
    );
    assert_eq!(upd["success"], true, "{upd}");
    let got = parse(
        &handler
            .instance_get(Parameters(InstanceGetParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: channel.clone(),
            }))
            .await,
    );
    assert_eq!(got["name"], "general-renamed");
    assert_eq!(got["isPinned"], false);
    assert_eq!(got["rank"], 3, "untouched property must survive the update");

    let bad_upd = parse(
        &handler
            .instance_update(Parameters(InstanceUpdateParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: channel.clone(),
                properties: props(&[("messages", json!([msg1.clone()]))]),
            }))
            .await,
    );
    assert!(bad_upd["error"]
        .as_str()
        .unwrap()
        .contains("instance_add_to_collection"));

    // Remove: refuses when the class doesn't match, cascades when it does.
    let wrong = parse(
        &handler
            .instance_remove(Parameters(InstanceRemoveParams {
                perspective_id: uuid.clone(),
                class_name: "Message".into(),
                base_uri: channel.clone(),
            }))
            .await,
    );
    assert!(wrong["error"]
        .as_str()
        .unwrap()
        .contains("No Message instance"));

    let removed = parse(
        &handler
            .instance_remove(Parameters(InstanceRemoveParams {
                perspective_id: uuid.clone(),
                class_name: "Message".into(),
                base_uri: msg2.clone(),
            }))
            .await,
    );
    assert_eq!(removed["success"], true, "{removed}");
    assert!(removed["links_removed"].as_u64().unwrap() > 0);

    let left = parse(
        &handler
            .instance_query(Parameters(InstanceQueryParams {
                perspective_id: uuid.clone(),
                class_name: "Message".into(),
                filter: None,
                parent: None,
                limit: None,
                offset: None,
            }))
            .await,
    );
    assert_eq!(left["count"], 1);
    assert_eq!(left["instances"][0]["id"], msg1);

    // Unknown class: lists what is registered.
    let unknown = parse(
        &handler
            .instance_query(Parameters(InstanceQueryParams {
                perspective_id: uuid.clone(),
                class_name: "Widget".into(),
                filter: None,
                parent: None,
                limit: None,
                offset: None,
            }))
            .await,
    );
    let err = unknown["error"].as_str().unwrap();
    assert!(err.contains("Widget") && err.contains("Channel") && err.contains("Message"));
}

#[tokio::test(flavor = "multi_thread")]
async fn dynamic_tools_are_hidden_unless_flag_is_set() {
    let (hidden, uuid, _guard) = setup(false).await;
    let names: Vec<String> = hidden
        .exposed_tools()
        .await
        .iter()
        .map(|t| t.name.to_string())
        .collect();
    for expected in [
        "describe_perspective",
        "instance_create",
        "instance_query",
        "instance_get",
        "instance_update",
        "instance_add_to_collection",
        "instance_remove",
    ] {
        assert!(names.iter().any(|n| n == expected), "missing {expected}");
    }
    assert!(
        !names
            .iter()
            .any(|n| n.starts_with("channel_") || n.starts_with("message_")),
        "per-class tools leaked into the static surface: {names:?}"
    );

    let refused = hidden
        .dispatch_non_router_tool(
            "channel_create",
            Some(props(&[
                ("perspective_id", json!(uuid.clone())),
                ("name", json!("x")),
            ])),
        )
        .await
        .expect("dispatch returns a tool result, not a protocol error");
    assert_eq!(refused.is_error, Some(true));
    let text = refused
        .content
        .iter()
        .filter_map(|c| c.as_text().map(|t| t.text.clone()))
        .collect::<Vec<_>>()
        .join("\n");
    assert!(text.contains("channel_create") && text.contains("instance_create"));
    assert!(text.contains("dynamicClassTools"));

    // Same perspective, flag on: both surfaces are advertised.
    let shown = Ad4mMcpHandler::new(McpContext {
        admin_credential: Some("test-admin".to_string()),
        auth_token: Arc::new(RwLock::new(Some("test-admin".to_string()))),
        dynamic_class_tools: true,
    });
    let names: Vec<String> = shown
        .exposed_tools()
        .await
        .iter()
        .map(|t| t.name.to_string())
        .collect();
    assert!(names.iter().any(|n| n == "channel_create"), "{names:?}");
    assert!(names.iter().any(|n| n == "instance_create"));
}

#[tokio::test(flavor = "multi_thread")]
async fn instance_remove_from_collection_removes_only_the_membership_link() {
    let (handler, uuid, _guard) = setup(false).await;

    let channel = parse(
        &handler
            .instance_create(Parameters(InstanceCreateParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                properties: Some(props(&[("name", json!("general"))])),
                base_uri: None,
                parent: None,
            }))
            .await,
    );
    let channel = channel["base_uri"].as_str().unwrap().to_string();

    let mut msgs = Vec::new();
    for body in ["kept", "dropped"] {
        let m = parse(
            &handler
                .instance_create(Parameters(InstanceCreateParams {
                    perspective_id: uuid.clone(),
                    class_name: "Message".into(),
                    properties: Some(props(&[("body", json!(body))])),
                    base_uri: None,
                    parent: Some(channel.clone()),
                }))
                .await,
        );
        assert_eq!(m["created"], true, "{m}");
        msgs.push(m["base_uri"].as_str().unwrap().to_string());
    }
    let (kept, dropped) = (msgs[0].clone(), msgs[1].clone());

    let members = |got: &Value| got["messages"].as_array().cloned().unwrap_or_default();
    let got = parse(
        &handler
            .instance_get(Parameters(InstanceGetParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: channel.clone(),
            }))
            .await,
    );
    assert_eq!(members(&got).len(), 2, "{got}");

    let removed = parse(
        &handler
            .instance_remove_from_collection(Parameters(InstanceRemoveFromCollectionParams {
                perspective_id: uuid.clone(),
                class_name: "channel".into(), // case-insensitive class name
                base_uri: channel.clone(),
                collection: "Messages".into(), // case-insensitive collection
                item_uri: dropped.clone(),
            }))
            .await,
    );
    assert_eq!(removed["success"], true, "{removed}");
    assert_eq!(removed["links_removed"], 1, "{removed}");
    assert_eq!(removed["collection"], "messages");

    let got = parse(
        &handler
            .instance_get(Parameters(InstanceGetParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: channel.clone(),
            }))
            .await,
    );
    assert_eq!(members(&got), vec![json!(kept)], "{got}");

    // Only the membership link went away — the item itself still exists.
    let still_there = parse(
        &handler
            .instance_get(Parameters(InstanceGetParams {
                perspective_id: uuid.clone(),
                class_name: "Message".into(),
                base_uri: dropped.clone(),
            }))
            .await,
    );
    assert_eq!(still_there["body"], "dropped", "{still_there}");

    // Removing again is a no-op, not an error.
    let again = parse(
        &handler
            .instance_remove_from_collection(Parameters(InstanceRemoveFromCollectionParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: channel.clone(),
                collection: "messages".into(),
                item_uri: dropped.clone(),
            }))
            .await,
    );
    assert_eq!(again["success"], true, "{again}");
    assert_eq!(again["links_removed"], 0, "{again}");

    // Same validation as the add side: unknown / single-valued collections.
    let unknown = parse(
        &handler
            .instance_remove_from_collection(Parameters(InstanceRemoveFromCollectionParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: channel.clone(),
                collection: "members".into(),
                item_uri: kept.clone(),
            }))
            .await,
    );
    assert!(
        unknown["error"]
            .as_str()
            .unwrap()
            .contains("unknown collection"),
        "{unknown}"
    );
    let scalar = parse(
        &handler
            .instance_remove_from_collection(Parameters(InstanceRemoveFromCollectionParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: channel.clone(),
                collection: "name".into(),
                item_uri: kept.clone(),
            }))
            .await,
    );
    assert!(
        scalar["error"]
            .as_str()
            .unwrap()
            .contains("instance_update"),
        "{scalar}"
    );

    // Wrong owner: refuses before touching any link.
    let missing = parse(
        &handler
            .instance_remove_from_collection(Parameters(InstanceRemoveFromCollectionParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                base_uri: "ad4m://obj/nosuchchannel".into(),
                collection: "messages".into(),
                item_uri: kept.clone(),
            }))
            .await,
    );
    assert!(
        missing["error"]
            .as_str()
            .unwrap()
            .contains("No Channel instance"),
        "{missing}"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn add_child_and_get_children_are_class_agnostic() {
    let (handler, uuid, _guard) = setup(false).await;

    // Bare-string parent and a URI child: the parent is wrapped as a literal.
    let first = parse(
        &handler
            .add_child(Parameters(AddChildParams {
                perspective_id: uuid.clone(),
                parent: "my-room".into(),
                child: "ad4m://obj/one".into(),
            }))
            .await,
    );
    assert_eq!(first["success"], true, "{first}");
    assert_eq!(first["link"]["predicate"], "ad4m://has_child");
    assert_eq!(first["link"]["target"], "ad4m://obj/one");
    let wrapped_parent = first["link"]["source"].as_str().unwrap().to_string();
    assert!(
        wrapped_parent.starts_with("literal:"),
        "bare parent must be literal-wrapped, got {wrapped_parent}"
    );

    // Bare-string child gets wrapped too.
    tokio::time::sleep(std::time::Duration::from_millis(5)).await;
    let second = parse(
        &handler
            .add_child(Parameters(AddChildParams {
                perspective_id: uuid.clone(),
                parent: "my-room".into(),
                child: "second-thing".into(),
            }))
            .await,
    );
    assert_eq!(second["success"], true, "{second}");
    let second_child = second["link"]["target"].as_str().unwrap().to_string();
    assert!(second_child.starts_with("literal:"), "{second_child}");

    let listed = parse(
        &handler
            .get_children(Parameters(GetChildrenParams {
                perspective_id: uuid.clone(),
                parent: "my-room".into(),
                limit: None,
            }))
            .await,
    );
    assert_eq!(listed["parent"], wrapped_parent, "{listed}");
    assert_eq!(listed["count"], 2, "{listed}");
    assert_eq!(listed["total_count"], 2);
    let children = listed["children"].as_array().unwrap();
    assert_eq!(
        children[0]["id"], "ad4m://obj/one",
        "oldest first: {listed}"
    );
    assert_eq!(children[1]["id"], second_child);
    assert!(
        children[0]["timestamp"].as_str().unwrap() <= children[1]["timestamp"].as_str().unwrap()
    );
    assert!(children[0]["author"].as_str().unwrap().starts_with("did:"));

    // limit keeps the most recent N but still reports the full count.
    let last = parse(
        &handler
            .get_children(Parameters(GetChildrenParams {
                perspective_id: uuid.clone(),
                parent: "my-room".into(),
                limit: Some(1),
            }))
            .await,
    );
    assert_eq!(last["count"], 1, "{last}");
    assert_eq!(last["total_count"], 2);
    assert_eq!(last["children"][0]["id"], second_child);

    // The already-wrapped form addresses the same node.
    let via_uri = parse(
        &handler
            .get_children(Parameters(GetChildrenParams {
                perspective_id: uuid.clone(),
                parent: wrapped_parent.clone(),
                limit: None,
            }))
            .await,
    );
    assert_eq!(via_uri["count"], 2, "{via_uri}");

    // Unknown parent: empty, not an error.
    let none = parse(
        &handler
            .get_children(Parameters(GetChildrenParams {
                perspective_id: uuid.clone(),
                parent: "nobody-home".into(),
                limit: None,
            }))
            .await,
    );
    assert_eq!(none["count"], 0, "{none}");
    assert_eq!(none["children"], json!([]));

    // Interop with the class-aware side: an instance created with `parent`
    // shows up in the raw children listing of that parent.
    let channel = parse(
        &handler
            .instance_create(Parameters(InstanceCreateParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                properties: Some(props(&[("name", json!("interop"))])),
                base_uri: None,
                parent: Some("ad4m://self".into()),
            }))
            .await,
    );
    let channel = channel["base_uri"].as_str().unwrap().to_string();
    let root = parse(
        &handler
            .get_children(Parameters(GetChildrenParams {
                perspective_id: uuid.clone(),
                parent: "ad4m://self".into(),
                limit: None,
            }))
            .await,
    );
    assert_eq!(root["parent"], "ad4m://self");
    assert!(
        root["children"]
            .as_array()
            .unwrap()
            .iter()
            .any(|c| c["id"] == channel),
        "{root}"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn instance_transcript_reads_children_chronologically() {
    let (handler, uuid, _guard) = setup(false).await;
    let channel = parse(
        &handler
            .instance_create(Parameters(InstanceCreateParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                properties: Some(props(&[("name", json!("general"))])),
                base_uri: None,
                parent: None,
            }))
            .await,
    );
    let channel = channel["base_uri"].as_str().unwrap().to_string();

    for body in ["first words", "second words", "third words"] {
        let m = parse(
            &handler
                .instance_create(Parameters(InstanceCreateParams {
                    perspective_id: uuid.clone(),
                    class_name: "Message".into(),
                    properties: Some(props(&[("body", json!(body))])),
                    base_uri: None,
                    parent: Some(channel.clone()),
                }))
                .await,
        );
        assert_eq!(m["created"], true, "{m}");
        // Distinct reifier timestamps so the chronological order is defined.
        tokio::time::sleep(std::time::Duration::from_millis(5)).await;
    }

    let all = handler
        .instance_transcript(Parameters(InstanceTranscriptParams {
            perspective_id: uuid.clone(),
            class_name: "message".into(), // case-insensitive class name
            parent: channel.clone(),
            limit: None,
            text_property: None,
        }))
        .await;
    assert!(!all.starts_with("(showing"), "{all}");
    let pos = |needle: &str| {
        all.find(needle)
            .unwrap_or_else(|| panic!("{needle} missing in:\n{all}"))
    };
    assert!(pos("first words") < pos("second words") && pos("second words") < pos("third words"));
    assert!(all.contains("(did:key:"), "author DID per entry: {all}");
    assert_eq!(
        all.matches("]:\n").count() + all.matches("):\n").count(),
        3,
        "{all}"
    );

    let last_two = handler
        .instance_transcript(Parameters(InstanceTranscriptParams {
            perspective_id: uuid.clone(),
            class_name: "Message".into(),
            parent: channel.clone(),
            limit: Some(2),
            text_property: None,
        }))
        .await;
    assert!(
        last_two.starts_with("(showing last 2 of 3 Message instances"),
        "{last_two}"
    );
    assert!(!last_two.contains("first words"), "{last_two}");
    assert!(last_two.find("second words").unwrap() < last_two.find("third words").unwrap());

    // Explicit text property on a class without `body`: the Channel's name.
    let channels = handler
        .instance_transcript(Parameters(InstanceTranscriptParams {
            perspective_id: uuid.clone(),
            class_name: "Channel".into(),
            parent: "ad4m://self".into(),
            limit: None,
            text_property: Some("name".into()),
        }))
        .await;
    assert!(channels.starts_with("(no Channel instances"), "{channels}");

    let bad_prop = parse(
        &handler
            .instance_transcript(Parameters(InstanceTranscriptParams {
                perspective_id: uuid.clone(),
                class_name: "Message".into(),
                parent: channel.clone(),
                limit: None,
                text_property: Some("subject".into()),
            }))
            .await,
    );
    let err = bad_prop["error"].as_str().unwrap();
    assert!(err.contains("subject") && err.contains("body"), "{err}");

    // A collection is a property of the class but not a text property: the
    // error says so and offers only the single-valued ones.
    let collection_prop = parse(
        &handler
            .instance_transcript(Parameters(InstanceTranscriptParams {
                perspective_id: uuid.clone(),
                class_name: "Channel".into(),
                parent: "ad4m://self".into(),
                limit: None,
                text_property: Some("messages".into()),
            }))
            .await,
    );
    let err = collection_prop["error"].as_str().unwrap();
    assert!(err.contains("'messages'") && err.contains("collection"), "{err}");
    assert!(err.contains("name") && err.contains("description"), "{err}");
    assert!(
        !err["Single-valued properties".len()..].contains("messages")
            || err.rfind("messages").unwrap() < err.find("Single-valued").unwrap(),
        "collections must not be offered as text properties: {err}"
    );

    let empty = handler
        .instance_transcript(Parameters(InstanceTranscriptParams {
            perspective_id: uuid.clone(),
            class_name: "Message".into(),
            parent: "ad4m://obj/emptyroom".into(),
            limit: None,
            text_property: None,
        }))
        .await;
    assert_eq!(empty, "(no Message instances under ad4m://obj/emptyroom)");
}
