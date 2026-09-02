//! Unit tests — pure primitives + stubbed `RequiresQueryable` async
//! composition, exercised without a live `PerspectiveInstance`. See
//! [`super::e2e_tests`] for the live-perspective integration coverage.

use super::primitives::{build_query_input_for_requires, cardinality_satisfied, evidence_hash};
use super::queryable::{
    evaluate_flow_transitions, evaluate_single_query, evaluate_state_requires, RequiresQueryable,
};
use super::*;
use crate::perspectives::shacl_parser::{
    ConsensusRule, ModelQuery, ModelQueryCount, PropertyCondition,
};
use async_trait::async_trait;
use serde_json::{json, Value};
use std::collections::BTreeMap;

fn mq(class: &str) -> ModelQuery {
    ModelQuery {
        class_name: class.to_string(),
        r#where: None,
        count: None,
        linked_to: None,
        did_property: None,
        or: None,
    }
}

// ---- evidence_hash ----

#[test]
fn evidence_hash_stable_across_id_permutations() {
    let classes = vec!["ns://Perspective".to_string()];
    let a = evidence_hash(&classes, &["b://2".into(), "a://1".into(), "c://3".into()]);
    let b = evidence_hash(&classes, &["a://1".into(), "b://2".into(), "c://3".into()]);
    let c = evidence_hash(&classes, &["c://3".into(), "a://1".into(), "b://2".into()]);
    assert_eq!(a, b);
    assert_eq!(a, c);
}

#[test]
fn evidence_hash_differs_on_class_change() {
    let ids = vec!["a://1".into(), "b://2".into()];
    let a = evidence_hash(&["ns://Perspective".into()], &ids);
    let b = evidence_hash(&["ns://Tension".into()], &ids);
    assert_ne!(a, b);
}

#[test]
fn evidence_hash_differs_on_id_diff() {
    let classes = vec!["ns://Perspective".into()];
    let a = evidence_hash(&classes, &["a://1".into()]);
    let b = evidence_hash(&classes, &["a://2".into()]);
    assert_ne!(a, b);
}

#[test]
fn evidence_hash_hex_length() {
    // SHA256 is 32 bytes → 64 hex chars. Guards against a future
    // switch to a different digest silently changing the on-graph
    // shape.
    let h = evidence_hash(&["ns://X".into()], &[]);
    assert_eq!(h.len(), 64);
    assert!(h.chars().all(|c| c.is_ascii_hexdigit()));
}

// ---- cardinality_satisfied ----

#[test]
fn cardinality_unset_requires_one_match() {
    assert!(!cardinality_satisfied(None, 0));
    assert!(cardinality_satisfied(None, 1));
    assert!(cardinality_satisfied(None, 100));
}

#[test]
fn cardinality_min_only() {
    let c = ModelQueryCount {
        min: Some(2),
        max: None,
    };
    assert!(!cardinality_satisfied(Some(&c), 0));
    assert!(!cardinality_satisfied(Some(&c), 1));
    assert!(cardinality_satisfied(Some(&c), 2));
    assert!(cardinality_satisfied(Some(&c), 999));
}

#[test]
fn cardinality_max_only() {
    let c = ModelQueryCount {
        min: None,
        max: Some(3),
    };
    assert!(cardinality_satisfied(Some(&c), 0));
    assert!(cardinality_satisfied(Some(&c), 3));
    assert!(!cardinality_satisfied(Some(&c), 4));
}

#[test]
fn cardinality_range() {
    let c = ModelQueryCount {
        min: Some(1),
        max: Some(3),
    };
    assert!(!cardinality_satisfied(Some(&c), 0));
    assert!(cardinality_satisfied(Some(&c), 1));
    assert!(cardinality_satisfied(Some(&c), 2));
    assert!(cardinality_satisfied(Some(&c), 3));
    assert!(!cardinality_satisfied(Some(&c), 4));
}

#[test]
fn cardinality_both_unset_object_accepts_zero() {
    // Distinct from `count = None`: the caller explicitly passed
    // `{}` — treat as "no bound at all", 0 is a legal count.
    let c = ModelQueryCount {
        min: None,
        max: None,
    };
    assert!(cardinality_satisfied(Some(&c), 0));
    assert!(cardinality_satisfied(Some(&c), 5));
}

#[test]
fn cardinality_max_zero_negative_guard() {
    // "at most 0 matches" — a valid Popperian falsifier.
    let c = ModelQueryCount {
        min: None,
        max: Some(0),
    };
    assert!(cardinality_satisfied(Some(&c), 0));
    assert!(!cardinality_satisfied(Some(&c), 1));
}

// ---- build_query_input_for_requires ----

#[test]
fn build_query_bare_class_produces_empty_input() {
    // Just a className, no where / count / or → the guard becomes
    // "does this class have any instances?" No filter needed.
    let out = build_query_input_for_requires(&mq("ns://Perspective"), "did:key:acting");
    assert_eq!(out, json!({}));
}

#[test]
fn build_query_where_scalar_shorthands() {
    let mut w: BTreeMap<String, PropertyCondition> = BTreeMap::new();
    w.insert("state".into(), PropertyCondition::Str("done".into()));
    w.insert("priority".into(), PropertyCondition::Num(3.0));
    w.insert("archived".into(), PropertyCondition::Bool(false));
    let q = ModelQuery {
        class_name: "ns://Task".into(),
        r#where: Some(w),
        ..mq("ns://Task")
    };
    let out = build_query_input_for_requires(&q, "did:key:acting");
    assert_eq!(
        out,
        json!({
            "where": {
                "state": "done",
                "priority": 3.0,
                "archived": false,
            }
        })
    );
}

#[test]
fn build_query_where_typed_operators() {
    let mut w: BTreeMap<String, PropertyCondition> = BTreeMap::new();
    w.insert(
        "author".into(),
        PropertyCondition::Equals {
            equals: json!("alice"),
        },
    );
    w.insert(
        "tag".into(),
        PropertyCondition::In {
            one_of: vec![json!("a"), json!("b")],
        },
    );
    w.insert(
        "title".into(),
        PropertyCondition::Matches {
            matches: r"^Q\d+".into(),
        },
    );
    w.insert(
        "deletedAt".into(),
        PropertyCondition::Exists { exists: false },
    );
    w.insert(
        "createdAt".into(),
        PropertyCondition::Exists { exists: true },
    );
    let q = ModelQuery {
        class_name: "ns://Thing".into(),
        r#where: Some(w),
        ..mq("ns://Thing")
    };
    let out = build_query_input_for_requires(&q, "did:key:acting");
    // Verify each field individually (BTreeMap order is deterministic
    // but readability matters more than terseness in the assertion).
    let where_ = out.get("where").unwrap();
    assert_eq!(where_.get("author"), Some(&json!("alice")));
    assert_eq!(where_.get("tag"), Some(&json!(["a", "b"])));
    assert_eq!(where_.get("title"), Some(&json!({"regex": "^Q\\d+"})));
    assert_eq!(
        where_.get("deletedAt"),
        Some(&json!({"equals": Value::Null}))
    );
    assert_eq!(
        where_.get("createdAt"),
        Some(&json!({"not": {"equals": Value::Null}}))
    );
}

#[test]
fn build_query_did_property_substitutes_acting_did() {
    let q = ModelQuery {
        class_name: "ns://Endorsement".into(),
        did_property: Some("author".into()),
        ..mq("ns://Endorsement")
    };
    let did = "did:key:zAlice";
    let out = build_query_input_for_requires(&q, did);
    assert_eq!(out, json!({ "where": { "author": did } }));
}

#[test]
fn build_query_did_property_combines_with_where() {
    let mut w: BTreeMap<String, PropertyCondition> = BTreeMap::new();
    w.insert("state".into(), PropertyCondition::Str("approved".into()));
    let q = ModelQuery {
        class_name: "ns://Review".into(),
        r#where: Some(w),
        did_property: Some("reviewer".into()),
        ..mq("ns://Review")
    };
    let out = build_query_input_for_requires(&q, "did:key:zBob");
    assert_eq!(
        out,
        json!({
            "where": {
                "state": "approved",
                "reviewer": "did:key:zBob",
            }
        })
    );
}

#[test]
fn build_query_did_property_expression_substitutes_in_place() {
    // Escape hatch for hardcoded expressions ("agent:$did" etc.) —
    // we substitute $did in the string but the field-name column is
    // still the raw property (which model_query will reject with a
    // clear "no such property" — that's OK, this is a schema-level
    // typo the caller has to catch).
    let q = ModelQuery {
        class_name: "ns://Note".into(),
        did_property: Some("owner:$did".into()),
        ..mq("ns://Note")
    };
    let out = build_query_input_for_requires(&q, "did:key:zCarol");
    let field_val = out
        .get("where")
        .and_then(|w| w.get("owner:$did"))
        .expect("expression field preserved");
    assert_eq!(field_val, &json!("owner:did:key:zCarol"));
}

#[test]
fn build_query_or_composes_to_subclauses() {
    let mut w1: BTreeMap<String, PropertyCondition> = BTreeMap::new();
    w1.insert("role".into(), PropertyCondition::Str("moderator".into()));
    let mut w2: BTreeMap<String, PropertyCondition> = BTreeMap::new();
    w2.insert("role".into(), PropertyCondition::Str("owner".into()));
    let q = ModelQuery {
        class_name: "ns://Membership".into(),
        or: Some(vec![
            ModelQuery {
                class_name: "ns://Membership".into(),
                r#where: Some(w1),
                ..mq("ns://Membership")
            },
            ModelQuery {
                class_name: "ns://Membership".into(),
                r#where: Some(w2),
                ..mq("ns://Membership")
            },
        ]),
        ..mq("ns://Membership")
    };
    let out = build_query_input_for_requires(&q, "did:key:acting");
    assert_eq!(
        out,
        json!({
            "where": {
                "OR": [
                    { "role": "moderator" },
                    { "role": "owner" },
                ]
            }
        })
    );
}

#[test]
fn build_query_or_composes_with_top_level_where() {
    let mut top: BTreeMap<String, PropertyCondition> = BTreeMap::new();
    top.insert(
        "channel".into(),
        PropertyCondition::Str("ch://alpha".into()),
    );
    let mut branch: BTreeMap<String, PropertyCondition> = BTreeMap::new();
    branch.insert("role".into(), PropertyCondition::Str("owner".into()));
    let q = ModelQuery {
        class_name: "ns://Access".into(),
        r#where: Some(top),
        or: Some(vec![ModelQuery {
            class_name: "ns://Access".into(),
            r#where: Some(branch),
            ..mq("ns://Access")
        }]),
        ..mq("ns://Access")
    };
    let out = build_query_input_for_requires(&q, "did:key:acting");
    let where_ = out.get("where").unwrap();
    assert_eq!(where_.get("channel"), Some(&json!("ch://alpha")));
    let or_ = where_.get("OR").unwrap();
    assert_eq!(or_, &json!([{"role": "owner"}]));
}

#[test]
fn build_query_or_empty_omitted() {
    let q = ModelQuery {
        class_name: "ns://X".into(),
        or: Some(vec![]),
        ..mq("ns://X")
    };
    let out = build_query_input_for_requires(&q, "did:key:acting");
    // Empty or-array → no OR key emitted (would otherwise be a
    // never-matches false-positive on model_query).
    assert_eq!(out, json!({}));
}

#[test]
fn build_query_recursive_or_nests_subclauses() {
    // Two-level OR — multi-role composition with a fallback that itself
    // has alternatives.
    let mut w_leaf: BTreeMap<String, PropertyCondition> = BTreeMap::new();
    w_leaf.insert("role".into(), PropertyCondition::Str("admin".into()));
    let leaf = ModelQuery {
        class_name: "ns://M".into(),
        r#where: Some(w_leaf),
        ..mq("ns://M")
    };
    let inner_or = ModelQuery {
        class_name: "ns://M".into(),
        or: Some(vec![leaf]),
        ..mq("ns://M")
    };
    let mut w_outer: BTreeMap<String, PropertyCondition> = BTreeMap::new();
    w_outer.insert("role".into(), PropertyCondition::Str("owner".into()));
    let outer = ModelQuery {
        class_name: "ns://M".into(),
        or: Some(vec![
            ModelQuery {
                class_name: "ns://M".into(),
                r#where: Some(w_outer),
                ..mq("ns://M")
            },
            inner_or,
        ]),
        ..mq("ns://M")
    };
    let out = build_query_input_for_requires(&outer, "did:key:acting");
    // Outer OR carries two branches: {role: owner} and {OR: [{role: admin}]}
    let branches = out
        .get("where")
        .unwrap()
        .get("OR")
        .unwrap()
        .as_array()
        .unwrap();
    assert_eq!(branches.len(), 2);
    assert_eq!(branches[0], json!({"role": "owner"}));
    assert_eq!(branches[1], json!({"OR": [{"role": "admin"}]}));
}

// ============================================================================
// Async layer tests (stubbed perspective)
// ============================================================================
//
// These stub `RequiresQueryable` in-process so the evaluator's async
// composition can be exercised deterministically without spinning up a
// `PerspectiveInstance`. The end-to-end e2e_tests module below adds a
// live-perspective integration test that pins the same behaviour against
// the real SPARQL/Prolog/SDNA stack.

use crate::perspectives::flow_context::FlowInstanceRecord;
use crate::perspectives::shacl_parser::{FlowState, FlowTransition, SHACLFlow};
use std::collections::HashMap;
use std::sync::Mutex;

/// In-process stub. Records every `model_query` call for assertions,
/// and either returns a canned JSON string or an error keyed on
/// `class_name`.
struct StubPerspective {
    // (class_name, query_json) recorded in call order.
    calls: Mutex<Vec<(String, String)>>,
    // class_name -> either a raw JSON response or an error string.
    responses: HashMap<String, Result<String, String>>,
}

impl StubPerspective {
    fn new() -> Self {
        Self {
            calls: Mutex::new(Vec::new()),
            responses: HashMap::new(),
        }
    }
    fn with_instances(mut self, class: &str, ids: &[&str]) -> Self {
        let payload = json!({
            "instances": ids.iter().map(|id| json!({"id": *id})).collect::<Vec<_>>(),
            "totalCount": ids.len(),
        })
        .to_string();
        self.responses.insert(class.to_string(), Ok(payload));
        self
    }
    fn with_error(mut self, class: &str, msg: &str) -> Self {
        self.responses
            .insert(class.to_string(), Err(msg.to_string()));
        self
    }
    fn call_count_for(&self, class: &str) -> usize {
        self.calls
            .lock()
            .unwrap()
            .iter()
            .filter(|(c, _)| c == class)
            .count()
    }
}

#[async_trait]
impl RequiresQueryable for StubPerspective {
    async fn model_query(
        &self,
        class_name: &str,
        query_json: &str,
    ) -> Result<String, deno_core::anyhow::Error> {
        self.calls
            .lock()
            .unwrap()
            .push((class_name.to_string(), query_json.to_string()));
        match self.responses.get(class_name) {
            Some(Ok(payload)) => Ok(payload.clone()),
            Some(Err(msg)) => Err(deno_core::anyhow::anyhow!(msg.clone())),
            None => Err(deno_core::anyhow::anyhow!(
                "StubPerspective: no canned response for `{}`",
                class_name
            )),
        }
    }
}

// ---- evaluate_single_query ----

#[tokio::test]
async fn single_query_satisfied_with_unset_count_at_one_match() {
    let stub = StubPerspective::new().with_instances("ns://T", &["ad4m://t/1"]);
    let (ok, ids) = evaluate_single_query(&stub, &mq("ns://T"), "did:key:x")
        .await
        .unwrap();
    assert!(ok);
    assert_eq!(ids, vec!["ad4m://t/1".to_string()]);
    assert_eq!(stub.call_count_for("ns://T"), 1);
}

#[tokio::test]
async fn single_query_unsatisfied_when_zero_matches_and_unset_count() {
    let stub = StubPerspective::new().with_instances("ns://T", &[]);
    let (ok, ids) = evaluate_single_query(&stub, &mq("ns://T"), "did:key:x")
        .await
        .unwrap();
    assert!(!ok);
    assert!(ids.is_empty());
}

#[tokio::test]
async fn single_query_folds_cardinality_min_and_max() {
    let mut q = mq("ns://T");
    q.count = Some(ModelQueryCount {
        min: Some(2),
        max: Some(3),
    });

    // 2 matches → satisfied
    let stub = StubPerspective::new().with_instances("ns://T", &["a", "b"]);
    let (ok, _) = evaluate_single_query(&stub, &q, "did:key:x").await.unwrap();
    assert!(ok);

    // 1 match → unsatisfied (below min)
    let stub = StubPerspective::new().with_instances("ns://T", &["a"]);
    let (ok, _) = evaluate_single_query(&stub, &q, "did:key:x").await.unwrap();
    assert!(!ok);

    // 4 matches → unsatisfied (above max) but ids still returned
    let stub = StubPerspective::new().with_instances("ns://T", &["a", "b", "c", "d"]);
    let (ok, ids) = evaluate_single_query(&stub, &q, "did:key:x").await.unwrap();
    assert!(!ok);
    assert_eq!(ids.len(), 4);
}

#[tokio::test]
async fn single_query_bubbles_perspective_error() {
    let stub = StubPerspective::new().with_error("ns://T", "SDNA class not registered");
    let err = evaluate_single_query(&stub, &mq("ns://T"), "did:key:x")
        .await
        .unwrap_err();
    assert!(err.to_string().contains("SDNA class not registered"));
}

#[tokio::test]
async fn single_query_errors_on_missing_instances_key() {
    let mut stub = StubPerspective::new();
    stub.responses.insert(
        "ns://T".to_string(),
        Ok(json!({"totalCount": 0}).to_string()),
    );
    let err = evaluate_single_query(&stub, &mq("ns://T"), "did:key:x")
        .await
        .unwrap_err();
    assert!(err.to_string().contains("missing `instances`"));
}

#[tokio::test]
async fn single_query_serializes_translated_input() {
    // Guards that the async layer forwards the pure translator's
    // output verbatim — a regression in either half would show up
    // here as a diff on the recorded query JSON.
    let mut w: BTreeMap<String, PropertyCondition> = BTreeMap::new();
    w.insert(
        "author".into(),
        PropertyCondition::Str("did:key:xyz".into()),
    );
    let q = ModelQuery {
        class_name: "ns://T".into(),
        r#where: Some(w),
        ..mq("ns://T")
    };
    let stub = StubPerspective::new().with_instances("ns://T", &["ad4m://t/1"]);
    let _ = evaluate_single_query(&stub, &q, "did:key:acting")
        .await
        .unwrap();
    let calls = stub.calls.lock().unwrap();
    assert_eq!(calls.len(), 1);
    let recorded: Value = serde_json::from_str(&calls[0].1).unwrap();
    assert_eq!(recorded, json!({"where": {"author": "did:key:xyz"}}));
}

// ---- evaluate_state_requires ----

#[tokio::test]
async fn state_requires_empty_returns_some_empty_evidence() {
    let stub = StubPerspective::new();
    let out = evaluate_state_requires(&stub, &[], "did:key:x")
        .await
        .unwrap();
    assert_eq!(out, Some((Vec::new(), Vec::new())));
    // No calls should be made — vacuous truth.
    assert_eq!(stub.calls.lock().unwrap().len(), 0);
}

#[tokio::test]
async fn state_requires_and_short_circuits_on_first_unsat() {
    let stub = StubPerspective::new()
        .with_instances("ns://A", &["ad4m://a/1"])
        .with_instances("ns://B", &[]) // fails the AND
        .with_instances("ns://C", &["ad4m://c/1"]);
    let requires = vec![mq("ns://A"), mq("ns://B"), mq("ns://C")];
    let out = evaluate_state_requires(&stub, &requires, "did:key:x")
        .await
        .unwrap();
    assert_eq!(out, None);
    // A + B queried, C skipped after B failed.
    assert_eq!(stub.call_count_for("ns://A"), 1);
    assert_eq!(stub.call_count_for("ns://B"), 1);
    assert_eq!(stub.call_count_for("ns://C"), 0);
}

#[tokio::test]
async fn state_requires_unions_evidence_and_dedups_across_queries() {
    // Same instance URI returned by two different guards; classes de-dup
    // and IDs de-dup while preserving first-occurrence order.
    let stub = StubPerspective::new()
        .with_instances("ns://A", &["ad4m://x/1", "ad4m://x/2"])
        .with_instances("ns://B", &["ad4m://x/2", "ad4m://x/3"]);
    let requires = vec![mq("ns://A"), mq("ns://B"), mq("ns://A")];
    let out = evaluate_state_requires(&stub, &requires, "did:key:x")
        .await
        .unwrap();
    let (classes, ids) = out.expect("all guards satisfied");
    assert_eq!(classes, vec!["ns://A".to_string(), "ns://B".to_string()]);
    assert_eq!(
        ids,
        vec![
            "ad4m://x/1".to_string(),
            "ad4m://x/2".to_string(),
            "ad4m://x/3".to_string()
        ]
    );
}

#[tokio::test]
async fn state_requires_bubbles_query_error() {
    let stub = StubPerspective::new()
        .with_instances("ns://A", &["ad4m://a/1"])
        .with_error("ns://B", "SDNA class not registered");
    let requires = vec![mq("ns://A"), mq("ns://B")];
    let err = evaluate_state_requires(&stub, &requires, "did:key:x")
        .await
        .unwrap_err();
    assert!(err.to_string().contains("SDNA class not registered"));
}

// ---- evaluate_flow_transitions ----

fn simple_flow(name: &str, transitions: &[(&str, &str)]) -> SHACLFlow {
    let mut states: Vec<FlowState> = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for (from, to) in transitions {
        for s in [from, to] {
            if seen.insert(s.to_string()) {
                states.push(FlowState {
                    name: s.to_string(),
                    value: 0.0,
                    interpretation_hint: None,
                    requires: None,
                    semantic_check: None,
                    consensus_rule: None,
                });
            }
        }
    }
    SHACLFlow {
        name: name.to_string(),
        namespace: format!("{}://", name.to_lowercase()),
        states,
        transitions: transitions
            .iter()
            .map(|(f, t)| FlowTransition {
                action_name: format!("{}To{}", f, t),
                from_state: f.to_string(),
                to_state: t.to_string(),
                actions: Vec::new(),
            })
            .collect(),
        interpretation_hint: None,
        input_types: Vec::new(),
        output_types: Vec::new(),
        creation_hint: None,
        context: None,
        consensus_rule: None,
    }
}

fn set_requires(state: &mut FlowState, requires: Vec<ModelQuery>) {
    state.requires = Some(requires);
}

fn record(flow_uri: &str, uri: &str, subject: &str, state: &str) -> FlowInstanceRecord {
    FlowInstanceRecord {
        flow_uri: flow_uri.into(),
        instance_uri: uri.into(),
        subject: subject.into(),
        current_state: state.into(),
        created_at: None,
    }
}

#[tokio::test]
async fn flow_transitions_emits_one_satisfied_per_reachable_state() {
    let mut flow = simple_flow("Delivery", &[("identified", "scoped")]);
    let scoped = flow.states.iter_mut().find(|s| s.name == "scoped").unwrap();
    set_requires(scoped, vec![mq("ns://Task")]);
    let flows = HashMap::from([("delivery://DeliveryFlow".into(), flow)]);
    let recs = vec![record(
        "delivery://DeliveryFlow",
        "ad4m://flow/instance/1",
        "ad4m://task/1",
        "identified",
    )];
    let stub = StubPerspective::new().with_instances("ns://Task", &["ad4m://task/1"]);
    let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
    assert_eq!(out.len(), 1);
    assert_eq!(out[0].flow_name, "Delivery");
    assert_eq!(out[0].instance_uri, "ad4m://flow/instance/1");
    assert_eq!(out[0].subject, "ad4m://task/1");
    assert_eq!(out[0].from_state, "identified");
    assert_eq!(out[0].to_state, "scoped");
    assert_eq!(out[0].evidence_ids, vec!["ad4m://task/1".to_string()]);
    assert_eq!(out[0].evidence_hash.len(), 64);
}

#[tokio::test]
async fn flow_transitions_skips_state_when_requires_unsatisfied() {
    let mut flow = simple_flow("Delivery", &[("identified", "scoped")]);
    let scoped = flow.states.iter_mut().find(|s| s.name == "scoped").unwrap();
    set_requires(scoped, vec![mq("ns://Task")]);
    let flows = HashMap::from([("delivery://DeliveryFlow".into(), flow)]);
    let recs = vec![record(
        "delivery://DeliveryFlow",
        "ad4m://flow/instance/1",
        "ad4m://task/1",
        "identified",
    )];
    let stub = StubPerspective::new().with_instances("ns://Task", &[]);
    let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
    assert!(out.is_empty());
}

#[tokio::test]
async fn flow_transitions_skips_states_without_requires() {
    // No `requires` = no deterministic guard; the semanticCheck gate is
    // a separate concern and doesn't fire here.
    let flow = simple_flow("Delivery", &[("identified", "scoped")]);
    let flows = HashMap::from([("delivery://DeliveryFlow".into(), flow)]);
    let recs = vec![record(
        "delivery://DeliveryFlow",
        "ad4m://flow/instance/1",
        "ad4m://task/1",
        "identified",
    )];
    let stub = StubPerspective::new();
    let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
    assert!(out.is_empty());
    // No query should have run — no guards.
    assert_eq!(stub.calls.lock().unwrap().len(), 0);
}

#[tokio::test]
async fn flow_transitions_skips_records_with_unknown_flow_name() {
    // Definition unpublished or not yet synced — must not blow up.
    let flow = simple_flow("Delivery", &[("identified", "scoped")]);
    let flows = HashMap::from([("delivery://DeliveryFlow".into(), flow)]);
    let recs = vec![
        record(
            "delivery://DeliveryFlow",
            "ad4m://flow/instance/1",
            "ad4m://task/1",
            "identified",
        ),
        record(
            "unknown://UnknownFlow",
            "ad4m://flow/instance/2",
            "ad4m://task/2",
            "some",
        ),
    ];
    let stub = StubPerspective::new();
    let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
    assert!(out.is_empty());
}

#[tokio::test]
async fn flow_transitions_swallows_query_error_at_debug_and_continues() {
    // A `requires` on state A errors; state B in another record should
    // still be evaluated. Guards against one bad shape poisoning the
    // whole pass.
    let mut delivery = simple_flow("Delivery", &[("identified", "scoped")]);
    let scoped = delivery
        .states
        .iter_mut()
        .find(|s| s.name == "scoped")
        .unwrap();
    set_requires(scoped, vec![mq("ns://Broken")]);
    let mut deliberation = simple_flow("Deliberation", &[("proposal", "tension")]);
    let tension = deliberation
        .states
        .iter_mut()
        .find(|s| s.name == "tension")
        .unwrap();
    set_requires(tension, vec![mq("ns://Perspective")]);
    let flows = HashMap::from([
        ("delivery://DeliveryFlow".into(), delivery),
        ("deliberation://DeliberationFlow".into(), deliberation),
    ]);
    let recs = vec![
        record(
            "delivery://DeliveryFlow",
            "ad4m://flow/instance/1",
            "ad4m://task/1",
            "identified",
        ),
        record(
            "deliberation://DeliberationFlow",
            "ad4m://flow/instance/2",
            "ad4m://proposal/1",
            "proposal",
        ),
    ];
    let stub = StubPerspective::new()
        .with_error("ns://Broken", "unregistered class")
        .with_instances("ns://Perspective", &["ad4m://persp/1"]);
    let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
    assert_eq!(out.len(), 1);
    assert_eq!(out[0].flow_name, "Deliberation");
    assert_eq!(out[0].to_state, "tension");
}

#[tokio::test]
async fn flow_transitions_uses_state_consensus_over_flow_default() {
    let mut flow = simple_flow("Delivery", &[("identified", "scoped")]);
    flow.consensus_rule = Some(ConsensusRule {
        n: 1,
        from_role: None,
    });
    let scoped = flow.states.iter_mut().find(|s| s.name == "scoped").unwrap();
    set_requires(scoped, vec![mq("ns://Task")]);
    scoped.consensus_rule = Some(ConsensusRule {
        n: 3,
        from_role: None,
    });
    let flows = HashMap::from([("delivery://DeliveryFlow".into(), flow)]);
    let recs = vec![record(
        "delivery://DeliveryFlow",
        "ad4m://flow/instance/1",
        "ad4m://task/1",
        "identified",
    )];
    let stub = StubPerspective::new().with_instances("ns://Task", &["ad4m://task/1"]);
    let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
    assert_eq!(out.len(), 1);
    assert_eq!(out[0].consensus_rule.as_ref().unwrap().n, 3);
}

#[tokio::test]
async fn flow_transitions_falls_back_to_flow_consensus_when_state_unset() {
    let mut flow = simple_flow("Delivery", &[("identified", "scoped")]);
    flow.consensus_rule = Some(ConsensusRule {
        n: 2,
        from_role: None,
    });
    let scoped = flow.states.iter_mut().find(|s| s.name == "scoped").unwrap();
    set_requires(scoped, vec![mq("ns://Task")]);
    let flows = HashMap::from([("delivery://DeliveryFlow".into(), flow)]);
    let recs = vec![record(
        "delivery://DeliveryFlow",
        "ad4m://flow/instance/1",
        "ad4m://task/1",
        "identified",
    )];
    let stub = StubPerspective::new().with_instances("ns://Task", &["ad4m://task/1"]);
    let out = evaluate_flow_transitions(&stub, &recs, &flows, "did:key:acting").await;
    assert_eq!(out.len(), 1);
    assert_eq!(out[0].consensus_rule.as_ref().unwrap().n, 2);
}
