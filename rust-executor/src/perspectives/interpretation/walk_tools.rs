//! The read tools an interpretation pass offers (#1804): the model walks the
//! graph with the generic MCP read tools, not with per-class reads.
//!
//! [`WALK_TOOLS`] is the one list of them. [`WalkToolsProvider`] narrows the
//! AD4M tool surface to that list and gives each tool the pass's defaults:
//!
//! - `class_name` is an enum of the offered classes. A call on another class
//!   is refused.
//! - `get_children` follows containment and declared relations (`via: all`),
//!   bound like `perspective_id`.
//! - Lists return [`DEFAULT_LIMIT`] rows (at most [`MAX_LIMIT`]), as outline
//!   rows, newest activity first. `fields: full` returns at most
//!   [`MAX_FULL_ROWS`] rows.
//! - Each list result ends with a line "N of M shown".
//!
//! The MCP defaults do not change: an MCP client gets the tools as they are.

use crate::ai_service::harness::provider::{ToolProvider, ToolSchema};
use anyhow::Result;
use serde_json::{json, Value};
use std::collections::BTreeSet;
use std::sync::Arc;

/// The read tools a pass offers, by MCP name. Add a read tool to the walk
/// here.
pub(crate) const WALK_TOOLS: [&str; 3] = ["instance_query", "get_children", "instance_get"];

/// Rows a list returns when the model gives no `limit`.
pub(crate) const DEFAULT_LIMIT: usize = 20;
/// The most rows a list returns.
pub(crate) const MAX_LIMIT: usize = 50;
/// The most full rows `instance_query` returns (`fields: full`).
pub(crate) const MAX_FULL_ROWS: usize = 5;

/// The AD4M tools of a pass: [`WALK_TOOLS`] only, with the pass's defaults.
pub(crate) struct WalkToolsProvider {
    inner: Arc<dyn ToolProvider>,
    /// The classes the pass offers, by the name the tools take.
    classes: BTreeSet<String>,
}

impl WalkToolsProvider {
    pub(crate) fn new(
        inner: Arc<dyn ToolProvider>,
        classes: impl IntoIterator<Item = String>,
    ) -> Self {
        Self {
            inner,
            classes: classes.into_iter().collect(),
        }
    }

    /// The schema the model sees: `class_name` as an enum of the offered
    /// classes, `via` bound, the pass's defaults in the descriptions.
    fn narrow(&self, mut tool: ToolSchema) -> ToolSchema {
        let Some(props) = tool
            .parameters
            .get_mut("properties")
            .and_then(Value::as_object_mut)
        else {
            return tool;
        };
        if let Some(class_name) = props.get_mut("class_name") {
            class_name["enum"] = json!(self.classes.iter().collect::<Vec<_>>());
        }
        if tool.name == "get_children" {
            props.remove("via");
        }
        if let Some(limit) = props.get_mut("limit") {
            limit["description"] = json!(format!(
                "How many rows to return (default {DEFAULT_LIMIT}, at most {MAX_LIMIT})."
            ));
        }
        if tool.name == "instance_query" {
            if let Some(order) = props.get_mut("order") {
                order["description"] = json!(
                    "Sort key. Default `activity`: the newest activity first (created, changed, \
                     or a row attached to it). `createdAt`: the newest first. A property name: \
                     ascending."
                );
            }
            if let Some(fields) = props.get_mut("fields") {
                fields["description"] = json!(format!(
                    "`outline` (default): id, title and parent links, one short row each. \
                     `full`: every property, at most {MAX_FULL_ROWS} rows. Get a full row only \
                     when you must decide about it."
                ));
            }
        }
        if tool.name == "get_children" {
            tool.description = "List the rows under a node, across classes: the rows whose \
                                declared relation points to it (its children, or the rows \
                                about it) and the rows it contains. Each child has id, class, \
                                title and via (the relation). Newest last."
                .to_string();
        }
        tool
    }

    /// The model's arguments with the pass's defaults and limits applied, or
    /// the error the model gets back.
    fn arguments(&self, name: &str, mut args: Value) -> std::result::Result<Value, String> {
        if !args.is_object() {
            args = json!({});
        }
        let map = args.as_object_mut().expect("an object");
        if let Some(class) = map.get("class_name") {
            let class = class.as_str().unwrap_or_default();
            if !self.classes.contains(class) {
                return Err(format!(
                    "class_name '{class}' is not offered in this pass. Use one of: {}.",
                    self.classes.iter().cloned().collect::<Vec<_>>().join(", ")
                ));
            }
        }
        match name {
            "instance_query" => {
                let full = map.get("fields").and_then(Value::as_str) == Some("full");
                if !full {
                    map.insert("fields".into(), json!("outline"));
                }
                let max = if full { MAX_FULL_ROWS } else { MAX_LIMIT };
                let limit = limit_of(map.get("limit")).unwrap_or(DEFAULT_LIMIT).min(max);
                map.insert("limit".into(), json!(limit));
                let order_missing = map
                    .get("order")
                    .and_then(Value::as_str)
                    .is_none_or(|o| o.trim().is_empty());
                if order_missing {
                    map.insert(
                        "order".into(),
                        json!(crate::perspectives::model_query::ACTIVITY_ORDER_KEY),
                    );
                }
            }
            "get_children" => {
                map.insert("via".into(), json!("all"));
                let limit = limit_of(map.get("limit"))
                    .unwrap_or(DEFAULT_LIMIT)
                    .min(MAX_LIMIT);
                map.insert("limit".into(), json!(limit));
            }
            _ => {}
        }
        Ok(args)
    }
}

/// A `limit` the model sent as a number or as a string of digits.
fn limit_of(value: Option<&Value>) -> Option<usize> {
    match value? {
        Value::Number(n) => n.as_u64().map(|n| n as usize),
        Value::String(s) => s.trim().parse().ok(),
        _ => None,
    }
}

/// The result with a last line "N of M shown" when it is a list.
fn with_shown_line(name: &str, result: String) -> String {
    let key = match name {
        "instance_query" => "instances",
        "get_children" => "children",
        _ => return result,
    };
    let Ok(parsed) = serde_json::from_str::<Value>(&result) else {
        return result;
    };
    let (Some(shown), Some(total)) = (
        parsed[key].as_array().map(Vec::len),
        parsed["total_count"].as_u64(),
    ) else {
        return result;
    };
    let more = if (shown as u64) < total {
        " Use offset, a filter or another order to see others."
    } else {
        ""
    };
    format!("{result}\n{shown} of {total} shown.{more}")
}

#[async_trait::async_trait]
impl ToolProvider for WalkToolsProvider {
    async fn tools(&self) -> Vec<ToolSchema> {
        let tools = self.inner.tools().await;
        // In the order of `WALK_TOOLS`, so the list is stable.
        WALK_TOOLS
            .iter()
            .filter_map(|name| tools.iter().find(|t| t.name == *name).cloned())
            .map(|t| self.narrow(t))
            .collect()
    }

    async fn call(&self, name: &str, args: Value) -> Result<String> {
        if !WALK_TOOLS.contains(&name) {
            anyhow::bail!("tool `{name}` is not offered in this pass");
        }
        let args = match self.arguments(name, args) {
            Ok(args) => args,
            Err(e) => return Ok(json!({ "error": e }).to_string()),
        };
        let result = self.inner.call(name, args).await?;
        Ok(with_shown_line(name, result))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    /// Records each dispatch; answers `ok:<name>`.
    struct MockProvider {
        tools: Vec<ToolSchema>,
        calls: Mutex<Vec<(String, Value)>>,
    }

    impl MockProvider {
        fn new(tools: Vec<ToolSchema>) -> Self {
            Self {
                tools,
                calls: Mutex::new(Vec::new()),
            }
        }

        fn recorded_calls(&self) -> Vec<(String, Value)> {
            self.calls.lock().unwrap().clone()
        }
    }

    #[async_trait::async_trait]
    impl ToolProvider for MockProvider {
        async fn tools(&self) -> Vec<ToolSchema> {
            self.tools.clone()
        }
        async fn call(&self, name: &str, args: Value) -> Result<String> {
            self.calls.lock().unwrap().push((name.to_string(), args));
            Ok(format!("ok:{name}"))
        }
    }

    fn schema(name: &str, props: Value) -> ToolSchema {
        ToolSchema {
            name: name.into(),
            description: String::new(),
            parameters: json!({"type": "object", "properties": props, "required": []}),
            side_effect: crate::ai_service::harness::provider::SideEffect::Read,
        }
    }

    fn mock() -> Arc<MockProvider> {
        Arc::new(MockProvider::new(vec![
            schema(
                "instance_query",
                json!({"class_name": {"type": "string"}, "limit": {"type": "integer"},
                       "order": {"type": "string"}, "fields": {"type": "string"}}),
            ),
            schema(
                "get_children",
                json!({"parent": {"type": "string"}, "limit": {"type": "integer"},
                       "via": {"type": "string"}}),
            ),
            schema("instance_get", json!({"class_name": {"type": "string"}})),
            schema("topic_query", json!({})),
            schema("add_perspective", json!({})),
        ]))
    }

    fn provider(inner: Arc<MockProvider>) -> WalkToolsProvider {
        WalkToolsProvider::new(inner, ["Topic".to_string(), "Item".to_string()])
    }

    #[tokio::test]
    async fn the_pass_sees_the_walk_tools_only_with_offered_classes() {
        let p = provider(mock());
        let tools = p.tools().await;
        let names: Vec<&str> = tools.iter().map(|t| t.name.as_str()).collect();
        assert_eq!(names, WALK_TOOLS.to_vec());
        let query = &tools[0].parameters["properties"];
        assert_eq!(query["class_name"]["enum"], json!(["Item", "Topic"]));
        assert!(
            tools[1].parameters["properties"].get("via").is_none(),
            "via is bound"
        );
        let refused = p.call("topic_query", json!({})).await;
        assert!(refused.is_err(), "a per-class read is not offered");
    }

    // Offered classes come from SHACL that neighbourhood peers write: a class
    // named after a static tool's first word (`List`, `Add`) admits no tool
    // beyond the walk tools.
    #[tokio::test]
    async fn a_class_named_like_a_static_tool_admits_nothing_more() {
        let p = WalkToolsProvider::new(mock(), ["List".to_string(), "Add".to_string()]);
        let names: Vec<String> = p.tools().await.into_iter().map(|t| t.name).collect();
        assert_eq!(names, WALK_TOOLS.to_vec());
        assert!(p.call("add_perspective", json!({})).await.is_err());
    }

    #[tokio::test]
    async fn defaults_and_limits_are_applied() {
        let inner = mock();
        let p = provider(inner.clone());
        p.call("instance_query", json!({"class_name": "Topic"}))
            .await
            .unwrap();
        p.call(
            "instance_query",
            json!({"class_name": "Topic", "limit": 500, "order": "createdAt"}),
        )
        .await
        .unwrap();
        p.call(
            "instance_query",
            json!({"class_name": "Item", "fields": "full", "limit": "40"}),
        )
        .await
        .unwrap();
        p.call(
            "get_children",
            json!({"parent": "x://p", "via": "containment"}),
        )
        .await
        .unwrap();
        let calls = inner.recorded_calls();
        assert_eq!(
            calls[0].1,
            json!({"class_name": "Topic", "fields": "outline", "limit": DEFAULT_LIMIT, "order": "activity"})
        );
        assert_eq!(calls[1].1["limit"], json!(MAX_LIMIT));
        assert_eq!(calls[1].1["order"], json!("createdAt"));
        assert_eq!(calls[2].1["fields"], json!("full"));
        assert_eq!(calls[2].1["limit"], json!(MAX_FULL_ROWS));
        assert_eq!(calls[3].1["via"], json!("all"), "via is always all");
        assert_eq!(calls[3].1["limit"], json!(DEFAULT_LIMIT));
    }

    #[tokio::test]
    async fn a_class_that_is_not_offered_is_refused() {
        let inner = mock();
        let p = provider(inner.clone());
        let out = p
            .call(
                "instance_get",
                json!({"class_name": "Secret", "base_uri": "x://1"}),
            )
            .await
            .unwrap();
        assert!(out.contains("not offered"), "{out}");
        assert!(inner.recorded_calls().is_empty(), "nothing dispatched");
    }

    #[test]
    fn a_list_result_ends_with_how_many_are_shown() {
        let out = with_shown_line(
            "instance_query",
            json!({"instances": [{"id": "a"}, {"id": "b"}], "total_count": 113}).to_string(),
        );
        assert!(
            out.ends_with("2 of 113 shown. Use offset, a filter or another order to see others."),
            "{out}"
        );
        let all = with_shown_line(
            "get_children",
            json!({"children": [{"id": "a"}], "total_count": 1}).to_string(),
        );
        assert!(all.ends_with("\n1 of 1 shown."), "{all}");
        let one = with_shown_line("instance_get", json!({"id": "a"}).to_string());
        assert!(!one.contains("shown"), "{one}");
    }
}
