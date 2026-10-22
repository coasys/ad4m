//! The template tools, compiled in.
//!
//! WE's rules for its templates (what a valid one is, how a patch applies, the reference an agent
//! reads) are JavaScript, and change with every WE release. Rather than a copy in Rust, the
//! executor carries WE's own build of them: `template_tools.bundle.js`, vendored by
//! `scripts/vendor-template-tools.sh`, which also writes `template_tools.json`: the bundle's tools
//! as the bundle declares them, and the WE commit they came from. Reading the tools from that file
//! keeps the tool list static, so it is known without starting an isolate; a test checks it says
//! what the bundle exports.
//!
//! **A record is seen by one call.** Each tool call runs in a fresh isolate of its own, started
//! from the bundle and dropped after the call, so nothing a call hands the bundle is there for any
//! other call, whoever makes it. One isolate is kept for the documentation, and is never given a
//! record.

use super::{BundleSandbox, SandboxError, SandboxLimits};
use serde::Deserialize;
use serde_json::{json, Value};
use std::sync::{Arc, LazyLock};
use tokio::sync::OnceCell;

/// The bundle, as WE builds it.
pub const SOURCE: &str = include_str!("template_tools.bundle.js");

/// The name the bundle exports its template toolset under, and the documentation topic every one
/// of its topics sits under.
pub const TOOLSET: &str = "template";

/// One tool, as the bundle describes it.
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct TemplateTool {
    pub name: String,
    pub description: String,
    /// The tool's own arguments, as JSON Schema.
    pub parameters: Value,
    /// Whether a call can answer with properties to write to the record.
    pub writes: bool,
}

/// What `template_tools.json` says about the bundle beside it.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Declared {
    /// The WE commit the bundle was built from.
    pub commit: String,
    /// The subject classes the tools work on.
    pub applies_to: Vec<String>,
    pub tools: Vec<TemplateTool>,
}

static DECLARED: LazyLock<Declared> = LazyLock::new(|| {
    serde_json::from_str(include_str!("template_tools.json"))
        .expect("template_tools.json is written by vendor-template-tools.sh")
});

/// The tools the bundle exports, and what they work on.
pub fn declared() -> &'static Declared {
    &DECLARED
}

/// The tool called `name`, if the bundle has one.
pub fn tool(name: &str) -> Option<&'static TemplateTool> {
    declared().tools.iter().find(|t| t.name == name)
}

fn source() -> Arc<str> {
    static SHARED: LazyLock<Arc<str>> = LazyLock::new(|| Arc::from(SOURCE));
    SHARED.clone()
}

/// Call a tool with `input` (the record, the arguments, the author) and await its answer, in a
/// fresh isolate that ends with the call.
pub async fn call(tool: &str, input: Value) -> Result<Value, SandboxError> {
    let sandbox = BundleSandbox::start(TOOLSET, source(), SandboxLimits::default()).await?;
    sandbox
        .call(&["toolsets", TOOLSET, "call"], vec![json!(tool), input])
        .await
}

/// The isolate that serves the documentation. Started on the first read; one that fails to start
/// is tried again on the next.
async fn docs() -> Result<&'static BundleSandbox, SandboxError> {
    static DOCS: OnceCell<BundleSandbox> = OnceCell::const_new();
    DOCS.get_or_try_init(|| BundleSandbox::start(TOOLSET, source(), SandboxLimits::default()))
        .await
}

/// Whether `topic` is one the bundle could serve: `template`, or under it.
pub fn is_topic(topic: &str) -> bool {
    topic == TOOLSET || topic.starts_with(&format!("{TOOLSET}/"))
}

/// The text of a documentation topic, or None when the bundle serves none at that path.
pub async fn read_doc(path: &str) -> Result<Option<String>, SandboxError> {
    let text = docs()
        .await?
        .call(&["toolsets", TOOLSET, "docs", "read"], vec![json!(path)])
        .await?;
    Ok(text.as_str().map(str::to_string))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::init_v8_platform;

    #[tokio::test]
    async fn template_tools_json_says_what_the_bundle_exports() {
        init_v8_platform();
        let sandbox = BundleSandbox::start(TOOLSET, source(), SandboxLimits::default())
            .await
            .expect("the vendored bundle loads");
        let applies_to: Vec<String> = serde_json::from_value(
            sandbox
                .read(&["toolsets", TOOLSET, "appliesTo"])
                .await
                .unwrap(),
        )
        .unwrap();
        let tools: Vec<TemplateTool> =
            serde_json::from_value(sandbox.read(&["toolsets", TOOLSET, "tools"]).await.unwrap())
                .unwrap();
        assert_eq!(
            (applies_to, tools),
            (declared().applies_to.clone(), declared().tools.clone()),
            "template_tools.json is stale: run scripts/vendor-template-tools.sh"
        );
    }

    #[tokio::test]
    async fn the_documentation_is_served_from_the_template_topic_down() {
        init_v8_platform();
        let start = read_doc(TOOLSET).await.unwrap().expect("a start topic");
        assert!(start.contains("template_update"), "{start}");
        assert_eq!(read_doc("template/no-such-topic").await.unwrap(), None);
        assert!(is_topic("template/core/expressions"));
        assert!(!is_topic("templates"));
    }

    #[tokio::test]
    async fn a_tool_the_bundle_does_not_have_is_answered_as_an_error() {
        init_v8_platform();
        let answer = call("no_such_tool", json!({ "record": {}, "args": {} }))
            .await
            .unwrap();
        assert!(answer["text"].as_str().unwrap().starts_with("Error"));
        assert_eq!(tool("no_such_tool"), None);
        assert!(tool("template_update").unwrap().writes);
    }
}
