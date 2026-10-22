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
//!
//! **One call at a time.** Every isolate has the sandbox's whole memory cap, so calls made at once
//! wait their turn ([`CALLS_AT_ONCE`]) rather than each start one: how much memory the template
//! tools hold does not grow with how many callers call at once.

use super::{BundleSandbox, SandboxError, SandboxLimits};
use serde::Deserialize;
use serde_json::{json, Value};
use std::sync::{Arc, LazyLock};
use tokio::sync::{OnceCell, Semaphore};

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
    call_with(TOOLSET, source(), SandboxLimits::default(), tool, input).await
}

/// How many tool calls may run at once, each in an isolate of its own. One, as when every call
/// went through a bundle's single isolate.
const CALLS_AT_ONCE: usize = 1;

/// [`call`], with `source` standing in for the bundle, in an isolate called `name`.
async fn call_with(
    name: &'static str,
    source: Arc<str>,
    limits: SandboxLimits,
    tool: &str,
    input: Value,
) -> Result<Value, SandboxError> {
    static SLOTS: Semaphore = Semaphore::const_new(CALLS_AT_ONCE);
    let tool = tool.to_string();
    // Waited for by the caller: one that gives up while it waits leaves the queue, and its call
    // never runs.
    let slot = SLOTS
        .acquire()
        .await
        .map_err(|_| SandboxError::Closed("the template tools' calls were closed".into()))?;
    // Then on a task of its own, which holds the slot until the isolate's thread has ended: a
    // caller that gives up must not free it while the isolate still runs or holds its memory.
    tokio::spawn(async move {
        let _slot = slot;
        let sandbox = BundleSandbox::start(name, source, limits).await?;
        let answer = sandbox
            .call(&["toolsets", TOOLSET, "call"], vec![json!(tool), input])
            .await;
        sandbox.close().await;
        answer
    })
    .await
    .unwrap_or_else(|e| Err(SandboxError::Closed(format!("the call's task failed: {e}"))))
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

    /// Stands in for the bundle: its one tool keeps its isolate busy for a while, 300 ms unless
    /// the input says `ms`.
    const SLOW: &str = r#"
    export const toolsets = {
      template: {
        call: async (_tool, input) => { const until = Date.now() + (input.ms ?? 300); while (Date.now() < until) {} return { text: "ok" }; },
      },
    };
    "#;

    /// The most isolates called `name` running at once while `calls` run.
    async fn most_isolates_while(
        name: &str,
        calls: Vec<tokio::task::JoinHandle<Result<Value, SandboxError>>>,
    ) -> usize {
        let count = || crate::tool_bundles::sandbox::running::count(name);
        let mut most = 0;
        while calls.iter().any(|c| !c.is_finished()) {
            most = most.max(count());
            tokio::time::sleep(std::time::Duration::from_millis(2)).await;
        }
        for call in calls {
            assert_eq!(call.await.unwrap(), Ok(json!({ "text": "ok" })));
        }
        most
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn template_tool_calls_run_one_at_a_time() {
        init_v8_platform();
        let name = "slots-at-once";
        let calls = (0..4)
            .map(|_| {
                tokio::spawn(call_with(
                    name,
                    Arc::from(SLOW),
                    SandboxLimits::default(),
                    "slow",
                    json!({}),
                ))
            })
            .collect();
        assert_eq!(
            most_isolates_while(name, calls).await,
            1,
            "calls made at once must not each hold an isolate"
        );
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn a_template_tool_call_has_let_go_of_its_isolate_when_it_answers() {
        init_v8_platform();
        let name = "slots-released";
        for _ in 0..3 {
            let answer = call_with(
                name,
                Arc::from(SLOW),
                SandboxLimits::default(),
                "slow",
                json!({}),
            )
            .await;
            assert_eq!(answer, Ok(json!({ "text": "ok" })));
            assert_eq!(
                crate::tool_bundles::sandbox::running::count(name),
                0,
                "the call answered while its isolate's thread was still running"
            );
        }
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn a_template_tool_call_given_up_on_keeps_its_turn_until_its_isolate_is_done() {
        init_v8_platform();
        let name = "slots-given-up";
        let given_up = tokio::spawn(call_with(
            name,
            Arc::from(SLOW),
            SandboxLimits::default(),
            "slow",
            json!({}),
        ));
        tokio::time::sleep(std::time::Duration::from_millis(150)).await;
        given_up.abort();
        let next = tokio::spawn(call_with(
            name,
            Arc::from(SLOW),
            SandboxLimits::default(),
            "slow",
            json!({}),
        ));
        assert_eq!(
            most_isolates_while(name, vec![next]).await,
            1,
            "the next call started while the given-up one still ran"
        );
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn a_template_tool_call_given_up_on_while_it_waits_never_runs() {
        init_v8_platform();
        use crate::tool_bundles::sandbox::running;
        let wait = |ms| tokio::time::sleep(std::time::Duration::from_millis(ms));
        let slow = |name, ms: u64| {
            tokio::spawn(call_with(
                name,
                Arc::from(SLOW),
                SandboxLimits::default(),
                "slow",
                json!({ "ms": ms }),
            ))
        };
        let (holder, given_up, next) = ("queue-holder", "queue-given-up", "queue-next");

        let holding = slow(holder, 1500);
        while running::count(holder) == 0 {
            wait(2).await;
        }
        let waiting = slow(given_up, 300);
        wait(100).await;
        waiting.abort();
        let live = slow(next, 300);
        wait(100).await;
        assert_eq!(
            running::count(holder),
            1,
            "the first call ended before the others queued behind it"
        );

        assert_eq!(live.await.unwrap(), Ok(json!({ "text": "ok" })));
        assert_eq!(
            running::most(given_up),
            0,
            "the call given up on while it waited for its turn ran anyway"
        );
        assert_eq!(holding.await.unwrap(), Ok(json!({ "text": "ok" })));
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn a_template_tool_call_whose_isolate_fails_to_load_keeps_its_turn_until_the_isolate_is_done(
    ) {
        init_v8_platform();
        // Never finishes loading, so the load runs into its time limit.
        const HANGS: &str = "while (true) {}\nexport const toolsets = {};";
        // Throws while loading, once it holds enough that tearing the isolate down takes a while.
        const THROWS: &str = "globalThis.kept = Array.from({ length: 2000000 }, (_, i) => ({ i }));\nthrow new Error(\"no\");\nexport const toolsets = {};";
        let short = SandboxLimits {
            call_timeout: std::time::Duration::from_millis(300),
            ..SandboxLimits::default()
        };
        let mut still_running = vec![];
        for (name, source, limits, failed) in [
            (
                "load-hangs",
                HANGS,
                short,
                (|a| matches!(a, Err(SandboxError::Timeout(_)))) as fn(&_) -> bool,
            ),
            ("load-throws", THROWS, SandboxLimits::default(), |a| {
                matches!(a, Err(SandboxError::Load(_)))
            }),
        ] {
            let answer = call_with(name, Arc::from(source), limits, "slow", json!({})).await;
            assert!(failed(&answer), "{name}: {answer:?}");
            if crate::tool_bundles::sandbox::running::count(name) != 0 {
                still_running.push(name);
            }
        }
        assert_eq!(
            still_running,
            Vec::<&str>::new(),
            "the call gave up its turn while the isolate that failed to load still ran"
        );
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
