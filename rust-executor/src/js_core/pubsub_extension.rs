//! `globalThis.PUBSUB.publish` for language code: refused.
//!
//! The op passed any topic and any payload from language code to the executor's pubsub, so a
//! language could forge any executor event (#1371): links on any perspective, app approvals,
//! notifications. No language publishes through it. The events a language legitimately raises
//! go through dedicated ops in `languages_extension.rs` (perspective diffs, sync state,
//! telepresence signals), and Rust names each one's topic and audience. So the allow-list of
//! topics language code may publish is empty, and every call fails.
//!
//! The op and `PUBSUB` stay, so the language runtime keeps the op table its snapshot was
//! built with and a language that calls it gets a clear error, not a missing global.

use crate::js_core::error::AnyhowWrapperError;
use deno_core::anyhow::anyhow;
use deno_core::op2;

/// Refuses to publish `topic`: language code may not publish executor events.
fn refuse(topic: &str) -> AnyhowWrapperError {
    let topic: String = topic.chars().take(100).collect();
    log::warn!("A language tried to publish executor event {topic:?}; refused");
    anyhow!("Language code may not publish executor events (topic {topic:?})").into()
}

#[op2(async(lazy), fast)]
async fn publish(
    #[string] topic: String,
    #[string] _data: String,
) -> Result<(), AnyhowWrapperError> {
    Err(refuse(&topic))
}

deno_core::extension!(
    pubsub_service,
    ops = [publish],
    esm_entry_point = "ext:pubsub_service/pubsub_extension.js",
    esm = [dir "src/js_core", "pubsub_extension.js"]
);

#[cfg(test)]
mod tests {
    use crate::events::Topic;
    use crate::pubsub::{get_global_pubsub, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC};

    /// #1371: language code called `PUBSUB.publish(topic, data)` with any topic, and the
    /// executor published it. Now the call fails and nothing reaches the pubsub: neither a
    /// typed event nor the one string topic left (live-query updates).
    #[tokio::test]
    async fn language_code_cannot_publish_executor_events() {
        let pubsub = get_global_pubsub().await;
        let mut typed = pubsub.subscribe_events();
        let mut query_updates = pubsub
            .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
            .await;
        let dir = tempfile::tempdir().unwrap();
        let runtime = crate::languages::language_runtime::LanguageRuntime::new(
            "pubsub-check".to_string(),
            dir.path().to_path_buf(),
            false,
        );
        runtime.init().await.unwrap();

        let forged = [
            PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC.as_str(),
            "perspective-link-added-topic",
            "apps-changed",
            "exception-occurred-topic",
            Topic::LinkAdded.as_str(),
        ];
        for topic in forged {
            let topic_lit = serde_json::to_string(topic).unwrap();
            let script = format!(
                r#"globalThis.PUBSUB.publish({topic_lit}, {{ perspectiveUuid: "p", link: {{}} }}).then(
                    () => "published",
                    e => "refused: " + (e && e.message))"#
            );
            let out = runtime.execute(&script).await.unwrap();
            assert!(
                out.contains("refused: Language code may not publish"),
                "{topic}: {out}"
            );
        }

        assert!(
            query_updates.try_recv().is_err(),
            "a forged live-query update reached the pubsub"
        );
        while let Ok(event) = typed.try_recv() {
            assert!(
                !event
                    .payload
                    .to_string()
                    .contains("\"perspectiveUuid\":\"p\""),
                "a forged event reached the pubsub: {event:?}"
            );
        }
    }
}
