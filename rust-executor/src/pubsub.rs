use crate::graphql::graphql_types::GetFilter;
use crate::graphql::graphql_types::GetValue;
use coasys_juniper::{graphql_value, FieldError, FieldResult};
use futures::Stream;
use futures::StreamExt;
use log::error;
use serde::de::DeserializeOwned;
use std::collections::HashMap;
use std::collections::HashSet;
use std::pin::Pin;
use std::sync::Arc;
use std::sync::LazyLock;
use tokio::sync::broadcast;
use tokio::sync::Mutex;
use tokio_stream::wrappers::BroadcastStream;

type Topic = String;
type Message = String;

pub struct PubSub {
    subscribers: Mutex<HashMap<Topic, broadcast::Sender<Message>>>,
    subscribers_sync: std::sync::Mutex<HashMap<Topic, broadcast::Sender<Message>>>,
}

impl PubSub {
    pub fn new() -> Self {
        Self {
            subscribers: Mutex::new(HashMap::new()),
            subscribers_sync: std::sync::Mutex::new(HashMap::new()),
        }
    }

    pub async fn subscribe(&self, topic: &Topic) -> broadcast::Receiver<Message> {
        let mut subscribers = self.subscribers.lock().await;
        let mut subscribers_sync = self.subscribers_sync.lock().unwrap();
        let sender = subscribers
            .entry(topic.to_owned())
            .or_insert_with(|| broadcast::channel(10000).0);
        // Keep sync map in sync
        subscribers_sync
            .entry(topic.to_owned())
            .or_insert_with(|| sender.clone());
        sender.subscribe()
    }

    pub async fn publish(&self, topic: &Topic, message: &Message) {
        let subscribers = self.subscribers.lock().await;
        if let Some(sender) = subscribers.get(topic) {
            let _ = sender.send(message.clone()); // Ignore if no receivers
        }
    }

    /// Synchronous publish that doesn't require tokio runtime.
    /// Uses a separate std::sync::Mutex to avoid blocking the async runtime.
    pub fn publish_sync(&self, topic: &Topic, message: &Message) {
        let subscribers = self.subscribers_sync.lock().unwrap();
        if let Some(sender) = subscribers.get(topic) {
            let _ = sender.send(message.clone());
        }
    }
}

pub(crate) async fn subscribe_and_process<
    T: DeserializeOwned + Send + 'static + std::fmt::Debug + GetValue + GetFilter,
>(
    pubsub: Arc<PubSub>,
    topic: Topic,
    filter: Option<String>,
) -> Pin<Box<dyn Stream<Item = FieldResult<T::Value>> + Send>> {
    let receiver = pubsub.subscribe(&topic).await;
    let receiver_stream = BroadcastStream::new(receiver);

    let mapped_stream = receiver_stream.filter_map(move |result| {
        match result {
            Ok(msg) => match serde_json::from_str::<T>(&msg) {
                Ok(data) => {
                    if let Some(filter) = &filter {
                        let data_filter = data
                            .get_filter()
                            .expect("Could not get filter on T where we expected to filter");
                        if &data_filter != filter {
                            log::debug!(
                                "PubSub filter mismatch on topic {}: data_filter and subscription_filter differ",
                                topic
                            );
                            return futures::future::ready(None);
                        }
                    }
                    let value = data.get_value();
                    futures::future::ready(Some(Ok(value)))
                }
                Err(e) => {
                    let type_name = std::any::type_name::<T>();
                    error!("Failed to deserialize pubsub message: {:?}", e);
                    error!("Type: {}", type_name);
                    error!("Message: {:?}", msg);

                    let field_error = FieldError::new(
                        e,
                        graphql_value!({ "type": "INTERNAL_ERROR_COULD_NOT_SERIALIZE" }),
                    );
                    futures::future::ready(Some(Err(field_error)))
                }
            },
            Err(e) => {
                error!("Broadcast stream error: {:?}", e);
                // Skip lagged messages
                futures::future::ready(None)
            }
        }
    });

    Box::pin(mapped_stream)
}

lazy_static::lazy_static! {
    static ref GLOBAL_PUB_SUB: Arc<PubSub> = Arc::new(PubSub::new());

    pub static ref AGENT_STATUS_CHANGED_TOPIC: String = "agent-status-changed-topic".to_owned();
    pub static ref APPS_CHANGED: String = "apps-changed".to_owned();
    pub static ref AGENT_UPDATED_TOPIC: String = "agent-updated-topic".to_owned();
    pub static ref EXCEPTION_OCCURRED_TOPIC: String = "exception-occurred-topic".to_owned();
    pub static ref NEIGHBOURHOOD_SIGNAL_TOPIC: String = "neighbourhood-signal-received-topic".to_owned();
    pub static ref PERSPECTIVE_ADDED_TOPIC: String = "perspective-added-topic".to_owned();
    pub static ref PERSPECTIVE_LINK_ADDED_TOPIC: String = "perspective-link-added-topic".to_owned();
    pub static ref PERSPECTIVE_LINK_REMOVED_TOPIC: String = "perspective-link-removed-topic".to_owned();
    pub static ref PERSPECTIVE_LINK_UPDATED_TOPIC: String = "perspective-link-updated-topic".to_owned();
    pub static ref PERSPECTIVE_REMOVED_TOPIC: String = "perspective-removed-topic".to_owned();
    pub static ref PERSPECTIVE_UPDATED_TOPIC: String = "perspective-updated-topic".to_owned();
    pub static ref PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC: String = "perspective-sync-state-change-topic".to_owned();
    pub static ref RUNTIME_MESSAGED_RECEIVED_TOPIC: String = "runtime-messaged-received-topic".to_owned();
    pub static ref RUNTIME_NOTIFICATION_TRIGGERED_TOPIC: String = "runtime-notification-triggered-topic".to_owned();
    pub static ref AI_TRANSCRIPTION_TEXT_TOPIC: String = "ai-transcription-text-topic".to_owned();
    pub static ref AI_MODEL_LOADING_STATUS: String = "ai-model-loading-status".to_owned();
    pub static ref PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC: String = "perspective-query-subscription-topic".to_owned();
    pub static ref HOSTING_USER_INFO_CHANGED_TOPIC: String = "hosting-user-info-changed-topic".to_owned();
    pub static ref COMPUTE_LOG_UPDATED_TOPIC: String = "compute-log-updated-topic".to_owned();
}

/// Per-user dirty set for batched credit change notifications.
/// When credits are mutated, the affected user's email is inserted.
/// A background flush loop periodically drains the set and publishes
/// updated HostingUserInfo only for the affected users.
pub static DIRTY_CREDIT_USERS: LazyLock<std::sync::Mutex<HashSet<String>>> =
    LazyLock::new(|| std::sync::Mutex::new(HashSet::new()));

pub fn mark_credits_dirty(email: &str) {
    if let Ok(mut set) = DIRTY_CREDIT_USERS.lock() {
        set.insert(email.to_owned());
    }
}

/// Buffer a compute log entry for async publication.
/// The flush loop will drain these and publish to the subscription topic.
pub static PENDING_COMPUTE_LOG_ENTRIES: LazyLock<
    std::sync::Mutex<Vec<crate::graphql::graphql_types::ComputeLogEntry>>,
> = LazyLock::new(|| std::sync::Mutex::new(Vec::new()));

pub fn push_compute_log_entry(
    email: &str,
    operation: &str,
    summary: Option<&str>,
    cost: f64,
    credits_after: f64,
) {
    let entry = crate::graphql::graphql_types::ComputeLogEntry {
        id: 0, // will be set from DB if needed; not critical for realtime push
        user_email: email.to_owned(),
        timestamp: chrono::Utc::now().to_rfc3339(),
        operation: operation.to_owned(),
        summary: summary.map(|s| s.to_owned()),
        cost,
        credits_after,
    };
    if let Ok(mut vec) = PENDING_COMPUTE_LOG_ENTRIES.lock() {
        vec.push(entry);
    }
}

pub async fn get_global_pubsub() -> Arc<PubSub> {
    GLOBAL_PUB_SUB.clone()
}

pub fn get_global_pubsub_sync() -> Arc<PubSub> {
    GLOBAL_PUB_SUB.clone()
}
