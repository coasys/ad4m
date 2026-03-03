use crate::graphql::graphql_types::GetFilter;
use crate::graphql::graphql_types::GetValue;
use coasys_juniper::{graphql_value, FieldError, FieldResult};
use futures::Stream;
use futures::StreamExt;
use log::error;
use serde::de::DeserializeOwned;
use std::collections::HashMap;
use std::pin::Pin;
use std::sync::Arc;
use tokio::sync::broadcast;
use tokio::sync::Mutex;
use tokio_stream::wrappers::BroadcastStream;

type Topic = String;
type Message = String;

pub struct PubSub {
    subscribers: Mutex<HashMap<Topic, broadcast::Sender<Message>>>,
}

impl PubSub {
    pub fn new() -> Self {
        Self {
            subscribers: Mutex::new(HashMap::new()),
        }
    }

    pub async fn subscribe(&self, topic: &Topic) -> broadcast::Receiver<Message> {
        let mut subscribers = self.subscribers.lock().await;
        let sender = subscribers
            .entry(topic.to_owned())
            .or_insert_with(|| broadcast::channel(10000).0); // 10000 message buffer
        sender.subscribe()
    }

    pub async fn publish(&self, topic: &Topic, message: &Message) {
        let subscribers = self.subscribers.lock().await;
        if let Some(sender) = subscribers.get(topic) {
            let _ = sender.send(message.clone()); // Ignore if no receivers
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
                        if &data
                            .get_filter()
                            .expect("Could not get filter on T where we expected to filter")
                            != filter
                        {
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
}

pub async fn get_global_pubsub() -> Arc<PubSub> {
    GLOBAL_PUB_SUB.clone()
}
